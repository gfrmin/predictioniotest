library(data.table)
library(lubridate)
options(lubridate.fasttime = TRUE)
library(plyr)
library(randomForest)
library(randomForestSRC)
library(pec)
library(reshape2)
library(survival)
library(peperr)
library(ROCR)

users <- fread("ada/Ada_users.csv")
users[,`:=`(gender = factor(gender), paymentplan = factor(paymentplan), goal = factor(goal), diet = factor(diet))]
users$signup_date <- ymd_hms(users$signup_date, tz = "UTC")
users$cancel_date <- ymd_hms(users$cancel_date, tz = "UTC")
appopen <- fread("ada/Ada_events_app_open.csv")
appopen$timestamp <- ymd_hms(appopen$timestamp, tz = "UTC")
loggedmeal <- fread("ada/Ada_events_logged_meal.csv")
loggedmeal$timestamp <- ymd_hms(loggedmeal$timestamp, tz = "UTC")
loggedweight <- fread("ada/Ada_events_logged_weight.csv")
loggedweight$timestamp <- ymd_hms(loggedweight$timestamp, tz = "UTC")
mealapproved <- fread("ada/Ada_events_meal_approved.csv"); mealapproved[,`:=`(V4 = NULL, V5 = NULL, V6 = NULL)]
mealapproved$timestamp <- ymd_hms(mealapproved$timestamp, tz = "UTC")
receivedcomment <- fread("ada/Ada_events_received_comment.csv")
receivedcomment$timestamp <- ymd_hms(receivedcomment$timestamp, tz = "UTC")


length(appopen$user_id)
# [1] 194235
length(unique(appopen$user_id))
# [1] 1872 with app open information
nrow(users)
# [1] 907 we have sign-up and cancellation information about
sum(users$user_id %in% appopen$user_id)
# [1] 906 users opened the app

setkey(users, user_id)
setkey(appopen, user_id)
appopen <- appopen[user_id %in% users$user_id] # only interested in users we have sign-up and cancellation information about
setkey(loggedmeal, user_id)
loggedmeal <- loggedmeal[user_id %in% users$user_id]
setkey(loggedweight, user_id)
loggedweight <- loggedweight[user_id %in% users$user_id]
setkey(mealapproved, user_id)
mealapproved <- mealapproved[user_id %in% users$user_id]
setkey(receivedcomment, user_id)
receivedcomment <- receivedcomment[user_id %in% users$user_id]

# ignore events after cancellation
canceldate <- users[,list(user_id, cancel_date)]; canceldate[, event_type := "cancel"]
canceldate[is.na(cancel_date), cancel_date := ymd("2222-12-12")] # cancel date far into the future for those who haven't cancelled yet
setnames(canceldate, "cancel_date", "timestamp")
usersextra <- data.table(rbind.fill(appopen, loggedmeal, loggedweight, mealapproved, receivedcomment, canceldate))
setkey(usersextra, user_id, timestamp)
usersextra <- data.table(ddply(usersextra, "user_id", function(x) {x[x$timestamp <= x[x$event_type == "cancel",]$timestamp,]}))
setkey(usersextra, user_id)
usersextra <- usersextra[event_type != "cancel"]
# write.csv(usersextra, file = "usersextra.csv", row.names = FALSE)

## simplest model: count number of incidents to predict cancellation

usersextran <- data.table(dcast(melt(usersextra[,.N,by=c("user_id", "event_type")], id.vars = c("user_id", "event_type"), measure.vars = c("N")), user_id ~ event_type))
setkey(usersextran, user_id)
usersextran <- usersextran[,lapply(.SD, function(x) {ifelse(is.na(x), 0, x)})]

usersplus <- merge(users, usersextran)
usersplus[,`:=`(year = year(signup_date), month = month(signup_date), wday = wday(signup_date), mday = mday(signup_date), hour = hour(signup_date), minute = minute(signup_date), second = second(signup_date))]

usersplus[,cancelled := as.numeric(!is.na(cancel_date))]
usersplus[,canceldays := difftime(cancel_date, signup_date, units = "days")]
usersplus[,canceldaysnumber := as.numeric(canceldays)]
lastcanceldate <- max(usersplus$cancel_date, na.rm = TRUE) + days(1)
epochtodate <- function(epochseconds) {as.POSIXct(epochseconds, origin = "1970-01-01", tz = "GMT")}
usersplus[,canceldatecensored := epochtodate(ifelse(is.na(cancel_date), lastcanceldate, cancel_date))]
usersplus[,canceldayscensored := difftime(canceldatecensored, signup_date, units = "days")]
usersplus[,canceldayscensorednumber := as.numeric(canceldayscensored)]
write.csv(usersplus, "usersplus.csv")


usersrfcancel <- randomForest(factor(cancelled) ~ gender+age+paymentplan+profile_picture+goal+diet+timezoneoffset+app_open+logged_meal+logged_weight+meal_approved+received_comment+year+month+wday+mday+hour+minute+second, data = usersplus, importance = TRUE, ntree = 1000) # around 19% error rate
varImpPlot(usersrfcancel) # interestingly month of signup is most important in predicting whether customer will cancel by now


## now predict WHEN customer will cancel by converting cancellation date into number of seconds since January 1st 2014

usersrfcanceltime <- randomForest(canceldays ~ gender+age+paymentplan+profile_picture+goal+diet+timezoneoffset+app_open+logged_meal+logged_weight+meal_approved+received_comment+year+month+wday+mday+hour+minute+second, data = usersplus, importance = TRUE, ntree = 1000, subset = !is.na(canceldays)) # around 48% of variance explained
varImpPlot(usersrfcanceltime) # number of received_comments most predictive, followed by number of logged_meal

userspluscanceltimepreds <- usersplus[is.na(canceldays)]
userspluscanceltimepreds[, preds := predict(usersrfcanceltime, usersplus[is.na(canceldays)])] # make predictions for those who haven't cancelled yet

# random survival forest

usersrsf <- rfsrc(Surv(canceldayscensorednumber, cancelled) ~ gender+age+paymentplan+profile_picture+goal+diet+timezoneoffset+app_open+logged_meal+logged_weight+meal_approved+received_comment+year+month+wday+mday+hour+minute+second, data = usersplus, importance = "none", ntree = 500, splitrule = "logrankscore")
cat("error rate:" , usersrsf$err.rate[usersrsf$ntree], "\n")
plot.survival(usersrsf) # estimate of survival function

predictSurvProb.rfsrc <- function(object, newdata, times, ...){ # predict probability of cancelling by a particular time (or times) given certain covariates; needed for next couple of lines
    ptemp <- predict(object,newdata=newdata,...)$survival
    pos <- sindex(jump.times = object$time.interest, eval.times = times)
    p <- cbind(1,ptemp)[, pos + 1]
    if (NROW(p) != NROW(newdata) || NCOL(p) != length(times))
        stop("Prediction failed")
    p
}
pec.f <- as.formula(Hist(canceldayscensorednumber, cancelled) ~ 1)
usersrsfprederror <- pec(list(usersrsf), data = usersplus, formula = pec.f,
                        splitMethod = "bootcv", B = 50)


## user time series

signupdate <- users[,list(user_id, signup_date)]; signupdate[, event_type := "signup"]
setnames(signupdate, "signup_date", "timestamp")
canceldate <- users[!is.na(cancel_date),list(user_id, cancel_date)]; canceldate[, event_type := "cancel"]
setnames(canceldate, "cancel_date", "timestamp")
userstimeseries <- data.table(rbind.fill(signupdate, canceldate, usersextra))
setkey(userstimeseries, user_id, timestamp)
userstimeseries <- data.table(ddply(userstimeseries, "user_id", function(x) {x[x$timestamp >= x[x$event_type == "signup",]$timestamp,]}))
setkey(userstimeseries, user_id, timestamp)
userstimeseries[,timeend := c(ymd_hms(timestamp[2:.N], tz = "UTC"), NA), by = user_id]
userstimeseries[,event_type := event_type[2:.N], by = user_id]
userstimeseries[,`:=`(appopensum = cumsum(event_type == "app_open"), loggedmealsum = cumsum(event_type == "logged_meal"), loggedweightsum = cumsum(event_type == "logged_weight"), mealapprovedsum = cumsum(event_type == "meal_approved"), receivedcommentsum = cumsum(event_type == "received_comment"), cancelled = cumsum(event_type == "cancel")), by = user_id]
userstimeseries <- userstimeseries[,list(user_id, timestamp, timeend, appopensum, loggedmealsum, loggedweightsum, mealapprovedsum, receivedcommentsum, cancelled)]
userstimeseries <- userstimeseries[!(cancelled == 1 & is.na(timeend))]
userstimeseries[is.na(timeend), timeend := ymd_hms("2014-06-01 00:00:00", tz="UTC")]
userstimeseries[,timeend := as.numeric(difftime(timeend, timestamp[1], units="hours")), by = user_id]
userstimeseries[,timestamp := as.numeric(difftime(timestamp, timestamp[1], units="hours")), by = user_id]
userstimeseries[timeend <= timestamp, timeend := timestamp + 1/3600] # when events happen at the same time
setnames(userstimeseries, "timestamp", "timestart")
setkey(userstimeseries, user_id, timestart)

usersconstants <- users[,list(user_id, gender, age, paymentplan, profile_picture, goal, diet)]
setkey(usersconstants, user_id)
userstimeseriesplus <- usersconstants[userstimeseries]


# models on time series

userstimeseriespluscox <- coxph(Surv(timestart, timeend, cancelled) ~ appopensum+loggedmealsum+loggedweightsum+mealapprovedsum+receivedcommentsum+gender+age+paymentplan+profile_picture+goal+diet, data = userstimeseriesplus[unique(user_id)[1:630]]) # concordance is 68%
userstimeseriespluscoxpreds <- predict(userstimeseriespluscox, newdata = userstimeseriesplus[unique(user_id)[631:902]], type='expected')
# mresid <- (userstimeseriesplus[unique(user_id)[631:902]]$cancelled) - predict(userstimeseriespluscox, newdata = userstimeseriesplus[unique(user_id)[631:902]], type='expected')

rocpred <- prediction(userstimeseriespluscoxpreds, userstimeseriesplus[unique(user_id)[631:902]]$cancelled)
rocperf <- performance(pred, measure = "tpr", x.measure = "fpr") 
rocauc <- performance(pred, measure = "auc") # AUC of 0.85 on validation dataset!
png("roccurve.png")
plot(rocperf, col=rainbow(10))
dev.off()

userstimeseriespluscoxsurvfit <- survfit(userstimeseriespluscox)
plot(userstimeseriespluscoxsurvfit) # "mean" survival curve
