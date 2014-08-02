library(data.table)
library(lubridate)
options(lubridate.fasttime = TRUE)
library(plyr)
library(randomForest)
library(reshape2)

users <- fread("ada/Ada_users.csv")
users[,`:=`(gender = factor(gender), paymentplan = factor(paymentplan), goal = factor(goal), diet = factor(diet))]
users$signup_date <- ymd_hms(users$signup_date)
users$cancel_date <- ymd_hms(users$cancel_date)
appopen <- fread("ada/Ada_events_app_open.csv")
appopen$timestamp <- ymd_hms(appopen$timestamp)
loggedmeal <- fread("ada/Ada_events_logged_meal.csv")
loggedmeal$timestamp <- ymd_hms(loggedmeal$timestamp)
loggedweight <- fread("ada/Ada_events_logged_weight.csv")
loggedweight$timestamp <- ymd_hms(loggedweight$timestamp)
mealapproved <- fread("ada/Ada_events_meal_approved.csv"); mealapproved[,`:=`(V4 = NULL, V5 = NULL, V6 = NULL)]
mealapproved$timestamp <- ymd_hms(mealapproved$timestamp)
receivedcomment <- fread("ada/Ada_events_received_comment.csv")
receivedcomment$timestamp <- ymd_hms(receivedcomment$timestamp)


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
write.csv(usersextra, file = "usersextra.csv", row.names = FALSE)

## simplest model: count number of incidents to predict cancellation

usersextran <- data.table(dcast(melt(usersextra[,.N,by=c("user_id", "event_type")], id.vars = c("user_id", "event_type"), measure.vars = c("N")), user_id ~ event_type))
setkey(usersextran, user_id)
usersextran <- usersextran[,lapply(.SD, function(x) {ifelse(is.na(x), 0, x)})]

usersplus <- merge(users, usersextran)
usersplus[,`:=`(year = year(signup_date), month = month(signup_date), wday = wday(signup_date), mday = mday(signup_date), hour = hour(signup_date), minute = minute(signup_date), second = second(signup_date))]

usersplus[,cancelled := factor(as.numeric(!is.na(cancel_date)))]
# write.csv(usersplus, "usersplus.csv")


usersrfcancel <- randomForest(cancelled ~ gender+age+paymentplan+profile_picture+goal+diet+timezoneoffset+app_open+logged_meal+logged_weight+meal_approved+received_comment+year+month+wday+mday+hour+minute+second, data = usersplus, importance = TRUE, ntree = 1000) # around 19% error rate
varImpPlot(usersrfcancel) # interestingly month of signup is most important in predicting whether customer will cancel by now


## now predict WHEN customer will cancel by converting cancellation date into number of seconds since January 1st 2014

usersplus[,canceldays := difftime(cancel_date, signup_date, units = "days")]
usersrfcanceltime <- randomForest(canceldays ~ gender+age+paymentplan+profile_picture+goal+diet+timezoneoffset+app_open+logged_meal+logged_weight+meal_approved+received_comment+year+month+wday+mday+hour+minute+second, data = usersplus, importance = TRUE, ntree = 1000, subset = !is.na(canceldays)) # around 48% of variance explained
varImpPlot(usersrfcanceltime) # number of received_comments most predictive, followed by number of logged_meal

userspluscanceltimepreds <- usersplus[is.na(canceldays)]
userspluscanceltimepreds[, preds := predict(usersrfcanceltime, usersplus[is.na(canceldays)])] # make predictions for those who haven't cancelled yet
