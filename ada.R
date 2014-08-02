library(data.table)
library(lubridate)
library(plyr)
library(randomForest)

users <- fread("ada/Ada_users.csv")
users[,`:=`(gender = factor(gender), paymentplan = factor(paymentplan), goal = factor(goal), diet = factor(diet))]
appopen <- fread("ada/Ada_events_app_open.csv")
loggedmeal <- fread("ada/Ada_events_logged_meal.csv")
loggedweight <- fread("ada/Ada_events_logged_weight.csv")
mealapproved <- fread("ada/Ada_events_meal_approved.csv"); mealapproved[,`:=`(V4 = NULL, V5 = NULL, V6 = NULL)]
receivedcomment <- fread("ada/Ada_events_received_comment.csv")


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
appopen <- appopen[users$user_id] # only interested in users we have sign-up and cancellation information about

setkey(loggedmeal, user_id)
loggedmeal <- loggedmeal[user_id %in% users$user_id]

setkey(loggedweight, user_id)
loggedweight <- loggedweight[user_id %in% users$user_id]

setkey(mealapproved, user_id)
mealapproved <- mealapproved[user_id %in% users$user_id]

setkey(receivedcomment, user_id)
receivedcomment <- receivedcomment[user_id %in% users$user_id]


# simplest model: count number of incidents to predict cancellation

appopenn <- appopen[,list(appopen = .N),by=user_id]
loggedmealn <- loggedmeal[,list(loggedmeal = .N),by=user_id]
loggedweightn <- loggedweight[,list(loggedweight = .N),by=user_id]
mealapprovedn <- mealapproved[,list(mealapproved = .N),by=user_id]
receivedcommentn <- receivedcomment[,list(receivedcomment = .N),by=user_id]

usersextra <- join_all(list(appopenn, loggedmealn, loggedweightn, mealapprovedn, receivedcommentn))
usersextra <- usersextra[,lapply(.SD, function(x) {ifelse(is.na(x), 0, x)})]

usersplus <- merge(users, usersextra)

usersplus[,`:=`(year = year(signup_date), month = month(signup_date), wday = wday(signup_date), mday = mday(signup_date), hour = hour(signup_date), minute = minute(signup_date), second = second(signup_date))]

usersplus[,cancelled := factor(as.numeric(cancel_date != ""))]
write.csv(usersplus, "usersplus.csv")


usersrf1 <- randomForest(cancelled ~ gender+age+paymentplan+profile_picture+goal+diet+timezoneoffset+appopen+loggedmeal+loggedweight+mealapproved+receivedcomment+year+month+wday+mday+hour+minute+second, data = usersplus, importance = TRUE, ntree = 1000) # around 19% error rate
varImpPlot(usersrf1) # interestingly month of signup is most important in predicting whether customer will cancel by now
