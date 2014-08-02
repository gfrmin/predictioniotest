library(data.table)

appopen <- fread("ada/Ada_events_app_open.csv")
loggedmeal <- fread("ada/Ada_events_logged_meal.csv")
loggedweight <- fread("ada/Ada_events_logged_weight.csv")
mealapproved <- fread("ada/Ada_events_meal_approved.csv")
receivedcomment <- fread("ada/Ada_events_received_comment.csv")
users <- fread("ada/Ada_users.csv")

length(appopen$user_id)
# [1] 194235
length(unique(appopen$user_id))
# [1] 1872
nrow(users)
# [1] 907
sum(users$user_id %in% appopen$user_id)
# [1] 906
