
####Mixed integer programming model####

# Set-up ------------------------------------------------------------------

# involking the necessary libraries
suppressPackageStartupMessages({
library(readr)
library(purrr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(calendR)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(googlesheets4)
})

# Data preparation --------------------------------------------------------

## Demo------

# Preferences
# 1: Shiroshita,
# 2: Suzuki,
# 3: Dodo,
# 4: Fujii(Y),
# 5: Nakai,
# 6, Moriyama,
# 7: Kita,
# 8: Ishida,
# 9: Iwasaka,
# 10: Okano,
# 11: Itahashi,
# 12: Nakano,
# 13: Fujii(M)
# 14: Koide

## Here set the "target"
target_hms <- ymd_hms("2021-11-01-01:01:01", tz = "Asia/Tokyo")

## Process
target <- as.Date(target_hms, tz = "Asia/Tokyo")
target_m <- month(target)
target_y <- year(target)
target_end <- as.Date(add_with_rollback(target_hms, months(1), roll_to_first = TRUE, preserve_hms = FALSE) - days(1), tz = "Asia/Tokyo")
target_w <- weekdays(target)
target_interval <- as.numeric(target_end - target + 1)

# Creating matrix
Days <- rep(1:target_interval, length = target_interval*15)
times <- rep(target_interval, length = 15)
StaffId <- rep(c(1:15), times = times)
Pref <- rep(1, times = target_interval * 15)
StaffShiftPref <- data.frame(StaffId, Days, Pref)

## Preference
# 1: Shiroshita, Monday, Friday
# 2: Suzuki, Monday, Friday
# 3: Dodo, Wednesday
# 4: Fujii(Y), Friday
# 5: Nakai, Monday, Tuesday, Thursday, Friday
# 6, Moriyama, Monday
# 7: Kita, Monday, Tuesday
# 8: Ishida, Tuesday
# 9: Iwasaka, Wednesday, Thursday
# 10: Okano, Monday, Thursday, Friday
# 11: Itahashi, Monday, Tuesday, Thursday, Friday
# 12: Nakano, Monday, Tuesday, Wednesday, Thursday, Friday
# 13: Fujii(M): Saturday
# 14: Off: Sunday

StaffShiftPref
target_w
shift_change <- function(name, number, StaffShiftPref){ # name = 1-14, number = unavailable - first (Monday - Sunday =1), 0-6
  change <- seq(1+number, target_interval, by = 7)
  StaffShiftPref[(target_interval * (name - 1) + change), 3] <- 0
  return(StaffShiftPref)
}

# 1: Shiroshita, Monday, Friday, number:0,3
id = 1
fixed = c(5,19,26)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

inconv = c(8,15,22,29)
StaffShiftPref[target_interval * (id-1) + inconv, 3] <- 0

# 2: Suzuki, Monday, Friday, number: 0,3
id = 2
fixed = c(1,8,15,22,29,12)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 3: Dodo, Wednesday, number: 0
id = 3
fixed = c(10,17,24)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 4: Fujii(Y), Friday, number:2
id = 4
fixed = c(5,19,26)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 5: Nakai, Monday, Tuesday, Thursday, Friday, number: 1-7
id = 5

inconv = c(3,10,17,24)
StaffShiftPref[target_interval * (id-1) + inconv, 3] <- 0

#fixed = c(6,13,20,27)
#StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 6, Moriyama, Monday, number:3

StaffShiftPref <- shift_change(6, 1, StaffShiftPref)
StaffShiftPref <- shift_change(6, 2, StaffShiftPref)
StaffShiftPref <- shift_change(6, 4, StaffShiftPref)
StaffShiftPref <- shift_change(6, 5, StaffShiftPref)
StaffShiftPref <- shift_change(6, 6, StaffShiftPref)
StaffShiftPref <- shift_change(6, 7, StaffShiftPref)

# 7: Kita, Monday, Tuesday, number: 5,6
id = 7
fixed = c(1,2,8,9,15,16,22,29,30)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 8: Ishida, Tuesday, number:7
id = 8
fixed = c(2,9,16,30)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 9: Iwasaka, Wednesday, Thursday, number: 2,3
StaffShiftPref <- shift_change(9, 0, StaffShiftPref)
StaffShiftPref <- shift_change(9, 1, StaffShiftPref)
StaffShiftPref <- shift_change(9, 4, StaffShiftPref)
StaffShiftPref <- shift_change(9, 5, StaffShiftPref)
StaffShiftPref <- shift_change(9, 6, StaffShiftPref)

# 10: Okano, Monday, Thursday, Friday, number: 0,3,4
id = 10
fixed = c(10,17,18)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 11: Itahashi, Monday, Tuesday, Thursday, Friday, number: 1,2,5,6
id = 11
fixed = c(2,16)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 12: Nakano, Monday, Tuesday, Wednesday, Thursday, Friday, number: 0,1,2,5,6
id = 12
fixed = c(9,18)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 13: Fujii(M): Saturday, number:1
id = 13
fixed = c(6,13,20,27)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 14: Koide
id = 14
fixed = c(1,2,5,8,9,10,11,15,16,17,18,19,24,25,26,30)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# 15: Off: Sunday, number:2
id = 15
fixed = c(7,14,21,23,28)
StaffShiftPref[target_interval * (id-1) + fixed, 3] <- 2

# Adjustment
#id = 8
#inconv = c(19,26)
#StaffShiftPref[target_interval * (id-1) + inconv, 3] <- 0

## from google spreadsheet-----
# sun_start = function(date){
#   date_fm = as.Date(date,format="%Y-%m-%d")+1
#   
#   week_W2 = as.numeric(
#    strftime(date_fm,"%W")
#  )
#  
#  # If the first day is week 00, add 1
#  week_W2 = ifelse(
#    strftime(as.Date(paste(lubridate::year(date_fm),"01","01",sep = "-"))+1,"%W") == "00",
#    week_W2 + 1,
#    week_W2
#   ) 
#   
#   # return result
#   week_W2
# }
# 
# sun_start(target)  

#gs4_auth()
#sheet_id <- "https://docs.google.com/spreadsheets/d/18UV22-0k5sbWOUQdRWU-E1vRW7R3xLcpp3lsFICg6yM/edit#gid=0"
#submitted <- read_sheet(sheet_id, sheet = 1)
#submitted <- submitted %>% 
#  mutate(across(date1:date3, ~ ymd(.x))) %>% 
#  mutate(number1 = as.numeric(date1 - target) %% 7) %>% 
#  mutate(weekday1 = weekdays(date1)) %>% 
#  mutate(number2 = as.numeric(date2 - target) %% 7) %>% 
#  mutate(weekday2 = weekdays(date2)) %>% 
#  mutate(number3 = as.numeric(date3 - target) %% 7) %>% 
#  mutate(weekday3 = weekdays(date3))

# September



# Optimization ------------------------------------------------------------

## Our objective is to build a schedule with just the optimal number of staff for each schedule 
## Constraint
### 1. Each day, one staff
### 2. Fixed number of outpatients

## function to check if staff is available for a given day and shift----
checkPref <- function(staff, day)
{
  staffShiftSubset <- StaffShiftPref[StaffShiftPref$StaffId == staff & StaffShiftPref$Days == day,]
  staffShiftDaySubset <- staffShiftSubset[which(!names(staffShiftSubset) %in% c("StaffId", "Days"))]
  isAvail <- staffShiftDaySubset[,"Pref"]
  #to ensure that non-preferred shifts are given least preference, place a high penalty. If needed this step could be made a constraint.
  isPref <- case_when(isAvail == 0 ~  -10000000,
                      isAvail == 2 ~ +100000000000000,
                      isAvail == 1 ~ 1)
  return(isPref)
}

## set the number of rows(staff) and columns(weekday) for the matrix----
numOfStaff <- length(unique(StaffShiftPref$StaffId))
numOfDays <- target_interval

## build integer programming model----
model <- MIPModel() %>%
  add_variable(x[i,j], i = 1:numOfStaff, j = 1:numOfDays, type = "binary") %>%
  # optimize the number of staff based on availability and fixed dates
  set_objective(sum_expr(checkPref(staff = i, day = j) * x[i, j],
                         i = 1:numOfStaff,
                         j = 1:numOfDays),
                sense = "max") %>%
  
  # each day must have 1 staff
  add_constraint(sum_expr(x[i,j], i = 1:numOfStaff) == 1, j =  1:numOfDays) %>%
  
  # shiroshita
  add_constraint(sum_expr(x[1,j], j = 1:numOfDays) == 2) %>% 
  
  # suzuki
  add_constraint(sum_expr(x[2,j], j = 1:numOfDays) == 2) %>%
  
  # dodo
  add_constraint(sum_expr(x[3,j], j = 1:numOfDays) == 1) %>%
  
  # fujii(y)
  add_constraint(sum_expr(x[4,j], j = 1:numOfDays) == 1) %>%
  
  # nakai
  add_constraint(sum_expr(x[5,j], j = 1:numOfDays) == 1) %>%
  
  # moriyama
  add_constraint(sum_expr(x[6,j], j = 1:numOfDays) == 1) %>%
  
  # kita
  add_constraint(sum_expr(x[7,j], j = 1:numOfDays) >= 1) %>%
  add_constraint(sum_expr(x[7,j], j = 1:numOfDays) <= 2) %>%
  
  # ishida
  add_constraint(sum_expr(x[8,j], j = 1:numOfDays) >= 1) %>%
  add_constraint(sum_expr(x[8,j], j = 1:numOfDays) <= 2) %>%
  
  # isawaka
  add_constraint(sum_expr(x[9,j], j = 1:numOfDays) >= 1) %>%
  add_constraint(sum_expr(x[9,j], j = 1:numOfDays) <= 2) %>%
  
  # okano
  add_constraint(sum_expr(x[10,j], j = 1:numOfDays) >= 1) %>%
  add_constraint(sum_expr(x[10,j], j = 1:numOfDays) <= 2) %>%
  
  # itabashi
  add_constraint(sum_expr(x[11,j], j = 1:numOfDays) >= 2) %>%
  add_constraint(sum_expr(x[11,j], j = 1:numOfDays) <= 3) %>%
  
  # nakano
  add_constraint(sum_expr(x[12,j], j = 1:numOfDays) >= 2) %>%
  add_constraint(sum_expr(x[12,j], j = 1:numOfDays) <= 3) %>%
  
  # fujii(m)
  add_constraint(sum_expr(x[13,j], j = 1:numOfDays) == 4) %>% 
  
  # koide
  add_constraint(sum_expr(x[14,j], j = 1:numOfDays) >= 2) %>%
  add_constraint(sum_expr(x[14,j], j = 1:numOfDays) <= 2) %>%
  
  # Off
  add_constraint(sum_expr(x[15,j], j = 1:numOfDays) == 5)

## inspect model
model

## solve integer programming model----
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

## for display----

roster <- result %>% 
  get_solution(x[i,j])
roster <- roster[,c("i", "j","value")]
colnames(roster) <- c("staff", "day" , "rostered") 
roster <- roster %>% 
  select(day, rostered, staff) %>%
  filter(rostered == 1) %>% 
  arrange(day, rostered) %>% 
  select(-rostered) 
roster
#roster %>% write.csv("roster.csv")

# Calendar
calendar <- roster %>% 
  mutate(staff = case_when(staff == 1 ~ "城下",
                           staff == 2 ~ "鈴木",
                           staff == 3 ~ "百々",
                           staff == 4 ~ "藤井（将）",
                           staff == 5 ~ "中井",
                           staff == 6 ~  "森山",
                           staff == 7 ~  "喜多",
                           staff == 8 ~  "石田",
                           staff == 9 ~  "岩坂",
                           staff == 10 ~  "岡野",
                           staff == 11 ~ "板橋",
                           staff == 12 ~ "中野",
                           staff == 13 ~ "藤井（真）",
                           staff == 14 ~ "小出",
                           staff == 15 ~ "休み"))
num <- as.numeric(target_end - target) + 1
calendR(year = target_y,
        month = target_m,
        title = "INCC外来11月",
        text = calendar$staff,
        text.pos = 1:num)

