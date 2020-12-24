#Loading in required packages
library(tidyverse)
library(vroom)
library(openxlsx)
library(lubridate)

#### SHEET 1 ####

#Loading in data
sessions <- vroom("/Users/quinnjohnson/Downloads/DataAnalyst_Ecom_data_sessionCounts.csv")
cart <- vroom("/Users/quinnjohnson/Downloads/DataAnalyst_Ecom_data_addsToCart.csv")

#Just getting glimpse into what the data look like
head(sessions)
head(cart)

#Checking variable types of the data
lapply(sessions, class)
#Need to change dim_date to date format
sessions$dim_date <- mdy(sessions$dim_date)

#Creating sheet1 to get each permutation of months and devices and their respective metrics
sheet1 <- sessions %>%
  #Separating the month from the date
  mutate(., month = month(dim_date)) %>%
  #Separating year from the date in sessions
  mutate(., year = year(dim_date)) %>%
  #Grouping the data by month and device
  group_by(month, year, dim_deviceCategory) %>%
  #Calculating the desired metrics for each permutation of month and device
  summarize(sessions = sum(sessions), transactions = sum(transactions), qty = sum(QTY), ecr = sum(transactions)/sum(sessions)) %>%
  #Ordering data chronologically
  arrange(year)

#### SHEET 2 ####

#Reloading in data to ensure everything is clean
sessions <- vroom("/Users/quinnjohnson/Downloads/DataAnalyst_Ecom_data_sessionCounts.csv")
cart <- vroom("/Users/quinnjohnson/Downloads/DataAnalyst_Ecom_data_addsToCart.csv")

#Checking variable types of the data
lapply(sessions, class)
#Need to change dim_date to date format
sessions$dim_date <- mdy(sessions$dim_date)

#Retrieving what the two most recents months of data are
cart[c(nrow(cart)-1,nrow(cart)),1:2]
#5/2013-6/2013

#Making sessions data more usable by extracting month and year from the date
sessions <- sessions %>%
  #Separating the month from the date
  mutate(., month = month(dim_date)) %>%
  #Separating year from the date in sessions
  mutate(., year = year(dim_date))

#Changing cart column names to be the same as sessions column names
colnames(cart) <- c("year", "month", "addsToCart")

#Joining both dataframes into one using year and month as the matches
full <- full_join(cart, sessions, by = c("year","month"))

#Creating sheet 2 to get month over month comparisons for every month initially
sheet2 <- full %>%
  #Grouping by month and year
  group_by(month, year) %>%
  #Get values for each month individually
  #NOTE: '_rec' is short for 'recent'. Can also be thought of as the current month
  summarize(sessions_rec = sum(sessions), transactions_rec = sum(transactions), qty_rec = sum(QTY), 
            ecr_rec = sum(transactions)/sum(sessions), addsToCart_rec = mean(addsToCart)) %>%
  #Ordering data chronologically
  arrange(year) %>%
  #Ungrouping the rows so the months can talk to eachother
  ungroup(.) %>%
  #Getting the previous month's data
  #NOTE: '_prev' is short for 'previous'
  mutate(sessions_prev = lag(sessions_rec), transactions_prev = lag(transactions_rec), qty_prev = lag(qty_rec), 
         ecr_prev = lag(ecr_rec), addsToCart_prev = lag(addsToCart_rec)) %>%
  #Calculating the absoulte difference between the current and previous month
  #NOTE: '_abs' is short for absolute difference
  mutate(sessions_abs = sessions_rec - sessions_prev, transactions_abs = transactions_rec - transactions_prev, 
         qty_abs = qty_rec - qty_prev, ecr_abs = ecr_rec - ecr_prev, addsToCart_abs = addsToCart_rec - addsToCart_prev) %>%
  #Calculating the relative difference between the current and previous month
  #NOTE: '_rel' is short for relative difference
  mutate(sessions_rel = (sessions_rec - sessions_prev)/sessions_prev, transactions_rel = (transactions_rec - transactions_prev)/transactions_prev, 
         qty_rel = (qty_rec - qty_prev)/qty_prev, ecr_rel = (ecr_rec - ecr_prev)/ecr_prev, addsToCart_rel = (addsToCart_rec - addsToCart_prev)/addsToCart_prev) %>%
  #Removing every row besides the last, as the most recent month is the only one we care about in this example
  slice(., nrow(.))

#Creating a list of the dataframes we created
df_list <- list("monthDeviceAggr" = sheet1, "recentMonthComp" = sheet2)

#Writing the dataframes as sheets in an xlsx file
write.xlsx(df_list, "ixis_challenge.xlsx")
