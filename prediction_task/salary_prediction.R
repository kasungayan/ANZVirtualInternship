library(ggplot2)
library(dplyr)
library(ggmap)
library(GGally)


df_anz <- read.csv("ANZdataset.csv", header = TRUE, na.strings=c("","NA"))
df_anz$date <- as.Date(df_anz$date,format = "%d/%m/%Y")

annual_salary_summary <- df_anz %>% group_by(customer_id, age,long_lat,gender) %>% filter(txn_description == "PAY/SALARY") %>%
  summarize(Salary= round(mean(amount)*12)) 

annual_salary_summary_withoutOutliers <- annual_salary_summary[annual_salary_summary$Salary < 72000,]

boxplot(annual_salary_summary$Salary, horizontal = TRUE, staplewex = 1,
        main= "Customer Mean Salary Distribution: ANZ Customer Transactions Database",  xlab = "Annual Salary")
text(x=fivenum(annual_salary_summary$Salary),labels = fivenum(annual_salary_summary$Salary), y=1.25)


annual_purchase_summary <- df_anz %>% group_by(customer_id) %>% filter(!(txn_description %in% c('PAY/SALARY',"INTER BANK", "PHONE BANK","PAYMEN
T"))) %>%summarize(Purchase= round(mean(amount)*12)) 

annual_purchase_summary_salary_without_outliers <-  annual_purchase_summary[
  !(annual_purchase_summary$customer_id == "CUS-1816693151" | annual_purchase_summary$customer_id == "CUS-2178051368" 
  |annual_purchase_summary$customer_id == "CUS-51506836"), ]

customer_summary <- cbind(annual_salary_summary,Purchase_Summary=annual_purchase_summary$Purchase)
customer_summary_withoutoutliers <- cbind(annual_salary_summary_withoutOutliers,
                                          Purchase_Summary=annual_purchase_summary_salary_without_outliers$Purchase)


customer_summary <- customer_summary[-20,]
customer_summary_withoutoutliers <- customer_summary_withoutoutliers[-20,]

customer_summary <- data.frame(customer_summary)
customer_summary_withoutoutliers <- data.frame(customer_summary_withoutoutliers)

register_google(key = "Enter your GoogleAPI Key here...")

####################################################
df_locations <- unique(customer_summary$long_lat)

lon_df = NULL
lat_df = NULL

for (k in 1: length(df_locations)){
  df_loc <- strsplit(as.character(df_locations[k]),"\\s+")[[1]]
  lon_df[k] <- as.numeric(df_loc[1])
  lat_df[k] <- as.numeric(df_loc[2])
}

state_df = NULL

for(i in 1:99){
  print(i)
  location <- revgeocode(c(lon_df[i], lat_df[i]))
  states <- c("VIC", "NSW", "QLD", "NT", "JBT", "SA","TAS","WA", "ACT")
  state_match <- stri_extract_all_regex(location, states)
  state<- unlist(state_match[!(is.na(state_match))])
  state_df[i] <-  state
}

customer_summary$state <- state_df
customer_summary <- customer_summary[, -3]

customer_summary_df <- data.frame(customer_summary)
customer_summary_df <- customer_summary_df[, -1]

customer_summary_df$state <- as.factor(customer_summary_df$state)


customer_prediction_df = NULL


customer_prediction_df$State <- customer_summary_df$state
customer_prediction_df$Gender <-  customer_summary_df$gender
customer_prediction_df$Age <- customer_summary_df$age
customer_prediction_df$Avg_Purch <- customer_summary_df$Purchase_Summary
customer_prediction_df$Ann_Salary <- customer_summary_df$Salary

customer_prediction_df <- data.frame(customer_prediction_df)
#############################################################
df_locations <- unique(customer_summary_withoutoutliers$long_lat)

lon_df = NULL
lat_df = NULL

for (k in 1: length(df_locations)){
  df_loc <- strsplit(as.character(df_locations[k]),"\\s+")[[1]]
  lon_df[k] <- as.numeric(df_loc[1])
  lat_df[k] <- as.numeric(df_loc[2])
}

state_df = NULL

for(i in 1:96){
  print(i)
  location <- revgeocode(c(lon_df[i], lat_df[i]))
  states <- c("VIC", "NSW", "QLD", "NT", "JBT", "SA","TAS","WA", "ACT")
  state_match <- stri_extract_all_regex(location, states)
  state<- unlist(state_match[!(is.na(state_match))])
  state_df[i] <-  state
}

customer_summary_withoutoutliers$state <- state_df
customer_summary_withoutoutliers <- customer_summary_withoutoutliers[, -3]

customer_summary_withoutoutliers_df <- customer_summary_withoutoutliers[, -1]

customer_summary_withoutoutliers_df$state <- as.factor(customer_summary_withoutoutliers_df$state)


customer_prediction_df_withoutoutliers = NULL


customer_prediction_df_withoutoutliers$State <- customer_summary_withoutoutliers_df$state
customer_prediction_df_withoutoutliers$Gender <-  customer_summary_withoutoutliers_df$gender
customer_prediction_df_withoutoutliers$Age <- customer_summary_withoutoutliers_df$age
customer_prediction_df_withoutoutliers$Avg_Purch <- customer_summary_withoutoutliers_df$Purchase_Summary
customer_prediction_df_withoutoutliers$Ann_Salary <- customer_summary_withoutoutliers_df$Salary

customer_prediction_df_withoutoutliers <- data.frame(customer_prediction_df_withoutoutliers)

################################################################

write.table(customer_prediction_df, "customer_prediction.txt", sep = ",",row.names = FALSE, col.names = TRUE)
write.table(customer_prediction_df_withoutoutliers, "cust-sum-without-out.txt", sep = ",",row.names = FALSE, col.names = TRUE)















