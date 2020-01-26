#ANZ Data Analysis
library(ggplot2)
library(dplyr)
library(forecast)
library(ggmap)


df_anz <- read.csv("ANZdataset.csv", header = TRUE, na.strings=c("","NA"))
df_anz$date <- as.Date(df_anz$date,format = "%d/%m/%Y")
df_anz[is.na(df_anz$merchant_state)] <- "Not Defined"


df_locations <- unique(df_anz$long_lat)

lon_df = NULL
lat_df = NULL

for (k in 1: length(df_locations)){
  df_loc <- strsplit(as.character(df_locations[k]),"\\s+")[[1]]
  lon_df[k] <- as.numeric(df_loc[1])
  lat_df[k] <- as.numeric(df_loc[2])
}

lon_df <- lon_df[-35]
lat_df <- lat_df[-35]

df <- as.data.frame(cbind(lon_df,lat_df))

lon <- c(144.266,145.81)
lat <- c(-38.552, -37.365 )

map <- openmap(c( lat[1], lon[1] ),
               c( lat[2], lon[2] ),
               zoom = 15, 'osm')


autoplot(OpenStreetMap::openproj(map) ) +
  geom_point( data = df, aes( x = lon_df, y = lat_df, size = 5 ) ) +
  geom_text( data = df, aes( x = lon_df + .001, y = lat_df, label = customer ), hjust = 0) +
  theme( legend.position = "none" )



register_google(key = "Enter your GoogleAPI Key here...")
# getting the map
#mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 4,
 #                     maptype = "satellite", scale = 2)
map <- get_map(location = 'Melbourne', zoom = 10)
mapPoints <- ggmap(map) +
     geom_point(aes(x = lon_df, y = lat_df, size = 99), data = df, alpha = .5) + theme( legend.position = "none" )


df_anz %>%
  group_by(gender, amount) %>% select(gender, amount) %>% ggplot(aes(x = gender, y = amount)) + 
  geom_boxplot(width = 5) + coord_flip()


df_anz %>%
  group_by(customer_id, gender) %>%
  summarize(count= n()) %>% ggplot(aes(gender)) +
  geom_bar(width = 0.3) + coord_flip() + geom_text(stat='Count', aes(label=..count..), vjust=5) +
  ggtitle("Gender Distribution: ANZ Customer Transactions Database") + ylab("Percentage") + xlab("Gender Category")


df_anz_id <- unique(df_anz$customer_id)
demog_df <- NULL

for (idr in 1: length(df_anz_id)){
     customer_idnfo <- df_anz_id[idr]
     customer_df <- df_anz[df_anz$customer_id == customer_idnfo, ]
     customer_df <- customer_df[1, c('customer_id','gender','age')]
     if(is.null(demog_df)){
       demog_df <- customer_df
     } else{
       demog_df <- rbind(demog_df, customer_df)
     }
}

stat_box_data1 <- function(y, upper_limit = max(demog_df$age) * 1.20) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Median =', round(median(y), 1), '\n',
                    'Max =', round(max(y), 1), '\n',
                    'Min =', round(min(y), 1), '\n',
                    'Mean =', round(mean(y), 1), '\n')
    )
  )
}

stat_box_data2 <- function(y, upper_limit = max(df_trans_count$Transcations) * 1.20) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Median =', round(median(y), 1), '\n',
                    'Max =', round(max(y), 1), '\n',
                    'Min =', round(min(y), 1), '\n',
                    'Mean =', round(mean(y), 1), '\n')
    )
  )
}

stat_box_data3 <- function(y, upper_limit = max(df_gender_trans_count$Transactions) * 1.20) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('Median =', round(median(y), 1), '\n',
                    'Max =', round(max(y), 1), '\n',
                    'Min =', round(min(y), 1), '\n',
                    'Mean =', round(mean(y), 1), '\n')
    )
  )
}


ggplot(demog_df, aes(x = gender, y = age)) + 
  geom_boxplot(width = 0.2) + coord_flip() +  ggtitle("Age Distribution: ANZ Customer Transactions Database") + 
  ylab("Age Value") + xlab("Gender Category") +
  stat_summary(
    fun.data = stat_box_data1, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.6
  )

df_trans_count <- 
  group_by(df_anz, customer_id) %>% summarise(Transcations = n())

boxplot(df_trans_count$Transcations, horizontal = TRUE, staplewex = 1,
        main= "Transaction Count Distribution: ANZ Customer Transactions Database",  xlab = "Transcation Count")
text(x=fivenum(df_trans_count$Transcations),labels = fivenum(df_trans_count$Transcations), y=1.25)
#Mean: 120.43

df_trans_value <- 
  group_by(df_anz, customer_id) %>% summarise(Mean_Expenditure = round(mean(amount)))
boxplot(df_trans_value$Mean_Expenditure, horizontal = TRUE, staplewex = 1,
        main= "Customer Mean Expenditure Distribution: ANZ Customer Transactions Database",  xlab = "Mean Expenditure")
text(x=fivenum(df_trans_value$Mean_Expenditure),labels = fivenum(df_trans_value$Mean_Expenditure), y=1.25)
#Mean: 228.94


ggplot(df_anz, aes(gender)) +
  geom_bar(width = 0.3) + coord_flip() + geom_text(stat='count', aes(label=..count..), vjust=5) +
  ggtitle("Gender Transaction Distribution: ANZ Customer Transactions Database") + ylab("Transactions") + xlab("Gender Category")


df_anz %>%
  group_by(gender) %>%
  summarize(expense= round(mean(amount))) %>% ggplot(aes(x= gender, y=expense)) + 
  geom_bar(width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Gender Mean Expenditure: ANZ Customer Transactions Database") + ylab("Mean Expenditure") + xlab("Gender Category") +
  geom_text(aes(label= expense), vjust=5) 

df_anz %>%
  group_by(merchant_state) %>%
  summarize(count= n()) %>% ggplot(aes(x=merchant_state , y=count)) + 
  geom_bar(width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Merchant State Transactions: ANZ Customer Transactions Database") + ylab("Transactions") + xlab("Merchant States") +
  geom_text(aes(label= count), vjust=2) 

df_anz %>%
  group_by(merchant_state) %>%
  summarize(expense= round(mean(amount))) %>% ggplot(aes(x= merchant_state, y=expense)) + 
  geom_bar(width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Merchant State Mean Expenditure: ANZ Customer Transactions Database") + ylab("Mean Expenditure") + xlab("Gender Category") +
  geom_text(aes(label= expense), vjust=2) 


df_anz %>%
  group_by(txn_description) %>%
  summarize(count= n()) %>% ggplot(aes(x=txn_description , y=count)) + 
  geom_bar(width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Transaction Description: ANZ Customer Transactions Database") + ylab("Transactions") + xlab("Transaction Type") +
  geom_text(aes(label= count), vjust=3) 

df_anz %>%
  group_by(txn_description) %>%
  summarize(expense= round(mean(amount))) %>% ggplot(aes(x= txn_description, y=expense)) + 
  geom_bar(width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Transaction Type Mean Expenditure: ANZ Customer Transactions Database") + ylab("Mean Expenditure") + xlab("Transaction Type") +
  geom_text(aes(label= expense), vjust=3) 


df_anz_new <- df_anz %>%
  mutate(AgeCategory = case_when(18 <= age & age < 30 ~ '18-30',
                                30 <= age & age < 50 ~ '30-50',
                                TRUE ~ '50>'))

df_anz_new %>%
  group_by(AgeCategory) %>%
  summarize(count= n()) %>% ggplot(aes(x=AgeCategory , y=count)) + 
  geom_bar(width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Age Category Transactions: ANZ Customer Transactions Database") + ylab("Transactions") + xlab("Age Category") +
  geom_text(aes(label= count), vjust=5) 

df_anz_new %>%
  group_by(AgeCategory) %>%
  summarize(expense= round(mean(amount))) %>% ggplot(aes(x= AgeCategory, y=expense)) + 
  geom_bar(width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Age Category Mean Expenditure: ANZ Customer Transactions Database") + ylab("Mean Expenditure") + xlab("Age Category") +
  geom_text(aes(label= expense), vjust=5)


daily_count_ts <- group_by(df_anz,date)%>% summarize(count= n())
autoplot(ts(daily_count_ts$count)) + 
           ggtitle("Daily Transcation Time Series: ANZ Customer Transactions Database") + 
           ylab("Transaction Count") + xlab("Time")

#colSums(matrix(dailycount,7))

decomp <- mstl(ts(daily_count_ts$count,frequency = 7))
autoplot(decomp) + ggtitle("Daily Transcation Decomposition: ANZ Customer Transactions Database") + 
  ylab("Decomposed Transaction Count") + xlab("Time")


daily_count_ts <- group_by(df_anz,date)%>% summarize(count= round(sum(amount)))
autoplot(ts(daily_count_ts$count)) + 
  ggtitle("Daily Transcation Value Sum Time Series: ANZ Customer Transactions Database") + 
  ylab("Transaction Value Sum") + xlab("Time")


decomp <- mstl(ts(daily_count_ts$count,frequency = 7))
autoplot(decomp) + ggtitle("Daily Transcation Value Sum Decomposition: ANZ Customer Transactions Database") + 
  ylab("Decomposed Transaction Value Sum") + xlab("Time")







location_info <- data.frame(id =c(1:100), location = unique(df_anz$long_lat))

ggplot(location_info, aes(x = id, y = location)) +
  geom_point()
