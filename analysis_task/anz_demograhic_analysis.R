library(ggplot2)
library(dplyr)
library(forecast)
library(plotly)


df_anz <- read.csv("ANZdataset.csv", header = TRUE, na.strings=c("","NA"))
df_anz$date <- as.Date(df_anz$date,format = "%d/%m/%Y")


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

p <- demog_df %>% group_by(gender) %>% summarize(Count = n()) %>% ggplot(aes(x=gender, y = Count, fill=gender)) +
  geom_bar(colour="black", width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Gender Distribution: ANZ Customer Transactions Database") + 
  ylab("Percentage") + xlab("Gender Category") + guides(fill=FALSE) + 
  geom_text(aes(gender, Count + 2.0, label = paste(Count, "%")))


GenderDist <- ggplotly(p)

df_anz <- df_anz %>%
  mutate(AgeCategory = case_when(18 <= age & age < 30 ~ '18-30',
                                 30 <= age & age < 50 ~ '30-50',
                                 TRUE ~ '50>'))

q <- df_anz_age %>% group_by(AgeCategory) %>% summarize(Count = n()) %>% ggplot(aes(x=AgeCategory, y = Count, fill=AgeCategory)) +
  geom_bar(colour="black", width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Age Distribution: ANZ Customer Transactions Database") + 
  ylab("No of Customers") + xlab("Gender Category") + guides(fill=FALSE) + 
  geom_text(aes(AgeCategory, Count + 2.0, label = Count))

AgeDist <- ggplotly(q)

a <- df_anz_age %>% group_by(gender,AgeCategory) %>% summarize(Count = n())
ggplot(a) +
  facet_wrap(~gender) +
  geom_bar(aes(y = Count, x = AgeCategory, fill = Count), stat = "identity") +
  theme() +
  coord_polar("x", start = 0) +  ggtitle("Age Distribution: ANZ Customer Transactions Database") + 
  ylab("Age of Customers") + xlab("Gender Distribution")

############ Transcations ####################################

df_trans <- 
  group_by(df_anz, customer_id) %>% summarise(Count = round(n()))
boxplot(df_trans$Count, horizontal = TRUE, staplewex = 1,
        main= "Customer Transcation Count: ANZ Customer Transactions Database",  xlab = "Transcation Count")
text(x=fivenum(df_trans$Count),labels = fivenum(df_trans$Count), y=1.25)
# Mean: 120.43 


gender_transcations <- df_anz %>% group_by(gender) %>% summarize(Count = n()) %>% 
  ggplot(aes(x=gender, y = Count, fill=gender)) +
  geom_bar(colour="black", width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Transcation Count Summary: Genderwise") + 
  ylab("Transcation Count") + xlab("Gender Category") + guides(fill=FALSE) + 
  geom_text(aes(gender, Count + 0.5, label = Count))


age_transcations <- df_anz %>% group_by(AgeCategory) %>% summarize(Count = n()) %>% 
  ggplot(aes(x=AgeCategory, y = Count, fill=AgeCategory)) +
  geom_bar(colour="black", width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Transcation Count Summary: AgeWise") + 
  ylab("Transcation Count") + xlab("Age Category") + guides(fill=FALSE) + 
  geom_text(aes(AgeCategory, Count + 2, label = Count))


gender_transcations_value <- df_anz %>% group_by(gender) %>% summarize(Expenditure = round(mean(amount))) %>% 
  ggplot(aes(x=gender, y = Expenditure, fill=gender)) +
  geom_bar(colour="black", width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Expenditure Summary: Genderwise") + 
  ylab("Expenditure Amount") + xlab("Gender Category") + guides(fill=FALSE) + 
  geom_text(aes(gender, Expenditure + 0.5, label = Expenditure))

Age_transcations_value <- df_anz %>% group_by(AgeCategory) %>% summarize(Expenditure = round(mean(amount))) %>% 
  ggplot(aes(x=AgeCategory, y = Expenditure, fill=AgeCategory)) +
  geom_bar(colour="black", width = 0.3, stat="identity") + coord_flip() + 
  ggtitle("Expenditure Summary: Agewise") + 
  ylab("Expenditure Amount") + xlab("Age Category") + guides(fill=FALSE) + 
  geom_text(aes(AgeCategory, Expenditure + 0.5, label = Expenditure))


###################################################################


df_anz <- read.csv("ANZdataset.csv", header = TRUE, na.strings=c("","NA"))
df_anz$date <- as.Date(df_anz$date,format = "%d/%m/%Y")

daily_count_ts <- group_by(df_anz,date)%>% summarize(count= n())
dailyts <- daily_count_ts$count
weeklycountts <- colSums(matrix(dailyts,7))
#mean : 926.3846
#median : 924

autoplot(ts(weeklycountts)) + ggtitle("Weekly Transcation Volume: ANZ Customer Transactions Database") + 
  ylab("Transaction Count") + xlab("Week")



daily_count_ts <- group_by(df_anz,date)%>% summarize(count= round(mean(amount)))
dailyts <- daily_count_ts$count
weeklycountts <- colSums(matrix(dailyts,7))
#mean : 1334.154
#median : 1355

autoplot(ts(weeklycountts)) + ggtitle("Weekly Transcation Value: ANZ Customer Transactions Database") + 
  ylab("Transaction Value") + xlab("Week")









