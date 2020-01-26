library(corrplot)

customer_correlation <- read.csv("cust-sum.txt", header = TRUE)
customer_correlation_without <- read.csv("cust-sum-without-out.txt", header = TRUE)

ggcorr(customer_correlation, 
       label = TRUE, 
       label_alpha = TRUE) + ggtitle("Correlation Analysis: ANZ Transcaction Database")

ggcorr(customer_correlation_without, 
       label = TRUE, 
       label_alpha = TRUE) + ggtitle("Correlation Analysis (Without Outliers): ANZ Transcaction Database")


customer_correlation <- customer_correlation[,c(-1,-2)]
customer_correlation_without <- customer_correlation_without[,c(-1,-2)]

pairs(customer_correlation, col=customer_correlation$Ann_Salary)
pairs(customer_correlation_without, col=customer_correlation$Ann_Salary)


########### Simple Regression #####################################

customer_correlation <- read.csv("cust-sum.txt", header = TRUE)
customer_correlation_without <- read.csv("cust-sum-without-out.txt", header = TRUE)

salary_train <- customer_correlation[1:70, ]
salary_test <- customer_correlation[71:99, ]

salary_model <- lm(Ann_Salary ~ Age + Avg_Purch, salary_train)

salary_train_predict <- predict(salary_model, salary_test[, c(-1, -2)])

res <- salary_test$Ann_Salary - as.numeric(salary_train_predict)

# Calculate RMSE, assign it to rmse and print it
rmse <- sqrt(mean(res ^ 2))
print(rmse)
#> print(rmse)
#[1] 15852.17


##############################################

salary_train <- customer_correlation_without[1:70, ]
salary_test <- customer_correlation_without[71:96, ]

salary_model <- lm(Ann_Salary ~ Age + Avg_Purch, salary_train)

salary_train_predict <- predict(salary_model, salary_test[, c(-1, -2)])

res <- salary_test$Ann_Salary - as.numeric(salary_train_predict)

# Calculate RMSE, assign it to rmse and print it
rmse <- sqrt(mean(res ^ 2))
print(rmse)
#> print(rmse)
#[1] 14072.89





















