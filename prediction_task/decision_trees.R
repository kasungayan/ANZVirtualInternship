library(rpart)
library(rpart.plot)

customer_correlation <- read.csv("cust-sum.txt", header = TRUE)
customer_correlation_without <- read.csv("cust-sum-without-out.txt", header = TRUE)


prediction_tree <- rpart(Ann_Salary ~ ., data = customer_correlation)


#plot(prediction_tree)
#text(prediction_tree, cex = 0.8, use.n = TRUE, xpd = TRUE)

#prp(prediction_tree, faclen = 0, cex = 0.8, extra = 1)


salary_train <- customer_correlation[1:70, ]
salary_test <- customer_correlation[71:99, ]

prediction_tree <- rpart(Ann_Salary ~ ., data = salary_train)
salary_train_predict <- predict(prediction_tree, salary_test[, -5])

res <- salary_test$Ann_Salary - as.numeric(salary_train_predict)

# Calculate RMSE, assign it to rmse and print it
rmse <- sqrt(mean(res ^ 2))
print(rmse)
#print(rmse)
#[1] 18669.96
prp(prediction_tree, faclen = 0, cex = 0.8, extra = 1)

###################################
salary_train <- customer_correlation_without[1:70, ]
salary_test <- customer_correlation_without[71:96, ]

prediction_tree <- rpart(Ann_Salary ~ ., data = salary_train)
salary_train_predict <- predict(prediction_tree, salary_test[, -5])

res <- salary_test$Ann_Salary - as.numeric(salary_train_predict)

# Calculate RMSE, assign it to rmse and print it
rmse <- sqrt(mean(res ^ 2))
print(rmse)
#print(rmse)
#[1] 18669.96
prp(prediction_tree, faclen = 0, cex = 0.8, extra = 1)
# > print(rmse)
#[1] 15785.71













