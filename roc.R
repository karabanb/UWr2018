library(caret)
library(rpart)

data("GermanCredit")

train <- sample(1:nrow(GermanCredit), nrow(GermanCredit)*0.7)

credit_train <- GermanCredit[train,]
credit_test <- GermanCredit[-train,]

m1 <- rpart(Class ~., credit_train)

m1_precited <- predict(m1, credit_test, type = "prob")
