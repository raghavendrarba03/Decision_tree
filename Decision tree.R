install.packages("rpat")
library(rpart)
summary(car.test.frame)
car=car.test.frame[,-3]
str(car)
set.seed(123);car_mixed=car[order(runif(60)), ]
car_train=car_mixed[1:45, ]
car_test=car_mixed[46:60, ]
CART_model=rpart(Price~.,data=car_train)
CART_pred=predict(CART_model,car_test[,-1])
mae(car_test[,1],CART_pred)
rplot.plot::rpart.plot(CART_model)
plot(CART_model)
rpart.plot(CART_model)
random=randomForest(car_train[,-1],car_train[,1])
random_pred=predict(random,car_test[,-1])
mae(car_test[,1],random_pred)
