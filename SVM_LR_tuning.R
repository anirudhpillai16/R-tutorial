
data <- read.csv("regression.csv",header = TRUE)
plot(data, pch=16)
# Create a linear regression model
model <- lm(Y ~ X, data)

# Add the fitted line
plot(data, pch=16)
abline(model)

#Make a prediction for each X

predictY <- predict(model,data)

#DISPLAY THE PREDICTIONS
plot(data, pch=16)
points(data$X,predictY, col = "blue",pch = 4)

#Root Mean Squared Error

rmse <- function(error){
  
  sqrt(mean(error^2))
}
error <- model$residuals

rms <- rmse(error)


#SVM
library(e1071)

msvm <- svm(Y~X, data)

predictedY <- predict(msvm, data)

plot(data, pch = 16)
points(data$X, predictedY, col = "red", pch =4)

error <- msvm$residuals
rms <- rmse(error)

#Perform grid search to tune 

tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
plot(tuneResult)

#Looking at the graph best value is between 0 and 0.2

tuneResult2 <- tune(svm, Y~X, data= data,
                    ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9))
)
print(tuneResult2)
plot(tuneResult2)


tunedModel <- tuneResult2$best.model
tunedModelY <- predict(tunedModel, data)

error <- data$Y - tunedModelY

RMSE <- rmse(error)

plot(data, pch = 16)
lines(data$X, predictedY, col = "red", pch =4)
lines(tunedModelY, col = "blue", pch = 16)
