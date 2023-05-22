#Defining libraries
library(neuralnet)
library(ggpubr)
library(ggplot2)

#Read data from csv file
data <- read.csv("new_uowdata.csv", header = TRUE)
head(data)

#Extract values into data frame
esp_data <- as.data.frame(data)
head(esp_data)

boxplot(esp_data[,-1])

#Normalize function
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}
#Normalizing extracted data
norm_data <- as.data.frame(lapply(esp_data[,-1], normalize))
head(norm_data)
summary(norm_data)
boxplot(norm_data)

#Split data for training and testing purposes
train_data <- norm_data[1:380,]
test_data <- norm_data[381:470,]
#View(as.data.frame(train_data))
#View(as.data.frame(test_data))

#Creating the training model
#Matrix for input and vector for output
trainm_in <- matrix(,nrow = 0, ncol = 4)
trainm_out <- c()
#Getting records of input value
for (i in 1:length(train_data$X20.00)) {
  target <- i+3
  
  #Stopping  the loop
  if (target+1 > length(train_data$X20.00)){break}
  
  #Getting new records from splited data for training
  input_dat <- train_data$X20.00[i:target]
  output_dat <- train_data$X20.00[target+1]
  
  trainm_in <- rbind(trainm_in,input_dat)
  trainm_out <- append(trainm_out,output_dat)
}
trainm_in
#Creating the data frame for neural network training
nn_data <- cbind(as.data.frame(trainm_in, trainm_out))
nn_data
neural_network <- neuralnet(trainm_out~V1+V2+V3+V4, data=nn_data,hidden = c(10,2),linear.output = TRUE )
print(neural_network)
plot(neural_network)

#Testing the model
#Creating matrix and vector for data handling
test_in <- matrix(, nrow = 0, ncol = 4)
test_out <- c()#Getting data records and availability
for (i in 1:length(test_data$X20.00)) {
  target1 <- i+3
  if (target1+1 > length(test_data$X20.00)){break}
  #Collecting data
  input_test <- test_data$X20.00[i:target1]
  output_test <- test_data$X20.00[target1+1]
  
  test_in <- rbind(test_in,input_test)
  test_out <- append(test_out,output_test)
  
  
}
test_df <- as.data.frame(test_in)
tst_df <- as.data.frame(test_out)
#Testing the nerural net
neural_network_result <- compute(neural_network,test_df)
result <- data.frame(actual= test_out,prediction=neural_network_result$net.result)
min_result <- min(esp_data$X20.00)
max_result <- max(esp_data$X20.00)

#Denormalizing the data
denorm <- function(x,min,max){
  return((max-min)*x+min)
}
#Comparing NN's data
predict <- denorm(result$prediction,min_result,max_result)
actual <- denorm(result$actual,min_result,max_result)

compare = data.frame(predict,actual)

#print(mean(denorm_data$actual-denorm_data$predict)/denorm_data$actual)
#mean((compare$actual-compare$predict)/compare$actual)
#Deviation for accuracy
dev <- ((compare$actual-compare$predict)/compare$actual)
summary(dev)
mean(dev)
#compare=data.frame(denorm_data$predict,denorm_data$actual,dev)
accuracy = 1 - abs(mean(dev))
accuracy

rmse <- sqrt(mean(predict-actual)^2)
rmse
mae <- mean(abs(compare$predict - compare$actual))
mae
mape <- mean(abs((predict - actual)/actual)) * 100
mape
smape <- function(actual, predict) {
  2 * mean(abs(actual - predict) / (abs(actual) + abs(predict)))
}
smape(actual,predict)

ggplot(compare, aes(x = actual, y = predict)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Actual", y = "Predicted", title = "Predicted vs. Actual Values")