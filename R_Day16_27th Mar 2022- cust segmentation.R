cust_data <- read.csv("C:/Amol/R/uploads/Train_aBjfeNk.csv",stringsAsFactors = T)
summary(cust_data)
install.packages("readxl")
library(readxl)

cust_data <- read_xlsx("C:/Amol/R/uploads/Train_aBjfeNk.xlsx",sheet = "Train_aBjfeNk" )
summary(cust_data)

prof_blank <- subset(cust_data,Profession == "" )
summary(prof_blank)
 

str(cust_data$Profession)
cust_data$Profession <- as.character(cust_data$Profession)
cust_data$Profession <- ifelse(cust_data$Profession == "",NA,cust_data$Profession)
cust_data$Profession <- as.factor(cust_data$Profession)

names(cust_data)

range1 <- c(3,5,6,10)

for (i in range1) {
  cust_data[,i] <- as.character(cust_data[,i])
  cust_data[,i] <- ifelse(cust_data[,i] == "",NA,cust_data[,i])
  cust_data[,i] <- as.factor(cust_data[,i])
}

summary(cust_data)
install.packages("Amelia")
library(Amelia)
missmap(cust_data)

install.packages("DMwR2")
library(DMwR2)

cust_data <- knnImputation(cust_data) # replace NA
summary(cust_data)


table(cust_data$Segmentation, cust_data$Graduated)
install.packages("ggplot2")
install.packages("plotly")
library(ggplot2)
library(plotly)
ggplotly(
ggplot(cust_data,aes(x = Segmentation, y = Age)) +
  geom_boxplot())

ggplotly(
  ggplot(cust_data,aes(x = Segmentation, y = Work_Experience)) +
    geom_boxplot())

library(caTools)
set.seed(123)
sample <- sample.split(cust_data$Segmentation,SplitRatio = 0.75)
train <- subset(cust_data, sample == T)
test <- subset(cust_data, sample == F)

install.packages("randomForest")
library(randomForest)

rf1 <- randomForest(Segmentation~.,data = train[,-1],mtry = 3, ntree = 200, importance = T)
varImpPlot(rf1)


table(cust_data$Segmentation, cust_data$Spending_Score)


test$pred <- predict(rf1, test, type = 'response')
table(test$Segmentation, test$pred)






cust_data$group_d <- ifelse(cust_data$Segmentation == "D",1,0)

glm(D~.)

data2 <- subset(cust_data, Segmentation != "D")
data2$group_c <- ifelse(data2, Segmentation == "C",1,0)

glm(group_c~ . , data2)

data3 <- subset(cust_data, Segmentation == "A" | Segmentation == "B")

valid <- read.csv("C:/Amol/R/uploads/Test_LqhgPWU.csv")


range1 <- c(3,5,6,10)

for (i in range1) {
  valid[,i] <- as.character(valid[,i])
  valid[,i] <- ifelse(valid[,i] == "",NA,valid[,i])
  valid[,i] <- as.factor(valid[,i])
}

summary(valid)
#install.packages("Amelia")
#library(Amelia)
#missmap(cust_data)

#install.packages("DMwR2")
library(DMwR2)
valid$Gender <- as.factor(valid$Gender)
valid$Spending_Score <- as.factor(valid$Spending_Score)
valid <- knnImputation(valid) # replace NA
summary(valid)


valid$Segmentation <- predict(rf1, valid,type = 'response')
table(valid$Segmentation)

valid_submit <- valid[,c(1,11)]
write.csv(valid_submit,"cust_submit.csv",row.names = F)









