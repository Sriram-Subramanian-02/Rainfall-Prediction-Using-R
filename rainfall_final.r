# reading the dataset
df = read.csv("C:\\Users\\srira\\Desktop\\ads\\Logistic_Regression\\weatherAUS.csv",header=TRUE)

# viewing the dataset
head(df)
dimnames(df)
View(df)

summary(df)

# dropping unnecessary columns
drop <- c("RainToday","RainTomorrow")

df_ = df[,!(names(df) %in% drop)]

df

#required libraries

library(caret)
library(klaR)
library(caTools)

mean(df$Evaporation, na.rm = TRUE)


class(df$Date)
df$Date <- as.Date(df$Date)

class(df$Date)


train_df = df[df$Date < "2016-01-01", ]

# dimnames(train_df)
View(train_df)

# val_df = df[df$Date < "2016-01-01" & df$Date > "2014-12-31", ]

test_df = df[df$Date >= "2016-01-01", ]
nrow(test_df)
nrow(train_df)

View(test_df)

colnames(df)


input_cols = colnames(df)[1:length(colnames(df))-1]
length(input_cols)

target_col = 'RainTomorrow'


train_inputs = train_df[input_cols]
train_targets = train_df[target_col]


test_inputs = test_df[input_cols]
test_targets =test_df[target_col]

View(train_targets)
View(train_inputs)



library(data.table)
library("dplyr")

numeric_cols = select_if(train_inputs,is.numeric)
head(numeric_cols)
numeric_cols = colnames(numeric_cols)
numeric_cols


categorical_cols = c("Location","WindGustDir","WindDir3pm","WindDir9am","RainToday")
length(numeric_cols)+length(categorical_cols)

summary(train_inputs[,c(numeric_cols)])

head(train_inputs[,c(numeric_cols)])



for(i in 1:ncol(train_inputs))
{
    train_inputs[ , i][is.na(train_inputs[ , i])] <- mean(train_inputs[ , i], na.rm = TRUE)
    # val_inputs[ , i][is.na(val_inputs[ , i])] <- mean(val_inputs[ , i], na.rm = TRUE)
    test_inputs[ , i][is.na(test_inputs[ , i])] <- mean(test_inputs[ , i], na.rm = TRUE)
}

View(train_inputs)

sum(is.na(train_inputs$Evaporation))


library(plyr)

mean(train_inputs$Evaporation)
sd(train_inputs$Evaporation)
nrow(train_inputs)

View(train_inputs)


range01 <- function(x){(x-min(x))/(max(x)-min(x))}


for(i in numeric_cols)
{
    train_inputs[,i] = range01(train_inputs[,i])
    # val_inputs[,i] = range01(val_inputs[,i])
    test_inputs[,i] = range01(test_inputs[,i])
}

# install.packages("mltools")
library(mltools)

one_hot_encoding = function(df,columns){
  # create a copy of the original data.frame for not modifying the original
  df = cbind(df)
  # convert the columns to vector in case it is a string
  columns = c(columns)
  # for each variable perform the One hot encoding
  for (column in columns){
    unique_values = sort(unique(df[column])[,column])
    non_reference_values  = unique_values[c(-1)] # the first element is going 
                                                 # to be the reference by default
    for (value in non_reference_values){
      # the new dummy column name
      new_col_name = paste0(column,'_',value)
      # create new dummy column for each value of the non_reference_values
      df[new_col_name] <- with(df, ifelse(df[,column] == value, 1, 0))
    }
    # delete the one hot encoded column
    df[column] = NULL

  }
  return(df)
}

train_inputs = one_hot_encoding(train_inputs,c(categorical_cols))
# val_inputs = one_hot_encoding(val_inputs,c(categorical_cols))
test_inputs = one_hot_encoding(test_inputs,c(categorical_cols))


colnames(train_inputs)
# View(train_inputs)

unique(df[,"Location"])

# head(train_inputs$RainToday_Yes)

colnames(train_inputs)

train_inputs$Date = NULL
# val_inputs$Date = NULL
test_inputs$Date = NULL

str(train_inputs)

train_targets = one_hot_encoding(train_targets,c("RainTomorrow"))
# val_targets = one_hot_encoding(val_targets,c("RainTomorrow"))
test_targets = one_hot_encoding(test_targets,c("RainTomorrow"))


head(train_targets)
sum(train_targets$RainTomorrow_Yes)

View(train_targets)

sum(is.na(train_targets$RainTomorrow_Yes))

train_targets[is.na(train_targets)] = 0
test_targets[is.na(test_targets)] = 0
# val_targets[is.na(val_targets)] = 0
# val_inputs[is.na(val_inputs)] = 0
test_inputs[is.na(test_inputs)] = 0
sum(is.na(train_targets$RainTomorrow_Yes))
sum(is.na(test_targets$RainTomorrow_Yes))
# sum(is.na(val_targets$RainTomorrow_Yes))




colnames(train_inputs)
train_inputs$RainTomorrow_Yes = train_targets$RainTomorrow_Yes
# val_inputs$RainTomorrow_Yes = val_targets$RainTomorrow_Yes
test_inputs$RainTomorrow_Yes = test_targets$RainTomorrow_Yes



sum(is.na(train_inputs[,c(colnames(train_inputs))]))
sum(is.na(train_inputs$Location_Albany))
summary(train_inputs)
train_inputs[is.na(train_inputs)] = 0
test_inputs[is.na(test_inputs)] = 0


logistic <- glm(train_inputs$RainTomorrow_Yes ~ train_inputs$RainToday_Yes,data=train_inputs,family="binomial")
logistic
summary(logistic)






logistic <- glm(train_inputs$RainTomorrow_Yes ~ . ,data=train_inputs,family="binomial")
summary(logistic)
length(logistic$fitted.values)
length(train_inputs$RainTomorrow_Yes)

predicted_data = data.frame(
  probability_of_rain_tmw = logistic$fitted.values,
  rain_tmw = train_inputs$RainTomorrow_Yes
)


predicted_data = predicted_data[order(predicted_data$probability_of_rain_tmw,decreasing=FALSE),]
predicted_data$rank = 1:nrow(predicted_data)

library(ggplot2)
install.packages("cowplot")
library(cowplot)

ggplot(data=predicted_data,aes(x=rank,y=probability_of_rain_tmw)) + 
geom_point(aes(color=train_inputs$RainTomorrow_Yes),alpha=1,shape=4,stroke=2) +
xlab("Index") +
ylab("Predicted probability of getting rain")


head(predicted_data)
View(predicted_data)


predict_reg <- predict(logistic, val_inputs, type = "response")
predict_reg  
   
# Changing probabilities
predict_reg <- ifelse(predict_reg >0.5, 1, 0)

table(val_inputs$RainTomorrow_Yes, predict_reg)

missing_classerr <- mean(predict_reg != val_inputs$RainTomorrow_Yes)

print(paste('Accuracy =', 1 - missing_classerr))
