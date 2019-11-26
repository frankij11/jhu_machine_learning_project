library(tidyverse)
library(randomForest)
library(caret)

training_url <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(!dir.exists("data")){dir.create("data")}
if(!file.exists("data/training.csv")){download.file(training_url, "data/training.csv")}
if(!file.exists("data/test.csv")){download.file(test_url, "data/test.csv")}

raw_training = read.csv("data/training.csv", na.strings=c("NA","#DIV/0!",""))
raw_test = read.csv("data/test.csv", na.strings=c("NA","#DIV/0!",""))


#drop columns with too many na's
na_cols <- sapply(1:length(names(raw_training)), function(i) sum(is.na(raw_training[,i]))/ nrow(raw_training)) >.9
training <- raw_training[,names(raw_training)[!na_cols]]
validation <- raw_test[, names(raw_test)[!na_cols]]
#drop columns that should not have predictive power
bad_cols <- c("X", "user_name")
time_cols <- training %>% select(contains("time")) %>% names()
bad_cols <- c(bad_cols, time_cols)

training <- select(training, -bad_cols)

dt <- NULL
for(i in 1:ncol(training)){
  tmp_df = data.frame("col" = names(training)[i], 
                      "num" = training[,i] %>% is.numeric()
  )
  if(is.null(dt)){dt <- tmp_df }else{
    
    dt <- rbind(dt, tmp_df)
  }
}

num_cols <- dt$col[dt$num==TRUE]
df <- data.frame(cor(as.numeric(training$classe), training[, num_cols]) %>% t())
names(df) <- c("corr")

training_sample <- createDataPartition(training$classe, p=.5, list=F)
testing <- training[-training_sample,]
training <- training[training_sample,]


set.seed(123)
mod.tree <- rpart(classe~., data=training, method="class")
pred.tree <- data.frame("Prediction" = predict(mod.tree, raw_training, type="class"), "Truth" = raw_training$classe)
pred.tree$Train <- "train"
pred.tree$Train[-training_sample] <- "test"
pred.tree$accuracy <- pred.tree$Prediction == pred.tree$Truth
print("rpart tree")
pred.tree %>% group_by(Train) %>% summarise(sum(accuracy)/n()) %>% print()

set.seed(123)
mod.rf <- randomForest(classe ~ ., data=training, ntree=100, importance=T)


# prediction table
pred <- data.frame("Prediction" = predict(mod.rf, raw_training), "Truth" = raw_training$classe)
pred$Train <- "train"
pred$Train[-training_sample] <- "test"
pred$accuracy <- pred$Prediction == pred$Truth
#test prediction table
print("random forest")
pred %>% group_by(Train) %>% summarise(sum(accuracy)/n()) %>% print()

validation$classe <- ""
validation <- select(validation,names(training))
validation <- rbind(training[1, ] , validation)
validation <- validation[-1,]
row.names(validation) <- NULL

predict(mod.rf, validation) %>% print()

