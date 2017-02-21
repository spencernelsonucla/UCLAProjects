test234 <- read.csv("C:/Users/spenc/Dropbox/UCLA/Spring 2016 Classes/Stats 101C/Final Project/finaltest.csv", stringsAsFactors=FALSE)

testinv <- test234
row.names(testinv) <- testinv$ARN
othervals <- testinv[is.na(testinv$Outcome.Date),]
inventorymatrix <- c((rep(0,350)), (rep(0,350)), rep(1,350))
inventorymatrix <- matrix(inventorymatrix,ncol=3)
row.names(inventorymatrix) <- row.names(othervals)
# row.names(testslit) <- testslit$ARN

library(ggplot2)
library(rpart)
library(nnet)
testslit <- read.csv("C:/Users/spenc/Downloads/testslit.csv", stringsAsFactors=FALSE)
train1 <- read.csv("C:/Users/spenc/Downloads/trainslit2.csv", stringsAsFactors=FALSE)
train1 <- train1[,-c(1)]

#tree example
set.seed(123012)
samp <- sample(dim(train1)[1],10000,replace=FALSE)
train.short <- train1[samp,]
rtree.shelter <- rpart(troutcome~ ., data=train1, method="class")
plotcp(rtree.shelter)
printcp(rtree.shelter)
plotcp(rtree.shelter, minline = TRUE, lty = 3, col = 1)
summary(rtree.shelter)  ## too much information

plot(rtree.shelter, margin=0.2, uniform=TRUE)
text(rtree.shelter, use.n=TRUE, cex=0.6)

rtree.shelter <- rpart(troutcome ~ . , data=train1, 
                       method="class", 
                       control=rpart.control(minsplit=30, cp=0.01))
plot(rtree.shelter, margin=0.2, uniform=TRUE)
text(rtree.shelter, use.n=TRUE, cex=0.8)
summary(rtree.shelter)

printcp(rtree.shelter)
plotcp(rtree.shelter, minline = TRUE, lty=3, col=1)

prune.rtree.shelter <- prune(rtree.shelter, 
                             cp=rtree.shelter$cptable[which.min(rtree.shelter$cptable[,"xerror"]),
                                                      "CP"])
plot(prune.rtree.shelter,uniform=TRUE,margin=0.2)
text(prune.rtree.shelter,  all=TRUE, cex=.8)

#### Classification Table ####

rtree.pred <- predict(prune.rtree.shelter,train1,type="class")
table(rtree.pred, train1$troutcome)
prop.table(table(rtree.pred, train1$troutcome), 2)

rtree.pred <- predict(rtree.shelter,test,type="class")
table(rtree.pred,train1$troutcome)
prop.table(table(rtree.pred,train1$troutcome),2)


submission <- data.frame(test$ARN,rtree.pred)
names(submission)[1] <- "ARN"
write.csv(submission, file="submission.csv", row.names=FALSE)

#### Random Forest ####
library(randomForest)
set.seed(8888)
train1$Species <- as.factor(train1$Species)
train1$Sex <- as.factor(train1$Sex)
train1$Intake.Type <- as.factor(train1$Intake.Type)
train1$Shelter <- as.factor(train1$Shelter)
train1$Omonth <- as.factor(train1$Omonth)
train1$Oyear <- as.factor(train1$Oyear)
train1$Odow <- as.factor(train1$Odow)
train1$duration <- as.factor(train1$duration)
train1$breedya <- as.factor(train1$breedya)
train1$nameya <- as.factor(train1$nameya)
train1$duration <- as.numeric(train1$duration)
train1$troutcome <- as.factor(train1$troutcome)

testslit$Species <- as.factor(testslit$Species)
testslit$Sex <- as.factor(testslit$Sex)
testslit$Intake.Type <- as.factor(testslit$Intake.Type)
testslit$Shelter <- as.factor(testslit$Shelter)
testslit$Omonth <- as.factor(testslit$Omonth)
testslit$Oyear <- as.factor(testslit$Oyear)
testslit$Odow <- as.factor(testslit$Odow)
testslit$duration <- as.factor(testslit$duration)
testslit$breedya <- as.factor(testslit$breedya)
testslit$nameya <- as.factor(testslit$nameya)
testslit$duration <- as.numeric(testslit$duration)
test234 <- read.csv("C:/Users/spenc/Dropbox/UCLA/Spring 2016 Classes/Stats 101C/Final Project/finaltest.csv", stringsAsFactors=FALSE)

testinv <- test234
row.names(testinv) <- testinv$ARN
othervals <- testinv[is.na(testinv$Outcome.Date),]
inventorymatrix <- c((rep(0,350)), (rep(0,350)), rep(1,350))
inventorymatrix <- matrix(inventorymatrix,ncol=3)
row.names(inventorymatrix) <- row.names(othervals)
row.names(testslit) <- testslit$ARN

samp <- sample(dim(train1)[1],10000,replace=FALSE)
train.short <- train1[samp,]
train.rest <- train1[-samp,]

RF.shelter <- randomForest(troutcome ~ ., data=train.short, mtry=3, nodesize=1, 
                           ntree=500, importance=TRUE)

RF <- predict(RF.shelter,testslit,type="prob")

RF2 <- rbind(RF, inventorymatrix)
RF2 <- as.data.frame(RF2)
RF2$ARN <- row.names(RF2)
submission <- RF2[c("ARN", "ADOPTION", "EUTHANASIA", "OTHER")]
names(submission)[1] <- "ARN"
write.csv(submission, file="submission2.csv", row.names=FALSE)

#Running on full train data
RF.shelter2 <- randomForest(troutcome ~ ., data=train1, mtry=3, nodesize=1, 
                           ntree=500, importance=TRUE)

RF_new <- predict(RF.shelter2,testslit,type="prob")

RF3 <- rbind(RF_new, inventorymatrix)
RF3 <- as.data.frame(RF3)
RF3$ARN <- row.names(RF3)
submission_new <- RF3[c("ARN", "ADOPTION", "EUTHANASIA", "OTHER")]
names(submission_new)[1] <- "ARN"
write.csv(submission_new, file="submission3.csv", row.names=FALSE)


importance(RF.shelter)
varImpPlot(RF.shelter)
RF2 <- rbind(RF, inventorymatrix)
#train1 <- train1[,-c(12)]
samp <- sample(dim(train1)[1],10000,replace=FALSE)

set.seed(12345)
samp <- sample(dim(train1)[1],10000,replace=FALSE)
train.short <- train1[samp,]

set.seed(8888)
bag.shelter <- randomForest(troutcome ~ ., data=train.short, mtry=7, nodesize=1, 
                            ntree=500, importance=TRUE)
importance(bag.shelter)
## variable importance
varImpPlot(bag.shelter)

plot(RF.shelter, ylim=c(0,1))
legend('topright', colnames(RF.shelter$err.rate), col=1:4, fill=1:4)

yhat.RF1 <- predict(RF.shelter ,newdata=train.short)
table(yhat.RF1, train.short$troutcome)
prop.table(table(yhat.RF1, train.short$troutcome),2)

#### Work on 6/24/16 ####

#boosting
library(xgboost)
train2 <- train1

train2$Species <- as.numeric(train2$Species)
train2$Sex <- as.numeric(train2$Sex)
train2$Intake.Type <- as.numeric(train2$Intake.Type)
train2$Shelter <- as.numeric(train2$Shelter)
train2$Omonth <- as.numeric(train2$Omonth)
train2$Oyear <- as.numeric(train2$Oyear)
train2$Odow <- as.numeric(train2$Odow)
train2$duration <- as.numeric(train2$duration)
train2$breedya <- as.numeric(train2$breedya)
train2$nameya <- as.numeric(train2$nameya)
train2$duration <- as.numeric(train1$duration)
train2$troutcome <- as.numeric(train2$troutcome)
train2$Species <- ifelse(train2$Species==2, 1, 0)
train2$breedya <- ifelse(train2$breedya==2,1,0)
train2$nameya <- ifelse(train2$nameya==2,1,0)
train2$troutcome <- train2$troutcome - 1
train2$Sex <- train2$Sex -1
train2$Intake.Type <- train2$Intake.Type - 1
train2$Shelter <- train2$Shelter - 1
train2$Omonth <- train2$Omonth - 1
train2$Oyear <- train2$Oyear - 1
train2$Odow <- train2$Odow - 1
train2$duration <- train2$duration - 1

# do the same to the test data
test2 <- testslit
test2$Species <- as.numeric(test2$Species)
test2$Sex <- as.numeric(test2$Sex)
test2$Intake.Type <- as.numeric(test2$Intake.Type)
test2$Shelter <- as.numeric(test2$Shelter)
test2$Omonth <- as.numeric(test2$Omonth)
test2$Oyear <- as.numeric(test2$Oyear)
test2$Odow <- as.numeric(test2$Odow)
test2$duration <- as.numeric(test2$duration)
test2$breedya <- as.numeric(test2$breedya)
test2$nameya <- as.numeric(test2$nameya)
test2$Species <- ifelse(test2$Species==2, 1, 0)
test2$breedya <- ifelse(test2$breedya==2,1,0)
test2$nameya <- ifelse(test2$nameya==2,1,0)
test2$Sex <- test2$Sex -1
test2$Intake.Type <- test2$Intake.Type - 1
test2$Shelter <- test2$Shelter - 1
test2$Omonth <- test2$Omonth - 1
test2$Oyear <- test2$Oyear - 1
test2$Odow <- test2$Odow - 1
test2$duration <- test2$duration - 1

bstDense <- xgboost(data = as.matrix(train2[,-1]), label = train2[,1], missing=NaN, max.depth = 6, eta = .4, nround = 1000, num_class=3, verbose=0, objective = "multi:softprob") 
test2 <- test2[,-c(1)]

row.names(test2) <- test2$ARN

test2 <- test2[,c("Species", "Sex", "Intake.Type", "Shelter", "Omonth", "Oyear", "Odow", "duration", "breedya", "nameya")]
xgtest <- as.matrix(test2)
pred <- predict(bstDense ,xgtest) 
mypred <- data.frame(testslit$ARN,matrix(pred, ncol=3, byrow=TRUE))
names(mypred) <- c("ARN", "ADOPTION","EUTHANASIA","OTHER")
inventorymatrix <- as.data.frame(inventorymatrix)
inventorymatrix$ARN <- row.names(inventorymatrix)
names(inventorymatrix) <- c("ADOPTION","EUTHANASIA","OTHER","ARN")
inventorymatrix <- inventorymatrix[c("ARN","ADOPTION","EUTHANASIA","OTHER")]

RF4 <- rbind(mypred, inventorymatrix)
RF4 <- as.data.frame(RF4)
#RF4$ARN <- row.names(RF4)
submission_Tuesday3 <- RF4[c("ARN", "ADOPTION", "EUTHANASIA", "OTHER")]
#names(submission_Tuesday)[1] <- "ARN"
write.csv(submission_Tuesday3, file="submission_Tuesday3.csv", row.names=FALSE)

#### Work on 5/25/16: Boosting using xgboost including the new vars and using dummy vars ####

#read the data set in
trainslit4 <- read.csv("C:/Users/spenc/Downloads/trainslit4.csv", stringsAsFactors=FALSE)
trainslit4 <- trainslit4[,-c(1)]
#first repeat boosting using new data and compare results
library(xgboost)


trainslit4$Species <- as.numeric(as.factor(trainslit4$Species))
trainslit4$Sex <- as.numeric(as.factor(trainslit4$Sex))
trainslit4$Intake.Type <- as.numeric(as.factor(trainslit4$Intake.Type))
trainslit4$Shelter <- as.numeric(as.factor(trainslit4$Shelter))
trainslit4$Omonth <- as.numeric(as.factor(trainslit4$Omonth))
trainslit4$Oyear <- as.numeric(as.factor(trainslit4$Oyear))
trainslit4$Odow <- as.numeric(as.factor(trainslit4$Odow))
trainslit4$breedya <- as.numeric(as.factor(trainslit4$breedya))
trainslit4$nameya <- as.numeric(as.factor(trainslit4$nameya))
trainslit4$troutcome <- as.numeric(as.factor(trainslit4$troutcome))
trainslit4$ageya <- as.numeric(as.factor(trainslit4$ageya))
trainslit4$Species <- ifelse(trainslit4$Species==2, 1, 0)
trainslit4$breedya <- ifelse(trainslit4$breedya==2,1,0)
trainslit4$nameya <- ifelse(trainslit4$nameya==2,1,0)
trainslit4$troutcome <- trainslit4$troutcome - 1
trainslit4$Sex <- trainslit4$Sex -1
trainslit4$Intake.Type <- trainslit4$Intake.Type - 1
trainslit4$Shelter <- trainslit4$Shelter - 1
trainslit4$Omonth <- trainslit4$Omonth - 1
trainslit4$Oyear <- trainslit4$Oyear - 1
trainslit4$Odow <- trainslit4$Odow - 1
trainslit4$ageya <- trainslit4$ageya - 1




bstDense1 <- xgboost(data = as.matrix(trainslit4[,-1]), label = trainslit4[,1], missing=NaN, max.depth = 6, eta = .3, nround = 1000, num_class=3, verbose=0, objective = "multi:softprob") 
test3 <- read.csv("C:/Users/spenc/Downloads/extratest2(nona).csv")
test3 <- test3[complete.cases(test3$duration),]
test3 <- test3[,c("Species","Sex","Intake.Type","Shelter","Omonth","Oyear","Odow","duration","breedya","nameya","Age","ageya")]
# do the same to the test data

test3$Species <- as.numeric(test3$Species)
test3$Sex <- as.numeric(test3$Sex)
test3$Intake.Type <- as.numeric(test3$Intake.Type)
test3$Shelter <- as.numeric(test3$Shelter)
test3$Omonth <- as.numeric(test3$Omonth)
test3$Oyear <- as.numeric(test3$Oyear)
test3$Odow <- as.numeric(test3$Odow)
test3$breedya <- as.numeric(test3$breedya)
test3$nameya <- as.numeric(test3$nameya)
test3$ageya <- as.numeric(test3$ageya)
test3$Species <- ifelse(test3$Species==2, 1, 0)
test3$breedya <- ifelse(test3$breedya==2,1,0)
test3$nameya <- ifelse(test3$nameya==2,1,0)
test3$ageya <- ifelse(test3$ageya==2,1,0)
test3$Sex <- test3$Sex -1
test3$Intake.Type <- test3$Intake.Type - 1
test3$Shelter <- test3$Shelter - 1
test3$Omonth <- test3$Omonth - 1
test3$Oyear <- test3$Oyear - 1
test3$Odow <- test3$Odow - 1



#row.names(test3) <- test3$ARN
xgtest1 <- xgb.DMatrix(as.matrix(test3), missing=NaN)
pred1 <- predict(bstDense1 ,xgtest1) 
mypred1 <- data.frame(as.character(testslit$ARN),matrix(pred1, ncol=3, byrow=TRUE))
names(mypred1) <- c("ARN", "ADOPTION","EUTHANASIA","OTHER")
inventorymatrix <- as.data.frame(inventorymatrix)
inventorymatrix$ARN <- row.names(inventorymatrix)
names(inventorymatrix) <- c("ADOPTION","EUTHANASIA","OTHER","ARN")
inventorymatrix <- inventorymatrix[c("ARN","ADOPTION","EUTHANASIA","OTHER")]

RF5 <- rbind(mypred1, inventorymatrix)
RF5 <- as.data.frame(RF5)
#RF4$ARN <- row.names(RF4)

write.csv(RF5, file="submission_Thurs_wo_dummy.csv", row.names=FALSE)

#### Dummy vars and new data, this works ####

library(caret)
#ohe_feats = c("Sex","Intake.Type","Shelter","Omonth","Oyear","Odow")
library(psych)
trainslit4 <- read.csv("C:/Users/spenc/Downloads/trainslit4.csv", stringsAsFactors=FALSE)
trainslit4 <- trainslit4[,-c(1)]
trainer <- trainslit4
trainer$troutcome <- as.factor(trainer$troutcome)
trainer$Species <- as.factor(trainer$Species)
trainer$Sex <- as.factor(trainer$Sex)
trainer$Intake.Type <- as.factor(trainer$Intake.Type)
trainer$Shelter <- as.factor(trainer$Shelter)
trainer$Omonth <- as.factor(trainer$Omonth)
trainer$Oyear <- as.factor(trainer$Oyear)
trainer$Odow <- as.factor(trainer$Odow)
trainer$breedya <- as.factor(trainer$breedya)
trainer$ageya <- as.factor(trainer$ageya)
trainer$nameya <- as.factor(trainer$nameya)

trainer$Species <- ifelse(trainer$Species== "DOG", 1, 0)
trainer$breedya <- ifelse(trainer$breedya=="Yes",1,0)
trainer$nameya <- ifelse(trainer$nameya=="Yes",1,0)
trainer$ageya <- ifelse(trainer$ageya=="Yes",1,0)



dSex <- dummy.code(trainer$Sex)
dIntake.Type <- dummy.code(trainer$Intake.Type)
dShelter <- dummy.code(trainer$Shelter)
dOmonth <- dummy.code(trainer$Omonth)
dOyear <- dummy.code(trainer$Oyear)
dOdow <- dummy.code(trainer$Odow)

trainer2 <- data.frame(trainer$troutcome, trainer$Species, dSex, dIntake.Type, dShelter, dOmonth, dOyear, dOdow, trainer$duration, trainer$breedya, trainer$nameya, trainer$Age, trainer$ageya)
#t1_all_ohe <- as.data.frame(predict(dummies, newdata = trainslit4))
#t1_all_combined <- cbind(trainslit4[,-c(which(colnames(trainslit4) %in% ohe_feats))],t1_all_ohe)
#t1_all_combined$troutcome <- as.numeric(as.factor(t1_all_combined$troutcome))
#t1_all_combined$troutcome <- t1_all_combined$troutcome - 1
trainer2$trainer.troutcome <- as.numeric(as.factor(trainer2$trainer.troutcome)) - 1
trainer2$trainer.duration <- as.numeric(trainer2$trainer.duration)
trainer2 <- trainer2[,-c(9,8,10,24:35, 13)]

bstDense2 <- xgboost(data = as.matrix(trainer2[,-1]), 
                    label =trainer2[,1], 
                    missing=NaN, 
                    max.depth = 6, 
                    eta = .3, 
                    nround = 1000, 
                    num_class=3, 
                    verbose=0, 
                    objective = "multi:softprob") 

# do the same for the test data

test3 <- read.csv("C:/Users/spenc/Downloads/extratest2(nona).csv")
test3 <- test3[complete.cases(test3$duration),]
test3 <- test3[,c("ARN", "Species","Sex","Intake.Type","Shelter","Omonth","Oyear","Odow","duration","breedya","nameya","Age","ageya")]

#test3$troutcome <- as.factor(test3$troutcome)
test3$Species <- as.factor(test3$Species)
test3$Sex <- as.factor(test3$Sex)
test3$Intake.Type <- as.factor(test3$Intake.Type)
test3$Shelter <- as.factor(test3$Shelter)
test3$Omonth <- as.factor(test3$Omonth)
test3$Oyear <- as.factor(test3$Oyear)
test3$Odow <- as.factor(test3$Odow)
test3$breedya <- as.factor(test3$breedya)
test3$ageya <- as.factor(test3$ageya)
test3$nameya <- as.factor(test3$nameya)

test3$Species <- ifelse(test3$Species== "DOG", 1, 0)
test3$breedya <- ifelse(test3$breedya=="Yes",1,0)
test3$nameya <- ifelse(test3$nameya=="Yes",1,0)
test3$ageya <- ifelse(test3$ageya=="Yes",1,0)



dSex <- dummy.code(test3$Sex)
dIntake.Type <- dummy.code(test3$Intake.Type)
dShelter <- dummy.code(test3$Shelter)
dOmonth <- dummy.code(test3$Omonth)
dOyear <- dummy.code(test3$Oyear)
dOdow <- dummy.code(test3$Odow)

tester <- data.frame(test3$ARN, test3$Species, dSex, dIntake.Type, dShelter, dOmonth, dOyear, dOdow, test3$duration, test3$breedya, test3$nameya, test3$Age, test3$ageya)
tester <- tester[,-c(9,8,10,24:35, 13)]
#tester <- tester[,-c("DISASTER", "ACTF", "EVIDENCE", "X2", "PERS.PROP", "X11", "X4", "X12", "X5", "X6", "X9", "X1", "X3", "X10", "X8", "X7")]

xgtest <- xgb.DMatrix(as.matrix(tester[,-c(1)]), missing=NaN)
xgb.importance(colnames(trainer2[,-1]), model = bstDense2)

pred <- predict(bstDense2 ,xgtest) 
mypred <- data.frame(tester$test3.ARN, matrix(pred, ncol=3, byrow=TRUE))
names(mypred) <- c("ARN", "ADOPTION","EUTHANASIA","OTHER")
inventorymatrix <- as.data.frame(inventorymatrix)
inventorymatrix$ARN <- row.names(inventorymatrix)
names(inventorymatrix) <- c("ADOPTION","EUTHANASIA","OTHER","ARN")
inventorymatrix <- inventorymatrix[c("ARN","ADOPTION","EUTHANASIA","OTHER")]

RF8 <- rbind(mypred, inventorymatrix)
RF8 <- as.data.frame(RF8)
#RF4$ARN <- row.names(RF4)

write.csv(RF8, file="submission_5.30_new.csv", row.names=FALSE)


#############
test3 <- read.csv("C:/Users/spenc/Downloads/extratest2(nona).csv")
test3 <- test3[complete.cases(test3$duration),]
test3 <- test3[,c("Species","Sex","Intake.Type","Shelter","Omonth","Oyear","Odow","duration","breedya","nameya","Age","ageya")]

dummies <- dummyVars(~ Sex + Intake.Type + Shelter + Omonth + Oyear + Odow, data = test3) 
t1_all_ohe <- as.data.frame(predict(dummies, newdata = test3))
t1_all_combined2 <- cbind(test3[,-c(which(colnames(test3) %in% ohe_feats))],t1_all_ohe)
t1_all_combined2$Species <- as.numeric(t1_all_combined2$Species)
t1_all_combined2$breedya <- as.numeric(t1_all_combined2$breedya)
t1_all_combined2$nameya <- as.numeric(t1_all_combined2$nameya)
t1_all_combined2$ageya <- as.numeric(t1_all_combined2$ageya)


xgtest <- xgb.DMatrix(as.matrix(t1_all_combined2), missing=NaN)
pred <- predict(bstDense ,xgtest) 
mypred <- data.frame(testslit$ARN,matrix(pred, ncol=3, byrow=TRUE))
names(mypred) <- c("ARN", "ADOPTION","EUTHANASIA","OTHER")
inventorymatrix <- as.data.frame(inventorymatrix)
inventorymatrix$ARN <- row.names(inventorymatrix)
names(inventorymatrix) <- c("ADOPTION","EUTHANASIA","OTHER","ARN")
inventorymatrix <- inventorymatrix[c("ARN","ADOPTION","EUTHANASIA","OTHER")]

RF6 <- rbind(mypred, inventorymatrix)
RF6 <- as.data.frame(RF6)
#RF4$ARN <- row.names(RF4)

write.csv(RF6, file="submission_Thurs.csv", row.names=FALSE)

#using cross validation
bst <- xgb.cv(data = as.matrix(t1_all_combined[,-1]), label = t1_all_combined[,1], missing=NaN, nfold = 5, prediction=TRUE, max.depth=3, eta = 0.1, verbose=0, nrounds = 100, objective = "multi:softprob", num_class=3, early.stop.round = 2, maximize = FALSE)
#get best iteration is 5

#### Trying again - new dataset, no dummies ####

library(caret)
#ohe_feats = c("Sex","Intake.Type","Shelter","Omonth","Oyear","Odow")
library(psych)
trainslit5 <- read.csv("C:/Users/spenc/Downloads/trainslit4.csv", stringsAsFactors=FALSE)
trainslit5 <- trainslit5[,-c(1)]
trainer <- trainslit5
trainer$troutcome <- as.factor(trainer$troutcome)
trainer$Species <- as.factor(trainer$Species)
trainer$Sex <- as.factor(trainer$Sex)
trainer$Intake.Type <- as.factor(trainer$Intake.Type)
trainer$Shelter <- as.factor(trainer$Shelter)
trainer$Omonth <- as.factor(trainer$Omonth)
trainer$Oyear <- as.factor(trainer$Oyear)
trainer$Odow <- as.factor(trainer$Odow)
trainer$breedya <- as.factor(trainer$breedya)
trainer$ageya <- as.factor(trainer$ageya)
trainer$nameya <- as.factor(trainer$nameya)

trainer$Species <- ifelse(trainer$Species== "DOG", 1, 0)
trainer$breedya <- ifelse(trainer$breedya=="Yes",1,0)
trainer$nameya <- ifelse(trainer$nameya=="Yes",1,0)
trainer$ageya <- ifelse(trainer$ageya=="Yes",1,0)



dSex <- dummy.code(trainer$Sex)
dIntake.Type <- dummy.code(trainer$Intake.Type)
dShelter <- dummy.code(trainer$Shelter)
dOmonth <- dummy.code(trainer$Omonth)
dOyear <- dummy.code(trainer$Oyear)
dOdow <- dummy.code(trainer$Odow)

trainer2 <- data.frame(trainer$troutcome, trainer$Species, dSex, dIntake.Type, dShelter, dOmonth, dOyear, dOdow, trainer$duration, trainer$breedya, trainer$nameya, trainer$Age, trainer$ageya)
#t1_all_ohe <- as.data.frame(predict(dummies, newdata = trainslit4))
#t1_all_combined <- cbind(trainslit4[,-c(which(colnames(trainslit4) %in% ohe_feats))],t1_all_ohe)
#t1_all_combined$troutcome <- as.numeric(as.factor(t1_all_combined$troutcome))
#t1_all_combined$troutcome <- t1_all_combined$troutcome - 1
trainer2$trainer.troutcome <- as.numeric(as.factor(trainer2$trainer.troutcome)) - 1
trainer2$trainer.duration <- as.numeric(trainer2$trainer.duration)
bstDense2 <- xgboost(data = as.matrix(trainer2[,-1]), 
                     label =trainer2[,1], 
                     missing=NaN, 
                     max.depth = 6, 
                     eta = .3, 
                     nround = 1000, 
                     num_class=3, 
                     verbose=0, 
                     objective = "multi:softprob") 

# do the same for the test data

test3 <- read.csv("C:/Users/spenc/Downloads/extratest2(nona).csv")
test3 <- test3[complete.cases(test3$duration),]
test3 <- test3[,c("ARN", "Species","Sex","Intake.Type","Shelter","Omonth","Oyear","Odow","duration","breedya","nameya","Age","ageya")]

#test3$troutcome <- as.factor(test3$troutcome)
test3$Species <- as.factor(test3$Species)
test3$Sex <- as.factor(test3$Sex)
test3$Intake.Type <- as.factor(test3$Intake.Type)
test3$Shelter <- as.factor(test3$Shelter)
test3$Omonth <- as.factor(test3$Omonth)
test3$Oyear <- as.factor(test3$Oyear)
test3$Odow <- as.factor(test3$Odow)
test3$breedya <- as.factor(test3$breedya)
test3$ageya <- as.factor(test3$ageya)
test3$nameya <- as.factor(test3$nameya)

test3$Species <- ifelse(test3$Species== "DOG", 1, 0)
test3$breedya <- ifelse(test3$breedya=="Yes",1,0)
test3$nameya <- ifelse(test3$nameya=="Yes",1,0)
test3$ageya <- ifelse(test3$ageya=="Yes",1,0)



dSex <- dummy.code(test3$Sex)
dIntake.Type <- dummy.code(test3$Intake.Type)
dShelter <- dummy.code(test3$Shelter)
dOmonth <- dummy.code(test3$Omonth)
dOyear <- dummy.code(test3$Oyear)
dOdow <- dummy.code(test3$Odow)

tester <- data.frame(test3$ARN, test3$Species, dSex, dIntake.Type, dShelter, dOmonth, dOyear, dOdow, test3$duration, test3$breedya, test3$nameya, test3$Age, test3$ageya)

xgtest <- xgb.DMatrix(as.matrix(tester[,-c(1)]), missing=NaN)
xgb.importance(colnames(trainer2[,-1]), model = bstDense2)

pred <- predict(bstDense2 ,xgtest) 
mypred <- data.frame(tester$test3.ARN, matrix(pred, ncol=3, byrow=TRUE))
names(mypred) <- c("ARN", "ADOPTION","EUTHANASIA","OTHER")
inventorymatrix <- as.data.frame(inventorymatrix)
inventorymatrix$ARN <- row.names(inventorymatrix)
names(inventorymatrix) <- c("ADOPTION","EUTHANASIA","OTHER","ARN")
inventorymatrix <- inventorymatrix[c("ARN","ADOPTION","EUTHANASIA","OTHER")]

RF7 <- rbind(mypred, inventorymatrix)
RF7 <- as.data.frame(RF7)
#RF4$ARN <- row.names(RF4)

write.csv(RF7, file="submission_5.30.csv", row.names=FALSE)

#### Using the newest dataset on 5/31 ####
#merging datasets
data1 <- read.csv("C:/Users/spenc/Dropbox/UCLA/Spring 2016 Classes/Stats 101C/Final Project/finaltrain.csv", stringsAsFactors=FALSE)
data2 <- read.csv("C:/Users/spenc/Downloads/trainslit4.csv", stringsAsFactors=FALSE)
data1 <- data1[,c(1,18)]
merged <- merge(data1,data2, by = "ARN")

library(caret)
#ohe_feats = c("Sex","Intake.Type","Shelter","Omonth","Oyear","Odow")
library(psych)
trainslit5 <- read.csv("C:/Users/spenc/Downloads/trainslit4.csv", stringsAsFactors=FALSE)
trainslit5 <- trainslit5[,-c(1)]
trainer2 <- trainslit5
trainer$troutcome <- as.factor(trainer$troutcome)
trainer$Species <- as.factor(trainer$Species)
trainer$Sex <- as.factor(trainer$Sex)
trainer$Intake.Type <- as.factor(trainer$Intake.Type)
trainer$Shelter <- as.factor(trainer$Shelter)
trainer$Omonth <- as.factor(trainer$Omonth)
trainer$Oyear <- as.factor(trainer$Oyear)
trainer$Odow <- as.factor(trainer$Odow)
trainer$breedya <- as.factor(trainer$breedya)
trainer$ageya <- as.factor(trainer$ageya)
trainer$nameya <- as.factor(trainer$nameya)

trainer$Species <- ifelse(trainer$Species== "DOG", 1, 0)
trainer$breedya <- ifelse(trainer$breedya=="Yes",1,0)
trainer$nameya <- ifelse(trainer$nameya=="Yes",1,0)
trainer$ageya <- ifelse(trainer$ageya=="Yes",1,0)



dSex <- dummy.code(trainer$Sex)
dIntake.Type <- dummy.code(trainer$Intake.Type)
dShelter <- dummy.code(trainer$Shelter)
dOmonth <- dummy.code(trainer$Omonth)
dOyear <- dummy.code(trainer$Oyear)
dOdow <- dummy.code(trainer$Odow)

trainer2 <- data.frame(trainer$troutcome, trainer$Species, dSex, dIntake.Type, dShelter, dOmonth, dOyear, dOdow, trainer$duration, trainer$breedya, trainer$nameya, trainer$Age, trainer$ageya)
#t1_all_ohe <- as.data.frame(predict(dummies, newdata = trainslit4))
#t1_all_combined <- cbind(trainslit4[,-c(which(colnames(trainslit4) %in% ohe_feats))],t1_all_ohe)
#t1_all_combined$troutcome <- as.numeric(as.factor(t1_all_combined$troutcome))
#t1_all_combined$troutcome <- t1_all_combined$troutcome - 1
trainer2$trainer.troutcome <- as.numeric(as.factor(trainer2$trainer.troutcome)) - 1
trainer2$trainer.duration <- as.numeric(trainer2$trainer.duration)
bstDense2 <- xgboost(data = as.matrix(trainer2[,-1]), 
                     label =trainer2[,1], 
                     missing=NaN, 
                     max.depth = 6, 
                     eta = .3, 
                     nround = 1000, 
                     num_class=3, 
                     verbose=0, 
                     objective = "multi:softprob") 

# do the same for the test data

test3 <- read.csv("C:/Users/spenc/Downloads/extratest2(nona).csv")
test3 <- test3[complete.cases(test3$duration),]
test3 <- test3[,c("ARN", "Species","Sex","Intake.Type","Shelter","Omonth","Oyear","Odow","duration","breedya","nameya","Age","ageya")]

#test3$troutcome <- as.factor(test3$troutcome)
test3$Species <- as.factor(test3$Species)
test3$Sex <- as.factor(test3$Sex)
test3$Intake.Type <- as.factor(test3$Intake.Type)
test3$Shelter <- as.factor(test3$Shelter)
test3$Omonth <- as.factor(test3$Omonth)
test3$Oyear <- as.factor(test3$Oyear)
test3$Odow <- as.factor(test3$Odow)
test3$breedya <- as.factor(test3$breedya)
test3$ageya <- as.factor(test3$ageya)
test3$nameya <- as.factor(test3$nameya)

test3$Species <- ifelse(test3$Species== "DOG", 1, 0)
test3$breedya <- ifelse(test3$breedya=="Yes",1,0)
test3$nameya <- ifelse(test3$nameya=="Yes",1,0)
test3$ageya <- ifelse(test3$ageya=="Yes",1,0)



dSex <- dummy.code(test3$Sex)
dIntake.Type <- dummy.code(test3$Intake.Type)
dShelter <- dummy.code(test3$Shelter)
dOmonth <- dummy.code(test3$Omonth)
dOyear <- dummy.code(test3$Oyear)
dOdow <- dummy.code(test3$Odow)

tester <- data.frame(test3$ARN, test3$Species, dSex, dIntake.Type, dShelter, dOmonth, dOyear, dOdow, test3$duration, test3$breedya, test3$nameya, test3$Age, test3$ageya)

xgtest <- xgb.DMatrix(as.matrix(tester[,-c(1)]), missing=NaN)
xgb.importance(colnames(trainer2[,-1]), model = bstDense2)

pred <- predict(bstDense2 ,xgtest) 
mypred <- data.frame(tester$test3.ARN, matrix(pred, ncol=3, byrow=TRUE))
names(mypred) <- c("ARN", "ADOPTION","EUTHANASIA","OTHER")
inventorymatrix <- as.data.frame(inventorymatrix)
inventorymatrix$ARN <- row.names(inventorymatrix)
names(inventorymatrix) <- c("ADOPTION","EUTHANASIA","OTHER","ARN")
inventorymatrix <- inventorymatrix[c("ARN","ADOPTION","EUTHANASIA","OTHER")]

RF7 <- rbind(mypred, inventorymatrix)
RF7 <- as.data.frame(RF7)
#RF4$ARN <- row.names(RF4)

write.csv(RF7, file="submission_5.30.csv", row.names=FALSE)







#### New stuff ####
train <- read.csv("C:/Users/spenc/Downloads/fulltrainslit.csv", stringsAsFactors=FALSE)
test <- read.csv("C:/Users/spenc/Downloads/fulltestclean.csv", stringsAsFactors=FALSE)
train <- train[,-c(1,2,3,4,6,8,9,10,11,13,15,16,17,18,28)]
test <- test[,-c(1,2,4,6,8,9,10,11,13,15,16,17,26)]

train$troutcome <- as.factor(train$troutcome)
train$Species <- as.factor(train$Species)
train$Sex <- as.factor(train$Sex)
train$Intake.Type <- as.factor(train$Intake.Type)
train$Shelter <- as.factor(train$Shelter)
train$Omonth <- as.factor(train$Omonth)
train$Oyear <- as.factor(train$Oyear)
train$Odow <- as.factor(train$Odow)
train$breedya <- as.factor(train$breedya)
train$ageya <- as.factor(train$ageya)
train$nameya <- as.factor(train$nameya)
train$Species <- ifelse(train$Species== "DOG", 1, 0)
train$breedya <- ifelse(train$breedya=="Yes",1,0)
train$nameya <- ifelse(train$nameya=="Yes",1,0)
train$ageya <- ifelse(train$ageya=="Yes",1,0)
train$chipya <- ifelse(train$chipya=="Yes",1,0)
train$SNya <- ifelse(train$SNya=="Yes",1,0)
train$Licenseya <- ifelse(train$Licenseya=="Yes",1,0)
train$BlackYa <- as.numeric(train$BlackYa)
train$troutcome <- as.numeric(as.factor(train$troutcome)) - 1
train$duration <- as.numeric(train$duration)
dSex <- dummy.code(train$Sex)
dIntake.Type <- dummy.code(train$Intake.Type)
dShelter <- dummy.code(train$Shelter)
dOmonth <- dummy.code(train$Omonth)
dOyear <- dummy.code(train$Oyear)
dOdow <- dummy.code(train$Odow)
dSize <- dummy.code(train$size)

train <- data.frame(train$troutcome, train$Species, dSex, dIntake.Type, dShelter, dOmonth, dOyear, dOdow, dSize, train$duration, train$breedya, train$nameya, train$Age, train$ageya, train$chipya, train$Licenseya, train$SNya, train$BlackYa)

#do the same to test
test$Species <- as.factor(test$Species)
test$Sex <- as.factor(test$Sex)
test$Intake.Type <- as.factor(test$Intake.Type)
test$Shelter <- as.factor(test$Shelter)
test$Omonth <- as.factor(test$Omonth)
test$Oyear <- as.factor(test$Oyear)
test$Odow <- as.factor(test$Odow)
test$breedya <- as.factor(test$breedya)
test$ageya <- as.factor(test$ageya)
test$nameya <- as.factor(test$nameya)
test$Species <- ifelse(test$Species== "DOG", 1, 0)
test$breedya <- ifelse(test$breedya=="Yes",1,0)
test$nameya <- ifelse(test$nameya=="Yes",1,0)
test$ageya <- ifelse(test$ageya=="Yes",1,0)
test$chipya <- ifelse(test$chipya=="Yes",1,0)
test$SNya <- ifelse(test$SNya=="Yes",1,0)
test$Licenseya <- ifelse(test$Licenseya=="Yes",1,0)
test$BlackYa <- as.numeric(test$BlackYa)
test$duration <- as.numeric(test$duration)
dSex <- dummy.code(test$Sex)
dIntake.Type <- dummy.code(test$Intake.Type)
dShelter <- dummy.code(test$Shelter)
dOmonth <- dummy.code(test$Omonth)
dOyear <- dummy.code(test$Oyear)
dOdow <- dummy.code(test$Odow)
dSize <- dummy.code(test$size)

test <- data.frame(test$ARN, test$Species, dSex, dIntake.Type, dShelter, dOmonth, dOyear, dOdow, dSize, test$duration, test$breedya, test$nameya, test$Age, test$ageya, test$chipya, test$Licenseya, test$SNya, test$BlackYa)

bstDense3 <- xgboost(data = as.matrix(train[,-1]), 
                     label =train[,1], 
                     missing=NaN, 
                     max.depth = 6, 
                     eta = .3, 
                     nround = 1000, 
                     num_class=3, 
                     verbose=0, 
                     objective = "multi:softprob") 

xgtest <- xgb.DMatrix(as.matrix(test[,-c(1)]), missing=NaN)
#xgb.importance(colnames(train[,-1]), model = bstDense3)

pred <- predict(bstDense3 ,xgtest) 
mypred <- data.frame(test$test.ARN, matrix(pred, ncol=3, byrow=TRUE))
names(mypred) <- c("ARN", "ADOPTION","EUTHANASIA","OTHER")
inventorymatrix <- as.data.frame(inventorymatrix)
inventorymatrix$ARN <- row.names(inventorymatrix)
names(inventorymatrix) <- c("ADOPTION","EUTHANASIA","OTHER","ARN")
inventorymatrix <- inventorymatrix[c("ARN","ADOPTION","EUTHANASIA","OTHER")]

RF9 <- rbind(mypred, inventorymatrix)
RF9 <- as.data.frame(RF9)
#RF4$ARN <- row.names(RF4)

write.csv(RF9, file="submission_5.31.csv", row.names=FALSE)

#### New stuff w dummy coding ####

train <- read.csv("C:/Users/spenc/Downloads/fulltrainslit.csv", stringsAsFactors=FALSE)
test <- read.csv("C:/Users/spenc/Downloads/fulltestclean.csv", stringsAsFactors=FALSE)
train <- train[,-c(1,2,3,4,6,8,9,10,11,13,15,16,17,18,28)]
test <- test[,-c(1,2,4,6,8,9,10,11,13,15,16,17,26)]

train$troutcome <- as.factor(train$troutcome)
train$Species <- as.factor(train$Species)
train$Sex <- as.factor(train$Sex)
train$Intake.Type <- as.factor(train$Intake.Type)
train$Shelter <- as.factor(train$Shelter)
train$Omonth <- as.factor(train$Omonth)
train$Oyear <- as.factor(train$Oyear)
train$Odow <- as.factor(train$Odow)
train$breedya <- as.factor(train$breedya)
train$ageya <- as.factor(train$ageya)
train$nameya <- as.factor(train$nameya)
train$Species <- ifelse(train$Species== "DOG", 1, 0)
train$breedya <- ifelse(train$breedya=="Yes",1,0)
train$nameya <- ifelse(train$nameya=="Yes",1,0)
train$ageya <- ifelse(train$ageya=="Yes",1,0)
train$chipya <- ifelse(train$chipya=="Yes",1,0)
train$SNya <- ifelse(train$SNya=="Yes",1,0)
train$Licenseya <- ifelse(train$Licenseya=="Yes",1,0)
train$BlackYa <- as.numeric(train$BlackYa)
train$troutcome <- as.numeric(as.factor(train$troutcome)) - 1
train$duration <- as.numeric(train$duration)
train$Sex <- as.numeric(train$Sex) - 1
train$Intake.Type <- as.numeric(train$Intake.Type) - 1
train$Shelter <- as.numeric(train$Shelter) - 1
train$Omonth <- as.numeric(train$Omonth) - 1
train$Oyear <- as.numeric(train$Oyear) - 1
train$Odow <- as.numeric(train$Odow) - 1



newvars <- read.csv("C:/Users/spenc/Downloads/trainslit8.csv") #add new vars
newvars <- newvars[,c(16,22:26)]
newvars$chihuahuaya <- ifelse(newvars$chihuahuaya=="yes",1,0)
newvars$pitbullya <- ifelse(newvars$pitbullya=="yes",1,0)
newvars$stateya <- ifelse(newvars$stateya=="Yes",1,0)
#dSamechipdate <- dummy.code(newvars$samechipdate)
newvars$ShelterN <- as.numeric(newvars$ShelterN)
newvars$OutcomeN <- as.numeric(newvars$OutcomeN)

#newvars <- data.frame(newvars[,-c(4)], dSamechipdate)

train <- data.frame(train,newvars)
#train <- train[,-c(24:25)]
#do the same to test
# make all the dummies then bind
dSpecies <- dummy.code(train$Species)
dSex <- dummy.code(train$Sex)
dIntake.Type <- dummy.code(train$Intake.Type)
dShelter <- dummy.code(train$Shelter)
dOmonth <- dummy.code(train$Omonth)
dOyear <- dummy.code(train$Oyear)
dOdow <- dummy.code(train$Odow)
dBreedya <- dummy.code(train$breedya)
dnameya <- dummy.code(train$nameya)
dageya <- dummy.code(train$ageya)
dSize <- dummy.code(train$size)
dchipya <- dummy.code(train$chipya)
dSNya <- dummy.code(train$SNya)
dlicenseya <- dummy.code(train$Licenseya)
dblackya <- dummy.code(train$BlackYa)
dchihuahuya <- dummy.code(train$chihuahuaya)
dpitbullya <- dummy.code(train$pitbullya)
dstateya <- dummy.code(train$stateya)
dSamechipdate <- dummy.code(train$samechipdate)

train <- train[,c("duration", "troutcome", "Age", "OutcomeN", "ShelterN")]
train <- data.frame(train, dSpecies, dSex, dIntake.Type, dShelter, dOmonth, dOyear, dOdow, dBreedya, dnameya, dageya, dSize, dchipya, dSNya, dlicenseya, dblackya, dchihuahuya, dpitbullya, dstateya, dSamechipdate)


test$Species <- as.factor(test$Species)
test$Sex <- as.factor(test$Sex)
test$Intake.Type <- as.factor(test$Intake.Type)
test$Shelter <- as.factor(test$Shelter)
test$Omonth <- as.factor(test$Omonth)
test$Oyear <- as.factor(test$Oyear)
test$Odow <- as.factor(test$Odow)
test$breedya <- as.factor(test$breedya)
test$ageya <- as.factor(test$ageya)
test$nameya <- as.factor(test$nameya)
test$Species <- ifelse(test$Species== "DOG", 1, 0)
test$breedya <- ifelse(test$breedya=="Yes",1,0)
test$nameya <- ifelse(test$nameya=="Yes",1,0)
test$ageya <- ifelse(test$ageya=="Yes",1,0)
test$chipya <- ifelse(test$chipya=="Yes",1,0)
test$SNya <- ifelse(test$SNya=="Yes",1,0)
test$Licenseya <- ifelse(test$Licenseya=="Yes",1,0)
test$BlackYa <- as.numeric(test$BlackYa)
test$duration <- as.numeric(test$duration)
test$Sex <- as.numeric(test$Sex) - 1
test$Intake.Type <- as.numeric(test$Intake.Type) - 1
test$Shelter <- as.numeric(test$Shelter) - 1
test$Omonth <- as.numeric(test$Omonth) - 1
test$Oyear <- as.numeric(test$Oyear) - 1
test$Odow <- as.numeric(test$Odow) - 1
#dSize <- dummy.code(test$size)

newtest <- read.csv("C:/Users/spenc/Downloads/testZsize2.csv") #add new vars
newtest <- newtest[,c(15, 21:25)]
newtest$chihuahuaya <- ifelse(newtest$chihuahuaya=="yes",1,0)
newtest$pitbullya <- ifelse(newtest$pitbullya=="yes",1,0)
newtest$stateya <- ifelse(newtest$stateya=="Yes",1,0)
dSamechipdate <- dummy.code(newtest$samechipdate)
newtest$ShelterN <- as.numeric(newtest$ShelterN) - 1


test <- data.frame(test, newtest)

#newvars <- data.frame(newvars[,-c(4)], dSamechipdate)

#test <- test[,-c(24:25)]
#do the same to test
# make all the dummies then bind
dSpecies <- dummy.code(test$Species)
dSex <- dummy.code(test$Sex)
dIntake.Type <- dummy.code(test$Intake.Type)
dShelter <- dummy.code(test$Shelter)
dOmonth <- dummy.code(test$Omonth)
dOyear <- dummy.code(test$Oyear)
dOdow <- dummy.code(test$Odow)
dBreedya <- dummy.code(test$breedya)
dnameya <- dummy.code(test$nameya)
dageya <- dummy.code(test$ageya)
dSize <- dummy.code(test$size)
dchipya <- dummy.code(test$chipya)
dSNya <- dummy.code(test$SNya)
dlicenseya <- dummy.code(test$Licenseya)
dblackya <- dummy.code(test$BlackYa)
dchihuahuya <- dummy.code(test$chihuahuaya)
dpitbullya <- dummy.code(test$pitbullya)
dstateya <- dummy.code(test$stateya)
dSamechipdate <- dummy.code(test$samechipdate)

test <- test[,c("ARN", "duration", "Age", "OutcomeN", "ShelterN")]
test <- data.frame(test, dSpecies, dSex, dIntake.Type, dShelter, dOmonth, dOyear, dOdow, dBreedya, dnameya, dageya, dSize, dchipya, dSNya, dlicenseya, dblackya, dchihuahuya, dpitbullya, dstateya, dSamechipdate)


bstDense5 <- xgboost(data = as.matrix(train[,-2]), 
                     label =train[,2], 
                     missing=NaN, 
                     max.depth = 6, 
                     eta = .3, 
                     nround = 1000, 
                     num_class=3, 
                     verbose=0, 
                     objective = "multi:softprob") 

xgtest <- xgb.DMatrix(as.matrix(test[,-c(1)]), missing=NaN)
xgb.importance(colnames(train[,-2]), model = bstDense5)

pred <- predict(bstDense5 ,xgtest) 
mypred <- data.frame(test$ARN, matrix(pred, ncol=3, byrow=TRUE))
names(mypred) <- c("ARN", "ADOPTION","EUTHANASIA","OTHER")
inventorymatrix <- as.data.frame(inventorymatrix)
inventorymatrix$ARN <- row.names(inventorymatrix)
names(inventorymatrix) <- c("ADOPTION","EUTHANASIA","OTHER","ARN")
inventorymatrix <- inventorymatrix[c("ARN","ADOPTION","EUTHANASIA","OTHER")]

RF10 <- rbind(mypred, inventorymatrix)
RF10 <- as.data.frame(RF10)
#RF4$ARN <- row.names(RF4)

write.csv(RF10, file="submission_6.4e.csv", row.names=FALSE)

#### combining ####
library(data.table)
a <- aggregate(. ~ ARN, rbind(RF8, RF9), mean)
d1 <- read.csv("C:/Users/spenc/Downloads/3103749/doubleRFmerge.csv", stringsAsFactors=FALSE)
d2 <- read.csv("C:/Users/spenc/Downloads/3101143/SubmissionJune1.csv", stringsAsFactors=FALSE)
d3 <- read.csv("C:/Users/spenc/Downloads/3100828/Submission31.3.csv", stringsAsFactors=FALSE)
d4 <- read.csv("C:/Users/spenc/Downloads/3100716/Submission31.2.csv", stringsAsFactors=FALSE)

a1 <- aggregate(. ~ ARN, rbind(d1, d2), mean)
a2 <- aggregate(. ~ ARN, rbind(a1, d3), mean)
a3 <- aggregate(. ~ ARN, rbind(a2, d4), mean)
a4 <- aggregate(. ~ ARN, rbind(a3, d4), mean)

write.csv(a4, file="submission_6.1.csv", row.names=FALSE)

d5 <- read.csv("C:/Users/spenc/Downloads/3114960/SubmissionJune3.csv", stringsAsFactors=FALSE)
d6 <- read.csv("C:/Users/spenc/Downloads/3104094/submission_6.1.csv", stringsAsFactors=FALSE)
a5 <- aggregate(. ~ ARN, rbind(d5, d6), mean)
a6 <- aggregate(. ~ ARN, rbind(a5, RF10), mean)
write.csv(a6, file="submission_6.4c.csv", row.names=FALSE)

#### correlation matrix ####
dat <- data.frame(train)
corr<-round(cor(dat),digits = 4)


#### Graphing methods ####

library(ggplot2)

Methods <- read.csv("C:/Users/spenc/Dropbox/UCLA/Spring 2016 Classes/Stats 101C/Final Project/Methods.csv")
summary(Methods)
RF <- Methods$Category[Methods$Category=="RF"]
KNN <- Methods$Category[Methods$Category=="KNN"]
NB <- Methods$Category[Methods$Category=="NB"]
NN <- Methods$Category[Methods$Category=="NN"]
Boost <- Methods$Category[Methods$Category=="Boost"]
Ensemble <- Methods$Category[Methods$Category=="Ensemble"]
summary(RF)
library(plyr)
tab <- aggregate(Methods$Value, by=list(Methods$Category), FUN=mean)[2]
tab2 <- aggregate(Methods$Value, by=list(Methods$Category), FUN=min)[2]
names <- c("Boost", "Ensemble", "KNN", "NB", "NN", "RF", "SVM")
df <- data.frame(names,tab, tab2)
colnames(df) <- c("Method", "Mean Score", "Minimum Score")
df <- as.data.frame(df)
write.csv(df, file="df1.csv")
library(xtable)
print(xtable(df), type="latex")

p <- ggplot(Methods,aes(Time, Value))
p + geom_line(colour = "limegreen", size=3) + ggtitle("Progression Over Time") + ylab("Score") + theme_minimal() + geom_point(aes(x=55, y=0.31889), size=5, color="red")

p2 <- ggplot(data=Methods, aes(x=Time, y=Value, group = Category, colour = Category)) +
  geom_line()

meth <- Methods[-1,]
p3 <- ggplot(data=meth, aes(x=Time, y=Value, group = Category, colour = Category)) +
  geom_line(size=1.5) + ylab("Score") + ggtitle("Scores of Different Methods") + xlab("Count")

# d=data.frame(a=c("a","b","c","d","e","f", "g"))
p4 <- ggplot(data=Methods, aes(x=Time, y=log(Value), group = Category, colour = Category)) +
  geom_point(aes(shape=Category), size=5) + 
  ylab("log of Score") + ggtitle("Scores of Different Methods") + 
  xlab("Submission Number") + 
  scale_shape_manual(values = c(8,15:21)) +
  theme_minimal() 



p4 + geom_point(aes(x=37, y=0.319), size = 6, color="yellow" ,scale_shape(solid = FALSE)) 
