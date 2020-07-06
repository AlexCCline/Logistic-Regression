#Load dataset 1a.
bh<-read.csv("boston-housing.csv")


#Data Partitioning 1e.
library(caret)
set.seed(2015)
sam<-createDataPartition(bh$MEDV_CAT, p=0.7, list = FALSE)
train<-bh[sam,]
test<-bh[-sam,]

#Building Tree 1c.
library(rpart)
bh.tree<-rpart(MEDV_CAT ~ ., data = train, control = rpart.control(minibucket = 10, cp=0))

library(e1071)
#A-Priori Probabilities 1d.
bh.nb <- naiveBayes(MEDV_CAT~., data = train)
bh.nb

#tree model 1f.
bh.tree2<-rpart(MEDV_CAT ~ ., data = train, control = rpart.control(minisplit = 10, cp=0))


#Visualizing Tree 1g.
library(rpart.plot)
png("ClassificationTree.png")
prp(bh.tree2, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, varlen = 8, shadow.col = "green")
dev.off()


#Predicting Training and Test Dataset and Create CM 1h.
pred.train<-predict(bh.tree2, train, type="class")
cm1<-table(train$MEDV_CAT,pred.train, dnn=c("Actual", "Predicted"))
cm1
pred.test<-predict(bh.tree2, test, type="class")
cm2<-table(test$MEDV_CAT, pred.test, dnn=c("Actual", "Predicted"))
cm2

#Rebuild tree 1i.
bh.tree3<-rpart(MEDV_CAT ~ ., data = train, control = rpart.control(minisplit = 10, cp=0))

#Visualizing Tree 1j.
library(rpart.plot)
png("ClassificationTree.png")
prp(bh.tree3, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, varlen = 8, shadow.col = "green")
dev.off()

#Prune Tree 1k. and 1l. 
printcp(bh.tree3)
plotcp(bh.tree3)
bh.pruned<-prune(bh.tree3, 0.018)

#Plot Pruned Tree Graph 1m.
prp(bh.pruned, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE, faclen = 4, varlen = 8, shadow.col = "gray")

#1n.
bh.tree4<-rpart(MEDV_CAT ~ ., data = test, control = rpart.control(minisplit = 10, cp=0))

#1n.
pred.test<-predict(bh.tree4, test, type="class")
cm3<-table(test$MEDV_CAT, pred.test, dnn=c("Actual", "Predicted"))
cm3

##############################################################################
#2 Load Dataset
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[, -c(1,5)]

#2a.
bank.df$Education <- factor(bank.df$Education, levels = c(1,2,3), labels = c("Undergrad","Graduate","Advanced/Professional"))

#2b.
set.seed(2020)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index,]

#2c., 2d.
logit.reg <- glm(train.df$Personal.Loan~., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)
logit.reg

#2g.,2h.
logit.reg2 <- glm(valid.df$Personal.Loan~ valid.df$Income+valid.df$Family+valid.df$Education+valid.df$CD.Account+valid.df$Online+valid.df$CreditCard, data = valid.df, family = "binomial")
options(scipen=999)
summary(logit.reg2)
logit.reg2

logit.reg.pred <- predict(logit.reg, valid.df[,-8], type = "response")
data.frame(actual = valid.df$Personal.Loan[1:5], predicted = logit.reg.pred[1:5])
table(valid.df$Personal.Loan, logit.reg.pred > 0.5)


blr.predict<-predict(logit.reg, valid.df)
blr<-data.frame(valid.df, blr.predict)
blr


#2i.
table(valid.df$Personal.Loan, blr.predict > 0.5)


#2j.
library(gains)
gain <- gains(valid.df$Personal.Loan, blr.predict, groups = length(blr.predict))

plot(c(0, gain$cume.pct.of.total*sum(valid.df$Personal.Loan))
     ~c(0,gain$cume.obs),xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0, sum(valid.df$Personal.Loan))~c(0, dim(valid.df)[1]), lty=2)

heights<- gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints<- barplot(heights, names.arg = gain$depth, ylim = c(0,9),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
text(midpoints, heights+0.5, labels=round(heights,1), cex = 0.8)



#2k.
PredictedBank <-data.frame(blr.predict, valid.df)
write.csv(PredictedBank, file = "Predicted-UniversalBank.csv")










