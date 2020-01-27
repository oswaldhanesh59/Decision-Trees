install.packages("rpart")
install.packages("ctree")
install.packages("dplyr")
install.packages("rattle")
install.packages("ROCR")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("RGtk2")

library(ctree)
library(party)
library(rpart)
library(dplyr)
library(rattle)
library(ROCR)
library(rpart.plot)
library(RColorBrewer)
library(RGtk2)

str(la)

#converting dependent variable to factor
la$Creditability <- as.factor(la$Creditability)

library(caTools)
split=sample.split(la$Creditability,SplitRatio = 0.7)
View(split)
Train=subset(la,split=="TRUE")
Test=subset(la,split=="FALSE")

#Decision tree base model
tree_base_model <- rpart(Creditability~., data=Train, method="class")

#plot
rattle()
fancyRpartPlot(tree_base_model)
prp(tree_base_model,yesno = 0)
?prp

#Accuracy
Test$pred <- predict(tree_base_model, Test, type="class")
accuracy <- mean(Test$pred == Test$Creditability)

##Checking Accuracy
accuracy

##Preprunning
tree_preprocess_model <- rpart(Creditability~., data= Train,
                               method="class", control= rpart.control(minsplit=1,
                              minbucket = 1, maxdepth = 30, mindepth=10, usesurrogate = 1, xval=10, cp=0.001))

##prune the tree using best cp
bestcp<- tree_base_model$cptable[which.min(tree_base_model$cptable[,"xerror"]),"CP"]

tree_postprocess_model <- prune(tree_base_model, cp=bestcp)

prp(tree_preprocess_model)


Test$pred1 <- predict(tree_preprocess_model, Test, type="class")
accuracy1 <- mean(Test$pred1 == Test$Creditability)

##Checking Accuracy
accuracy1

##cp
bestcp <- tree_base_model$cptable[which.min(tree_base_model$cptable[,"xerror"]), "CP"]


val1= predict(tree_preprocess_model, Test, type="prob")

pred_val <- prediction(val1[,2], Test$Creditability)

perf_val <- performance(pred_val,"auc")
perf_val

perf_val<- performance(pred_val, "tpr", "fpr")

plot(perf_val,col="green", lwd= 1.5)

plot(performance(pred_val, measure = "lift", x.measure = "rpp"), colorize= TRUE)

##Beautify tree
prp(tree_preprocess_model, faclen = 0, cex=0.8, extra=1)

##view2
tot_count <- function(x,labs,digits,varlen)
{paste(labs, "/n/nn=", x$frame$n)}

prp(tree_preprocess_model, faclen = 0, cex=0.8, node.fun = tot_count)

fancyRpartPlot(tree_preprocess_model)
