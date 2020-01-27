library(party)

data(iris)

##CHAID model
fit<- ctree(Species~., data=iris)

plot(fit, main="Species")

summary(fit)

fit

View(d)

str(ds$card_offer)
ds$card_offer <- ifelse(ds$card_offer=="FALSE",0,1)


str(d)

d$card_offer <- as.factor(d$card_offer)
d$ad_exp <- as.factor(d$ad_exp)

#str(iris)
f <- ctree(card_offer~est_income, data=d)

plot(f, main="card_offer")

summary(fit)

colnames(d)

predictions <- predict(f,d[,1:11])

dst <- cbind(predictions,d)

getwd()

write.csv(dst,"asg.csv", row.names = FALSE)

table(predictions, d$card_offer)

8469/(1531+8469)
