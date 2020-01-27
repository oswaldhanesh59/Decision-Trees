### install packages

install.packages("party")
library(party)

##Read the data in the file
cust_data<-read.csv(file.choose(),header = T)

View(cust_data)

# Conditional Inference Tree for Default_On_Payment
fit <- ctree(Species~., 
             data=iris )

plot(fit, main="Species ")

###detailed results including splits
summary(fit)


