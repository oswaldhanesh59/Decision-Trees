install.packages("rJava", type="source", INSTALL_opts = '--merge-multiarch')
install.packages("RWeka")

library(rJava)

Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre1.8.0_231/bin/client")

inDL(x, as.logical("C:/Users/OSIS/Documents/R/win-library/3.6/rJava/libs/x64"), as.logical(now), ...)

install.packages("Rtools")

data(iris)

#C45 model
fit <- j48(Species~., data=iris)

summary(fit)

predictions <- predict(fit, iris[,1:4])

table(predictions, iris$Species)