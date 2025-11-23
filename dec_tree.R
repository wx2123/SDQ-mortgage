# Wujian Xue
# Sep 2024


# install the package
# install.packages("tree")
# install.packages("ISLR2")


library(tree)
library(ISLR2)

attach(Carseats)
High <- factor(ifelse(Sales <= 8, "No", "Yes"))

Carseats <- data.frame(Carseats, High)


tree.carseats <- tree(High ~ . - Sales, Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

tree.carseats


install.packages("randomForest")
install.packages("Boston")

library(randomForest)
library(Boston)

set.seed(1)
