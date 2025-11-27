# Wujian Xue
# 2024/8/17

# import data
# install.packages("readxl")
# install.packages("lubridate")

library(readxl)
MFLP <- read.csv("C:/Users/xuewu/Downloads/FNMA_MF_Loan_Performance_Data_202312.csv")
MFLP

head(MFLP)
tail(MFLP)
summary(MFLP)
names(MFLP)

library(dplyr)
MFLP2 <- MFLP

#filter(Liquidation.Prepayment.Code != "")

# remove duplicates
MFLP2 %>% distinct(Loan.Number, .keep_all = TRUE)
summary(MFLP2)

my_string <- gsub('$', '', Loan.Acquisition.UPB)

MFLP_new <- MFLP2 %>% 
  mutate(Loan.Acquisition.UPB2 = gsub('[\\$,]', '', Loan.Acquisition.UPB),
         Loan.Acquisition.UPB3 = as.numeric(Loan.Acquisition.UPB2),
         SDQ        = ifelse(SDQ.Indicator =='Y',1,0),
         SDQ.factor = factor(SDQ.Indicator)
  ) 
summary(MFLP_new)

# select variables
MFLP3 <- MFLP_new %>% 
  select(Loan.Acquisition.UPB3,
         #Amortization.Term,
         Original.Interest.Rate,
         Loan.Acquisition.LTV,
         #Underwritten.DSCR,
         Original.Term,
         Original.I.O.Term,
         #Modified.Loss.Sharing.Percentage,
         #Number.of.Properties.at.Acquisition,
         #Property.Acquisition.Total.Unit.Count,
         #Physical.Occupancy..,
         #Loan.Active.Property.Count,
         #Note.Rate,
         #Loan.Age,
         Amortization.Type,
         Lien.Position,
         SDQ,
         SDQ.Indicator,
         SDQ.factor
  )

summary(MFLP3)

table(MFLP3$SDQ)
table(MFLP3$Amortization.Type)
table(MFLP3$Lien.Position)

# Logistic Regression
log_model <- glm(SDQ ~ Loan.Acquisition.UPB3 + Original.Interest.Rate + Original.Term +
                   Amortization.Type     +  Lien.Position, 
                 data = MFLP3, family = "binomial")

summary(log_model)

#install.packages("writexl")
library("writexl")
write_xlsx(out,"C:\\1910_UoNA\\out.xlsx")

table(MFLP$SDQ.Indicator)

library(tree)
library(ISLR2)

# Decision Tree
tree.MFLP3 <- tree(SDQ.factor ~ Loan.Acquisition.UPB3 + Original.Interest.Rate + Original.Term +
                     Amortization.Type     +  Lien.Position,          MFLP3)
tree.MFLP3 <- tree(SDQ.factor ~ . - SDQ.factor - SDQ,MFLP3)

summary(tree.MFLP3)

plot(tree.MFLP3)
text(tree.MFLP3, pretty = 0)

# Bag and Random Forest

library(randomForest)
set.seed(1)
bag.MFLP <- randomForest(SDQ.factor ~ ., data = MFLP3, 
                         mtry = 12, importance = TRUE)

bag.MFLP 

library(randomForest)
set.seed(1)
bag.MFLP <- randomForest(SDQ.factor ~ Loan.Acquisition.UPB3 + Original.Interest.Rate + Original.Term +
                           Amortization.Type     +  Lien.Position, data = MFLP3, 
                         mtry = 12, 
                         importance = TRUE)

bag.MFLP 

importance(bag.MFLP)

varImpPlot(bag.MFLP)


yhat.bag <- predict(bag.MFLP, newdata = MFLP3 )
plot(yhat.bag, MFLP3)
abline(0, 1)
mean((yhat.bag - MFLP3)^2)

set.seed(1)
rf.MFLP <- randomForest(SDQ.factor ~ Loan.Acquisition.UPB3 + Original.Interest.Rate + Original.Term +
                          Amortization.Type     +  Lien.Position, data = MFLP3, 
                        mtry = 2, importance = TRUE)
rf.MFLP
importance(rf.MFLP )
varImpPlot(rf.MFLP )


# Boosting
#install.packages("gbm")
library(gbm)
set.seed(1)
boost.MFLP<- gbm(SDQ ~ Loan.Acquisition.UPB3 + Original.Interest.Rate + Original.Term + Loan.Acquisition.LTV,
                 #+ Amortization.Type     +  Lien.Position, 
                 data = MFLP3, 
                 distribution = "bernoulli", n.trees = 5000,
                 interaction.depth = 4)
summary(boost.MFLP)

plot(boost.MFLP, i = "Original.Interest.Rate")
plot(boost.MFLP, i = "Loan.Acquisition.UPB3")
plot(boost.MFLP, i = "Original.Term")































# Fit the model to the training data
tree_fit <- MFLP3 %>%
  fit(SDQ.factor ~ ., data = MFLP3)






# Split the data into training and testing sets
library(tidymodels)
set.seed(123)
data_split <- initial_split(MFLP3, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)


# Create a decision tree model specification
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Fit the model to the training data
tree_fit <- tree_spec %>%
  fit(SDQ.factor ~ ., data = train_data)


# Make predictions on the testing data
predictions <- tree_fit %>%
  predict(test_data) %>%
  pull(.pred)

# Calculate RMSE and R-squared
metrics <- metric_set(rmse, rsq)
model_performance <- test_data %>%
  mutate(predictions = predictions) %>%
  metrics(truth = medv, estimate = predictions)

print(model_performance)

# Make predictions on new data
new_data <- tribble(
  ~crim, ~zn, ~indus, ~chas, ~nox, ~rm, ~age, ~dis, ~rad, ~tax, ~ptratio, ~black, ~lstat,
  0.03237, 0, 2.18, 0, 0.458, 6.998, 45.8, 6.0622, 3, 222, 18.7, 394.63, 2.94
)
predictions <- predict(tree_fit, new_data)
print(predictions)

# Load the library
library(rpart.plot)

# Plot the decision tree
rpart.plot(tree_fit$fit, type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")


rules <- rpart.rules(tree_fit$fit)
print(rules)

# Load the necessary library
library(vip)

# Create a variable importance plot
var_importance <- vip::vip(tree_fit, num_features = 10)
print(var_importance)



