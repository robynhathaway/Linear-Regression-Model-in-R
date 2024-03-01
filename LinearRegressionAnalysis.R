# Guided project - Building and analysing linear regression model in R
# Instructor - Dr. Nikunj Maheshwari

# Required libraries have been installed but needs to be loaded
library(sjPlot)
library(dplyr)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())

# Task 1 - Load dataset and summarize it
data <- read.csv("cars.csv", header = TRUE, stringsAsFactors = FALSE)
head(data)
str(data)
summary(data)
# Task 2 - Clean your dataset
cols <- names(data)[vapply(data, is.character,logical(1))]
# filter for columns which only contain characteristics
# remove white spaces within any of these columns
data[,cols] <- lapply(data[,cols], trimws)
# R will automatically convert empty values to NA, but sometimes a dataset with contain manual N/A entries
data[data=="N/A"] = NA
# count average number of missing values
sapply(data, function(x) mean(is.na(x)))
# sapply applies a function to each column in the dataset
# remove columns with high numbers of missing values, in this case Market Category 
data$Market.Category <- NULL
#remove rows that contain one or more empty values
data <- data[complete.cases(data), ]
# complete.cases function returns rows that have no missing values

# Task 3 - Split into training and test set
# the training set is used to construct and refine the model, the models accuracy is then checked using the test data set to provide a completely unbiased evaluation of the model, regression line formula, using new unseen data 
data_num <- data %>% select_if(is.numeric) 
# selects numeric columns and stores the result as data_num variable 
# target variable for regression model is the price column MSRP
# create histogram of its values to see the distribution 
hist(data_num$MSRP, breaks=100)
# histogram shows some outliers which may cause issues in the model
# filter the data set to include cars with a price range between 15-50000
data_num <- data_num %>%
  filter(MSRP>15000) %>%
  filter(MSRP<50000)
# the data set has been updated to contain around 8000 cars and 8 columns 
# select 80% of data set as training and the remaining 20% as test data 
# floor function rounds to an integer
set.seed(123)
size <- floor(0.8 * nrow(data_num))
# use the sample function to randomly select 80% rows from the data set and store the row numbers/indices
train_ind <- sample(seq_len(nrow(data_num)), size = size)

# to return the training set: filter for train_ind, row numbers for training set 
# to return the test set: filter to ignore train_ind row number 
train <- data_num[train_ind, ]
test <- data_num[-train_ind, ]

# Task 4 - Fit linear regression model and interpret model summary statistics
# linear regression model assumes a linear relationship between the predictive variables and the outcome variable (MSRP)
# aim is to build a model that accurately predicts the MSRP using the other characteristics
# the lm function is used, focus on two parameters, model equation and the data set to be used
model <- lm(MSRP ~ ., data = train)
summary(model)
# the coefficients are the important part of the summary, the stars indicate the variables that are significantly associated with the outcome
# more stars, more significantly associated 
# column labelled estimates are the coefficients, can be interpreted as the average effect on the response variable for a one unit increase in a predictor given that the other predictors are fixed
# residual st error, multiple R squared, F-statistic are the metrics used to measure how well the model fits to the data 
# residual st error = predicted error in the training data set, average difference between the observed values and predicted values by the model
# R squared ranges from 0-1 and represents the proportion of variation in the response variable that can be explained by the predictor variables
# higher R squared = better model, but it will always increase when more predictors are added, so adjusted R squared accounts for the number of predictor variables
# mainly consider adjusted R squared value
# F statistic = overall significance of the model, assesses if at least one predictor variable has a non zero coefficient 
# can plot the estimates to show this visually using plot_model() function
plot_model(model, show.values = TRUE, value.offset = 0.2)
# shows the coefficient and significance value 
# can build the linear regression model by explicitly specifying the predictors that you want 
model2 <- lm(MSRP ~ Engine.HP + highway.MPG + Engine.Cylinders, 
             data = train)

# Task 5 - Plot and analyse model residuals
# Residuals are leftover values of the response variable after fitting a model to data
# they can reveal unexplained patterns, show how poorly a model represents the data, allows us to check if linear regression assumptions are met, shows how you can improve your model
# the plot function works with linear regression models
# it creates 4 diagnostic plots to show the residuals in different ways
# you can show all 4 plots within one 
par(mfrow=c(2,2))
# sets plotting frame as two rows and two columns 
plot(model)
# residuals vs fitted shows residuals have non-linear patterns but no distinctive patterns
# normal QQ plot shows if residuals are normally distributed, some deviation towards the end but mostly normal
# scale locations shows if residuals are spread equally along the ranges of predictors, use it to check the assumption of equal variance
# want to see a horizontal line with equally or randomly spread points,
# residuals vs leverage shows influential cases in the data set
# these might be extreme against the regression line and can alter the results if excluded from the model
# patterns are not relevant, but must check for outlying values at upper or lower right corners
# cases outside the cooks distance will influence the regression results 
# the four plots show row numbers of potential problematic cases, if cases appear across all four then should investigate them
# can always alter model by changing the predictors included 

# Task 6 - Predict future values and calculate model error metrics
# predict MSRP values of the test data set and compare with the observed MSRP values
test$pred <- predict(model, newdata = test)
# these values are stored in column pred in the test data set
# use ggplot to plot the predicted and observed values 
# first reset plotting pane to one plot per pane
par(mfrow=c(1,1))
ggplot(test, aes(x=MSRP, y=pred)) +
  geom_point() +
  geom_smooth(method="lm", color="blue") +
  theme_bw()
# calculate the error metrics of the linear model
# first find error = predicted value - observed value 
error <- test$pred - test$MSRP
# calculate 2 important error metrics: 
# root mean square error RMSE, good measure of accuracy of models outcome prediction
rmse <- sqrt(mean(error^2))
rmse
# mean absolut error MAE, measures average magnitude of the errors in your predictions without considering their direction
mae <- mean(abs(error))
mae
# RMSE gives larger errors a relatively larger weighting, so its more useful when large errors are undesireable
# MAE is better for interpretation 
### End of project