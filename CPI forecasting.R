### The objective of this script is to create a predictive model of the monthly inflation rate,
### measured by the percent change in the CPI from month to month. 
### All data from the Federal Reserve Bank of St.Louis (FRED) 


library(dplyr)
library(leaps)
library(caret)
#------------------------------------------Data Importation and Cleaning--------------

CPI <- read.csv('C:/Users/kduba/OneDrive/Desktop/projects/CPIAUCSL.csv')
UNRATE <- read.csv('C:/Users/kduba/OneDrive/Desktop/projects/UNRATE.csv')
M1 <- read.csv('C:/Users/kduba/OneDrive/Desktop/projects/M1SL.csv')

CPI$DATE <- as.Date(CPI$DATE, '%m/%d/%Y')
UNRATE$DATE <- as.Date(UNRATE$DATE,'%m/%d/%Y')
M1$DATE <- as.Date(M1$DATE,'%m/%d/%Y')

# Generating Percent Change of the CPI
# This is equivalent to the monthly inflation rate

monthly_infl <- c()
for (i in 2:nrow(CPI)){
    growth <- ((CPI$CPIAUCSL[i] - CPI$CPIAUCSL[i-1])/CPI$CPIAUCSL[i-1]) * 100
    monthly_infl <- c(monthly_infl, growth)
    print(i)
} 

CPI2 <- CPI[2:898,]

CPI3 <- cbind(CPI2, monthly_infl)

# Generating new columns in the CPI data frame containing lags 1-6 of the monthly 
# inflation rate

for (i in 1:6){
    CPI3[i + 3] <- lag(CPI3$monthly_infl, i) 
}

#renaming new columns 

colnames(CPI3)[4:9] <- c("monthly_infl_lag1", "monthly_infl_lag2", "monthly_infl_lag3", 
                         "monthly_infl_lag4", "monthly_infl_lag5", "monthly_infl_lag6")

CPI4 <- CPI3[8:897, ]

# Generating new columns in the UNRATE data frame containing lags 1-3 of the monthly
# unemployment rate 


for (i in 1:3){
    UNRATE[i + 2] <- lag(UNRATE$UNRATE, i) 
}


#renaming new columns 

colnames(UNRATE)[3:5] <- c("UNRATE_lag1", "UNRATE_lag2", "UNRATE_lag3")
UNRATE2 <- UNRATE[4:nrow(UNRATE), ]

#Merge UNRATE with CPI4 to get a data frame with unemployment rate and monthly inflation rate

df_merge <- merge(UNRATE2, CPI4, by = "DATE")


#Generating new columns in the Money Supply Data frame containing lags 1-2 of the monthly money
#supply (M1)

for (i in 1:2){
    M1[i + 2] <- lag(M1$M1SL, i) 
}

colnames(M1)[3:4] <- c("M1_lag1", "M2_lag2")
M1_2 <- M1[3:nrow(M1), ]


#Add money supply data to the merged data set

df_merge2 <- merge(M1_2, df_merge, by = "DATE")

#Remove dates from the data set

forecasting_data <- df_merge2[2:16]


#-----------------------------------Forecasting---------------------------------

#Best Subset Selection
models <- regsubsets(monthly_infl ~., data = forecasting_data, nvmax = 14)
summary(models)


#Function to get the models from best subsets 

get_model_formula <- function(id, object, outcome){
    # get models data
    models <- summary(object)$which[id,-1]
    # Get outcome variable
    #form <- as.formula(object$call[[2]])
    #outcome <- all.vars(form)[1]
    # Get model predictors
    predictors <- names(which(models == TRUE))
    predictors <- paste(predictors, collapse = "+")
    # Build model formula
    as.formula(paste0(outcome, "~", predictors))
}


# Use a for loop to carry out cross validation on the best subset models

train_control <- trainControl(method = 'cv', number = 5)

for (i in 1:14){
    fit <- train(get_model_formula(i, models, 'monthly_infl'), data = forecasting_data, 
          method = 'lm', trControl = train_control)
    print(fit)
}

#Getting the model with the highest average out of sample R-squared 

get_model_formula(5,models, 'monthly_infl')


# Fit Regression

model5 <- lm(monthly_infl ~ UNRATE + UNRATE_lag1 + monthly_infl_lag1 + monthly_infl_lag4 + 
                 monthly_infl_lag6, data = forecasting_data)

summary(model5)

#Generate point forecasts 

predicted_vals <- predict(model5, forecasting_data)

#Put point forecasts into a data frame to be compared with actual

final_df <- data.frame(DATE = df_merge2$DATE, Predicted_Values = predicted_vals,
                       Actual = df_merge2$monthly_infl )

#Plot point forecasts vs actual 

plot(final_df$DATE, final_df$Actual, xlab = 'Time', ylab = 'Percent Change', main = 'Monthly Inflation Rate', type = 'l', col = 'orange')
lines(final_df$DATE, final_df$Predicted_Values, type = 'l', col = 'blue')
legend('bottomleft', legend = 'Predicted Values', col = 'blue', lty = 1)

#Calculate residuals and make a residual plot

final_df$residuals <- final_df$Actual - final_df$Predicted_Values
plot(final_df$DATE, final_df$residuals, type = 'l')


#Generate Upper and Lower Bounds of an 80% forecast interval

final_df$lower_bound <- final_df$Predicted_Values - 1.282*0.2333
final_df$upper_bound <- final_df$Predicted_Values + 1.282*0.2333

#October 2021 prediction

october_data<- data.frame(UNRATE = 4.8, UNRATE_lag1 = 5.2, monthly_infl_lag1 = 0.2743650488,
                  monthly_infl_lag4 = 0.6442255801, monthly_infl_lag6 = 0.6201526822)

october <- predict(model5, october_data)
october_l <- october - 1.282*0.2333
october_u <- october + 1.282*.2333
forecast <- c(october_l, october, october_u)
names(forecast) <- c("Lower Bound", "Point Forecast", "Upper Bound")
forecast
