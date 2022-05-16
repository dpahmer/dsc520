## Assignment 8.2
# Regressions
# Housing Data
# Work individually on this assignment. You are encouraged to collaborate on ideas and strategies pertinent to this assignment. Data for this assignment is focused on real estate transactions recorded from 1964 to 2016 and can be found in Housing.xlsx. Using your skills in statistical correlation, multiple regression, and R programming, you are interested in the following variables: Sale Price and several other possible predictors.
# If you worked with the Housing dataset in previous week – you are in luck, you likely have already found any issues in the dataset and made the necessary transformations. If not, you will want to take some time looking at the data with all your new skills and identifying if you have any clean up that needs to happen.

library(readxl)
library(plyr)
library(dplyr)

setwd("C:/users/pahme/onedrive/documents/github/dsc520")
housingdf <- read_xlsx("data/week-7-housing.xlsx")


# Complete the following:
# Explain any transformations or modifications you made to the dataset

  # Consolidate the bathrooms into one variable
  # rename the sale date variable to sale_date (avoids problems)
  # make a latest build variable that takes the later of renovation or build year
  # extract the sale year from sale date to aggregate the sales into bins of a year

housingdf$all_bathrooms <- housingdf$bath_full_count+housingdf$bath_3qtr_count+housingdf$bath_half_count/2

names(housingdf)[names(housingdf)=='Sale Date'] <- 'sale_date'
names(housingdf)[names(housingdf)=='Sale Price'] <- 'sale_price'

housingdf$last_build <- housingdf$year_built
housingdf$last_build[housingdf$year_renovated > 0] <- housingdf$year_renovated[housingdf$year_renovated > 0]

housingdf$sale_year <- format(as.Date(housingdf$sale_date, format="%d%m%Y"), "%Y")
housingdf$sale_year <- strtoi(housingdf$sale_year) # because we want it as a number not a string


# Create two variables; one that will contain the variables Sale Price and Square Foot of Lot (same variables used from previous assignment on simple regression) and one that will contain Sale Price and several additional predictors of your choice. Explain the basis for your additional predictor selections.

simple_reg <- lm(sale_price ~ sq_ft_lot, data=housingdf)
multi_reg <- lm(sale_price ~ sq_ft_lot + square_feet_total_living + sale_year + bedrooms + all_bathrooms, data=housingdf)


  # Presumably in addition to the lot size we would expect the living area to correlate to the sale price
  # Presumably the prices rise over time, due to inflation
  # presumably price reflects the number of bathrooms, and bedrooms

  # If I understood the building grade, sale reason, sale warning, property type, current use codes- those might be important




# Execute a summary() function on two variables defined in the previous step to compare the model results. What are the R2 and Adjusted R2 statistics? Explain what these results tell you about the overall model. Did the inclusion of the additional predictors help explain any large variations found in Sale Price?

summary(simple_reg)
summary(multi_reg)



# Considering the parameters of the multiple regression model you have created. What are the standardized betas for each parameter and what do the values indicate?
library(lm.beta)
lm.beta(multi_reg)




# Calculate the confidence intervals for the parameters in your model and explain what the results indicate.
confint(multi_reg)

# Assess the improvement of the new model compared to your original model (simple regression model) by testing whether this change is significant by performing an analysis of variance.

anova(simple_reg, multi_reg)

# Perform casewise diagnostics to identify outliers and/or influential cases, storing each function's output in a dataframe assigned to a unique variable name.

resid <- data.frame(resid(multi_reg))



# Calculate the standardized residuals using the appropriate command, specifying those that are +-2, storing the results of large residuals in a variable you create.
stand.resid <- data.frame(rstandard(multi_reg))
large.stresid <- stand.resid > 2 | stand.resid < -2


# Use the appropriate function to show the sum of large residuals.
sum(large.stresid)

  ## Just to check, we expect the number of outliers to be about 5% of the observations, so
sum(large.stresid)/nrow(stand.resid)
  # which is actually half of that, so I wonder if I should be suspicious...

# Which specific variables have large residuals (only cases that evaluate as TRUE)?
  # I would have to run the simple regression on each variable individually, right? 

#   Investigate further by calculating the leverage, cooks distance, and covariance rations. Comment on all cases that are problematics.
lev <- hatvalues(multi_reg)
cook <- cooks.distance(multi_reg)
cov.ratio <- covratio(multi_reg)

cook[large.stresid]



# Perform the necessary calculations to assess the assumption of independence and state if the condition is met or not.
library(car)

dwt(multi_reg)

  # Hmmm.... so it seems not. We were looking for values between 1 and 3, but we got .54


# Perform the necessary calculations to assess the assumption of no multicollinearity and state if the condition is met or not.
vif(multi_reg)

1 / vif(multi_reg)

mean(vif(multi_reg))

  # So it looks like we are avoiding multicolinearity. 

# Visually check the assumptions related to the residuals using the plot() and hist() functions. Summarize what each graph is informing you of and if any anomalies are present.

plot(multi_reg)

hist(rstudent(multi_reg))

# Overall, is this regression model unbiased? If an unbiased regression model, what does this tell us about the sample vs. the entire population model?






