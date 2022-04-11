# We interact with a few datasets in this course, one you are already familiar with, the 2014 American Community Survey 
# and the second is a Housing dataset, that provides real estate transactions recorded from 1964 to 2016.  
# For this exercise, you need to start practicing some data transformation steps – which will carry into next week, 
# as you learn some additional methods.  For this week, using either dataset (or one of your own – although I 
# will let you know ahead of time that the Housing dataset is used for a later assignment, 
# so not a bad idea for you to get more comfortable with now!), perform the following data transformations:

library(readxl)
library(dplyr)
library(plyr)

#     Use the apply function on a variable in your dataset
setwd("C:/users/pahme/onedrive/documents/github/dsc520")
housingdf <- read_xlsx("data/week-7-housing.xlsx")
str(housingdf)


#     Create at least 2 new variables
housingdf$all_bathrooms <- housingdf$bath_full_count+housingdf$bath_3qtr_count+housingdf$bath_half_count/2
  #this calculated the overall number of bathrooms as one variable

names(housingdf)[names(housingdf)=='Sale Date'] <- 'sale_date'
  # that was to deal with the space in the variable name

housingdf$sale_year <- format(as.Date(housingdf$sale_date, format="%d%m%Y"), "%Y")
  #this extracts the year of the sale and makes it a distinct variable



#     Use the aggregate function on a variable in your dataset

aggregate(all_bathrooms ~ year_built+ year_renovated, housingdf, median)
    #This tabulates the median number of bathrooms using the calculated bathroom number above

    # This shows pretty well that number of bathrooms increased over time especially when renovations were performed.


#     Use the plyr function on a variable in your dataset – more specifically, 
#     I want to see you split some data, perform a modification to the data, and then bring it back together
housingdf <- subset(housingdf, zip5 !=98059)  # because it is alone


hbyzip <- ddply(housingdf,  .(zip5), mutate ,  zipmean=mean(sq_ft_lot))  # mean lot size by zip code
housingdf <- hbyzip



#     Check distributions of the data
library(ggplot2)
ggplot(housingdf, aes(sq_ft_lot)) + geom_histogram(bins=20)
ggsave("sq_ft_histogram.jpg")

    #It's crazy- it is so positively skewed that we can't read the graph!


#     Identify if there are any outliers

    # Let's make a z-score of the lot sizes!

housingdf$zplot <- with(housingdf, (sq_ft_lot-mean(sq_ft_lot))/sd(sq_ft_lot))

housingdf <- housingdf[order(housingdf$zplot, decreasing = TRUE),]

housingdf[1:20,"zplot"]

    # Sure are- the z-scores show at least 20 observations with very large departures from mean.

