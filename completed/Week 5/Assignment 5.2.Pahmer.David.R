# Week 5
# Assignment 5.2
# David Pahmer

# Using either the same dataset(s) you used in the previous weeks’ exercise or a brand-new dataset of your choosing, 
#   perform the following transformations (Remember, anything you learn about the Housing dataset in these two weeks 
#   can be used for a later exercise!)


library(readxl)
library(plyr)
library(dplyr)
library (purrr)

setwd("C:/users/pahme/onedrive/documents/github/dsc520")
housingdf <- read_xlsx("data/week-7-housing.xlsx")
str(housingdf)

# Using the dplyr package, use the 6 different operations to analyze/transform the data - GroupBy, Summarize, Mutate, 
#   Filter, Select, and Arrange – Remember this isn’t just modifying data, you are learning about your data also – so 
#   play around and start to understand your dataset in more detail

# Let's see what the building grade field is about. 
housingdf %>% group_by(building_grade) %>% 
  summarize(freq = n(), pr=mean(`Sale Price`))

# so it almost correlates to the sale price. But why not entirely? Maybe the homes that were renovated change things?
housingdf$renovated <- (housingdf$year_renovated > 0) #to distinguish renovations

pr_by_gr <-   housingdf %>%   group_by(renovated, building_grade ) %>% 
  summarize(freq = n(), pr=mean(`Sale Price`))

table(pr_by_gr)

# nope. So maybe the grade 6 is weird.
grade6 <- housingdf %>% filter(building_grade == 6)

grade6 %>% group_by(current_zoning) %>% summarise(f=n())

# maybe there is something weird about the current zoning breakdown for grade 6
gr_and_zone <- housingdf %>% group_by(building_grade, current_zoning) %>% summarize(freq=n())

housingdf %>% group_by(current_zoning) %>% summarise(f=n()) %>% print(n=Inf)
# This checks the tally for each zoning code


liv_space <- housingdf %>% select(building_grade, sq_ft_lot, square_feet_total_living) %>% 
  mutate(living_ratio=square_feet_total_living/sq_ft_lot) %>% 
  arrange(desc(living_ratio))
# This checks the ratios of living space to total property area, and sorts by ratio beginning with highest.


print(liv_space,n=sum(liv_space$living_ratio>1)) # which should be impossible, 
        #  since it shows living area greater than total property area!!



# Using the purrr package – perform 2 functions on your dataset.  You could use zip_n, keep, discard, compact, etc.


rep(10, 10) %>%
    map(sample, 5)%>%
  keep(~ mean(.x) > 6)

# Use the cbind and rbind function on your dataset

  #See next example

# Split a string, then concatenate the results back together

library(stringr)

# Lets delete the house number from the address:

address <- str_split(string=housingdf$addr_full, pattern = " " )
str(address)

addressDF <- data.frame(Reduce(rbind,address))

addressDF <- addressDF %>% select(-1) # This deletes the home number

addressDF$combined <- with(addressDF, paste(X2,X3,X4))
# MAybe it would have been smarter to name the columns, but it doesn't matter for this task.

housingdf <- cbind(housingdf,addressDF$combined)
# New column with the address minus the home number, in one column
