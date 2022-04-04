# Assignment: ASSIGNMENT 3
# Name: Pahmer, David
# Date: 2022-4-3

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory
setwd("C:/users/pahme/onedrive/documents/github/dsc520")

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data/r4ds/heights.csv")

# https://ggplot2.tidyverse.org/reference/geom_point.html
## Using `geom_point()` create three scatterplots for
## `height` vs. `earn`
ggplot(heights_df, aes(x=height, y=earn)) + geom_point()
ggsave("earn vs height.jpg")

## `age` vs. `earn`
ggplot(heights_df, aes(y=earn, x=age)) + geom_point()
ggsave("earn vs age.jpg")

## `ed` vs. `earn`
ggplot(heights_df, aes(y=earn, x=ed)) + geom_point()
ggsave("earn vs ed.jpg")


## Re-create the three scatterplots and add a regression trend line using
## the `geom_smooth()` function
## `height` vs. `earn`
ggplot(heights_df, aes(x=height, y=earn)) + geom_point() + geom_smooth()
ggsave("earn vs height and reg.jpg")


## `age` vs. `earn`
ggplot(heights_df, aes(y=earn, x=age)) + geom_point() + geom_smooth()
ggsave("earn vs age and reg.jpg")

## `ed` vs. `earn`
ggplot(heights_df, aes(y=earn, x=ed)) + geom_point() + geom_smooth()
ggsave("earn vs ed and reg.jpg")


## Create a scatterplot of `height`` vs. `earn`.  Use `sex` as the `col` (color) attribute
ggplot(heights_df, aes(x=height, y=earn, col=sex)) + geom_point()
ggsave("earn vs height and sex.jpg")

## Using `ggtitle()`, `xlab()`, and `ylab()` to add a title, x label, and y label to the previous plot
## Title: Height vs. Earnings
## X label: Height (Inches)
## Y Label: Earnings (Dollars)
ggplot(heights_df, aes(x=height, y=earn, col=sex)) + geom_point() + ggtitle("Earnings vs. Height") + 
  xlab("Height (inches)") + ylab("Earnings (Dollars)")
ggsave("earn vs height with labels.jpg")


# https://ggplot2.tidyverse.org/reference/geom_histogram.html
## Create a histogram of the `earn` variable using `geom_histogram()`
ggplot(heights_df, aes(earn)) + geom_histogram()
ggsave("earn histogram.jpg")

## Create a histogram of the `earn` variable using `geom_histogram()`
## Use 10 bins
ggplot(heights_df, aes(earn)) + geom_histogram(bins = 10)
ggsave("earn histogram 10 bins.jpg")


# https://ggplot2.tidyverse.org/reference/geom_density.html
## Create a kernel density plot of `earn` using `geom_density()`
ggplot(heights_df, aes(earn)) + geom_density()
ggsave("earn density.jpg")

