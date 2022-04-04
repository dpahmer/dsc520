# Assignment 3.2.2 sort of
# Pahmer, David
# 4-3-22

# American Community Survey Exercise
# 
# .	For this exercise, you will use the following dataset, 2014 American Community Survey. 
# This data is maintained by the US Census Bureau and are designed to show how communities are changing. 
# Through asking questions of a sample of the population, it produces national data on more than 35 categories of 
#   information, such as education, income, housing, and employment. For this assignment, you will need to load and 
#   activate the ggplot2 package. For this deliverable, you should provide the following:

setwd("C:/users/pahme/onedrive/documents/github/dsc520")
library(ggplot2)
theme_set(theme_minimal())

acs <- read.csv("data/acs-14-1yr-s0201.csv")

#     What are the elements in your data (including the categories and data types)?
        # Id, ID2, and Geography are categorical (nominal), 
        # PopGroupID and POPGROUP.Display.Label are also categorical (or nominal) but seem to be dummy variables
        # RacesReported is a mislabel of the population, which is a continuous (discrete, ratio)
        # HSDegree and BachDegree are continuous (also ratios)



#   i.	Please provide the output from the following functions: str(); nrow(); ncol()

str(acs)

# 'data.frame':	136 obs. of  8 variables:
#   $ Id                    : chr  "0500000US01073" "0500000US04013" "0500000US04019" "0500000US06001" ...
# $ Id2                   : int  1073 4013 4019 6001 6013 6019 6029 6037 6059 6065 ...
# $ Geography             : chr  "Jefferson County, Alabama" "Maricopa County, Arizona" "Pima County, Arizona" "Alameda County, California" ...
# $ PopGroupID            : int  1 1 1 1 1 1 1 1 1 1 ...
# $ POPGROUP.display.label: chr  "Total population" "Total population" "Total population" "Total population" ...
# $ RacesReported         : int  660793 4087191 1004516 1610921 1111339 965974 874589 10116705 3145515 2329271 ...
# $ HSDegree              : num  89.1 86.8 88 86.9 88.8 73.6 74.5 77.5 84.6 80.6 ...
# $ BachDegree            : num  30.5 30.2 30.8 42.8 39.7 19.7 15.4 30.3 38 20.7 ...
# > 



nrow(acs)

# [1] 136
# > 

ncol(acs)

# [1] 8


# .	Create a Histogram of the HSDegree variable using the ggplot2 package.
# 0.	Set a bin size for the Histogram.
# 1.	Include a Title and appropriate X/Y axis labels on your Histogram Plot.

ggplot(acs, aes(HSDegree)) + geom_histogram(binwidth = 1) +
  ggtitle("Frequencies of various Percentages of HS education per County") +
  xlab("Percentage of HS degree") + ylab("Number of Counties")
ggsave("histogram.jpg")


# iii.	Answer the following questions based on the Histogram produced:
#   0.	Based on what you see in this histogram, is the data distribution unimodal?

          # Actually, that seems to depend on the bin size. If the bin width is 0.1, then no, it isn't unimodal.
          # However, if I make it 0.5, then it is unimodal, but if I make it 1.0 then it is not. The actual raw data 
          # seem to be given to the nearest 0.1, so it is not.

#   1.	Is it approximately symmetrical?
          # no, although it might be called that if we ignore outliers.

#   2.	Is it approximately bell-shaped?
          # no, especially since the right side declines abruptly and there is a pronounced tail.

#   3.	Is it approximately normal?
          # no- and this is practically the same question as the previous one.

#   4.	If not normal, is the distribution skewed? If so, in which direction?
          # it has a left tail, so it is left-skewed, or negatively skewed.

#   5.	Include a normal curve to the Histogram that you plotted.
ggplot(acs, aes(HSDegree)) + geom_histogram(binwidth = 1) +
    stat_function(fun = function(x, mean, sd, n){
        n * dnorm(x = x, mean = mean, sd = sd)
        }, args = with(acs, c(mean = mean(HSDegree), sd = sd(HSDegree), n = length(HSDegree))))
ggsave("hist and norm.jpg")

# 6.	Explain whether a normal distribution can accurately be used as a model for this data.

        # Since the model would be built upon the assumption that the values are more frequent at the mean and 
        # drop in frequency as we move away from the mean, that assumption does not hold for these data, so our 
        # analysis would be invalid.


# iv.	Create a Probability Plot of the HSDegree variable.
qplot(sample=acs$HSDegree, stat="qq")
ggsave("probability plot.jpg")


# v.	Answer the following questions based on the Probability Plot:
#   0.	Based on what you see in this probability plot, is the distribution approximately normal? Explain how you know.

        # Not very normal, since a normal distribution would look more nearly straight 
        # along the bottom-left to top-right diagonal. Furthermore, this plot isn't even scaled to range from 0-100,
        # meaning it's heavily leaning above the means.


# 1.	If not normal, is the distribution skewed? If so, in which direction? Explain how you know.

        # as before, the values are strongly above the diagonal, suggesting a left skew. The Field book, however, 
        # seems to say that this shape implies a kurtosis abnormality.


# vi.	Now that you have looked at this data visually for normality, you will now quantify normality 
#     with numbers using the stat.desc() function. Include a screen capture of the results produced.

library(pastecs)
round(stat.desc(acs$HSDegree, basic=TRUE, norm=TRUE), digits=4)


# vii.	In several sentences provide an explanation of the result produced for skew, kurtosis, and z-scores. 
#     In addition, explain how a change in the sample size may change your explanation?
#   
          # The skewness value of -1.67 confirms that this distributions is negatively skewed. Likewise, kurtosis 
          # shows significant deviation from 0. Actually, the skewness/2SE statistic shows -4, which is much larger 
          # than the 1.65 that would imply acceptable normality, as does the kurtosis/2SE (4.4). 

          # If the sample size were greater, then the values of the z-scores of skewness and kurtosis would indicate
          # deviation from normality even when a reasonable assessment of the data would properly describe them as 
          # close enough to normal, judging by the actual density and the shape of the histogram.

