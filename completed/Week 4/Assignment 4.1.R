# Test Scores
# A professor has recently taught two sections of the same course with only one difference between the sections. In one section, he used only examples taken from sports applications, and in the other section, he used examples taken from a variety of application areas. The sports themed section was advertised as such; so students knew which type of section they were enrolling in. The professor has asked you to compare student performance in the two sections using course grades and total points earned in the course. You will need to import the Scores.csv dataset that has been provided for you.

setwd("C:/users/pahme/onedrive/documents/github/dsc520")
sdata <- read.csv("data/scores.csv")
library(dplyr)

# Use the appropriate R functions to answer the following questions:
#   What are the observational units in this study?
str(sdata)

    # Seems to be the scores and the section types.


#   Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?
    # Scores: quantitative
    # Counts: quantitative
    # Section: categorical


#   Create one variable to hold a subset of your data set that contains only the Regular Section and one variable for the Sports Section.
reg.sections <- filter(sdata,Section=="Regular")
sport.sections <- filter(sdata, Section=="Sports")

str(reg.sections)

str(sport.sections)


# Use the Plot function to plot each Sections scores and the number of students achieving that score. Use additional Plot Arguments to label the graph and give each axis an appropriate label.

plot(reg.sections$Score, reg.sections$Count,xlim=c(200,400), xlab="Sum of Student Scores", ylab="Tally of Students", main ="Regular Sections")

plot(sport.sections$Score, sport.sections$Count,xlim=c(200,400), xlab="Sum of Student Scores", ylab="Tally of Students", main ="Sports Sections")


# Once you have produced your Plots answer the following questions:


  #   Comparing and contrasting the point distributions between the two section, looking at both tendency and consistency: Can you say that one section tended to score more points than the other? Justify and explain your answer.

        # First of all, to do any meaningful comparison between the two graphs, we need to scale them correctly, and in this case I would say that we need to set the low and high limits of the scores to the same (I chose 200 - 400); otherwise you cannot judge the low scores compared to the high scores. Having done so, it seems that the sports section scores were more scattered so it is hard to tell although they seem to be more hovered around the 300 point, while the regular section seemed to have more students around the 350 point, so - yes the regular section tended to score higher.  



# Did every student in one section score more points than every student in the other section? If not, explain what a statistical tendency means in this context.

    # Clearly not- some sports students scored higher than any of the regular students, but also some scored lower than any of the regular students. So the tendency means that the average of one is higher, or the median is higher.

# What could be one additional variable that was not mentioned in the narrative that could be influencing the point distributions between the two sections?

    # Well, possibly we would need to look at gender as an influencing variable (since women tend to be more aggressively sporty, or otherwise) or alternatively at the overall GPA for each section, since this might tend to be a self-selecting population breakdown with disparate means.
    
    