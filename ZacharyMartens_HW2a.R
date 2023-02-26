# Load libraries
library(tidyverse)
library(lubridate)
library(dplyr)

# load data
data = read_csv("restaurant_inspections.csv")

View(data[1:20,])


## Question 1 ##
ggplot(data, aes(x=data$SCORE)) + 
  xlim(80,100) + geom_histogram(binwidth=1) #histogram visualizing the overall distribution of inspection scores

## Question 2 ##
ggplot(data, aes(x=data$RESTAURANTOPENDATE, y=data$SCORE)) +
  ylim(80,100) + geom_point(size=1, shape=1) #scatter plot of the data, comparing restaurant open date to score on their inspections

# There doesn't seem to be a trend in terms of how highly older
# versus newer restaurants score on their inspections. The range
# of scores for older and newer restaurants are generally the same,
# as shown in a scatter plot of the data. However, six of the 
# lowest scores in the data set are attributed to relatively newer restaurants.

## Question 3 ##
unique(data$CITY) #check to see which city names need to be cleaned in the dataset
data$CITY = recode(data$CITY, "RESEARCH TRIANGLE PARK"="RTP", "Apex"="APEX", "Raleigh"="RALEIGH", "Zebulon"="ZEBULON",
                              "Fuquay Varina"="FUQUAY-VARINA", "Fuquay-Varina"="FUQUAY-VARINA", "Garner"="GARNER", "Holly Springs"="HOLLY SPRINGS",
                              "HOLLY SPRING"="HOLLY SPRINGS", "NORTH CAROLINA"="RALEIGH", "Wake Forest"="WAKE FOREST", "Morrisville"="MORRISVILLE",
                              "Cary"="CARY", "FUQUAY VARINA"="FUQUAY-VARINA") #clean city names
unique(data$CITY) #check to make sure all city names have been cleaned properly
ggplot(data, aes(x=data$CITY, y=data$SCORE)) +
  ylim(80,100) + geom_point(size=1, shape=1) #connect restaurant scores to cities

# Raleigh held the lowest 4 observed scores in the entire data set. Additionally, 
# Raleigh had the most variation in inspection scores out of any city in the data set.
# This is likely attributable to it being a hub for many restaurants (as it had the most
# values in the data set), leading to a capacity for more variation among a larger number
# of data points. Holly Springs had the highest (highest minimum) scores of the cities in
# the data set. Overall, the trend seems to be that the more restaurants that are present
# in a city, the better chance that some of the restaurants have an inspection score below 90.
# However, cities with fewer recorded restaurants appear to have higher average inspeciton scores.

## Question 4 ##
ggplot(data, aes(x=data$INSPECTOR, y=data$SCORE)) +
  ylim(80,100) + geom_point(size=1, shape=1) #scatter plot of scores for each inspector in the data set
ggplot(data, aes(x=data$INSPECTOR, y=data$SCORE)) +
  ylim(80,100) + geom_boxplot() #box plot of scores for each inspector in the data set

# The box plot is somewhat more informative than the scatter plot of the data, as it displays
# the median and quartiles of the inspection scores for each inspector. Additionally, the presence of more
# outliers in the scores of some inspectors speaks further to the distribution and variation of scores
# between inspectors. Some inspectors have very tightly distributed scores, and others have a large amount
# of outliers. Furthermore, one inspector in particular had a high amount of variety in inspection scores: 
# Jason Dunn. All of this said, inspection scores do vary significantly by inspector, with most inspectors
# having very different statistics (e.g. median, 1st and 3rd quartiles, outliers, etc.) than others. Some inspectors
# also seem to give very low scores with some frequency, whereas some other inspectors give exclusively higher
# scores to restaurants inspected.

## Question 5 ##
data %>% count(data$CITY) #table that displays counts of inspections in each city in the data set

data %>% count(data$INSPECTOR) #table that displays counts of inspections for each inspector in the data set

data$month_extract <- format(as.Date(data$DATE_, format="%d/%m/%Y"),"%m")
data %>% count(data$month_extract) #table that displays counts of inspections in each month of the year

data %>% count(data$INSPECTOR, data$CITY, data$month_extract) #table that displays counts of inspections for each inspector, in each city, in each month of the year

# There are particularly small sample sizes for the inspectors Brittny Thomas and Daryl Beasley, which leads to their median inspection scores
# to be significantly greater than those of most other inspectors in the dataset. Furthermore, The months of February through June have significantly
# less inspections than the rest of the months in the data set. Different seasons can have a particularly high impact on scores given. For
# example, the most inspections occur in the months of November, December, and January. Inspections in these months may have higher scores because of
# the general feelings of goodwill stemming from the holiday season, including Thanksgiving, Christmas, New Year's, and other significant holidays occurring
# during this time-frame. 

## Question 6 ##
facility_unique <- unique(data$FACILITYTYPE) #this line of code finds the names of unique facility types present in the dataset in the data$FACILITYTYPE column
facility_unique

facility_sum_score <- group_by(data, FACILITYTYPE) %>%
  summarize(sum_score=sum(SCORE)) #this line of code finds the fum of the scores for each unique facility type in the dataset
facility_sum_score

facility_sum_frame <- data.frame(facility_sum_score) #this line of code converts the above line into a data frame or table
facility_sum_frame
View(facility_sum_frame)

facility_sum_frame$number_facility <- data %>% count(data$FACILITYTYPE) #this line of code adds a column to the above table that inserts the counts of each unique facility type into the table within a new column

facility_sum_frame$number_facility$`data$FACILITYTYPE` <- NULL #this line of code drops an unnecessary column from the table that essentially repeats the FACILITYTYPE column

facility_sum_frame$mean_score <- facility_sum_frame$sum_score/facility_sum_frame$number_facility #this line of code adds a column to the table that calculates the mean score for each facility type by dividing the sum of each unique facility type's score by the number of inspections involving each particular facility type

# Among all of the facility types, the Restaurant designation has the lowest average score
# (not including NA). According to the table above, the Restaurant-designated objects have
# have an average score of 96.68, whereas the range of the average scores for all of the
# other facility types ranges from 96.90 - 99.25. The higher average on many of these facility
# types may perhaps result from the relatively few number that were inspected in comparison to the 
# 2,352 that were inspected for the Restaurant designation.

## Question 7 ##
data1 <- filter(data, data$FACILITYTYPE=="Restaurant") #this line of code drops all rows in the table where the facility type is NOT Restaurant, leaving new data set "data1"
View(data1)

#1
ggplot(data1, aes(x=data1$SCORE)) + 
  xlim(80,100) + geom_histogram(binwidth=1) #this line of code displays the distribution of scores for the Restaurant facility type

#2
ggplot(data1, aes(x=data1$RESTAURANTOPENDATE, y=data1$SCORE)) +
  ylim(80,100) + geom_point(size=1, shape=1) #this line of code displays the distribution of scores for the Restaurant facility type with regard to restaruant open date

#3
unique(data1$CITY) #check to see which city names need to be cleaned in the dataset
data$CITY = recode(data$CITY, "RESEARCH TRIANGLE PARK"="RTP", "Apex"="APEX", "Raleigh"="RALEIGH", "Zebulon"="ZEBULON",
                   "Fuquay Varina"="FUQUAY-VARINA", "Fuquay-Varina"="FUQUAY-VARINA", "Garner"="GARNER", "Holly Springs"="HOLLY SPRINGS",
                   "HOLLY SPRING"="HOLLY SPRINGS", "NORTH CAROLINA"="RALEIGH", "Wake Forest"="WAKE FOREST", "Morrisville"="MORRISVILLE",
                   "Cary"="CARY", "FUQUAY VARINA"="FUQUAY-VARINA") #clean city names
unique(data1$CITY) #check to make sure all city names have been cleaned properly
ggplot(data1, aes(x=data1$CITY, y=data1$SCORE)) +
  ylim(80,100) + geom_point(size=1, shape=1) #this line of code displays the distribution of inspection scores by city in Wake County

#4
ggplot(data1, aes(x=data1$INSPECTOR, y=data1$SCORE)) +
  ylim(80,100) + geom_point(size=1, shape=1) #scatter plot of scores for each inspector in the data set
ggplot(data1, aes(x=data1$INSPECTOR, y=data1$SCORE)) +
  ylim(80,100) + geom_boxplot() #box plot of scores for each inspector in the data set

#5
data1 %>% count(data1$CITY) #table that displays counts of inspections in each city in the data set

data1 %>% count(data1$INSPECTOR) #table that displays counts of inspections for each inspector in the data set

data1$month_extract <- format(as.Date(data1$DATE_, format="%d/%m/%Y"),"%m") #convert datetime into just month #, numbered 1-12 (for January-December)
data1 %>% count(data1$month_extract) #table that displays counts of inspections in each month of the year

data1 %>% count(data1$INSPECTOR, data1$CITY, data1$month_extract) #table that aggregates number of inspections by inspector, city, and month


# The analyses above are very similar for Restaurants as for the entire restaurant_inspections dataset as
# a whole. This is likely the case because the Restaurants facility type composes a majority of the observations
# contained in the original data set. The primary noticeable difference between the analysis for the Restaurants
# facility type and all of the combined facility types, was a somewhat lower average in the distribution of the
# scores of this facility type, as seen in the first part of the analysis where the distribution of the scores was
# plotted. Furthermore, only the inspectors Angela Myers, Cristofer LeClair, and Dipatrimarki Farkas have over 100
# inspection instances in the Restaurant facility type, so these inspectors may have a significant impact on
# a lower average inspection score among inspections of this facility type. Finally, over 1,000 of the inspections
# of the restaurant facility type were of facilities in Raleigh, NC, which would have potentially further factored
# into the lower average score for this facility type, as facilities in more urban areas such as Raleigh may be of a
# lower quality.