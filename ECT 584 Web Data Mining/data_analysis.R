setwd("C:/Users/ndokku/Desktop/ECT-584 Final Project")
# installing and loading packages

install.packages("scatterplot3d")
install.packages("ggmap")
install.packages("rgl")
library(tm)
library(topicmodels)
library(ggplot2)
library(MASS)
library(NLP)
library(readr)
library(dplyr)
library(scatterplot3d)
library(rgl)
#Loading Datasets 
user <- read.csv("yelp_academic_dataset_user.csv", stringsAsFactors=FALSE)
business <- read.csv("yelp_academic_dataset_business.csv",stringsAsFactors=FALSE)
review <- read.csv("yelp_academic_dataset_review.csv",stringsAsFactors=FALSE)
tip <-read.csv("yelp_academic_dataset_tip.csv",stringsAsFactors=FALSE)
checkin <-read.csv("yelp_academic_dataset_checkin.csv",stringsAsFactors=FALSE)


#  User Dataset Analysis 
cat("Dimension:\n"); print(dim(user))
cat("Summary of user dataset:\n"); print(summary(user))
# business Dataset Analysis
cat("Dimension:\n"); print(dim(business))
cat("Summary of user dataset:\n"); print(summary(business))
# reviews Dataset Analysis 
cat("Dimension:\n"); print(dim(review))
cat("Summary of user dataset:\n"); print(summary(review))
# checkins dataset Analysis
cat("Dimension:\n"); print(dim(checkin))
cat("Summary of user dataset:\n"); print(summary(checkin))
# tips dataset analysis
cat("Dimension:\n"); print(dim(tip))
cat("Summary of user dataset:\n"); print(summary(tip))


#-------------------------**********---------------------
library(MASS)

avg_stars <- user$average_stars

hist(avg_stars, xlab="Average Stars", ylab="Count", main="Average Star Rating of Users", breaks=15, col="blue", prob=TRUE)

normal_param <- fitdistr(avg_stars, "normal")
#chisq_param <- fitdistr(avg_stars, "chi-squared", start=list(df=4.6))
#cat("Chi-squared parameter:\n"); print(chisq_param)
curve(dnorm(x, mean=normal_param$estimate[1], sd=normal_param$estimate[2]), col="green", add=TRUE)
#curve(dchisq(x, df=4.6), col="red", add=TRUE)
legend("topleft", c("Normal"), col=c("green"), lwd=3 )

hist(user$review_count, xlab="Review Count", ylab="Frequency", main="Review Count Frequency", breaks=100, col="blue", prob=TRUE)
cat("Summary of user.review_count:\n"); print(summary(user$review_count))

# Trim review_count to remove outliers
review_count_lt200 <- user$review_count[user$review_count < 200]
cat("Percent covered (w/ count < 500):\n"); print(length(review_count_lt200)/length(user$review_count))
hist(review_count_lt200, xlab="Review Count", ylab="Frequency", main="Review Count Frequency (Count < 500)", breaks=100, col="blue", prob=TRUE)


#-------------------------------**************------------------
#General Trend Analysis fro review dataset


review$length <- (nchar((review$text)))
# Efficient way to replace non-positive values with 1
review$length <- (review$length + abs(review$length) + 2) / 2
cat("New matrix dimension (after adding length column): "); print(dim(review))

# Separate review dataset based on star rating
review_1stars <- subset(review, stars==1)
review_2stars <- subset(review, stars==2) 
review_3stars <- subset(review, stars==3)
review_4stars <- subset(review, stars==4)
review_5stars <- subset(review, stars==5)

# Look at rating (stars) distribution
# Distribution is obviously skewed. People tend to write positive reviews
counts <- c(dim(review_1stars)[1], dim(review_2stars)[1], dim(review_3stars)[1], 
            dim(review_4stars)[1], dim(review_5stars)[1])
barplot(counts, xlab="Star Rating", ylab="Count", main="Number of Reviews with Star Ratings 1-5", col="blue")

# Correlation between review length and star rating?
hist(review_1stars$length, xlab="Length", ylab="Count", main="Count of 1-Stars Review Lengths", 
     breaks=50, col="blue", prob=TRUE)
param_1star <- fitdistr(review_1stars$length, "log-normal")
curve(dlnorm(x, meanlog=param_1star$estimate[1], sdlog=param_1star$estimate[2]), col="red", add=TRUE)
legend("right", c("Log-normal"), col=c("red"), lwd=3)

hist(review_3stars$length, xlab="Length", ylab="Count", main="Count of 3-Stars Review Lengths", 
     breaks=50, col="blue", prob=TRUE)
param_3star <- fitdistr(review_3stars$length, "log-normal")
curve(dlnorm(x, meanlog=param_3star$estimate[1], sdlog=param_3star$estimate[2]), col="red", add=TRUE)
legend("right", c("Log-normal"), col=c("red"), lwd=3)

hist(review_5stars$length, xlab="Length", ylab="Count", main="Count of 5-Stars Review Lengths", 
     breaks=50, col="blue", prob=TRUE)
param_5star <- fitdistr(review_5stars$length, "log-normal")
curve(dlnorm(x, meanlog=param_5star$estimate[1], sdlog=param_5star$estimate[2]), col="red", add=TRUE)
legend("right", c("Log-normal"), col=c("red"), lwd=3)
# nearly identical log-normal distributions
cat("1 star log-normal fit parameters:" );print(param_1star$estimate) # nearly identical means and standard deviations suggesting
cat("3 star log-normal fit parameters:" );print(param_3star$estimate) # we cannot classify stars based on review length
cat("5 star log-normal fit parameters:" );print(param_5star$estimate)


#-------------------------*******************----------------------
#Topic Modeling
#Split data into 90% training and 10% testing

# Find indices of business with "restaurant" as category
loc <- grep("Restaurant", business$categories, perl=TRUE, value=FALSE)
business_subset <- business[loc,]

# Select only restaurant reviews
review_restaurant <- subset(review, review$business_id %in% business_subset$business_id &
                                   nchar(toString(text)) > 100)
cat("Number of restaurant reviews: "); print(dim(review_restaurant))
print(dim(review_restaurant)[1]*9/10)

# Separate into training and testing set 
index <- sample(nrow(review_restaurant), 200000)
review_restaurant.train <- review_restaurant[index, ]
review_restaurant.test <- review_restaurant[-index, ]
cat("Training set: "); print(dim(review_restaurant.train))
cat("Testing set: "); print(dim(review_restaurant.test))

