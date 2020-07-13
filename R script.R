knitr::opts_chunk$set(echo = TRUE)
# reading the data
advert <- read.csv("~/R/Introduction to R basics-EDA/advertising.csv")
head(advert)
#summary of the dataset
summary(advert)
# information about the dataset
str(advert)
#Data Cleaning 
#Checking for missing values
colSums(is.na(advert))
#Checking for duplicated
sum(duplicated(advert))
#checking for unique values
unique(advert)
#checking for outliers
colnames(advert, do.NULL = TRUE, prefix = "col")
num_cols <- advert[, c("Daily.Time.Spent.on.Site", "Age", "Area.Income", "Daily.Internet.Usage")]
boxplot(num_cols)
#Displaying the outliers in area.income column
boxplot(advert$Area.Income,
        main = "Outliers in Area.Income",
        xlab = "Area.Income",
        col = "maroon",
        border = "orange",
        horizontal = TRUE,
        notch = TRUE
)
#droping Ad.Topic.Line column
subset(advert, select = -c(Ad.Topic.Line))


#EXPLORATORY DATA ANALYSIS 
#univariate analysis
# mean of the daily time spent on site
mean(advert$Daily.Time.Spent.on.Site)

# Mean daily internet usage
mean(advert$Daily.Internet.Usage)


# median of the daily time spent on site
median(advert$Daily.Time.Spent.on.Site)

# Median daily internet usage
median(advert$Daily.Internet.Usage)

#creating the mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#mode of daily internet usage  
getmode(advert$Daily.Internet.Usage)


#creating the mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#mode of daily time spent on site   
getmode(advert$Daily.Time.Spent.on.Site)

### Measures of Dispersion
#### Minimum and Maximum
#minimum and maximum of age
min(advert$Age)
max(advert$Age)

#minimum and maximum of area.income
min(advert$Area.Income)
max(advert$Area.Income)

#range in Daily.Internet.Usage
range(advert$Daily.Internet.Usage)

#range in daily time spent on site
range(advert$Daily.Time.Spent.on.Site)

#Quantiles in Age
quantile(advert$Age)

#Quantiles in Area income
quantile(advert$Area.Income)

# variance of internet usage
var(advert$Daily.Internet.Usage)

# variance of Daily.Time.Spent.on.Site
var(advert$Daily.Time.Spent.on.Site)

#Sd in Age
sd(advert$Age)

#sd in Area income
sd(advert$Area.Income)

### Histograms
# Histogram of daily time spent on site
hist(advert$Daily.Time.Spent.on.Site)

### Bar graph
#Bar graph of daily internet usage
barplot(advert$Daily.Internet.Usage)

### Pie Chart
#pie chart of gender
pie(table(advert$Male))

### Bivariate Analysis
### covariance and correlation

# covariance between Daily.Time.Spent.on.Site AND clicked on ad
Time <- advert$Daily.Time.Spent.on.Site
ad <- advert$Clicked.on.Ad 
#getting the covariance between the two variables
cov(Time, ad )
#getting the ccorrelation coefficient 
cor(Time, ad)





## Scatter plots
plot(Time, ad, xlab="Time spent on the internet", ylab="clicked on ad")


# Scatterplot
timespent <- advert$Daily.Time.Spent.on.Site
internetusage<- advert$Daily.Internet.Usage
plot(timespent, internetusage, xlab="Daily time spent on site", ylab="Daily Internet Usage")

plot(Area.Income ~ Daily.Internet.Usage, data = advert)

### Grouped Bar Plot
counts = table(advert$Male, advert$Clicked.on.Ad)
barplot(counts, main="number of Clicks on an Ad as per each sex, 0=Female, 1=male",
        xlab="Ad Clicks", col=c("green","red"),
        legend = rownames(counts), beside=TRUE)

counts = table(advert$Age, advert$Clicked.on.Ad)
barplot(counts, main="number of Clicks on an Ad as per Age",
        xlab="Ad Clicks", col=c("blue","yellow"),
        legend = rownames(counts), beside=TRUE)


###CORRELATION MATRIX


col <- advert[, c("Daily.Time.Spent.on.Site", "Age", "Area.Income", "Daily.Internet.Usage","Clicked.on.Ad")]
cor<- cor(col, method = c("spearman"))
cor
# install packages
install.packages("Hmisc")

install.packages("corrplot")

library("corrplot")
corrplot(cor, method="color")

