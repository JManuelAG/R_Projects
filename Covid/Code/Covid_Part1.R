# File: alvjy005_Assessment2.R
# Author: Jose Manuel Alvarez Gonzalez
# Student Id: 110364322
# Email Id:   alvjy005@mymail.unisa.edu.au
# Date:       9/2/2022


# loading the csv data in to a data frame of our name
Data1 = read_csv("Project_Data1.csv")

# Import the meaningful data, keep only the columns that we will use for the analysis 
Russia = Data1[Data1$location == "Russia", 3:5]

USA = Data1[Data1$location == "United States", 3:5]

# Creating a function that we will use for the measures of centrality and spread
summary.fun = function(x){
  round(c(min = min(x), max = max(x), IQR = IQR(x),
    median = median(x), mean = mean(x), SD = sd(x), kurtosis = kurtosis(x), 
    SK = skewness(x)),4)
}

# Add mortality rate column for each country data frame
Russia %>%
  mutate(mortality_rt = new_deaths/new_cases) -> Russia 

USA %>%
  mutate(mortality_rt = new_deaths/new_cases) -> USA


# Calculate the summaries for each data, creating a new list for each column 
Russia.summaries = lapply(na.omit(Russia[,-1]), summary.fun)
USA.summaries = lapply(na.omit(USA[,-1]), summary.fun)

# Creating a new variable that will have as a value the top outliers for each 
# column of the original data set. Formula that uses is (1.5 * IQR + 3 Quartile)
Russia.Out =  sapply(na.omit(Russia[,2:4]), function(x) quantile(x,probs = (0.75))) +
  c(Russia.summaries[[1]]["IQR"]*1.5, Russia.summaries[[2]]["IQR"]*1.5, Russia.summaries[[3]]["IQR"]*1.5) 
names(Russia.Out) = c("new_cases","new_deaths", "mortality_rate" )  # Result is a vector

USA.Out =  sapply(na.omit(USA[,2:4]), function(x) quantile(x,probs = (0.75))) +
  c(USA.summaries[[1]]["IQR"]*1.5, USA.summaries[[2]]["IQR"]*1.5, USA.summaries[[3]]["IQR"]*1.5)
names(USA.Out) = c("new_cases","new_deaths", "mortality_rate")


# Look at the linear model coefficient to understand death rates from new cases
# This is a good measurement to understand how new cases affect death rate, on a 
# linear model.
lm(Russia$new_deaths ~ Russia$new_cases)
model = lm(USA$new_deaths ~ USA$new_cases)
round(model$coefficients,6)



### Plots

## New Cases Plots

par(mfrow=c(1, 2))  # I want to be able to compare both plots at the same time

# Russia plot for the cases, it will also consider the mean and top outlier
# as a measure of centrality and spread.
plot(x = Russia$date, y = Russia$new_cases, main = "Russia Daily Covid Cases",
     xlab = "Time Range", ylab = "Population Numbers", col = "violet")
abline(h = c(Russia.summaries$new_cases["mean"], Russia.Out[1]), col = c("red", "blue"))
legend("topleft", legend = "Mean: 15,545", col = "red", lty = 1, cex = .8)

# USA - Same reference measurements than Russia
plot(x = USA$date, y = USA$new_cases, main = "USA Daily Covid Cases", 
     xlab = "Time Range", ylab = "Population Numbers", col = "violet")
abline(h = c(USA.summaries$new_cases["mean"], USA.Out[1]), 
       col = c("red", "blue"), lty = c(1, 2))
legend("topleft", legend = c("Mean: 76,635", "Outlier Range: 229,358"), 
       col = c("red", "blue"), lty = c(1, 2), cex = .8)

# USA - Creating an histogram for the second half of 2021, this will tell me
# were the majority of frequencies are located
hist(USA$new_cases[USA$date > "2021-6-1"], main = "USA Daily New Cases for \n Second Half of 2021",
     xlab = "Daily New Cases", col = "blue")
abline(v = USA.summaries$new_cases["mean"], col = "red" )
legend("topright", legend = "Mean: 76,635", col = "red", lty = 1, cex = .8)


## New Deaths

# Russia plot for the deaths, it will also consider the mean and top outlier
# as a measure of centrality and spread.
plot(x = Russia$date, y = Russia$new_deaths, main = "Russia Daily Covid Deaths",
     xlab = "Time Range", ylab = "Population Numbers", col = "violet")
abline(h = c(Russia.summaries$new_deaths["mean"], Russia.Out[2]), col = c("red", "blue"))
legend("topleft", legend = "Mean: 448", col = "red", lty = 1, cex = .8)

# USA - Same reference measurements than Russia
plot(x = USA$date, y = USA$new_deaths, main = "USA Daily Covid Deaths", 
     xlab = "Time Range", ylab = "Population Numbers", col = "violet")
abline(h = c(USA.summaries$new_deaths["mean"], USA.Out[2]), 
       col = c("red", "blue"), lty = c(1, 2))
legend("topleft", legend = c("Mean: 1,221", "Outlier Range: 3,577"), 
       col = c("red", "blue"), lty = c(1, 2), cex = .7)

# USA - Creating an histogram for the second half of 2021, this will tell me
# were the majority of frequencies are located
hist(USA$new_deaths[USA$date > "2021-6-1"], main = "USA Daily New Deaths for \n Second Half of 2021",
     xlab = "Daily New Deaths", col = "blue")
abline(v = USA.summaries$new_deaths["mean"], col = "red" )
legend("topright", legend = "Mean: 1,221", col = "red", lty = 1, cex = .8)


## Mortality Rates

# Russia plot for the deaths, it will also consider the median and top outlier
# as a measure of centrality and spread.
plot(x = Russia$date, y = Russia$mortality_rt, main = "Russia Covid Death Rate",
     xlab = "Time Range", ylab = "Death Rate New Cases", col = "violet")
abline(h = Russia.summaries$mortality_rt["median"], col = "red")
legend("topleft", legend = "Median Rate: 2.73%", col = "red", lty = 1, cex = .8)

# USA - Same reference measurements than Russia
plot(x = USA$date, y = USA$mortality_rt, main = "USA Covid Death Rate", 
     xlab = "Time Range", ylab = "Death Rate New Cases", col = "violet",
     ylim = c(0, 0.15))
abline(h = c(USA.summaries$mortality_rt["median"], USA.Out[3]), 
              col = c("red", "blue"), lty = c(1, 2))
legend("topleft", legend = c("Median Rate: 1.54%", "Outlier Range: 4.56%"), 
       col = c("red", "blue"), lty = c(1, 2), cex = .8)


## New_Deaths vs New_cases

# This plot compare the two original variables of cases and deaths for the USA
plot(USA$new_deaths ~ USA$new_cases, xlab = "New Cases", ylab = "New Deaths",
     main = "USA Covid: New Deaths vs New Cases", col = "violet")
abline(model, col = "blue")
legend("topleft", legend = "Regression line", 
       col = "blue", lty = 1, cex = .6)  # Adding a linear model as reference or relation 



