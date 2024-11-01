# File: alvjy005_Assessment2_Part2.R
# Author: Jose Manuel Alvarez Gonzalez
# Student Id: 110364322
# Email Id:   alvjy005@mymail.unisa.edu.au
# Date:       15/3/2022

library(tidyverse)
library(ggrepel)
library(scales)
library(e1071)

# Read the data 
df1 <- read_csv("Project_Data1.csv") 
df2 <- read_csv("Project_Data2.csv")


# Create a variable for the countries we will work
countries <- c("Australia", "China", "India", "Sweden", "Russia",
               "United Kingdom", "United States")

# Create a new data set for the countries we will work 
Data1 = df1[df1$location %in% countries,]
Data2 = df2[df2$location %in% countries,]

# Checking how many and where NA are present
Data1 %>% filter(is.na(new_cases)) 

# Create a cumulative plot, it will for each country at the moment they got 1,000 cases
Data1 %>% select(location, date, new_cases) %>%
 na.omit %>%  group_by(location) %>% 
  # This part of the code creates labels at the end of each line
  mutate(cumulative = cumsum(new_cases), 
         label = if_else(date == max(date), as.character(location), NA_character_))  %>% 
  filter(cumulative > 1000) %>% # Starting the plot for each country after reaching 1,000 cases 
  ggplot(mapping = aes(x = date, y = cumulative, col = location))+
  geom_line() + 
  scale_y_continuous(labels = scales::comma, breaks = 10^seq(1,10), trans = "log10")+
  scale_x_date(date_breaks = "3 months") + 
  geom_text_repel(aes(label = label), nudge_x = 1, na.rm = TRUE, show.legend = F)+
  labs(title = "Cumulative Cases Count - Log10 scale",
       subtitle = "Cumulative COVID-19 cases by country\n
       starting from a total of 1,000 cases",
       x = "Start of 2020 to end of 2021",
       y = "Cumulative cases (log10-scale)",
       color = "Country") +
  theme_bw()

# Merge Data1 and Data2 and create a new variable for new cases per million people
merge(Data1, Data2, by = "location") %>% 
  select(location, new_cases, population) %>% na.omit %>% group_by(location) %>% 
  mutate(new_cases_perM = (new_cases/(population/1000000))) %>%
  select(location, new_cases_perM) -> df.cases_perM

# Descriptive Statistics for each country new variable on cases per million people
df.cases_perM %>% summarise(Min = min(new_cases_perM),
                            Max = max(new_cases_perM),
                            Q1 = quantile(new_cases_perM, .25),
                            Q3 = quantile(new_cases_perM, .75),
                            IQR = IQR(new_cases_perM),
                            Median = median(new_cases_perM),
                            Mean = mean(new_cases_perM),
                            SD = sd(new_cases_perM), 
                            Kurtosis = kurtosis(new_cases_perM), 
                            SK = skewness(new_cases_perM)) %>%
  mutate_at(-1, round, 2) -> summary.cases_perM 

# Boxplot for new variable to understand distribution
df.cases_perM %>%
  ggplot(mapping = aes(x = new_cases_perM,  fill = location))+
  geom_boxplot() + 
  facet_wrap( vars(location), scales = "free", nrow = 7)+
  scale_y_continuous(breaks = NULL)+
  labs(title = "Boxplot Distribution for COVID-19 Daily Cases",
       subtitle = "Cases per million people by country ",
       x = "Measurment of daily cases per million ppl",
       fill = "Country")+
  theme_bw() 

# Density plot for new variable to understand distribution  
df.cases_perM %>%
  ggplot(mapping = aes(x = new_cases_perM, fill = location))+
  geom_density() + 
  facet_wrap( vars(location), scales = "free", nrow = 7)+
  labs(title = "Density Distribution for COVID-19 Daily Cases",
       subtitle = "Cases per million people by country ",
       x = "Measurment of daily cases per million ppl",
       y = "Density",
       fill = "Country")+
  theme_bw()

# Merge original DF1 and DF2
merge(df1, df2, by = "location")  %>% # We can only work with countries available on both data sets
  # Here I remove the locations that are not countries
  filter(!is.na(continent.x)) -> Merge.Data1_2 # New variable we will use again

Merge.Data1_2 %>% filter(is.na(population)) # Checking for NA in the data, will need to omit

# Select the variables we will work with and new variable
# with the total deaths per million people
Merge.Data1_2 %>%
  select(location, new_deaths, median_age, population) %>% 
  na.omit %>% # Remove NA values, as we are doing totals 
  group_by(location) %>% 
  summarise(total_deaths_perM = sum(new_deaths)/(mean(population)/1000000),
            median_age = mean(median_age)) -> df.total_deaths # Create variable with result

# Create a dot plot for each country where data is available
# divide the plot in four quadrants to understand if is any increase on values
df.total_deaths %>% 
  ggplot(aes(x = median_age, y = total_deaths_perM))+
  geom_point(col = "blue") + 
  geom_hline(aes(yintercept = median(total_deaths_perM), linetype = "Median Deaths"), col = 2)+
  geom_vline(aes(xintercept = median(median_age), linetype = "Total Median Age"), col = 2) +
  scale_linetype_manual(name = "Middle Values", values = c(2, 2),
                        guide = guide_legend(override.aes = list(color = c(2, 2))))+
  scale_y_continuous(labels = scales::comma, breaks = seq(0,6000,1000))+
  scale_x_continuous(breaks = seq(0,60,5)) +
  labs(title = "Relationship in Total Deaths and Median Age",
       subtitle = "By each country",
       x = "Country Median Age",
       y = "COVID-19 Total Deaths Per Million People",
       colour = "Country") +
  theme_bw() -> P.deathVage # Create a variable as we will use it again

# Run a correlation analysis
cor(df.total_deaths[-1])

# Create a linear model that will predict total deaths from median age
Deaths_Age_Model <- lm(total_deaths_perM~median_age, df.total_deaths) 

# look at the summary and relevance of the coefficients of the model
summary(Deaths_Age_Model)

# Plot the preivious grapgh with a linear regression line
P.deathVage + geom_smooth(method = "lm", colour = "red") + 
  labs(subtitle = "Fitted regression line")



# Work with the previous merge of Data1 and Data2
# Select the variables we will use and remove the variables missing entries
# Summarise the totals and create a new category according to DGP level
Merge.Data1_2 %>% 
  select(location, new_vaccinations, gdp_per_capita, median_age, population) %>%
  na.omit %>% # We will have to omit countries were data is not available 
  group_by(location) %>%
  summarise(total_vaccines = sum(new_vaccinations), gdp = mean(gdp_per_capita), 
            median_age= mean(median_age), ppl = mean(population)) %>%
  #Here I divide in categories
  mutate(gdp_level = if_else(gdp < median(gdp), "Poor", 
                             if_else(gdp < quantile(gdp, .75), "Average", "Rich"))) %>%
  group_by(gdp_level) %>% arrange(gdp_level) %>% 
  summarise(median_gdp = median(gdp), median_ppl = median(ppl), median_age = median(median_age),
    total_vaccines_perMppl = sum(total_vaccines)/(sum(ppl)/1000000),
                          total_vaccines = sum(total_vaccines), 
                          total_ppl = sum(ppl)) -> Data.GDPlvl.Totals #Statistical summaries of the new GDP categorical data set

# We will use the previous totals and plot each variable to see differences
# Here I will create the set up we will use for plots
Data.GDPlvl.Totals %>% 
ggplot(mapping = aes(x = gdp_level, fill = gdp_level))+
  scale_y_continuous(labels = scales::comma)+
  labs(fill = "GDP Category", x = "GDP Category")+
  theme_bw()-> Plot.GDPlvl #

#Bar plot for Total Number of Vaccines
Plot.GDPlvl + geom_bar(aes(weight = total_vaccines))+
  labs(title = "Total COVID-19 Vaccinations by GDP Category",
       y = "Number of COVID-19 Vaccinations")

#Bar plot for Total Population
Plot.GDPlvl + geom_bar(aes(weight = total_ppl))+
  labs(title = "Total Population by GDP Category",
       y = "Number of People")

#Bar plot for Vaccines per Million People
Plot.GDPlvl + geom_bar(aes(weight = total_vaccines_perMppl))+
labs(title = "COVID-19 Vaccinations by GDP Category",
     subtitle = "Per one million people",
     y = "COVID-19 Vaccinations (per million poeple)")



