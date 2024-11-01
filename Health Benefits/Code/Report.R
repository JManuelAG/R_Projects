library(tidyverse)
library(rstatix)
library(moments)
library(effsize)
library(pwr)
library(gridExtra)


# ------------------------ Read the Data --------------------------------------

# Read the data and convert to NA the null values 
diary <- read_csv("./Fruit-Veg-diary.csv", na = "#NULL!")
intervention <- read_csv("./Fruit-veg-intervention.csv", na = "#NULL!")

# ------------------------ Custom Functions ------------------------------------

# Creating a Shapiro test function that can check on each treatment level for
# a desired factor
Normality_test <- function(factor, levels){
  result <- list()
  for (level in unique(levels)){
    to_test <- factor[levels == level]
    result[[level]] <- shapiro.test(to_test)
  }
  return(result)
}

# Creating a function that we will use for the measures of centrality and spread
summary.fun = function(x){
  round(c(min = min(x), max = max(x),
          median = median(x), mean = mean(x), SD = sd(x), kurtosis = kurtosis(x), 
          SK = skewness(x)),4)
}

# ------------------------ Cleaning & Wrangling --------------------------------

# Check if NA are present
diary %>% summarise(across(everything(), ~ sum(is.na(.))))
intervention %>% summarise(across(everything(), ~ sum(is.na(.))))

# look at the rows with NA
diary[!complete.cases(diary), ]
intervention[!complete.cases(intervention), ]

# For Diary I will drop NA as we can see that weekly recordings are inconsistent 
# between different ID
diary <- na.omit(diary)

# For intervention I will drop the 4 ID, I will keep record of dropped ID's
ID_dropped <- intervention$ID[!complete.cases(intervention)]
intervention <- na.omit(intervention)

# Create a difference field for Vitamin C and Cartenoids
intervention$VitC_diff <- intervention$VitC_post - intervention$VitC_pre
intervention$Carot_diff <- intervention$Carot_post - intervention$Carot_pre

# Convert to Factors
diary %>% convert_as_factor(ID, Diary) -> diary
intervention$intervention <-factor(intervention$intervention, 
                                   labels = c("Control", "EMI", "FVI"))
intervention %>% convert_as_factor(ID, gender, ancestry) -> intervention

# Create a long table for intervention
intervention[, c(-9,-10,-11,-12)] %>% 
  gather(key = "Factor", value = "Level", BMI:Carot_diff) -> intervention_L

# ------------------------ Descriptive Summary of Data ------------------------

# Summary Statistics of diary
summary(diary)

# Find how many weeks each ID recorded fruit intake
table(diary$ID)
diary %>% group_by(ID) %>% tally() %>% summarise(range(n))
# People recorded from 8 to 14 weeks of fruit intake

# Summary Statistics of intervention
summary(intervention)

# Stats of Factors
lapply(intervention[,5:14], summary.fun)


# ------------------------ Initial Visualizations -------------------------------

# General view on factors according to intervention
intervention_L %>% ggplot(aes(x=intervention, y=Level, col=intervention)) + 
  geom_boxplot() + 
  facet_wrap(vars(Factor), scales = "free") + 
  xlab("Intervention") +
  labs(title = "Resonse Variables Results",
       subtitle = "Groupped by each Intervention Lvl",
       col = "Intervention") +
  theme_bw()

# General view on factors distribution
intervention_L %>% ggplot(aes(x=Level, fill=intervention)) + 
  geom_density(alpha = .5) + 
  facet_wrap(vars(Factor), scales = "free") + 
  xlab("Intervention") +
  labs(title = "Distribution Density for Factors Results",
       subtitle = "Groupped by each Intervention Lvl",
       col = "Intervention") +
  theme_bw()

# Most factors look not normally distributed

# General view on factors with gender as a block  
intervention_L %>% ggplot(aes(x=intervention, y=Level, col=gender)) + 
  geom_boxplot() + 
  facet_wrap(vars(Factor), scales = "free") +
  xlab("Intervention") +
  labs(title = "Main Factors With Gender as Block ",
       subtitle = "Groupped by each Intervention Lvl",
       col = "Intervention") +
  theme_bw()

# We can see that gender makes a difference but lets see if is statistical significant

# dFV histogram distribution
diary %>% ggplot(aes(x=dFV)) + 
  geom_histogram(bins=10, color = "blue", fill = "red") +
  ylab("Frequency") +
  xlab("dFV Consumption") +
  labs(title = "Histogram of dFV Consumption per Day",
       subtitle = "For all records") +
  theme_bw()

# dFV histogram on total average consumption per member
diary %>% group_by(ID) %>% summarise("dFV" = mean(dFV)) %>% 
  ggplot(aes(x=dFV)) + 
  geom_histogram(bins=10, color = "blue", fill = "red") + 
  ylab("Frequency") +
  xlab("dFV Consumption") +
  labs(title = "Histogram of Total dFV Consumption",
       subtitle = "Average for all records") +
  theme_bw()

# dFV Average consumption across the 14 days
diary %>% group_by(Diary) %>% summarise(Total = mean(dFV)) %>%
  ggplot(aes(x=Diary, y=Total, group = 1)) + geom_line(col="red") +
  geom_point(col="blue") +
  ylab("Average dFV Consumption") +
  xlab("Day of Recording") +
  labs(title = "Average Daily dFV Consumption",
       subtitle = "Across the 14 days") +
  theme_bw()

# We can see that most of the average consumption decreasead from the first
# couple of days, we will figure it out if is the same for al treatments.

# ------------------------ BMI Visualization -----------------------------------

# Plot densities of the BMI according to intervention Lvl
intervention %>% ggplot(aes(x=BMI, fill=intervention)) + 
  geom_histogram(bins = 25) + 
  ylab("Frequency") +
  labs(title = "Histogram of BMI",
       subtitle = "Groupped by Intervention Lvl",
       fill = "Intervention") +
  theme_bw()

# Identify outliers by treatment
intervention %>% 
  group_by(intervention) %>%
  identify_outliers(BMI) -> BMI_outliers

# Remove Extreme outliers
int_BMI_Nout <- intervention[!(intervention$ID %in% 
                                 BMI_outliers$ID[BMI_outliers$is.extreme == T]),]

# We have two extreme outliers and will remove ONLY for the visuals


# Creating a Gender table for BMI 
int_BMI_Nout %>% group_by(gender) %>% 
  summarise(mean = mean(BMI), 
            se = sd(BMI)/sqrt(length(BMI))) -> gender_BMI_mse

# Creating an Intervention table for BMI 
int_BMI_Nout %>% group_by(intervention) %>% 
  summarise(mean = mean(BMI), 
            se = sd(BMI)/sqrt(length(BMI))) -> intervention_BMI_mse

# Creating a Intervention and Gender table for BMI 
int_BMI_Nout %>% group_by(intervention, gender) %>% 
  summarise(mean = mean(BMI), 
            se = sd(BMI)/sqrt(length(BMI))) -> gender_int_BMI_mse

# First visualization with gender on the total experimental units
dodge <- position_dodge(width=0.9)
ggplot(gender_BMI_mse, aes(x = gender, y = mean, fill = gender)) + 
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                position = dodge,
                width  = .6,
                size = .8) +
  coord_cartesian(ylim = c(22, 25)) +
  ylab("BMI") +
  labs(title = "Average BMI by Gender",
       x = "Gender Level",
       fill= "Gender") +
  theme_bw() -> bmi_1

# BMI between intervention groups   
ggplot(intervention_BMI_mse, 
              aes(x = intervention, y = mean, fill = intervention)) + 
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                position = dodge,
                width  = .6,
                size = .8) +
  coord_cartesian(ylim = c(22, 26)) +
  ylab("BMI") +
  xlab("Intervention Level") +
  labs(title = "Average BMI Between Intervention Groups",
       fill = "Intervention") + 
  theme_bw()  ->bmi_2

# BMI between intervention groups and gender  
ggplot(gender_int_BMI_mse, 
              aes(x = intervention, y = mean, fill = gender)) + 
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                position = dodge,
                width  = .6,
                size = .8) +
  coord_cartesian(ylim = c(22, 26)) +
  ylab("BMI") +
  xlab("Intervention Level") +
  labs(title = "Average BMI Between Intervention Groups",
       subtitle = "Groupped by Gender",
      fill = "Gender") + 
  theme_bw() -> bmi_3

# Place them together
grid.arrange(arrangeGrob(bmi_1, bmi_2, ncol=2), bmi_3,nrow = 2)

# We can confidently say is the same by gender and with gender as a block
# I can also say that in no difference between treatments but only a test will
# statistically determine, visually it looks the same

# ------------------------ Assumption check for all factors --------------------

# Creating a vector with factors we want to test 
factors <- c("BMI","happy","Energy","Fatigue","VitC_diff","Carot_diff")
factors_normality <- list()

# Normality test on all factors by treatment
for (factor in factors){
  factors_normality[[factor]] <- Normality_test(intervention[[factor]], 
                                                intervention$intervention)
}

# Check result
factors_normality

# Looks like we are going need to fix this, I will look at normality of each test
# CBD will be my fix, I don't like to transform, I find like forcing something to happen.

# Test homogeneity for all factors 
var_homogenity_intervention <- list()
for (factor in factors){
  var_homogenity_intervention[[factor]] <- bartlett.test(intervention[[factor]] ~ 
                                              intervention$intervention)
}


# Test homogeneity for gender 
var_homogenity_gender <- list()
for (factor in factors){
  var_homogenity_gender[[factor]] <- bartlett.test(intervention[[factor]] ~ 
                                              intervention$gender)
}

# check result
var_homogenity_intervention
var_homogenity_gender

# Not as bad but not perfect.

# ------------------------ Creating a CBD by gender ----------------------------

# Find the distribution of gender for each Treatment
intervention %>% group_by(intervention, gender) %>% 
  summarise(unique = n()) -> gender_design

# Check the minimum value for the gender block design
min(gender_design$unique)

# Create a complete block design for gender, couldn't functionalise it, it will
# sorry for the copy paste in future Analysis.
new_inds <- c()
for (i in 1:6) {
  w <- which(intervention$intervention == gender_design$intervention[i] & 
               intervention$gender == gender_design$gender[i])
  s <- sample(c(1:length(w)), size = 17, replace = F)
  new_inds <- c(new_inds, w[s])
}
cbd_intervention <- intervention[new_inds, ]

# Check if worked
cbd_intervention %>% group_by(intervention, gender) %>% summarise(n())

# I will also work with replication, as this still not normally distributed on 
# each level

# ------------------------ Analysis for Happiness and Gender Interaction -------

# Analysis
aov_happ <- aov(happy ~ intervention * gender, data = intervention)
summary(aov_happ)

# It looks to be an interaction using the full data, we know that this is not 
# totally correct by the broken assumptions. 

# Checking Normality of Residuals from original ANOVA model 
qqnorm(aov_happ$residuals)
qqline(aov_happ$residuals)

# Normality test
shapiro.test(aov_happ$residuals)

# normality is broken and we can't trust the test fully.


# Test multiple times with a CBD
p_values <- length(500)
for (test in 1:500){
  
  # Creating a new CBD
  new_inds <- c()
  for (i in 1:6) {
    w <- which(intervention$intervention == gender_design$intervention[i] & 
                 intervention$gender == gender_design$gender[i])
    s <- sample(c(1:length(w)), size = 17, replace = F)
    new_inds <- c(new_inds, w[s])
  }
  cbd_intervention <- intervention[new_inds, ]
  
  # Testing
  aov_results <- summary(aov(happy ~ intervention * gender, 
                             data = cbd_intervention))
  p_values[test] <- aov_results[[1]]$`Pr(>F)`[3]
}
# Result of replicates 
sum(p_values < .05)/length(p_values)

# This is interesting and only about 50% of the time is a difference
# this is due to the different pockets of results of happines across genders
# I will try to make it visually to understand


# Interaction plot
interaction.plot(intervention$intervention, intervention$gender, 
                 intervention$happy,
                 xlab = "Intervention Lvl", 
                 trace.label = "Gender", ylab = "Happines")

# We can see how it looks to be an interaction, sadly the data is not reliable enough
# To make it a statistical fact.

# Dot plot graph with a Box plot
intervention %>% ggplot(aes(x=intervention, y=happy, fill=gender)) + 
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(.9)) +
  geom_boxplot(position=position_dodge(0.8), alpha=.1, show.legend = F)  + 
  theme_bw()

# Dot plot graph with densities and quantiles 
intervention %>% ggplot(aes(x=intervention, y=happy, fill=gender)) + 
  geom_violin(alpha=.1, draw_quantiles = c(0.25, 0.75), linetype = "dashed",
              colour = "red", size = 1, show.legend = F) +
  geom_violin(alpha=.05, draw_quantiles = 0.5, size = 1, show.legend = F) +
  geom_dotplot(binaxis='y', stackdir='center', position=dodge, 
               alpha = .7) + 
  ylab("Hippnes Lvl") +
  xlab("Intervention") +
  labs(title = "Distribution Result of Happines",
       subtitle = "Groupped by Intervention\nQuantile divisions",
       fill= "Gender") +
  theme_bw()

# In the dot plot we can see how the Control for gender 1 is really bad distributed
# and cluster on results above the median and below

# ------------------------ Analysis for Energy and BMI interaction -------------

# Create an Ancova test to identify any interaction
ancova_model <- aov(Energy ~ intervention * BMI, data = intervention)
summary(ancova_model)
Anova(ancova_model, type=3)

# No interaction, I'm going to check normality of residuals to understand the 
# reliability of model

# Normality of residuals
shapiro.test(ancova_model$residuals)

# Residuals seem to be normal distributed


# Test a linear model with each intervention level as a baseline
# I'm doing this to find any statistical significance interaction
mylm <- list()
for (x in levels(intervention$intervention)){
  intervention$intervention <- relevel(intervention$intervention, ref = x)
  mylm[[x]] <- summary(lm(Energy ~ intervention * BMI, data = intervention))
}

# Check result
mylm

# Seems to be a bit of an interaction with the control compared to EMI, but
# not statistical significant. 

# Checking Normality of Residuals from Linear model 
qqnorm(mylm[[1]]$residuals, col="blue")
qqline(mylm[[1]]$residuals, col="blue")

# Residuals look normal distributed
hist(mylm[[1]]$residuals, xlab = "Residuals", ylab="Frequency", 
     main = "Histogram for Resiguals", col = "blue")

# Test normality of residuals
shapiro.test(mylm[[1]]$residuals)

# Graph for the interaction with Intervention and BMI fro Energy 
intervention %>% ggplot(aes(x=BMI, y=Energy, col=intervention)) + 
  geom_point() + geom_smooth(method='lm', show.legend = F, 
                             aes(fill=intervention), alpha = .08) + 
  labs(title = "Energy vs BMI",
       subtitle = "Groupped by Intervention Lvl\nWith Regression Lines",
       col="Intervention") +
  theme_bw()

# Here we can see how visually looks to be an increase on Enegry by BMI on the
# EMI, making it interact different to the other groups. At the same time
# we can see that the error difference make this not a statistical difference.

# ------------------------ Analysis for Fatigue with Gender as Block  ----------

# Analysis
aov_fatigue <- aov(Fatigue ~ intervention + gender, data = intervention)
summary(aov_fatigue)

# No really a difference

# Checking Normality of Residuals from original ANOVA model 
qqnorm(aov_fatigue$residuals)
qqline(aov_fatigue$residuals)

# Normality test
shapiro.test(aov_happ$residuals)

# The full data is not the best model

# Test multiple times with a CBD
p_values <- length(500)
for (test in 1:500){
  
  # Creating a new CBD
  new_inds <- c()
  for (i in 1:6) {
    w <- which(intervention$intervention == gender_design$intervention[i] & 
                 intervention$gender == gender_design$gender[i])
    s <- sample(c(1:length(w)), size = 17, replace = F)
    new_inds <- c(new_inds, w[s])
  }
  cbd_intervention <- intervention[new_inds, ]
  
  # Testing
  aov_results <- summary(aov(Fatigue ~ intervention + gender, 
                             data = cbd_intervention))
  p_values[test] <- aov_results[[1]]$`Pr(>F)`[1]
}
# Result of replicates 
sum(p_values < .05)/length(p_values)

# Only 10% of the time is a difference


# ------------------------ Analysis for Energy with Gender as Block  -----------
# Analysis
aov_energy <- aov(Energy ~ intervention + gender, data = intervention)
summary(aov_energy)

# Not at all 

# Checking Normality of Residuals from original ANOVA model 
qqnorm(aov_energy$residuals)
qqline(aov_energy$residuals)

# Normality test and broken on full ANOVA test
shapiro.test(aov_happ$residuals)

# Test multiple times with a CBD
p_values <- length(500)
for (test in 1:500){
  
  # Creating a new CBD
  new_inds <- c()
  for (i in 1:6) {
    w <- which(intervention$intervention == gender_design$intervention[i] & 
                 intervention$gender == gender_design$gender[i])
    s <- sample(c(1:length(w)), size = 17, replace = F)
    new_inds <- c(new_inds, w[s])
  }
  cbd_intervention <- intervention[new_inds, ]
  
  # Testing
  aov_results <- summary(aov(Energy ~ intervention + gender, 
                             data = cbd_intervention))
  p_values[test] <- aov_results[[1]]$`Pr(>F)`[1]
}
# Result of replicates 
sum(p_values < .05)/length(p_values)

# only a difference 4% of the time if lucky



# ------------------------ Analysis for Vitamin C for FVI group ----------------

# Simplify
FVI_VitC_pre <- intervention$VitC_pre[intervention$intervention == "FVI"]
FVI_VitC_post <- intervention$VitC_post[intervention$intervention == "FVI"]

# Simple paired T-Test
t.test(FVI_VitC_pre, FVI_VitC_post, paired = TRUE)

# No real difference

# Calculate Effect size
cd <- cohen.d(FVI_VitC_pre, FVI_VitC_post, paired = T)

# Calculate Power of Analysis
pwr.t.test(n= length(FVI_VitC_pre), d= cd$estimate, sig.level=0.05,
           type="paired")

# Really low power 

# ------------------------ Analysis for Vitamin C for EMI group ----------------

# Normality check for the group
EMI_VitC_pre <- intervention$VitC_pre[intervention$intervention == "EMI"]
EMI_VitC_post <- intervention$VitC_post[intervention$intervention == "EMI"]

# Simple paired T-Test
t.test(EMI_VitC_pre, EMI_VitC_post, paired = TRUE)

# Yay, finally we got something, a real statistical difference

# Calculate Effect size
cd <- cohen.d(EMI_VitC_pre, EMI_VitC_post, paired = T)

# Calculate Power of Analysis
pwr.t.test(n= length(EMI_VitC_pre), d= cd$estimate, sig.level=0.05,
           type="paired")

# Might be a difference but is low and only 5% practical difference. Also really
# low power 

# ------------------------ Analysis for Cartenoids for FVI group ----------------

# Normality check for the group
FVI_Cart_pre <- intervention$Carot_pre[intervention$intervention == "FVI"]
FVI_Cart_post <- intervention$Carot_post[intervention$intervention == "FVI"]

# Simple paired T-Test
t.test(FVI_Cart_pre, FVI_Cart_post, paired = TRUE)

# No difference at alpha 5%, close to be 

# Calculate Effect size
cd <- cohen.d(FVI_Cart_pre, FVI_Cart_post, paired = T)

# Calculate Power of Analysis
pwr.t.test(n= length(FVI_Cart_pre), d= cd$estimate, sig.level=0.05,
           type="paired")

# The difference still low and power is only 41%, not perfect

# ------------------------ Analysis for Cartenoids for EMI group ----------------

# Normality check for the group
EMI_Cart_pre <- intervention$Carot_pre[intervention$intervention == "EMI"]
EMI_Cart_post <- intervention$Carot_post[intervention$intervention == "EMI"]

# Wilcox Test
wilcox.test(EMI_Cart_pre, EMI_Cart_post, paired = TRUE)

# No difference by the non parametric test

# Calculate Effect size
cd <- cohen.d(EMI_Cart_pre, EMI_Cart_post, paired = T)

# Calculate Power of Analysis
pwr.t.test(n= length(EMI_Cart_pre), d= cd$estimate, sig.level=0.05,
           type="paired")


# ------------------------ Data prep for dFV across day 1, 7 and 14 ------------

# Sorry I'm more of a pandas thinker, Identify which ID had 14 days of diary
diary %>% group_by(ID) %>% summarise(total = n()) -> diary_weeks

# Only 44 out of the 171, that's a really big loss 75% !!!!

# Select only the ID with 14 days 
diary_weeks <- diary_weeks[diary_weeks$total == 14,] 

# Now we collect the ID values 
diary_14 <- diary[diary$ID %in% diary_weeks$ID, ]

# Now comes the fun, identify their Intervention level
diary_14 <- left_join(diary_14, intervention[,c("ID","intervention")])
diary_14 <- na.omit(diary_14)

# Lets hope is balanced enough 
diary_14 %>% group_by(ID, intervention) %>% 
  summarise(total = n()) %>% 
  group_by(intervention) %>%
  summarise(total = n()) -> diary_design


# Now we keep only the days we are interested 1, 7, 14
days <- c(1,7,14)

# First I save a new variable for days 1 to 14, I will use at the end
diary_1_to_14 <- diary_14

# Now only keep days 1, 7, 14
diary_14 <- diary_14[diary_14$Diary %in% days,]

# Vuala clean data
diary_14

# ------------------------ Assumptions check for dFV across day 1, 7 and 14 ----
# Assumptions check, yay!
diary_normality <- list()

# Normality test on all factors by treatment
for (level in unique(diary_14$intervention)){
  diary_normality[[level]] <- Normality_test(diary_14$dFV[diary_14$intervention == level], 
                                             diary_14$Diary)
}

# Check results
diary_normality

# Not as bad, we are working with paired data so the Anova test will check shpericity  


# Difference of variance
diary_homogenity <- list()
for (level in unique(diary_14$intervention)){
  diary_homogenity[[level]] <- bartlett.test(diary_14$dFV[diary_14$intervention == level],
                                               diary_14$Diary[diary_14$intervention == level])
}

# Check Results
diary_homogenity
  
# Check for outlier
diary_14 %>% group_by(intervention, Diary) %>% 
  identify_outliers(dFV)

# Last steps is just to understand the data a bit more but probably not fully needed

# ------------------------ Analysis for dFV across day 1, 7 and 14 -------------

# Boxplot
diary_14 %>% ggplot(aes(x = Diary, y = dFV, col = intervention)) +
  geom_boxplot()

# Is a paired data so we can't fully asume by this visuals, but gives an idea

# Point Plot and Linear models
diary_14 %>% ggplot(aes(x = Diary, y = dFV, fill = intervention)) +
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(.9), 
               alpha = .7) + geom_boxplot(alpha = .1, width = .2, 
                                          position=position_dodge(.9),
                                          show.legend = F) + 
  facet_wrap(vars(intervention)) +
  xlab("Day of Recording") +
  labs(title = "dFV Consumption per Unit",
       subtitle = "Across the [1, 7, 14] days",
       fill= "Intervention") +
  theme_bw()


# Paired ANOVA
aov_diary <- list()
for (level in unique(diary_14$intervention)){
  diary_14[diary_14$intervention == level,] %>%  
    anova_test(dv = dFV, wid = ID, within = c(Diary)) -> aov_diary[[level]]
  
}

# Check results
aov_diary

# Nothing statistical, Control stayed almos the same, FVI had a difference but not
# statistically, if we had more subjects maybe it could be. Sphericity broken in all 
# cases but not that badly 

# Get tables
for (level in aov_diary){
  print(get_anova_table(level))
}

# Differences by pairwise T-Test
diary_14 %>% group_by(intervention) %>%
    pairwise_t_test(dFV ~ Diary, paired = TRUE, 
                    p.adjust.method = "bonferroni")

# just to have an idea between changes, FVI change increase between day 1 and 14
# we can also see a reduction on consumption on the EMI group. But sadly again
# not statistically, more subjects next time or rigurosity as we lost so many subjects.


# Differences per day on intervention by Linear Model
mylm <- list()
for (level in unique(diary_14$intervention)){
  summary(lm(data = diary_14[diary_14$intervention == level,], 
             dFV ~ Diary)) -> mylm[[level]]
}

# Check results
mylm

# No differences statistically significant, but we can see the tendencies of before 
# in the coefficients, FVI consumption increases and EMI decreases


# Visual for 1, 7 and 14 days
diary_14$Day <- as.numeric(diary_14$Diary)
diary_14 %>% 
  ggplot(aes(x=Day, y=dFV, col=intervention)) +
  geom_point() + geom_smooth(method='lm', show.legend = F, 
                             aes(fill=intervention), alpha = .08) + 
  labs(title = "dFV vs Days [1 - 7 - 14]",
       subtitle = "Groupped by Intervention Lvl\nWith Regression Lines",
       col="Intervention") +
  scale_x_continuous(breaks=c(1,7,14)) + 
  theme_bw()

# Here the standard errors show why we can't see any statistical difference


# -------------------- Analysis of Average consumption of dFV per treatment ----

# Create an average total consumption per ID
diary_1_to_14 %>% 
  group_by(ID) %>%
  summarise(dFV_avg = mean(dFV), 
            intervention = unique(intervention)) -> diary_avg

# Check normality
Normality_test(diary_avg$dFV_avg, diary_avg$intervention)

#Yay finally something normal

# Check Variance homogenity
bartlett.test(diary_avg$dFV_avg, diary_avg$intervention)

# Great news, all assumptions pass

# Anova
diary_avg_aov <- aov(data = diary_avg, dFV_avg ~ intervention) 

#Almost 
summary(diary_avg_aov)

# thats close to be a difference with a p-value of 0.08

# Lm for differences
lm_avg_dFV <- lm(data = diary_avg, dFV_avg ~ intervention)
summary(lm_avg_dFV) # Interesting, but as ANOVA was used I will report Tukey

# This is interesting, here we have a statistical significant difference with the
# FVI and Control with a p-value of 0.288

# Normality Model
shapiro.test(lm_avg_dFV$residuals)

# Residuals are normal distributed

# Tukey test
tukey_ad<- TukeyHSD(diary_avg_aov)

# Now we don't have a difference, most liklely this is with the adjusment on the 
# Tukey test, I will report on this finding as we follwed an ANOVA test with no 
# assumptions broken 

# lets visualise
par(mar = c(1, 6, 2, 1)+ 2)
plot(tukey_ad, las = 1)


# Create a table with Average and Margin of Error
diary_avg %>% group_by(intervention) %>% 
  summarise(mean = mean(dFV_avg), 
            se = sd(dFV_avg)/sqrt(length(dFV_avg))) -> diary_dFV_mse

# Create a bar plot with error bars
ggplot(diary_dFV_mse, 
       aes(x = intervention, y = mean, fill = intervention)) + 
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                position = dodge,
                width  = .6,
                size = .8) +
  coord_cartesian(ylim = c(2, 4)) +
  ylab("Average consumption of dFV") +
  xlab("Intervention Level") +
  labs(title = "Average dFV Between Intervention Groups",
       subtitle = "Total consumption for all 14 days",
       fill = "Intervention") + 
  theme_bw() +
  theme(plot.subtitle=element_text(size=9)) -> dFV_1

# Line plot for all 14 days by Intervention
diary_1_to_14 %>% group_by(intervention, Diary) %>% summarise(Total = mean(dFV)) %>%
  ggplot(aes(x=Diary, y=Total, col=intervention, group=intervention)) + geom_line() +
  geom_point() +
  ylab("Average dFV Consumption") +
  xlab("Day of Recording") +
  labs(title = "Average Daily dFV Consumption",
       subtitle = "Across the 14 days\nGroupped by Intervention") +
  theme_bw() -> dFV_2

# Place them together
grid.arrange( dFV_1, dFV_2,nrow = 1)

# Here seems to be a difference as is no adjusments, is also interesting to see 
# the daily averages, more like a time series analysis. 

# --------------------- Analysis of Happiness with Ancestry and Interactions----
## I won't include this on the Report but I leave it here


# Find the distribution of gender for each Treatment
intervention %>% group_by(intervention, ancestry) %>% 
  summarise(unique = n()) -> ancestry_design

# We will have to create a new complet block desing by ancestry, with a level of
# 15 units

# Analysis
aov_happ_anc <- aov(happy ~ intervention * ancestry, data = intervention)
summary(aov_happ_anc)

# No difference on full data

# Checking Normality of Residuals from original ANOVA model 
qqnorm(aov_happ_anc$residuals)
qqline(aov_happ_anc$residuals)

# Normality is broken adain
shapiro.test(aov_happ_anc$residuals)

# Test multiple times with a CBD
p_values <- length(500)
for (test in 1:500){
  
  # Creating a new CBD
  new_inds <- c()
  for (i in 1:6) {
    w <- which(intervention$intervention == ancestry_design$intervention[i] & 
                 intervention$ancestry == ancestry_design$ancestry[i])
    s <- sample(c(1:length(w)), size = 15, replace = F)
    new_inds <- c(new_inds, w[s])
  }
  cbd_intervention <- intervention[new_inds, ]
  
  # Testing
  aov_results <- summary(aov(happy ~ intervention + ancestry, 
                             data = cbd_intervention))
  p_values[test] <- aov_results[[1]]$`Pr(>F)`[1]
}
# Result of replicates 
sum(p_values < .05)/length(p_values)

# No difference at all, maybe only one time in the 500 tests

# Interaction plot
interaction.plot(intervention$intervention, intervention$ancestry, 
                 intervention$happy,
                 xlab = "Intervention Lvl", 
                 trace.label = "Gender", ylab = "Happines")

# not much really, more drastic the lower on happiness by the gender 0 on control group

