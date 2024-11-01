## Question 1
# a)
airpollution = read.csv("airpollution.csv", header = FALSE, skip = 2)
df = apply(airpollution, 2, str_split, boundary("word"))
df = df$V1

table(sapply(df, length))

for (a in 1:length(df)) {
  if (length(df[[a]]) == 14) {
    df[[a]] = c(paste(df[[a]][1],df[[a]][2]), df[[a]][3:14])
  }
}

df = df[-82]

df = data.frame(matrix(unlist(df), nrow = length(df), byrow=TRUE))
names(df) = df[1,]
df = df[-1,]
row.names(df) = 1:80
df = data.frame("CITY" = df[,1],apply(df[,-1], 2, as.double))

df



# (a)	(5 marks) Use R to produce a histogram of all the variables.  

par(mfrow = c(1, 1))

 for (a in 2:length(df)){
   hist(df[,a], xlab = colnames(df[a]), main = c("Histogram of", colnames(df[a])),
        col = "blue", breaks = 10)
 }

# (b)	(5 marks) Use R to produce Descriptive Statistics for all the variables.

summary.fun = function(x){
  round(c(summary(x), IQR = IQR(x), SD = sd(x), kurtosis = kurtosis(x), SK = skewness(x)),4)
}

df.summary = sapply(df[-1], summary.fun)


# (c)	(2 marks) Use R to produce boxplots describing the variables side by side. 
# This should be one picture. 

df %>% pivot_longer(col = -CITY, names_to = "Measures", values_to = "Results") %>%
  arrange(Measures) %>% ggplot(mapping = aes(y = Results, x = Measures, col = Measures))+
  geom_boxplot()+ labs(title = "Boxplot for each Variable", x = "Variables",
       color = "Variables", y = "Measurments")+
  scale_y_continuous(breaks = seq(0, 1500, 100))


#(e)	(4 marks) Which measures of central tendency and dispersion are the most appropriate 
# to numerically summarise the data? For full marks, justify your choice of measures, 
# interpret the corresponding values and display them in a table.

# Base on the fact that all variables (except NONPOOR) have outliers
# My choice of measure:
# if skeewnes is on the range of [-0.5, 0.5] we will consider data symmetric, otherwise it will
# be consider skewed and median and IQR will be the measurements of centrality and dispersion
as.data.frame(t(df.summary)) %>% 
  mutate("Centrality" = ifelse(abs(SK)>.5,paste("Median:",Median), paste("Mean:", Mean) ),
         "Dispersion" = ifelse(abs(SK)>.5,paste("IQR:", IQR), paste("SD:",SD) )) %>%
  select(Centrality, Dispersion) -> df.Measurments

# (f)	(4 marks) Use R to test the variables for Normality. 
# Briefly describe whether the data follows a Normal Distribution. 

Normality = sapply(df[-1], function(x) shapiro.test(x)[2])
df.Measurments = cbind(df.Measurments, "P.Value" = t(as.data.frame(Normality)))
df.Measurments %>%  
  mutate(P.Value = round(P.Value,5), 
         Distribution = ifelse(P.Value<.05, "Not Normally Dist", "Yes Normally Dist")) %>%
  select(P.Value, Distribution) -> df.distribution



# (g)	(5 marks) Based on part (f), consider the variable TMR that is approximately 
# Normally distributed. Use R to calculate the probability that the TMR will be more than 1200.

1 - pnorm(1200, mean(df$TMR), sd(df$TMR))


# (h)	(5 marks) Based on part (f), use R to calculate the probability that the 
# TMR will be less than 783.

pnorm(783, mean(df$TMR), sd(df$TMR))

# (i)	(5 marks) If a city measures in the lowest 5% of the cities, 
# what is the required TMR level? For full marks, show all your working out by hand, 
# provide a correct probability statement and include the R output to verify your answer.

z.value = qnorm(.05)
mean(df$TMR)+z.value*sd(df$TMR)



# (j)	(5 marks) By using R, produce five random samples of size 30 by randomly 
# selecting 30 values from the PMAX variable. Repeat for SMAX variable. 
# For full marks, provide a screenshot of your samples in a table format.  

sample.PMAX = as.data.frame(matrix(nrow = 30, ncol = 5))
sample.SMAX = as.data.frame(matrix(nrow = 30, ncol = 5))
for (i in 1:5) {
  sample.PMAX[,i]= as.data.frame(sample(df$PMAX, 30, replace = F))
  sample.SMAX[,i] = as.data.frame(sample(df$SMAX, 30, replace = F))
}
names(sample.PMAX) = paste("Sample:", c(1:5))
names(sample.SMAX) = paste("Sample:", c(1:5))


# (k)	(5 marks) Use R to produce the descriptive statistics for each sample 
# and store the information in another table, please ensure you state the mean 
# and standard deviation of each sample. Determine the sampling distribution 
#of means for PMAX and SMAX and state the parameters based off your first sample? 
# Justify your answer, quoting any theorems you used.

summary.fun = function(x){
  round(c(summary(x), IQR = IQR(x), SD = sd(x), kurtosis = kurtosis(x), SK = skewness(x)),4)
}
summary.S.PMAX = sapply(sample.PMAX, summary.fun)
summary.S.SMAX = sapply(sample.SMAX, summary.fun)

# PMAX Values
SDM.PMAX = mean(df$PMAX) # Sample Distribution Mean
SE.PMAX = sd(df$PMAX)/sqrt(30) # Standard Error

SDM.SMAX = mean(df$SMAX)  #Sample Distribution Mean
SE.SMAX = sd(df$SMAX)/sqrt(30) # Standard Error



# (l)	(5 marks) Calculate the probability that the average 
# PMAX is less than 150 based on your sampling distribution of the means 
# from part (k). For full marks, show all your working out by hand, provide a 
# correct probability statement and include the R output to verify your answer.

z_score = (150 - SDM.PMAX)/ SE.PMAX
pnorm(150, SDM.PMAX, SE.PMAX)
format(pnorm(z_score, 0, 1), scientific = F)


#(m)	 (5 marks) Construct and interpret the 95% confidence interval for the population
# mean for PMAX and SMAX based on the sample data. All calculations should 
#be done manually without using R, however, use R to visualise and verify the results.

# PMAX CI at 95%
summary.S.PMAX["Mean", 1] + c(-1.96, 1.96)*SE.PMAX

# SMAX CI at 95%
summary.S.SMAX["Mean", 1] + c(-1.96, 1.96)*SE.SMAX


# (n) Repeat the previous question for a 99% confidence interval for the population 
# mean based on the sample data. Compare and contrast the two confidence intervals 
# and comment whether the means of the original dataset airpollution for PMAX and SMAX 
# are included in these interval estimates. Justify your answer by providing calculations 
# for full marks.


# PMAX CI at 99%
summary.S.PMAX["Mean", 1] + c(-2.58, 2.58)*SE.PMAX

# SMAX CI at 99%
summary.S.SMAX["Mean", 1] + c(-2.58, 2.58)*SE.SMAX


# A health organisation is debating that there is no difference between 
# the variables PMAX and SMAX. Statistically test at a 5% level of significance 
# if there is a difference in the average of PMAX and SMAX. Give a verdict and 
# conclusion to your analysis.


df %>% select(CITY, PMAX, SMAX) %>%
  pivot_longer(col = -CITY, names_to = "Measures", values_to = "Results") %>%
  arrange(Measures) %>% ggplot(mapping = aes(y = Results, x = Measures, col = Measures))+
  geom_boxplot()+ labs(title = "Boxplot for PMAX & SMAX", x = "Variables",
                       color = "Variables", y = "Measurments")+
  geom_hline(aes(yintercept = mean(df$PMAX), linetype = "PMAX"), col = "red")+
  geom_hline(aes(yintercept = mean(df$SMAX), linetype = "SMAX"), col = "blue")+
  scale_linetype_manual(name = "Means", values = c(2, 2),
                        guide = guide_legend(override.aes = list(color = c("red", "blue"))))

mean.PMAX = mean(df$PMAX)
SD.PMAX = sd(df$PMAX)
mean.SMAX = mean(df$SMAX)
SD.SMAX = sd(df$SMAX)

t_statistic = (mean.PMAX - mean.SMAX)/
  sqrt((SD.PMAX^2/length(df$PMAX))+(SD.SMAX^2/length(df$SMAX)))


1 - (pt(t_statistic, 79) - pt(-t_statistic, 79))



# Just to prove
t.test(x = df$PMAX, y = df$SMAX, paired = F)




# They now believe that PMAX should be greater than SMAX in most cities on average.  
# Statistically test at the 10% level of significance whether this claim is true 
# based on the data collected. 
?t.test(x = df$PMAX, y = df$SMAX, conf.level = .90, paired = F )

t_value = qt(.95, 79)

(mean.PMAX - mean.SMAX) + c(-t_value, t_value) * sqrt((SD.PMAX^2/80)+(SD.SMAX^2/80))


# Another claim the health organisation is making is that ideally the average TMR 
# should be less than 1000. Statistically test at a 10% level of significance whether 
# the average TMR is less than 1000. Include the set-up of the null and alternative 
# hypotheses, a diagram and conclusion in your answer. 
# All calculations should be done without using R. 

t_value = qt(.95, 79)
t_value2 = qt(.1, 79)

t_statistic = (mean(df$TMR)-1000)/(sd(df$TMR)/sqrt(80))


pt(t_value2, 79)


mean(df$TMR) + c(-t_value, t_value)* (sd(df$TMR)/sqrt(80))
mean(df$TMR) + c(-t_value2, t_value2)* (sd(df$TMR)/sqrt(80))

t.test(x = df$TMR, mu = 1000, alternative = "less", conf.level = .90)


1.664 * 15.90
c(925.45 - 26.46, 925.45 + 26.46)
