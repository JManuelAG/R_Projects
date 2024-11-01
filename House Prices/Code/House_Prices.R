# Assignment 1.2
# I know I'm not loading libraries 

# Read DF
df <- read_csv("./melb_houses.csv")

# Keep only the useful stuff
df<- df[,c("MetroRegion","MultipleBath","Price")]

# Convert to factor
df$MultipleBath <- factor(df$MultipleBath)
df$MetroRegion <- factor(df$MetroRegion)

table(df$MetroRegion)
table(df$MultipleBath)

# ---------- Part a - bar plot diagram with error bars ------------------------
# Get the Mean and SE for each factor
df %>% group_by(MetroRegion) %>% 
  summarise(mean = mean(Price), 
            se = sd(Price)/sqrt(length(Price))
  ) -> region_mean_and_se

df %>% group_by(MultipleBath) %>% 
  summarise(mean = mean(Price), 
            se = sd(Price)/sqrt(length(Price))
  ) -> bath_mean_and_se

df %>% group_by(MetroRegion, MultipleBath) %>% 
  summarise(mean = mean(Price), 
            se = sd(Price)/sqrt(length(Price))
  ) -> df_mean_and_se

# Plot each one

dodge <- position_dodge(width=0.9)

p1 <- ggplot(region_mean_and_se, aes(x = MetroRegion, y = mean)) + 
  geom_bar(stat = "identity", fill = "#F8766D") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                position = dodge,
                width  = .6,
                size = .8) +
  scale_y_continuous(labels = scales::dollar_format(), 
                     breaks = seq(0,2000000,250000)) +
  ylab("Mean House Prices in AUD") +
  labs(subtitle = "Groupped by Region",
       x = "Metropolitan Region") +
  theme_bw() 
  

p2 <- ggplot(bath_mean_and_se, aes(x = MultipleBath, y = mean)) + 
  geom_bar(stat = "identity", fill = "#F8766D") +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                position = dodge,
                width  = .6,
                size = .8) +
  scale_y_continuous(labels = scales::dollar_format(), 
                     breaks = seq(0,2000000,250000)) +
  ylab(NULL) +
  labs(subtitle = "Groupped by Bathrooms",
       x = "Multiple Bathrooms") +
  theme_bw() 

p1 + p2 + 
  plot_annotation(title = "Average Melbourne House Prices") &
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle=element_text(size=10))



df_mean_and_se %>% ggplot(aes(x = MetroRegion, y = mean, fill = MultipleBath)) + 
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                position = dodge,
                width  = .6,
                size = .8) +
  scale_y_continuous(labels = scales::dollar_format(), 
                     breaks = seq(0,2000000,250000)) +
  ylab("Mean House Prices in AUD") +
  xlab("Melbourne Metropolitan Region") +
  labs(title = "Average Melbourne House Prices",
       subtitle = "Groupped by Region and Multiple Bathrooms",
       fill = "Multiple \nBathrooms") + 
  theme_bw() +
  theme(plot.subtitle=element_text(size=9))


# ----------------- Part b ANOVA ----------------------------------------------

# Transform
df$log_prices <- log(df$Price)

# Check Normality
for (x in levels(factor(df$MetroRegion))){
  print(x)
  print(
    shapiro.test(df$log_prices[df$MetroRegion == x])
  )
}

for (x in levels(factor(df$MultipleBath))){
  print(x)
  print(
    shapiro.test(df$log_prices[df$MultipleBath == x])
  )
}

# Check equal variability
bartlett.test(data = df, log_prices ~ MetroRegion)
bartlett.test(data = df, log_prices ~ MultipleBath)

# Create a Linear Model and see R values
linear_model <- lm(data = df, log_prices ~ MetroRegion * MultipleBath)
summary(linear_model)

# Create an ANOVA model and look their table
summary(lm(data = df, log_prices ~ MultipleBath))

m_anova <- aov(linear_model)
summary(m_anova)

# ----------------- Part c Interaction Plot -----------------------------------

with(df, {interaction.plot(MetroRegion, MultipleBath, Price,
                              fixed = TRUE)})

# It doesn't add any new information to what we already know, lines are 
# parallel and we could see that previously 


# ------------------ Part D Turkey test ---------------------------------------

# Select Yes toilet
df_bath <- df[df$MultipleBath == "Yes",]

# Create Models
linear_model_b <- lm(data = df_bath, log_prices ~ MetroRegion)
summary(linear_model_b)

m_anova_b <- aov(linear_model_b)
summary(m_anova_b)

# Turkey analysis
dfB_turkey <- TukeyHSD(m_anova_b)
par(mar = c(1, 8, 2, 1)+ 2)
plot(dfB_turkey, las = 1)

getAnywhere(plot.TukeyHSD)



# Testing with a LM changing levels

df_bath$MetroRegion <- 
  factor(df_bath$MetroRegion, levels = c("Southern", "Eastern",  "Northern", "Western"))
linear_model_b <- lm(data = df_bath, log_prices ~ MetroRegion)
summary(linear_model_b)

df_bath$MetroRegion <- 
  factor(df_bath$MetroRegion, levels = c( "Eastern", "Southern",  "Northern", "Western"))
linear_model_b <- lm(data = df_bath, log_prices ~ MetroRegion)
summary(linear_model_b)



