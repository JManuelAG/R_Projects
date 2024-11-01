library(tidyverse)
library(rstatix)
library(GGally)
library(reshape2)
library(ggrepel)
library(grid)
library(forcats)
library(moments)
library(zoo)
library(lubridate)
library(scales)

# ------------------------- Custom Functions -----------------------------------

# Creates category levels, evenly spread across 4 quartiles 
categorize_columns <- function(data, columns) {
  quartile_labels <- c("Low", "Low-Middle", "Up-Middle", "High")
  
  for (col in columns) {
    cat_name <- paste(col, "cat", sep = "_")
    data <- data %>%
      mutate(!!cat_name := cut(!!sym(col), 
                               quantile(!!sym(col), probs = 0:4/4, na.rm = TRUE), 
                               labels = quartile_labels, 
                               include.lowest = TRUE))
  }
  
  return(data)
}

# ------------------------- Load Data sets -------------------------------------

# Read each of the 3 datasets we will be working with
cheese <- read_csv("./wche.csv")
d_stores <-read_csv("./Dominick_Stores.csv")

unique(cheese$WEEK)

# ------------------------- Data Inspection -----------------------------------

# Keep only the desired features of each table
cheese <- cheese[cheese$MOVE != 0, c(-7,-9, -10, -11)]
d_stores <- na.omit(d_stores[,c("Store", "Price_Tier", "Zone")])

# before merging I want to look at distribution of Zone and Price_Tier
table(d_stores$Zone)

# Change the levels, this will be helpful later
d_stores$Price_Tier <- factor(d_stores$Price_Tier, 
                              levels = c("CubFighter", "Low", "Medium", "High"))

table(d_stores$Price_Tier)

# Unique UPC 
length(unique(cheese$UPC))

# Unique stores on each Table
unique(cheese$STORE)
unique(d_stores$Store)

# --------------------------- Merge Tables -------------------------------------
# We can merge, I will only keep the ones with Zone and Price_Tier
cheese <- 
  merge(cheese, d_stores, by.x = "STORE", by.y = "Store", all.x = FALSE)

# Check for NA
sum(is.na(cheese))

# Final amount of stores 
length(unique(cheese$STORE))

# Remove duplicates, just in case
cheese <- distinct(cheese)

# -------------------------- Feature Engineering -------------------------------

# For the Cheese Table, I want to create new features 
# First we create new features for the total Sales amount
# Following, Sales = Price * Move / Qty
cheese$Sales <- cheese$PRICE * cheese$MOVE/cheese$QTY

# Gross Profit = Sales * Profit/100 
cheese$Gross_Profit <- round(cheese$Sales * cheese$PROFIT/100,2)

# Total Units sold = Move * Qty
cheese$Units_Sold = cheese$MOVE/cheese$QTY

# *********  by STORE
# Now I want to do an aggregate per store for their Sales
cheese %>%
  group_by(STORE) %>%
  summarize(Total_SALES = sum(Sales), 
            Total_Gross_Profit = sum(Gross_Profit),
            Price_Tier = unique(Price_Tier)) -> cheese_agg_store

# I want to categorize the weekly totals on four equal quadrants
cheese_agg_store <- 
  categorize_columns(cheese_agg_store, c("Total_SALES", "Total_Gross_Profit"))


# *********  by Week
# Now I will do aggregates by product 
cheese %>%
  group_by(WEEK) %>%
  summarize(Total_Units_Sold = sum(Units_Sold),
            Total_Sales = round(sum(Sales)), 
            Total_Gross_Profit = round(sum(Gross_Profit))) -> cheese_agg_week


# Add labels to the dates
base_date <- as.Date("1989-09-20", format = "%Y-%m-%d")

# Calculate the dates for each week
cheese$Date <- base_date + (cheese$WEEK - 1) * 7
cheese_agg_week$Date <- base_date + (cheese_agg_week$WEEK - 1) * 7

# Change format
cheese$Date <- as.Date(cheese$Date, format = "%d/%m/%Y")
cheese_agg_week$Date <- as.Date(cheese_agg_week$Date, format = "%d/%m/%Y")


# ------------------------- Final DF to work -----------------------------------
cheese
cheese_agg_week
cheese_agg_store



# --------------------------- Visual - 1 ---------------------------------------
# Calculate the 4-week moving average
cheese_agg_week$MA <- 
  rollmean(cheese_agg_week$Total_Sales, k = 4, na.pad = TRUE, align = "right")

# Calculate above_MA 
cheese_agg_week$above_MA <- 
  ifelse(cheese_agg_week$Total_Sales >= cheese_agg_week$MA, 
         cheese_agg_week$Total_Sales, cheese_agg_week$MA)

# below_MA
cheese_agg_week$below_MA <- ifelse(cheese_agg_week$Total_Sales < cheese_agg_week$MA, 
                                   cheese_agg_week$Total_Sales, cheese_agg_week$MA)


# Create the main plot
ggplot(cheese_agg_week, aes(x = Date, y = MA)) +
  geom_line(aes(color = "Moving Average"), size = .9) +
  geom_ribbon(aes(ymin = below_MA, ymax = MA, fill = "Below MA"), alpha = 0.7) +
  geom_ribbon(aes(ymin = MA, ymax = above_MA, fill = "Above MA"), alpha = 0.7) +
  labs(
    title = "Weekly Total Cheese Sales from 1989 to 1997",
    subtitle = "Sales are across all stores, moving average is for a 4-week period",
    x = "Year",
    y = "Total Cheese Sales",
    color = NULL,
    fill = NULL
  ) +
  
  # Scale fixtures
  scale_y_continuous(labels = scales::dollar_format(scale = 1), 
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") + 
  scale_color_manual(values = "black") +
  scale_fill_manual(values = c("Below MA" = "tomato", "Above MA" = "steelblue")) +
  
  # Theme Details
  theme_minimal() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 15, face="bold"),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 13)
  ) 
  
  

# --------------------------- Visual - 2 ---------------------------------------

# Extract month the Date column
cheese_agg_week$Month <- month(cheese_agg_week$Date, label = TRUE)

# Calculate the monthly average of Total_Sales
monthly_avg <- aggregate(Total_Sales ~ Month, data = cheese_agg_week, FUN = mean)

# Create the jitter plot with average points
ggplot(cheese_agg_week, aes(x = Month, y = Total_Sales)) +
  
  # Jitter Plot
  geom_jitter(
    size = 3,
    alpha = 0.5,
    width = 0.2,
    color = "steelblue") +
  
  # Separate across months
  geom_vline(
    xintercept = seq(0.5, length(unique(cheese_agg_week$Month)), by = 1),
    color = "gray90",
    size = 1) +
  
  # Mean point
  stat_summary(
    fun = "mean",
    geom = "point",
    size = 5,
    alpha = 0.6,
    shape = 16,
    color = "tomato") +
  
  # Label
  stat_summary(
    aes(label = dollar(round(..y.., 0),  prefix = "$")),
    fun = mean,
    geom = "label",
    size = 4.5,
    alpha = 0.8,
    label.size = NA,
    vjust = -0.5,
    color = "tomato") +
  
  # Labs
  labs(
    title = "Weekly Total Cheese Sales by Month from 1989 to 1997",
    subtitle = "Showing the average value for each month, weekly sales over a million not included",
    x = NULL,
    y = "Total Sales") +
  
  # Scale format and breaks 
  scale_y_continuous(labels = scales::dollar_format(scale = 1), 
                     breaks = scales::pretty_breaks(n = 7)) +
  coord_cartesian(ylim = c(400000, 1000000)) +
  
  # Theme details 
  theme_minimal() +
  theme(
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        axis.title = element_text(size = 15, face="bold"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(face = "bold", size = 22),
        plot.subtitle = element_text(size = 13, color = "tomato")
        ) 


# --------------------------- Visual - 3 ---------------------------------------
# Create Gross Profit Percent for each week
cheese_agg_week$Gross_Percent <- 
  cheese_agg_week$Total_Gross_Profit / cheese_agg_week$Total_Sales

# A general 1 vector 
cheese_agg_week$Cost_Of_Goods_Percent <- 1

# Main plot without labels 
ggplot(cheese_agg_week, aes(x = Date)) +
  
  #Area Charts
  geom_area(aes(y = Cost_Of_Goods_Percent, fill = "Cost of Sales"), alpha = 0.5) +
  geom_area(aes(y = Gross_Percent, fill = "Gross Profit"), alpha = 0.5) +
  
  # Labels
  labs(
    title = "Weekly Gross Profit Percent on Cheese Sales (1989-1997)",
    subtitle = "The following plot partitions the weekly total cheese sales on percentages, between cost of sales and gross profit.\nThe labels identify the top and bottom gross profit percent for each year, with a final GP of 32.28% for the entire period.",
    x = "Year",
    y = "Percentage of Total Sales",
    fill = NULL  # Remove legend title
  ) + 
  
  # Scales
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 15, face="bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.ticks.x = element_line(color = "black", size = .8),
    axis.ticks.y = element_line(color = "black", size = .8),
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 13)
  ) -> p

# Create a year feature
cheese_agg_week$Year <- year(cheese_agg_week$Date)

# Find the rows with maximum Gross Profit for each year
max_gross_profit_data <- cheese_agg_week %>%
  group_by(Year) %>%
  filter(Gross_Percent == max(Gross_Percent)) %>%
  ungroup() %>%
  select(Year, Gross_Percent, Date) 

# Find the rows with minimum Gross Profit for each year
min_gross_profit_data <- cheese_agg_week %>%
  group_by(Year) %>%
  filter(Gross_Percent == min(Gross_Percent)) %>%
  ungroup() %>%
  select(Year, Gross_Percent, Date) 

# Plot with labels
p + geom_text_repel(data = bind_rows(max_gross_profit_data,min_gross_profit_data), 
                    aes(x = Date, y = Gross_Percent, 
                        label = scales::percent(round(Gross_Percent, 3))),
                    box.padding = .5, size = 4, 
                    color = "black", fontface = "bold")


# --------------------------- Visual - 4 ---------------------------------------
# New aggregate for the price tier and each week
cheese %>%
  group_by(Date, Price_Tier) %>%
  summarize(Total_Units_Sold = sum(Units_Sold),
            Total_Sales = round(sum(Sales)), 
            Total_Gross_Profit = round(sum(Gross_Profit))) -> cheese_agg_week_p

# Add the year and number of the week 
cheese_agg_week_p$Year <- year(cheese_agg_week_p$Date)
cheese_agg_week_p$Week <- week(cheese_agg_week_p$Date)

# Create a 100% stacked bar chart
ggplot(cheese_agg_week_p, aes(x = Week, y = Total_Gross_Profit, fill = Price_Tier)) +
  geom_bar(stat = "identity", position = "fill", alpha = 2) +
  
  # Labs
  labs(
    title = "Weekly Gross Profit Distribution on Cheese Sales (1989-1997)",
    subtitle = "The following plot demonstrates the distribution of the weekly gross profit across each price tier.\n\nThe final distribution for the entire period is as follow:\n[CubFighter: 12%, Low: 12%, Medium: 45%, High: 31%]",
    x = "Week Number",
    y = "Percentage of Total Gross Profit",
    fill = "Price Tier"
  ) +
  
  # Scales
  scale_x_continuous(
    breaks = seq(0, 52, by = 10),
    labels = seq(0, 52, by = 10)
  ) + 
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = scales::pretty_breaks(n = 8)) +
  
  # Theme
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgray", color = "white"),
    strip.text.x = element_text(face = "bold", color = "black"),
    panel.border = element_blank(),
    panel.grid = element_line(size=1),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 15, face="bold"),
    axis.text.x = element_text(size = 10,
      margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.text.y = element_text(size = 12,
      margin = margin(t = 0, r = 5, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.ticks.x = element_line(color = "black", size = .8),
    axis.ticks.y = element_line(color = "black", size = .8),
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 12)
  ) +
  
  # Separate by year
  facet_wrap(~Year, nrow = 1)


# --------------------------- Visual - 5 ---------------------------------------
# Reorder STORE based on Total Gross Profit
cheese_agg_store <- cheese_agg_store %>%
  arrange(Total_Gross_Profit) %>%
  mutate(STORE = factor(STORE, levels = unique(STORE)))

# Filter the top and bottom 5 stores
top_bottom_stores <- cheese_agg_store %>%
  arrange(Total_Gross_Profit) %>%
  filter(row_number() <= 5 | row_number() > n() - 5)

# Create the plot
ggplot(cheese_agg_store, aes(x = STORE, y = Total_Gross_Profit, label = STORE)) +
  
  # Points
  geom_point(aes(color = Price_Tier), stat = 'identity', size = 8) +
  
  # Lines for the median value
  geom_segment(
    aes(
      y = median(Total_Gross_Profit),
      x = STORE,
      yend = Total_Gross_Profit+1000,
      xend = STORE
    ), alpha = 0.5
  ) +
  
  # Labels
  geom_text(data = top_bottom_stores, 
            color = "black", size = 2.8, fontface = "bold") +
  
  # Labs
  labs(
    title = "Total Gross Profit on Cheese Sales by Store (1989-1997)",
    subtitle = "Categorise by the Store Price Tier, the central point is the Median value with labels on top and bottom 5 performers.\n\nMedian Gross Profit: $899,368 (Store 83)\nTop Performer: $1'797,111 (Store 137)\nBottom Performer: $302,609 (Store 50)",
    x = "Stores",
    y = "Total Gross Profit",
    fill = NULL
  ) +
  
  # Scales
  scale_y_continuous(labels = scales::dollar_format(scale = 1), 
                     breaks = scales::pretty_breaks(n = 7)) +
  
  # Theme
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        panel.border = element_blank(),
        #panel.grid = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 11)) 


# --------------------------- Visual - 6 ---------------------------------------

# Similar to the last but comparing with Sales to see if makes an influence
ggplot(cheese_agg_store, 
       aes(x = Total_Gross_Profit, y = Total_SALES, col = Price_Tier)) + 
  geom_point(size = 5, alpha=0.8) +
  
  # Function to create a median line
  geom_vline(aes(xintercept= median(Total_Gross_Profit), linetype = "Median"), 
             col = "black", linetype = "dashed", size = .9) +
  
  # Function to create a median line
  geom_hline(aes(yintercept= median(Total_SALES), linetype = "Median"), 
             col = "black", linetype = "dashed", size = .9) +
  
  # Text for top and bottom 5
  geom_text_repel(aes(label = ifelse(STORE %in% top_bottom_stores$STORE, 
                                     as.character(top_bottom_stores$STORE), "")),
                  show.legend = FALSE,
                  segment.color = NA,
                  color = "black") +
  # Labs
  labs(
    title = "Total Sales vs Gross Profit for Cheese Products by Store (1989-1997)",
    subtitle = "Categorise by the Store Price Tier, the dividing line is for the Median values and label on top and bottom 5 gross profit performers.\n\nMedian Sales: $2'821,755\nMedian Profit: $899,368",
    x = "Total Gross Profit",
    y = "Total Sales",
    fill = NULL
  ) +
  
  # Scales
  scale_y_continuous(labels = scales::dollar_format(scale = 1),
                     breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(labels = scales::dollar_format(scale = 1),
                     breaks = scales::pretty_breaks(n = 8)) +
  
  # Theme
  theme_minimal() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 15, face="bold"),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 12)
  ) 
  










  
  

