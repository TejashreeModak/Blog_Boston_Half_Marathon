# Downlaoded Boston Run to Remember 2025 results from 
# https://results.raceroster.com/v2/en-US/results/36vs6pxhw573fv4c/results?subEvent=227956
# Filtered for Females 

library(ggplot2)
library(tidyr)
library(plotly)

### Helper function to convert time columns to total minutes ####
convert_to_minutes <- function(x) {
  parts <- strsplit(x, ":")[[1]]
  
  if (length(parts) == 3) {
    # Format: hours:minutes:seconds
    h <- as.numeric(parts[1])
    m <- as.numeric(parts[2])
    s <- as.numeric(parts[3])
    total_minutes <- h * 60 + m + s / 60
    total_minutes <- round(total_minutes, 3)
  } else if (length(parts) == 2) {
    # Format: minutes:seconds (with possible decimal seconds)
    m <- as.numeric(parts[1])
    s <- as.numeric(parts[2])
    total_minutes <- m + s / 60
    total_minutes <- round(total_minutes, 3)
  } else {
    # Unexpected format, return NA or handle as needed
    total_minutes <- NA
  }
  
  return(total_minutes)
}


### Read in data ####
df <- read.delim("Boston_Half_marathon_2025.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

### Convert time and pace to total minutes ####

# Apply the function "convert_to_minutes" to the column pace and split times
df$Pace_num <- sapply(df$Pace, convert_to_minutes)
df$X5k_num <- sapply(df$X5k, convert_to_minutes)
df$X10k_num <- sapply(df$X10k, convert_to_minutes)
df$X10m_num <- sapply(df$X10m, convert_to_minutes)
df$FinishTime_numeric <- sapply(df$NetTime, convert_to_minutes)

### Data cleaning ####
#Remove rows with empty
df <- df %>% na.omit() %>% filter(!is.na(Division) & Division != "")

### How many runners per age group? ####
df <- df %>% group_by(Division) %>% mutate(count = n()) %>% ungroup()
df_count <- df %>% select(Division, count) %>% distinct() 
ggplot(df_count, aes(x = reorder(Division, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Number of Runners per Age Group",
    x = "Age Group (Division)",
    y = "Number of Runners"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Using plotly
gg <- ggplot(df_count, aes(x = reorder(Division, -count), y = count)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(
    title = "Number of Runners per Age Group",
    x = "Age Group (Division)",
    y = "Number of Runners"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(gg)

## Spread of finish times per age group ####
ggplot(df, aes(x = Division, y = FinishTime_numeric)) +
  geom_boxplot() +
  labs(title = "Finish Time by Age Group", 
       x = "Age Group", 
       y = "Finish Time") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### finish times per age group with plotly interactive plots ####
## LOVE THIS! 
library(plotly)
ggplotly(
  ggplot(df, aes(x = Division, y = FinishTime_numeric)) +
    geom_boxplot()
)

## highlight top 5 runners per age group ####
top_runners <- df %>%
  group_by(Division) %>%
  arrange(FinishTime_numeric) %>%
  slice_head(n = 5)
ggplot(top_runners, aes(x = reorder(Name, FinishTime_numeric), y = FinishTime_numeric, fill = Division)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Division, scales = "free") +
  theme(axis.text.x = element_blank()) +
  labs(title = "Top 5 Finishers per Age Group", x = "", y = "Finish Time")

### normalize finish time within each age group ####
df_norm <- df %>%
  group_by(Division) %>%
  mutate(FinishTime_z = scale(FinishTime_numeric)) %>%
  ungroup()

ggplot(df_norm, aes(x = Division, y = FinishTime_z)) +
  geom_boxplot() +
  labs(title = "Normalized Finish Times by Age Group", y = "Z-Score of Finish Time")

### What is the frequency of runners per finish time slot per age group ####
#Histogram of number of runners per finish time per age division.
ggplot(df, aes(x = FinishTime_numeric)) + geom_freqpoly(binwidth = 2.5) + 
  labs(
    title = "Histogram of Finish Time",
    x = "Finish Time (Total min)",
    y = "Frequency"
  )
# Histogram of number of runners per finish time split by age group
ggplot(df, aes(x = FinishTime_numeric, colour = Division)) + geom_freqpoly(binwidth = 2) + facet_wrap(~Division)+ 
  labs(
    title = "Histogram of Finish Time",
    x = "Finish Time (Total min)",
    y = "Frequency"
  )

### Modeling ####
###Is age and finish time correlated? ####
#convert age division to age numeric
#Remove rows with FOPEN or empty
df_age <- df %>% filter(!Division == "FOPEN") 
# Extract the number from the Division column
df_age$AgeGroup <- str_extract(df_age$Division, "\\d+")  # Extracts the first set of digits
df_age$AgeGroup <- as.numeric(df_age$AgeGroup)
df_age <- df_age %>%
  mutate(AgeGroup = ifelse(AgeGroup == 0, 18, AgeGroup))

# Calculate the correlation between Age and Finish Time
cor(df_age$AgeGroup, df_age$FinishTime_numeric) #0.1327042
ggplot(df_age, aes(x = AgeGroup, y = FinishTime_numeric)) + geom_point() +
  labs(title = "Scatter Plot of Age Group vs Finish Time",
       x = "Age Group", y = "Finish Time (in minutes)")

### Fit a quadratic model ####
lm(FinishTime_numeric ~ poly(AgeGroup, 2), data = df_age)
# Plot the results
ggplot(df_age, aes(x = AgeGroup, y = FinishTime_numeric)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  labs(title = "Quadratic Fit of Finish Time vs Age Group",
       x = "Age Group", y = "Finish Time (in minutes)") +
  theme_minimal()
#The positive quadratic term (81.51) suggests a U-shaped curve: finish times tend to be 
#lower (faster) for middle age groups and higher (slower) for both younger and older runners.

### Fit a multiple regression model ####
model_multivariate <- lm(FinishTime_numeric ~ AgeGroup + Pace_num + X10k_num, data = df_age)
# Summary of the model
summary(model_multivariate)
# Call:
#   lm(formula = FinishTime_numeric ~ AgeGroup + Pace_num + X10k_num, 
#      data = df_age)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.122729 -0.054211 -0.000184  0.055538  0.118219 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) -0.0918040  0.0116744   -7.864 6.35e-15 ***
#   AgeGroup    -0.0001358  0.0001270   -1.069    0.285    
# Pace_num    13.1090694  0.0038025 3447.488  < 2e-16 ***
#   X10k_num     0.0001492  0.0006686    0.223    0.823    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.06334 on 1814 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 5.273e+07 on 3 and 1814 DF,  p-value: < 2.2e-16
#Key stats
# Residual standard error: 0.29 — very low, indicating tight prediction fit.
# R-squared: 0.9998 — this model explains 99.98% of the variance in finish time. Almost a perfect fit.
# F-statistic is massive and p-value is effectively 0 → The model overall is highly significant.
# Interpretation:
# Pace is the dominant predictor: unsurprisingly, your average pace is the biggest driver of your finish time.
# AgeGroup and 10k split have negligible effects in this multivariate context —
# because Pace captures most of their influence.
# The model may be overfit or redundant if pace already encapsulates most of what split and age provide.

### Correlation between 5k/10k split and Finish Time ####
cor(df_age$X5k_num, df_age$FinishTime_numeric, use = "complete.obs") #0.9087487
cor(df_age$X10k_num, df_age$FinishTime_numeric, use = "complete.obs") #-0.739319
cor(df_age$X10m_num, df_age$FinishTime_numeric, use = "complete.obs") #0.9908262

### Check outliers ####
# Calculate IQR for finish time
Q1 <- quantile(df_age$FinishTime_numeric, 0.25)
Q3 <- quantile(df_age$FinishTime_numeric, 0.75)
IQR <- Q3 - Q1
# Identify outliers (outside of 1.5*IQR range)
outliers <- df_age %>%
  filter(FinishTime_numeric < (Q1 - 1.5 * IQR) | FinishTime_numeric > (Q3 + 1.5 * IQR))
# View outliers
outliers

### Kmeans clustering ####
# Prepare data for clustering
df_cluster <- df_age %>% ungroup() %>% select(AgeGroup, FinishTime_numeric)
# Apply k-means clustering
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(df_cluster, centers = 3)
# Add cluster assignment to the data
df_age$cluster <- as.factor(kmeans_result$cluster)
# Visualize clusters
#This will group participants into clusters based on their age and finish time, 
#which could reveal interesting patterns.
ggplot(df_age, aes(x = AgeGroup, y = FinishTime_numeric, color = cluster)) +
  geom_point() +
  labs(title = "Clustering of Age Group and Finish Time",
       x = "Age Group", y = "Finish Time (in minutes)") +
  theme_minimal()


## Split-wise ####
df_split <- df %>%
  mutate(
    pace_0_5k = X5k_num / 5,
    pace_5k_10k = (X10k_num - X5k_num) / 5,
    pace_10k_10m = (X10m_num - X10k_num) / 6.0934,  # 10 miles ≈ 16.09 km ie 16km -10km = 6km
    pace_10m_finish = (FinishTime_numeric - X10m_num) / 5 # 13.1 miles ≈ 21.1 km ie 16.1km - 21.1km = 5km
  )

df_long <- df_split %>%
  select(Name, starts_with("pace_")) %>% select(-Pace_num) %>% 
  pivot_longer(cols = starts_with("pace_"), names_to = "split", values_to = "pace")
df_long$split <- factor(
  df_long$split,
  levels = c("pace_0_5k", "pace_5k_10k", "pace_10k_10m", "pace_10m_finish"),
  ordered = TRUE
)

ggplot(df_long, aes(x = split, y = pace, group = Name)) +
  geom_line(alpha = 0.2) +
  stat_summary(aes(group = 2), fun = mean, geom = "line", color = "blue", size = 1.2) +
  labs(title = "Split-wise Pace per Runner", y = "Pace (min/km)", x = "Split") +
  theme_minimal()



### how evenly a runner paces across splits — and correlate with performance ###
#Test the idea that more consistent pacing leads to better outcomes
df_split2 <- df_split %>%
  rowwise() %>%
  mutate(pace_sd = sd(c(pace_0_5k, pace_5k_10k, pace_10k_10m, pace_10m_finish)))

ggplot(df_split2, aes(x = pace_sd, y = FinishTime_numeric)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  labs(title = "Pacing Variability vs Finish Time", x = "Pace Standard Deviation", y = "Finish Time (min)")

### Identify those who ran the second half faster ####
df_split3 <- df_split %>%
  mutate(first_half = X10k_num, second_half = FinishTime_numeric - X10k_num,
         negative_split = second_half < first_half)

ggplot(df_split3, aes(x = negative_split, y = FinishTime_numeric)) +
  geom_boxplot() +
  labs(title = "Finish Time by Negative Split", x = "Negative Split?", y = "Finish Time")


### Use split times to reconstruct a "progression" of each runner:####
df_progression <- df %>%
  select(Name, X5k_num, X10k_num, X10m_num, FinishTime_numeric) %>%
  pivot_longer(-Name, names_to = "split", values_to = "cumulative_time") %>%
  mutate(split = factor(split, levels = c("X5k_num", "X10k_num", "X10m_num", "FinishTime_numeric")))

ggplot(df_progression, aes(x = split, y = cumulative_time, group = Name)) +
  geom_line(alpha = 0.1) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "blue", size = 1.2) +
  labs(title = "Runner Progression Over Splits", x = "Split", y = "Cumulative Time (min)")




