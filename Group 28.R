
# Chan Min Huey, TP083261
# Kang Hong Qian, TP081910
# Gong Yee Cheng, TP081205
# Yap Li Shan, TP080968


# 2.0 Data Preparation

# ---------------------------------
# 1. import packages
# ---------------------------------
library(ggplot2)
library(lubridate)
library(tidyverse)
library(tidyr)
library(dplyr)
library(maps)

# ---------------------------------
# 2.1 Data import
# ---------------------------------
#read csv file
flights <- read_csv("flights_100k.csv")
airlines <- read_csv("iata_airline_codes.csv")
airports <- read_csv("iata_airport_codes.csv")


# Preliminary view of data
glimpse(flights)
summary(flights)


# save function
save_plot <- function(filename, gplot) {
  ggsave(filename, gplot, width=8, height=6, bg="white")
}

# ---------------------------------
# 2.2.1 Convert hhmm variables to date-time variable
# ---------------------------------
convert_hhmm <- function(x, ymd_date){
  # # Convert numeric hhmm (e.g. 930, 113) into proper datetime
  x <- ifelse(is.na(x), NA, sprintf("%04d", as.integer(x)))
  hour <- as.numeric(substr(x, 1, 2))
  minute <- as.numeric(substr(x, 3, 4))
  
  # combine y-m-d to hour and minute
  ymd_date + hours(hour) + minutes(minute)
}


# Create date-time columns
flights <- flights |> 
  mutate(
    FLIGHT_DATE = make_date(YEAR, MONTH, DAY)
  )

flights <- flights |> 
  mutate(
    SCHEDULED_DEPARTURE = convert_hhmm(SCHEDULED_DEPARTURE, FLIGHT_DATE),
    DEPARTURE_TIME = convert_hhmm(DEPARTURE_TIME, FLIGHT_DATE),
    WHEELS_OFF = convert_hhmm(WHEELS_OFF, FLIGHT_DATE),
    WHEELS_ON = convert_hhmm(WHEELS_ON, FLIGHT_DATE),
    SCHEDULED_ARRIVAL = convert_hhmm(SCHEDULED_ARRIVAL, FLIGHT_DATE),
    ARRIVAL_TIME = convert_hhmm(ARRIVAL_TIME, FLIGHT_DATE)
  ) 

# ---------------------------------
# 2.2.2 Handle abnormal flights (overnight / same time)
# ---------------------------------
# Flights with ARRIVAL < DEPARTURE
midnight_flights <- flights |> 
  filter(ARRIVAL_TIME < DEPARTURE_TIME) |> 
  select(FLIGHT_NUMBER, ORIGIN_AIRPORT, DESTINATION_AIRPORT,
         DEPARTURE_TIME, ARRIVAL_TIME, SCHEDULED_DEPARTURE, SCHEDULED_ARRIVAL)

print(paste("Number of midnight flights (arrival < departure): ", nrow(midnight_flights)))


# Flights with ARRIVAL == DEPARTURE
same_time_flights <- flights |> 
  filter(!is.na(DEPARTURE_TIME), !is.na(ARRIVAL_TIME), ARRIVAL_TIME == DEPARTURE_TIME)

print(paste("Number of flights with identical ARRIVAL & DEPARTURE: ", nrow(same_time_flights)))


# Fix overnight flights by adding +1 day to arrival-related times
flights <- flights |> 
  mutate(
    ARRIVAL_TIME = if_else(!is.na(DEPARTURE_TIME) & !is.na(ARRIVAL_TIME) & ARRIVAL_TIME < DEPARTURE_TIME,
                           ARRIVAL_TIME + days(1), ARRIVAL_TIME),
    SCHEDULED_ARRIVAL = if_else(!is.na(SCHEDULED_DEPARTURE) & !is.na(SCHEDULED_ARRIVAL) & 
                                  SCHEDULED_ARRIVAL < SCHEDULED_DEPARTURE,
                                SCHEDULED_ARRIVAL + days(1), SCHEDULED_ARRIVAL),
    WHEELS_ON = if_else(!is.na(WHEELS_OFF) & !is.na(WHEELS_ON) & WHEELS_ON < WHEELS_OFF,
                        WHEELS_ON + days(1), WHEELS_ON)
  )


# Remove flights where ARRIVAL == DEPARTURE (not logical)
flights <- flights |> 
  filter(!(ARRIVAL_TIME == DEPARTURE_TIME & !is.na(ARRIVAL_TIME) & !is.na(DEPARTURE_TIME)))


# ---------------------------------
# 2.2.3 remove unneeded variables
# ---------------------------------
remove_cols <- c("...1", "CANCELLATION_REASON", "DIVERTED",
                 "WHEELS_ON", "WHEELS_OFF",
                 "ELAPSED_TIME",
                 "YEAR", "DAY")

flights <- flights |> 
  select(-all_of(remove_cols))


# ---------------------------------
# 2.2.4 Missing Value analysis
# ---------------------------------
missing_summary <- flights |> 
  summarise(across(everything(),
                   list(missing_count = ~sum(is.na(.)),
                        missing_pct   = ~mean(is.na(.)) * 100),
                   .names = "{.col}.{.fn}")) |> 
  pivot_longer(cols = everything(),
               names_to = c("variable", ".value"),
               names_sep = "\\.") |> 
  arrange(desc(missing_pct))

print(missing_summary)


# ---------------------------------
# 2.2.5 Cancelled flights logic
# ---------------------------------
# View the number of canceled vs. non-cancelled flights
print(table(flights$CANCELLED))


# Replace NA in delay columns (means "no delay") with 0
delay_cols <- c("AIR_SYSTEM_DELAY", "SECURITY_DELAY", "AIRLINE_DELAY", 
                "LATE_AIRCRAFT_DELAY", "WEATHER_DELAY")
flights <- flights |>
  mutate(across(all_of(delay_cols), ~replace_na(., 0)))


# Key columns related to flight timing
time_delay_cols <- c("ARRIVAL_DELAY", "ARRIVAL_TIME",
                     "DEPARTURE_TIME", "DEPARTURE_DELAY", "AIR_TIME")


# Check for cancelled flights that still have departure times
print(table(is.na(flights$DEPARTURE_TIME), flights$CANCELLED))


# Check for missing key time fields for non-canceled flights
for (col in time_delay_cols) {
  print(paste("Missing in", col))
  print(table(is.na(flights[[col]]), flights$CANCELLED))
}


# Ensure consistency:
# - CANCELLED = 1 → no departure time should exist
# - CANCELLED = 0 → all key time columns must be present
flights <- flights |> 
  filter((
    CANCELLED == 1 & is.na(DEPARTURE_TIME)) | 
      (CANCELLED == 0 & if_all(all_of(time_delay_cols), ~ !is.na(.)))
  )


# For CANCELLED flights: delay & air_time fields should be NA
flights <- flights |> 
  mutate(
    ARRIVAL_DELAY   = if_else(CANCELLED == 1, NA, ARRIVAL_DELAY),
    DEPARTURE_DELAY = if_else(CANCELLED == 1, NA, DEPARTURE_DELAY),
    AIR_TIME        = if_else(CANCELLED == 1, NA, AIR_TIME)
  )


# ---------------------------------
# 2.2.6 Check for duplicate records
# ---------------------------------
duplicates <- flights |> 
  count(FLIGHT_DATE, AIRLINE, FLIGHT_NUMBER, ORIGIN_AIRPORT, DESTINATION_AIRPORT, SCHEDULED_DEPARTURE) |> 
  filter(n > 1)

if(nrow(duplicates) > 0) {
  print(paste("Found", nrow(duplicates), "groups of duplicate records"))
  flights <- flights |> 
    distinct(FLIGHT_DATE, AIRLINE, FLIGHT_NUMBER, ORIGIN_AIRPORT, DESTINATION_AIRPORT, SCHEDULED_DEPARTURE, .keep_all = TRUE)
  print("Duplicate records removed")
} else {
  print("No duplicate records found")
}


# ---------------------------------
# 2.2.7 Check for Outlier records
# ---------------------------------
# use ggplot to visual check
plot_data <- flights |> 
  select(DEPARTURE_DELAY, ARRIVAL_DELAY) |> 
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

ggplot(plot_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "skyblue") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Boxplots of Delays",
       x = "Variable", y = "Value") +
  theme_minimal() ->p1

save_plot("image/Boxplots of Delays.png", p1)

# Remove extreme Outliers (> 800 min delay)
flights <- flights |> 
  filter((is.na(DEPARTURE_DELAY) | DEPARTURE_DELAY <= 800) &
           (is.na(ARRIVAL_DELAY) | ARRIVAL_DELAY <= 800))


# ---------------------------------
# 2.2.8 Identify abnormal airport codes
# ---------------------------------
#Find out if ORIGIN_AIRPORT or DESTINATION_AIRPORT is a pure number
invalid_airports <- flights |> 
  filter(str_detect(ORIGIN_AIRPORT, "^[0-9]+$") | 
           str_detect(DESTINATION_AIRPORT, "^[0-9]+$"))

print(paste("Number of flights with numeric airport codes: ", nrow(invalid_airports)))

print(unique(c(invalid_airports$ORIGIN_AIRPORT, invalid_airports$DESTINATION_AIRPORT)))

# Delete these abnormal airport codes
flights <- flights |> 
  filter(!(str_detect(ORIGIN_AIRPORT, "^[0-9]+$") | 
             str_detect(DESTINATION_AIRPORT, "^[0-9]+$")))


# ---------------------------------
# 2.2.9 Factorize airports and group rare ones
# ---------------------------------
# count total flights per airport (origin + destination)
airport_counts <- flights |> 
  select(ORIGIN_AIRPORT, DESTINATION_AIRPORT) |> 
  pivot_longer(cols = everything(), values_to = "airport") |> 
  count(airport, name = "flights_count")

print(paste("Total flights per airport: ", nrow(airport_counts)))

# airports with more than 2000 flights
major_airports <- airport_counts |> 
  filter(flights_count > 2000) |> 
  pull(airport)

# print out major airports list
major_airports_list <- airport_counts |> 
  filter(flights_count > 2000) |> 
  arrange(desc(flights_count))

print(major_airports_list)


# Recode airports code: keep major, others → "Others"
flights <- flights |> 
  mutate(
    ORIGIN_AIRPORT = if_else(ORIGIN_AIRPORT %in% major_airports, ORIGIN_AIRPORT, "Others"),
    DESTINATION_AIRPORT = if_else(DESTINATION_AIRPORT %in% major_airports, DESTINATION_AIRPORT, "Others")
  )

# convert airports code to factor
flights <- flights |> 
  mutate(
    ORIGIN_AIRPORT = factor(ORIGIN_AIRPORT),
    DESTINATION_AIRPORT = factor(DESTINATION_AIRPORT)
  )

# ---------------------------------
# 3.0 data verification
# ---------------------------------

head(flights)
summary(flights)
str(flights)

# Basic data overview
print(paste("Total records after cleaning:", nrow(flights)))
print(paste("Number of canceled flights:", sum(flights$CANCELLED == 1, na.rm = TRUE)))
print(paste("Number of normal flights:", sum(flights$CANCELLED == 0, na.rm = TRUE)))

# Logical check: departure must happen before arrival
time_issues <- flights |> 
  filter(!is.na(DEPARTURE_TIME) & !is.na(ARRIVAL_TIME) & 
           DEPARTURE_TIME >= ARRIVAL_TIME)

print(paste("Number of flights with time logic errors: ", nrow(time_issues)))


# Final missing value check
missing_summary <- flights |> 
  summarise(across(everything(),
                   list(missing_count = ~sum(is.na(.)),
                        missing_pct   = ~mean(is.na(.)) * 100),
                   .names = "{.col}.{.fn}")) |> 
  pivot_longer(cols = everything(),
               names_to = c("variable", ".value"),
               names_sep = "\\.") |> 
  arrange(desc(missing_pct))

print(missing_summary)


# ---------------------------------
# 4.0 save clean dataset to csv
# ---------------------------------
write_csv(flights, "flights_clean.csv")







# ******************************************************************************
# 3.0 Analysis
# ******************************************************************************
# Objective 1: To investigate the relationship between departure delay and 
# arrival delay, including the impact of additional flight factors 
# (e.g., distance, air time and flight date), and explore their overall 
# correlation and trend. – Chan Min Huey (TP083261)

#Load nessasary library
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

#read cleaned data file
flights <- read_csv("flights_clean.csv")

# save function
save_plot <- function(filename, gplot) {
  ggsave(filename, gplot, width=8, height=6, bg="white")
}

# select variable and remove NA values
df <- flights %>% 
  select(DEPARTURE_DELAY, ARRIVAL_DELAY, DISTANCE, AIR_TIME, FLIGHT_DATE) %>% 
  na.omit()




# Analysis 1-1: To analyse the distribution of departure delay and arrival delay 
# in order to understand their basic characteristics. (Descriptive)
# -----------------------------
# Reshape data (long format)
# -----------------------------
df_long <- df %>%
  pivot_longer(cols = c(DEPARTURE_DELAY, ARRIVAL_DELAY),
               names_to = "Type", values_to = "Delay")

# -----------------------------
# 1.Summary Statistics
# -----------------------------
desc_table <- df_long %>%
  group_by(Type) %>%
  summarise(
    Mean   = mean(Delay, na.rm = TRUE),
    Median = median(Delay, na.rm = TRUE),
    SD     = sd(Delay, na.rm = TRUE),
    Min    = min(Delay, na.rm = TRUE),
    Max    = max(Delay, na.rm = TRUE)
  )

print(desc_table)
# -----------------------------
# 2.Faceted frequency polygon（Distribution）
# -----------------------------
ggplot(df_long, aes(x = Delay, color = Type)) +  
  geom_freqpoly(binwidth = 10, linewidth = 1) +  
  facet_wrap(~Type, scales = "free_y") +
  coord_cartesian(xlim = c(-50, 300)) +
  labs(title="Distribution of Departure and Arrival Delays (Frequency Polygon)",
       x="Delay (minutes)", y="Count") +        
  scale_color_manual(values=c("pink", "purple")) + 
  theme_minimal() -> P1

save_plot("image/P1_Frequency_polygon.png",P1)

# -----------------------------
# 3. Combined Boxplot with Jitter point（Outlier detection）
# -----------------------------
#Set colours
delay_colors <- c(
  "<0" = "lightgrey",
  "0–99" = "orange",
  "100–199" = "yellow",
  "200–299" = "green",
  "300+" = "blue"
)

#Delay group
df_long <- df_long %>%
  mutate(DelayGroup = case_when(
    Delay < 0 ~ "<0",
    Delay >= 0 & Delay < 100 ~ "0–99",
    Delay >= 100 & Delay < 200 ~ "100–199",
    Delay >= 200 & Delay < 300 ~ "200–299",
    Delay >= 300 ~ "300+"
  ))


ggplot(df_long, aes(x = Type, y = Delay, fill = Type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = DelayGroup), width = 0.2, alpha = 0.4) +
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="red") +
  labs(title="Boxplot of Departure and Arrival Delays with Jittered Points",
       x="Type", y="Delay (minutes)", color="Delay Range") +
  scale_fill_manual(values=c("pink", "purple")) +
  scale_color_manual(values=delay_colors) + 
  theme_minimal() -> P2

save_plot("image/P2_Boxplot_Of_Delays.png",P2)

# -----------------------------
# 4. Additional : Combined Boxplot with Jitter point（whether distance affect distribution）
# -----------------------------
# set colours
dist_colours <- c(
  "short-haul" = "skyblue1",
  "medium-haul" = "skyblue2",
  "long-haul" = "skyblue3"
)

# distance group
df_long <- df_long %>%
  mutate(DistanceGroup = case_when(
    DISTANCE < 1500 ~ "short-haul",
    DISTANCE >= 1500 & DISTANCE <= 4000 ~ "medium-haul",
    DISTANCE > 4000 ~ "long-haul"
  ))

delay_colors <- c(
  "<0" = "red4",     
  "0–99" = "orange3",   
  "100–199" = "yellow3", 
  "200–299" = "green",
  "300+" = "green4"     
)

# Delay group
df_long <- df_long %>%
  mutate(DelayGroup = case_when(
    Delay < 0 ~ "<0",
    Delay >= 0 & Delay < 100 ~ "0–99",
    Delay >= 100 & Delay < 200 ~ "100–199",
    Delay >= 200 & Delay < 300 ~ "200–299",
    Delay >= 300 ~ "300+"
  ))

ggplot(df_long, aes(x = DistanceGroup, y = Delay, fill = DistanceGroup)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(aes(color = DelayGroup), width = 0.2, alpha = 0.4) +
  stat_summary(fun=mean, geom="point", shape=23, size=3, fill="#EEAEEE") +
  labs(title="Boxplot of delays by distance group with Jittered Points",
       x="Distance group", y="Delay (minutes)", color="Delay Range") +
  scale_fill_manual(values=dist_colours) +
  scale_color_manual(values=delay_colors) + 
  theme_minimal() -> P3

save_plot("image/P3_Distance_Arrival_Delay.png", P3)



# Analysis 1-2: To examine the relationship between departure delay and arrival delay (Diagnostic)
# -----------------------------
# Analysis 3.1.1 1-2: Correlation Analysis
# -----------------------------
library(reshape2)

# 1. Correlation coefficient + significance test
cor_test <- cor.test(df$DEPARTURE_DELAY, df$ARRIVAL_DELAY, method = "pearson")
print(cor_test)

# Extract correlation & p-value for reporting
correlation <- cor_test$estimate
p_value <- cor_test$p.value
print(paste("Pearson correlation =", round(correlation, 3),
            ", p-value =", signif(p_value, 3)))


# ------------------------------------
# 4. 2D Binned Heatmap with Regression Line
# ------------------------------------
ggplot(df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  
  geom_bin2d(bins = 100) + 
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method="lm", color="red", se=TRUE) + 
  
  annotate("text", x = max(df$DEPARTURE_DELAY) * 0.7,
           y = max(df$ARRIVAL_DELAY) * 0.9,
           label = paste("r =", round(correlation, 2),
                         "\np =", signif(p_value, 3)),
           color="black", size=4, hjust=0) +
  
  labs(title="2D Heatmap of Departure Delay vs. Arrival Delay",
       x="Departure Delay (minutes)", 
       y="Arrival Delay (minutes)",
       fill="Count") +
  theme_minimal() -> P4

save_plot("image/P4_Heatmap2D_Departure_vs_Arrival.png", P4)



# Analysis 1-3: To evaluate whether schedule depature with flight factors like 
# flight distance, and air time significantly predict arrival delay. (Predictive)

# -----------------------------
# Analysis 3.1.1 1-3: Linear Regression (Enhanced with Distance & Air Time)
# -----------------------------
library(broom)

# 1. Fit multiple regression model
model <- lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY + DISTANCE + AIR_TIME, data=df)
summary(model)

model_data <- augment(model)

# -----------------------------
# 2. Residuals vs Fitted Plot
# -----------------------------
residuals_df <- data.frame(
  Fitted = fitted(model),
  Residuals = resid(model)
)

ggplot(residuals_df, aes(x=Fitted, y=Residuals, color=Residuals)) +
  geom_point(alpha=0.6, size=2) +
  geom_hline(yintercept=0, color="black", linetype="dashed") +
  scale_color_gradient2(low="yellow", mid="lightgreen", high="red", midpoint=0) +
  labs(title="Residuals vs Fitted Plot (Multiple Regression)",
       x="Fitted Values (Predicted Arrival Delay)",
       y="Residuals (Actual - Predicted Arrival delay)", color="Residuals") +
  theme_minimal() -> P5

save_plot("image/P5_Residuals_vs_Fitted_Multiple.png", P5)

# -----------------------------
# 3. Observed vs Predicted Plot
# -----------------------------
ggplot(model_data, aes(x = .fitted, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.4, color="darkgreen") +
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed") +
  labs(title="Observed vs Predicted Arrival Delay (Multiple Regression)",
       x="Predicted Arrival Delay", y="Observed Arrival Delay") +
  theme_minimal() -> P6

save_plot("image/P6_Observed_vs_Predicted_Multiple.png", P6)



# Analysis 1-4: To provide recommendations on managing different departure delay 
# ranges and evaluate whether weekday/weekend patterns influence arrival delays. (Prescriptive)
# -----------------------------
# Analysis 3.1.1 (1-4) - Group Comparison (Enhanced with Weekday vs Weekend)
# -----------------------------

# 1. Add Weekday/Weekend variable 
df <- df %>%
  mutate(WeekType = ifelse(weekdays(FLIGHT_DATE) %in% c("Saturday","Sunday"),
                           "Weekend","Weekday"))

# 2. Create Delay Ranges (Departure Delay Groups)
df <- df %>%
  mutate(DepDelayGroup = case_when(
    DEPARTURE_DELAY < 0 ~ "<0 (Early)",
    DEPARTURE_DELAY >= 0 & DEPARTURE_DELAY < 30 ~ "0–29 (On Time / Slight Delay)",
    DEPARTURE_DELAY >= 30 & DEPARTURE_DELAY < 60 ~ "30–59 (Moderate Delay)",
    DEPARTURE_DELAY >= 60 & DEPARTURE_DELAY < 120 ~ "60–119 (Heavy Delay)",
    DEPARTURE_DELAY >= 120 ~ "120+ (Severe Delay)"
  ))

df$DepDelayGroup <- factor(df$DepDelayGroup,
                           levels = c("<0 (Early)",
                                      "0–29 (On Time / Slight Delay)",
                                      "30–59 (Moderate Delay)",
                                      "60–119 (Heavy Delay)",
                                      "120+ (Severe Delay)"))

t_test_result <- t.test(ARRIVAL_DELAY ~ WeekType, data = df)

print(t_test_result)

# -----------------------------
# 3. Violin + Boxplot (Weekday vs Weekend)
# -----------------------------
ggplot(df, aes(x = DepDelayGroup, y = ARRIVAL_DELAY, fill = WeekType)) +
  geom_violin(trim = FALSE, alpha = 0.5, position=position_dodge(width=0.9)) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7,
               position=position_dodge(width=0.9)) +
  labs(title="Arrival Delays across Departure Delay Groups (Weekday vs Weekend)",
       x="Departure Delay Group", y="Arrival Delay (minutes)", fill="Week Type") +
  scale_fill_manual(values=c("Weekday"="darkmagenta", "Weekend"="blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=30, hjust=1)) -> P7

save_plot("image/P7_Violin_Boxplot_Weektype.png", P7)

# -----------------------------
# 4. Bar Plot with Error Bars (Mean ± 95% CI, Weekday vs Weekend)
# -----------------------------
P8 <- df %>%
  group_by(DepDelayGroup, WeekType) %>%
  summarise(
    Mean_Arrival = mean(ARRIVAL_DELAY, na.rm = TRUE),
    SD_Arrival   = sd(ARRIVAL_DELAY, na.rm = TRUE),
    N = n(),
    .groups="drop"
  ) %>%
  mutate(
    SE = SD_Arrival / sqrt(N),
    Lower = Mean_Arrival - 1.96 * SE,
    Upper = Mean_Arrival + 1.96 * SE
  ) %>%
  ggplot(aes(x = DepDelayGroup, y = Mean_Arrival, fill=WeekType)) +
  geom_col(position=position_dodge(), alpha=0.7) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper),
                width=0.2, position=position_dodge(0.9), color="black") +
  labs(title="Mean Arrival Delay with 95% CI (Weekday vs Weekend)",
       x="Departure Delay Group", y="Mean Arrival Delay (minutes)",
       fill="Week Type") +
  scale_fill_manual(values=c("Weekday"="darkmagenta", "Weekend"="blue"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle=30, hjust=1))

save_plot("image/P8_Barplot_ErrorBars_Weektype.png", P8)








# Objective 2: To evaluate the performance of different airlines in managing flight
# delays – Kang Hong Qian (TP081205)

# ============================================================
# Flight Delay Analysis: Descriptive, Diagnostic, Predictive, and Trend
# ============================================================

# ---------------------------
# Load Required Libraries
# ---------------------------
library(ggplot2)
library(dplyr)
library(ggridges)
library(lubridate)
library(viridis)
library(zoo)
library(corrplot)
library(e1071)
library(forecast)
library(caret)

# ---------------------------
# Load Dataset
# ---------------------------
flights <- read.csv("flights_clean.csv")


# Analysis 2-1: Flight Delay Distribution by Airline (Descriptive)
# ============================================================
# 1. DESCRIPTIVE ANALYSIS — Violin Plot + Summary Statistics
# ============================================================

# Clean missing data
flights <- flights %>%
  filter(!is.na(AIRLINE), !is.na(ARRIVAL_DELAY))

# Summary statistics by airline
summary_stats <- flights %>%
  group_by(AIRLINE) %>%
  summarise(
    mean_delay  = mean(ARRIVAL_DELAY, na.rm = TRUE),
    median_delay = median(ARRIVAL_DELAY, na.rm = TRUE),
    sd_delay    = sd(ARRIVAL_DELAY, na.rm = TRUE),
    Q1          = quantile(ARRIVAL_DELAY, 0.25, na.rm = TRUE),
    Q3          = quantile(ARRIVAL_DELAY, 0.75, na.rm = TRUE),
    skewness    = skewness(ARRIVAL_DELAY, na.rm = TRUE),
    kurtosis    = kurtosis(ARRIVAL_DELAY, na.rm = TRUE)
  )

print(summary_stats)



# Ridgeline Plot
ggplot(flights, aes(x = ARRIVAL_DELAY, y = AIRLINE, fill = AIRLINE)) +
  geom_density_ridges(alpha = 0.7, scale = 1) +
  scale_x_continuous(limits = c(-50, 300)) +
  labs(
    title = "Ridgeline Plot of Arrival Delays by Airline",
    x = "Arrival Delay (minutes)",
    y = "Airline"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Airline order (by median delay)
airline_order <- flights %>%
  group_by(AIRLINE) %>%
  summarise(median_delay = median(ARRIVAL_DELAY, na.rm = TRUE)) %>%
  arrange(desc(median_delay)) %>%
  pull(AIRLINE)

# Dynamic axis limits
y_min <- min(flights$ARRIVAL_DELAY, na.rm = TRUE)
y_max <- max(flights$ARRIVAL_DELAY, na.rm = TRUE)

# Violin Plot + Median Points
ggplot(flights, aes(x = factor(AIRLINE, levels = airline_order),
                    y = ARRIVAL_DELAY, fill = AIRLINE)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  stat_summary(fun = median, geom = "point", color = "black", size = 0.7) +
  scale_fill_viridis_d(option = "C", name = "Airline") +
  scale_y_continuous(limits = c(max(-50, y_min), min(200, y_max))) +
  labs(
    title = "Arrival Delay Distribution by Airline",
    subtitle = "Violin plot with median points and summary statistics",
    x = "Airline",
    y = "Arrival Delay (minutes)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



# Analysis 2-2: Impact of Departure Time and Airline on Delay (Diagnostic)
# ============================================================
# 2. DIAGNOSTIC ANALYSIS — Heatmap / Stacked Bar / Regression
# ============================================================

# Convert to datetime and extract hour
flights <- flights %>%
  mutate(
    SCHEDULED_DEPARTURE = ymd_hms(SCHEDULED_DEPARTURE),
    DEP_HOUR = hour(SCHEDULED_DEPARTURE)
  )

# Prepare diagnostic dataset
flights_diag <- flights %>%
  filter(!is.na(ARRIVAL_DELAY), !is.na(DEP_HOUR), !is.na(AIRLINE)) %>%
  mutate(AIRLINE = as.factor(AIRLINE))

# Multiple Linear Regression
lm_model <- lm(ARRIVAL_DELAY ~ DEP_HOUR + AIRLINE, data = flights_diag)
summary(lm_model)

# Time group and delay categories
flights <- flights %>%
  mutate(
    HOUR_GROUP = case_when(
      DEP_HOUR >= 0 & DEP_HOUR < 8  ~ "0-7",
      DEP_HOUR >= 8 & DEP_HOUR < 16 ~ "8-15",
      TRUE                          ~ "16-23"
    ),
    HOUR_GROUP = factor(HOUR_GROUP, levels = c("0-7", "8-15", "16-23")),
    delay_group = case_when(
      ARRIVAL_DELAY <= 0  ~ "On-time or Early",
      ARRIVAL_DELAY <= 15 ~ "0-15",
      ARRIVAL_DELAY <= 30 ~ "15-30",
      ARRIVAL_DELAY <= 45 ~ "30-45",
      ARRIVAL_DELAY <= 60 ~ "45-60",
      ARRIVAL_DELAY >  60 ~ ">60",
      TRUE ~ "Unknown"
    ),
    delay_group = factor(delay_group, levels = c("On-time or Early", "0-15", "15-30", "30-45", "45-60", ">60", "Unknown"))
  )

#Stacked Bar Proportion Plot
ggplot(flights, aes(x = HOUR_GROUP, fill = delay_group)) +
  geom_bar(color = "white", position = "fill") +
  facet_grid(. ~ AIRLINE) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(option = "E", name = "Delay Group") +
  labs(
    title = "Proportion of Delay Categories by Time of Day and Airline",
    x = "Time of Day",
    y = "Proportion of Flights"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    panel.grid.minor = element_blank()
  )

# Heatmap: Average Delay by Airline & Hour
heatmap_data <- flights %>%
  group_by(AIRLINE, DEP_HOUR) %>%
  summarise(avg_delay = mean(ARRIVAL_DELAY, na.rm = TRUE), .groups = "drop")

ggplot(heatmap_data, aes(x = factor(DEP_HOUR), y = AIRLINE, fill = avg_delay)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "G", name = "Average Delay") +
  labs(
    title = "Average Arrival Delay by Airline and Hour",
    x = "Scheduled Departure Hour",
    y = "Airline"
  ) +
  theme_minimal()


# Analysis 2-3: Assessing Chaining Delay Risk Using Logistic Regression (Predictive)
# ============================================================
# 3. PREDICTIVE ANALYSIS — Logistic Regression (Delay Risk)
# ============================================================

flights_modeling <- flights %>%
  mutate(
    SCHEDULED_DEPARTURE = ymd_hms(SCHEDULED_DEPARTURE),
    DEPARTURE_TIME      = ymd_hms(DEPARTURE_TIME),
    FLIGHT_DATE         = as.Date(SCHEDULED_DEPARTURE),
    DEP_HOUR            = hour(SCHEDULED_DEPARTURE),
    DEPARTURE_DELAY     = as.numeric(DEPARTURE_TIME - SCHEDULED_DEPARTURE, units = "mins"),
    Severe_Chaining_Risk = factor(
      ifelse(DEPARTURE_DELAY > 30, "High_Risk", "Normal_Risk"),
      levels = c("Normal_Risk", "High_Risk")
    )
  ) %>%
  filter(!is.na(DEPARTURE_DELAY), !is.na(TAIL_NUMBER), !is.na(AIRLINE))

# Add aircraft-based features
flights_segments <- flights_modeling %>%
  arrange(TAIL_NUMBER, SCHEDULED_DEPARTURE) %>%
  group_by(TAIL_NUMBER, FLIGHT_DATE) %>%
  mutate(
    AIRCRAFT_DAILY_CYCLE = n(),
    FLIGHT_SEQUENCE = row_number(),
    SEGMENTS_LEFT   = AIRCRAFT_DAILY_CYCLE - FLIGHT_SEQUENCE,
    AVG_PREV_DEPART_DELAY = lag(DEPARTURE_DELAY, n = 1L)
  ) %>%
  ungroup()

# Final dataset
flights_model_final <- flights_segments %>%
  filter(!is.na(AVG_PREV_DEPART_DELAY), !is.na(AIRCRAFT_DAILY_CYCLE)) %>%
  mutate(AIRLINE = factor(AIRLINE), DEP_HOUR = factor(DEP_HOUR))

# Train logistic regression model
set.seed(42)
index <- createDataPartition(flights_model_final$Severe_Chaining_Risk, p = 0.7, list = FALSE)
train_data <- flights_model_final[index, ]

logit_model <- glm(
  Severe_Chaining_Risk ~ AIRLINE + AIRCRAFT_DAILY_CYCLE + SEGMENTS_LEFT +
    AVG_PREV_DEPART_DELAY + DEP_HOUR,
  data = train_data,
  family = binomial(link = "logit")
)

# Extract Odds Ratios
airline_coefficients <- summary(logit_model)$coefficients %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Feature") %>%
  filter(grepl("^AIRLINE", Feature)) %>%
  mutate(
    AIRLINE = gsub("AIRLINE", "", Feature),
    Odds_Ratio = exp(Estimate)
  )

# Add baseline
baseline_airline_code <- levels(flights_model_final$AIRLINE)[1]
baseline_df <- data.frame(AIRLINE = baseline_airline_code, Odds_Ratio = 1.0)

airline_coefs <- bind_rows(
  airline_coefficients %>% select(AIRLINE, Odds_Ratio),
  baseline_df
)

# Print result
cat("\n======== Analysis 4: Airline Severe Delay Odds Ratio (Performance Ranking) ========\n")
print(airline_coefs %>% arrange(Odds_Ratio))
cat("====================================================================\n\n")

# Visualization: Odds Ratio Ranking
ggplot(airline_coefs, aes(x = reorder(AIRLINE, Odds_Ratio), y = Odds_Ratio, fill = Odds_Ratio)) +
  geom_col() +
  scale_fill_gradient2(low = "darkblue", mid = "lightblue", high = "red", midpoint = 1.0) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "black") +
  labs(
    title = "Airline Delay Management Performance (Severe Delay Odds Ratio Ranking)",
    subtitle = "Higher values indicate higher risk of severe delays",
    x = "Airline",
    y = "Severe Delay Odds Ratio"
  ) +
  coord_flip() +
  theme_minimal()




# Analysis 2-4: Identifying Monthly and Seasonal Delay Patterns (Prescriptive)
# ============================================================
# 4. TREND ANALYSIS — Monthly Delay Trend (LOESS + Rolling Mean)
# ============================================================

flights <- flights %>%
  mutate(
    FLIGHT_DATE = as.Date(SCHEDULED_DEPARTURE),
    MONTH = floor_date(FLIGHT_DATE, "month")
  )

monthly_trend <- flights %>%
  group_by(MONTH) %>%
  summarise(avg_arrival_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)) %>%
  mutate(
    loess_pred = predict(loess(avg_arrival_delay ~ as.numeric(MONTH), span = 0.5)),
    roll_mean = rollmean(avg_arrival_delay, k = 3, fill = NA, align = "right")
  )

# Identify peaks and troughs
max_point <- monthly_trend %>% filter(avg_arrival_delay == max(avg_arrival_delay))
min_point <- monthly_trend %>% filter(avg_arrival_delay == min(avg_arrival_delay))

# Plot trend
ggplot(monthly_trend, aes(x = MONTH)) +
  geom_line(aes(y = avg_arrival_delay), color = "skyblue", size = 1) +
  geom_line(aes(y = loess_pred), color = "red", linetype = "dashed", size = 1) +
  geom_point(aes(y = avg_arrival_delay), color = "darkblue", size = 1.6) +
  annotate("text", x = max_point$MONTH, y = max_point$avg_arrival_delay + 1,
           label = paste("Max:", round(max_point$avg_arrival_delay, 2)), color = "red") +
  annotate("text", x = min_point$MONTH, y = min_point$avg_arrival_delay - 1,
           label = paste("Min:", round(min_point$avg_arrival_delay, 2)), color = "red") +
  labs(
    title = "Trend of Average Arrival Delays Over Time",
    subtitle = "LOESS trend (red dashed) with monthly averages and peaks/troughs",
    x = "Month",
    y = "Average Arrival Delay (minutes)"
  ) +
  theme_minimal()















# Objective 3: To analyse how various factors , including time of day, day of 
# week, flight distance, and departure delay, influence flight arrival delays, 
# and to identify the relationships that contribute to overall delay performance.
# – Gong Yee Cheng (TP081910)

library(dplyr)
library(ggplot2)
library(lubridate)



# Analysis 3-1: Assessing Time-of-Day and Flight Distance Interaction Effects on 
# Flight Arrival Delays using Two-Way ANOVA (Diagnostic Analysis)
# =========================================================
# 3.3.1: Time of Day (Diagnostic Analysis)
# =========================================================

# Create time_bin variable
flights_clean <- flights %>%
  mutate(dep_hour = hour(SCHEDULED_DEPARTURE),
         time_bin = case_when(
           dep_hour >=5 & dep_hour < 9  ~ "Early_Morning",
           dep_hour >=9 & dep_hour < 12 ~ "Morning",
           dep_hour >=12 & dep_hour < 15 ~ "Noon_Afternoon",
           dep_hour >=15 & dep_hour < 18 ~ "Afternoon",
           dep_hour >=18 & dep_hour < 22 ~ "Evening",
           TRUE ~ "Night"),
         time_bin = factor(time_bin,
                           levels=c("Early_Morning","Morning","Noon_Afternoon",
                                    "Afternoon","Evening","Night")))

cat("Frequency by time_bin:\n")
print(table(flights_clean$time_bin))

# Create distance group variable
flights_clean <- flights_clean %>%
  mutate(distance_group = case_when(
    DISTANCE < 500 ~ "Short",
    DISTANCE >= 500 & DISTANCE < 1500 ~ "Medium",
    DISTANCE >= 1500 ~ "Long"
  )) %>%
  mutate(distance_group = factor(distance_group,
                                 levels=c("Short", "Medium", "Long")))

cat("\nFrequency by distance_group:\n")
print(table(flights_clean$distance_group))


# Two-way ANOVA (Time × Distance)
cat("\nTwo-way ANOVA result (Time of Day × Flight Distance):\n")
anova_interact <- aov(ARRIVAL_DELAY ~ time_bin * distance_group, data=flights_clean)
summary(anova_interact)


# Boxplot with both factors
p1 <- ggplot(flights_clean, aes(x=time_bin, y=ARRIVAL_DELAY, fill=distance_group)) +
  geom_boxplot(alpha=0.7, outlier.size=0.5) +
  labs(title="Arrival Delay by Time of Day and Flight Distance",
       x="Time of Day", y="Arrival Delay (minutes)", fill="Distance Group") +
  theme_minimal()
print(p1)


# Mean Arrival Delay Heatmap
heat_data <- flights_clean %>%
  group_by(time_bin, distance_group) %>%
  summarise(mean_delay = mean(ARRIVAL_DELAY, na.rm=TRUE), .groups="drop")

p2 <- ggplot(heat_data, aes(x=time_bin, y=distance_group, fill=mean_delay)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="lightyellow", high="red") +
  labs(title="Mean Arrival Delay by Time of Day and Flight Distance",
       x="Time of Day", y="Distance Group", fill="Mean Delay (min)") +
  theme_minimal()
print(p2)



# Analysis 3-2: Exploring Weekly Delay Trends through Descriptive Statistical Profiling (Descriptive Analysis)
# =========================================================
# 3.3.2: Day of Week (Descriptive Analysis)
# =========================================================

if(!'DAY_OF_WEEK' %in% names(flights_clean)) {
  if('FLIGHT_DATE' %in% names(flights_clean)) {
    flights_clean <- flights_clean %>% mutate(DAY_OF_WEEK = wday(FLIGHT_DATE, week_start=1))
  } else {
    stop("FLIGHT_DATE not found; cannot create DAY_OF_WEEK.")
  }
}

summary_dow <- flights_clean %>%
  group_by(DAY_OF_WEEK) %>%
  summarise(n = n(),
            mean_arr = mean(ARRIVAL_DELAY, na.rm=TRUE),
            median_arr = median(ARRIVAL_DELAY, na.rm=TRUE))
cat("\nSummary of arrival delays by DAY_OF_WEEK:\n")
print(summary_dow)

# Barplot: Mean arrival delay by day of week
p3 <- ggplot(summary_dow, aes(x=factor(DAY_OF_WEEK), y=mean_arr)) +
  geom_col(fill="steelblue") +
  labs(title="Mean Arrival Delay by Day of Week",
       x="Day of Week (1=Mon ... 7=Sun)",
       y="Mean Arrival Delay (minutes)") +
  theme_minimal()
print(p3)



# Analysis 3-3: Examining the Relationship between Flight Distance and Arrival 
# Delay using Spearman Rank Correlation (Correlation Analysis)
# =========================================================
# 3.3.3: Flight Distance (Correlation Analysis)
# =========================================================

cat("\nSpearman correlation between DISTANCE and ARRIVAL_DELAY:\n")
cor_spearman <- cor.test(flights_clean$DISTANCE, flights_clean$ARRIVAL_DELAY,
                         method="spearman", use="complete.obs")
print(cor_spearman)

# Scatter plot
p4 <- ggplot(flights_clean, aes(x=DISTANCE, y=ARRIVAL_DELAY)) +
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", color="red", se=FALSE) +
  labs(title="Arrival Delay vs Flight Distance",
       x="Flight Distance (miles)", y="Arrival Delay (minutes)") +
  theme_minimal()
print(p4)



# Analysis 3-4: Evaluating the Predictive Influence of Departure Delay on Arrival 
# Delay through Linear Regression Modelling (Predictive Analysis)
# =========================================================
# 3.3.4: Departure Delay (Predictive Analysis)
# =========================================================

cat("\nSimple Linear Regression: ARRIVAL_DELAY ~ DEPARTURE_DELAY\n")
lm_dep <- lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY, data=flights_clean)
print(summary(lm_dep))

# Scatter plot with regression line
p5 <- ggplot(flights_clean, aes(x=DEPARTURE_DELAY, y=ARRIVAL_DELAY)) +
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", color="darkgreen", se=FALSE) +
  labs(title="Arrival Delay vs Departure Delay",
       x="Departure Delay (minutes)", y="Arrival Delay (minutes)") +
  theme_minimal()
print(p5)








# Objective 4: To analyse the propagation mechanism of flight delays, with a focus 
# on the relationship between Late Aircraft Delay and Arrival Delay and find out 
# Objective 4: To analyse the propagation mechanism of flight delays, with a 
# focus on the relationship between Late Aircraft Delay and Arrival Delay and find 
# out the key drivers that amplify delay propagation as well as potential intervention 
# points for mitigation. – Yap Li Shan (TP080968)the key drivers that amplify delay 
# propagation as well as potential intervention points for mitigation. – Yap Li Shan (TP080968)


library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(interactions)
library(randomForest)
library(pROC)
library(reshape2)
# ---------------------------------
# 4.0 Load data
# ---------------------------------
# Load the cleaned dataset
flights <- read.csv("flights_clean.csv", stringsAsFactors = FALSE)

# Convert date columns with proper format specification
flights$FLIGHT_DATE <- as.Date(flights$FLIGHT_DATE)
flights$SCHEDULED_DEPARTURE <- as.POSIXct(flights$SCHEDULED_DEPARTURE, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
flights$DEPARTURE_TIME <- as.POSIXct(flights$DEPARTURE_TIME, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
flights$SCHEDULED_ARRIVAL <- as.POSIXct(flights$SCHEDULED_ARRIVAL, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
flights$ARRIVAL_TIME <- as.POSIXct(flights$ARRIVAL_TIME, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Defining core delay attribution variables
delay_cols <- c("AIR_SYSTEM_DELAY", "SECURITY_DELAY", "AIRLINE_DELAY", 
                "LATE_AIRCRAFT_DELAY", "WEATHER_DELAY")

# clean data
flights_clean <- flights |> 
  filter(CANCELLED == 0) |> 
  filter(ARRIVAL_DELAY > 0)

# save function
save_plot <- function(filename, gplot) {
  ggsave(filename, gplot, width=8, height=6, bg="white")
}




# Analysis 4-1: Identify the primary attributable factors to flight delays and 
# analysing the localized delay risk profile across the Top 15 busiest origin airports.(Descriptive Analysis)
# ---------------------------------
# 4.1 Descriptive Analysis
# ---------------------------------
# 4.1.1 Horizontal bar chart
# 4.1.1.1 Calculate the total number of minutes for each delay attribution
total_delay_minutes <- flights_clean |> 
  summarise(across(all_of(delay_cols), sum))

# 4.1.1.2 Convert data into long format for easy plotting
delay_long_format <- total_delay_minutes |> 
  pivot_longer(
    cols = all_of(delay_cols),
    names_to = "Delay_Type",
    values_to = "Total_Minutes"
  ) |> 
  # calculate percentage
  mutate(Percentage = Total_Minutes / sum(Total_Minutes) * 100) |> 
  arrange(desc(Total_Minutes)) |> 
  mutate(Delay_Type = factor(Delay_Type, levels = Delay_Type))

# 4.1.1.3 start plotting
p1 <- ggplot(delay_long_format, 
             aes(x = Delay_Type, y = Total_Minutes, fill = Delay_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(comma(Total_Minutes), "\n(", round(Percentage, 1), "%)")), 
            hjust = -0.1, 
            size = 4, 
            color = "black") +
  labs(
    title = "Total Delay Contribution by Attributable Factor",
    x = "Attributable Delay Type",
    y = "Total Delay Minutes (Mins)",
    fill = "Delay Type"
  ) +
  coord_flip() + 
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none" 
  )

print(p1)
save_plot("image/objective4/p1.png", p1)



# 4.1.2 Stacked Bar Chart 
# 4.1.2.1 Find the top 15 origin airports with the highest total delay minutes
top_15_airports <- flights_clean |> 
  group_by(ORIGIN_AIRPORT) |> 
  summarise(Total_Delay_Minutes = sum(ARRIVAL_DELAY)) |> 
  arrange(desc(Total_Delay_Minutes)) |> 
  head(15)

top_15_airports_list <- top_15_airports$ORIGIN_AIRPORT

# 4.1.2.2 Filter the data for the top 5 airports and calculate each delay type's minute
top_15_delay_attribution <- flights_clean |> 
  filter(ORIGIN_AIRPORT %in% top_15_airports_list) |> 
  group_by(ORIGIN_AIRPORT) |> 
  summarise(across(all_of(delay_cols), sum, .names = "{.col}")) |> 
  ungroup()

# 4.1.2.3 Convert data into long format for easy plotting
top_15_long_format <- top_15_delay_attribution |> 
  pivot_longer(
    cols = all_of(delay_cols),
    names_to = "Delay_Type",
    values_to = "Total_Minutes"
  ) |> 
  group_by(ORIGIN_AIRPORT) |> 
  mutate(
    Total_Airport_Minutes = sum(Total_Minutes),
    Percentage = Total_Minutes / Total_Airport_Minutes
  ) |> 
  ungroup() 

# 4.1.2.4 start plotting
p2 <- ggplot(top_15_long_format, aes(x = ORIGIN_AIRPORT, y = Percentage, fill = Delay_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Delay Contribution Structure for Top 15 Busiest Airports",
    x = "Origin Airport",
    y = "Percentage",
    fill = "Delay Type"
  ) +
  geom_text(
    aes(label = scales::percent(Percentage, accuracy = 0.01)), 
    position = position_stack(vjust = 0.5),                  
    color = "white",                                         
    size = 2.5
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p2)
save_plot("image/objective4/p2.png", p2)


# 4.1.3 Heatmap
p3 <- ggplot(top_15_long_format, aes(x = Delay_Type, y = ORIGIN_AIRPORT, fill = Percentage)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                       labels = scales::percent, 
                       name = "Delay Percentage") +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 0.1),
                color = ifelse(Percentage > 0.35, "white", "black")),
            size = 3.5) +
  scale_color_identity() + 
  labs(
    title = "Delay Attribution Heatmap for Top 15 Origin Airports",
    x = "Delay Type (Responsibility)",
    y = "Origin Airport"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(p3)
save_plot("image/objective4/p3.png", p3)



# Analysis 4-2: To diagnose how ground operational efficiency (measured by TAXI_OUT) 
# moderates the relationship between LATE_AIRCRAFT_DELAY and ARRIVAL_DELAY (Diagnostic Analysis)
# ---------------------------------
# 4.2 Diagnostic Analysis
# ---------------------------------
# 4.2.1 Find out which delay attribution contributes most to ARRIVAL_DELAY
model_1 <- lm(
  ARRIVAL_DELAY ~ AIR_SYSTEM_DELAY + SECURITY_DELAY + AIRLINE_DELAY + 
    LATE_AIRCRAFT_DELAY + WEATHER_DELAY, 
  data = flights_clean
)

# print out summary
summary(model_1)

# 4.2.2 Diagnose whether TAXI_OUT can moderate the impact of LATE_AIRCRAFT_DELAY on arrive delay
# 4.2.2.1 Centering of core continuous variables
# to eliminating multicollinearity
# makethe coefficients when there is no interaction more explanatory power
flights_clean_c <- flights_clean |> 
  mutate(
    LATE_AIRCRAFT_DELAY_c = LATE_AIRCRAFT_DELAY - mean(LATE_AIRCRAFT_DELAY),
    TAXI_OUT_c = TAXI_OUT - mean(TAXI_OUT)
  )

# 4.2.2.2 Build a moderated model
model_2 <- lm(
  ARRIVAL_DELAY ~ LATE_AIRCRAFT_DELAY_c * TAXI_OUT_c,
  data = flights_clean_c
)

# print out summary
summary(model_2)

# 4.2.2.3 Interaction Plot
p4 <- interact_plot(
  model = model_2,
  pred = LATE_AIRCRAFT_DELAY_c,  # X-axis predictor variable
  modx = TAXI_OUT_c,            # Moderating variable
  plot.points = FALSE,          
  modx.labels = c("Low TAXI_OUT (Efficient)", "Medium TAXI_OUT (Average)", "High TAXI_OUT (Inefficient)"),
  legend.main = "Airport Efficiency (TAXI_OUT)",
  x.label = "LATE_AIRCRAFT_DELAY (Mins)",
  y.label = "Predicted ARRIVAL_DELAY (Mins)",
  main = "Moderating Effect of TAXI_OUT on Delay Propagation"
) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p4)
save_plot("image/objective4/p4.png", p4)




# Analysis 4-3: To develop a pre-departure predictive model by focused on pre-flight 
# observable factors (ORIGIN_AIRPORT, AIRLINE, MONTH) to identify inherent flight 
# delay risks before take-off for preventive intervention. (Predictive Analysis)
# ---------------------------------
# 4.3 Predictive Analysis
# ---------------------------------
# 4.3.1 prepare variable
flights_rf <- flights_clean |> 
  select(ARRIVAL_DELAY, AIRLINE, ORIGIN_AIRPORT, DESTINATION_AIRPORT,
         MONTH, DAY_OF_WEEK) |> 
  mutate(
    AIRLINE = as.factor(AIRLINE),
    ORIGIN_AIRPORT = as.factor(ORIGIN_AIRPORT),
    DESTINATION_AIRPORT = as.factor(DESTINATION_AIRPORT),
    MONTH = as.factor(MONTH),
    DAY_OF_WEEK = as.factor(DAY_OF_WEEK)
  )

# 4.3.2 split out training set and testing set
set.seed(42) 
train_ratio <- 0.7
sample_index <- sample(nrow(flights_rf), size = floor(train_ratio * nrow(flights_rf)))

train_data <- flights_rf[sample_index, ]
test_data <- flights_rf[-sample_index, ] 

# 4.3.3 train random forest model
rf_model <- randomForest(
  ARRIVAL_DELAY ~ ., 
  data = train_data,
  ntree = 500,       
  importance = TRUE  
)

# 4.3.4 model performance evaluation
rf_pred <- predict(rf_model, newdata = test_data)


#  calculate model performance 
# calculate RMSE (Root Mean Squared Error)
rf_rmse <- sqrt(mean((rf_pred - test_data$ARRIVAL_DELAY)^2))

# calculate R-squared 
rf_r2 <- 1 - sum((rf_pred - test_data$ARRIVAL_DELAY)^2) / 
  sum((mean(test_data$ARRIVAL_DELAY) - test_data$ARRIVAL_DELAY)^2)

print(paste("Random Forest RMSE:", round(rf_rmse, 2)))
print(paste("Random Forest R²:", round(rf_r2, 2)))

# 4.3.5 Prediction vs Actual Graph
performance_df <- data.frame(
  Actual = test_data$ARRIVAL_DELAY,
  Predicted = rf_pred
)

max_val <- max(performance_df$Actual, performance_df$Predicted) * 1.05
p5 <- ggplot(performance_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "#0072B2") + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 1) +
  coord_fixed(xlim = c(0, max_val), ylim = c(0, max_val)) + 
  labs(
    title = "Random Forest Model Performance (Actual vs. Predicted)",
    subtitle = paste0("RMSE: ", round(rf_rmse, 2), " | R²: ", round(rf_r2, 2)),
    x = "Actual ARRIVAL_DELAY (Mins)",
    y = "Predicted ARRIVAL_DELAY (Mins)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p5)
save_plot("image/objective4/p5.png", p5)


# 4.3.6 Variable Importance Visualization
importance_rf <- importance(rf_model)
importance_df <- data.frame(Variables = rownames(importance_rf), 
                            MeanDecreaseAccuracy = importance_rf[, "%IncMSE"]) |> 
  arrange(desc(MeanDecreaseAccuracy))

# Define variable categories for color differentiation
operational_vars <- c("ORIGIN_AIRPORT", "DESTINATION_AIRPORT", "AIRLINE")
contextual_vars <- c("MONTH", "DAY_OF_WEEK")

importance_df_colored <- importance_df |> 
  mutate(
    Variable_Type = case_when(
      Variables %in% operational_vars ~ "Operational / Location",
      Variables %in% contextual_vars ~"Contextual / Time",
      TRUE ~ "Other" 
    ),
    Variables = factor(Variables, levels = rev(Variables)) 
  )

# %IncMSE plot
p6 <- ggplot(importance_df_colored, 
             aes(x = Variables, y = MeanDecreaseAccuracy, fill = Variable_Type)) +
  
  geom_bar(stat = "identity") +
  coord_flip() + 
  scale_fill_manual(values = c("Operational / Location" = "lightblue", 
                               "Contextual / Time" = "lightcoral"
  )) +
  geom_text(aes(label = round(MeanDecreaseAccuracy, 0)), 
            hjust = -0.1, 
            size = 3.5) +
  labs(
    title = "Pre-Departure Model Variable Importance",
    subtitle = "Ranked by Mean Decrease in MSE (%IncMSE)",
    x = "Variables",
    y = "Importance Score (% Increase in MSE)",
    fill = "Variable Category"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p6)
save_plot("image/objective4/p6.png", p6)




# Analysis 4-4: Quantify each airport's average total contribution in minutes 
# across the two core risk factors and identify the top 15 origin airports with 
# the highest minute. (Prescriptive Analysis)
# ---------------------------------
# 4.4 Prescriptive Analysis
# ---------------------------------
# 4.4.1 Calculate the two core delay minutes for each airports
airport_risk_for_prescriptive <- flights_clean |> 
  group_by(ORIGIN_AIRPORT) |> 
  mutate(CORE_DELAY = AIRLINE_DELAY + LATE_AIRCRAFT_DELAY) |> 
  summarise(Mean_Core_Delay = mean(CORE_DELAY, na.rm = TRUE)) |> 
  ungroup() |> 
  arrange(desc(Mean_Core_Delay)) |> 
  slice_head(n = 15) |> 
  mutate(ORIGIN_AIRPORT = factor(ORIGIN_AIRPORT, levels = rev(ORIGIN_AIRPORT)))

# 4.4.2 lollipop chart
p7<- ggplot(airport_risk_for_prescriptive, 
            aes(x = ORIGIN_AIRPORT, y = (Mean_Core_Delay))) +
  geom_segment(aes(xend =  ORIGIN_AIRPORT, y = 0, yend = Mean_Core_Delay), 
               color = "grey50", linewidth = 1) +
  geom_point(color = "#0072B2", size = 4) +
  geom_text(aes(label = scales::number(Mean_Core_Delay, accuracy = 1)), 
            vjust = -0.5, size = 4, color = "black")+
  labs(
    title = "Top 15 Origin Airports with the Highest Average Core Delay Per Flight",
    subtitle = "Based on average total minutes of AIRLINE_DELAY and LATE_AIRCRAFT_DELAY (Core Risk Factors)",
    x = "Origin Airports",
    y = "Mean_Core_Delay(minute)"
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text.x = element_text(face = "bold", angle = 0),
    axis.text.y = element_text(size = 10)
  )

print(p7)
save_plot("image/objective4/p7.png", p7)


