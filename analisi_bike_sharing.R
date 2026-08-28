# ============================================================
# BIKE SHARING ANALYSIS
# Exploratory Data Analysis + Random Forest Regression
# ============================================================

# ----------------------------
# 1. PACKAGES
# ----------------------------
required_packages <- c(
  "tidyverse",
  "lubridate",
  "randomForest",
  "corrplot",
  "scales",
  "mgcv"
)

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing packages: ", paste(missing_packages, collapse = ", "),
      ". Install them before running the script."
    )
  )
}

library(tidyverse)
library(lubridate)
library(randomForest)
library(corrplot)
library(scales)
library(mgcv)

set.seed(500)


# ----------------------------
# 2. LOAD DATA
# ----------------------------
# The script first looks for data/9.csv and then for 9.csv
candidate_paths <- c(file.path("data", "bike_sharing.csv"), "9.csv")
DATA_PATH <- candidate_paths[file.exists(candidate_paths)][1]

if (is.na(DATA_PATH)) {
  stop(
    "Dataset not found. Place '9.csv' either in the project root or inside a 'data/' folder."
  )
}

bike_raw <- read.csv(DATA_PATH, header = TRUE, sep = ",", stringsAsFactors = FALSE)

cat("Original dataset dimensions:", nrow(bike_raw), "rows x", ncol(bike_raw), "columns\n")
str(bike_raw)


# ----------------------------
# 3. DATA CLEANING
# ----------------------------
# Split datetime into date and hour while keeping the original field.
bike <- bike_raw %>%
  separate(
    datetime,
    into = c("date", "time"),
    sep = " ",
    remove = FALSE
  )

# Build the complete hourly sequence for the first 19 days of each month,
# matching the structure of the original Bike Sharing dataset.
full_time_grid <- tibble(
  date_hour = seq(
    from = as.POSIXct("2011-01-01 00:00:00", tz = "UTC"),
    to   = as.POSIXct("2012-12-31 23:00:00", tz = "UTC"),
    by   = "hour"
  )
) %>%
  filter(day(date_hour) <= 19) %>%
  mutate(
    date = format(date_hour, "%Y-%m-%d"),
    time = format(date_hour, "%H:%M:%S")
  )

# Join the original observations to the complete hourly grid.
bike_clean <- full_time_grid %>%
  left_join(bike, by = c("date", "time"))

# Missing rental counts correspond to hours not present in the original file.
bike_clean <- bike_clean %>%
  mutate(
    count = replace_na(count, 0),
    casual = replace_na(casual, 0),
    registered = replace_na(registered, 0)
  )

# Fill explanatory variables for newly created timestamps.
# Down/up filling avoids leaving missing values at the beginning/end of the series.
bike_clean <- bike_clean %>%
  fill(
    season,
    holiday,
    workingday,
    weather,
    temp,
    atemp,
    humidity,
    windspeed,
    .direction = "downup"
  )

# Create useful calendar features.
bike_clean <- bike_clean %>%
  mutate(
    year = year(date_hour),
    month = month(date_hour),
    hour = hour(date_hour),
    season = factor(
      season,
      levels = c(1, 2, 3, 4),
      labels = c("Winter", "Spring", "Summer", "Autumn")
    ),
    weather = factor(
      weather,
      levels = c(1, 2, 3, 4),
      labels = c("Good", "Normal", "Poor", "Bad")
    ),
    holiday = factor(
      holiday,
      levels = c(0, 1),
      labels = c("Not holiday", "Holiday")
    ),
    workingday = factor(
      workingday,
      levels = c(0, 1),
      labels = c("Non-working day", "Working day")
    ),
    across(c(temp, atemp, humidity, windspeed, casual, registered), as.numeric)
  ) %>%
  select(
    date_hour,
    year,
    month,
    hour,
    season,
    holiday,
    workingday,
    weather,
    temp,
    atemp,
    humidity,
    windspeed,
    casual,
    registered
  )

cat("Clean dataset dimensions:", nrow(bike_clean), "rows x", ncol(bike_clean), "columns\n")
summary(bike_clean)


# ----------------------------
# 4. EXPLORATORY DATA ANALYSIS
# ----------------------------

# Average hourly rentals by year.
yearly_means <- bike_clean %>%
  group_by(year) %>%
  summarise(
    mean_registered = mean(registered, na.rm = TRUE),
    mean_casual = mean(casual, na.rm = TRUE),
    .groups = "drop"
  )

print(yearly_means)

# Monthly rental trends.
monthly_rentals <- bike_clean %>%
  mutate(month_date = floor_date(date_hour, unit = "month")) %>%
  group_by(month_date) %>%
  summarise(
    registered = sum(registered, na.rm = TRUE),
    casual = sum(casual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(registered, casual),
    names_to = "user_type",
    values_to = "rentals"
  )

plot_monthly <- ggplot(
  monthly_rentals,
  aes(x = month_date, y = rentals, group = user_type, linetype = user_type)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Monthly Bike Rentals",
    subtitle = "Registered vs casual users",
    x = NULL,
    y = "Number of rentals",
    linetype = "User type"
  ) +
  theme_minimal()

print(plot_monthly)

# Rental share by season.
season_distribution <- bike_clean %>%
  group_by(season) %>%
  summarise(
    registered = sum(registered, na.rm = TRUE),
    casual = sum(casual, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(registered, casual),
    names_to = "user_type",
    values_to = "rentals"
  ) %>%
  group_by(user_type) %>%
  mutate(share = rentals / sum(rentals)) %>%
  ungroup()

plot_season <- ggplot(
  season_distribution,
  aes(x = season, y = share, fill = user_type)
) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Rental Distribution by Season",
    x = "Season",
    y = "Share of rentals",
    fill = "User type"
  ) +
  theme_minimal()

print(plot_season)

# Working-day / holiday comparison.
plot_day_type <- bike_clean %>%
  select(holiday, registered, casual) %>%
  pivot_longer(
    cols = c(registered, casual),
    names_to = "user_type",
    values_to = "rentals"
  ) %>%
  ggplot(aes(x = holiday, y = rentals, fill = user_type)) +
  geom_boxplot(outlier.alpha = 0.15) +
  labs(
    title = "Bike Rentals by Day Type",
    x = NULL,
    y = "Hourly rentals",
    fill = "User type"
  ) +
  theme_minimal()

print(plot_day_type)

# Weather comparison.
plot_weather <- bike_clean %>%
  select(weather, registered, casual) %>%
  pivot_longer(
    cols = c(registered, casual),
    names_to = "user_type",
    values_to = "rentals"
  ) %>%
  ggplot(aes(x = weather, y = rentals, fill = user_type)) +
  geom_boxplot(outlier.alpha = 0.15) +
  labs(
    title = "Bike Rentals by Weather Condition",
    x = "Weather",
    y = "Hourly rentals",
    fill = "User type"
  ) +
  theme_minimal()

print(plot_weather)

# Temperature and humidity relationships.
plot_temperature <- bike_clean %>%
  select(temp, registered, casual) %>%
  pivot_longer(
    cols = c(registered, casual),
    names_to = "user_type",
    values_to = "rentals"
  ) %>%
  ggplot(aes(x = temp, y = rentals)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE) +
  facet_wrap(~ user_type, scales = "free_y") +
  labs(
    title = "Bike Rentals and Temperature",
    x = "Temperature",
    y = "Hourly rentals"
  ) +
  theme_minimal()

print(plot_temperature)

plot_humidity <- bike_clean %>%
  select(humidity, registered, casual) %>%
  pivot_longer(
    cols = c(registered, casual),
    names_to = "user_type",
    values_to = "rentals"
  ) %>%
  ggplot(aes(x = humidity, y = rentals)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE) +
  facet_wrap(~ user_type, scales = "free_y") +
  labs(
    title = "Bike Rentals and Humidity",
    x = "Humidity",
    y = "Hourly rentals"
  ) +
  theme_minimal()

print(plot_humidity)

# Correlation matrix for numerical variables.
numeric_data <- bike_clean %>%
  select(year, month, hour, temp, atemp, humidity, windspeed, casual, registered)

correlation_matrix <- cor(numeric_data, use = "complete.obs")

corrplot(
  correlation_matrix,
  type = "upper",
  method = "color",
  addCoef.col = "black",
  tl.cex = 0.8,
  number.cex = 0.7
)


# ----------------------------
# 5. TRAIN / TEST SPLIT
# ----------------------------
# Use one split for both targets so model comparisons are based on the same rows.
train_index <- sample(
  seq_len(nrow(bike_clean)),
  size = floor(0.75 * nrow(bike_clean)),
  replace = FALSE
)

train_data <- bike_clean[train_index, ]
test_data <- bike_clean[-train_index, ]

cat("Training observations:", nrow(train_data), "\n")
cat("Test observations:", nrow(test_data), "\n")


# ----------------------------
# 6. RANDOM FOREST MODELLING
# ----------------------------
# date_hour is excluded because year/month/hour already represent temporal information.
# The other response variable is also excluded to avoid target leakage.

mtry_values <- c(5, 8, 10)

fit_random_forest_models <- function(target, train_df, test_df, mtry_grid) {
  opposite_target <- ifelse(target == "registered", "casual", "registered")
  
  model_train <- train_df %>%
    select(-date_hour, -all_of(opposite_target))
  
  model_test <- test_df %>%
    select(-date_hour, -all_of(opposite_target))
  
  formula_rf <- as.formula(paste(target, "~ ."))
  
  models <- lapply(mtry_grid, function(mtry_value) {
    randomForest(
      formula = formula_rf,
      data = model_train,
      ntree = 500,
      mtry = mtry_value,
      importance = TRUE
    )
  })
  
  names(models) <- paste0("mtry_", mtry_grid)
  
  performance <- map2_dfr(models, mtry_grid, function(model, mtry_value) {
    predictions <- predict(model, newdata = model_test)
    
    tibble(
      mtry = mtry_value,
      oob_mse = tail(model$mse, 1),
      test_mse = mean((model_test[[target]] - predictions)^2)
    )
  })
  
  # Select the model using OOB MSE only; the test set remains an independent check.
  best_mtry <- performance$mtry[which.min(performance$oob_mse)]
  best_model <- models[[paste0("mtry_", best_mtry)]]
  
  list(
    models = models,
    performance = performance,
    best_mtry = best_mtry,
    best_model = best_model,
    test_data = model_test
  )
}

registered_rf <- fit_random_forest_models(
  target = "registered",
  train_df = train_data,
  test_df = test_data,
  mtry_grid = mtry_values
)

casual_rf <- fit_random_forest_models(
  target = "casual",
  train_df = train_data,
  test_df = test_data,
  mtry_grid = mtry_values
)

cat("\nRegistered-user models:\n")
print(registered_rf$performance)
cat("Selected mtry based on OOB MSE:", registered_rf$best_mtry, "\n")

cat("\nCasual-user models:\n")
print(casual_rf$performance)
cat("Selected mtry based on OOB MSE:", casual_rf$best_mtry, "\n")


# ----------------------------
# 7. MSE EVOLUTION
# ----------------------------
extract_mse_history <- function(model_list, target_name) {
  imap_dfr(model_list, function(model, model_name) {
    tibble(
      trees = seq_along(model$mse),
      mse = model$mse,
      mtry = sub("mtry_", "", model_name),
      target = target_name
    )
  })
}

mse_history <- bind_rows(
  extract_mse_history(registered_rf$models, "Registered"),
  extract_mse_history(casual_rf$models, "Casual")
)

plot_mse <- ggplot(
  mse_history,
  aes(x = trees, y = mse, linetype = mtry)
) +
  geom_line() +
  facet_wrap(~ target, scales = "free_y") +
  labs(
    title = "Out-of-Bag MSE Across Trees",
    x = "Number of trees",
    y = "OOB MSE",
    linetype = "mtry"
  ) +
  theme_minimal()

print(plot_mse)


# ----------------------------
# 8. VARIABLE IMPORTANCE
# ----------------------------
extract_importance <- function(model, target_name) {
  importance_df <- importance(model, type = 1) %>%
    as.data.frame()
  
  tibble(
    variable = rownames(importance_df),
    importance = importance_df[[1]],
    target = target_name
  ) %>%
    arrange(desc(importance))
}

importance_registered <- extract_importance(
  registered_rf$best_model,
  "Registered"
)

importance_casual <- extract_importance(
  casual_rf$best_model,
  "Casual"
)

importance_all <- bind_rows(
  importance_registered,
  importance_casual
)

print(importance_registered)
print(importance_casual)

plot_importance <- ggplot(
  importance_all,
  aes(x = reorder(variable, importance), y = importance)
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ target, scales = "free") +
  labs(
    title = "Random Forest Variable Importance",
    x = NULL,
    y = "% Increase in MSE"
  ) +
  theme_minimal()

print(plot_importance)


# ----------------------------
# 9. FINAL MODEL SUMMARY
# ----------------------------
model_summary <- bind_rows(
  registered_rf$performance %>%
    mutate(target = "Registered"),
  casual_rf$performance %>%
    mutate(target = "Casual")
) %>%
  select(target, mtry, oob_mse, test_mse) %>%
  arrange(target, oob_mse)

cat("\nModel comparison:\n")
print(model_summary)

cat("\nBest registered-user model: mtry =", registered_rf$best_mtry, "\n")
cat("Best casual-user model: mtry =", casual_rf$best_mtry, "\n")