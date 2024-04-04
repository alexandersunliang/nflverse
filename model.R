library(nflverse)
library(dplyr)
library(tidyverse)
library(caret)
qb_regular_season_stats <- 
  load_player_stats(seasons = TRUE) |> 
  filter(season_type == "REG" & position == "QB")
# Assuming qb_regular_season_stats has been loaded as per the instructions

# Remove rows with any NA values in relevant columns (e.g., passing_epa)
qb_regular_season_stats_clean <- qb_regular_season_stats %>%
  select(player_id, season, week, recent_team, passing_epa) %>%  # Select relevant columns
  na.omit()  # Remove rows with NAs in any of the selected columns

# For simplicity, we'll use data up to Week 9 of the 2023 season for modeling
qb_data_for_modeling <- qb_regular_season_stats_clean %>%
  filter(season == 2023, week <= 9)
qb_data_for_modeling <- qb_data_for_modeling %>%
  group_by(player_id) %>%
  mutate(avg_passing_epa = mean(passing_epa, na.rm = TRUE)) %>%
  ungroup()
# Split data - using a simple approach for demonstration
set.seed(42)  # Ensure reproducibility
training_indices <- createDataPartition(qb_data_for_modeling$avg_passing_epa, p = 0.8, list = FALSE)
train_data <- qb_data_for_modeling[training_indices, ]
test_data <- qb_data_for_modeling[-training_indices, ]

# Train a simple linear regression model
model <- lm(passing_epa ~ avg_passing_epa, data = train_data)

# Summary to inspect model coefficients and statistics
summary(model)

# Predict on the test set
predictions <- predict(model, newdata = test_data)

# Evaluation with simple metrics such as MAE and RMSE
test_data$predicted_epa = predictions
mae <- mean(abs(test_data$predicted_epa - test_data$passing_epa))
rmse <- sqrt(mean((test_data$predicted_epa - test_data$passing_epa)^2))

print(paste("MAE:", mae))
print(paste("RMSE:", rmse))
# Assuming you've already computed the `predictions`, `mae`, and `rmse`

# Create a dataframe to display in the table
results_df <- data.frame(
  Actual_EPA = test_data$passing_epa,
  Predicted_EPA = predictions
)

# Use knitr to create a nice table
library(knitr)

# Create the table of actual vs predicted values
results_table <- kable(results_df, 
                       col.names = c("Actual EPA", "Predicted EPA"), 
                       caption = "Actual vs Predicted EPA Values", 
                       align = 'c', digits = 2)

# Append MAE and RMSE to the table
error_metrics <- data.frame(
  Actual_EPA = c("MAE", "RMSE"),
  Predicted_EPA = c(mae, rmse)
)

print(results_table)

