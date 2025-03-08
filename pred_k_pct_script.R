############## Loading Libraries Function ####################

load_libraries <- function(packages) {
  # Check for missing packages
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # install missing packages
  if(length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
  
  # Load all packages
  lapply(packages, library, character.only = TRUE)
  
  cat("All packages are loaded succesfully.\n")
}


# Loading necessary libraries
load_libraries(c("tidyverse", "lubridate", "stats", "ggplot2", "corrplot",
                 "tidymodels", "modeldata", "themis", "vip", "baguette",
                 "yardstick", "gsheet", "caret", "randomForest", "here", "tibble", "dplyr", "ISAR", "tidyr", "mgcv",
                 "teamcolors", "baseballr", "Lahman", "remotes", "ggcorrplot", "broom", "readr", "glmnet", "xgboost", "Matrix"))

# Load only the necessary functions from 'car'
library(car, exclude = "select")

# Turning off warning messages
options(warnings = 0)



#################### Loading datasets #########################################

# Provided dataset to predict K%
dse_6620 <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1FJe0862XJQX2VIxi3Y-zvo3g3XF68gUzgC96okl-lIk/edit?gid=1675022910#gid=1675022910")

# The following datasets were exported from Fangraphs. All datasets are filtered from the 2021 season to the 2024 season with a minimum IP of 30.
# The year 2024 will be used only to predict the K%.

# Standard pitcher data
standard <- gsheet2tbl("https://docs.google.com/spreadsheets/d/13HrhJ7xaxdrqBvxKj9viuH6ssnyrcZ2acTsED0z7Av0/edit?gid=996093168#gid=996093168")

# Batted ball pitcher data. Only interested in balls, strikes, and pitches here.
batted_ball <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1lRE0OveLeoC0SChPd85j5xac7toc3jxzi_2lNCnqa6w/edit?gid=333204929#gid=333204929")

# Statcast data that includes the percentage of pitch type thrown by pitcher
# The gsheet2tbl function was not reading KN%, SC%, and FO% properly, so we had to adjust for that
url_pitch_type <- "https://docs.google.com/spreadsheets/d/115Q7zOQcwUF4ZJf5ikbKgyycMZJ4W5pwN_cSJjk6INQ/gviz/tq?tqx=out:csv"
pitch_type <- read_csv(url_pitch_type, col_types = cols(
  `KN%` = col_double(),
  `SC%` = col_double(),
  `FO%` = col_double()
))

# Statcast data that shows the average velocity of each pitch type
velocity <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1JzLD9vRPnLCSYL_cPfpTNLcYt0Dugqp0cCHarVkU6e4/edit?gid=1519052523#gid=1519052523")

# Statcast data that shows the horizontal movement of each pitch type
h_movement <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1IF-Bm3eNHErs9szG5WqbCefKV27QF8hawjOFep5vqII/edit?gid=1735867164#gid=1735867164")

# Statcast data that shows the vertival movement of each pitch type
v_movement <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1G1uou7v9NUE_IwTrScT8daU0niCM44ZEPNr8pHz9SAE/edit?gid=728882332#gid=728882332")

# Statcast data that shows batters plate discipline against pitchers. Shows the percentage of contact and swings in zone vs out of zone
plate_discipline <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1OmuTfGUP5khUpW5xXjxbstIWy8UDKOfmlHTtAUC7X8g/edit?gid=1214043071#gid=1214043071")

################### Data Cleaning ############################################

# Looking at the structure of standard dataset to see if there are any variables that need to be changed.
str(standard)

# Selecting only the necessary variables in the batted_ball dataset
batted_ball <- batted_ball %>% 
  select(Season, Name, Team, NameASCII, PlayerId, MLBAMID, Balls, Strikes, Pitches) %>% 
  mutate(
    strike_pct = (Strikes / Pitches), # creating a variable for strikes thrown percentage
    ball_pct = (Balls / Pitches)      # creating a variable for balls thrown percentage
  )


### Join all the datasets together ###

# Joining dse_6620 and standard datasets to create pitcher_df
pitcher_df <- left_join(dse_6620, standard,
                        by = c("MLBAMID", "PlayerId", "Name", "Team", "Season", "TBF"))

# Joining pitcher_df dataset and batted_ball dataset
pitcher_df <- left_join(pitcher_df, batted_ball, 
                        by = c("PlayerId", "MLBAMID", "Season", "Name", "Team", "NameASCII"))

# Joining pitcher_df and pitch_type
pitcher_df <- left_join(pitcher_df, pitch_type,
                        by = c("PlayerId", "MLBAMID", "Season", "Name", "Team", "NameASCII", "IP"))

# Joining pitcher_df and velocity
pitcher_df <- left_join(pitcher_df, velocity,
                        by = c("PlayerId", "MLBAMID", "Season", "Name", "Team", "NameASCII", "IP"))

# Joining pitcher_df and h_movement
pitcher_df <- left_join(pitcher_df, h_movement,
                        by = c("PlayerId", "MLBAMID", "Season", "Name", "Team", "NameASCII", "IP"))

# Joining pitcher_df and v_movement
pitcher_df <- left_join(pitcher_df, v_movement,
                        by = c("PlayerId", "MLBAMID", "Season", "Name", "Team", "NameASCII", "IP"))

# Joining pitcher_df and plate_discipline
pitcher_df <- left_join(pitcher_df, plate_discipline,
                        by = c("PlayerId", "MLBAMID", "Season", "Name", "Team", "NameASCII", "IP"))


# Dimension of pitcher_df
dim(pitcher_df) # we have a total of 1892 observations and 97 variables

### Creating a dataset for just the 2024 season. This is the dataset we will be making our predictions on.
pitcher_2024 <- pitcher_df %>% 
  filter(Season == 2024)

### Filtering for Seasons 2021-2023 because we can't use the 2024 season.
### This is the dataset we will be working with to create our models.
pitcher_21_23 <- pitcher_df %>% 
  filter(Season <= 2023)

############### Data Exploration ##########################

# Select only numeric columns and compute correlation
cor_matrix <- cor(select_if(pitcher_21_23, is.numeric), use = "pairwise.complete.obs")

# Extract correlations with K%
k_percent_cor <- cor_matrix[, "K%"] %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  rename(Correlation = ".")

# View top correlations
print(k_percent_cor)


# Check distribution of K%
ggplot(pitcher_21_23, aes(x = `K%`)) +
  geom_histogram(binwidth = .03, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of K%", x = "Strikeout Percentage (K%)", y = "Count") +
  theme_bw()

# Scatterplot: Relationship between fastball velocity (vFA) and K%
ggplot(data = pitcher_21_23,
       aes(x = vFA, y = `K%`)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", col = "red") + 
  labs(title = "K% vs. Fastball Velocity (vFA)", x = "Fastball velocity", y = "strikeout percentage") +
  theme_bw()

# Scatterplot: Relationship between splitter velocity (vFS) and K%
ggplot(pitcher_21_23,
       aes(x = vFS, y = `K%`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "K% vs. Splitter Velocity (vFS)", x = "Splitter Velocity", y = "Strikeout Percentage (K%)") +
  theme_bw()

# Scatterplot: Relationship between O-Swing% (% of pitches batters swing outside of strikezone) and K%
ggplot(pitcher_21_23,
       aes(x = `O-Swing%`, y = `K%`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "K% vs. % of Pitches Batters Swing Outside of Strikezone (O-Swing%)",
       x = "O-Swing%", y = "Strikeout Percentage (K%)") +
  theme_bw()

# Scatterplot: Relationship between games played (G) and K%
ggplot(pitcher_21_23,
       aes(x = G, y = `K%`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "K% vs. Games Played (G)",
       x = "Games Played (G)", y = "Strikeout Percentage (K%)") +
  theme_bw()

# Scatterplot: Relationship between sinker velocity (vSI) and K%
ggplot(pitcher_21_23,
       aes(x = vSI, y = `K%`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "K% vs. Sinker Velocity (vSI)",
       x = "Sinker Velocity (vSI)", y = "Strikeout Percentage (K%)") +
  theme_bw()

# Scatterplot: Relationship between Contact % and K%
ggplot(pitcher_21_23,
       aes(x = `Contact%`, y = `K%`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "K% vs. Percent of Contact Batters Made When Swinging (Contact%)",
       x = "Contact%", y = "Strikeout Percentage (K%)") +
  theme_bw()

# Scatterplot: Relationship between Z-Contact% and K%
ggplot(pitcher_21_23,
       aes(x = `Z-Contact%`, y = `K%`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "K% vs. Percent of Contact Batters Made When Swinging at Pitches Inside Strikezone",
       x = "Z-Contact%", y = "Strikeout Percentage (K%)") +
  theme_bw()

# Scatterplot: Relationship between O-Contact% and K%
ggplot(pitcher_21_23,
       aes(x = `O-Contact%`, y = `K%`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "K% vs. Percent of Contact Batters Made When Swinging at Pitches Outside Strikezone",
       x = "O-Contact%", y = "Strikeout Percentage (K%)") +
  theme_bw()

# Scatterplot: Relationship between ERA and K%
ggplot(pitcher_21_23,
       aes(x = ERA, y = `K%`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "K% vs. Earned Runs Average (ERA)",
       x = "ERA", y = "Strikeout Percentage (K%)") +
  theme_bw()

################### Selecting Predictors ####################

# We want to create a model with the following variables as predictors: vFA, O-Swing%, Contact%, Z-Contact%, O-Contact%, and ERA
# Before we continue, we are going to see if there are any NAs in those columns
sapply(pitcher_df[, c("vFA", "O-Swing%", "Contact%", "Z-Contact%", "O-Contact%", "ERA")], function(x) sum(is.na(x)))

# There are 102 pitchers with missing vFA. We need to replace those NAs with a value.
# We should notice that many pitchers who have vFA missing have vSI instead.

# Count pitchers where vFA is missing but vSI is present
missing_vFA_with_vSI <- pitcher_df %>% filter(is.na(vFA) & !is.na(vSI))

# Print summary
cat("Total pitchers missing vFA but have vSI:", nrow(missing_vFA_with_vSI), "\n") # There are 100 total

# We are going to replace the NAs in vFA for those pitchers with their vSI values instead.
# Replace missing vFA with vSI where vSI is available
pitcher_df <- pitcher_df %>%
  mutate(vFA = ifelse(is.na(vFA) & !is.na(vSI), vSI, vFA))

# Check if any remaining NA values exist in vFA
sum(is.na(pitcher_df$vFA)) # There are still 2 NAs

# Identify pitchers missing both vFA and vSI
missing_both <- pitcher_df %>% filter(is.na(vFA) & is.na(vSI))

# Looking who the pitchers are that are missing vFA and vSI
missing_both %>% select(Name, Team, Season, vFA, vSI)

# Filter for all rows where David Robertson or Wander Suero appear
missing_pitchers <- pitcher_df %>%
  filter(Name %in% c("David Robertson", "Wander Suero")) %>%
  select(Name, Team, Season, vFA, vFC, vSI)  # Select relevant pitch velocity columns

# Display results
print(missing_pitchers)

# Replace missing vFA with vFC for David Robertson and Wander Suero
pitcher_df <- pitcher_df %>%
  mutate(vFA = ifelse(is.na(vFA) & !is.na(vFC), vFC, vFA))

# Verify if any missing values remain in vFA
sum(is.na(pitcher_df$vFA))

### Updating the pitcher_2024 dataset since the NAs for vFA have been filled
pitcher_2024 <- pitcher_df %>% 
  filter(Season == 2024)

### Updating the pitcher_21_23 dataset since the NAs for vFA have been filled
pitcher_21_23 <- pitcher_df %>% 
  filter(Season <= 2023)


################### Linear Regression Model ########################

# Select relevant variables for the model
model_data <- pitcher_21_23 %>%
  select(`K%`, vFA, `O-Swing%`, `Contact%`, `Z-Contact%`, `O-Contact%`, ERA)

# Split data into training (80%) and testing (20%) sets
set.seed(123)  # Ensures reproducibility
train_indices <- createDataPartition(model_data$`K%`, p = 0.8, list = FALSE)
train_data <- model_data[train_indices, ]
test_data  <- model_data[-train_indices, ]

# Fit a linear regression model
lm_model <- lm(`K%` ~ vFA + `O-Swing%` + `Contact%` + `Z-Contact%` + `O-Contact%` + ERA, data = train_data)

# Print model summary
summary(lm_model)

# Predict on test set
predictions <- predict(lm_model, newdata = test_data)

# Function to compute MAE
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Evaluate model performance
lm_mae <- mae(test_data$`K%`, predictions)
rmse <- sqrt(mean((predictions - test_data$`K%`)^2))  # Root Mean Squared Error
r2 <- cor(predictions, test_data$`K%`)^2  # R-squared

# Print results
cat("Linear Regression MAE:", lm_mae, "\n")
cat("Model RMSE:", rmse, "\n")
cat("Model R-squared:", r2, "\n")

# Plot actual vs. predicted K%
ggplot(test_data, aes(x = `K%`, y = predictions)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted K%", x = "Actual K%", y = "Predicted K%")


# Computing residuals 
linear_residuals <- test_data$`K%` - predictions  # Linear model

# Function to create QQ plot
qq_plot <- function(residuals, title) {
  ggplot(data = data.frame(residuals = residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(title = title, x = "Theoretical Quantiles", y = "Sample Quantiles")
}

# QQ Plot for Linear Model
qq_plot_linear <- qq_plot(linear_residuals, "QQ Plot - Linear Regression")

# Display the qq plot
print(qq_plot_linear)



################# Lasso and Ridge Regression Models #########################


# Prepare data: Convert to matrix format (glmnet requires matrix inputs)
X_train <- model.matrix(`K%` ~ vFA + `O-Swing%` + `Contact%` + `Z-Contact%` + `O-Contact%` + ERA, data = train_data)[, -1]
y_train <- train_data$`K%`

X_test <- model.matrix(`K%` ~ vFA + `O-Swing%` + `Contact%` + `Z-Contact%` + `O-Contact%` + ERA, data = test_data)[, -1]
y_test <- test_data$`K%`

# Define lambda values to test
lambda_seq <- 10^seq(3, -3, by = -0.1)

# Train Ridge Regression (alpha = 0 for Ridge)
ridge_model <- cv.glmnet(X_train, y_train, alpha = 0, lambda = lambda_seq, standardize = TRUE)
best_lambda_ridge <- ridge_model$lambda.min  # Optimal lambda

# Train Lasso Regression (alpha = 1 for Lasso)
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1, lambda = lambda_seq, standardize = TRUE)
best_lambda_lasso <- lasso_model$lambda.min  # Optimal lambda

# Predict on test data using best lambda for Ridge
ridge_predictions <- predict(ridge_model, s = best_lambda_ridge, newx = X_test)
ridge_rmse <- sqrt(mean((ridge_predictions - y_test)^2))
ridge_r2 <- cor(ridge_predictions, y_test)^2
ridge_mae <- mae(test_data$`K%`, ridge_predictions)

# Predict on test data using best lambda for Lasso
lasso_predictions <- predict(lasso_model, s = best_lambda_lasso, newx = X_test)
lasso_rmse <- sqrt(mean((lasso_predictions - y_test)^2))
lasso_r2 <- cor(lasso_predictions, y_test)^2
lasso_mae <- mae(test_data$`K%`, lasso_predictions)

# Print results
cat("Ridge Regression RMSE:", ridge_rmse, "\n")
cat("Ridge Regression R-squared:", ridge_r2, "\n")
cat("Ridge Regression MAE:", ridge_mae, "\n")
cat("Lasso Regression RMSE:", lasso_rmse, "\n")
cat("Lasso Regression R-squared:", lasso_r2, "\n")
cat("Lasso Regression MAE:", lasso_mae, "\n")


# Compute residuals for each model
ridge_residuals <- y_test - ridge_predictions  # Ridge model
lasso_residuals <- y_test - lasso_predictions  # Lasso model

# QQ Plot for Ridge Model
qq_plot_ridge <- qq_plot(ridge_residuals, "QQ Plot - Ridge Regression")

# QQ Plot for Lasso Model
qq_plot_lasso <- qq_plot(lasso_residuals, "QQ Plot - Lasso Regression")

# Display the plots
print(qq_plot_ridge)
print(qq_plot_lasso)

##################### Random Forrest Model ####################################

# Create copies of train_data and test_data for Random Forest
rf_train_data <- train_data
rf_test_data <- test_data

# Rename columns to remove special characters in rf_train_data and rf_test_data since it does not like the ``
rf_train_data <- rf_train_data %>%
  rename(O_Swing = `O-Swing%`,
         Contact = `Contact%`,
         Z_Contact = `Z-Contact%`,
         O_Contact = `O-Contact%`)

rf_test_data <- rf_test_data %>%
  rename(O_Swing = `O-Swing%`,
         Contact = `Contact%`,
         Z_Contact = `Z-Contact%`,
         O_Contact = `O-Contact%`)

# Set seed for reproducibility
set.seed(123)

# Train Random Forest model
rf_model <- randomForest(`K%` ~ vFA + O_Swing + Contact + Z_Contact + O_Contact + ERA,
                         data = rf_train_data, 
                         ntree = 500,  # Number of trees
                         mtry = 3,  # Number of variables randomly sampled at each split
                         importance = TRUE)

# Print model summary
print(rf_model)

# Predict on test set
rf_predictions <- predict(rf_model, newdata = rf_test_data)

# Evaluate model performance
rf_rmse <- sqrt(mean((rf_predictions - rf_test_data$`K%`)^2))  # Root Mean Squared Error
rf_r2 <- cor(rf_predictions, rf_test_data$`K%`)^2  # R-squared
rf_mae <- mae(rf_test_data$`K%`, rf_predictions)

# Print results
cat("Random Forest RMSE:", rf_rmse, "\n")
cat("Random Forest R-squared:", rf_r2, "\n")
cat("Random Forest MAE:", rf_mae, "\n")

# Feature Importance Plot
varImpPlot(rf_model)

### Random forest again but trying to see if we can improve it before moving on ###

# Define hyperparameter grid
tune_grid <- expand.grid(
  mtry = c(2, 3, 4),  # Number of variables randomly sampled at each split
  ntree = c(500, 1000),  # Number of trees
  nodesize = c(5, 10, 15),  # Minimum samples per terminal node
  maxnodes = c(50, 100, 150)  # Maximum number of terminal nodes
)

# Initialize variables to store results
best_rmse <- Inf
best_model <- NULL

# Grid search loop
for (i in 1:nrow(tune_grid)) {
  
  set.seed(42)  # Ensure reproducibility
  
  rf_tuned_model <- randomForest(`K%` ~ vFA + O_Swing + Contact + Z_Contact + O_Contact + ERA,
                                 data = rf_train_data, 
                                 ntree = tune_grid$ntree[i], 
                                 mtry = tune_grid$mtry[i],
                                 nodesize = tune_grid$nodesize[i],
                                 maxnodes = tune_grid$maxnodes[i],
                                 importance = TRUE)
  
  # Predict on test set
  rf_tuned_predictions <- predict(rf_tuned_model, newdata = rf_test_data)
  
  # Compute RMSE
  rf_tuned_rmse <- sqrt(mean((rf_tuned_predictions - rf_test_data$`K%`)^2))
  
  # Store best model based on RMSE
  if (rf_tuned_rmse < best_rmse) {
    best_rmse <- rf_tuned_rmse
    best_model <- rf_tuned_model
  }
}

# Print best model details
print(best_model)

# Evaluate best model
rf_final_predictions <- predict(best_model, newdata = rf_test_data)
rf_final_rmse <- sqrt(mean((rf_final_predictions - rf_test_data$`K%`)^2))
rf_final_r2 <- cor(rf_final_predictions, rf_test_data$`K%`)^2
rf_final_mae <- mae(rf_test_data$`K%`, rf_final_predictions)

# Print results
cat("Tuned Random Forest RMSE:", rf_final_rmse, "\n")
cat("Tuned Random Forest R-squared:", rf_final_r2, "\n")
cat("Tuned Random Forest MAE:", rf_final_mae, "\n")

# Feature Importance Plot
varImpPlot(best_model)


##################### Gradient Boosting Model (XGBoost) ##########################


# Prepare data for XGBoost (convert to matrix format)
xgb_train <- as.matrix(rf_train_data %>% select(vFA, O_Swing, Contact, Z_Contact, O_Contact, ERA))
xgb_test <- as.matrix(rf_test_data %>% select(vFA, O_Swing, Contact, Z_Contact, O_Contact, ERA))

y_train <- rf_train_data$`K%`
y_test <- rf_test_data$`K%`

# Convert data into XGBoost format (DMatrix)
dtrain <- xgb.DMatrix(data = xgb_train, label = y_train)
dtest <- xgb.DMatrix(data = xgb_test, label = y_test)

# Define XGBoost parameters
xgb_params <- list(
  objective = "reg:squarederror",  # Regression task
  eval_metric = "rmse",  # Root Mean Squared Error
  eta = 0.1,  # Learning rate
  max_depth = 6,  # Depth of each tree
  subsample = 0.8,  # Sample 80% of data for each tree
  colsample_bytree = 0.8  # Use 80% of features per tree
)

# Train the XGBoost model
set.seed(123)
xgb_model <- xgb.train(params = xgb_params,
                       data = dtrain,
                       nrounds = 500,  # Number of boosting rounds
                       watchlist = list(train = dtrain, test = dtest),
                       early_stopping_rounds = 20,  # Stop if no improvement in 20 rounds
                       verbose = 1)

# Predict on test data
xgb_predictions <- predict(xgb_model, newdata = dtest)

# Compute RMSE and R-squared
xgb_rmse <- sqrt(mean((xgb_predictions - y_test)^2))
xgb_r2 <- cor(xgb_predictions, y_test)^2
xgb_mae <- mae(y_test, xgb_predictions)

# Print results
cat("XGBoost RMSE:", xgb_rmse, "\n")
cat("XGBoost R-squared:", xgb_r2, "\n")
cat("XGBoost MAE:", xgb_mae, "\n")

# Feature Importance Plot
xgb.importance(model = xgb_model) %>% xgb.plot.importance()

################## Retraining all models with G (games) added as a predictor #########################

# Recreate dataset including 'G' (Games)
model_data_G <- pitcher_21_23 %>%
  select(`K%`, vFA, `O-Swing%`, `Contact%`, `Z-Contact%`, `O-Contact%`, ERA, G)

# Split data into training (80%) and testing (20%) sets
set.seed(123)  # Ensures reproducibility
train_indices_G <- createDataPartition(model_data_G$`K%`, p = 0.8, list = FALSE)
train_data_G <- model_data_G[train_indices_G, ]
test_data_G  <- model_data_G[-train_indices_G, ]

# Retrain models with updated dataset
# Linear Regression
lm_model_new <- lm(`K%` ~ vFA + `O-Swing%` + `Contact%` + `Z-Contact%` + `O-Contact%` + ERA + G, data = train_data_G)
lm_predictions_new <- predict(lm_model_new, newdata = test_data_G)
lm_rmse_new <- sqrt(mean((lm_predictions_new - test_data_G$`K%`)^2))
lm_r2_new <- cor(lm_predictions_new, test_data_G$`K%`)^2
lm_mae_new <- mae(test_data_G$`K%`, lm_predictions_new)

# Ridge & Lasso Regression
X_train_new <- model.matrix(`K%` ~ vFA + `O-Swing%` + `Contact%` + `Z-Contact%` + `O-Contact%` + ERA + G, data = train_data_G)[, -1]
X_test_new <- model.matrix(`K%` ~ vFA + `O-Swing%` + `Contact%` + `Z-Contact%` + `O-Contact%` + ERA + G, data = test_data_G)[, -1]

ridge_model_new <- cv.glmnet(X_train_new, train_data_G$`K%`, alpha = 0)
lasso_model_new <- cv.glmnet(X_train_new, train_data_G$`K%`, alpha = 1)

ridge_predictions_new <- predict(ridge_model_new, s = ridge_model_new$lambda.min, newx = X_test_new)
lasso_predictions_new <- predict(lasso_model_new, s = lasso_model_new$lambda.min, newx = X_test_new)

ridge_rmse_new <- sqrt(mean((ridge_predictions_new - test_data_G$`K%`)^2))
ridge_r2_new <- cor(ridge_predictions_new, test_data_G$`K%`)^2
ridge_mae_new <- mae(test_data_G$`K%`, ridge_predictions_new)

lasso_rmse_new <- sqrt(mean((lasso_predictions_new - test_data_G$`K%`)^2))
lasso_r2_new <- cor(lasso_predictions_new, test_data_G$`K%`)^2
lasso_mae_new <- mae(test_data_G$`K%`, lasso_predictions_new)

# Random Forest

# Create copies of train_data and test_data for Random Forest
rf_train_data_G <- train_data_G
rf_test_data_G <- test_data_G

# Rename columns to remove special characters in rf_train_data and rf_test_data
rf_train_data_G <- rf_train_data_G %>%
  rename(O_Swing = `O-Swing%`,
         Contact = `Contact%`,
         Z_Contact = `Z-Contact%`,
         O_Contact = `O-Contact%`)

rf_test_data_G <- rf_test_data_G %>%
  rename(O_Swing = `O-Swing%`,
         Contact = `Contact%`,
         Z_Contact = `Z-Contact%`,
         O_Contact = `O-Contact%`)

set.seed(123)
rf_model_new <- randomForest(`K%` ~ vFA + O_Swing + Contact + Z_Contact + O_Contact + ERA + G,
                             data = rf_train_data_G, 
                             ntree = 500, 
                             mtry = 3, 
                             importance = TRUE)

rf_predictions_new <- predict(rf_model_new, newdata = rf_test_data_G)
rf_rmse_new <- sqrt(mean((rf_predictions_new - rf_test_data_G$`K%`)^2))
rf_r2_new <- cor(rf_predictions_new, rf_test_data_G$`K%`)^2
rf_mae_new <- mae(rf_test_data_G$`K%`, rf_predictions_new)

# XGBoost
xgb_train_new <- as.matrix(train_data_G %>% select(vFA, `O-Swing%`, `Contact%`, `Z-Contact%`, `O-Contact%`, ERA, G))
xgb_test_new <- as.matrix(test_data_G %>% select(vFA, `O-Swing%`, `Contact%`, `Z-Contact%`, `O-Contact%`, ERA, G))

dtrain_new <- xgb.DMatrix(data = xgb_train_new, label = train_data_G$`K%`)
dtest_new <- xgb.DMatrix(data = xgb_test_new, label = test_data_G$`K%`)

xgb_model_new <- xgb.train(params = xgb_params,
                           data = dtrain_new,
                           nrounds = 500,
                           watchlist = list(train = dtrain_new, test = dtest_new),
                           early_stopping_rounds = 20,
                           verbose = 1)

xgb_predictions_new <- predict(xgb_model_new, newdata = dtest_new)
xgb_rmse_new <- sqrt(mean((xgb_predictions_new - test_data_G$`K%`)^2))
xgb_r2_new <- cor(xgb_predictions_new, test_data_G$`K%`)^2
xgb_mae_new <- mae(test_data_G$`K%`, xgb_predictions_new)


# Print results
cat("Updated Linear Regression RMSE:", lm_rmse_new, " R-squared:", lm_r2_new, "MAE", lm_mae_new, "\n")
cat("Previous Linear Regression RMSE:", rmse, " R-squared:", r2, "MAE", lm_mae, "\n")

cat("Updated Ridge Regression RMSE:", ridge_rmse_new, " R-squared:", ridge_r2_new, "MAE", ridge_mae_new, "\n")
cat("Previous Ridge Regression RMSE:", ridge_rmse, " R-squared:", ridge_r2, "MAE", ridge_mae, "\n")

cat("Updated Lasso Regression RMSE:", lasso_rmse_new, " R-squared:", lasso_r2_new, "MAE", lasso_mae_new, "\n")
cat("Previous Lasso Regression RMSE:", lasso_rmse, " R-squared:", lasso_r2, "MAE", lasso_mae, "\n")

cat("Updated Random Forest RMSE:", rf_rmse_new, " R-squared:", rf_r2_new, "MAE", rf_mae_new, "\n")
cat("Previous Random Forest RMSE:", rf_rmse, " R-squared:", rf_r2, "MAE", rf_mae, "\n")

cat("Updated XGBoost RMSE:", xgb_rmse_new, " R-squared:", xgb_r2_new, "MAE", xgb_mae_new, "\n")
cat("Previous XGBoost RMSE:", xgb_rmse, " R-squared:", xgb_r2, "MAE", xgb_mae, "\n")

# It seems that adding G as a predictor did not improve the models.
# I thought perhaps that G would improve since pitching in more games can lead to potentially getting more strikeouts.

################# Tuning Linear Regression Model ###################

# Looking at the summary of our original model
summary(lm_model)

# The Contact% variable seems to not be significant, so let's remove that variable and see if the model improves.

# Select relevant variables for the model
model_data_2 <- pitcher_21_23 %>%
  select(`K%`, vFA, `O-Swing%`, `Z-Contact%`, `O-Contact%`, ERA)

# Split data into training (80%) and testing (20%) sets
set.seed(123)  # Ensures reproducibility
train_indices_2 <- createDataPartition(model_data_2$`K%`, p = 0.8, list = FALSE)
train_data_2 <- model_data_2[train_indices_2, ]
test_data_2  <- model_data_2[-train_indices_2, ]

# Fit a linear regression model
lm_model_2 <- lm(`K%` ~ vFA + `O-Swing%` + `Z-Contact%` + `O-Contact%` + ERA, data = train_data_2)

# Print model summary
summary(lm_model_2)

# Predict on test set
predictions_2 <- predict(lm_model_2, newdata = test_data_2)

# Evaluate model performance
lm_rmse_2 <- sqrt(mean((predictions_2 - test_data_2$`K%`)^2))  # Root Mean Squared Error
lm_r2_2 <- cor(predictions_2, test_data_2$`K%`)^2  # R-squared
lm_mae_2 <- mae(test_data_2$`K%`, predictions_2)

# Print results
cat("First LM Model RMSE:", rmse, "\n")
cat("Second LM Model RMSE:", lm_rmse_2, "\n")

cat("First LM Model R-squared:", r2, "\n")
cat("Second LM Model R-squared:", lm_r2_2, "\n")

cat("First LM Model MAE:", lm_mae, "\n")
cat("Second LM Model MAE:", lm_mae_2, "\n")

# After removing the Contact% variable, the model does not improve.
# It seems like the first linear regression model is still the best model we have for predicting K%.


################## Final Predictions on Season 2024 ####################

# We are going to use the ridge regression model since it performed the best.
# The ridge model we selected is the one that included the following variables:
# vFA, O-Swing%, Contact%, Z-Contact%, O-Contact%, ERA, and G

# Prepare test data (2024) for prediction
X_test_2024 <- model.matrix(`K%` ~ vFA + `O-Swing%` + `Contact%` + `Z-Contact%` + `O-Contact%` + ERA + G, data = pitcher_2024)[, -1]

# Predict 2024 K% using the pre-trained ridge model
predictions_2024 <- predict(ridge_model_new, s = ridge_model_new$lambda.min, newx = X_test_2024)

# Add predictions to the dataset
pitcher_2024$Predicted_K <- predictions_2024


# Load actual 2024 K% values
actual_2024_K <- pitcher_2024$`K%`

# Compute RMSE for 2024 predictions
rmse_2024 <- sqrt(mean((predictions_2024 - actual_2024_K)^2))

# Compute MAE for 2024 predictions
mae_2024 <- mean(abs(predictions_2024 - actual_2024_K))

# Compute R-squared for 2024 predictions
r2_2024 <- cor(predictions_2024, actual_2024_K)^2

# Print results
cat("2024 Prediction RMSE:", rmse_2024, "\n")
cat("2024 Prediction MAE:", mae_2024, "\n")
cat("2024 Prediction R-squared:", r2_2024, "\n")

# Compute residuals for each model
final_ridge_residuals <- actual_2024_K - predictions_2024  # Ridge model

# QQ Plot for Ridge Model
qq_plot_final_ridge <- qq_plot(final_ridge_residuals, "QQ Plot - Final Ridge Regression Model")
print(qq_plot_final_ridge)

# Combining the predicted K% with the actual K% and the other variables that are in dse_6620
final_result <- pitcher_2024 %>% 
  select(c(MLBAMID, PlayerId, Name, Team, Age, Season, TBF, `K%`, Predicted_K))


# Looking to see how many observations fall within 5% of the predictions
within_5_pct <- final_result %>% 
  select(c(`K%`, Predicted_K))

within_5_pct <- within_5_pct %>% 
  mutate(
    Absolute_Error = `K%` - Predicted_K
  )

# Count observations where Absolute_Error is within the range (-0.05, 0.05)
count_within_range <- t %>%
  filter(Absolute_Error > -0.05, Absolute_Error < 0.05) %>%
  nrow()

# Print the result
print(count_within_range)

# Finding the percentage of observations that fell with in 5%
count_within_range / 474










