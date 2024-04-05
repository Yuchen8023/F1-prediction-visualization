suppressPackageStartupMessages({
  library(knitr)
  library(kableExtra)
  library(tidyverse)  
})

#------Load the data--------
circuits <- read.csv("./Data/FinalProject/circuits.csv")#list of record of circuits where races are held
constructorResults <- read.csv("./Data/FinalProject/constructor_results.csv")#Race results of the constructor's championship
constructorStandings <- read.csv("./Data/FinalProject/constructor_standings.csv")#Final standings of the constructor's championship
constructors <- read.csv("./Data/FinalProject/constructors.csv")#List of Constructors in F1 with link to wikipedia page
df_drivers <- read.csv("./Data/FinalProject/drivers.csv")#Drivers in F1 with link to their wiki
driverStandings <- read.csv("./Data/FinalProject/driver_standings.csv")#Final standings of the driver's championship
lapTimes <- read.csv("./Data/FinalProject/lap_times.csv")#Lap times in F1
pitStops <- read.csv("./Data/FinalProject/pit_stops.csv")#Pit stops in F1
df_qualifying <- read.csv("./Data/FinalProject/qualifying.csv")#Qualifying in F1
df_races <- read.csv("./Data/FinalProject/races.csv")#list of Races in F1
df_results <- read.csv("./Data/FinalProject/results.csv")#Race results per driver and per constructor
status <- read.csv("./Data/FinalProject/status.csv")#Mapping of various statuses
seasons <- read.csv("./Data/FinalProject/seasons.csv")#seasons of F1 with wikipedia link
sprintResults <- read.csv("./Data/FinalProject/sprint_results.csv")#Results of F1 sprint races per race,driver,and constructor

#-----------------Machine Learning------------------------------------------------
# the idea is to build a machine learning model using qualifying results to predict 
# race results: whether the driver will be on top 5 or not. 
# to do so, we are taking the following dataframes: drivers, races, results, qualifying

suppressPackageStartupMessages({
  suppressWarnings({
    library(tidyverse)
    library(tidymodels)
    library(data.table)
    library(corrplot)
    library(kknn)
    library(tidyr)
    library(stringr)#column transformation
    library(vip)        # Feature importance
    library(probably)   # Thresholds evaluation
    library(scales)
    library(janitor)    # Data cleaning
    library(themis)# Extra recipes
    library(dplyr)
    library(ggplot2)
    library(ggmap)
    library(sf)
    library(glmnet)
    
  })
})


##----------data manulupation--------------


###----------Add year to df_results---------------
df_new <- df_races %>%
  select(raceId, date) %>%
  distinct(raceId, .keep_all = TRUE) %>%
  arrange(as.Date(date, format = "%Y-%m-%d"))

df_new$raceIdOrdered <- seq_along(df_new$raceId)

#Add Correct Order of Races to df_results
df_results <- merge(df_results, df_new[, c("raceId", "raceIdOrdered")], by = "raceId", all.x = TRUE)

# Sort DataFrame Based on Order of Races
df_results <- df_results %>%
  arrange(raceIdOrdered)

# Add Years to DataFrame Results
df_results <- df_results %>%
  left_join(df_races[, c("year", "raceId")], by = "raceId")

# Select and Rename Columns
df_results <- df_results %>%
  select(raceId, driverId, grid, racePosition = positionOrder, year, raceIdOrdered) %>%
  rename(startingPosition = grid)

# View the First 10 Rows
head(df_results, 10)

###---------Adding the year in which the driver has started racing---------
#for the purpose of creating additional variable of exparience (later in the code)

min_year <- df_results %>%
  group_by(driverId) %>%
  summarise(yearStarted = min(year)) %>%
  ungroup()

# Merge the minimum year back into df_results
df_results <- merge(df_results, min_year, by = "driverId", all.x = TRUE)

###---------Instepct if the number of races has changed over the years--------- 
# Calculate the number of races per year
races_per_years <- df_races %>%
  count(year) %>%
  rename(Total = n)

# Get list of race IDs per year
race_ids_per_years <- df_races %>%
  group_by(year) %>%
  summarise(RaceIds = list(raceId))

# Combine the two data frames
races_per_years <- merge(races_per_years, race_ids_per_years, by = "year") %>%
  rename(Year = year) %>%
  arrange(Year)

# Plotting the number of races per year
ggplot(races_per_years, aes(x = Year, y = Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Year", y = "Total number of races", title = "Number of races per year") +
  theme_minimal()

#the number of races has grown and the regulations also have changed (save this for shiny)

###---------------inspect if there is missing data in df_results----------------
# Count unique races per year in df_results
data_for_races <- df_results %>%
  group_by(year) %>%
  summarise(raceId_count = n_distinct(raceId)) %>%
  ungroup()

# Add expected race counts from races_per_years
data_for_races <- merge(data_for_races, races_per_years[, c('Year', 'Total')], 
                        by.x = 'year', by.y = 'Year', all.x = TRUE)

# Calculate the difference
data_for_races$diff <- data_for_races$Total - data_for_races$raceId_count

# Filter years where the difference is not zero
data_for_races <- data_for_races[data_for_races$diff != 0, ]
#it shows that there are missing data in year 2023

#In the dataset, the first year for which there is information about qualifying is 1994. 
#The closest to the current format of qualifying was first introduced in 1996. 
#It differs from the current system in limiting drivers to 12 laps only, 
#while currently there is no limits on the number of laps to be completed. 
#Huge changes and a lot af variation was introduced to the qualifying format 
#between 2003 and 2005. Starting with the year 2006, 
#a three-part system of qualifying was introduced.
#These days, drivers are limited by time in which they have to complete the fastest lap they can. 
#After each round of qualifying, 5 drivers with the worst lap times are eliminated, 
#so that only 10 advance to the final round (Q3). 
#For these reasons, we limit the data to years 2006 - 2022.

###---------add year to df_qualifying---------

# Add years to df_qualifying
years <- df_races[, c('year', 'raceId')]
df_qualifying <- merge(df_qualifying, years, by = 'raceId')

# Print the first year of qualifying information
first_quali_year <- min(df_qualifying$year)
print(paste("First quali info: ", first_quali_year))


###------------filter years 2006-2022------------------------------------
# Remove 2003 - 2005 from qualifying
df_qualifying <- df_qualifying %>%
  filter(year <= 2002 | year >= 2006)

# Remove any years but 2006 - 2022 from results
df_results <- df_results %>%
  filter(year >= 2006 & year <= 2022)


###-----------------remove missing values-----------------------
# Remove cases where the driver didn’t start a race
df_results <- df_results %>%
  filter(startingPosition != 0)

# Evaluate Quali info missing for a race
x <- unique(df_results$raceId)
y <- unique(df_qualifying$raceId)

missing_quali <- setdiff(x, y)
#(no missing values)

# Remove Races for which there is no quali info
df_results <- df_results %>%
  filter(raceId %in% y)

# Recheck for missing quali info
x_updated <- unique(df_results$raceId)
missing_quali_updated <- setdiff(x_updated, y)
print(missing_quali_updated)
#(no missing values)

###---------------------Merge df_qualifying and df_results into one DataFrame-------------------------------
df <- merge(df_results, df_qualifying, by = c("raceId", "driverId"), all.x = TRUE)

# Drop unnecessary columns
df <- df %>%
  select(-c(year.y, constructorId, qualifyId, number)) %>%
  rename(year = year.x, qualiResultPosition = position)

# Display the first 10 rows
head(df, 10)
#from this we can see not all races have q1,q2,q3 values. we can handle that by extracting meanPace and maxPace

##------------transform laptime data------------------------
###-------------------Handle Missing Values and Count Drivers without Lap Time in Q1:-------------------------
df <- df %>%
  mutate(across(c(q1, q2, q3), ~na_if(.x, "\\N")))

no_lap_time_q1 <- sum(is.na(df$q1))
cat("No lap time in q1 set: ", no_lap_time_q1, "\n")

df <- df %>%
  filter(!is.na(q1))

no_lap_time_q1_after <- sum(is.na(df$q1))
cat("No lap time in q1 set (after removing): ", no_lap_time_q1_after, "\n")

###-------------------------Handle Null Values for Advancing Rounds:---------------------
df <- replace_na(df, list(q2 = "0", q3 = "0"))
null_values_in_df <- any(is.na(df))
cat("Null values in dataframe: ", null_values_in_df, "\n")


###-----------------------Split Lap Time Strings into [min, sec, msec] and Convert to Milliseconds:---------------
get_time_lst <- function(df, col) {
  str_split_fixed(df[[col]], pattern = ":|\\.", n = 3)
}

convert_to_msec <- function(time_lst) {
  if (all(is.na(time_lst))) {
    return(NA)
  } else {
    mins <- ifelse(is.na(as.numeric(time_lst[1])), 0, as.numeric(time_lst[1]))
    secs <- ifelse(is.na(as.numeric(time_lst[2])), 0, as.numeric(time_lst[2]))
    msec <- ifelse(is.na(as.numeric(time_lst[3])), 0, as.numeric(time_lst[3]))
    return(mins * 60000 + secs * 1000 + msec)
  }
}


df$q1_lst <- get_time_lst(df, 'q1')
df$q2_lst <- get_time_lst(df, 'q2')
df$q3_lst <- get_time_lst(df, 'q3')

df$q1Msec <- apply(df$q1_lst, 1, convert_to_msec)
df$q2Msec <- apply(df$q2_lst, 1, convert_to_msec)
df$q3Msec <- apply(df$q3_lst, 1, convert_to_msec)

null_values_in_df_after <- any(is.na(df))
cat("Null values in dataframe (added q_Msec): ", null_values_in_df_after, "\n")


###------------add mean max pace----------------------------------
df$maxPace <- apply(df[, c('q1Msec', 'q2Msec', 'q3Msec')], 1, max)

# Custom function to calculate meanPace safely
safe_mean_pace <- function(q1, q2, q3) {
  valid_values <- c(q1, q2, q3)
  valid_values <- valid_values[valid_values != 0]  # Remove zeroes
  
  if (length(valid_values) == 0) {
    return(0)  # Return NA if all values are zero or NA
  } else {
    return(mean(valid_values))
  }
}

# Apply the safe_mean_pace function to each row
df$meanPace <- mapply(safe_mean_pace, df$q1Msec, df$q2Msec, df$q3Msec)

# Check for null values
null_values_in_df_mean_max <- any(is.na(df))
cat("Null values in dataframe (added meanPace and maxPace): ", null_values_in_df_mean_max, "\n")




###------------------drop unnecessary columns-------------------------

df <- df %>%
  select(-c(q1_lst, q2_lst, q3_lst, q1, q2, q3))

# Display the first 10 rows
head(df, 10)

##-----------------add driver's experience column-------------------------
df <- df %>%
  mutate(driverExpYears = year - yearStarted)
##----------------add grid penalty column--------------------------
df <- df %>%
  mutate(grid_penalty = qualiResultPosition != startingPosition)#return false/true 

#after consideration, change to 0 as false and 1 as true 
df <- df %>%
  mutate(grid_penalty = as.integer(qualiResultPosition != startingPosition))


##--------------Creation of prediction target------------------------
df <- df %>%
  mutate(is_top_5 = racePosition <= 5)
#we target a binary prediction on whether this driver would rank in top 5 of race or not based on their qualification result
#for prediction, we select the following features: driverId, driverExpYears, qualiResultPosition, meanPace, maxPace, grid_penalty

df$is_top_5 <- as.factor(df$is_top_5) #required by the training 

##------------------create the model dataframe-----------------------------------
df_model <- df %>%
  select(driverId, driverExpYears, qualiResultPosition, meanPace, maxPace, grid_penalty, is_top_5)

#check if there is missing data in any column 
missing_data <- df_model %>%
  summarise_all(~sum(is.na(.)))
#all checked 


#save the dataframe in CSV just in case 
write.csv(df_model, file = "./Data/FinalProject/df_model.csv", row.names = FALSE)

##-----------------inspect the %of top 5 drivers----------------------------
top_5 <- df_model %>%
  group_by(is_top_5) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))
#24% true and 76% false, this dataset is unbalanced, we should consider to downsample in order to balance it 

##-------------------train and test split--------------------
seed <- 123
set.seed(seed)
race_split <- initial_split(df_model, prop = 0.7, strata = is_top_5)
race_train <- training(race_split)
race_test <- testing(race_split)

##-------------------create 10 fold-----------------------
set.seed(seed)
race_folds <- vfold_cv(race_train, v = 10, strata = is_top_5)


##--------------------create recipe------------------------
#as there is only numerical variable in the dataset, we only need to downsample and normalize the data
race_rec <- recipe(is_top_5 ~ ., data = race_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%  # Convert all nominal variables to dummy variables
  step_normalize(all_numeric(), -all_outcomes())%>%
  step_downsample(is_top_5) #downsample the majority class



##------------------create metric------------------------------------
#metrics for evaluation of classification task 
race_metrics <- metric_set(accuracy, yardstick::sens, yardstick::spec, yardstick::precision, yardstick::recall, f_meas,roc_auc)

##---------------------------create model-------------------------------------
#we will try 3 models: decision tree, random forest, logistics regression 

###-----------------------Decision Tree-----------------------------------------
dectree_spec <- decision_tree(
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_mode('classification') %>%
  set_engine('rpart')

#Decision Tree Grid (defined the grid search citeria so that the running time was significantly shortened to in total 30min)
dectree_params <- parameters(
  tree_depth(c(3, 10)), # range for tree depth
  min_n(c(2, 20))       
)

# Create the tuning grid for Decision Tree
dectree_grid <- grid_latin_hypercube(
  dectree_params,
  size = 10
)

#workflow 
dectree_workflow <- workflow() %>%
  add_model(dectree_spec) %>%
  add_recipe(race_rec)

doParallel::registerDoParallel()

# Creation of a function to keep the coefficients of the folds
get_coefs <- function(x) {
  extract_fit_parsnip(x) %>% tidy()
}

# Control grid setup remains the same
grid_control <- control_grid(
  save_pred = TRUE,
  allow_par = TRUE,
  verbose = TRUE,
  extract = get_coefs
)

set.seed(123)
# Timing the model fitting process for the decision tree
system.time({
  dectree_results <- tune_grid(
    dectree_workflow,
    resamples = race_folds,
    metrics = race_metrics,
    grid = dectree_grid, 
    control = grid_control
  )
})

# Save the results
save(dectree_results, file = './DATA/DT_race_results.RData')

#show_notes(.Last.tune.result)
#not sure why it is returing "Error in UseMethod("tidy"): no applicable method for 'tidy' applied to an object of class "rpart""

###-----------------------Random Forest-----------------------------------------
rf_spec <- rand_forest(
  trees = tune(),
  mtry = tune(),
  min_n = tune()
) %>%
  set_mode('classification') %>%
  set_engine('ranger',importance = 'impurity')

rf_workflow <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(race_rec)

rf_grid <- grid_latin_hypercube(
  trees(c(50, 100)),
  mtry(c(2, round(sqrt(ncol(race_train))))),
  min_n(c(2, 20)),
  size = 10
)

set.seed(123)
system.time({
  rf_results <- tune_grid(
    rf_workflow,
    resamples = race_folds,
    metrics = race_metrics,
    grid = rf_grid, 
    control = grid_control
  )
})

save(rf_results, file = './DATA/rf_race_results.RData')


###-----------------------xgboost-----------------------------------------

xgboost_spec <- boost_tree(
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune()
) %>%
  set_mode('classification') %>%
  set_engine('xgboost')

# XGB Grid
xgboost_grid <- grid_latin_hypercube(
  trees(c(50, 80)),
  tree_depth(c(3, 5)),
  min_n(c(5, 10)),
  learn_rate(c(0.01, 0.3)),
  size = 10
)

xgboost_workflow <- workflow() %>%
  add_model(xgboost_spec) %>%
  add_recipe(race_rec)


set.seed(123)
system.time({
  xgboost_results <- tune_grid(
    xgboost_workflow,
    resamples = race_folds,
    metrics = race_metrics,
    grid = xgboost_grid, 
    control = grid_control
  )
})

save(xgboost_results, file = './DATA/xgboost_race_results.RData')


##-----------------------Model Evaluation---------------------------------------
#as it is equally bad to equally bad to incorrectly predict a top 5 finisher 
#as it is to miss a top 5 finisher, then Accuracy or F1 Score are the best metrics 
#to evaluate the model

# decision tree
best_dt <- select_best(dectree_results, "accuracy")
avg_dectree_metrics <- dectree_results %>%
  collect_metrics() %>%
  group_by(.metric) %>%
  summarize(avg_mean_DT = mean(mean), .groups = 'drop')


# random forest
best_rf <- select_best(rf_results, "accuracy")
avg_rf_metrics <- rf_results %>%
  collect_metrics() %>%
  group_by(.metric) %>%
  summarize(avg_mean_RF = mean(mean), .groups = 'drop')

# XGBoost
best_xgb <- select_best(xgboost_results, "accuracy")
avg_xgboost_metrics <- xgboost_results %>%
  collect_metrics() %>%
  group_by(.metric) %>%
  summarize(avg_mean_XGB = mean(mean), .groups = 'drop')

# Printing the average metrics for each model
print(avg_dectree_metrics)
print(avg_rf_metrics)
print(avg_xgboost_metrics)

#decision tree metrics
# A tibble: 7 × 2
#.metric   avg_mean_DT
#<chr>           <dbl>
#  1 accuracy        0.833
#2 f_meas          0.883
#3 precision       0.943
#4 recall          0.832
#5 roc_auc         0.835
#6 sens            0.832
#7 spec            0.838

#Random Forest Metrics
# A tibble: 7 × 2
#.metric   avg_mean_RF
#<chr>           <dbl>
#  1 accuracy        0.829
#2 f_meas          0.880
#3 precision       0.944
#4 recall          0.824
#5 roc_auc         0.900
#6 sens            0.824
#7 spec            0.844

#xgboost metrics
# A tibble: 7 × 2
#.metric   avg_mean_XGB
#<chr>            <dbl>
#  1 accuracy         0.780
#2 f_meas           0.843
#3 precision        0.923
#4 recall           0.777
#5 roc_auc          0.855
#6 sens             0.777
#7 spec             0.790

#comparing the results, although decision tree has slightly higher accuracy and F1 score, random forest seems to be more balanced 
#in terms of roc_auc.

#however, as our target is accuracy and F1, we will proceed with decision tree model. 

##--------------------Finalize Workflow--------------------------

# Finalize the Decision Tree model
#step1: extract the workflow with best hyperparameters from random forest
best_dt_workflow <- finalize_workflow(dectree_workflow, best_dt)


#Step 2: With this best model object we can finalize the workflow (using the best hyperparameters)
# and fit over the split with last_fit()
final_fit <- best_dt_workflow %>%
  last_fit(split = race_split,
           metrics = race_metrics)

#step 3: see the metrics
final_fit_metrics<- collect_metrics(final_fit)
final_fit_metrics

#save the workflow 
trained_workflow <- final_fit$.workflow[[1]]
save(trained_workflow, file = './DATA/trained_workflow.RData')

##-------------------check for overfitting------------------------
#step 4: check for overfitting with the test data with accuracy 
# Average accuracy across all resamples for cross-validation
cross_validation_acc <-dectree_results %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  summarize(avg_accuracy = mean(mean)) 

# Print the accuracy for the final test set 
test_acc <- final_fit %>%
  collect_metrics(summary = "testing") %>%
  filter(.metric == "accuracy") %>%
  summarize(test_accuracy = mean(.estimate))

print(cross_validation_acc)
print(test_acc)

# as test accuracy is 0.854 and average accuracy is 0.833, we can say that the model is not overfitting

##----------------------Predictions-------------------------------------

#is in shiny 


#----------------------Data Visaulization-------------------------------------------

##----------------------1. extract a list of current drivers--------------------------
library(dplyr)

# Joining the tables
df_joined <- df_results %>%
  left_join(df_drivers %>% select(driverId, forename, surname, dob, nationality, url), by = "driverId")

#combine surname and forename 
df_joined$driverName <- paste(df_joined$forename, df_joined$surname, sep = " ")

#drop unnecessary columns (forename and surname)
df_joined <- df_joined %>%
  select(-c(forename, surname))
