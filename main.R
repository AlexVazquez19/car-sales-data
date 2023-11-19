## Load Libraries and Data ----

# load libraries
library(ggplot2)
library(GGally)
library(tidyr)
library(dplyr)
library(fixest)
library(car)
library(caret)

# clear environment
rm(list = ls())

# set working directory
setwd("/Users/alejandrovazquez/Desktop/econ121/car-sales-data")

# load data
df <- read.csv("Ad_table.csv")

## Cleaning and Transformation ----

# verify data types are what they should be
sapply(df, class)

# change 'Runned_Miles' to integer type
df <- df %>% mutate(Runned_Miles = na_if(Runned_Miles, ""))
df$Runned_Miles <- as.integer(df$Runned_Miles)

# change 'Price' to integer type
df <- df %>% mutate(Price = na_if(Price, "Uknown"))
df$Price <- as.integer(df$Price)

# change blank values to NA
df <- df %>% mutate(Bodytype = na_if(Bodytype, "")) # Bodytype
df <- df %>% mutate(Color = na_if(Color, "")) # Color
df <- df %>% mutate(Engin_size = na_if(Engin_size, "")) # Engin_size
df <- df %>% mutate(Gearbox = na_if(Gearbox, "")) # Gearbox
df <- df %>% mutate(Fuel_type = na_if(Fuel_type, "")) # Fuel_type

# Remove 'L' from the end of the Engin_size values and convert to numeric
df$Engin_size <- gsub("L", "", df$Engin_size)
df$Engin_size <- as.numeric(df$Engin_size)

# view a summary of the data to see if any other adjustments are needed
summary(df)
  # There may be some mis-entered data in the month column as evidenced by
  # the max value being 33. The number of null values shouldn't be an issue
  # considering the size of the dataset.

# Lets take a look at the month outlier
month <- df %>% arrange(desc(Adv_month))
head(month['Adv_month'], 10)
  # It appears that there are 3 observations where the month is above 12. I will
  # remove them from the dataset since it is just 3 observations and won't have
  # a big impact on the analysis.

# Remove month outliers
df <- df %>% filter(Adv_month <= 12)

## Exploratory Analysis: Maker ----

# view the top 10 manufacturers represented in this dataset
make_counts <- df %>%
  group_by(Maker) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

top_10 <- head(make_counts, 10) # top 10
ggplot(top_10, aes(x = reorder(Maker, -count), y = count)) +
  geom_bar(stat = "identity", fill = "deepskyblue4") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  xlab("Maker") +
  ylab("Count") +
  ggtitle("Top 10 Car Makers by Count")

summary(make_counts$count)
  # It seems that 25% of the makes in our dataset have less than 3 vehicles. 
  # Because of this we may need to remove these makes before creating dummies for
  # 'Maker' to prevent overfitting and reduce model complexity.

# Lets set a threshold at 50 vehicles.
df_fil <- df %>% filter(Maker %in% make_counts$Maker[make_counts$count >= 50])
  # I believe this is reasonable because  removes makes infrequently represented
  # in the data while maintaining a majority of the data. Plus, this threshold 
  # ensures most high-end low-volume manufacturers like McLaren and Aston Martin 
  # remain in the dataset.

## Exploratory Analysis: Body Type ----

# create a bar chart to show the body types in our dataset
bodytype_counts <- df %>% 
  group_by(Bodytype) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(bodytype_counts, aes(x = reorder(Bodytype, -count), y = count)) +
  geom_bar(stat = "identity", fill = "firebrick4") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Body Type") +
  ylab("Count") +
  ggtitle("Count of Vehicles by Body Type")
  # We will need to remove some of the body types with low counts if we
  # wish to create a dummy variable for 'Bodytype'.

summary(bodytype_counts$count)

# We will set a threshold at 700.
df_fil <- df_fil %>% filter(Bodytype %in% bodytype_counts$Bodytype[bodytype_counts$count >= 700])
  # This removes all the specialty body types while maintaining all the standard
  # body types, which includes the majority of the data.

## Exploratory Analysis: Fuel Type ----

# create a bar chart to show the fuel types in our dataset
fueltype_counts <- df %>% 
  group_by(Fuel_type) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(fueltype_counts, aes(x = reorder(Fuel_type, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Fuel Type") +
  ylab("Count") +
  ggtitle("Count of Vehicles by Fuel Type")
  # It seems the vast majority of vehicles are Diesel or Petrol (~ 97%), with a 
  # small proportion being hybrid (~ 1.7%) or electric (~ 0.5%). This is also
  # a concern if we wish to create dummy variables for fuel type.

summary(fueltype_counts$count)

# We will set a threshold at 300.
df_fil <- df_fil %>% filter(Fuel_type %in% fueltype_counts$Fuel_type[fueltype_counts$count >= 300])
  # This removes the fuel types that are not common while maintaining the most
  # common fuel types which represent the majority of the data.

## Exploratory Analysis: Color ----

# create a bar chart to show the colors in our dataset
color_counts <- df %>% 
  group_by(Color) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(color_counts, aes(x = reorder(Color, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Color") +
  ylab("Count") +
  ggtitle("Count of Vehicles by Color")
  # Most of the vehicles in our dataset are black, silver, blue, grey, white,
  # and red. If we wish to create dummy variables for color we may have to
  # remove the other colors, but there is a large amount of NAs which may
  # prevent us from doing so.

summary(color_counts$count)
# For now I won't filter out any colors because I do not think color has
# that large of an impact on sale price. Although I may revisit this later.

## Exploratory Analysis: Engine Size ----

# plot a box plot for engine size to see outliers
boxplot(df_fil$Engin_size, main = "Boxplot of Engine Size", ylab = "Engine Size")
  # Looks like there are some significant outliers, one vehicle even has
  # 3000 Liters! Lets remove them since it is only a few.

# identify outliers by engine size
IQR_values <- IQR(df$Engin_size, na.rm = TRUE)
Q1 <- quantile(df$Engin_size, 0.25, na.rm = TRUE)
Q3 <- quantile(df$Engin_size, 0.75, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_values # 0.5
upper_bound <- Q3 + 1.5 * IQR_values # 2.9

outliers <- subset(df, Engin_size < lower_bound | Engin_size > upper_bound)
nrow(outliers)
  # Because there are 37,314 outliers, I think we should only remove the 
  # extreme outliers so as not to lost too much data.

# Remove outliers with extremely large engine size (5 observations)
df_fil <- subset(df_fil, Engin_size <= 8)

# create a histogram to show the engine sizes in our dataset
ggplot(df_fil, aes(x = Engin_size)) +
  geom_histogram(binwidth = 0.5, fill = "snow3", color = "black") +
  xlab("Engine Size (L)") +
  ylab("Frequency") +
  ggtitle("Distribution of Vehicle Engine Sizes")

# Log transformation of Engine Size to mitigate effect of outliers
df_fil$log_Engin_size <- log(df_fil$Engin_size + 1)

## Exploratory Analysis: Gearbox ----

# create a bar chart to show the transmissions in our dataset
gearbox_counts <- df %>% 
  group_by(Gearbox) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(gearbox_counts, aes(x = reorder(Gearbox, -count), y = count)) +
  geom_bar(stat = "identity", fill = "sienna2") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  xlab("Transmission") +
  ylab("Count") +
  ggtitle("Count of Vehicles by Transmission")
  # If we create a dummy variable for Gearbox we can just remove semi-automatic
  # since the vast majority of the data is either manual or automatic.

# We will set a threshold at 200.
df_fil <- df_fil %>% filter(Gearbox %in% gearbox_counts$Gearbox[gearbox_counts$count >= 200])
  # This removes semi-automatic vehicles which represent a tiny fraction of
  # the data.

## Exploratory Analysis: Model Years ----

# view the distribution of model years for vehicles sold
year_counts <- df %>% 
  group_by(Reg_year) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))
summary(year_counts$Reg_year)
tail(year_counts, 10)
  # Since there are only 134 vehicles made before 2000, we can filter our data
  # to only include post-2000 vehicles to reduce model complexity.

# remove vehicles older than 2000
df_fil <- df_fil %>% filter(Reg_year >= 2000)

# now we can plot a histogram of the years
ggplot(df_fil, aes(x = Reg_year)) +
  geom_histogram(binwidth = 1, fill = "lightgoldenrod1", color = "black") +
  xlab("Registration/Model Year") +
  ylab("Frequency") +
  ggtitle("Distribution of Vehicle Model Years")
  # Left skewed. It seems the majority of our data is relatively newer
  # vehicles manufactured within the past decade.

## Exploratory Analysis: Ad Years ----

# plot a bar chart of the years the ads were posted
adyear_counts <- df %>% 
  group_by(Adv_year) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(adyear_counts, aes(x = reorder(Adv_year, -count), y = count)) +
  geom_bar(stat = "identity", fill = "olivedrab") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  xlab("Year Ad was Posted") +
  ylab("Count") +
  ggtitle("Count of Vehicles by Year Ad was Posted")
  # Most of the vehicles in our data were listed for sale in 2018, which means
  # our model will be most accurate for predicting sale price in 2018 value.

## Exploratory Analysis: Miles ----

# plot a histogram for miles
ggplot(df, aes(x = Runned_Miles)) +
  geom_histogram(binwidth = 10000, fill = "dodgerblue2", color = "black") +
  xlab("Miles") +
  ylab("Frequency") +
  ggtitle("Distribution of Vehicle Mileage at Time of Sale")
  # We have some outliers because the histogram is extremely right skewed. Let's
  # get rid of those observations.

# plot a box plot for miles to see outliers more clearly
boxplot(df$Runned_Miles, main = "Boxplot of Runned Miles", ylab = "Miles")
  # It seems one of the vehicles somehow sold with over 6,000,000 miles. We
  # should remove this vehicle along with the other outliers.

# identify outliers by miles
IQR_values <- IQR(df$Runned_Miles, na.rm = TRUE)
Q1 <- quantile(df$Runned_Miles, 0.25, na.rm = TRUE)
Q3 <- quantile(df$Runned_Miles, 0.75, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_values # -77,120
upper_bound <- Q3 + 1.5 * IQR_values # 166,272

# Because a vehicle cannot have negative miles, we can disregard the lower bound
outliers <- subset(df, Runned_Miles > upper_bound)
nrow(outliers)
  # We have 1,749 outliers. Lets remove them since it is only a small portion
  # of the data and it will give us a clearer effect of miles on sale price
  # in our model.

# Remove outliers with high mileage
df_fil <- subset(df_fil, Runned_Miles <= upper_bound)

# re-plot a histogram for miles
ggplot(df_fil, aes(x = Runned_Miles)) +
  geom_histogram(binwidth = 10000, fill = "dodgerblue2", color = "black") +
  xlab("Miles") +
  ylab("Frequency") +
  ggtitle("Distribution of Vehicle Mileage at Time of Sale")
  # Now that we can properly interpret this histogram, we can see it is right
  # skewed, which means most of the vehicles in our data have lower mileage
  # (less than 50,000).

## Exploratory Analysis: Month of Sale ----

# create bar plot for months of sale
ggplot(df, aes(x = as.factor(Adv_month))) +
  geom_bar(fill = "thistle2", color = "black") +
  scale_x_discrete(labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  xlab("Month") +
  ylab("Frequency") +
  ggtitle("Distribution of Data by Month")
  # For some reason there is a lack of sales during the last four months of the
  # year.

## Exploratory Analysis: Price ----

# check how many rows have no selling price
sum(is.na(df_fil$Price))

# remove rows without a selling price since it is a tiny portion of the data
df_fil <- df_fil[!is.na(df_fil$Price), ]

# plot a box plot for sale price to see outliers
boxplot(df_fil$Price, main = "Boxplot of Sale Price", ylab = "Sale Price")
  # There are some extreme outliers that we will need to remove.

# identify outliers by price
IQR_values <- IQR(df$Price, na.rm = TRUE)
Q1 <- quantile(df$Price, 0.25, na.rm = TRUE)
Q3 <- quantile(df$Price, 0.75, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_values # -13,250
upper_bound <- Q3 + 1.5 * IQR_values # 35,390

# Because a vehicle cannot sell for a negative price, we can disregard the lower bound
outliers <- subset(df, Price > upper_bound)
nrow(outliers) 
  # Since there are 19,033 outliers, we will need to be careful not to remove
  # all of them. I will set a threshold at 500,000 to remove the extreme
  # outliers while retaining data on some high-value vehicles.

# Remove outliers with extremely high price
df_fil <- subset(df_fil, Price <= 500000)

# Log transformation of Price to mitigate effect of outliers
df_fil$log_Price <- log(df_fil$Price)

# Histogram for Price data
ggplot(df_fil, aes(x = Price)) +
  geom_histogram(bins = 50, fill = "forestgreen", color = "black") +
  ggtitle("Log Transformed Price Distribution") +
  xlab("Log(Price)") +
  ylab("Frequency")

# Histogram for the log-transformed Price data
ggplot(df_fil, aes(x = log_Price)) +
  geom_histogram(bins = 50, fill = "lightgreen", color = "black") +
  ggtitle("Log Transformed Price Distribution") +
  xlab("Log(Price)") +
  ylab("Frequency")
  # The distribution is much closer to normal
  
## Exploratory Analysis Conclusion ----

# Lets calculate what % of observations were dropped from my original dataset
original_count <- nrow(df)
filtered_count <- nrow(df_fil)
percentage_filtered_out <- ((original_count - filtered_count) / original_count) * 100
paste("Percentage of observations filtered out: ", round(percentage_filtered_out, 2), "%", sep = "")

## Scatter plots ----

# plot miles vs log price
plot(df_fil$Runned_Miles, df_fil$log_Price, main = "Runned Miles vs Log(Price)")

# plot engine size vs log price
plot(df_fil$Engin_size, df_fil$log_Price, main = "Engine Size vs Log(Price)")

# plot model year vs log price
plot(df_fil$Reg_year, df_fil$log_Price, main = "Model Year vs Log(Price)")

## Split Data into Training, Validation, Test ----

set.seed(792002) # Set a seed for reproducibility

# convert categorical variables to factors
df_fil$Maker <- as.factor(df_fil$Maker)
df_fil$Fuel_type <- as.factor(df_fil$Fuel_type)
df_fil$Bodytype <- as.factor(df_fil$Bodytype)
df_fil$Gearbox <- as.factor(df_fil$Gearbox)
df_fil$Color <- as.factor(df_fil$Color)

# view baselines for categorical variables
levels(df_fil$Maker)[1] # Make
levels(df_fil$Bodytype)[1] # Body Type
levels(df_fil$Gearbox)[1] # Gearbox
levels(df_fil$Fuel_type)[1] # Fuel Type
levels(df_fil$Color)[1] # Color

# change the baselines for easier interpretation of coefficients
df_fil$Maker <- relevel(df_fil$Maker, ref = "Toyota")
df_fil$Bodytype <- relevel(df_fil$Bodytype, ref = "Saloon")
df_fil$Fuel_type <- relevel(df_fil$Fuel_type, ref = "Petrol")
df_fil$Color <- relevel(df_fil$Color, ref = "White")

# Randomize the dataset by sampling rows
df_fil <- df_fil[sample(nrow(df_fil)), ]

# Split dataset into training (60%) and the test + validation (40%)
trainingIndex <- createDataPartition(df_fil$Price, p = 0.60, list = FALSE)
df_training <- df_fil[trainingIndex, ]
tempSet <- df_fil[-trainingIndex, ]

# Split the remaining 40% into validation (50% of 40%) and test sets (50% of 40%)
validationIndex <- createDataPartition(tempSet$Price, p = 0.5, list = FALSE)
df_validation <- tempSet[validationIndex, ]
df_test <- tempSet[-validationIndex, ]

## Linear Model 1 ----

# Model 1 - contains the most possible relevant dependent variables
model_1 <- lm(log_Price ~ Runned_Miles + Reg_year + Engin_size + Bodytype + Maker 
            + Fuel_type + Gearbox + Color, 
            data = df_training)

# View the model summary
summary(model_1)
  # Note: since the dependent variable is log transformed, we must exponentiate
  # the coefficient (e^coeff) to get the multiplicative factor for every 
  # one-unit increase in the independent variable. 
  # For example, the coefficient on MakerBMW is 1.393e-01 --> exp(1.393e-01)
  # is 1.149469. This means for every 1 unit increase in the MakerBMW variable,
  # the sale price increases by a factor of 1.149, or approximately 15%.

# create fitted vs residuals plot
residuals <- residuals(model_1)
fitted_values <- fitted(model_1)
plot(fitted_values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Fitted vs Residuals")
abline(h = 0, col = "red", lty = "dotted", lwd = 1.5) # add a horizontal line at 0
lowess_fit <- lowess(fitted_values, residuals)
lines(lowess_fit, col = "royalblue", lwd = 1.5) # add lowess line

# create histogram of residuals
hist(model_1$residuals, main = "Residual Histogram")

# create normal Q-Q plot
plot_data <- data.frame(Residuals = residuals, Fitted = fitted_values)
ggplot(plot_data, aes(sample = Residuals)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("Normal Q-Q Plot") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

## Linear Model 2 ----

# Model 2 - simpler model with only make, mileage, and year
model_2 <- lm(log_Price ~ Runned_Miles + Reg_year + Maker, 
            data = df_training)

# View the model summary
summary(model_2)

# create fitted vs residuals plot
residuals <- residuals(model_2)
fitted_values <- fitted(model_2)
plot(fitted_values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Fitted vs Residuals")
abline(h = 0, col = "red", lty = "dotted", lwd = 1.5) # add a horizontal line at 0
lowess_fit <- lowess(fitted_values, residuals)
lines(lowess_fit, col = "royalblue", lwd = 1.5) # add lowess line

# create histogram of residuals
hist(model_2$residuals, main = "Residual Histogram")

# create normal Q-Q plot
plot_data <- data.frame(Residuals = residuals, Fitted = fitted_values)
ggplot(plot_data, aes(sample = Residuals)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("Normal Q-Q Plot") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

## Linear Model 3 ----

# Model 3 - Model 1 but without engine size or color because I believe they have a negligible effect on price
model_3 <- lm(log_Price ~ Runned_Miles + Reg_year + Bodytype + Maker + Fuel_type 
            + Gearbox, 
            data = df_training)

# View the model summary
summary(model_3)

# create fitted vs residuals plot
residuals <- residuals(model_3)
fitted_values <- fitted(model_3)
plot(fitted_values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Fitted vs Residuals")
abline(h = 0, col = "red", lty = "dotted", lwd = 1.5) # add a horizontal line at 0
lowess_fit <- lowess(fitted_values, residuals)
lines(lowess_fit, col = "royalblue", lwd = 1.5) # add lowess line

# create histogram of residuals
hist(model_3$residuals, main = "Residual Histogram")

# create normal Q-Q plot
plot_data <- data.frame(Residuals = residuals, Fitted = fitted_values)
ggplot(plot_data, aes(sample = Residuals)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("Normal Q-Q Plot") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

## Make Predictions and Evaluate Model ----

# predict models on validation set
predictions_1 <- predict(model_1, newdata = df_validation)
predictions_2 <- predict(model_2, newdata = df_validation)
predictions_3 <- predict(model_3, newdata = df_validation)

# get the actual values that the predictions correspond to
actual_values <- df_validation$log_Price

# plot actual vs predicted log_Price for Model 1
plot(exp(actual_values), exp(predictions_1), main = "Actual vs Predicted [Model 1]",
     xlab = "Actual Log(Price)", ylab = "Predicted Log(Price)")
abline(a = 0, b = 1, col='red')

# Calculate performance metrics for Model 1
mae1 <- mean(abs(exp(predictions_1) - exp(actual_values)), na.rm = TRUE)
rmse1 <- sqrt(mean((exp(predictions_1) - exp(actual_values))^2, na.rm = TRUE))
cat(paste("Model 1 - Mean Absolute Error (MAE):", format(mae1, nsmall = 4)), "\n")
cat(paste("Model 1 - Root Mean Squared Error (RMSE):", format(rmse1, nsmall = 4)), "\n")

# plot actual vs predicted log_Price for Model 2
plot(exp(actual_values), exp(predictions_2), main = "Actual vs Predicted [Model 2]",
     xlab = "Actual Log(Price)", ylab = "Predicted Log(Price)")
abline(a = 0, b = 1, col='red')

# Calculate performance metrics for Model 2
mae2 <- mean(abs(exp(predictions_2) - exp(actual_values)), na.rm = TRUE)
rmse2 <- sqrt(mean((exp(predictions_2) - exp(actual_values))^2, na.rm = TRUE))
cat(paste("Model 2 - Mean Absolute Error (MAE):", format(mae2, nsmall = 4)), "\n")
cat(paste("Model 2 - Root Mean Squared Error (RMSE):", format(rmse2, nsmall = 4)), "\n")

# plot actual vs predicted log_Price for Model 3
plot(exp(actual_values), exp(predictions_3), main = "Actual vs Predicted [Model 3]",
     xlab = "Actual Log(Price)", ylab = "Predicted Log(Price)")
abline(a = 0, b = 1, col='red')

# Calculate performance metrics for Model 3
mae3 <- mean(abs(exp(predictions_3) - exp(actual_values)), na.rm = TRUE)
rmse3 <- sqrt(mean((exp(predictions_3) - exp(actual_values))^2, na.rm = TRUE))
cat(paste("Model 3 - Mean Absolute Error (MAE):", format(mae3, nsmall = 4)), "\n")
cat(paste("Model 3 - Root Mean Squared Error (RMSE):", format(rmse3, nsmall = 4)), "\n")

# It seems that model 1 is the best model.

## Test the Best Model ----

# make predictions on test set
predictions_test <- predict(model_1, newdata = df_test)

# get actual log_Price values from test set
actual_values_test <- df_test$log_Price

# Calculate performance metrics
mae_test <- mean(abs(exp(predictions_test) - exp(actual_values_test)), na.rm = TRUE)
rmse_test <- sqrt(mean((exp(predictions_test) - exp(actual_values_test))^2, na.rm = TRUE))
cat(paste("Test - Mean Absolute Error (MAE):", format(mae_test, nsmall = 4)), "\n")
cat(paste("Test - Root Mean Squared Error (RMSE):", format(rmse_test, nsmall = 4)), "\n")

# plot actual vs predicted log_Price for test data
plot(exp(actual_values_test), exp(predictions_test), main = "Actual vs Predicted [Test]",
     xlab = "Actual Log(Price)", ylab = "Predicted Log(Price)")
abline(a = 0, b = 1, col='red')
