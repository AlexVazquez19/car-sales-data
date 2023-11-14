## Loal Libraries and Data ----

# load libraries
library(ggplot2)
library(GGally)
library(tidyr)
library(dplyr)
library(fixest)

# clear environment
rm(list = ls())

# set working directory
setwd("/Users/alejandrovazquez/Desktop/econ121/car-sales-data")

# load data
df <- read.csv("Ad_table.csv")

## Clean and Transform ----

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

# view a summary of the data to see if any other adjustments are needed
summary(df)
  # There may be some mis-entered data in the month column as evidenced by
  # the max value being 33. The number of null values shouldn't be an issue
  # considering the size of the dataset.

# Lets take a look at the month outlier
month <- df %>% arrange(desc(Adv_month))
head(month, 10)
  # It appears that there are 3 observations where the month is above 12. I will
  # remove them from the dataset since it is just 3 observations and won't have
  # a big impact on the analysis.

# Remove month outliers
df <- df %>% filter(Adv_month <= 12)

## Exploratory Analysis ----

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

# create a bar chart to show the engine sizes in our dataset
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
  # We have 1,739 outliers. Lets remove them since it is only a small portion
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

# create bar plot for months of sale
ggplot(df, aes(x = as.factor(Adv_month))) +
  geom_bar(fill = "thistle2", color = "black") +
  scale_x_discrete(labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) +
  xlab("Month") +
  ylab("Frequency") +
  ggtitle("Distribution of Data by Month")
  # For some reason there is a lack of sales during the last four months of the
  # year.

# Lets calculate what % of observations were dropped from my original dataset
original_count <- nrow(df)
filtered_count <- nrow(df_fil)
percentage_filtered_out <- ((original_count - filtered_count) / original_count) * 100
paste("Percentage of observations filtered out: ", round(percentage_filtered_out, 2), "%", sep = "")
  # We only filtered out 2.13% of our original data
