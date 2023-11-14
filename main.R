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

# view the vehicle top and bottom 10 manufacturers represented in this dataset
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
top_10

bottom_10 <- tail(make_counts, 10) # bottom 10
bottom_10
  # It seems there are quite a few manufacturers with only 1 vehicle in this
  # dataset. We may need to remove these before making dummies for 'Maker' to
  # prevent overfitting and reduce model complexity.

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
  # We may also have to remove some of the body types with low counts if we
  # wish to create a dummy variable for 'Bodytype'.

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

# view the distribution of model years for vehicles sold
year_counts <- df %>% 
  group_by(Reg_year) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))
year_counts
tail(year_counts, 10)
  # Since there are only 134 vehicles made before 2000, we can filter our data
  # to only include post-2000 vehicles to reduce model complexity.

# remove vehicles older than 2000
df <- df %>% filter(Reg_year >= 2000)

# now we can plot a histogram of the years
ggplot(df, aes(x = Reg_year)) +
  geom_histogram(binwidth = 1, fill = "lightgoldenrod1", color = "black") +
  xlab("Registration/Model Year") +
  ylab("Frequency") +
  ggtitle("Distribution of Vehicle Model Years")
  # It seems the majority of our data is relatively newer vehicles manufactured
  # within the past decade.


