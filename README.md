# Modelling Used Car Prices in R

In this project I created a model using multiple linear regression to estimate the sale price of used cars in the UK market based on a number of different features, including: make, mileage, model year, engine size, color, fuel type, body type, and transmission. My goal with this project was to gain experience using R for exploratory analysis on large datasets and learn more about using linear regression on real data. 

## Hypothesis
The sale price of used cars is significantly influenced by specific features such as make, mileage, model year, engine size, fuel type, body type, and gearbox, with each of these factors contributing uniquely to the determination of the car’s value.

Breakdown of the Hypothesis:
* **Make** Different car makes have distinct impacts on sale prices, reflecting brand value and reputation. For example, an Aston Martin may hold a higher value over a longer time period because of brand prestige.
* **Mileage**: Higher mileage negatively impacts the sale price of used cars.
* **Model Year**: Newer model years are associated with higher sale prices.
* **Engine Size**: Larger engine sizes correlate with higher sale prices, reflecting the vehicle's performance characteristics.
* **Fuel Type**: The type of fuel a car uses influences its sale price, potentially reflecting operational costs and environmental considerations.
* **Body Type**: The body style of a vehicle may indicate characteristics that influence the price. For example, coupes are typically sports cars which carry a higher price. 
* **Gearbox**: The type of transmission (manual or automatic) affects the car's sale price, indicating preferences or perceived value in the market.

## Data
The dataset I used for this project is titled ["DVM-CAR: A large-scale automotive dataset for visual marketing research and applications"](https://deepvisualmarketing.github.io/). It is a large-scale automotive dataset with six tables containing over 20 years of automotive sales data collected in the UK market. For this project, I used the *Ad Table* which contains information on over 260,000 used car advertisements collected over 10 years across multiple online sources in the United Kingdom. 

The data contains the following columns (star (*) indicates data I used in my model):
* **Maker***: make (manufacturer) of the vehicle
* **Genmodel**: model of the vehicle
* **Genmodel_ID**: ID for linking to other tables
* **Adv_ID**: ID for linking to other tables
* **Adv_year**: year the advertisement was posted
* **Adv_month**: month the advertisement was posted
* **Color***: color of the vehicle
* **Reg_year***: year the vehicle was first able to be registered (essentially the model year)
* **Bodytype***: body type (sedan, truck, etc.)
* **Runned_Miles***: number of miles
* **Engin_size***: size of the engine in liters
* **Gearbox***: transmission (automatic, manual, etc.)
* **Fuel_type**:* type of fuel (petrol, hybrid, etc.)
* **Price***: sale price 
* **Seat_num**: number of seats
* **Door_num**: number of doors

## How to Run the Script
To run my script on your own computer, all you need to do is fork this repository and then set your working directory in line 16. The *Ad Table* needed for the script is included in this repository, but you can download all the data from [here](https://deepvisualmarketing.github.io/). 
