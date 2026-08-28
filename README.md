# Bike Sharing Analysis with R and Random Forest

A statistical analysis and machine learning project developed in **R** to explore bike-sharing usage patterns and identify the factors that are most important in predicting rental demand.

The project was originally developed as part of the **Statistics module of my Master's course** and has subsequently been refactored to improve code quality, reproducibility, and model evaluation.

## Project Overview

Bike-sharing demand can be influenced by several environmental, seasonal, and calendar-related factors.

This project investigates the behaviour of two different groups of bike-sharing users:

* **Registered users**
* **Casual users**

The analysis combines:

* Data cleaning and preprocessing
* Exploratory Data Analysis (EDA)
* Data visualization
* Correlation analysis
* Random Forest regression
* Hyperparameter comparison
* Model evaluation
* Variable importance analysis

Separate Random Forest models are developed for registered and casual users to investigate whether the factors influencing rental demand differ between the two groups.

## Dataset

The dataset contains hourly bike rental information together with weather and calendar-related variables.

After preprocessing, the main variables used in the analysis are:

| Variable     | Description                                               |
| ------------ | --------------------------------------------------------- |
| `date_hour`  | Date and time of the observation                          |
| `season`     | Season of the year                                        |
| `holiday`    | Indicates whether the day is a holiday                    |
| `workingday` | Indicates whether the observation refers to a working day |
| `weather`    | Weather conditions                                        |
| `temp`       | Temperature                                               |
| `atemp`      | Perceived temperature                                     |
| `humidity`   | Humidity level                                            |
| `windspeed`  | Wind speed                                                |
| `casual`     | Number of rentals from non-registered users               |
| `registered` | Number of rentals from registered users                   |

The two response variables are:

* `registered`
* `casual`

They are modelled separately throughout the machine learning stage.

## Data Preparation

Before performing the analysis, the raw dataset is cleaned and transformed using R.

The preprocessing workflow includes:

* parsing date and time information
* reconstructing the hourly time sequence
* joining the complete time sequence with the original observations
* handling missing rental observations
* filling missing explanatory-variable values
* extracting temporal information
* creating a unified `date_hour` variable
* removing unnecessary intermediate variables
* converting categorical variables to factors
* converting numerical variables to appropriate data types
* replacing numerical category codes with descriptive labels

The final dataset provides a cleaner structure for both exploratory analysis and machine learning.

## Exploratory Data Analysis

The first part of the project explores differences and similarities between registered and casual users.

### Rental Trends Over Time

Bike rentals are aggregated over time to compare demand patterns between registered and casual customers.

This provides an overview of how bike-sharing usage evolves during the period covered by the dataset.

### Seasonality

Rental activity is analysed across different seasons to investigate the relationship between seasonal conditions and bike demand.

Registered and casual users are analysed separately to highlight differences between the two groups.

### Working Days and Holidays

Rental distributions are compared across different types of days.

This helps investigate how usage patterns change between:

* working days
* weekends
* holidays

### Weather Conditions

Bike rentals are compared across different weather conditions to explore how weather may influence demand.

### Temperature and Humidity

The relationships between rental demand and continuous environmental variables are explored graphically.

Smoothed curves are used to investigate potentially non-linear relationships involving:

* temperature
* humidity

### Correlation Analysis

A correlation matrix is calculated for the numerical variables to explore relationships among weather conditions and bike rental demand.

## Random Forest Regression

The machine learning stage uses **Random Forest regression** to model bike rental demand.

Two independent modelling workflows are created:

1. Random Forest for **registered users**
2. Random Forest for **casual users**

When predicting one customer group, the rental count of the other group is excluded from the explanatory variables.

This prevents information from the other response variable from being used directly to predict the target.

## Train/Test Split

A reproducible random split is used to divide the observations into:

* **75% training set**
* **25% test set**

The same train/test split is used for both registered and casual user models, making the comparison between the two modelling workflows more consistent.

The training set is used for model development and hyperparameter selection.

The test set is kept separate and used only for the final evaluation of predictive performance.

## Hyperparameter Comparison

Random Forest models are trained using different values of the `mtry` hyperparameter.

The candidate values are:

```text
mtry = 5
mtry = 8
mtry = 10
```

For each response variable, the candidate models are compared using **Out-of-Bag Mean Squared Error (OOB MSE)**.

OOB observations provide an internal estimate of prediction error during Random Forest training without requiring the test set for model selection.

## Model Selection

The Random Forest configuration with the lowest **OOB MSE** is selected independently for:

* registered users
* casual users

This means that hyperparameter selection is performed using only information available during model training.

The test set therefore remains independent from the model-selection process.

## Model Evaluation

After selecting the best `mtry` configuration, the final models are evaluated on the previously unseen test set.

Performance is measured using:

* **Out-of-Bag Mean Squared Error**
* **Test Mean Squared Error**

The project also visualizes how OOB MSE evolves as the number of trees increases for the different `mtry` configurations.

This makes it possible to compare model behaviour and evaluate whether prediction error stabilizes as additional trees are added.

## Variable Importance

Variable importance is extracted from the selected Random Forest models.

The analysis uses the increase in Mean Squared Error (`%IncMSE`) to measure the contribution of each explanatory variable.

A variable is considered more important when randomly perturbing its values causes a larger increase in prediction error.

Variable importance is calculated separately for:

* registered users
* casual users

This provides insight into whether the factors associated with bike-sharing demand differ between the two customer groups.

## Analysis Workflow

```text
Raw Bike-Sharing Data
        |
        v
Data Cleaning & Preparation
        |
        v
Exploratory Data Analysis
        |
        +-----------------------+
        |                       |
        v                       v
Registered Users           Casual Users
        |                       |
        +-----------+-----------+
                    |
                    v
          Reproducible 75/25 Split
                    |
        +-----------+-----------+
        |                       |
        v                       v
Registered Model           Casual Model
        |                       |
        v                       v
Random Forest              Random Forest
        |                       |
        v                       v
mtry Comparison            mtry Comparison
        |                       |
        v                       v
OOB MSE                    OOB MSE
        |                       |
        v                       v
Best Model                 Best Model
        |                       |
        v                       v
Test Evaluation            Test Evaluation
        |                       |
        v                       v
Variable Importance        Variable Importance
        +-----------+-----------+
                    |
                    v
         User Behaviour Comparison
```

## Technologies

The project is developed in **R**.

Main technologies and packages include:

* **R**
* **tidyverse**
* **randomForest**
* **lubridate**
* **ggplot2**
* **corrplot**
* **gridExtra**
* **cowplot**
* **scales**

## Skills Demonstrated

This project demonstrates practical skills in:

* R programming
* data cleaning and transformation
* exploratory data analysis
* data visualization with `ggplot2`
* temporal data manipulation
* correlation analysis
* feature engineering
* train/test splitting
* Random Forest regression
* hyperparameter comparison
* Out-of-Bag error estimation
* model selection
* model evaluation on unseen data
* variable importance analysis
* customer behaviour segmentation
* reproducible analytical workflows

## Repository Structure

```text
Bike-Sharing-Analysis/
├── data/
│   └── 9.csv
├── analisi_bike_sharing.R
└── README.md
```

> The analysis script can also detect `9.csv` when the dataset is stored directly in the project root.

## How to Run the Project

Clone the repository:

```bash
git clone https://github.com/NicolaRizzitello/Bike-Sharing-Analysis.git
```

Open the project directory in RStudio or set it as your R working directory.

Make sure the required packages are installed:

```r
install.packages(c(
  "tidyverse",
  "randomForest",
  "lubridate",
  "corrplot",
  "gridExtra",
  "cowplot",
  "scales"
))
```

Then run:

```r
source("analisi_bike_sharing.R")
```

The script performs the complete workflow from data preparation and exploratory analysis to Random Forest training, model selection, evaluation, and variable importance analysis.

## Conclusion

This project combines statistical exploration and machine learning to investigate bike-sharing demand.

By modelling registered and casual users separately, the analysis makes it possible to compare how environmental, seasonal, calendar-related, and temporal variables contribute to the rental behaviour of the two customer groups.

The Random Forest models are selected using **Out-of-Bag error**, while the independent test set is reserved for final performance evaluation.

The project demonstrates an end-to-end analytical workflow covering **data preparation, exploratory analysis, visualization, predictive modelling, model evaluation, and interpretation**.
