# [Bike Sharing Analysis](https://github.com/NicolaRizzitello/Bike-Sharing-Analysis/blob/main/analisi_bike_sharing.R)
This one of my project that I did during Master's course for Statistics module. In this project I used the Random Forest algorithm to see which are the most important variables among the explanatory variable.

**Language**

![alt text](https://user-images.githubusercontent.com/103247709/162481933-f4edc1f6-b68c-4113-a03c-15400b240f10.png)

## About Dataset
The dataset was composed by 10866 rows and 14 variables initially. The data cleaning process has brought some changes so the final dataset is composed by 10944 and 11 variables:
* **date_hour**: date and time of rent
* **season**: season of the period (categorical)
* **holiday**: working-days and no-working-days (categorical)
* **workingday**: week-day and week-end (categorical)
* **weather**: weather situation (categorical)
* **temp**: temperature in celsius degree
* **atemp**: perceived temperature
* **humidity**: humidity rate
* **windspeed**: wind speed
* **casual**: customers not registered (response variable)
* **registred**: customers registered (response variable) 

## About the analysis 
In order to highlight differences and similarities between the habits of registered and non-registered users, an analysis exploratory allows to have a brief description of the behavior of the response variables in relation to explanatory variables.

The second step of the analysis has been to divide the dataset in training and test set, the first composed by 75% of the rows of the dataset and the test set of the 25% and I used the Regression Tree to see which are the most important explonatory variables.
