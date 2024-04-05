# F1-prediction-visualization
Project for Data Analysis with R. 

# Machine learning project with R 
## Problem: Do qualifications determine race results in Formula 1?
Formula 1 is a pinnacle of motorsport, but also a sport that is driven by data. Throughout its 73-year history, a lot of data has been gathered during the racing weekends (yet not all of it published for obvious strategic reasons). Every racing weekend, a question arises of how important the starting grid position is for the race finish. Does pole position translate to an automatic victory? If that was the case, what are the drivers even racing for? The purpose of this project is to explore the link between qualifying results and the finishing position in the race through the means of ML. 

## Algorithm applied
Classification is performed using a Decision Tree, Random Forest and XGBoost. 
We predict the aspect in a 0/1 way: being in the top 5. 

## Interactive model
A shiny app was built to allow users to input driver and qualifications results, and return the race outcome. 

