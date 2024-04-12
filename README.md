# F1-prediction-visualization
Project for Data Analysis with R. 

## Machine learning project with R 
### Problem: Do qualifications determine race results in Formula 1?
Formula 1 is a pinnacle of motorsport, but also a sport that is driven by data. Throughout its 73-year history, a lot of data has been gathered during the racing weekends (yet not all of it published for obvious strategic reasons). Every racing weekend, a question arises of how important the starting grid position is for the race finish. Does pole position translate to an automatic victory? If that was the case, what are the drivers even racing for? The purpose of this project is to explore the link between qualifying results and the finishing position in the race through the means of ML. 

### Algorithm applied
Classification is performed using a Decision Tree, Random Forest and XGBoost. 
We predict the aspect in a 0/1 way: being in the top 5. 

### Interactive model
A shiny app was built to allow users to input driver and qualifications results, and return the race outcome. 

## Data Visualization
The idea is to create an interactive dashboard to allow users to search for information related to the team or driver they are interested in while having a brief overview of the history of F1.

## Data Sources
The data used in this dashboard is sourced from several CSV files containing historical records and statistics about Formula 1 races, drivers, constructors, and circuits. These files include:
circuits.csv: Records of circuits where races are held.
constructor_results.csv: Race results of the constructor's championship.
constructor_standings.csv: Final standings of the constructor's championship.
constructors.csv: List of Constructors in F1 with links to Wikipedia pages.
drivers.csv: F1 drivers with links to their Wikipedia pages.
driver_standings.csv: Final standings of the driver's championship.
lap_times.csv: Lap times in F1.
pit_stops.csv: Pit stops in F1.
qualifying.csv: Qualifying rounds in F1.
races.csv: List of F1 races.
results.csv: Race results per driver and constructor.
status.csv: Mapping of various statuses in the races.
seasons.csv: Seasons of F1 with Wikipedia links.
sprint_results.csv: Results of F1 sprint races per race, driver, and constructor.

## Feature Engineering for Machine Learning
To prepare the data for analysis and modelling, several features were engineered:
year started: The first year a driver participated in F1 races. 
meanPace and maxPace: The average and maximum pace of a driver during the qualifying rounds. 
grid_penalty: A binary feature indicating whether a driver had a grid penalty. 
is_top_5: A binary target variable for modelling, indicating whether a driver finished in the top 5.
