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

Predictive Modeling
A machine learning model was developed to predict whether a driver would finish in the top 5 based on qualifying results. The features used for this prediction include driver ID, years of experience, qualifying result position, mean and max pace, and whether there was a grid penalty.
 
Interactive Components
F1 Circuit Winners Map
An interactive map displays the various circuits where F1 races have taken place. Users can interact with the map to discover information about each circuit, including its name, location, and country.
Historical Data Analysis
Graphical visualisations provide insights into the historical data, such as the number of races held each year and the distribution of wins among teams.
Race Outcome Predictions
The dashboard allows users to input race-related parameters to predict the outcome of races, providing insights into which drivers are likely to finish in the top 5 based on various performance metrics.
Visualisations
Several types of visualisations are included:
Bar charts: Display the number of races per year with an option to highlight recent years in a different colour.
Line charts: Show constructors' championship points over the years.
Heatmaps: Visualize wins by drivers and circuits.
Pie charts: Represent race wins by teams.
 
Usage
The dashboard is designed with user interactivity in mind. Users can select different years, drivers, and other parameters to filter the data and interact with the visualisations.
 
Technical Implementation
The dashboard is built using R and the Shiny web application framework. The tidyverse collection of R packages is extensively used for data manipulation and visualisation, along with other packages such as plotly for interactive plots, leaflet for mapping, and shinydashboard for the dashboard layout.

