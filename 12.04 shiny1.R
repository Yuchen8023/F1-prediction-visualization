
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

# Define libraries ----------------------------------------------------------
suppressWarnings({library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(viridis)
library(reshape2)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(caret)})

# create variables ----------------------------------------------------
team_colors <- c(
  "Mercedes" = "#00D2BE",
  "Ferrari" = "#DC0000",
  "Red Bull" = "#1E41FF",
  "McLaren" = "#FF8700",
  "Alpine" = "#0090FF",
  "Aston Martin" = "#006F62",
  "Williams" = "#005AFF",
  "Alfa Romeo" = "#900000",
  "Haas" = "#F0D787",
  "AlphaTauri" = "#2B4562",
  "Renault" = "#FFF500", # Renault's color is a vibrant yellow
  "Racing Point" = "#F596C8", # Racing Point often uses a pink color scheme
  "Force India" = "#F596C8", # Force India also used a similar pink before becoming Racing Point
  "Toro Rosso" = "#469BFF", # Toro Rosso has used various shades of blue; adjust as needed
  "Sauber" = "#006EFF" # Sauber has used blue and white colors
  # Add more teams and their colors as necessary
)

#load data-----------------------------------------------------------
df_joined <- read_csv("./Data/df_joined.csv")

# Define pages and the UI ---------------------------------------------------
##------------------------"Race analysis and strategy"tab content and UI---------------------------

Race_analysis_content <- fluidPage(
  titlePanel("Race Analysis and Strategy"),
  
  #subtitle
  fluidRow(
    column(12, h5("Apply filters in order to browse data: single select for driver, multiple select for year.", align = "left"))
  ),
  
  # Filters at the top
  fluidRow(
    column(6, pickerInput("driverNameInput", "Select Driver", 
                          choices = sort(unique(df_joined$driverName)),
                          selected = NULL, multiple = FALSE, 
                          options = list(`actions-box` = TRUE))),
    column(6, pickerInput("yearInput", "Select Year", 
                          choices = sort(unique(df_joined$year)),
                          selected = NULL, multiple = TRUE, 
                          options = list(`actions-box` = TRUE)))
  ),
  
  # Call-out cards
  fluidRow(
    div(style = "display: flex; justify-content: space-between;",  # Ensures boxes are evenly spaced
        valueBoxOutput("totalStopsCard", width = 4),
        valueBoxOutput("avgDurationCard", width = 4),
        valueBoxOutput("highestrankingCard", width = 4)
    )
  ),
  
  # Plots
  fluidRow(
    tabsetPanel(
      tabPanel("Qualifying vs. Race Outcomes", plotOutput("scatterPlot")),
      tabPanel("Average Lap Times per Race", plotOutput("barGraph"))
    )
  )
)



##---------------------- "Driver Records" tab content and UI-----------------------------
page1Content <- fluidPage(
  titlePanel("F1 Race Record Dictionary"),
  
  #subtitle
  fluidRow(
    column(12, h5("Apply filters in order to browse data of driver race and qualification results.", align = "left"))
  ),
  
  fluidRow(
    column(6, pickerInput("driverNameInput", "Select Driver", 
                          choices = sort(unique(df_joined$driverName)),
                          selected = NULL, multiple = TRUE, 
                          options = list(`actions-box` = TRUE))),
    column(6, pickerInput("yearInput", "Select Year", 
                          choices = sort(unique(df_joined$year)),
                          selected = NULL, multiple = TRUE, 
                          options = list(`actions-box` = TRUE)))),
  DTOutput("driversTable")
)

# "Race Predictions" tab content and UI
page2Content <- fluidPage(
  titlePanel("F1 Race Top 5 Prediction"),
  #subtitle
  fluidRow(
    column(12, h5("Maybe this page can make you earn money. But we are not responsible for the loss.", align = "left"))
  ),
  
  tags$head(
    tags$style(HTML("
      #predict {
        background-color: #007bff; /* Bootstrap primary color */
        color: white;
        border-color: #007bff;
      }
      #predict:hover {
        background-color: #0056b3; /* Darker shade for hover */
        border-color: #0056b3;
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("driverId", "Driver ID", value = 1),
      numericInput("driverExpYears", "Driver Experience (Years)", value = 0),
      numericInput("qualiResultPosition", "Qualifying Result Position", value = 1),
      numericInput("meanPace", "Mean Pace", value = 0),
      numericInput("maxPace", "Max Pace", value = 0),
      numericInput("grid_penalty", "Grid Penalty (Enter 0 or 1)", value = 0),
      helpText("0 for no grid penalty, 1 for yes."),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      textOutput("prediction")
    )
  )
)

# "Historical Data and Evolution" tab content and UI
page3Content <- fluidPage(
  titlePanel("F1 Circuit Winners Map"),
  
  # Subtitle
  fluidRow(
    column(12, 
           h5("Where the races have been held and the evolution of the number of races per year of the whole F1 history.", 
              style = "text-align: left; margin-bottom: 20px;")  
    )
  ),
  
  # Map output
  leafletOutput("circuitMap")
)

# Dashboard/Home/Navbar UI
addResourcePath("www", "./www")

ui <- dashboardPage(
  dashboardHeader(
    title = HTML("<img src='/www/F1-Logo.png' style='height:30px; width:auto; margin:10px;'>")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Driver Analysis and Strategy", tabName = "race_analysis_strategy", icon = icon("line-chart")),
      menuItem("Driver Records", tabName = "driver_records", icon = icon("user")),
      menuItem("Race Predictions", tabName = "race_predictions", icon = icon("flag-checkered")),
      menuItem("Overview of Performance", tabName = "overview_perf", icon = icon("globe"),
               menuSubItem("Constructor Performance", tabName = "cons_perf", icon = icon("industry")),
               menuSubItem("Driver Performance", tabName = "driver_perf", icon = icon("user")),
               menuSubItem("Driver on Circuit Performance", tabName = "driver_circuit", icon = icon("road")),
               menuSubItem("Wins by team", tabName = "wins_by_team", icon = icon("trophy"))
      ),        
      menuItem("Historical Data", tabName = "historical_data", icon = icon("calendar")) 
    )
      # ... add other menu items here if needed
  ),
  

  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Change the background color of the top navbar */
      .skin-blue .main-header .navbar, .skin-blue .main-header .logo {
        background-color: #FF1801 !important;
      }
      
      .skin-blue .sidebar-menu>li.active>a {
        color: #fff;
        background: #1e282c;
        border-left-color: #fff;
      }
      
      #homeContent::before {
        content: '';
        display: block;
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        background: url('/www/F1-background.jpg') no-repeat center center fixed;
        background-size: cover;
        filter: brightness(0.5); /* Apply brightness filter to the background */
        z-index: -2; /* Ensure the pseudo-element is behind the content */
      }
      
      
        /* Apply the background to the home content area only */
        #homeContent {
          background: url('/www/F1-background.jpg') no-repeat center center fixed;
          background-size: cover;
          background-attachment: fixed;
          position: fixed;
          height: 100vh; /* Full viewport height */
          width: 100%; /* Full width */
          color: white; /* White text color */
          overflow-y: hidden; /* Prevent vertical scrolling */
        }
        
          /* Adjust the margin-top for the h1 in the home content only */
        #homeContent h1 {
          margin-top: 20px; /* No space above h1 */
          font-size: 50px;
          margin-left: 20px;
          font-weight: bold;
          border-bottom: 1px solid white; /* White line under the heading */
          padding-bottom: 20px; /* Space between text and underline */
        }

        /* General background none for other tabs */
        .tab-content > .tab-pane:not(#homeContent) {
          background: none !important;
        }
        
        /* Specific background for other tabs if necessary, e.g., white */
        #page1Content, #page2Content {
          background-color: white;
          padding: 20px; /* Adjust or remove padding as needed */
        }
        
        /* Adjust content-wrapper to prevent any additional padding or margin */
        .content-wrapper {
          padding: 0 !important;
          overflow-x: hidden; /* Prevent horizontal scrolling */
        }
        
           /* Adjust content-wrapper to prevent any additional padding or margin */
        .content {
        padding-left: 0px;
        padding-right: 0px;
        padding: 0px;
        }
        
        /* Styles for the footer on the home page */
      #homeFooter {
          position: absolute; /* Positioned absolutely within its parent */
          right: 0; /* Align to the right edge of the parent */
          bottom: 50px; /* Align to the bottom edge of the parent */
          left: 0; /* Stretch across the bottom of the parent */
          color: #f6f6f6; /* Light grey text color */
          background-color: #ff0017; /* Red background color */
          text-align: left; /* Align the text to the right */
          padding: 10px 20px; /* Padding inside the footer */
          z-index: 1; /* Ensure it's above the background image */
      }
      
        
        
      "))
    ),
    tabItems(
      tabItem(tabName = "home", div(id = "homeContent",
                                    # Here you put all the content you want on the home page
                                    h1("Welcome to the F1 Race Analysis App!"),
                                    div(id="homeFooter", "Master in Business Analytics - Final Project in Data Analytics with R - Group 5 Section B - 2024")
                                    
                                    
                                    # ... additional home content ...
      )),
      tabItem(tabName = "driver_records", div(id = "page1Content", page1Content)),
      tabItem(tabName = "race_predictions", div(id = "page2Content", page2Content)),
      tabItem(tabName = "race_analysis_strategy", div(id ="Race_analysis_content",Race_analysis_content)),
      tabItem(tabName = "overview_perf",h2("Overview of Performance")),
      tabItem(tabName = "cons_perf",
              fluidPage(
                titlePanel("Line Chart of Constructors' Championship Points Over Years"),
                plotlyOutput("teamPerformancePlot")
              )
      ),
      tabItem(tabName = "driver_perf",
              fluidPage(
                titlePanel("Bar Chart of Driver Standings (Top 5 each year)"),
                plotlyOutput("driverStandingsBarChart")
              )
      ),
      tabItem(tabName = "driver_circuit",
              fluidPage(
                titlePanel("Heatmap of Wins by Driver and Circuit"),
                plotlyOutput("winHeatmap")
              )
      ),
      tabItem(tabName = "wins_by_team",
              fluidPage(
                titlePanel("Pie Chart of Race Wins by Team"),
                plotlyOutput("raceWinsPieChart")
              )
      ),
      
      tabItem(tabName = "historical_data", 
              fluidPage(
                titlePanel("F1 Circuit Winners Map and Analysis"),
                leafletOutput("circuitMap"), 
                plotOutput("racesPerYearPlot") 
              )
      )       
      # ... add other tab items here if needed
    
    )
  )
)

# ... server logic ...
# ... shinyApp call ...


# Server logic -----------------------------------------------------------

server <- function(input, output, session) {
  #Logic for Overview of Performance
  df_joined <- read_csv("./Data/df_joined.csv")
  readRDS('./Data/fitted_workflow.rds')
  performance_data <- read_csv("./Data/overview.csv")
  driver_standings_data <- read_csv("./Data/overview_driver_standings.csv")
  heatmap_data <- read_csv("./Data/heatmap.csv")
  long_data <- melt(heatmap_data, id.vars = "surname", variable.name = "Circuit", value.name = "Wins")
  race_wins <- reactive({read_csv("./Data/race_wins.csv")})
  circuits <- read_csv("./Data/circuits.csv")
  races_per_years <- read_csv("./Data/races_per_years.csv")
  
  df_grouped<- df_joined %>%    
    group_by(driverName,year) %>%
    summarise(total_stops = sum(total_stops),mean_stop_duration = mean(average_stop_duration_in_seconds, na.rm = TRUE))
  
  Filtered_data<- reactive({
    df_joined %>%
      filter(driverName %in% input$driverNameInput)%>%
      filter(year %in% input$yearInput)
  })
  
  df_grouped_re <- reactive({
    df_grouped %>%
      filter(driverName %in% input$driverNameInput)%>%
      filter(year %in% input$yearInput)
  })
  
  
  #logic for driversTable
  output$driversTable <- renderDT({
    req(Filtered_data())  # Ensure the filtered data exists
    #filter columns of table 
    data <- Filtered_data() %>%
      select(driverId, driverName, year, racePosition, driverExpYears, qualiResultPosition, nationality, url, maxPace, meanPace)
    datatable(data, rownames = FALSE, options = list(pageLength = 25))
  })
  
  output$teamPerformancePlot <- renderPlotly({
    p <- ggplot(performance_data, aes(x = year, y = total_points, group = name, color = name, text = paste("Team:", name, "<br>Total Points:", total_points))) +
      geom_line(size = 1.5) +  # Increased line thickness
      geom_point(size = 3, show.legend = FALSE) +  # Larger points
      scale_color_manual(values = team_colors) +
      theme_minimal() +
      labs(title = "F1 Team Performance Over the Years",
           x = "Year", y = "Total Points",
           color = "Team") +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 20),  # Corrected to size for text elements
        axis.title = element_text(size = 14),  # Increased axis titles size
        axis.text = element_text(size = 12)    # Increased axis text size
      ) +
      scale_x_continuous(breaks = seq(min(performance_data$year), max(performance_data$year), by = 1))
    
    # Convert ggplot object to plotly for interactivity
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")  # Configure hover behavior
  })
  
  #Logic for Overview of Performance - Driver Standings
  output$driverStandingsBarChart <- renderPlotly({
    # Generate a sufficient number of colors using viridis
    num_drivers <- length(unique(driver_standings_data$driverRef))
    colors <- viridis::viridis(n = num_drivers, option = "D")
    
    # Create the plot
    plot <- ggplot(driver_standings_data, aes(x = reorder(driverRef, -total_points), y = total_points, fill = driverRef)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      facet_wrap(~ year, scales = "free_x") +  # Add faceting by year
      labs(title = "Top 5 Driver Standings by Year", x = "", y = "Total Points") +
      theme_minimal() +
      theme(
        text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        strip.text = element_text(size = 14, face = "bold")  # Style the facet labels
      ) +
      scale_fill_manual(values = colors)
    
    # Convert to plotly for interactivity
    ggplotly(plot, tooltip = c("x", "y")) %>%
      layout(hovermode = "closest", hoverlabel = list(bgcolor = "white", font = list(size = 16)))
  })
  # Logic for Driver Circuit Performance
  output$winHeatmap <- renderPlotly({
    # Melt the data from wide to long format
    long_data <- reshape2::melt(heatmap_data, id.vars = "surname", variable.name = "Circuit", value.name = "Wins")
    
    # Generate the heatmap plot
    plot_ly(long_data, x = ~Circuit, y = ~surname, z = ~Wins, type = 'heatmap', colorscale = 'Viridis') %>%
      layout(
        title = 'F1 Wins by Driver and Circuit',
        xaxis = list(title = '', tickangle = 45),
        yaxis = list(title = ''),
        margin = list(l = 150, b = 150)  # Adjust margins to fit axis labels
      )
  })
  # Logic Render the Pie Chart
  output$raceWinsPieChart <- renderPlotly({
    data <- race_wins()
    
    plot_ly(data, labels = ~name, values = ~wins, type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            marker = list(colors = unname(team_colors[data$name]))) %>%
      layout(title = "F1 Team Wins (2018-2022)",
             showlegend = TRUE,
             legend = list(orientation = "h", x = 0.1, y = 1.1),
             annotations = list(text = 'Wins', x = 0.5, y = 0.7, font = list(size = 16),font = list(size = 18, family = "Arial, bold"), showarrow = FALSE))
  })
  
  
  
  # Logic for Race Predictions Page
  observeEvent(input$predict, {
    tryCatch({
      new_data <- data.frame(
        driverId = as.numeric(input$driverId),
        driverExpYears = as.numeric(input$driverExpYears),
        qualiResultPosition = as.numeric(input$qualiResultPosition),
        meanPace = as.numeric(input$meanPace),
        maxPace = as.numeric(input$maxPace),
        grid_penalty = as.numeric(input$grid_penalty)
      )
      
      fitted_workflow <- readRDS("./Data/fitted_workflow.rds")
      # Make predictions using the loaded model
      prediction <- predict(object=fitted_workflow, new_data=new_data,type = "class")
      
      # Output the prediction
      output$prediction <- renderText({
        paste("Predicted Top 5 Finish:", prediction$.pred_class)
      })
    }, error = function(e) {
      output$prediction <- renderText(paste("Error in prediction:", e$message))
    })
  })
  
  
  
  ##----------logic for Race_analysis_content page------------------------
  
  output$totalStopsCard <- renderValueBox({
    data <- Filtered_data()
    total_stops <- sum(data$total_stops, na.rm = TRUE)
    valueBox(
      format(total_stops, big.mark = ","), "Total Pit Stops", icon = icon("car"),
      color = "purple"
    )
  })
  
  output$avgDurationCard <- renderValueBox({
    data <- Filtered_data()
    avg_duration <- mean(data$average_stop_duration_in_seconds, na.rm = TRUE)
    valueBox(
      format(round(avg_duration, 2), nsmall = 2), "Average Stop Duration (s)", icon = icon("clock"),
      color = "green"
    )
  })
  
  output$highestrankingCard <- renderValueBox({
    data <- Filtered_data()
    highest_ranking <- min(data$racePosition, na.rm = TRUE)
    valueBox(
      format(highest_ranking, na.rm = TRUE), "Highest Race Ranking", icon = icon("trophy"),
      color = "orange"
    )
  })
  
  # Scatter plot output for Qualifying vs. Race Outcomes
  output$scatterPlot <- renderPlot({
    req(Filtered_data())  # Ensure data is ready
    data <- Filtered_data()
    if(nrow(data) > 0) {
      ggplot(data, aes(x = qualiResultPosition, y = racePosition, color = as.factor(year))) +
        geom_point() +
        labs(x = "Qualifying Position", y = "Final Race Position", title = "Qualifying vs. Race Outcomes") +
        scale_color_brewer(palette = "Set1")
    }
  }, height = 600)
  
  
  # Bar graph for Average Lap Times over Time
  output$barGraph <- renderPlot({
    req(Filtered_data())
    data <- Filtered_data() 
    ggplot(data, aes(x = date, y = avg_lap_milliseconds, fill = as.factor(year))) +
      geom_bar(stat = "identity", position = "dodge") +  # Ensure you use 'identity' for pre-summarised data
      labs(x = "Race Date", y = "Average Lap Time (s)", title = "Average Lap Times Over Time") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate X labels for better readability
  },height = 600)
  
  
  
  # Logic for Historical Data Page
  output$circuitMap <- renderLeaflet({
    if (is.null(circuits)) {
      return(NULL)
    }
    
    leaflet(circuits) %>%
      addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(
        lng = ~lng, lat = ~lat,
        icon = awesomeIcons(
          icon = 'flag-checkered',
          iconColor = 'white',
          markerColor = 'red',
          spin = TRUE
        ),
        popup = ~paste("<strong>", name, "</strong><br/>", location, "<br/>", country, "<br/>", "<a href='", url, "' target='_blank'>Link</a>", sep=""),
        label = ~name  # simple tooltip showing name on hover
      ) %>%
      addMiniMap(tiles = providers$CartoDB.Positron, toggleDisplay = TRUE) %>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Esri World Imagery", "CartoDB"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$racesPerYearPlot <- renderPlot({
    if (is.null(races_per_years)) {
      return(NULL)
    }
    
    # Prepare the plot
    p <- ggplot(data = races_per_years, aes(x = Year, y = Total, fill = ifelse(Year >= 2018, "Post-2017", "Pre-2018"))) +
      geom_col() +  # Removed the fill color here as we will be using fill inside aes()
      scale_fill_manual(values = c("Post-2017" = "red", "Pre-2018" = "#0073C2FF")) +
      labs(title = "Number of F1 Races per Year",
           subtitle = "Annual count of races held globally",
           x = "Year",
           y = "Total Races",
           caption = "Data source: FIA") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, color = "#555555"),
        plot.caption = element_text(hjust = 1, color = "#888888"),
        axis.title.x = element_text(face = "bold", color = "#333333", size = 14),
        axis.title.y = element_text(face = "bold", color = "#333333", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#555555"),
        panel.grid.major = element_line(color = "#cccccc"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none"  # Hide the legend if not needed
      ) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  # Adjust x-axis breaks nicely
    
    print(p)
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
