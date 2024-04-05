#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)

#--------------------------define pages and the ui---------------------------------
page1UI<- fluidPage(
  titlePanel("F1 Race Record Dictionary"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("yearSelect", "Select Year", choices = unique(df_joined$year))
    ),
    mainPanel(
      DTOutput("driversTable")
    )
  )
)

page2UI <- fluidPage(
  titlePanel("F1 Race Top 5 Prediction"),
  
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
      # Add input fields corresponding to the features your model needs
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

#main ui for combining pages: 
ui <- navbarPage("F1 Race Analysis", id = "nav",
                 tabPanel("Driver Records", page1UI),  # Page 1 content
                 tabPanel("Race Predictions", page2UI) # Page 2 content
)


#---------------Define server logic----------------------------------
server <- function(input, output) {
  # Logic for Record Page
  filtered_data <- reactive({
    df_joined %>%
      filter(year == input$yearSelect)
  })
  
  output$driversTable <- renderDT({
    filtered_data()
  }, options = list(pageLength = 25))
  
  # Logic for Prediction Page
  observeEvent(input$predict, {
    # Safeguard against potential errors in prediction
    tryCatch({
      new_data <- data.frame(
        driverId = input$driverId,
        driverExpYears = input$driverExpYears,
        qualiResultPosition = input$qualiResultPosition,
        meanPace = input$meanPace,
        maxPace = input$maxPace,
        grid_penalty = input$grid_penalty
      )
      
      # Make predictions using the loaded model
      prediction <- predict(trained_workflow, new_data)
      
      # Output the prediction
      output$prediction <- renderText({
        paste("Predicted Top 5 Finish:", prediction$.pred_class)
      })
    }, error = function(e) {
      output$prediction <- renderText("Error in prediction: Check input values")
    })
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
