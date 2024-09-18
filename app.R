# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Assuming merged_df is already loaded
# merged_df <- read.csv("path_to_your_dataset.csv")

# Create the Shiny UI
ui <- fluidPage(
  
  # App title
  titlePanel("Impact of Log Revenues on Rank - 2024 Fortune 1000"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      # Slider input for log_Revenues_M
      sliderInput("logRevenueRange", 
                  "Select Log Revenue Range:",
                  min = round(min(merged_df$log_Revenues_M, na.rm = TRUE), 1),
                  max = round(max(merged_df$log_Revenues_M, na.rm = TRUE), 1),
                  value = c(min(merged_df$log_Revenues_M, na.rm = TRUE), 
                            max(merged_df$log_Revenues_M, na.rm = TRUE)),
                  step = 0.1)
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      # Output: Scatter plot
      plotOutput("rankPlot")
    )
  )
)

# Create the Shiny Server
server <- function(input, output) {
  
  # Reactive expression to filter the dataset based on slider input
  filteredData <- reactive({
    merged_df %>%
      filter(log_Revenues_M >= input$logRevenueRange[1],
             log_Revenues_M <= input$logRevenueRange[2])
  })
  
  # Render the scatter plot
  output$rankPlot <- renderPlot({
    ggplot(filteredData(), aes(x = log_Revenues_M, y = Rank)) +
      geom_point(aes(fill = Rank), alpha = 0.6) +  # Changed color to fill for better continuous data representation
      scale_y_reverse() +  # Use reverse scale for Rank
      scale_x_continuous(labels = scales::comma_format()) +
      labs(
        title = "Effect of Log Revenues on Company Rank",
        x = "Log of Revenues (Million USD)",
        y = "Company Rank",
        fill = "Rank"  # Changed legend label
      ) +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right"
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
