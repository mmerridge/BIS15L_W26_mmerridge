library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)

elephants <- read_csv("data/elephants_data/elephants.csv") %>%
  clean_names()

ui <- dashboardPage(
  dashboardHeader(title = "African Elephants Age and Height by Sex"),
  dashboardSidebar(disable = T),
  dashboardBody(
    selectInput("y",
                "Select Variable",
                choices  = c("age", "height"),
                selected = "age"),
    plotOutput("plot", width = "500px", height = "400px")
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    elephants %>%
      filter(!is.na(sex)) %>%
      ggplot(aes(x    = sex,
                 y    = .data[[input$y]],
                 fill = sex)) +
      geom_boxplot(alpha = 0.8, color = "black") +
      labs(title = paste("Range of", input$y, "by Sex"),
           x     = "Sex",
           y     = input$y) +
      theme_minimal()
  })
}

shinyApp(ui, server)
