#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("SHIRE Cost Sharing"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Adrianne",
                  "Adrianne's Salary:",
                  min = 0,
                  max = 300000,
                  step = 5000,
                  value = 200000),
      sliderInput("Daryn",
                  "Daryn's Salary:",
                  min = 0,
                  max = 300000,
                  step = 5000,
                  value = 80000),
      sliderInput("Emerson",
                  "Emerson's Salary:",
                  min = 0,
                  max = 300000,
                  step = 5000,
                  value = 0),
      sliderInput("Sade",
                  "Sade's Salary:",
                  min = 0,
                  max = 300000,
                  step = 5000,
                  value = 90000),
      br(),
      br(),
      sliderInput("Min_p_des",
                  "Minimum contribution percent (DES)",
                  min = 0,
                  max = 1,
                  value = 0.15
      ),
      sliderInput("Min_p_a",
                  "Minimum contribution percent (A)",
                  min = 0,
                  max = 1,
                  value = 0.3
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barplot"),
      # plotOutput("piechart"),
      tableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output$barplot <- renderTable({
  
  df_final <- reactive({ 
    df <- data.frame("Name" = c("Adrianne", "Daryn", "Emerson", "Sade"),
                     "Salary" = c(input$Adrianne, input$Daryn, input$Emerson, input$Sade),
                     "Min_p" = c(input$Min_p_a, rep(input$Min_p_des, 3)))
    
    df_p <- df %>% 
      mutate(
        Total = sum(Salary),
        P_actual = Salary/Total,
        Which_min = case_when(P_actual < Min_p ~ Min_p,
                              P_actual >= Min_p ~ 0),
        Total_minus_min = (1 - sum(Which_min)),
        Salary_adj = case_when(P_actual < Min_p ~ 0,
                               P_actual >= Min_p ~ Salary),
        Total_adj = sum(Salary_adj),
        P_adj = case_when(
          P_actual < Min_p ~ Min_p,
          P_actual >= Min_p ~ (Salary_adj / Total_adj)*Total_minus_min
        )
      )
    
    df_final <- df_p %>% 
      transmute(
        Name,
        Salary,
        Min_p,
        P_actual,
        P_adj = round(P_adj*100)
      )
    
  })
  
  output$barplot <- renderPlot({
    ggplot(df_final(), aes(x = Name, y = P_adj, fill = Name)) +
      geom_col() +
      geom_text(aes(label = P_adj, vjust = 5)) +
      theme_minimal() +
      ylab("") +
      xlab("")  +
      theme(legend.position="none",
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
  })
  
  output$piechart <- renderPlot({
    
    ggplot(df_final(), aes(x = "", y = P_adj, fill = Name)) +
      geom_col() +
      theme_minimal() +
      ylab("") +
      xlab("")
  })
  
  output$table <- renderTable({
    df_final()
  })    
}

# Run the application 
shinyApp(ui = ui, server = server)
