library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(skimr)


# UI -------------------------------

ui <- fluidPage(
  
  theme = shinytheme("sandstone"),
  tags$style(
    type = "text/css",
    "h1, h2, h3, h4 { text-align: center; }",
    "p { text-align: center; color: grey; }",
    "hr { margin-top: 2em; margin-bottom: 2em; }",
    "#children_madlib { color: white; }"
  ),
  
  navbarPage(
    "Reducing re-arrests in Chicago",
    id = "main",
    collapsible = T, 
    position = "fixed-top",
             
     # Tab panel: home -----------------
     tabPanel(
       "Home",
       fluidRow(
         width = 12,
         p(
           "In January 2012, the Cook County State’s Attorney’s Office established a program intended to reduce re-arrest among people on bail awaiting trial. The program ran through October 2013. The SA’s Office asked you to evaluate the effectiveness of the program and provided the data described below."
         )
       )
     ),
   
    # Descriptives ------------------
     tabPanel(
       "Descriptive statistics",
       
       fluidRow(
         width = 12,
         tableOutput("bal_tab")
       )
     )
  )    
)

server <- function(input, output, session) {
  
  output$bal_tab <-
    function () {
      data %>%
        select(
          treat,
          prior_arrests,
          race,
          gender,
          age
        ) %>%
        sumtable(
          group = "treat", 
          group.test = TRUE,
          out = "kable"
        )
    }
  
}

shinyApp(ui = ui, server = server)