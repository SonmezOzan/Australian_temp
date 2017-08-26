library(shiny)
library(shinydashboard)
library(fda)
load("Temp.rda")

ui <- dashboardPage(
   dashboardHeader(title="Australian Temp Data"
   ),
   ## Sidebar content
   dashboardSidebar(
      sidebarMenu(
         menuItem("Analysis", tabName = "widgets", icon = icon("th"))
      )
   ),
   dashboardBody(
      tabItems(
         
         # Second tab content
         tabItem(tabName = "widgets",
                 sidebarPanel(
                    selectInput("dataset", "Choose a station:", 
                                choices = c("Sydney(Observatory Hill)",
                                            "Melbourne(Regional Office)",
                                            "Boulia Airport", 
                                            "Cape Otway Lighthouse", 
                                            "Gayndah Post Office",
                                            "Gunnedah Pool", 
                                            "Hobart(Ellerslie Road)",
                                            "Robe Comparison")),
                    submitButton("Update View")
                 ),
                 
                 mainPanel(
                    tabsetPanel(
                       tabPanel("Plot",  plotOutput("plot")), 
                       tabPanel("Proposed Method",  tableOutput("ff")), 
                       tabPanel("fPCA-based Method", tableOutput("fpca")),
                       tabPanel("Estimated Change Function",  plotOutput("plot3")),
                       tabPanel("Explained Change",  plotOutput("plot2"))
                    ))
                 
         )
      )
   )
)

