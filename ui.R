library(shiny)
library(shinythemes)
library(shinycssloaders)

# Define UI for application that draws a histogram

load(file="dataPrep.RData")

shinyUI(
  navbarPage("Title of app",
             tabPanel("Crimes on map",
                      fluidPage(theme = shinytheme("flatly")),
             pageWithSidebar(headerPanel('Apply filters'),
                             sidebarPanel(width = 4,
                                          checkboxGroupInput(inputId = "RegionFinder",
                                                             label = "Select Region(s):",
                                                             choices = c("New England" = "NewEngland", "Mid Atlantic" = "MidAtlantic", "Mid West" = "MidWest", "South", "West", "South West" = "SouthWest", "Pacific", "Alaska", "Hawaii"),
                                                             selected = "NewEngland")
                             ),
                             mainPanel(column(8), tableOutput('table1'))))
             ,
             tabPanel("Dumbbell plot",
                      fluidPage(theme = shinytheme("flatly")),
                      pageWithSidebar(headerPanel('Apply filters'),
                                      sidebarPanel(width = 4),
                                      mainPanel(column(8), tableOutput('table2'))
                                      ))
             ,
             tabPanel("Scatterplot",
                      fluidPage(theme = shinytheme("flatly")),
                      pageWithSidebar(headerPanel('Scatterplot analysis'),
                                      sidebarPanel(width = 3,
                                                   sliderInput("year", 
                                                               "Year:",
                                                               min = 2001, 
                                                               max = 2016,
                                                               sep="",
                                                               value = c(2004,2008)),
                                                   checkboxInput("percapita",
                                                                 "Data per capita",
                                                                 value = FALSE),
                                                   checkboxGroupInput("crime_type",
                                                                      "Crime type:", 
                                                                      choices = list("murder and manslaughter" = "murder_manslaughter", 
                                                                                     "robbery" = "robbery", 
                                                                                     "aggrevated assault" = "agg_assault", 
                                                                                     "burglary" = "burglary", 
                                                                                     "larceny" = "larceny", 
                                                                                     "vehicle theft" = "vehicle_theft"),
                                                                      selected = 1
                                                   ),
                                                   selectizeInput("states",
                                                                  "States:",
                                                                  choices = states_vector,
                                                                  multiple=TRUE),
                                                   submitButton("Update filters")
                                      ),
                                      mainPanel(height = 10, 
                                                withSpinner(plotOutput("scatter", height = "900px"), type = getOption("spinner.type", 7))
                                      )
                                      ))             
))
