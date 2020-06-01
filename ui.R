# install.packages(c("shiny", "shinythemes", "shinycssloaders"))
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)

load(file="dataPrep.RData")

shinyUI(
  navbarPage("Crimes in USA",
             tabPanel("Crimes on map",
                      fluidPage(theme = shinytheme("flatly")),
                      pageWithSidebar(headerPanel(''),
                                      sidebarPanel(width = 3,
                                                   helpText('In order to see plot, specify required parameters: Year, Crime type, Regions.'),
                                                   sliderInput("yearMap", 
                                                               "Year:",
                                                               min = 2001, 
                                                               max = 2016,
                                                               sep="",
                                                               value = c(2004,2008)),
                                                   radioButtons(inputId = "crime_type_map",
                                                                label = "Crime type:",
                                                                choices = c("Violent" = "violent", 
                                                                            "Property" = "property"),
                                                                selected = "violent",
                                                                inline = TRUE),
                                                   selectizeInput("RegionFinder",
                                                                  "Regions:",
                                                                  choices = region_vector,
                                                                  multiple=TRUE),
                                                   helpText("Click Download in order to save plot in .png format."),
                                                   downloadButton('downloadPlot', 'Download')
                                      ),
                                      mainPanel(column(6), withSpinner(plotOutput('mapPlot', width="100%", height="700px"), type = getOption("spinner.type", 7)))))
             ,
             tabPanel("Dumbbell plot",
                      fluidPage(theme = shinytheme("flatly")),
                      pageWithSidebar(headerPanel(''),
                                      sidebarPanel(width = 3,
                                                   helpText('In order to see plot, specify required parameters: Crime type, States.'),
                                                   radioButtons(inputId = "rate_type",
                                                                label = "Crime type:",
                                                                choices = c("Violent crime rate" = "violent", 
                                                                            "Property crime rate" = "property",
                                                                            "Imprisonment rate" = "imprisonment"),
                                                                selected = "violent"),
                                                   selectizeInput("states",
                                                                  "States:",
                                                                  choices = states_vector,
                                                                  multiple=TRUE)
                                                   ),
                                      mainPanel(withSpinner(plotlyOutput('dumbPlot'),  type = getOption("spinner.type", 7))))
                      )
             ,
             tabPanel("Scatterplot",
                      fluidPage(theme = shinytheme("flatly")),
                      pageWithSidebar(headerPanel(''),
                                      sidebarPanel(width = 3,
                                                   helpText('In order to see plot, specify required parameters: Year, Crime type, States.'),
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
                                                                      choices = list("Select all" = "all",
                                                                                     "Murder and manslaughter" = "murder_manslaughter", 
                                                                                     "Robbery" = "robbery", 
                                                                                     "Aggrevated assault" = "agg_assault", 
                                                                                     "Burglary" = "burglary", 
                                                                                     "Larceny" = "larceny", 
                                                                                     "Vehicle theft" = "vehicle_theft"),
                                                                      selected = 1
                                                   ),
                                                   verbatimTextOutput("selected"),
                                                   selectizeInput("states",
                                                                  "States:",
                                                                  choices = states_vector,
                                                                  multiple=TRUE)
                                                   
                                      ),
                                      mainPanel(height = 10,
                                                withSpinner(plotlyOutput("scatter", height = "600px"), 
                                                            type = getOption("spinner.type", 7)),
                                                fluidRow(column(3, offset = 9, 
                                                                radioButtons(inputId = "legendChoice",
                                                                             label = "Legend:",
                                                                             choices = c("year", "state"),
                                                                             selected = "year",
                                                                             inline = TRUE))))
                      ))
  ))
