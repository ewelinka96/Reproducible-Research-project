# install.packages(c("shiny", "shinythemes", "shinycssloaders"))
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)

# Define UI for application that draws a histogram

load(file="dataPrep.RData")

shinyUI(
  navbarPage("Crimes in USA",
             tabPanel("Crimes on map",
                      fluidPage(theme = shinytheme("flatly")),
                      pageWithSidebar(headerPanel('Apply filters'),
                                      sidebarPanel(width = 3,
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
                                                   downloadButton('downloadPlot', 'Download')
                                      ),
                                      mainPanel(column(6), withSpinner(plotOutput('mapPlot', width="100%", height="700px"), type = getOption("spinner.type", 7)))))
             ,
             tabPanel("Dumbbell plot",
                      fluidPage(theme = shinytheme("flatly")),
                      pageWithSidebar(headerPanel('Apply filters'),
                                      sidebarPanel(width = 3,
                                                   radioButtons(inputId = "rate_type",
                                                                label = "Crime type:",
                                                                choices = c("Violent crime rate" = "violent", 
                                                                            "Property crime rate" = "property",
                                                                            "Imprisonment rate" = "imprisonment"),
                                                                selected = "violent")),
                                      mainPanel(withSpinner(plotlyOutput('dumbPlot')),  type = getOption("spinner.type", 7)))
                      )
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
