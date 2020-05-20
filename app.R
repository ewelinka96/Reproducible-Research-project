
setwd("/Users/ewelinka/Desktop/RR_app/Reproducible-Research-project")
library(shinythemes)
library(shiny)
library(devtools)
library(dplyr)
# devtools::install_github('daattali/shinycssloaders')
library(shinycssloaders)
source("dataPrep.R")

ui <- navbarPage("Title of app",
                 tabPanel("Crimes on map",
                          fluidPage(theme = shinytheme("flatly")),
                          pageWithSidebar(headerPanel('Apply filters'),
                                          sidebarPanel(width = 4,
                                                       checkboxGroupInput(inputId = "RegionFinder",
                                                                          label = "Select Region(s):",
                                                                          choices = c("New England" = "NewEngland", "Mid Atlantic" = "MidAtlantic", "Mid West" = "MidWest", "South", "West", "South West" = "SouthWest", "Pacific", "Alaska", "Hawaii"),
                                                                          selected = "NewEngland")
                                          ),
                                          mainPanel(column(8))
                          )
                 )
                 ,
                 tabPanel("Dumbbell plot",
                          fluidPage(theme = shinytheme("flatly")),
                          pageWithSidebar(headerPanel('Apply filters'),
                                          sidebarPanel(width = 4),
                                          mainPanel(column(8)))),
                 
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
                                          )))
                 
)

server <- function(input, output){
    
    states_finder <- reactive({
        req(input$RegionFinder)
        filter(ucr, region %in% input$RegionFinder)
    })   
    
    
    regions <- c("MidWest", "SouthWest")
    years <- c("2008", "2009")
    
    
    output$map <- renderPlot({
 
        ucr_grouped <- ucr %>% 
            filter(region %in% "MidWest" & as.numeric(ucr$year) >= 2008 & as.numeric(year) <= 2009) %>% 
            group_by(jurisdiction) %>% 
            summarise(robbery_mean = mean(robbery))
        
        
        ucr %>% 
            filter(region %in% c("MidWest", "SouthWest")) %>% 
            filter(as.numeric(year) <= (2008-2000)) %>%
            filter(as.numeric(year) >= (2002-2000)) %>%  
            group_by(jurisdiction) %>% 
            summarise(robbery_mean = mean(robbery))
        
        # rename variable for merging
        names(ucr_grouped)[names(ucr_grouped) == "jurisdiction"] <- "NAME"
        # merge grouped ucr and state spatial data
        library(spData)
        us_states_ucr <- merge(us_states, ucr_grouped, by = "NAME")
        
        ggplot(data = us_states_ucr) +
            geom_sf(aes(fill = robbery_mean), lwd = 0, color = "white") +
            scale_fill_viridis_c(option = "viridis", trans = "sqrt", name = "Property crimes\nper population") +
            theme(legend.position = "none") +
            theme_minimal()
        
    })
    
    
    output$scatter <- renderPlot({
        
        if(input$percapita){
            
            prison_ucr_choice <- prison_ucr %>% filter((jurisdiction %in% input$states) & (year>=input$year[1] & year<=input$year[2]))
            ggplot(prison_ucr_choice,
                   aes(x = prison/state_population, 
                       y = (prison_ucr_choice[,input$crime_type] %>% rowSums())/prison_ucr_choice$state_population,
                       colour = as.factor(jurisdiction),
                       label = as.integer(year))) +
                geom_point(show.legend = TRUE, alpha = 0.5, size=10) +
                geom_text_repel(size=5, hjust=-0.5, vjust=-0.5, color="black") +
                scale_color_viridis_d(name = "States") +
                scale_y_continuous(labels = comma) + 
                scale_x_continuous(labels = comma) +            
                labs(x = "Number of prisoners", y = "Total crimes") +
                theme_minimal() +
                theme(legend.position = "bottom", text = element_text(size=20))        
            
        }
        else{
            prison_ucr_choice <- prison_ucr %>% filter((jurisdiction %in% input$states) & (year>=input$year[1] & year<=input$year[2]))
            ggplot(prison_ucr_choice,
                   aes(x = prison, 
                       y = prison_ucr_choice[,input$crime_type] %>% rowSums(),
                       colour = as.factor(jurisdiction),
                       label = as.integer(year))) +
                geom_point(show.legend = TRUE, alpha = 0.5, size=10) +
                geom_text_repel(size=5, hjust=-0.5, vjust=-0.5, color="black") +
                scale_color_viridis_d(name = "States") +
                scale_y_continuous(labels = comma) +
                scale_x_continuous(labels = comma) +        
                labs(x = "Number of prisoners", y = "Total crimes") +
                theme_minimal() +
                theme(legend.position = "bottom", text = element_text(size=20))        
            
        }
        
    })
    
}


shinyApp(ui = ui, server = server)
