library(shiny)
library(dplyr)
library(viridis)
library(ggplot2)
library(ggrepel)
library(scales)
# library(spData)
# library(readr)

setwd("/srv/shiny-server/myapp")
load(file="dataPrep.RData")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # library(readr)

  output$table1 <- renderTable(ucr)
  output$table2 <- renderTable(head(ucr))
  
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
)
