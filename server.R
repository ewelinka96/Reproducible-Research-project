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
    prison_ucr_choice <- prison_ucr %>% filter((jurisdiction %in% input$states) & (year>=input$year[1] & year<=input$year[2]))
    
    if(input$percapita){
      ggplot(prison_ucr_choice,
             aes(x = prison/state_population, 
                 y = (prison_ucr_choice[,input$crime_type] %>% rowSums())/prison_ucr_choice$state_population)) +
                 {if(input$labelChoice == "year") geom_point(aes(colour = as.factor(jurisdiction)), show.legend = TRUE, alpha = 0.5, size=10)} +
                 {if(input$labelChoice == "state") geom_point(aes(colour = as.factor(year)), show.legend = TRUE, alpha = 0.5, size=10)} +
                 {if(input$labelChoice == "year") geom_text_repel(aes(label = as.factor(year)), size=5, hjust=-0.5, vjust=-0.5, color="black")} +   
                 {if(input$labelChoice == "state") geom_text_repel(aes(label = as.factor(jurisdiction)), size=5, hjust=-0.5, vjust=-0.5, color="black")} + 
        scale_color_viridis_d(name = "Years") +        
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = comma) +
        labs(x = "Number of prisoners", y = "Total crimes") +
        theme_minimal() +
        theme(legend.position = "bottom", text = element_text(size=20))  
      
    }
    else{
      ggplot(prison_ucr_choice,
             aes(x = prison, 
                 y = prison_ucr_choice[,input$crime_type] %>% rowSums())) + 
                 {if(input$labelChoice == "year") geom_point(aes(colour = as.factor(jurisdiction)), show.legend = TRUE, alpha = 0.5, size=10)} +
                 {if(input$labelChoice == "state") geom_point(aes(colour = as.factor(year)), show.legend = TRUE, alpha = 0.5, size=10)} +
                 {if(input$labelChoice == "year") geom_text_repel(aes(label = as.factor(year)), size=5, hjust=-0.5, vjust=-0.5, color="black")} +   
                 {if(input$labelChoice == "state") geom_text_repel(aes(label = as.factor(jurisdiction)), size=5, hjust=-0.5, vjust=-0.5, color="black")} + 
        scale_color_viridis_d(name = "Years") +
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = comma) +        
        labs(x = "Number of prisoners", y = "Total crimes") +
        theme_minimal() +
        theme(legend.position = "bottom", text = element_text(size=20))  
      
    }
    
  })
  
  
}
)
