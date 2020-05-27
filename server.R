library(shiny)
library(dplyr)
library(viridis)
library(ggplot2)
library(ggrepel)
library(scales)
library(sf)
library(spData)
library(readr)
library(plotly)


setwd("/srv/shiny-server/myapp")
# setwd("/Users/ewelinka/Desktop/RR_app/Reproducible-Research-project")
load(file="dataPrep.RData")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


  output$mapPlot <- renderPlot({
    
    regions <- input$RegionFinder
    years <- input$yearMap
    
    req(length(regions)>0 & length(years)>0)
    
    options(scipen=999)
    
        {if(input$crime_type_map == "violent"){
          
                  ucr_grouped <- ucr %>%
                    filter(region %in% regions) %>%
                    filter(as.numeric(year) <= (years[2]-2000)) %>%
                    filter(as.numeric(year) >= (years[1]-2000)) %>%
                    group_by(jurisdiction) %>%
                    summarise(violent_crime_total_mean = mean(violent_crime_total))
                  
                  names(ucr_grouped)[names(ucr_grouped) == "jurisdiction"] <- "NAME"
                  
                  us_states_ucr <- merge(us_states, ucr_grouped, by = "NAME")
                  us_states_ucr <- sf::st_as_sf(us_states_ucr)
                  
                  ggplot(data = us_states_ucr) +
                    geom_sf(aes(fill = violent_crime_total_mean, geometry = geometry), lwd = 0, color = "white") +
                    scale_fill_viridis_c(option = "viridis", trans = "sqrt", name = "Violent crimes", labels = comma) +
                    theme_minimal() +
                    theme(legend.key.height = unit(5, "lines"),
                          text = element_text(size=20))
        }
        else
        {
          ucr_grouped <- ucr %>%
            filter(region %in% regions) %>%
            filter(as.numeric(year) <= (years[2]-2000)) %>%
            filter(as.numeric(year) >= (years[1]-2000)) %>%
            group_by(jurisdiction) %>%
            summarise(property_crime_total_mean = mean(property_crime_total))
          
          names(ucr_grouped)[names(ucr_grouped) == "jurisdiction"] <- "NAME"
          
          us_states_ucr <- merge(us_states, ucr_grouped, by = "NAME")
          us_states_ucr <- sf::st_as_sf(us_states_ucr)
          
          ggplot(data = us_states_ucr) +
            geom_sf(aes(fill = property_crime_total_mean, geometry = geometry), lwd = 0, color = "white") +
            scale_fill_viridis_c(option = "viridis", trans = "sqrt", name = "Property crimes", labels = comma) +
            theme_minimal() +
            theme(legend.key.height = unit(5, "lines"),
                  text = element_text(size=20))            
          }}


    })
  
  
  output$dumbPlot <- renderPlotly({
    
    
    violent_plot <- plot_ly(violent, color = I("gray80"),
                            text = ~paste('Pct change: ', round(pct_change_violent), '%')) %>%
      add_segments(x = ~violent_crime_total_rate_2001, 
                   xend = ~violent_crime_total_rate_2016, 
                   y = ~jurisdiction, 
                   yend = ~jurisdiction, 
                   showlegend = FALSE) %>%
      add_markers(x = ~violent_crime_total_rate_2001, 
                  y = ~jurisdiction, 
                  color = I("#79D150")) %>%
      add_markers(x = ~violent_crime_total_rate_2016, 
                  y = ~jurisdiction, 
                  color = I("#355F8D")) %>%
      layout(title="Percent change in state violent crime rates, 2001 vs 2016",
             margin = list(l = 65),
             showlegend = FALSE,
             yaxis = list(size = 0.8),
             autosize = F,
             height = 1000)
    
    ggplotly(violent_plot, tooltip = "text")
    
  })
  
  output$scatter <- renderPlotly({

    prison_ucr_choice <- prison_ucr %>% filter((jurisdiction %in% input$states) & (year>=input$year[1] & year<=input$year[2]))
    req(nrow(prison_ucr_choice)>0)
    
    if(input$percapita){
        ggplotly(ggplot(prison_ucr_choice,
               aes(x = prison/state_population,
                   y = (prison_ucr_choice[,input$crime_type] %>% rowSums())/prison_ucr_choice$state_population,
                   text = paste("Year: ", as.factor(year), 
                                "<br>State: ", as.factor(jurisdiction),
                                "<br>Number of prisoners:", round(prison/state_population, 4),
                                "<br>Total crimes:", round((prison_ucr_choice[,input$crime_type] %>% rowSums())/prison_ucr_choice$state_population,4)))) +
                   {if(input$legendChoice == "state") geom_point(aes(colour = as.factor(jurisdiction)), show.legend = TRUE, alpha = 0.5, size=5)} +
                   {if(input$legendChoice == "year") geom_point(aes(colour = as.factor(year)), show.legend = TRUE, alpha = 0.5, size=5)} +
          scale_color_viridis_d(name = "Years") +
          scale_x_continuous(labels = scales::comma, breaks = seq(0, 0.1, 0.0005)) +            
          scale_y_continuous(labels = scales::comma, breaks = seq(0, 0.1, 0.005)) +
          labs(x = "Number of prisoners", y = "Total crimes") +
          theme_minimal() +
          theme(legend.position = "bottom",
                text = element_text(size=20),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                panel.grid = element_line(size=1, colour="grey", linetype = "solid")),
          tooltip = "text") %>% layout(legend = list(orientation = "h",
                                                     x = 0, y = -0.2),
                                       height = 700)

      }
      else{
        ggplotly(ggplot(prison_ucr_choice,
               aes(x = prison,
                   y = prison_ucr_choice[,input$crime_type] %>% rowSums(),
                   text = paste("Year: ", as.factor(year), 
                                "<br>State: ", as.factor(jurisdiction),
                                "<br>Number of prisoners:", as.integer(prison) %>% comma(accuracy=1),
                                "<br>Total crimes:", (prison_ucr_choice[,input$crime_type] %>% rowSums()) %>% comma(accuracy=1)))) +
                   {if(input$legendChoice == "state") geom_point(aes(colour = as.factor(jurisdiction)), show.legend = TRUE, alpha = 0.5, size=5)} +
                   {if(input$legendChoice == "year") geom_point(aes(colour = as.factor(year)), show.legend = TRUE, alpha = 0.5, size=5)} +
          scale_color_viridis_d(name = "Years") +
          scale_y_continuous(labels = comma, breaks = seq(0, 5000000, 100000)) +
          scale_x_continuous(labels = comma, breaks = seq(0, 1000000, 10000)) +
          labs(x = "Number of prisoners", y = "Total crimes") +
          theme_minimal() +
          theme(legend.position = "bottom",
                text = element_text(size=20),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                panel.grid = element_line(size=1, colour="grey", linetype = "solid")),
          tooltip = "text") %>% layout(legend = list(orientation = "h",
                                                     x = 0, y = -0.2),
                                       height = 700)

    }

  })
  
  
}
)
