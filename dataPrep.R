packages_vector = c("shiny", "shinythemes", "dplyr", "panelr", "viridis", "ggplot2", "readr", "ggrepel", "scales", "spData")

package.check <- lapply(packages_vector, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

remove(packages_vector)
remove(package.check)

library(shiny)
library(shinythemes)
library(dplyr)
library(panelr)
library(viridis)
library(ggplot2)
library(readr)
library(ggrepel)
library(scales)
library(spData)
library(sf)

# setwd("/srv/shiny-server/myapp")
setwd("/home/ewaewaewa/Documents/RR - shiny/Reproducible-Research-project/")
# setwd("/Users/ewelinka/Desktop/RR_app/Reproducible-Research-project")

#### UCR PREPROCESSING
ucr <- read_csv("data/ucr_by_state.csv")
ucr$year <- as.factor(ucr$year)
ucr <- ucr[, -c(16:21)]
ind <- apply(ucr, 1, function(x) all(is.na(x)))
ucr <- ucr[ !ind, ]
remove(ind)
ucr$jurisdiction[ucr$jurisdiction=="DC"] <- "District of Columbia"
ucr <- ucr %>% filter(jurisdiction!="Puerto Rico")
ucr$rape_legacy <- NULL
ucr$rape_revised <- NULL

states_vector <- ucr$jurisdiction %>% na.omit() %>% unique()
region_vector <- ucr$region %>% na.omit() %>% unique()

#### NUMBER OF PRISONERS

prison <- read_csv("data/prison_custody_by_state.csv")
colnames(prison)[3:18] <- paste0(colnames(prison)[3:18],'1')
prison <- long_panel(prison, begin = 2001, end = 2016, label_location = "beginning", id = "jurisdiction")
names(prison)[names(prison) == "wave"] <- "year"
names(prison)[names(prison) == "1"] <- "prison"
prison$year <- as.factor(prison$year)

prison_ucr = inner_join(prison, ucr, by=c("jurisdiction", "year"))
prison_ucr$violent_crime_total_per_pop <- prison_ucr$violent_crime_total/prison_ucr$state_population
prison_ucr$property_crime_total_per_pop <- prison_ucr$property_crime_total/prison_ucr$state_population
prison_ucr$prisoners_per_pop <- prison_ucr$prison/prison_ucr$state_population

#### DUMBBELL PLOT

us_states_info <- data.frame(jurisdiction = us_states$NAME, 
                             region = us_states$REGION,
                             area_km2 = as.numeric(round(us_states$AREA, 0)))

additional_states <- data.frame(jurisdiction = c("Hawaii", "Alaska"),
                                region = c("remote", "remote"),
                                area_km2 = c(16638, 1481346))

us_states_info <- rbind(us_states_info, additional_states)

prison_data <- prison %>% filter(year==2001 | year==2016) %>% 
  select(jurisdiction, year, prison)

data_prep <- left_join(ucr, us_states_info, by = "jurisdiction")
data_prep1 <- left_join(data_prep, prison_data, by = c("jurisdiction", "year"))

v1 <- data_prep1 %>% 
  filter(year==2001) %>% 
  mutate(violent_crime_total_rate_2001 = violent_crime_total/state_population,
         property_crime_total_rate_2001 = property_crime_total/state_population,
         imprisonment_rate_2001 = prison/state_population) %>% 
  select(jurisdiction, 
         violent_crime_total_rate_2001, 
         property_crime_total_rate_2001, 
         imprisonment_rate_2001)  

v2 <- data_prep1 %>% 
  filter(year==2016) %>% 
  mutate(violent_crime_total_rate_2016 = violent_crime_total/state_population,
         property_crime_total_rate_2016 = property_crime_total/state_population,
         imprisonment_rate_2016 = prison/state_population) %>% 
  select(jurisdiction, 
         violent_crime_total_rate_2016, 
         property_crime_total_rate_2016,
         imprisonment_rate_2016)

final_data <- left_join(v1, v2, by = "jurisdiction")

violent <- final_data %>%
                    dplyr::select(jurisdiction, violent_crime_total_rate_2001, violent_crime_total_rate_2016) %>%
                    dplyr::arrange(desc(violent_crime_total_rate_2001))

violent$jurisdiction <- factor(violent$jurisdiction, levels=as.character(violent$jurisdiction))
violent$pct_change_violent <- (violent$violent_crime_total_rate_2016 - violent$violent_crime_total_rate_2001)/violent$violent_crime_total_rate_2001*100
violent$violent_crime_total_rate_2001 <- round(violent$violent_crime_total_rate_2001, 4)
violent$violent_crime_total_rate_2016 <- round(violent$violent_crime_total_rate_2016, 4)

property <- final_data %>%
                    dplyr::select(jurisdiction, property_crime_total_rate_2001, property_crime_total_rate_2016) %>%
                    dplyr::arrange(desc(property_crime_total_rate_2001))

property$jurisdiction <- factor(property$jurisdiction, levels=as.character(property$jurisdiction))
property$pct_change_property <- (property$property_crime_total_rate_2016 - property$property_crime_total_rate_2001)/property$property_crime_total_rate_2001*100
property$property_crime_total_rate_2001 <- round(property$property_crime_total_rate_2001, 4)
property$property_crime_total_rate_2016 <- round(property$property_crime_total_rate_2016, 4)

imprisonment <- final_data %>%
                    dplyr::select(jurisdiction, imprisonment_rate_2001, imprisonment_rate_2016) %>%
                    dplyr::arrange(desc(imprisonment_rate_2001))

imprisonment$jurisdiction <- factor(imprisonment$jurisdiction, levels=as.character(imprisonment$jurisdiction))
imprisonment$pct_change_imprisonment <- (imprisonment$imprisonment_rate_2016 - imprisonment$imprisonment_rate_2001)/   imprisonment$imprisonment_rate_2001*100
imprisonment$imprisonment_rate_2001 <- round(imprisonment$imprisonment_rate_2001, 4)
imprisonment$imprisonment_rate_2016 <- round(imprisonment$imprisonment_rate_2016, 4)

remove(final_data, v1, v2, us_states_info, data_prep, data_prep1, additional_states, prison_data, prison)

save.image("dataPrep.RData")

