packages_vector = c("shiny", "shinythemes", "dplyr", "panelr", "viridis", "ggplot2", "readr", "ggrepel", "scales")

package.check <- lapply(packages_vector, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

#setwd("/srv/shiny-server/myapp")
setwd("/Users/ewelinka/Desktop/RR_app/Reproducible-Research-project")

#### UCR PREPROCESSING
ucr <- read_csv("data/ucr_by_state.csv")
ucr$year <- as.factor(ucr$year)
ucr <- ucr[, -c(16:21)]
ind <- apply(ucr, 1, function(x) all(is.na(x)))
ucr <- ucr[ !ind, ]
ucr$jurisdiction[ucr$jurisdiction=="DC"] <- "District of Columbia"
ucr <- ucr %>% filter(jurisdiction!="Puerto Rico")
ucr$rape_legacy <- NULL
ucr$rape_revised <- NULL

states_vector <- ucr$jurisdiction %>% na.omit() %>% unique()

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


#### STATE GEOMETRY
library(spData)
ucr_state <- ucr %>% group_by(jurisdiction)
names(ucr_state)[names(ucr_state) == "jurisdiction"] <- "NAME"
us_states_ucr <- merge(us_states, ucr_state, by = "NAME")

ucr_grouped <- ucr %>% 
  filter(region %in% c("MidWest", "SouthWest")) %>% 
  filter(as.numeric(year) <= 2008)%>%
  filter(as.numeric(year) >= 2005)%>%
  group_by(jurisdiction) 

sapply(ucr_grouped[,6:13], function(x) {mean(x)})



ucr_grouped <- ucr %>% 
  filter(region %in% c("MidWest", "SouthWest")) %>% 
  filter(as.numeric(year) <= 2008)%>%
  group_by(jurisdiction) %>% 
  summarise(robbery_mean = mean(robbery))

#rename variable for merging
names(ucr_grouped)[names(ucr_grouped) == "jurisdiction"] <- "NAME"
#merge grouped ucr and state spatial data
library(spData)
us_states_ucr <- merge(us_states, ucr_grouped, by = "NAME")

ggplot(data = us_states_ucr) +
  geom_sf(aes(fill = robbery_mean), lwd = 0, color = "white") +
  scale_fill_viridis_c(option = "viridis", trans = "sqrt", name = "Property crimes\nper population") +
  theme(legend.position = "none") +
  theme_minimal()


