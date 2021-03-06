# Crimes in USA - shiny app

## TL;DR

Shiny application address: http://35.153.18.124:3838/myapp/ or https://tinyurl.com/crimeshinyapp

## About the app

**Files**

- `ui.R`: ui for shiny application  
- `server.R`: server for shiny application  
- `index.Rmd`: description of project in shiny markdown version
- `README.md`: description of project as github README
- `dataPrep.R`: code used for preprocesing data  
- `dataPrep.RData`: RData with preprocessed data from dataPrep.R  
- `data/prison_custody_by_state.csv`: Incarcerations in prison dataset  
- `data/ucr_by_state.csv`: UCR dataset
 
**Packages**

The application uses the following packages : 
[`shiny`](https://shiny.rstudio.com/), 
[`shinythemes`](https://rstudio.github.io/shinythemes/), 
[`dplyr`](https://github.com/tidyverse/dplyr),
[`scales`](https://github.com/r-lib/scales),
[`viridis `](https://ggplot2.tidyverse.org/reference/scale_viridis.html),
[`ggplot2`](https://ggplot2.tidyverse.org/),
[`ggrepel`](https://www.rdocumentation.org/packages/ggrepel/versions/0.7.0),
[`sf`](https://cran.r-project.org/web/packages/sf/index.html),
[`spData`](https://cran.r-project.org/web/packages/spData/index.html),
[`readr`](https://readr.tidyverse.org/),
[`shinycssloaders`](https://github.com/daattali/shinycssloaders),
[`plotly`](https://plotly.com/)

## App functionality


**Crimes on map**

In this tab can be seen maps of US states divided by the severity of a crime, that is violent compared to property crimes.

The following parameters can be specified:

 - Year range for which mean value per region is calculated
 - Crime type as radiobutton
 - Regions as multiselect (map dynamically adjusted to regions)
 - Download option of plot

**Dumbbell plot**

In this tab can be seen analysis of percentage change of violent crime rates, property crime rates and imprisonment rates between 2001 and 2016. 

The following parameters can be specified:

- Data type
- States as multiselect

Additional information:

- Plot height adjusts to number of chosen states
- At least 3 states have to be chosen for the plot to show

**Scatterplot**

In this tab can be seen the analysis examining the relation between incarceration and crime rates.

The following parameters can be specified:

- Year range
- Checkbox if data per capita
- Crime type as multiselect
- States as multiselect
- Legend and color choice

Additional information: 

- Possibility of selecting all crime types

**Data**

In this tab can be seen a tabular version of data on different types of crimes committed in different states across years.

## About the analysis

### Introduction

The main objective of this project is to review general crime and incarceration trends in the United States and also examine the relationships in data regarding crimes committed and number of prisoners across time and states. 

### Data description and preprocessing

The datasets used in the analysis were sourced from www.kaggle.com website <sup>[1](#kaggle)</sup>. They were created based on several sources including the Bureau of Justice Statistics <sup>[2](#bjs)</sup> and FBI Uniform Crime Reporting Program <sup>[2](#fbi)</sup>. The National Prisoner Statistics Program conducted by the Bureau of Justice Statistics has collected data on the number of prisoners in state and federal prison facilities since 1926. It is produced annually on national and state level. Data are sourced from 50 state departments of correction, the Federal Bureau of Prisons, and until 2001, from the District of Columbia. The UCR Program provides statistics on violent and property crimes. Data are collected annually and are available on national, state and city level. For the purposes of our analysis we are using state-level statistics.

**UCR**

The UCR dataset consist of 15 variables, two of which are the jurisdiction and year of the observation. It provides information about the state population and also about number of violent crimes (murder, manslaughter, rape, robbery, aggravated assault) and property crimes (burglary, larceny, vehicle theft) per state yearly. Detailed definitions of each crimes can be found on UCR Program website.

**Incarcerations in prison**

The prison data, compared to ucr is in a panel form, consisting of years as columns from 2001 to 2016. Using long_panel, we converted the dataframe so that each row is a different jurisdiction and year. The dataset includes also information indicating whether the number of prisoners also includes jails population.

**State area and region**

In order to enhance further visualizations, we add an information about state area and region based on R built-in *us_states* dataset


<a name="kaggle">1</a>: Source: https://www.kaggle.com/christophercorrea/prisoners-and-crime-in-united-states

<a name="bjs">2</a>: Source: https://www.bjs.gov/index.cfm?ty=dcdetail&iid=269.

<a name="fbi">3</a>: Source: https://www.ucrdatatool.gov/Search/Crime/State/RunCrimeStatebyState.cfm.
