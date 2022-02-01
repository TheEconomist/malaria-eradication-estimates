# Step 0. Import libraries and set defaults

library(tidyverse)
library(ggplot2)
library(anytime)
library(readxl)

# Step 1. Import data

# Most recent estimates of malaria burden by country (WHO):
burden <- read_xlsx("source-data/WMR2021_Annex5F.xlsx")

# Most populations by country (historical and projected - UN):
estimates <- read_xlsx("source-data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", 
                       sheet = 1)
projections <- read_xlsx("source-data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", 
                         sheet = 2) # We use the "medium variant" of population projections

# Estimates of work days lost to malaria
days_lost_per_case <- 3 # This is from researchers at LSHT: 

#####
# Quoting their email:
# 
# "...The number of work days lost per case are going to depend on: 1) How quickly/if treatment is received; 2) Age of person with malaria; 3) Whether it remains uncomplicated or progresses to severe; 4) various other factors, which will vary between individuals and between countries and contexts.
# 
# But, we could assume that:
#   
#  - 1 person per case loses productive time (either the adult with malaria, or one adult caring for one child with malaria – obviously, there is variation in this)
#  - 3 days (range: 1 to 7) lost per episode (taking into account a weighted average of uncomplicated and severe cases, and cases resulting in death; this is shorter than standard assumptions of duration of illness, but I think it’s reasonably and conservative to assume people return to work before they/their child are fully recovered)
# ...
# In addition, it may be worth having a look at some of the macroeconomic literature about the wider negative impacts of the presence of malaria on the economy. A human capital approach would also potentially account for the cumulative effects of loss of schooling and cognitive function on future productivity – but all of that is obviously even harder to quantify."
#####

# Comparable numbers:

# Source: https://data.oecd.org/emp/labour-force.htm#indicator-chart
sweden_labor_force <- 5522000
sweden_annual_hours_per_worker <- 1424
us_labor_force <- 161204000 
us_annual_hours_per_worker <- 1767  
eu_labor_force <- 212287000
eu_annual_hours_per_worker <- 1513

