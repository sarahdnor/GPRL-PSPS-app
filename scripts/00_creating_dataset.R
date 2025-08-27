# loading packages
library(haven)  # for reading .dta
library(readr)  # for writing .csv
library(here)
library(dplyr)
library(tidyverse)

# loading/writing data
work_income_data <- read_dta("data/dataset_3.dta")
write_csv(work_income_data, "data/work_income_data.csv")

demographic_data <- read_dta("data/dataset_1.dta")
write_csv(demographic_data, "data/demographic_data.csv")

# reading in data
demographic_data <- read_csv("data/demographic_data.csv")
work_income_data <- read_csv("data/work_income_data.csv")

# combining datasets
PSPS <- full_join(work_income_data, demographic_data, by = c("caseid", "memid")) |>
  relocate(caseid, memid)

# saving data
save(PSPS, file = here("data/PSPS.rda"))


# can be looked at by gender
# age

#Faceted bar chart by nowork_reason - change valueing labeling
#bar chart faceted by nowork_reason
#Pie or bar chart showing proportion with ag vs. non-ag income: earn_inc_ag vs earn_inc_nonag
#Stacked bar by gender or region (if available) showing ag vs. non-ag work

#Histogram of agwork_hr_1 and nonagwork_hr_1 (hours worked)
#Boxplot of work hours grouped by sector (ag/non-ag)

#comparing agricultural jobs to non agricultural jobs
#Bar chart: most common govt_type sources
#Boxplot of total amount (govt_tot_cash, govt_tot_inkind)

#income distribution - log transform - do by non agri and agri jobs


#unearn_fremit_yn, unearn_dremit_yn, unearn_gift_yn, unearn_govt_yn - look at unearned income earning faceted - where are people getting support that arent working - are they working

#work_any_30d, earn_inc_ag


#agwork_prod

#agwork_dy days a week for ppl that 

data_2 |>
  select(nowork_reason)

