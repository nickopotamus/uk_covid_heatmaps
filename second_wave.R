### Set up
rm(list = ls())
gc()

library(ukcovid19)
library(tidyverse)
library(tsibble)
library(lubridate)
#library(zoo)

### Get data

# Filters
all_countries <- c('areaType=nation')
all_uk <- c('areaType=overview')

# Structure
query_structure = list(
  date = "date",
  areaName = "areaName",
  newCasesByPublishDate = "newCasesByPublishDate",
  cumCasesByPublishDate = "cumCasesByPublishDate",
  cumCasesByPublishDateRate = "cumCasesByPublishDateRate",
  newDeaths28DaysByPublishDate = "newDeaths28DaysByPublishDate",
  cumDeaths28DaysByPublishDate = "cumDeaths28DaysByPublishDate",
  cumDeaths28DaysByPublishDateRate = "cumDeaths28DaysByPublishDateRate",
  newTestsByPublishDate = "newTestsByPublishDate", 
  newPillarOneTestsByPublishDate = "newPillarOneTestsByPublishDate",
  newPillarTwoTestsByPublishDate = "newPillarTwoTestsByPublishDate",
  newAdmissions = "newAdmissions",
  cumAdmissions =  "cumAdmissions",
  hospitalCases = "hospitalCases",
  covidOccupiedMVBeds = "covidOccupiedMVBeds"
)
# Comment out lines not needed
# nb newAdmissions 19/3 onwards, MVBeds 2/4 onwards
# Some metrics are not available for specific areaType values. 
#   nation - ByPublishDate 
#   region, utla, and ltla - BySpecimenDate

# Call API
raw_data <- get_data(filters = all_uk, structure = query_structure)

full_data <- raw_data %>% 
  as_tibble() %>% 
  mutate(areaName = as.factor(areaName), date=parse_date(date)) %>%
  filter(date >= '2020-03-01') %>% 
  mutate(cases_per_test = newCasesByPublishDate/newTestsByPublishDate) %>% 
  as_tsibble(key = areaName, index = date)

# Extract useful features
regions = unique(full_data$areaName)

# Get timestamp
timestamp <- last_update(filters = all_uk, structure = query_structure)
datestamp = as_date(timestamp)

### Data of interest

# Divide data into regions
for (region in regions){
  nam = paste("data_", region, sep = "")
  data_region = full_data %>% filter(areaName == region)
  assign(nam, data_region)
}

# Graphs
ggplot(full_data, aes(x=date, y=hospitalCases)) +
            geom_line() + xlab("")

# Cases per testing
# Cases: All +ve PCR (so not pillar 3)
# Cases in England: Pillar 1 until 2/7, then pillar 1 and 2. 15841 cases 25/9-2/10 missed, added to 3+4/10
# Testing in England: Available from 1/4, pillar 2 from 14/7

# Maths to generate dual axis plot
ylim.prim <- c(0, max(full_data$newPillarTwoTestsByPublishDate, na.rm=T))
ylim.sec <- c(0, max(full_data$cases_per_test, na.rm=T))
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

testing <- full_data %>% 
  select(date, 
         newCasesByPublishDate, 
         newPillarOneTestsByPublishDate, 
         newPillarTwoTestsByPublishDate,
         cases_per_test) %>% 
  mutate(casesPerTest = a + cases_per_test*b) %>% 
  select(-cases_per_test) %>% 
  gather('newCasesByPublishDate', 'newPillarOneTestsByPublishDate', 'newPillarTwoTestsByPublishDate', 'casesPerTest',
         key = "category", value = "total")


  testing$category = factor(testing$category, 
                           levels = c('newCasesByPublishDate', 
                                      "newPillarOneTestsByPublishDate", 
                                      "newPillarTwoTestsByPublishDate", 
                                      "casesPerTest"))

ggplot() + 
  geom_line(data = testing, aes(x = date, y = total, col = category)) +
  xlab('Months of 2020') +
  scale_y_continuous('Number of tests/cases',
                     sec.axis = sec_axis(~ (. - a)/b, name = "Positive cases per test")) +
  labs(title = 'Is there a second wave? Cases vs tests through time',
       subtitle = sprintf("Visualisation by @Nickopotamus. Data: gov.uk. Last updated: %s", datestamp)) +
  theme_light() +
  theme(legend.title = element_blank(), legend.position = c(0.5, 0.8)) +
  scale_color_manual(labels = c("Daily new cases", "Daily pillar 1 tests", "Daily pillar 2 tests", "Cases/test"),
                     values = c("red", "blue", "green", "black")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")

cases <- full_data %>% 
  select(date, 
         hospitalCases, 
         covidOccupiedMVBeds, 
         newDeaths28DaysByPublishDate) %>% 
  gather('hospitalCases', 'covidOccupiedMVBeds', 'newDeaths28DaysByPublishDate',
         key = "category", value = "total")


cases$category = factor(cases$category, 
                          levels = c('hospitalCases', 
                                     "covidOccupiedMVBeds",
                                     "newDeaths28DaysByPublishDate"))




ggplot() + 
  geom_line(data = cases, aes(x = date, y = total, col = category)) +
  xlab('Months of 2020') +
  scale_y_continuous('Daily caseload' ) +
  labs(title = 'Is there a second wave? Types of cases through time',
       subtitle = sprintf("Visualisation by @Nickopotamus. Data: gov.uk. Last updated: %s", datestamp)) +
  theme_light() +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.8)) +
  scale_color_manual(labels = c("Cases in hospital", "Ventilated in hospital", "Deaths"),
                     values = c("green", "blue", "red")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")

