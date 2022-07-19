library(tidyverse)

valid_units <- read_tsv('Data/included-units_2Dec2021.txt', col_names = c('UnitName'))
spike_data <- read_csv('Data/all-spike-data.csv')
pleas_data <- read_csv('Data/all_pleasantness_data.csv')

# units we have that should be excluded
setdiff(
  unique(spike_data$UnitName),
  valid_units$UnitName
  )

# units we don't have that we should find
setdiff(
  valid_units$UnitName,
  unique(spike_data$UnitName)
)

