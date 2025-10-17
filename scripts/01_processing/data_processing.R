################################################################################
# Shearwater Peregrine Dive Log Data

################################################################################
#
# Priya Schlenzka
# pxs1282@miami.edu
# October 7th, 2025
#
# Manipulating dive computer data to calculating the total bottom time, average
# bottom time, and number of dives.
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
library(EVR628tools)
library(tidyverse)
library(janitor)

## Load data -------------------------------------------------------------------
divecomp_data <- read_csv(file = "data/raw/Dive_Log.csv")

# TIDYING/WRANGLE ###################################################################
#Tidy data removed columns, made names into snake case, converted seconds to minutes,
#removed freediving/short dives, converted depth to metric where in imperial,
#removed imperial column, and reordered columns
tidy_divecomp_data <- divecomp_data %>%
  select(1, 5, 8, 13, 14, 18) %>%
  clean_names() %>%
  mutate(max_time = max_time/60) %>%
  filter(max_time >=  3) %>%
  mutate(max_depth = if_else(imperial_units, max_depth * 0.3048, max_depth)) %>%
  select(-imperial_units) %>%
  select(dive_number, start_date, end_date, max_time, max_depth)

tidy_divecomp_data

# ANALYSIS #####################################################################

# Calculating total bottom time in hours
total_max_time <- tidy_divecomp_data %>%
  summarize(total_max_time = sum(max_time)) |>
  mutate(total_max_time = total_max_time/60)

# Calculate total number of dives
total_dives <- tidy_divecomp_data %>%
  summarize(total_dives = n())

# Calculate average time spent per dive
tidy_divecomp_data |>
  summarize(avg_time_per_dive = total_max_time / total_dives)

# Calculate average depth per dive
tidy_divecomp_data |>
  summarize(avg_depth_per_dive = mean(max_depth))

# EXPORT #######################################################################

# Tabular representation of data set

write_rds(tidy_divecomp_data, "data/processed/Tidy_Dive_Computer_Data.rds")
read_rds("data/processed/Tidy_Dive_Computer_Data.rds")
