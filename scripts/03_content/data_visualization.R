################################################################################
# Shearwater Peregrine Dive Log Data

################################################################################
#
# Priya Schlenzka
# pxs1282@miami.edu
# October 28th, 2025
#
# Visualizing data previously cleaned in data-processing script
#
################################################################################

# SET UP #######################################################################

## Load packages ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(cowplot)

## Assign to object -------------------------------------------------------------------
read_rds("data/processed/Tidy_Dive_Computer_Data.rds")

# VISUALIZE ###################################################################

##Calculate how many dives per month
dives_by_month <- tidy_divecomp_data %>%
  mutate(year_month = floor_date(date, unit = "month")) %>%  # convert date to first day of month
  group_by(year_month) %>%
  summarize(num_dives = n(), .groups = "drop")  # remove grouping structure/information

##Dives shown by month
dives_by_month_plot <- ggplot(dives_by_month, aes(x = year_month, y = num_dives)) +
  geom_col(fill = "#5F9EA0") +
  labs(title = "Number of Dives per Month",
       x = "Month",
       y = "Number of Dives")

##Create Cowplot
##Panel 1: Dive time in relation to depth
p1 <- ggplot(tidy_divecomp_data, aes(x = dive_time, y = max_depth)) +
   geom_point(color = "#2E8B57",
              size = 2,
              alpha = 0.5) +
     labs(title = "Dive time vs. Maximum depth",
     x = "Dive time (minutes)",
    y = "Maximum depth (m)")

##Panel 2: Dive depth in relation to date
p2 <- ggplot(tidy_divecomp_data, aes(x = date, y = max_depth)) +
  geom_point(color = "#008080",
             size = 2,
             alpha = 0.7) +
  labs(title = "Date of dive vs. Maximum depth",
       x = "Date of dive",
       y = "Maximum depth (m)")

##Combining panels using cowplot
combined_plot <- cowplot::plot_grid(p1, p2, labels = c("A", "B"), ncol = 2)

# EXPORT #######################################################################

# Exporting figures

ggsave(filename = "results/img/dives_by_month.png",
  plot = dives_by_month_plot,
  width = 8, height = 5, dpi = 300)

ggsave(filename = "results/img/combined_dive_plots.png",
  plot = combined_plot,
  width = 12, height = 6, dpi = 300)
