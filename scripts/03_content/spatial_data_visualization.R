################################################################################
# Shearwater Peregrine Dive Log Data

################################################################################
#
# Priya Schlenzka
# pxs1282@miami.edu
# November 10th, 2025
#
# Working with dive computer data to create spacial representation of dive site
# locations around the globe
#
################################################################################

  # SET UP #######################################################################

  ## Load packages ---------------------------------------------------------------

library(ggplot2)       # plot the data
library(ggspatial)     # add map elements to a ggplot
library(rnaturalearth) # country outlines
library(sf)            # vector data
library(mapview)       # quickly inspect data
library(terra)         # raster data
library(tidyterra)     # bridge raster and ggplot

  ## Load data -------------------------------------------------------------------

depth <- rast("data/raw/depth_raster.tif")

world <- ne_countries(scale = "small", returnclass = "sf")

 ## Peak at the framework
plot(depth)
plot(world, add = TRUE)

  # PROCESSING ###################################################################

##Create bounding boxes for regular dive areas holding multiple sites/dives
monterey_box <- st_as_sfc(st_bbox(c(xmin = -122.1, xmax = -121.7,
                                 ymin = 36.5, ymax = 36.7),
                               crs = 4326)) |>
  st_as_sf(dive_location = "Monterey")

gili_box <- st_as_sfc(st_bbox(c(xmin = 116.00, xmax = 116.15,
                                 ymin = -8.40, ymax = -8.30),
                               crs = 4326)) |>
  st_as_sf(dive_location = "Gili Islands")

boxes <-bind_rows (monterey_box, gili_box)

  # VISUALIZE ####################################################################

  ## Create the plot
regular_dive_areas <- ggplot() + geom_spatraster(data = depth) +
  scale_fill_viridis_c(option = "mako", name = "Ocean Depth (m)") +
  geom_sf(data = world, fill = "grey", color = "grey") +
  geom_sf(data = st_centroid(boxes), mapping = aes(color = dive_location),
          size = 4) +
  scale_color_discrete(name = "Dive Areas") +
  ggtitle("Regular Dive Areas") +
  theme(plot.title = element_text(face = "bold")) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "br") +
    labs(caption = "P.S. Dive Computer Data")

  # EXPORT #######################################################################

  ## Figure(s) are exported to the results/img folder. as .png
  ggsave("results/img/regular_dive_areas.png",
         plot = regular_dive_areas,
         width = 10, height = 8, dpi = 300)
