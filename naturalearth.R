#Natural Earth

library(ncdf4)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(ggspatial)
library(tibble)
library(dplyr)

# Set the path to the NetCDF file and open it
nc_path <- "dataraw/cbefs_2024_v20251101.nc"
nc <- nc_open(nc_path)

# Extract 2D geographic coordinate matrices
lon_matrix <- ncvar_get(nc, "longitude")
lat_matrix <- ncvar_get(nc, "latitude")


temp_surface <- ncvar_get(
  nc,
  varid = "temperature",
  start = c(1, 1, 2, 1),
  count = c(-1, -1, 1, 365)
)
  
nc_close(nc)

#Compute the pixel-wise annual mean grid across 365 days
temp_surface_mean <- apply(temp_surface, c(1, 2), mean, na.rm = TRUE)

#Format the grid into a tidy spatial object (sf) in the global standard geographic coordinate reference system WGS84 (EPSG:4326)
temp_surface_mean_sf <- tibble(
  Longitude   = as.vector(lon_matrix),
  Latitude    = as.vector(lat_matrix),
  Temperature   = as.vector(temp_surface_mean)
) |>
  filter(!is.na(Temperature)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#Download the public domain land background (US States from Natural Earth)
land_bg <- ne_states(country = "United States of America", returnclass = "sf")

#Build a map with background land overlay, scale bar, and north arrow. Note that if using the map in a journal article, the title and subtitle text should be moved to the figure caption
ggplot() +
  # Land layer
  geom_sf(data = land_bg, fill = "grey90", color = "grey65", linewidth = 0.3) +
  
  # CBEFS temperatures
  geom_sf(data = temp_surface_mean_sf, aes(color = Temperature), size = 0.5) +
  scale_color_viridis_c(option = "plasma", name = "Mean\ntemperature\n(°C)") +
  
  # Set map spatial limits to Chesapeake Bay region
  coord_sf(
    xlim = c(-77.5, -75.3),
    ylim = c(36.8, 39.7),
    crs = 4326,
    expand = FALSE
  ) +
  
  # Map scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    unit_category = "metric"
  ) +
  
  # North pointer
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering()
  ) +
  
  labs(
    title = "Chesapeake Bay 2024 mean surface water temperature",
    subtitle = "CBEFS model output with Natural Earth public domain land boundaries",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey85", linetype = "dotted"),
    panel.background = element_rect(fill = "aliceblue"), # water backdrop for unmodeled ocean
    legend.position = "right"
  )
