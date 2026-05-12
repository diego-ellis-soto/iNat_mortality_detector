# NEXT STEPS: IDENTIFY WHICH COUNTIES HAVE THE MOST MORTALITY RECORDS AND IF THERE IS A CORRELATION BETWEEN DENSITY, INCOME, POPULATION DENSITY, ETC

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#
# Global, near real-time ecological forecasting of mortality events through participatory science
#
# Author: Diego Ellis Soto
# Department of Environmental Science Policy & Management, University of California, Berkeley, USA
# diego.ellissoto@berkeley.edu

#
#
# Downloaded Mortality records across California for 2022-2025 on February 15th 2025
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

library(tidyverse)
library(ggplot2)
library(scales)
library(forcats)
library(sf)
library(raster)
library(units)
library(tidycensus)
library(tigris)
library(mapview)
library(viridis)
library(cowplot)
library(patchwork)
library(gridExtra)
library(ggimage)
library(rnaturalearth)
library(rnaturalearthdata)
library(arrow)
library(treemapify)
library(tidygraph)
library(ggraph)
library(ineq)
library(sp)
require(tigris)
options(tigris_use_cache = TRUE)
census_api_key('88f3fbb46861a0190c673e39c99bd3c067f15072', install = TRUE, overwrite = TRUE)
# https://gis.stackexchange.com/questions/119993/convert-line-shapefile-to-raster-value-total-length-of-lines-within-cell
# https://catalog.data.gov/dataset/enviroatlas-road-density-metrics-by-12-digit-huc-for-the-conterminous-united-states3

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Figure 2 California ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Download road data for counties in California
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

ca_counties <- tigris::counties(state = "CA", year = 2022)
county_names <- ca_counties$NAME

# Get roads from California
california_roads <- roads(state = "CA",county = county_names, year = 2022)

# LA = c('Orange', 'Los Angeles', 'San Bernardino')
# california_roads <- roads(state = "CA",county = LA, year = 2022)

# save(california_roads, file = 'outdir/roads_cali.Rdata')
# mapview(california_roads)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# California
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Get the spatial boundary for California
california <- states(year = 2022) %>%
  filter(NAME == "California") %>%
  st_transform(3310)  # Keep in lat/lon (same as inat_sf)
# save(california, file = 'outdir/california.Rdata')

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# iNaturalist
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Step 1: Read your CSV
# inat <- read.csv("/Users/diegoellis/Downloads/inat_dead_ALL_2025-02-13 (2).csv", 
#                  stringsAsFactors = FALSE)

inat <- read.csv("indir/California_inat_dead_ALL_2025-02-15.csv", 
                 stringsAsFactors = FALSE)

# Resubmission:
inat <- read.csv('indir/resubmission_case_studies/California_mammals_inat_dead_filtered_2026-04-11.csv', 
                 stringsAsFactors = FALSE)

inat <- read.csv('indir/resubmission_case_studies/California/California_mammals_v2_inat_dead_filtered_2026-04-15.csv', 
                 stringsAsFactors = FALSE)

inat <- read.csv('indir/resubmission_case_studies/California/California_mammals_v3_2026-04-15.csv', 
                 stringsAsFactors = FALSE)



range(inat$time_observed_at, na.rm=T)

inat_sf <- inat %>%
  filter(!is.na(.data$location)) %>%
  separate(.data$location, into = c("latitude", "longitude"), sep = ",", convert = TRUE) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# # Step 2: Parse lat/lon from the "location" column
# inat_sf <- inat %>%
#   # Drop rows with no location
#   filter(!is.na(location)) %>%
#   # Split "location" into two new columns: "latitude" and "longitude"
#   separate(location, into = c("latitude", "longitude"), sep = ",", convert = TRUE) %>%
#   # Convert to sf points (coords = c("x", "y") => c("longitude", "latitude"))
#   # Drop rows with no location
#   filter(!is.na(location)) %>%
#   # Split "location" into two new columns: "latitude" and "longitude"
#   separate(location, into = c("latitude", "longitude"), sep = ",", convert = TRUE) %>%
#   # Convert to sf points (coords = c("x", "y") => c("longitude", "latitude"))
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
# # 4326 = WGS84 (lat/long)

# Reproject roads to EPSG:3310
california_roads_3310 <- st_transform(california_roads, 3310)

# Reproject inat points
inat_sf_3310 <- st_transform(inat_sf, 3310)

# Intersection:
inat_sf_3310_cali_only = inat_sf_3310 |> st_intersection(california)

# Convert sf object to a dataframe for ggplot binning
inat_df <- as.data.frame(st_coordinates(inat_sf_3310_cali_only))

# Hexbin visualization

sampling_across_cali = ggplot() +
  geom_sf(data = california, fill = "lightgray", color = "black") +  # Base California map
  stat_bin_hex(
    data = inat_df,
    aes(X, Y),
    bins = 50,  # Adjust for desired granularity
    color = "black",
    alpha = 0.8
  ) +
  scale_fill_viridis_c(option = "plasma", name = "Number of samples") +  # Color scale
  labs(
    title = "iNaturalist Observations in California (Hexbins)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()+
  theme_classic() + ylab('Longitude') + xlab('Latitude') +
  theme(axis.text.x = element_text(face = "bold", size = 16 ,color='black'),
        axis.title.x = element_text(face = "bold", size = 16 ,color='black'),
        axis.text.y = element_text(face = "bold", size = 16 ,color='black'),
        axis.title.y = element_text(face = "bold", size = 16 ,color='black'))
ggsave(sampling_across_cali, file = 'outdir/resubmission_mortality_sampling_cali.png')

# --- --- --- --- --- --- --- --- --- --- ---
# Distance to nearest road:
# --- --- --- --- --- --- --- --- --- --- ---

# For each point, get the index of the nearest road
nearest_idx <- st_nearest_feature(inat_sf_3310_cali_only, california_roads_3310)

# Extract geometry of the nearest roads
nearest_roads <- st_geometry(california_roads_3310)[nearest_idx]

# Calculate distance for each point to its nearest road (in meters)
inat_sf_3310_cali_only$dist_to_road_m <- st_distance(inat_sf_3310_cali_only, nearest_roads, by_element = TRUE)

# Ensure 0 has same unit as dist_to_road_m
inat_sf_3310_cali_only$dist_to_road_m = as.numeric(inat_sf_3310_cali_only$dist_to_road_m)

dist2road = ggplot(inat_sf_3310_cali_only, aes(x = (dist_to_road_m))) +
  geom_histogram(aes(y = ..density..), 
                 bins = 100, 
                 fill = "#0072B2", 
                 color = "black", 
                 alpha = 0.7) +
  # geom_density(color = "red") +  # Overlay density plot
  labs(
    title = "Density Histogram of Distance to Road",
    x = "Distance to \n Nearest Road km",
    y = "Density"
  ) +
  theme_classic() + ylab('Sampling density') + xlab('Distance to road') +
  theme(axis.text.x = element_text(face = "bold", size = 16 ,color='black'),
        axis.title.x = element_text(face = "bold", size = 16 ,color='black'),
        axis.text.y = element_text(face = "bold", size = 16 ,color='black'),
        axis.title.y = element_text(face = "bold", size = 16 ,color='black'))+ # +
  xlim( c(as.numeric(0), as.numeric(round(quantile(inat_sf_3310_cali_only$dist_to_road_m, 0.99)))  ) )+
  xlim(0, 1000)
# theme(legend.position="none") # Remove legend

ggsave(dist2road, file = 'outdir/resubmission_dist2road_cali.png')


# total number of records
n_total <- nrow(inat_sf_3310_cali_only)

# % within thresholds
pct_10m  <- mean(inat_sf_3310_cali_only$dist_to_road_m <= 10) * 100
pct_100m  <- mean(inat_sf_3310_cali_only$dist_to_road_m <= 100) * 100
pct_250m  <- mean(inat_sf_3310_cali_only$dist_to_road_m <= 250) * 100
pct_500m  <- mean(inat_sf_3310_cali_only$dist_to_road_m <= 500) * 100
pct_1000m <- mean(inat_sf_3310_cali_only$dist_to_road_m <= 1000) * 100
n_100m <- sum(inat_sf_3310_cali_only$dist_to_road_m <= 100)
quantile(inat_sf_3310_cali_only$dist_to_road_m, probs = c(0.25, 0.5, 0.75, 0.9))

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Human modification
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

human_mod_americas_masked = raster('indir/hmod_americas_masked.tif')

# If different, transform points to match raster CRS
if (st_crs(inat_sf_3310_cali_only) != st_crs(human_mod_americas_masked)) {
  inat_california_3310 <- st_transform(inat_sf_3310_cali_only, crs(human_mod_americas_masked))
}

inat_california_3310$human_mod <- raster::extract(human_mod_americas_masked, st_coordinates(inat_california_3310))

hmod_space_cali = ggplot() +
  geom_sf(data = california, fill = "lightgray", color = "black") +
  geom_sf(data = inat_california_3310, aes(color = human_mod), size = 2) +
  scale_color_viridis_c() +
  labs(title = "iNaturalist Points with Extracted Human Modification Values") +
  theme_minimal()+
  theme(axis.text.x = element_text(face = "bold", size = 16 ,color='black'),
        axis.title.x = element_text(face = "bold", size = 16 ,color='black'),
        axis.text.y = element_text(face = "bold", size = 16 ,color='black'),
        axis.title.y = element_text(face = "bold", size = 16 ,color='black'))
ggsave(hmod_space_cali, file = 'outdir/resubmission_hmod_space_cali.png')


hmod_hist_cali = ggplot(inat_california_3310, aes(x = human_mod)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10, 
                 fill = "#0072B2", 
                 color = "black", 
                 alpha = 0.7) +
  # geom_density(color = "red") +  # Overlay density plot
  labs(
    title = "Density Histogram of Human Modification",
    x = "Human Landscape Modification",
    y = "Density"
  ) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(face = "bold", size = 16 ,color='black'),
        axis.title.x = element_text(face = "bold", size = 16 ,color='black'),
        axis.text.y = element_text(face = "bold", size = 16 ,color='black'),
        axis.title.y = element_text(face = "bold", size = 16 ,color='black'))
ggsave(hmod_hist_cali, file = 'outdir/resubmission_hmod_hist_cali_cali.png')

sampling_across_cali + hmod_hist_cali / dist2road

dist2road_ecdf <- ggplot(inat_sf_3310_cali_only, aes(x = dist_to_road_m)) +
  stat_ecdf(geom = "step", linewidth = 1.2, color = "#0072B2") +
  geom_vline(xintercept = 100, linetype = "dashed", color = "red", linewidth = 1) +
  annotate(
    "text",
    x = 120,
    y = 0.2,
    label = paste0(
      round(mean(inat_sf_3310_cali_only$dist_to_road_m <= 100, na.rm = TRUE) * 100, 1),
      "% within 100 m"
    ),
    hjust = 0,
    color = "red",
    size = 5
  ) +
  labs(
    x = "Distance to nearest road (m)",
    y = "Cumulative proportion of records"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(face = "bold", size = 16, color = "black"),
    axis.title.x = element_text(face = "bold", size = 16, color = "black"),
    axis.text.y = element_text(face = "bold", size = 16, color = "black"),
    axis.title.y = element_text(face = "bold", size = 16, color = "black")
  )

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Income - Not used in Manuscript
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

inat_df$Long = inat_df$X
inat_df$Lat = inat_df$Y

# Convert to sf object (points)
df_sf <- st_as_sf(inat_df, coords = c("Long", "Lat"), crs = 3310) %>%
  st_transform(4326)  # Ensure CRS is WGS84

# Download Income & Age Data from ACS
state_income_age <- get_acs(
  state = "CA",
  geography = "block group",
  variables = c(medinc = "B19013_001",
                medage = "B01002_001"),
  geometry = TRUE,
  year = 2023,
  output = "wide"
)
# save(state_income_age, file = 'outdir/state_income_age.Rdata')

# Transform CRS to match ACS data
df_sf <- df_sf %>%
  st_transform(st_crs(state_income_age))

# Spatial join points with income data
df_sf_income_age <- st_join(df_sf, state_income_age)


# Plotting income distribution
col_pal <- c('#046C9A', 'bisque3')

ggplot_income <- ggplot() +
  geom_density(aes(medincE / 1000, fill = "Income across Mortality records"),
               alpha = .2, data = df_sf_income_age, linewidth = 0.8) +
  geom_density(aes(medincE / 1000, fill = "State background income"),
               alpha = .2, data = state_income_age, linewidth = 0.8) +
  ggtitle("iNat mortality mammal \n records across California") +
  scale_fill_manual(values = col_pal) + theme_classic() +
  ylab("Sampling density") + xlab("Median household income in $") +
  theme(axis.text.x = element_text(face = "bold", size = 16, color = "black"),
        axis.title.x = element_text(face = "bold", size = 16, color = "black"),
        axis.text.y = element_text(face = "bold", size = 16, color = "black"),
        axis.title.y = element_text(face = "bold", size = 16, color = "black"))
ggsave(ggplot_income, file = 'outdir/resubmission_ggplot_income_cali.png')


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Create a buffer around roads (e.g., 100 m)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

road_buffer <- st_buffer(california_roads_3310, dist = 100)

# Get only iNaturalist points that intersect (i.e. are near) the road buffer
inat_california_3310_trans = st_transform(inat_california_3310, st_crs(road_buffer))
inat_near_roads <- st_intersection(inat_california_3310_trans, road_buffer)

# Convert the points to a data frame for plotting using hexbin
inat_near_df <- as.data.frame(st_coordinates(inat_near_roads))
# Rename columns to X and Y for consistency with ggplot aesthetics
colnames(inat_near_df) <- c("X", "Y")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Hexbin visualization: iNaturalist mortality observations overlapping with road networks
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

hexbin_plot <- ggplot() +
  # Base California map (using the projected boundary)
  geom_sf(data = california, fill = "lightgray", color = "black") +
  # Hexbin counts from the inat points near roads
  stat_bin_hex(
    data = inat_near_df,
    aes(x = X, y = Y),
    bins = 50,         # adjust for desired granularity
    color = "black",
    alpha = 0.8
  ) +
  scale_fill_viridis_c(option = "plasma", name = "Observation Count") +
  labs(
    title = "iNaturalist Mortality Observations Near Road Networks",
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  theme_minimal()

# Save the plot to a file supplementary figure 2
ggsave(hexbin_plot, filename = 'outdir/resubmission_hexbin_roads_inat.png', width = 8, height = 6)

hexbin_by_road <- ggplot(inat_near_roads, 
                         aes(x = st_coordinates(inat_near_roads)[,1], y = st_coordinates(inat_near_roads)[,2])) +
  stat_bin_hex(bins = 50, color = "black", alpha = 0.8) +
  facet_wrap(~ RTTYP) +
  scale_fill_viridis_c(option = "plasma", name = "Observation Count") +
  geom_sf(data = california, fill = NA, color = "gray40", inherit.aes = FALSE) +
  labs(
    title = "iNaturalist Mortality Observations Near Different Road Types",
    x = "Easting (m)",
    y = "Northing (m)"
  ) +
  theme_minimal()

ggsave(hexbin_by_road, filename = 'outdir/resubmission_hexbin_by_road.png', width = 8, height = 6)
# Patchwork them together for Figure 2 of the MS:

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Analysis 3 - Latin America - Animalia   
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

animalia = read.csv('indir/animalia_2023_LA.csv')

table(animalia$taxon.conservation_status.status_name)

animalia |> filter(taxon.conservation_status.status_name == 'critically endangered') |>
  distinct(taxon.name)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Figure 3 Puma ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

world <- ne_countries(scale = "medium", returnclass = "sf")
americas <- ne_countries(continent = c("South America", 'North America', 'Central America'))

# For puma concolor
# Bounding box:
#   SW corner: (-64.0721995786728, -165.294524913526) 
# NE corner: (82.8806662722686, -24.5820567201415)
# 

puma = read.csv('indir/puma_concolor_inat_dead_ALL_2025-06-20 (1).csv',
                stringsAsFactors = FALSE)

puma = read.csv('indir/resubmission_case_studies/puma_case_study_inat_dead_filtered_2026-04-11.csv',
                stringsAsFactors = FALSE)

puma = read.csv('indir/resubmission_case_studies/Puma/puma_case_study_2_inat_dead_filtered_2026-04-14.csv',
                stringsAsFactors = FALSE)


puma_range <- st_read('indir/redlist_species_data_0df88387-092f-4347-a28b-a17db80c88bc/data_0.shp')

puma_inat <- puma %>% 
  filter(grepl("Puma concolor", taxon.name, ignore.case = TRUE))

# Convert to an sf object assuming the 'location' column holds "lat,lon"
puma_sf <- puma_inat %>%
  filter(!is.na(location)) %>%
  separate(location, into = c("latitude", "longitude"), sep = ",", convert = TRUE) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mutate(year = year(time_observed_at))# WGS84


# Add a dummy column to puma_range for legend
puma_range$layer <- "Range Map"

bbox <- st_bbox(puma_sf)

# Expand by ~0.9 degrees (~100 km)
expand_deg <- 4

bbox_expanded <- bbox
bbox_expanded["xmin"] <- bbox["xmin"] - expand_deg
bbox_expanded["xmax"] <- bbox["xmax"] + expand_deg
bbox_expanded["ymin"] <- bbox["ymin"] - expand_deg
bbox_expanded["ymax"] <- bbox["ymax"] + expand_deg

# Create plot
puma_map <- ggplot() +
  # Light grey world background
  geom_sf(data = world, fill = "grey90", color = "white", size = 0.2) +
  
  # Puma range map with fill mapped to legend
  geom_sf(data = puma_range, aes(fill = layer), color = NA, alpha = 0.4) +
  scale_fill_manual(name = "", values = c("Range Map" = "goldenrod2")) +
  
  # Americas boundaries
  geom_sf(data = americas, fill = NA, color = "black", size = 0.5) +
  
  # Puma mortality points colored by year
  geom_sf(data = puma_sf, aes(color = year), size = 2) +
  scale_color_viridis_c(name = "Year", na.value = "darkgrey") +
  
  # Crop extent to points
  coord_sf(
    xlim = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
    ylim = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
    expand = FALSE
  )+
  # Labels and theme
  labs(
    #  title = expression("Political biogeography of "*italic("Puma concolor")*" mortalities"),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 15),
    legend.title = element_text(face = "bold")
  )+
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    
    # Axis titles
    axis.title = element_text(face = "bold", size = 16),
    
    # Axis tick labels
    axis.text = element_text(face = "bold", size = 12),
    
    # Legend title
    legend.title = element_text(face = "bold", size = 14),
    
    # Legend text
    legend.text = element_text(face = "bold", size = 13),
    
    # Optional: transparent background
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

puma_map

ggsave("outdir/resubmission_puma_map_highres.png", plot = puma_map,
       width = 12, height = 10, units = "in", dpi = 1000, bg = "transparent")


# Now death by country:

world_countries <- ne_countries(scale = "medium", returnclass = "sf")

# Make sure both layers use the same CRS
puma_sf <- st_transform(puma_sf, crs = st_crs(world_countries))
library(dplyr)
library(sf)
library(tibble)
library(ggplot2)
library(ggimage)

# 1. Spatial join — run only once
puma_sf_with_country <- st_join(
  puma_sf,
  world_countries[, c("admin", "iso_a3")],
  left = TRUE
)

# 2. Hard-code unmatched point(s) to USA only if that is biologically/geographically correct
puma_sf_with_country <- puma_sf_with_country %>%
  mutate(
    admin  = if_else(is.na(admin), "United States of America", admin),
    iso_a3 = if_else(is.na(iso_a3), "USA", iso_a3)
  )

# 3. Count records by country
df <- puma_sf_with_country %>%
  st_drop_geometry() %>%
  count(country = admin, name = "count") %>%
  arrange(desc(count)) %>%
  as_tibble()

# 4. Recode France -> French Guiana if that is the intended geographic label
df <- df %>%
  mutate(
    country = if_else(country == "France", "French Guiana", country)
  )

# 5. Complete flag lookup
flag_lookup <- tibble(
  country = c(
    "Argentina", "Belize", "Brazil", "Canada", "Chile",
    "Colombia", "Costa Rica", "Ecuador", "French Guiana",
    "Mexico", "Paraguay", "United States of America", "Venezuela"
  ),
  code = c(
    "ar", "bz", "br", "ca", "cl",
    "co", "cr", "ec", "gf",
    "mx", "py", "us", "ve"
  )
)

# 6. Join lookup and create flag URL
df <- df %>%
  left_join(flag_lookup, by = "country") %>%
  mutate(
    flag = if_else(
      is.na(code),
      NA_character_,
      paste0("https://flagcdn.com/w40/", code, ".png")
    )
  )

# 7. Check unmatched countries before plotting
unmatched <- df %>% filter(is.na(code))
print(unmatched)

# 8. Create barplot
puma_death <- ggplot(df, aes(x = reorder(country, count), y = count)) +
  geom_col(fill = "grey") +
  geom_image(
    aes(y = count + 1, image = flag),
    size = 0.08,
    na.rm = TRUE
  ) +
  geom_text(
    aes(y = count + 5, label = count),
    size = 6,
    fontface = "bold",
    color = "black"
  ) +
  coord_flip() +
  labs(
    x = "Country",
    y = "Death Count",
    title = "Mountain Lion Deaths (2022–2026)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(face = "bold", size = 16, color = "black"),
    axis.text.y = element_text(face = "bold", size = 16, color = "black"),
    axis.title.x = element_text(face = "bold", size = 16, color = "black"),
    axis.title.y = element_text(face = "bold", size = 16, color = "black"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black")
  )

puma_death

ggsave(
  filename = "outdir/resubmission_puma_death.png",
  plot = puma_death,
  width = 10,
  height = 6,
  dpi = 600
)

# # Spatial join: attach country attributes to each puma point
# puma_sf_with_country <- st_join(puma_sf, world_countries[, c("admin", "iso_a3")])
# 
# df = puma_sf_with_country %>%
#   count(country = admin, name = "count") %>%
#   arrange(desc(count)) |>
#   st_drop_geometry() |>
#   as_tibble()
# 
# # Create a data frame with counts and flag URLs
# # df <- data.frame(
# #   country = c("Argentina", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "French Guiana", "Mexico", "United States"),
# #   count = c(17, 3, 4, 4, 1, 2, 1, 9, 44)
# # )
# 
# # # Adding flag image URLs
# # df$flag <- c("https://flagcdn.com/w40/ar.png", 
# #              "https://flagcdn.com/w40/br.png",
# #              "https://flagcdn.com/w40/ca.png",
# #              "https://flagcdn.com/w40/cl.png",
# #              "https://flagcdn.com/w40/co.png",
# #              "https://flagcdn.com/w40/cr.png",
# #              "https://flagcdn.com/w40/gf.png",  # French Guiana
# #              # "https://flagcdn.com/w40/guf.png",  # French Guiana
# #              #     'https://upload.wikimedia.org/wikipedia/commons/6/6e/Flag_of_French_Guiana.svg',
# #              "https://flagcdn.com/w40/mx.png",
# #              "https://flagcdn.com/w40/us.png")
# # 
# # flag_lookup <- tibble::tibble(
# #   country = c("Argentina", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", 
# #               "French Guiana", "Mexico", "United States of America"),
# #   code = c("ar", "br", "ca", "cl", "co", "cr", "gf", "mx", "us")
# # )
# # 
# # df <- df %>%
# #   mutate(country = if_else(country == "France", "French Guiana", country))
# # 
# # # 3. Join to main df
# # df <- df %>%
# #   left_join(flag_lookup, by = "country") %>%
# #   mutate(flag = paste0("https://flagcdn.com/w40/", code, ".png"))
# # # 
# # # df <- df %>%
# # #   left_join(flag_lookup, by = "country") %>%
# # #   mutate(flag = ifelse(!is.na(code), 
# # #                        paste0("https://flagcdn.com/w40/", code, ".png"), 
# # #                        as.character(flag)))
# 
# # # Create the barplot
# # puma_death <- ggplot(df, aes(x = reorder(country, count), y = count)) +
# #   geom_bar(stat = "identity", fill = "grey") +
# #   # Place flag images slightly above each bar
# #   geom_image(aes(y = count + 1, image = flag), size = 0.08) +
# #   coord_flip() +
# #   labs(x = "Country", y = "Death Count",
# #        title = "Mountain Lion Deaths (2022-2025)") +
# #   theme_bw() +  # Use a clean black & white theme
# #   theme(
# #     axis.text.x = element_text(face = "bold", size = 16, color = "black"),
# #     axis.text.y = element_text(face = "bold", size = 16, color = "black"),  # Country names bold & black
# #     axis.title.x = element_text(face = "bold", size = 16, color = "black"),
# #     axis.title.y = element_text(face = "bold", size = 16, color = "black"),
# #     plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),  # Bold title, centered
# #     legend.text = element_text(face = "bold", size = 14, color = "black"),
# #     legend.title = element_text(face = "bold", size = 16, color = "black")
# #   ) +
# #   # Move count labels further to the right for clarity
# #   geom_text(aes(y = count + 5, label = count), size = 6, fontface = "bold", color = "black")
# # 
# # 
# 
# # Adress reviewer comment:
# 
# library(dplyr)
# library(lubridate)
# 
# puma <- puma %>%
#   mutate(
#     time = ymd_hms(time_observed_at)
#   )
# 
# # potential duplicates: same day + close space (e.g., 100 m)
# library(dplyr)
# library(lubridate)
# library(geosphere)
# 
# puma <- puma %>%
#   mutate(
#     time = ymd_hms(time_observed_at),
#     obs_week = floor_date(as.Date(observed_on), unit = "week")
#   )
# 
# puma_dups <- puma %>%
#   arrange(obs_week, time) %>%
#   group_by(obs_week) %>%
#   mutate(
#     lag_dist_m = geosphere::distHaversine(
#       cbind(longitude, latitude),
#       cbind(lag(longitude), lag(latitude))
#     ),
#     lag_time_days = as.numeric(difftime(time, lag(time), units = "days"))
#   ) %>%
#   ungroup() %>%
#   filter(!is.na(lag_dist_m) & lag_dist_m < 10000)
# 
# nrow(puma_dups)
# puma_dups
# 
# 
# 
# # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# # Updated code bellow as new record from paraguay and a NA location
# # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# #
# # 1. Spatial join
# puma_sf_with_country <- st_join(
#   puma_sf,
#   world_countries[, c("admin", "iso_a3")],
#   left = TRUE
# )
# puma_sf_with_country <- st_join(
#   puma_sf,
#   world_countries[, c("admin", "iso_a3")],
#   left = TRUE
# )
# 
# # 2. Hard-code unmatched point(s) to USA
# puma_sf_with_country <- puma_sf_with_country %>%
#   mutate(
#     admin = if_else(is.na(admin), "United States of America", admin),
#     iso_a3 = if_else(is.na(iso_a3), "USA", iso_a3)
#   )
# 
# # 3. Count records by country
# df <- puma_sf_with_country %>%
#   st_drop_geometry() %>%
#   count(country = admin, name = "count") %>%
#   arrange(desc(count)) %>%
#   as_tibble()
# 
# # 4. Recode France -> French Guiana before lookup
# df <- df %>%
#   mutate(
#     country = if_else(country == "France", "French Guiana", country)
#   )
# 
# # 5. Flag lookup table
# flag_lookup <- tibble(
#   country = c(
#     "Argentina", "Brazil", "Canada", "Chile", "Colombia",
#     "Costa Rica", "French Guiana", "Mexico",
#     "United States of America", "Paraguay"
#   ),
#   code = c(
#     "ar", "br", "ca", "cl", "co",
#     "cr", "gf", "mx", "us", "py"
#   )
# )
# 
# # 6. Join lookup and create flag URL
# df <- df %>%
#   left_join(flag_lookup, by = "country") %>%
#   mutate(
#     flag = paste0("https://flagcdn.com/w40/", code, ".png")
#   )
# 
# # 7. Optional check for unmatched countries
# print(df %>% filter(is.na(code)))
# 
# # 8. Create the barplot
# puma_death <- ggplot(df, aes(x = reorder(country, count), y = count)) +
#   geom_col(fill = "grey") +
#   geom_image(
#     aes(y = count + 1, image = flag),
#     size = 0.08
#   ) +
#   geom_text(
#     aes(y = count + 5, label = count),
#     size = 6,
#     fontface = "bold",
#     color = "black"
#   ) +
#   coord_flip() +
#   labs(
#     x = "Country",
#     y = "Death Count",
#     title = "Mountain Lion Deaths (2022–2025)"
#   ) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(face = "bold", size = 16, color = "black"),
#     axis.text.y = element_text(face = "bold", size = 16, color = "black"),
#     axis.title.x = element_text(face = "bold", size = 16, color = "black"),
#     axis.title.y = element_text(face = "bold", size = 16, color = "black"),
#     plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),
#     legend.text = element_text(face = "bold", size = 14, color = "black"),
#     legend.title = element_text(face = "bold", size = 16, color = "black")
#   )

puma_death

# Save the plot as a high-quality image
ggsave(puma_death, file = "outdir/resubmission_puma_death.png", width = 10, height = 6, dpi = 600)


puma_map + puma_death


# Taxonomic breakdown of the parquet file ####


# =========================================================
# 1. READ ARCHIVE: Taxonomic breakdown
# =========================================================
parquet_path <- "https://huggingface.co/datasets/diegoellissoto/iNaturalist_mortality_records_12Apr2025/resolve/main/inat_all_Apr122025.parquet"
# Newer 2026 one:
parquet_path <- "https://huggingface.co/datasets/diegoellissoto/inat_observations-2026-04-21.parquet/resolve/main/inat_mortality_observations-2026-04-21.parquet?download=true"

inat_all_raw <- arrow::read_parquet(parquet_path)


# Pull into memory if needed
inat_all <- inat_all_raw %>%
  collect()

# Suppl Figure 1
records_by_year <- inat_all %>%
  mutate(
    year = year(as.Date(observed_on))
    # if observed_on is missing in your file, use:
    # year = year(as.POSIXct(time_observed_at))
  ) %>%
  filter(!is.na(year)) %>%
  count(year, name = "n_records")

growth_by_year = ggplot(records_by_year, aes(x = year, y = n_records)) +
  geom_col(fill = "darkolivegreen4") +
  labs(
  #  title = "iNaturalist Mortality Records by Year",
    x = "Year",
    y = "Number of records"
  ) +
  theme_minimal()+
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 17, face = "bold"),
    axis.title.y = element_text(size = 17, face = "bold"),
    plot.title = element_text(size = 18, face = "bold")
  )+xlim(2000, 2027)


growth_by_year_v2 = ggplot(records_by_year, aes(x = year, y = n_records)) +
  geom_col(
    fill = "darkolivegreen4",
    width = 0.85
  ) +
  scale_x_continuous(
    limits = c(2000, 2027),
    breaks = seq(2000, 2025, by = 5),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Year",
    y = "Number of records"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 28, color = "black"),
    axis.text.y = element_text(size = 28, color = "black"),
    
    axis.title.x = element_text(face = "bold", size = 28),
    axis.title.y = element_text(face = "bold", size = 28),
    
    plot.title = element_text(face = "bold", size = 28),
    
    axis.line = element_line(linewidth = 1),
    axis.ticks = element_line(linewidth = 1),
    axis.ticks.length = unit(0.25, "cm")
  )

growth_by_year_v2


ggsave(growth_by_year, file = "outdir/suppl_fig1.png", width = 10, height = 6, dpi = 600)
ggsave(growth_by_year, file = "outdir/fig1_top_left.png", width = 10, height = 6, dpi = 600)
ggsave(growth_by_year_v2, file = "outdir/fig1_top_left_v2.png", width = 10, height = 6, dpi = 600)



taxon_summary <- inat_all %>%
  transmute(
    iconic_taxon_name = na_if(`taxon.iconic_taxon_name`, ""),
    iconic_taxon_name = coalesce(iconic_taxon_name, "Unknown")
  ) %>%
  count(iconic_taxon_name, name = "n") %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))


# For old parquet file
# taxon_summary <- inat_all %>%
#   mutate(
#     iconic_taxon_name = dplyr::coalesce(iconic_taxon_name, "Unknown")
#   ) %>%
#   count(iconic_taxon_name, name = "n") %>%
#   mutate(prop = n / sum(n)) %>%
#   arrange(desc(n))

taxon_summary

# taxon_summary <- inat_all %>%
#   mutate(iconic_taxon_name = coalesce(iconic_taxon_name, "Unknown")) %>%
#   filter(iconic_taxon_name != "Animalia") %>%  # remove redundant level
#   count(iconic_taxon_name, name = "n") %>%
#   mutate(prop = n / sum(n))

taxon_comp_ggplot = ggplot(taxon_summary,
       aes(x = fct_reorder(iconic_taxon_name, prop), y = prop)) +
  geom_col(fill = "grey70", color = "black", width = 0.7) +
  geom_text(aes(label = percent(prop, accuracy = 0.1)),
            hjust = -0.1, size = 5) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = NULL,
    y = "Proportion of archive",
    title = "Taxonomic composition of mortality records"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(face = "bold", size = 14)
  )

ggsave(taxon_comp_ggplot, file = 'outdir/resubmission_taxonomic_composition_parquet.png')



inat_all <- inat_all %>%
  dplyr::mutate(
    broad_group = case_when(
      taxon.iconic_taxon_name %in% c("Mammalia", "Aves", "Reptilia", "Amphibia", "Actinopterygii") ~ "Vertebrates",
      taxon.iconic_taxon_name %in% c("Insecta", "Arachnida", "Mollusca") ~ "Invertebrates",
      TRUE ~ "Other"
    )
  )

group_summary <- inat_all %>%
  count(broad_group, taxon.iconic_taxon_name) %>%
  group_by(broad_group) %>%
  mutate(prop = n / sum(n))

ggplot(group_summary,
       aes(x = fct_reorder(taxon.iconic_taxon_name, prop), y = prop, fill = broad_group)) +
  geom_col(color = "black") +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = NULL,
    y = "Proportion within group",
    fill = "Group",
    title = "Taxonomic composition grouped by major biological divisions"
  ) +
  theme_classic()

tree_df <- inat_all %>%
  mutate(
    taxon.iconic_taxon_name = coalesce(taxon.iconic_taxon_name, "Unknown"),
    broad_group = case_when(
      taxon.iconic_taxon_name %in% c("Mammalia", "Aves", "Reptilia", "Amphibia", "Actinopterygii") ~ "Vertebrates",
      taxon.iconic_taxon_name %in% c("Insecta", "Arachnida", "Mollusca") ~ "Invertebrates",
      TRUE ~ "Other"
    )
  ) %>%
  count(broad_group, taxon.iconic_taxon_name)

ggplot(tree_df,
       aes(area = n,
           fill = broad_group,
           label = paste0(taxon.iconic_taxon_name, "\n", scales::comma(n)))) +
  geom_treemap(color = "white") +
  geom_treemap_text(place = "centre", reflow = TRUE, size = 4) +
  labs(
    title = "Hierarchical taxonomic composition of mortality records",
    fill = "Major group"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 18))

taxa_counts <- inat_all %>%
  count(taxon.iconic_taxon_name) %>%
  pull(n)

Lc(taxa_counts) %>%
  plot(main = "Inequality in taxonomic representation")

taxa_counts <- inat_all %>%
  count(taxon.iconic_taxon_name) %>%
  pull(n)

gini <- ineq(taxa_counts, type = "Gini")
gini

inat_all <- inat_all %>%
  mutate(
    taxon.iconic_taxon_name = coalesce(taxon.iconic_taxon_name, "Unknown"),
    broad_group = case_when(
      taxon.iconic_taxon_name %in% c("Mammalia", "Aves", "Reptilia", "Amphibia", "Actinopterygii") ~ "Vertebrates",
      taxon.iconic_taxon_name %in% c("Insecta", "Arachnida", "Mollusca") ~ "Invertebrates",
      TRUE ~ "Other"
    )
  )

taxon_counts <- inat_all %>%
  filter(!taxon.iconic_taxon_name %in% c("Unknown")) %>%
  filter(!broad_group %in% c("Other")) %>%
  count(broad_group, taxon.iconic_taxon_name, name = "n")


# Nodes
nodes <- tibble(
  name = c("Life", unique(taxon_counts$broad_group), unique(taxon_counts$taxon.iconic_taxon_name))
)

# Edges
# edges <- bind_rows(
#   tibble(from = "Life", to = unique(taxon_counts$broad_group)),
#   taxon_counts %>% select(from = broad_group, to = taxon.iconic_taxon_name)
# )

edges <- taxon_counts %>%
  select(from = broad_group, to = taxon.iconic_taxon_name)

nodes <- tibble(
  name = c(unique(taxon_counts$broad_group),
           unique(taxon_counts$taxon.iconic_taxon_name))
)

nodes <- nodes %>%
  left_join(
    taxon_counts %>%
      group_by(taxon.iconic_taxon_name) %>%
      summarise(n = sum(n), .groups = "drop"),
    by = c("name" = "taxon.iconic_taxon_name")
  ) %>%
  mutate(n = replace_na(n, 0))

graph <- tbl_graph(nodes = nodes, edges = edges)

ggraph(graph, layout = "tree") +
  geom_edge_link(alpha = 0.4) +
  geom_node_point(aes(size = n, color = n)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_color_viridis_c() +
  theme_void() +
  labs(title = "Taxonomic structure of mortality records")

plot_df <- inat_all %>%
  mutate(taxon.iconic_taxon_name = coalesce(taxon.iconic_taxon_name, "Unknown")) %>%
  filter(!taxon.iconic_taxon_name %in% c("Unknown", "Animalia")) %>%
  count(taxon.iconic_taxon_name, broad_group, name = "n") %>%
  mutate(prop = n / sum(n))

taxon_comp_loliplot = ggplot(plot_df,
       aes(x = fct_reorder(taxon.iconic_taxon_name, n), y = n, color = broad_group)) +
  geom_segment(aes(xend = taxon.iconic_taxon_name, y = 0, yend = n),
               linewidth = 1, alpha = 0.7) +
  geom_point(size = 6) +
  coord_flip() +
  scale_y_continuous(labels = comma_format()) +
  labs(
    x = NULL,
    y = "Number of mortality records",
    color = "Major group",
    title = "Taxonomic composition of mortality records"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(face = "bold", size = 14)
  )
ggsave(taxon_comp_loliplot, file = 'outdir/resubmission_taxonomic_composition_plot_parquet.png')



group_df <- plot_df %>%
  group_by(broad_group) %>%
  mutate(within_group_prop = n / sum(n)) %>%
  ungroup()

ggplot(group_df,
       aes(x = broad_group, y = within_group_prop, fill = taxon.iconic_taxon_name)) +
  geom_col(width = 0.7, color = "black") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Major group",
    y = "Proportion within major group",
    fill = "Taxon",
    title = "Within-group taxonomic composition of mortality records"
  ) +
  theme_classic()

# starting totals
n_total <- nrow(inat_all)

# define what will be excluded from the plot
filtered_out_df <- inat_all %>%
  mutate(taxon.iconic_taxon_name = coalesce(taxon.iconic_taxon_name, "Unknown")) %>%
  filter(taxon.iconic_taxon_name %in% c("Unknown", "Animalia"))

n_filtered_out <- nrow(filtered_out_df)
prop_filtered_out <- n_filtered_out / n_total

n_total
n_filtered_out
prop_filtered_out

filtered_out_df <- inat_all %>%
  mutate(taxon.iconic_taxon_name = coalesce(taxon.iconic_taxon_name, "Unknown")) %>%
  filter(taxon.iconic_taxon_name %in% c("Unknown", "Animalia") | broad_group == "Other")

n_filtered_out <- nrow(filtered_out_df)
prop_filtered_out <- n_filtered_out / n_total


plot_df <- inat_all %>%
  mutate(taxon.iconic_taxon_name = coalesce(taxon.iconic_taxon_name, "Unknown")) %>%
  filter(!taxon.iconic_taxon_name %in% c("Unknown", "Animalia")) %>%
  filter(!broad_group %in% c("Other")) %>%   # remove if you want to keep "Other"
  count(taxon.iconic_taxon_name, broad_group, name = "n") %>%
  mutate(prop_archive = n / n_total)

ggplot(plot_df,
       aes(x = fct_reorder(taxon.iconic_taxon_name, prop_archive),
           y = prop_archive,
           color = broad_group)) +
  geom_segment(aes(xend = taxon.iconic_taxon_name, y = 0, yend = prop_archive),
               linewidth = 1, alpha = 0.7) +
  geom_point(size = 6) +
  # geom_text(aes(label = percent(prop_archive, accuracy = 0.1)),
  #           hjust = -0.15, size = 4.5, color = "black") +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    x = NULL,
    y = "Proportion of archive",
    color = "Major group",
    title = "Taxonomic composition \n of mortality records"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold")
  )


n_total <- nrow(inat_all)

plot_df <- inat_all %>%
  mutate(taxon.iconic_taxon_name = coalesce(taxon.iconic_taxon_name, "Unknown")) %>%
  filter(!taxon.iconic_taxon_name %in% c("Unknown", "Animalia")) %>%
  count(taxon.iconic_taxon_name, broad_group, name = "n") %>%
  mutate(prop = n / n_total)


taxon_comp_loliplot <- ggplot(
  plot_df,
  aes(x = fct_reorder(taxon.iconic_taxon_name, prop),
      y = prop,
      color = broad_group)
) +
  geom_segment(
    aes(xend = taxon.iconic_taxon_name, y = 0, yend = prop),
    linewidth = 1, alpha = 0.7
  ) +
  geom_point(size = 6) +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    hjust = -0.25,
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_color_viridis_d(option = "E", end = 0.85) +
  labs(
    x = NULL,
    y = "Percentage of archive",
    color = "Major group",
    title = "Taxonomic composition of mortality records"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(face = "bold", size = 14)
  )


ggsave(taxon_comp_loliplot, file = 'outdir/resubmission_parquet_taxon_composition_viridis.png')

taxon_comp_loliplot_v2 = ggplot(
  plot_df,
  aes(x = fct_reorder(taxon.iconic_taxon_name, n),
      y = n,
      color = broad_group)
) +
  geom_segment(
    aes(xend = taxon.iconic_taxon_name, y = 0, yend = n),
    linewidth = 1, alpha = 0.7
  ) +
  geom_point(size = 6) +
  # geom_text(
  #   aes(label = n, accuracy = 0.1),
  #   hjust = -0.25,
  #   size = 4
  # ) +
  coord_flip() +
  scale_y_continuous(
    
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_color_viridis_d(option = "E", end = 0.85) +
  labs(
    x = NULL,
    y = "Number of records",
    color = "Major group",
    # title = "Taxonomic composition of mortality records"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 28),
    axis.text.x = element_text(size = 28, color = "black"),
    axis.text.y = element_text(size = 28, color = "black"),
    axis.title.x = element_text(face = "bold", size = 28),
    axis.title.y = element_blank()
  ) +
  theme(
    legend.position = c(0.70, 0.48),   # x, y inside plot area
    legend.justification = c(0, 0.5),
    
    legend.title = element_text(size = 28, face = "bold"),
    legend.text  = element_text(size = 28),
    legend.key.size = unit(0.8, "cm"),
    
    legend.background = element_rect(
      fill = scales::alpha("white", 0.8),
      colour = NA
    )
  )


taxon_comp_loliplot_v2 = ggplot(
  plot_df,
  aes(x = fct_reorder(taxon.iconic_taxon_name, n),
      y = n,
      color = broad_group)
) +
  geom_segment(
    aes(xend = taxon.iconic_taxon_name, y = 0, yend = n),
    linewidth = 5, alpha = 0.7   # thicker sticks
  ) +
  geom_point(size = 18) +            # bigger dots
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_color_viridis_d(option = "E", end = 0.85) +
  labs(
    x = NULL,
    y = "Number of records",
    color = "Major group"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 36),
    axis.text.x = element_text(size = 36, color = "black"),
    axis.text.y = element_text(size = 42, color = "black"),
    axis.title.x = element_text(face = "bold", size = 36),
    legend.position = c(0.65, 0.48),
    legend.justification = c(0, 0.5),
    legend.title = element_text(size = 42, face = "bold"),
    legend.text  = element_text(size = ),
    legend.key.size = unit(0.8, "cm"),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.8),
      colour = NA
    )
  )


ggsave(taxon_comp_loliplot_v2, file = 'outdir/resubmission_parquet_taxon_composition_viridis_v2.png')

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Africa endangered species March 2026:
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

inat <- read.csv('indir/resubmission_case_studies/Endangered_species/africa_march_2026_inat_dead_filtered_2026-04-15.csv', 
                 stringsAsFactors = FALSE)

View(inat$taxon.conservation_status.iucn)
table(inat$taxon.conservation_status.status)


# --- --- ---
library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

# ---------------------------------------------------
# 1. Load mortality data directly from iNaturalist API
# ---------------------------------------------------

api_url <- "https://api.inaturalist.org/v1/observations/iconic_taxa_counts?term_id=17&term_value_id=19"

cat("\nLoading data from iNaturalist API...\n")
json_file <- fromJSON(api_url, flatten = TRUE)

# ---------------------------------------------------
# 2. Make full dataframe BEFORE filtering
# ---------------------------------------------------

mortality_all <- json_file$results %>%
  transmute(
    common_name = taxon.preferred_common_name,
    taxon_name = taxon.name,
    taxon_id = taxon_id,
    dead_records = count
  ) %>%
  mutate(
    common_name = ifelse(is.na(common_name), taxon_name, common_name)
  ) %>%
  arrange(desc(dead_records))

cat("\nAll categories returned by API:\n")
print(mortality_all)

cat("\nTotal mortality records BEFORE filtering:\n")
print(sum(mortality_all$dead_records, na.rm = TRUE))

# ---------------------------------------------------
# 3. Define selected categories
# ---------------------------------------------------

keep_categories <- c(
  "Insects",
  "Molluscs",
  "Mammals",
  "Birds",
  "Reptiles",
  "Ray-finned Fishes",
  "Amphibians",
  "Arachnids"
)

excluded_categories <- mortality_all %>%
  filter(!common_name %in% keep_categories)

cat("\nCategories EXCLUDED by keep_categories filter:\n")
print(excluded_categories)

cat("\nTotal mortality records EXCLUDED:\n")
print(sum(excluded_categories$dead_records, na.rm = TRUE))

# ---------------------------------------------------
# 4. Filtered dataframe: selected categories only
# ---------------------------------------------------

plot_df <- mortality_all %>%
  filter(common_name %in% keep_categories) %>%
  mutate(
    major_group = case_when(
      common_name %in% c("Insects", "Molluscs", "Arachnids") ~ "Invertebrates",
      common_name %in% c("Mammals", "Birds", "Reptiles",
                         "Ray-finned Fishes", "Amphibians") ~ "Vertebrates",
      TRUE ~ "Other"
    ),
    common_name = factor(common_name, levels = rev(common_name))
  )

cat("\nTotal mortality records AFTER filtering:\n")
print(sum(plot_df$dead_records, na.rm = TRUE))


library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

plot_df_clean <- plot_df %>%
  mutate(
    major_group = str_remove(major_group, "^[ab]\\s*")
  )

p_count_kept <- ggplot(
  plot_df_clean,
  aes(x = dead_records, y = common_name, color = major_group)
) +
  geom_segment(
    aes(x = 0, xend = dead_records, y = common_name, yend = common_name),
    linewidth = 1.8,
    alpha = 0.7
  ) +
  geom_point(size = 10) +
  scale_x_continuous(
    labels = comma,
    limits = c(0, max(plot_df_clean$dead_records, na.rm = TRUE) * 1.10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Invertebrates" = "#0b2c66",
      "Vertebrates"   = "#d4bf57",
      "Other"         = "grey50"
    )
  ) +
  labs(
    title = "Taxonomic composition of mortality records",
    x = "Number of mortality records",
    y = NULL,
    color = "Major group"
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "right"
  )

p_count_kept




p_count_kept <- ggplot(
  plot_df,
  aes(x = dead_records, y = common_name, color = major_group)
) +
  geom_segment(
    aes(x = 0, xend = dead_records, y = common_name, yend = common_name),
    linewidth = 1.8,
    alpha = 0.7
  ) +
  geom_point(size = 10) +
  geom_text(
    aes(label = comma(dead_records)),
    hjust = -0.15,
    size = 6
  ) +
  scale_x_continuous(
    labels = comma,
    limits = c(0, max(plot_df$dead_records, na.rm = TRUE) * 1.28),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Invertebrates" = "#0b2c66",
      "Vertebrates"   = "#d4bf57",
      "Other"         = "grey50"
    )
  ) +
  labs(
    title = "Taxonomic composition of mortality records",
    x = "Number of mortality records",
    y = NULL,
    color = "Major group"
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "right"
  )

p_count_kept





plot_df_all <- mortality_all %>%
  mutate(
    major_group = case_when(
      common_name %in% c("Insects", "Molluscs", "Arachnids") ~ "Invertebrates",
      common_name %in% c("Mammals", "Birds", "Reptiles",
                         "Ray-finned Fishes", "Amphibians") ~ "Vertebrates",
      TRUE ~ "Other"
    ),
    common_name = factor(common_name, levels = rev(common_name))
  )

p_count_all <- ggplot(
  plot_df_all,
  aes(x = dead_records, y = common_name, color = major_group)
) +
  geom_segment(
    aes(x = 0, xend = dead_records, y = common_name, yend = common_name),
    linewidth = 1.8,
    alpha = 0.7
  ) +
  geom_point(size = 10) +
  geom_text(
    aes(label = comma(dead_records)),
    hjust = -0.15,
    size = 6
  ) +
  scale_x_continuous(
    labels = comma,
    limits = c(0, max(plot_df_all$dead_records, na.rm = TRUE) * 1.28),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_color_manual(
    values = c(
      "Invertebrates" = "#0b2c66",
      "Vertebrates"   = "#d4bf57",
      "Other"         = "grey50"
    )
  ) +
  labs(
    title = "Mortality records across all iNaturalist iconic taxa",
    x = "Number of mortality records",
    y = NULL,
    color = "Major group"
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = "right"
  )

p_count_all


ggsave(
  "outdir/mortality_records_selected_categories_counts.png",
  p_count_kept,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  "outdir/mortality_records_all_common_names_counts.png",
  p_count_all,
  width = 12,
  height = 8,
  dpi = 300
)




library(jsonlite)
library(dplyr)
library(tibble)
library(ggplot2)
library(scales)
library(stringr)

# ---------------------------------------------------
# 1. Choose whether to count by upload year or observed year
# ---------------------------------------------------

date_field <- "created"   # "created" = stored/added to iNat
# date_field <- "observed" # "observed" = date organism was observed

api_url <- paste0(
  "https://api.inaturalist.org/v1/observations/histogram?",
  "term_id=17&term_value_id=19",
  "&date_field=", date_field,
  "&interval=year"
)

cat("\nAPI URL:\n")
cat(api_url, "\n\n")

json_file <- fromJSON(api_url, flatten = TRUE)

# ---------------------------------------------------
# 2. Convert histogram JSON into a dataframe
# ---------------------------------------------------

mortality_by_year <- enframe(
  unlist(json_file$results$year),
  name = "year_raw",
  value = "mortality_records"
) %>%
  mutate(
    year = as.integer(str_extract(year_raw, "^\\d{4}")),
    mortality_records = as.numeric(mortality_records)
  ) %>%
  select(year, mortality_records) %>%
  arrange(year)

cat("\nMortality records per year:\n")
print(mortality_by_year)

cat("\nTotal mortality records across all years:\n")
print(sum(mortality_by_year$mortality_records, na.rm = TRUE))

inat_green <- "#74AC00"


p_year <- ggplot(mortality_by_year, aes(x = year, y = mortality_records)) +
  # geom_col(width = 0.8) +
  geom_col(
    width = 0.8,
    fill = inat_green,
    colour = inat_green,
    linewidth = 0.3
  )+
  scale_y_continuous(labels = comma) +
  scale_x_continuous(
    breaks = pretty_breaks(),
    labels = function(x) as.integer(x)
  ) +
  labs(
    title = "iNaturalist mortality records per year",
  #  subtitle = paste("Date field:", date_field),
    x = "Year",
    y = "Number of mortality records"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

p_year


ggsave(
  "outdir/inat_mortality_records_per_year.png",
  p_year,
  width = 10,
  height = 6,
  dpi = 300
)



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Percent dead or alive
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)
library(scales)
library(glue)

# ------------------------------------------------------------
# Core API helper
# ------------------------------------------------------------

inat_api <- "https://api.inaturalist.org/v1"

get_total_results <- function(endpoint, params = list()) {
  req <- request(paste0(inat_api, endpoint)) |>
    req_user_agent("iNat mortality summary R script")
  
  req <- do.call(req_url_query, c(list(req), params))
  
  resp <- req |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  out <- resp_body_json(resp, simplifyVector = TRUE)
  
  out$total_results
}

# ------------------------------------------------------------
# Annotation IDs
# ------------------------------------------------------------
# iNaturalist controlled terms:
# Alive or Dead = term_id 17
# Alive = term_value_id 18
# Dead = term_value_id 19
# Cannot Be Determined = term_value_id 20

alive_dead_term <- 17
alive_value <- 18
dead_value <- 19
cannot_determine_value <- 20

animalia_taxon_id <- 1

# ------------------------------------------------------------
# Shared parameters
# ------------------------------------------------------------
# verifiable = "any" avoids silently restricting counts to only
# verifiable observations.

base_params <- list(
  per_page = 0,
  verifiable = "any"
)

dead_params <- c(
  base_params,
  list(
    term_id = alive_dead_term,
    term_value_id = dead_value
  )
)

alive_params <- c(
  base_params,
  list(
    term_id = alive_dead_term,
    term_value_id = alive_value
  )
)

cannot_determine_params <- c(
  base_params,
  list(
    term_id = alive_dead_term,
    term_value_id = cannot_determine_value
  )
)

animalia_params <- c(
  base_params,
  list(taxon_id = animalia_taxon_id)
)

dead_animalia_params <- c(
  dead_params,
  list(taxon_id = animalia_taxon_id)
)

# ------------------------------------------------------------
# API calls
# ------------------------------------------------------------

query_date <- Sys.Date()

total_observations <- get_total_results(
  endpoint = "/observations",
  params = base_params
)

animalia_observations <- get_total_results(
  endpoint = "/observations",
  params = animalia_params
)

dead_observations <- get_total_results(
  endpoint = "/observations",
  params = dead_params
)

dead_animalia_observations <- get_total_results(
  endpoint = "/observations",
  params = dead_animalia_params
)

alive_observations <- get_total_results(
  endpoint = "/observations",
  params = alive_params
)

cannot_determine_observations <- get_total_results(
  endpoint = "/observations",
  params = cannot_determine_params
)

dead_species <- get_total_results(
  endpoint = "/observations/species_counts",
  params = dead_params
)

dead_observers <- get_total_results(
  endpoint = "/observations/observers",
  params = dead_params
)

dead_identifiers <- get_total_results(
  endpoint = "/observations/identifiers",
  params = dead_params
)

summary_df <- tibble(
  query_date = query_date,
  total_observations = total_observations,
  animalia_observations = animalia_observations,
  dead_observations = dead_observations,
  dead_animalia_observations = dead_animalia_observations,
  alive_observations = alive_observations,
  cannot_determine_observations = cannot_determine_observations,
  alive_or_dead_annotated_observations = alive_observations + dead_observations,
  alive_dead_or_uncertain_annotated_observations =
    alive_observations + dead_observations + cannot_determine_observations,
  dead_species = dead_species,
  dead_observers = dead_observers,
  dead_identifiers = dead_identifiers
) |>
  mutate(
    pct_dead_of_all_inat =
      dead_observations / total_observations,
    pct_dead_of_animalia =
      dead_animalia_observations / animalia_observations,
    pct_dead_of_alive_or_dead_annotated =
      dead_observations / alive_or_dead_annotated_observations,
    pct_dead_of_alive_dead_or_uncertain_annotated =
      dead_observations / alive_dead_or_uncertain_annotated_observations
  )

summary_df

paper_sentence <- glue_data(
  summary_df,
  "As of {format(query_date, '%B %d, %Y')}, iNaturalist contained ",
  "{comma(total_observations)} total observations, including ",
  "{comma(animalia_observations)} Animalia observations. ",
  "There were {comma(dead_observations)} observations annotated as 'dead' ",
  "for {comma(dead_species)} species, representing ",
  "{percent(pct_dead_of_all_inat, accuracy = 0.01)} of all iNaturalist observations and ",
  "{percent(pct_dead_of_animalia, accuracy = 0.01)} of Animalia observations. ",
  "Among observations explicitly annotated as either Alive or Dead, ",
  "{percent(pct_dead_of_alive_or_dead_annotated, accuracy = 0.01)} were annotated as Dead."
)

cat(paper_sentence)


annotated_sentence <- glue(
  "Among observations explicitly annotated as either Alive or Dead, ",
  "{percent(summary_df$pct_dead_of_alive_or_dead_annotated, accuracy = 0.01)} ",
  "were annotated as Dead ",
  "({comma(dead_observations)} of ",
  "{comma(summary_df$alive_or_dead_annotated_observations)} annotated observations)."
)

cat(annotated_sentence)



# --- --- ---


library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
library(scales)
library(stringr)
library(glue)
library(forcats)

inat_api <- "https://api.inaturalist.org/v1"

inat_green <- "#74AC00"
inat_dark <- "#4D7C00"

# Colorblind-friendly palette for taxa
taxon_cols <- c(
  "Insects" = "#009E73",
  "Molluscs" = "#56B4E9",
  "Mammals" = "#0072B2",
  "Birds" = "#E69F00",
  "Reptiles" = "#CC79A7",
  "Ray-finned Fishes" = "#8A5FD3",
  "Amphibians" = "#D55E00",
  "Arachnids" = "#7F7F7F"
)

inat_get <- function(endpoint, params = list()) {
  req <- request(paste0(inat_api, endpoint)) |>
    req_user_agent("iNat mortality figures R script")
  
  req <- do.call(req_url_query, c(list(req), params))
  
  resp <- req |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  resp_body_json(resp, simplifyVector = TRUE)
}

get_total_results <- function(endpoint, params = list()) {
  out <- inat_get(endpoint, params)
  out$total_results
}

get_year_histogram <- function(params, value_name) {
  out <- inat_get("/observations/histogram", params)
  
  enframe(
    unlist(out$results$year),
    name = "year_raw",
    value = value_name
  ) |>
    mutate(year = as.integer(str_extract(year_raw, "^\\d{4}"))) |>
    select(year, all_of(value_name)) |>
    arrange(year)
}





dead_by_year <- get_year_histogram(
  params = list(
    term_id = 17,
    term_value_id = 19,
    taxon_id = 1,            # Animalia
    date_field = "created",
    interval = "year",
    verifiable = "any"
  ),
  value_name = "dead_records"
)

animalia_by_year <- get_year_histogram(
  params = list(
    taxon_id = 1,            # Animalia
    date_field = "created",
    interval = "year",
    verifiable = "any"
  ),
  value_name = "animalia_records"
)

rate_df <- dead_by_year |>
  left_join(animalia_by_year, by = "year") |>
  mutate(
    dead_per_100k_animalia = dead_records / animalia_records * 100000,
    current_year = year == max(year)
  )

p_rate <- ggplot(rate_df, aes(x = year, y = dead_per_100k_animalia)) +
  geom_col(
    aes(alpha = current_year),
    fill = inat_green,
    colour = inat_dark,
    width = 0.8
  ) +
  scale_alpha_manual(values = c("FALSE" = 1, "TRUE" = 0.5), guide = "none") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "Mortality records relative to Animalia observations",
    subtitle = "Dead-annotated observations per 100,000 Animalia observations; current year is partial",
    x = "Year added to iNaturalist",
    y = "Dead-annotated observations per 100,000 Animalia observations"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

p_rate



summary_df <- tibble(
  total_observations = get_total_results(
    "/observations",
    list(per_page = 0, verifiable = "any")
  ),
  animalia_observations = get_total_results(
    "/observations",
    list(per_page = 0, verifiable = "any", taxon_id = 1)
  ),
  dead_observations = get_total_results(
    "/observations",
    list(per_page = 0, verifiable = "any", term_id = 17, term_value_id = 19)
  ),
  alive_observations = get_total_results(
    "/observations",
    list(per_page = 0, verifiable = "any", term_id = 17, term_value_id = 18)
  )
) |>
  mutate(
    alive_or_dead_annotated = alive_observations + dead_observations,
    pct_dead_all = dead_observations / total_observations,
    pct_dead_animalia = dead_observations / animalia_observations,
    pct_dead_annotated = dead_observations / alive_or_dead_annotated
  )

denominator_df <- tibble(
  denominator = c(
    "All iNaturalist observations",
    "Animalia observations",
    "Alive/Dead annotated observations"
  ),
  proportion_dead = c(
    summary_df$pct_dead_all,
    summary_df$pct_dead_animalia,
    summary_df$pct_dead_annotated
  )
) |>
  arrange(proportion_dead) |>
  mutate(denominator = factor(denominator, levels = denominator))

p_denom <- ggplot(denominator_df, aes(x = proportion_dead, y = denominator)) +
  geom_segment(
    aes(x = 0, xend = proportion_dead, y = denominator, yend = denominator),
    linewidth = 2,
    colour = inat_green,
    alpha = 0.7
  ) +
  geom_point(size = 6, colour = inat_green) +
  geom_text(
    aes(label = percent(proportion_dead, accuracy = 0.01)),
    hjust = -0.15,
    size = 4.5
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 0.1),
    limits = c(0, max(denominator_df$proportion_dead) * 1.3)
  ) +
  labs(
    title = "Mortality prevalence depends on the denominator",
    x = "Percentage annotated as dead",
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )

p_denom






taxa <- tribble(
  ~common_name, ~taxon_id,
  "Insects", 47158,
  "Molluscs", 47115,
  "Mammals", 40151,
  "Birds", 3,
  "Reptiles", 26036,
  "Ray-finned Fishes", 47178,
  "Amphibians", 20978,
  "Arachnids", 47119
)

taxon_year <- taxa |>
  mutate(
    data = map2(common_name, taxon_id, \(nm, tx) {
      get_year_histogram(
        params = list(
          term_id = 17,
          term_value_id = 19,
          taxon_id = tx,
          date_field = "created",
          interval = "year",
          verifiable = "any"
        ),
        value_name = "dead_records"
      ) |>
        mutate(common_name = nm)
    })
  ) |>
  select(data) |>
  unnest(data)

taxon_year_prop <- taxon_year |>
  group_by(year) |>
  mutate(
    total_year = sum(dead_records, na.rm = TRUE),
    proportion = dead_records / total_year
  ) |>
  ungroup()

p_taxon <- ggplot(taxon_year_prop, aes(x = factor(year), y = proportion, fill = common_name)) +
  geom_col(width = 0.9, colour = "white", linewidth = 0.15) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = taxon_cols) +
  labs(
    title = "Taxonomic composition of iNaturalist mortality records through time",
    subtitle = "Annual proportional composition of dead-annotated records",
    x = "Year added to iNaturalist",
    y = "Share of annual dead-annotated records",
    fill = "Taxon"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_taxon






years <- seq(2008, as.integer(format(Sys.Date(), "%Y")), by = 1)

species_accum_df <- tibble(
  year = years,
  created_d2 = paste0(year, "-12-31T23:59:59Z")
) |>
  mutate(
    n_species = map_int(created_d2, \(d2) {
      get_total_results(
        "/observations/species_counts",
        list(
          per_page = 0,
          verifiable = "any",
          term_id = 17,
          term_value_id = 19,
          created_d2 = d2
        )
      )
    })
  )

p_species_accum <- ggplot(species_accum_df, aes(x = year, y = n_species)) +
  geom_line(linewidth = 1.2, colour = inat_green) +
  geom_point(size = 2.5, colour = inat_dark) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(
    title = "Species accumulation of iNaturalist mortality records",
    subtitle = "Cumulative number of taxa with at least one dead-annotated observation",
    x = "Year added to iNaturalist",
    y = "Cumulative number of taxa"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

p_species_accum


# --- --- --- --- ---
# Compare old and new parquet file
# --- --- --- --- ---

parquet_path <- "https://huggingface.co/datasets/diegoellissoto/iNaturalist_mortality_records_12Apr2025/resolve/main/inat_all_Apr122025.parquet"
inat_all_raw <- arrow::read_parquet(parquet_path)
names(inat_all_raw)



new_parquet <- arrow::read_parquet('/Users/diegoellis/Downloads/inat_observations-2026-04-21.parquet')
names(new_parquet)

