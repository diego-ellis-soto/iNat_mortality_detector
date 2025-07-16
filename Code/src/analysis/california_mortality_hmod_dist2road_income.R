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

library(ggplot2)
library(units)
require(sp)
require(sf)
require(tidycensus)
require(tigris)
require(tidyverse)
options(tigris_use_cache = TRUE)
census_api_key('88f3fbb46861a0190c673e39c99bd3c067f15072', install = TRUE, overwrite = TRUE)
require(raster)
# https://gis.stackexchange.com/questions/119993/convert-line-shapefile-to-raster-value-total-length-of-lines-within-cell
# https://catalog.data.gov/dataset/enviroatlas-road-density-metrics-by-12-digit-huc-for-the-conterminous-united-states3
require(tigris)
options(tigris_use_cache = TRUE)
library(tidyverse)
library(sf)
library(tidycensus)
library(ggplot2)
library(tidyverse)
library(sf)
library(tidycensus)
library(ggplot2)
require(mapview)
require(gridExtra)
require(patchwork)
require(mapview)

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
range(inat$time_observed_at, na.rm=T)

# Step 2: Parse lat/lon from the "location" column
inat_sf <- inat %>%
  # Drop rows with no location
  filter(!is.na(location)) %>%
  # Split "location" into two new columns: "latitude" and "longitude"
  separate(location, into = c("latitude", "longitude"), sep = ",", convert = TRUE) %>%
  # Convert to sf points (coords = c("x", "y") => c("longitude", "latitude"))
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
# 4326 = WGS84 (lat/long)

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
ggsave(sampling_across_cali, file = 'outdir/mortality_sampling_cali.png')

# --- --- --- --- --- --- --- --- --- --- ---
# Distance to nearest road:
# --- --- --- --- --- --- --- --- --- --- ---

# For each point, get the index of the nearest road
nearest_idx <- st_nearest_feature(inat_sf_3310_cali_only, california_roads_3310)

# Extract geometry of the nearest roads
nearest_roads <- st_geometry(california_roads_3310)[nearest_idx]

# Calculate distance for each point to its nearest road (in meters)
inat_sf_3310_cali_only$dist_to_road_m <- st_distance(inat_sf_3310_cali_only, nearest_roads, by_element = TRUE)

library(units)

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
xlim( c(as.numeric(0), as.numeric(round(quantile(inat_sf_3310_cali_only$dist_to_road_m, 0.99)))  ) )
# theme(legend.position="none") # Remove legend

ggsave(dist2road, file = 'outdir/dist2road_cali.png')

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
ggsave(hmod_space_cali, file = '/Users/diegoellis/Desktop/Projects/Postdoc/hmod_space_cali.png')


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
ggsave(hmod_hist_cali, file = '/Users/diegoellis/Desktop/Projects/Postdoc/hmod_hist_cali_cali.png')

sampling_across_cali + hmod_hist_cali / dist2road


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Income
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
  save(state_income_age, file = '/Users/diegoellis/Desktop/Projects/Postdoc/state_income_age.Rdata')
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
  ggsave(ggplot_income, file = '/Users/diegoellis/Desktop/Projects/Postdoc/ggplot_income_cali.png')

    

  # For puma concolor
  # Bounding box:
  #   SW corner: (-64.0721995786728, -165.294524913526)
  # NE corner: (82.8806662722686, -24.5820567201415)
  # 
  
  
  
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
  
  # Save the plot to a file
  ggsave(hexbin_plot, filename = '/Users/diegoellis/Desktop/Projects/Postdoc/hexbin_roads_inat.png', width = 8, height = 6)
  
 
  st_coordinates(inat_near_roads)[,1]
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
  
  # Patchwork them together for Figure 2 of the MS:
  
  
  
  
  
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Puma:
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  
  puma = read.csv('/Users/diegoellis/Downloads/puma_concolor_inat_dead_ALL_2025-02-16.csv',
  stringsAsFactors = FALSE)
  
  View(head(inat))
  # "project_ids," "project_observations," "project_ids_with_curator_id," or "project_ids_without_curator_id,"
  
  library(raster)
  library(sf)
  library(tidyverse)
  library(ggplot2)
  library(viridis)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(cowplot)  # for arranging plots if needed
  
  # === 1. Load and Crop Raster Data ===
  
  # # Load a global DEM (replace with your DEM file path)
  # dem <- raster("/path/to/global_DEM.tif")
  # 
  # # Load a global human footprint raster (replace with your human footprint file path)
  # hfp <- raster("/path/to/human_footprint.tif")
  # 
  # # Define an extent for the Americas (rough bounds)
  # ext_americas <- extent(-170, -30, -60, 80)
  # 
  # # Crop both rasters to the Americas extent
  # dem_americas <- crop(dem, ext_americas)
  # hfp_americas <- crop(hfp, ext_americas)
  # 
  # # Convert rasters to data frames for ggplot
  # dem_df <- as.data.frame(dem_americas, xy = TRUE)
  # colnames(dem_df)[3] <- "elevation"
  # 
  # hfp_df <- as.data.frame(hfp_americas, xy = TRUE)
  # colnames(hfp_df)[3] <- "human_footprint"
  # 
  # === 2. Get Americas Boundaries ===
  
  americas <- ne_countries(continent = c("South America", 'North America', 'Central America'))
  
  # === 3. Load and Filter iNaturalist Puma Data ===
  
  # Filter for Puma concolor records (adjust field name if needed)
  puma_inat <- puma %>% 
    filter(grepl("Puma concolor", taxon.name, ignore.case = TRUE))
  
  # Convert to an sf object assuming the 'location' column holds "lat,lon"
  puma_sf <- puma_inat %>%
    filter(!is.na(location)) %>%
    separate(location, into = c("latitude", "longitude"), sep = ",", convert = TRUE) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  # WGS84
  
  # mapview(puma_sf)
  
  # === 4. Create the Map using ggplot ===
  
  puma_map <- ggplot() +
    # DEM background
    # geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +
    # scale_fill_viridis_c(option = "viridis", name = "Elevation (m)") +
    # # Overlay human footprint (red, with transparency)
    # geom_raster(data = hfp_df, aes(x = x, y = y, alpha = human_footprint),
    #             fill = "red") +
    # scale_alpha(range = c(0, 0.6), guide = "none") +
    # # Americas boundaries
    geom_sf(data = americas, fill = NA, color = "black", size = 0.5) +
    # Puma mortality points
    geom_sf(data = puma_sf, 
            color = "blue", size = 2, shape = 21, fill = "yellow") +
   #  coord_sf(xlim = c(-140, -110), ylim = c(30, 45)) +
    labs(
      title = "Puma Concolor Mortality Records\nOverlaid on DEM & Human Footprint of the Americas",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 14)
    )
  
  # Display the map
  print(puma_map)
  
  # === 5. Save the Plot ===
  
  ggsave(puma_map, filename = "/Users/diegoellis/Desktop/Projects/Postdoc/puma_americas_map.png",
         width = 12, height = 8)

  
  
  countries <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Make sure your puma_sf is in the same CRS as the countries layer.
  # Both should be in WGS84 (EPSG:4326) but if not, transform:
  puma_sf <- st_transform(puma_sf, crs = st_crs(countries))
  
  # Perform a spatial join. This will add the country info (like the 'admin' field)
  puma_sf_country <- st_join(puma_sf, countries[, c("admin", "iso_a2")])
  
  # Check the unique countries found
  unique_countries <- unique(puma_sf_country$admin)
  print(unique_countries)
  
  table(puma_sf_country$admin)
  
  puma_metric <- st_transform(puma_sf_country, 3857)
  
  # Compute the distance matrix (in meters)
  dist_matrix <- st_distance(puma_metric)
  
  # Find the maximum distance
  max_distance <- max(dist_matrix)
  print(max_distance)
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Latin America - Animalia   
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  
  animalia = read.csv('/Users/diegoellis/Downloads/animalia_2023_LA.csv')

  table(animalia$taxon.conservation_status.status_name)

    
  animalia |> filter(taxon.conservation_status.status_name == 'critically endangered') |>
    distinct(taxon.name)
  
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Puma country
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  
  
  library(ggplot2)
  library(ggimage)
  # 
  # # Create a data frame with counts and flag URLs
  # df <- data.frame(
  #   country = c("Argentina", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "French Guiana", "Mexico", "United States"),
  #   count = c(17, 3, 4, 4, 1, 2, 1, 9, 44)
  # )
  # 
  # # Adding flag image URLs (using 40px width images from flagcdn.com)
  # df$flag <- c("https://flagcdn.com/w40/ar.png", 
  #              "https://flagcdn.com/w40/br.png",
  #              "https://flagcdn.com/w40/ca.png",
  #              "https://flagcdn.com/w40/cl.png",
  #              "https://flagcdn.com/w40/co.png",
  #              "https://flagcdn.com/w40/cr.png",
  #              "https://flagcdn.com/w40/gf.png",  # French Guiana
  #              "https://flagcdn.com/w40/mx.png",
  #              "https://flagcdn.com/w40/us.png")
  # 
  # # Create the barplot with flags above the bars.
  # puma_death = ggplot(df, aes(x = reorder(country, count), y = count)) +
  #   geom_bar(stat = "identity", fill = "grey") +
  #   # Place flag images slightly above each bar; adjust 'size' as needed.
  #   geom_image(aes(y = count + 1, image = flag), size = 0.08) +
  #   coord_flip() +
  #   labs(x = "Country", y = "Death Count",
  #        title = "Mountain Lion Deaths (2022-2025)")+
  #   theme(
  #     axis.text.x = element_text(face = "bold", size = 16, color = "black"),
  #     axis.text.y = element_text(face = "bold", size = 16, color = "black"),
  #     axis.title.x = element_text(face = "bold", size = 16, color = "black"),
  #     axis.title.y = element_text(face = "bold", size = 16, color = "black"),
  #     plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "black"),  # Bold title, centered
  #     legend.text = element_text(face = "bold", size = 14, color = "black"),  # Bold legend text
  #     legend.title = element_text(face = "bold", size = 16, color = "black")  # Bold legend title
  #   )+theme_bw()+
  #   geom_text(aes(y = count + 5, label = count), size = 5, fontface = "bold")
  # ggsave(puma_death, file = '/Users/diegoellis/Desktop/Projects/Postdoc/puma_death.png')
  # 
  # 
  
  library(ggplot2)
  library(ggimage)
  
  # Create a data frame with counts and flag URLs
  df <- data.frame(
    country = c("Argentina", "Brazil", "Canada", "Chile", "Colombia", "Costa Rica", "French Guiana", "Mexico", "United States"),
    count = c(17, 3, 4, 4, 1, 2, 1, 9, 44)
  )
  
  # Adding flag image URLs
  df$flag <- c("https://flagcdn.com/w40/ar.png", 
               "https://flagcdn.com/w40/br.png",
               "https://flagcdn.com/w40/ca.png",
               "https://flagcdn.com/w40/cl.png",
               "https://flagcdn.com/w40/co.png",
               "https://flagcdn.com/w40/cr.png",
               "https://flagcdn.com/w40/gf.png",  # French Guiana
               "https://flagcdn.com/w40/mx.png",
               "https://flagcdn.com/w40/us.png")
  
  # Create the barplot
  puma_death <- ggplot(df, aes(x = reorder(country, count), y = count)) +
    geom_bar(stat = "identity", fill = "grey") +
    # Place flag images slightly above each bar
    geom_image(aes(y = count + 1, image = flag), size = 0.08) +
    coord_flip() +
    labs(x = "Country", y = "Death Count",
         title = "Mountain Lion Deaths (2022-2025)") +
    theme_bw() +  # Use a clean black & white theme
    theme(
      axis.text.x = element_text(face = "bold", size = 16, color = "black"),
      axis.text.y = element_text(face = "bold", size = 16, color = "black"),  # Country names bold & black
      axis.title.x = element_text(face = "bold", size = 16, color = "black"),
      axis.title.y = element_text(face = "bold", size = 16, color = "black"),
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),  # Bold title, centered
      legend.text = element_text(face = "bold", size = 14, color = "black"),
      legend.title = element_text(face = "bold", size = 16, color = "black")
    ) +
    # Move count labels further to the right for clarity
    geom_text(aes(y = count + 5, label = count), size = 6, fontface = "bold", color = "black")
  
  # Save the plot as a high-quality image
  ggsave(puma_death, file = "/Users/diegoellis/Desktop/Projects/Postdoc/puma_death.png", width = 10, height = 6, dpi = 300)
  
  
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  # Latin America - Animalia   
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
  
  
  inat <- read.csv("/Users/diegoellis/Downloads/inat_dead_ALL_2025-02-27.csv", 
                   stringsAsFactors = FALSE)
  
  
  require(dplyr)

  # Count records per day
  date_counts <- inat %>%
    group_by(observed_on) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) |>
    head(1)

  