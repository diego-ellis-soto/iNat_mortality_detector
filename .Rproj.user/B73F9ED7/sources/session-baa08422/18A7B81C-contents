
# Add avonet and panthera as traits ####

# landuse type 

# https://www.inaturalist.org/observations/106026250

#################################################
# waterfowl_mortality_analysis.R
# ----------------------------------------------
# This script retrieves "Dead" birds or mammals from iNaturalist,
# within a specified place (ID) or bounding box, for specified years.
# It generates:
#   - Daily mortality time-series plot
#   - Top-20 species bar chart
#   - Hotspots map of top 90th percentile mortality days
# It also saves the retrieved data and plots to a specified output directory.
#
# Diego Ellis Soto
# diego.ellissoto@berkeley.edu
# January 2025
#################################################

### Required Packages
required_packages <- c(
  "httr", "jsonlite", "tidyverse", "glue", "lubridate",
  "wesanderson", "viridis"
)

installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(glue)
library(lubridate)
library(wesanderson)
library(viridis)

#########################################################
# A) fetch_dead_data_once(): Single date-range query
#    Uses correct iNaturalist API parameters:
#      q=Dead, term_id=17, term_value_id=19, iconic_taxa
#########################################################
fetch_dead_data_once <- function(
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    start_date,
    end_date,
    iconic_taxa = "Aves",  # "Aves" or "Mammalia"
    per_page    = 1000,
    max_pages   = 50
) {
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  # Build query for "Dead" observations
  query_params <- glue(
    "iconic_taxa={iconic_taxa}&",  # Birds or Mammals
   #   "q=Dead&",                    # Search keyword "Dead" # In uncommented this just to make sure im not loosing data?!?!?! # ID why this can be commented out ! 
    "term_id=17&",                # "Alive or Dead" annotation
    "term_value_id=19&",          # "Dead" value
    "verifiable=true&",
    "d1={start_date}&d2={end_date}&",
    "order=desc&order_by=created_at&",
    "per_page={per_page}"
  )
  
  # Build location parameter
  loc_part <- ""
  if (!is.null(place_id)) {
    loc_part <- glue("&place_id={place_id}")
  } else if (!is.null(swlat) && !is.null(swlng) &&
             !is.null(nelat) && !is.null(nelng)) {
    loc_part <- glue("&nelat={nelat}&nelng={nelng}&swlat={swlat}&swlng={swlng}")
  } else {
    stop("Must provide either 'place_id' OR bounding box (swlat, swlng, nelat, nelng).")
  }
  
  observations_list <- list()
  current_page <- 1
  
  while (current_page <= max_pages) {
    query_url <- paste0(base_url, "?", query_params,
                        "&page=", current_page, loc_part)
    
    message("Fetching page ", current_page,
            " for date range [", start_date, " to ", end_date, "]:\n", query_url)
    
    resp <- GET(query_url)
    if (http_error(resp)) {
      warning("HTTP error on page ", current_page, ": ", status_code(resp))
      break
    }
    
    parsed <- content(resp, as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE)
    
    if (length(parsed$results) == 0) {
      message("No more results at page ", current_page)
      break
    }
    
    obs_page_df <- as_tibble(parsed$results)
    observations_list[[current_page]] <- obs_page_df
    
    if (nrow(obs_page_df) < per_page) {
      message("Reached last page of results at page ", current_page)
      break
    }
    
    current_page <- current_page + 1
    Sys.sleep(1)  # polite pause for API
  }
  
  observations_all <- bind_rows(observations_list)
  return(observations_all)
}

#########################################################
# B) fetch_dead_data_monthly():
#    For one YEAR, split into 12 monthly intervals.
#########################################################
fetch_dead_data_monthly <- function(
    year,
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    iconic_taxa = "Aves"   # "Aves" or "Mammalia"
) {
  monthly_list <- list()
  
  for (month_i in 1:12) {
    start_date <- as.Date(glue("{year}-{sprintf('%02d', month_i)}-01"))
    end_date   <- start_date %m+% months(1) %m-% days(1)
    
    if (year(start_date) != year) break
    
    message("\n--- Querying year ", year, ", month ", month_i, " ---")
    
    df_month <- fetch_dead_data_once(
      place_id   = place_id,
      swlat      = swlat,
      swlng      = swlng,
      nelat      = nelat,
      nelng      = nelng,
      start_date = start_date,
      end_date   = end_date,
      iconic_taxa= iconic_taxa
    )
    monthly_list[[month_i]] <- df_month
  }
  
  year_df <- bind_rows(monthly_list)
  return(year_df)
}

#########################################################
# C) getDeadVertebrates_monthlyLoop():
#    Integrates the monthly data to produce:
#      - daily_plot
#      - top_species_plot
#      - map_hotspots_gg
#      - merged_df (top 90% daily mortality)
#########################################################
getDeadVertebrates_monthlyLoop <- function(
    years       = c(2022, 2023),
    # Either place_id OR bounding box (swlat, swlng, nelat, nelng)
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    iconic_taxa = "Aves",  # "Aves" or "Mammalia"
    per_page    = 1000,
    max_pages   = 50,
    outdir      = NULL
) {
  # 1) Gather monthly data for each year
  all_years_list <- list()
  for (yr in years) {
    message("\n========= YEAR: ", yr, " ==========\n")
    yr_df <- fetch_dead_data_monthly(
      year       = yr,
      place_id   = place_id,
      swlat      = swlat,
      swlng      = swlng,
      nelat      = nelat,
      nelng      = nelng,
      iconic_taxa= iconic_taxa
    ) %>%
      mutate(Window = as.character(yr))
    
    all_years_list[[as.character(yr)]] <- yr_df
  }
  
  # Combine data for all years
  merged_df_all <- bind_rows(all_years_list)
  
  # If no data, return empty plots
  if (!"created_at_details.date" %in% names(merged_df_all) ||
      nrow(merged_df_all) == 0) {
    daily_plot <- ggplot() +
      labs(title = "No 'Dead' Observations Found", x = NULL, y = NULL) +
      theme_void()
    
    top_species_plot <- ggplot() +
      labs(title = "No species data", x = NULL, y = NULL) +
      theme_void()
    
    map_hotspots_gg <- ggplot() +
      labs(title = "No data for hotspots map") +
      theme_void()
    
    return(list(
      merged_df        = merged_df_all,  # empty
      daily_plot       = daily_plot,
      top_species_plot = top_species_plot,
      map_hotspots_gg  = map_hotspots_gg
    ))
  }
  
  # Optionally save all raw data
  if (!is.null(outdir)) {
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    readr::write_csv(merged_df_all, file.path(outdir, "merged_df_top_all_data.csv"))
  }
  
  # 2) Daily time-series
  counts_by_day <- merged_df_all %>%
    mutate(obs_date = as.Date(`created_at_details.date`)) %>%
    group_by(Window, obs_date) %>%
    summarise(n = n(), .groups = "drop")
  
  # daily_plot
  n_windows  <- length(unique(counts_by_day$Window))
  wes_colors <- wes_palette("Zissou1", n_windows, type = "discrete")
  
  daily_plot <- ggplot(counts_by_day, aes(x = obs_date, y = n, color = Window)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = wes_colors) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      title = glue("Daily 'Dead' Observations (Years {paste(years, collapse=', ')})"),
      x     = "Month",
      y     = "Number of Observations",
      color = "Year"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 3) Top-20 species bar plot
  if ("taxon.name" %in% names(merged_df_all)) {
    species_counts <- merged_df_all %>%
      filter(!is.na(taxon.name)) %>%
      group_by(Window, taxon.name) %>%
      summarise(dead_count = n(), .groups = "drop")
    
    top_species_overall <- species_counts %>%
      group_by(taxon.name) %>%
      summarise(total_dead = sum(dead_count)) %>%
      arrange(desc(total_dead)) %>%
      slice_head(n = 20)
    
    species_top20 <- species_counts %>%
      filter(taxon.name %in% top_species_overall$taxon.name)
    
    top_species_plot <- ggplot(species_top20, aes(
      x   = reorder(taxon.name, -dead_count),
      y   = dead_count,
      fill= Window
    )) +
      geom_col(position = position_dodge(width = 0.7)) +
      coord_flip() +
      scale_fill_manual(values = wes_colors) +
      labs(
        title = "Top 20 Species with 'Dead' Observations",
        x     = "Species",
        y     = "Number of Dead Observations",
        fill  = "Year"
      ) +
      theme_minimal(base_size = 14)
  } else {
    top_species_plot <- ggplot() +
      labs(title = "No 'taxon.name' column found", x = NULL, y = NULL) +
      theme_void()
  }
  
  # 4) Identify top 90th percentile daily mortality => "hotspots"
  daily_quantile <- quantile(counts_by_day$n, probs = 0.90, na.rm = TRUE)
  high_mortality_days <- counts_by_day %>%
    filter(n >= daily_quantile) %>%
    pull(obs_date)
  
  merged_high <- merged_df_all %>%
    mutate(obs_date = as.Date(`created_at_details.date`)) %>%
    filter(obs_date %in% high_mortality_days)
  
  # Convert "location" col to lat/lon if present
  if ("location" %in% names(merged_high)) {
    location_df <- merged_high %>%
      filter(!is.na(location) & location != "") %>%
      separate(location, into = c("lat_str", "lon_str"), sep = ",", remove = FALSE) %>%
      mutate(
        latitude  = as.numeric(lat_str),
        longitude = as.numeric(lon_str)
      )
    
    if (nrow(location_df) == 0) {
      map_hotspots_gg <- ggplot() +
        labs(title = "No data in top 90th percentile days with valid location") +
        theme_void()
    } else {
      min_lon <- min(location_df$longitude, na.rm = TRUE)
      max_lon <- max(location_df$longitude, na.rm = TRUE)
      min_lat <- min(location_df$latitude,  na.rm = TRUE)
      max_lat <- max(location_df$latitude,  na.rm = TRUE)
      
      map_hotspots_gg <- ggplot(location_df, aes(x = longitude, y = latitude, color = Window)) +
        borders("world", fill = "gray80", colour = "white") +
        geom_point(alpha = 0.6, size = 2) +
        scale_color_manual(values = wes_colors) +
        coord_quickmap(xlim = c(min_lon, max_lon),
                       ylim = c(min_lat, max_lat),
                       expand = TRUE) +
        labs(
          title = glue("Top 90th percentile mortality days ({paste(years, collapse=', ')})"),
          x     = "Longitude",
          y     = "Latitude",
          color = "Year"
        ) +
        theme_minimal(base_size = 14)
    }
  } else {
    map_hotspots_gg <- ggplot() +
      labs(title = "No 'location' column for top 90% days map") +
      theme_void()
  }
  
  # 5) If outdir specified, save plots + CSV
  if (!is.null(outdir)) {
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    
    # Save the top-90% CSV
    readr::write_csv(merged_high, file.path(outdir, "merged_df_top90.csv"))
    
    # daily_plot
    ggsave(file.path(outdir, "daily_plot.png"),
           daily_plot, width = 8, height = 5, dpi = 300)
    # top_species_plot
    ggsave(file.path(outdir, "top_species_plot.png"),
           top_species_plot, width = 7, height = 7, dpi = 300)
    # map_hotspots_gg
    ggsave(file.path(outdir, "map_hotspots.png"),
           map_hotspots_gg, width = 8, height = 5, dpi = 300)
  }
  
  # 6) Return results
  return(list(
    merged_df        = merged_high,   # top 90% days
    daily_plot       = daily_plot,
    top_species_plot = top_species_plot,
    map_hotspots_gg  = map_hotspots_gg,
    daily_90th_quant = daily_quantile
  ))
}

#########################################################
# D) Example Usage
#########################################################

########### 1) All Birds in Canada, 2021-2025 ###########
res_canada_birds <- getDeadVertebrates_monthlyLoop(
  years       = 2021:2025,
  place_id    = 6712,   # Canada
  iconic_taxa = "Aves",
  outdir      = "outputs/canada_birds"
)

########### 2) All Birds in the USA, 2021-2025 ###########
res_usa_birds <- getDeadVertebrates_monthlyLoop(
  years       = 2021:2025,
  place_id    = 1,      # USA
  iconic_taxa = "Aves",
  outdir      = "outputs/usa_birds"
)

########### 3) All Mammals in Canada, 2021-2025 ###########
res_canada_mammals <- getDeadVertebrates_monthlyLoop(
  years       = 2021:2025,
  place_id    = 6712,
  iconic_taxa = "Mammalia",
  outdir      = "outputs/canada_mammals"
)

########### 4) All Mammals in the USA, 2021-2025 ###########
res_usa_mammals <- getDeadVertebrates_monthlyLoop(
  years       = 2021:2025,
  place_id    = 1,
  iconic_taxa = "Mammalia",
  outdir      = "outputs/usa_mammals"
)

########### 5) Birds in California Bounding Box ###########
#   bounding box approximate:
#     SW corner: (32.5, -124.5)
#     NE corner: (42.0, -114.1)
res_ca_birds <- getDeadVertebrates_monthlyLoop(
  years       = 2021:2025,
  swlat       = 32.5,
  swlng       = -124.5,
  nelat       = 42.0,
  nelng       = -114.1,
  iconic_taxa = "Aves",
  outdir      = "outputs/california_birds"
)

########### 6) Mammals in California Bounding Box ###########
res_ca_mammals <- getDeadVertebrates_monthlyLoop(
  years       = 2021:2025,
  swlat       = 32.5,
  swlng       = -124.5,
  nelat       = 42.0,
  nelng       = -114.1,
  iconic_taxa = "Mammalia",
  outdir      = "outputs/california_mammals"
)

########### 7) (Optional) Waterfowl (Anatidae) in California ###########
# If you want to further subset to just waterfowl (Anatidae) from the returned data:
res_ca_birds_all <- getDeadVertebrates_monthlyLoop(
  years       = 2021:2025,
  swlat       = 32.5,
  swlng       = -124.5,
  nelat       = 42.0,
  nelng       = -114.1,
  iconic_taxa = "Aves",
  outdir      = "outputs/california_all_birds"
)

# Now filter the returned merged_df by "Anatidae" in 'taxon.family' or 'taxon.family_name'
if ("taxon.family" %in% names(res_ca_birds_all$merged_df)) {
  waterfowl_df <- res_ca_birds_all$merged_df %>%
    filter(taxon.family == "Anatidae")
} else if ("taxon.family_name" %in% names(res_ca_birds_all$merged_df)) {
  waterfowl_df <- res_ca_birds_all$merged_df %>%
    filter(taxon.family_name == "Anatidae")
} else {
  message("No family column found, can't filter for Anatidae.")
  waterfowl_df <- tibble()
}

cat("Number of waterfowl (Anatidae) in top 90% days for CA birds:", nrow(waterfowl_df), "\n")

### End of Script


table(res_ca_birds$merged_df$id %in% 144990634)
table(res_ca_birds$merged_df$quality_grade)
