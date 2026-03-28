# Next steps -> Ask Lizzy About page and the How to Use page
# Ask Carl for feedback
# Ask Avery for feedback
# Threatened species -> Use the life query critically endangered species
# Ask Avery -> Could be fun with taxonomy plot? but not needed
# Redo every case study
# Try out different shapes
####################################################################################################
# app.R
#
# Dead Wildlife Observations from iNaturalist
#
# FULL INTEGRATED SHINY APP
#
# --------------------------------------------------------------------------------------------------
# PURPOSE
# --------------------------------------------------------------------------------------------------
# This application supports exploration of dead wildlife observations from iNaturalist in both:
#   1) archived mode (remote parquet snapshot)
#   2) live mode (dynamic iNaturalist API queries)
#
# The app is designed for near real-time ecological surveillance, exploratory analysis, and
# hypothesis generation around wildlife mortality patterns.
#
# --------------------------------------------------------------------------------------------------
# MAJOR FEATURES
# --------------------------------------------------------------------------------------------------
# 1. Archived parquet mode
# 2. Live iNaturalist API mode
# 3. Adaptive live querying
#    - starts with weekly windows
#    - if a weekly window appears saturated, the app splits to day windows
#    - if a day window appears saturated, the app splits to 12-hour windows
#    - progress bar reports these transitions
# 4. Upload study area from:
#    - zipped shapefile
#    - zipped .gdb folder
#    - .gpkg
#    - .geojson
# 5. Draw rectangle as alternative study area selector
# 6. Adaptive time-series axis handling
# 7. Time-series metrics:
#    - raw counts
#    - unique observers
#    - observations per observer
# 8. Time aggregation:
#    - day
#    - week
#    - month
# 9. Smoother toggle
# 10. Anomaly highlighting
# 11. Top species panel
# 12. Interactive clustered map
# 13. Static hexbin hotspot map
# 14. Filtering by:
#     - taxonomy
#     - conservation / threat status
# 15. Strict archived and live threat-status filtering using:
#       taxon.conservation_status.status_name
# 16. Query diagnostics
# 17. Query metadata download
# 18. Expanded About and How to Use pages
# 19. UI warnings about archived vs live threat-status filtering limitations
#
# --------------------------------------------------------------------------------------------------
# IMPORTANT THREAT-STATUS LOGIC
# --------------------------------------------------------------------------------------------------
# Live mode:
#   - threat-status filtering works most reliably in LIVE mode
#   - the live query may use a broad threatened flag to widen candidate retrieval
#   - AFTER retrieval, records are strictly post-filtered using:
#       taxon.conservation_status.status_name
#   - this prevents CR / EN / VU selections from collapsing into a broad threatened pool
#
# Archived mode:
#   - archived mode can still use taxon.conservation_status.status_name if that field is present
#   - however, archived snapshots may have different metadata structure and may not retain
#     conservation-status information consistently across records
#   - therefore, threat-status filtering in archived mode should be treated as LIMITED relative
#     to live mode
#
####################################################################################################

####################################################################################################
# PACKAGES
####################################################################################################

required_packages <- c(
  "httr",
  "jsonlite",
  "tidyverse",
  "glue",
  "lubridate",
  "viridis",
  "hexbin",
  "shinycssloaders",
  "DT",
  "maps",
  "mapdata",
  "leaflet",
  "leaflet.extras",
  "shinythemes",
  "shiny",
  "arrow",
  "sf",
  "cowplot",
  "stringr",
  "scales",
  "shinyjs"
)

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(httr)
library(jsonlite)
library(tidyverse)
library(glue)
library(lubridate)
library(viridis)
library(hexbin)
library(shinycssloaders)
library(DT)
library(maps)
library(mapdata)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(shiny)
library(arrow)
library(sf)
library(cowplot)
library(stringr)
library(scales)
library(shinyjs)

####################################################################################################
# GLOBAL OPTIONS
####################################################################################################

options(shiny.maxRequestSize = 100 * 1024^2)

# addResourcePath(
#   prefix = "assets",
#   directoryPath = file.path(getwd(), "assets/www")
# )

parquet_path <- "https://huggingface.co/datasets/diegoellissoto/iNaturalist_mortality_records_12Apr2025/resolve/main/inat_all_Apr122025.parquet"

####################################################################################################
# GENERIC COLUMN HELPERS
####################################################################################################

detect_first_existing_col <- function(df, candidates) {
  existing <- candidates[candidates %in% names(df)]
  if (length(existing) == 0) return(NULL)
  existing[1]
}

####################################################################################################
# COLUMN STANDARDIZATION
####################################################################################################

standardize_taxonomy_columns <- function(df) {
  
  scientific_col <- detect_first_existing_col(
    df,
    c(
      "scientific_name",
      "taxon.name",
      "taxon_name"
    )
  )
  
  family_col <- detect_first_existing_col(
    df,
    c(
      "taxon_family_name",
      "family",
      "family_name"
    )
  )
  
  order_col <- detect_first_existing_col(
    df,
    c(
      "taxon_order_name",
      "order",
      "order_name"
    )
  )
  
  iconic_col <- detect_first_existing_col(
    df,
    c(
      "iconic_taxon_name",
      "iconic_taxon",
      "taxon.iconic_taxon_name"
    )
  )
  
  observer_col <- detect_first_existing_col(
    df,
    c(
      "user.id",
      "user_id",
      "user.login",
      "user_login",
      "user.name",
      "user_name",
      "observer_id"
    )
  )
  
  common_name_col <- detect_first_existing_col(
    df,
    c(
      "taxon.preferred_common_name",
      "common_name",
      "species_guess"
    )
  )
  
  taxon_id_col <- detect_first_existing_col(
    df,
    c(
      "taxon.id",
      "taxon_id"
    )
  )
  
  if (!is.null(scientific_col) && !"scientific_name_std" %in% names(df)) {
    df$scientific_name_std <- as.character(df[[scientific_col]])
  }
  
  if (!is.null(family_col) && !"family_name_std" %in% names(df)) {
    df$family_name_std <- as.character(df[[family_col]])
  }
  
  if (!is.null(order_col) && !"order_name_std" %in% names(df)) {
    df$order_name_std <- as.character(df[[order_col]])
  }
  
  if (!is.null(iconic_col) && !"iconic_taxon_name_std" %in% names(df)) {
    df$iconic_taxon_name_std <- as.character(df[[iconic_col]])
  }
  
  if (!is.null(observer_col) && !"observer_std" %in% names(df)) {
    df$observer_std <- as.character(df[[observer_col]])
  }
  
  if (!is.null(common_name_col) && !"common_name_std" %in% names(df)) {
    df$common_name_std <- as.character(df[[common_name_col]])
  }
  
  if (!is.null(taxon_id_col) && !"taxon_id_std" %in% names(df)) {
    df$taxon_id_std <- df[[taxon_id_col]]
  }
  
  df
}

standardize_date_column <- function(df) {
  if ("observed_on" %in% names(df)) {
    df <- df %>% mutate(observed_on = as.Date(observed_on))
  }
  df
}

standardize_coordinate_columns <- function(df) {
  
  if (!("latitude" %in% names(df) && "longitude" %in% names(df))) {
    if ("location" %in% names(df)) {
      tmp <- df %>%
        mutate(location = as.character(location)) %>%
        tidyr::separate(
          col = location,
          into = c("lat_str", "lon_str"),
          sep = ",",
          remove = FALSE,
          fill = "right"
        )
      
      if (!"latitude" %in% names(tmp)) {
        tmp$latitude <- suppressWarnings(as.numeric(tmp$lat_str))
      }
      
      if (!"longitude" %in% names(tmp)) {
        tmp$longitude <- suppressWarnings(as.numeric(tmp$lon_str))
      }
      
      df <- tmp
    }
  }
  
  df
}

# Threat-status standardization
# Priority is given to the exact flattened columns:
#   taxon.conservation_status.authority
#   taxon.conservation_status.status
#   taxon.conservation_status.status_name
standardize_status_columns <- function(df) {
  
  authority_col <- detect_first_existing_col(
    df,
    c(
      "taxon.conservation_status.authority",
      "conservation_status.authority"
    )
  )
  
  status_code_col <- detect_first_existing_col(
    df,
    c(
      "taxon.conservation_status.status",
      "conservation_status.status"
    )
  )
  
  status_name_col <- detect_first_existing_col(
    df,
    c(
      "taxon.conservation_status.status_name",
      "conservation_status.status_name",
      "iucn_status",
      "iucn_category",
      "red_list_category",
      "conservation_status",
      "status",
      "threat_status",
      "threatened_status"
    )
  )
  
  if (!is.null(authority_col) && !"threat_authority_raw" %in% names(df)) {
    df$threat_authority_raw <- as.character(df[[authority_col]])
  }
  
  if (!is.null(status_code_col) && !"threat_status_code_raw" %in% names(df)) {
    df$threat_status_code_raw <- as.character(df[[status_code_col]])
  }
  
  if (!is.null(status_name_col) && !"threat_status_raw" %in% names(df)) {
    df$threat_status_raw <- as.character(df[[status_name_col]])
  }
  
  if (!is.null(status_name_col) && !"threat_status_std" %in% names(df)) {
    
    raw_status <- str_trim(as.character(df[[status_name_col]]))
    raw_upper  <- toupper(raw_status)
    
    df$threat_status_std <- dplyr::case_when(
      raw_upper %in% c("LEAST CONCERN", "LC") ~ "LC",
      raw_upper %in% c("NEAR THREATENED", "NT") ~ "NT",
      raw_upper %in% c("VULNERABLE", "VU") ~ "VU",
      raw_upper %in% c("ENDANGERED", "EN") ~ "EN",
      raw_upper %in% c("CRITICALLY ENDANGERED", "CR") ~ "CR",
      raw_upper %in% c("EXTINCT IN THE WILD", "EW") ~ "EW",
      raw_upper %in% c("EXTINCT", "EX") ~ "EX",
      raw_upper %in% c("DATA DEFICIENT", "DD") ~ "DD",
      raw_upper %in% c("NOT EVALUATED", "NE") ~ "NE",
      TRUE ~ raw_upper
    )
  }
  
  if (!"threat_authority_std" %in% names(df) && "threat_authority_raw" %in% names(df)) {
    df$threat_authority_std <- str_trim(as.character(df$threat_authority_raw))
  }
  
  df
}

standardize_inat_columns <- function(df) {
  df %>%
    standardize_taxonomy_columns() %>%
    standardize_date_column() %>%
    standardize_coordinate_columns() %>%
    standardize_status_columns()
}

####################################################################################################
# TIME HELPERS
####################################################################################################

make_time_bin <- function(date_vec, time_bin = "day") {
  if (time_bin == "week") {
    return(floor_date(date_vec, unit = "week", week_start = 1))
  }
  if (time_bin == "month") {
    return(floor_date(date_vec, unit = "month"))
  }
  as.Date(date_vec)
}

make_adaptive_date_scale <- function(start_date, end_date, time_bin = "day") {
  
  if (time_bin == "month") {
    span_days <- as.numeric(as.Date(end_date) - as.Date(start_date))
    if (span_days <= 730) {
      return(
        scale_x_date(
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = expansion(mult = c(0.01, 0.02))
        )
      )
    } else {
      return(
        scale_x_date(
          date_labels = "%Y",
          date_breaks = "1 year",
          expand = expansion(mult = c(0.01, 0.02))
        )
      )
    }
  }
  
  if (time_bin == "week") {
    span_days <- as.numeric(as.Date(end_date) - as.Date(start_date))
    if (span_days <= 120) {
      return(
        scale_x_date(
          date_labels = "%b %d",
          date_breaks = "1 week",
          expand = expansion(mult = c(0.01, 0.02))
        )
      )
    } else if (span_days <= 730) {
      return(
        scale_x_date(
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = expansion(mult = c(0.01, 0.02))
        )
      )
    } else {
      return(
        scale_x_date(
          date_labels = "%Y",
          date_breaks = "1 year",
          expand = expansion(mult = c(0.01, 0.02))
        )
      )
    }
  }
  
  span_days <- as.numeric(as.Date(end_date) - as.Date(start_date))
  if (is.na(span_days)) span_days <- 30
  
  if (span_days <= 10) {
    return(scale_x_date(date_labels = "%b %d", date_breaks = "1 day", expand = expansion(mult = c(0.01, 0.02))))
  }
  if (span_days <= 45) {
    return(scale_x_date(date_labels = "%b %d", date_breaks = "3 days", expand = expansion(mult = c(0.01, 0.02))))
  }
  if (span_days <= 120) {
    return(scale_x_date(date_labels = "%b %d", date_breaks = "1 week", expand = expansion(mult = c(0.01, 0.02))))
  }
  if (span_days <= 400) {
    return(scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", expand = expansion(mult = c(0.01, 0.02))))
  }
  if (span_days <= 1095) {
    return(scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", expand = expansion(mult = c(0.01, 0.02))))
  }
  
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = expansion(mult = c(0.01, 0.02)))
}

####################################################################################################
# STUDY AREA HELPERS
####################################################################################################

read_uploaded_study_area <- function(uploaded_file, uploaded_name) {
  ext <- tolower(tools::file_ext(uploaded_name))
  
  if (ext == "gpkg") {
    return(sf::st_read(uploaded_file, quiet = TRUE))
  }
  
  if (ext == "geojson") {
    return(sf::st_read(uploaded_file, quiet = TRUE))
  }
  
  if (ext == "zip") {
    unzip_dir <- tempfile(pattern = "study_area_")
    dir.create(unzip_dir)
    utils::unzip(uploaded_file, exdir = unzip_dir)
    
    shp_files <- list.files(
      unzip_dir,
      pattern = "\\.shp$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(shp_files) > 0) {
      return(sf::st_read(shp_files[1], quiet = TRUE))
    }
    
    gdb_dirs <- list.dirs(unzip_dir, recursive = TRUE, full.names = TRUE)
    gdb_dirs <- gdb_dirs[grepl("\\.gdb$", gdb_dirs, ignore.case = TRUE)]
    
    if (length(gdb_dirs) > 0) {
      gdb_layers <- sf::st_layers(gdb_dirs[1])
      if (length(gdb_layers$name) == 0) {
        stop("A .gdb folder was found, but no readable layers were detected.")
      }
      return(sf::st_read(dsn = gdb_dirs[1], layer = gdb_layers$name[1], quiet = TRUE))
    }
    
    stop("The uploaded .zip did not contain a readable .shp file or .gdb folder.")
  }
  
  stop("Unsupported file type. Please upload a .zip shapefile, zipped .gdb, .gpkg, or .geojson file.")
}

prepare_uploaded_study_area <- function(study_area) {
  if (is.null(study_area) || nrow(study_area) == 0) return(NULL)
  
  tryCatch(
    {
      if (is.na(sf::st_crs(study_area))) {
        sf::st_set_crs(study_area, 4326)
      } else {
        sf::st_transform(study_area, 4326)
      }
    },
    error = function(e) NULL
  )
}

bbox_from_sf <- function(sf_obj) {
  bb <- sf::st_bbox(sf_obj)
  c(
    as.numeric(bb["ymin"]),
    as.numeric(bb["xmin"]),
    as.numeric(bb["ymax"]),
    as.numeric(bb["xmax"])
  )
}

####################################################################################################
# FILTERING HELPERS
####################################################################################################

filter_by_taxonomy <- function(
    df,
    query_type,
    iconic_taxon = NULL,
    order_name = NULL,
    family_name = NULL,
    taxon_name_input = NULL
) {
  
  df <- standardize_taxonomy_columns(df)
  
  if (query_type == "iconic" && !is.null(iconic_taxon) && nzchar(iconic_taxon) && "iconic_taxon_name_std" %in% names(df)) {
    df <- df %>% filter(str_to_lower(iconic_taxon_name_std) == str_to_lower(iconic_taxon))
  }
  
  if (query_type == "order" && !is.null(order_name) && nzchar(order_name) && "order_name_std" %in% names(df)) {
    df <- df %>% filter(str_to_lower(order_name_std) == str_to_lower(order_name))
  }
  
  if (query_type == "family" && !is.null(family_name) && nzchar(family_name) && "family_name_std" %in% names(df)) {
    df <- df %>% filter(str_to_lower(family_name_std) == str_to_lower(family_name))
  }
  
  if (query_type == "taxon" && !is.null(taxon_name_input) && nzchar(taxon_name_input) && "scientific_name_std" %in% names(df)) {
    df <- df %>% filter(str_to_lower(scientific_name_std) == str_to_lower(taxon_name_input))
  }
  
  df
}

filter_by_threat_status <- function(df, threat_status = "all", require_iucn_for_exact = TRUE) {
  
  df <- standardize_status_columns(df)
  
  if (is.null(threat_status) || threat_status == "all") return(df)
  if (!"threat_status_std" %in% names(df)) return(df)
  
  out <- df
  
  if (threat_status == "threatened_plus") {
    out <- out %>% filter(threat_status_std %in% c("NT", "VU", "EN", "CR", "EW", "EX"))
    return(out)
  }
  
  out <- out %>% filter(threat_status_std == threat_status)
  
  if (require_iucn_for_exact && "threat_authority_std" %in% names(out)) {
    exact_levels <- c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD", "NE")
    if (threat_status %in% exact_levels) {
      out <- out %>%
        filter(
          is.na(threat_authority_std) |
            threat_authority_std == "" |
            str_detect(str_to_lower(threat_authority_std), "iucn")
        )
    }
  }
  
  out
}

make_filter_status_mode_label <- function(data_source, filter_mode) {
  if (filter_mode != "status") return("Threat-status filtering: not selected")
  if (data_source == "live") return("Threat-status filtering: FULL (live)")
  "Threat-status filtering: LIMITED (archived)"
}

####################################################################################################
# ANALYTICS + METADATA
####################################################################################################

get_high_mortality_days <- function(df, time_bin = "day") {
  
  if (!"observed_on" %in% names(df) || nrow(df) == 0) return(NULL)
  
  tmp <- df %>%
    mutate(
      obs_date = as.Date(observed_on),
      obs_bin  = make_time_bin(obs_date, time_bin = time_bin)
    ) %>%
    filter(!is.na(obs_bin))
  
  if (nrow(tmp) == 0) return(NULL)
  
  counts_by_bin <- tmp %>%
    group_by(obs_bin) %>%
    summarise(n = n_distinct(id), .groups = "drop")
  
  if (nrow(counts_by_bin) == 0) return(NULL)
  
  cutoff <- quantile(counts_by_bin$n, probs = 0.90, na.rm = TRUE)
  high_bins <- counts_by_bin %>% filter(n >= cutoff) %>% pull(obs_bin)
  
  list(days = high_bins, quant = cutoff)
}

make_summary_text <- function(df) {
  
  if (nrow(df) == 0 || !"observed_on" %in% names(df)) return("No data available.")
  
  tmp <- df %>%
    standardize_inat_columns() %>%
    mutate(obs_date = as.Date(observed_on)) %>%
    filter(!is.na(obs_date))
  
  if (nrow(tmp) == 0) return("No data available.")
  
  n_obs  <- nrow(tmp)
  n_days <- n_distinct(tmp$obs_date)
  n_observers <- if ("observer_std" %in% names(tmp)) n_distinct(tmp$observer_std[!is.na(tmp$observer_std)]) else NA_integer_
  
  span_days <- if (n_days > 1) paste0(range(tmp$obs_date, na.rm = TRUE), collapse = " to ") else as.character(unique(tmp$obs_date))
  counts_by_day <- tmp %>% count(obs_date)
  peak <- counts_by_day %>% filter(n == max(n)) %>% pull(obs_date)
  peak_val <- max(counts_by_day$n)
  avg_day  <- round(mean(counts_by_day$n), 2)
  
  paste0(
    "Summary:\n",
    "- Total mortality records: ", n_obs, "\n",
    "- Unique observers: ", ifelse(is.na(n_observers), "NA", n_observers), "\n",
    "- Date range:\n", span_days, "\n",
    "- Days with data: ", n_days, "\n",
    "- Average per day: ", avg_day, "\n",
    "- Peak day: ", paste(peak, collapse = ", "), " (", peak_val, " records)\n",
    if (peak_val > avg_day * 2) "- Spike in mortality observations" else ""
  )
}

make_sampling_note <- function(metric_type) {
  if (metric_type == "raw_counts") {
    return("Sampling note: raw mortality counts reflect both mortality signal and observation effort. Compare against unique observers or observations per observer when interpreting spikes.")
  }
  if (metric_type == "unique_observers") {
    return("Sampling note: this view summarizes observer effort rather than mortality burden directly.")
  }
  "Sampling note: observations per observer partially adjusts for effort, but it is still not a formal bias-corrected rate."
}

make_query_metadata <- function(input, bbox, mode, diagnostics, n_records) {
  list(
    timestamp = as.character(Sys.time()),
    data_source = mode,
    bbox = list(
      swlat = bbox[1],
      swlng = bbox[2],
      nelat = bbox[3],
      nelng = bbox[4]
    ),
    date_range = list(
      start = as.character(input$date_range[1]),
      end   = as.character(input$date_range[2])
    ),
    time_aggregation = input$time_bin,
    metric_type = input$metric_type,
    show_smoother = isTRUE(input$show_smoother),
    filter_mode = input$filter_mode,
    taxonomy_query_type = if (input$filter_mode == "taxonomy") input$query_type else NULL,
    iconic_taxon = if (input$filter_mode == "taxonomy") input$iconic_taxon else NULL,
    order_name = if (input$filter_mode == "taxonomy") input$order_name else NULL,
    family_name = if (input$filter_mode == "taxonomy") input$family_name else NULL,
    taxon_name_input = if (input$filter_mode == "taxonomy") input$taxon_name_input else NULL,
    threat_status = if (input$filter_mode == "status") input$threat_status else NULL,
    returned_records = n_records,
    threat_status_mode = make_filter_status_mode_label(mode, input$filter_mode),
    diagnostics = diagnostics
  )
}

make_diagnostics_text <- function(diag, threat_mode_label = NULL) {
  paste0(
    "Query diagnostics:\n",
    "- Weekly windows queried: ", diag$weekly_windows, "\n",
    "- Weeks subdivided to days: ", diag$weeks_subdivided_to_days, "\n",
    "- Days subdivided to half-days: ", diag$days_subdivided_to_halfdays, "\n",
    "- Windows that returned API cap (200): ", diag$windows_hit_limit, "\n",
    "- Final records returned: ", diag$total_rows, "\n",
    "- Adaptive querying used: ", ifelse(diag$adaptive_used, "Yes", "No"), "\n",
    ifelse(is.null(threat_mode_label), "", paste0("- ", threat_mode_label))
  )
}

####################################################################################################
# PLOT BUILDERS
####################################################################################################

make_daily_plot <- function(
    df,
    start_date,
    end_date,
    time_bin = "day",
    metric_type = "raw_counts",
    show_smoother = TRUE
) {
  
  if (!"observed_on" %in% names(df)) return(ggplot() + theme_void() + labs(title = "No date info available"))
  if (nrow(df) == 0) return(ggplot() + theme_void() + labs(title = "No data"))
  
  tmp <- df %>%
    standardize_inat_columns() %>%
    mutate(
      obs_date = as.Date(observed_on),
      obs_bin  = make_time_bin(obs_date, time_bin = time_bin)
    ) %>%
    filter(!is.na(obs_bin))
  
  if (nrow(tmp) == 0) return(ggplot() + theme_void() + labs(title = "No valid dates found"))
  if (!"observer_std" %in% names(tmp)) tmp$observer_std <- NA_character_
  
  counts_by_bin <- tmp %>%
    group_by(obs_bin) %>%
    summarise(
      dead_obs  = n_distinct(id),
      observers = n_distinct(observer_std[!is.na(observer_std)]),
      .groups = "drop"
    ) %>%
    mutate(
      observers = ifelse(is.na(observers), 0, observers),
      obs_per_observer = ifelse(observers > 0, dead_obs / observers, NA_real_)
    )
  
  if (metric_type == "unique_observers") {
    plot_df <- counts_by_bin %>% transmute(obs_bin, value = observers)
    y_lab <- "Number of unique observers"
    plot_title <- "Observer Effort Through Time"
  } else if (metric_type == "obs_per_observer") {
    plot_df <- counts_by_bin %>% transmute(obs_bin, value = obs_per_observer)
    y_lab <- "Dead wildlife observations per observer"
    plot_title <- "Dead Wildlife Observations Per Observer"
  } else {
    plot_df <- counts_by_bin %>% transmute(obs_bin, value = dead_obs)
    y_lab <- "Number of dead wildlife observations"
    plot_title <- "Daily Dead Wildlife Observations"
  }
  
  plot_df <- plot_df %>% filter(!is.na(value))
  if (nrow(plot_df) == 0) return(ggplot() + theme_void() + labs(title = "No data available for selected metric"))
  
  anomaly_cutoff <- quantile(plot_df$value, probs = 0.90, na.rm = TRUE)
  anomaly_df <- plot_df %>% filter(value >= anomaly_cutoff)
  
  p <- ggplot(plot_df, aes(x = obs_bin, y = value)) +
    geom_line(linewidth = 0.9, color = viridis(4, option = "D")[3]) +
    geom_point(size = 1.5, color = viridis(4, option = "D")[3]) +
    geom_point(data = anomaly_df, aes(x = obs_bin, y = value), color = "red3", size = 2.6) +
    make_adaptive_date_scale(start_date, end_date, time_bin = time_bin) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.04))) +
    labs(
      title    = plot_title,
      subtitle = glue("{start_date} to {end_date}"),
      x        = "Observation date",
      y        = y_lab
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title        = element_text(face = "bold"),
      plot.subtitle     = element_text(color = "grey30"),
      axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1, color = "black"),
      axis.text.y       = element_text(color = "black"),
      axis.title.x      = element_text(face = "bold", margin = margin(t = 14)),
      axis.title.y      = element_text(face = "bold", margin = margin(r = 14)),
      panel.grid.minor  = element_blank(),
      plot.margin       = margin(t = 12, r = 18, b = 28, l = 28)
    )
  
  if (show_smoother && nrow(plot_df) >= 6) {
    p <- p + geom_smooth(se = FALSE, method = "loess", span = 0.25, color = "black", linewidth = 0.8)
  }
  
  p
}

make_top_species_plot <- function(df) {
  
  if (!"scientific_name_std" %in% names(df)) df <- standardize_taxonomy_columns(df)
  if (!"scientific_name_std" %in% names(df)) return(ggplot() + theme_void() + labs(title = "No species information available"))
  if (nrow(df) == 0) return(ggplot() + theme_void() + labs(title = "No data"))
  
  tmp <- df %>%
    mutate(
      obs_date = as.Date(observed_on),
      Window   = format(obs_date, "%Y")
    ) %>%
    filter(!is.na(scientific_name_std))
  
  if (nrow(tmp) == 0) return(ggplot() + theme_void() + labs(title = "No species information available"))
  
  species_counts <- tmp %>%
    group_by(Window, scientific_name_std) %>%
    summarise(dead_count = n(), .groups = "drop")
  
  top_species_overall <- species_counts %>%
    group_by(scientific_name_std) %>%
    summarise(total_dead = sum(dead_count), .groups = "drop") %>%
    arrange(desc(total_dead)) %>%
    slice_head(n = 20)
  
  species_top20 <- species_counts %>% filter(scientific_name_std %in% top_species_overall$scientific_name_std)
  
  ggplot(species_top20, aes(x = reorder(scientific_name_std, dead_count), y = dead_count, fill = Window)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    coord_flip() +
    scale_fill_viridis_d(option = "D", end = 0.9) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Top 20 Species with Dead Wildlife Observations",
      x     = "Species",
      y     = "Number of dead wildlife observations",
      fill  = "Year"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title        = element_text(face = "bold"),
      axis.text.x       = element_text(color = "black"),
      axis.text.y       = element_text(color = "black", size = 11),
      axis.title.x      = element_text(face = "bold", margin = margin(t = 14)),
      axis.title.y      = element_text(face = "bold", margin = margin(r = 14)),
      panel.grid.minor  = element_blank(),
      plot.margin       = margin(t = 12, r = 18, b = 20, l = 20)
    )
}

make_hexbin_context_map <- function(df_coords, start_date, end_date) {
  
  if (!all(c("latitude", "longitude") %in% names(df_coords))) {
    return(ggplot() + theme_void() + labs(title = "No spatial data available for map"))
  }
  
  df_coords <- df_coords %>% filter(!is.na(latitude), !is.na(longitude))
  if (nrow(df_coords) == 0) return(ggplot() + theme_void() + labs(title = "No spatial data available for map"))
  
  world_df <- map_data("world")
  
  x_limits <- range(df_coords$longitude, na.rm = TRUE)
  y_limits <- range(df_coords$latitude,  na.rm = TRUE)
  x_span <- diff(x_limits)
  y_span <- diff(y_limits)
  
  x_pad <- max(0.5, x_span * 0.08)
  y_pad <- max(0.5, y_span * 0.08)
  
  x_limits_padded <- c(x_limits[1] - x_pad, x_limits[2] + x_pad)
  y_limits_padded <- c(y_limits[1] - y_pad, y_limits[2] + y_pad)
  
  bbox_rect <- data.frame(
    xmin = x_limits[1],
    xmax = x_limits[2],
    ymin = y_limits[1],
    ymax = y_limits[2]
  )
  
  n_bins <- dplyr::case_when(
    nrow(df_coords) < 150   ~ 12,
    nrow(df_coords) < 500   ~ 18,
    nrow(df_coords) < 2000  ~ 25,
    nrow(df_coords) < 10000 ~ 35,
    TRUE ~ 45
  )
  
  main_map <- ggplot() +
    geom_polygon(
      data = world_df,
      aes(x = long, y = lat, group = group),
      fill = "grey97",
      color = "grey88",
      linewidth = 0.12
    ) +
    stat_bin_hex(
      data = df_coords,
      aes(x = longitude, y = latitude),
      bins = n_bins,
      linewidth = 0.08,
      color = scales::alpha("black", 0.18)
    ) +
    scale_fill_viridis_c(option = "C", trans = "sqrt", labels = scales::comma, name = "Records") +
    coord_quickmap(xlim = x_limits_padded, ylim = y_limits_padded, expand = FALSE) +
    labs(
      title = "Wildlife Mortality Hotspots",
      subtitle = glue("{start_date} to {end_date}"),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey35", margin = margin(b = 8)),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "grey20"),
      panel.grid.major = element_line(color = "grey92", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      legend.key.height = unit(18, "pt"),
      plot.margin = margin(t = 10, r = 8, b = 8, l = 8)
    )
  
  context_map <- ggplot() +
    geom_polygon(
      data = world_df,
      aes(x = long, y = lat, group = group),
      fill = "grey95",
      color = "grey80",
      linewidth = 0.08
    ) +
    geom_rect(
      data = bbox_rect,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = scales::alpha("#2C7FB8", 0.08),
      color = "#2C7FB8",
      linewidth = 0.7
    ) +
    coord_quickmap(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
    labs(title = "Global context") +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 9, margin = margin(b = 2)),
      plot.background = element_rect(fill = "white", color = "grey75", linewidth = 0.25),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 2, r = 70, b = 2, l = 70)
    )
  
  cowplot::plot_grid(main_map, context_map, ncol = 1, rel_heights = c(5, 1.1), align = "v")
}

make_hexbin_map <- function(df, start_date, end_date) {
  if (!all(c("latitude", "longitude") %in% names(df))) return(ggplot() + theme_void() + labs(title = "No spatial data available for map"))
  
  df_coords <- df %>%
    dplyr::select(latitude, longitude) %>%
    filter(!is.na(latitude), !is.na(longitude))
  
  make_hexbin_context_map(df_coords, start_date, end_date)
}

make_leaflet_hotspot_map <- function(df, bbox = NULL, max_points = 50000) {
  if (!all(c("latitude", "longitude") %in% names(df))) return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron))
  
  map_df <- df %>%
    standardize_inat_columns() %>%
    filter(!is.na(latitude), !is.na(longitude))
  
  if (nrow(map_df) == 0) return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron))
  
  if (nrow(map_df) > max_points) {
    set.seed(1)
    map_df <- dplyr::slice_sample(map_df, n = max_points)
  }
  
  popup_text <- if ("scientific_name_std" %in% names(map_df)) {
    paste0("<b>", map_df$scientific_name_std, "</b>")
  } else {
    NULL
  }
  
  m <- leaflet(map_df) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = 4,
      stroke = FALSE,
      fillOpacity = 0.45,
      color = "#440154",
      popup = popup_text,
      clusterOptions = markerClusterOptions(
        showCoverageOnHover = FALSE,
        zoomToBoundsOnClick = TRUE,
        spiderfyOnMaxZoom = TRUE,
        removeOutsideVisibleBounds = TRUE,
        disableClusteringAtZoom = 12
      )
    )
  
  if (!is.null(bbox)) {
    m <- m %>% fitBounds(lng1 = bbox[2], lat1 = bbox[1], lng2 = bbox[4], lat2 = bbox[3])
  } else {
    m <- m %>% fitBounds(
      lng1 = min(map_df$longitude, na.rm = TRUE),
      lat1 = min(map_df$latitude,  na.rm = TRUE),
      lng2 = max(map_df$longitude, na.rm = TRUE),
      lat2 = max(map_df$latitude,  na.rm = TRUE)
    )
  }
  
  m
}

####################################################################################################
# LIVE QUERY HELPERS
####################################################################################################

fetch_dead_data_once <- function(
    swlat,
    swlng,
    nelat,
    nelng,
    start_date,
    end_date,
    iconic_taxa = NULL,
    taxon_name  = NULL,
    threatened_flag = FALSE,
    per_page    = 200,
    progress    = NULL
) {
  
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  q_parts <- list(
    "term_id=17",
    "term_value_id=19",
    "verifiable=true",
    glue("d1={start_date}"),
    glue("d2={end_date}"),
    "order=desc",
    "order_by=created_at",
    glue("per_page={per_page}")
  )
  
  if (!is.null(iconic_taxa) && nzchar(iconic_taxa)) q_parts <- c(q_parts, glue("iconic_taxa={iconic_taxa}"))
  if (!is.null(taxon_name)  && nzchar(taxon_name))  q_parts <- c(q_parts, glue("taxon_name={URLencode(taxon_name)}"))
  if (isTRUE(threatened_flag)) q_parts <- c(q_parts, "threatened=true")
  
  query_params <- paste(q_parts, collapse = "&")
  loc_part <- glue("&nelat={nelat}&nelng={nelng}&swlat={swlat}&swlng={swlng}")
  
  if (!is.null(progress)) {
    progress$set(detail = glue("Fetching {start_date} to {end_date}"), value = NULL)
  }
  
  query_url <- paste0(base_url, "?", query_params, loc_part)
  resp <- httr::GET(query_url)
  
  if (httr::http_error(resp)) {
    warning("HTTP error: ", httr::status_code(resp))
    return(list(data = tibble::tibble(), hit_limit = FALSE, n_rows = 0))
  }
  
  parsed <- httr::content(resp, as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)
  
  obs_df <- tibble::as_tibble(parsed$results)
  
  list(
    data = obs_df,
    hit_limit = nrow(obs_df) >= per_page,
    n_rows = nrow(obs_df)
  )
}

split_date_window <- function(start_date, end_date, unit = c("week", "day", "halfday")) {
  unit <- match.arg(unit)
  
  start_date <- as.POSIXct(start_date, tz = "UTC")
  end_date   <- as.POSIXct(end_date, tz = "UTC")
  
  if (unit == "week") {
    breaks <- seq.Date(as.Date(start_date), as.Date(end_date), by = "1 week")
    breaks <- as.POSIXct(breaks, tz = "UTC")
  } else if (unit == "day") {
    breaks <- seq.Date(as.Date(start_date), as.Date(end_date), by = "1 day")
    breaks <- as.POSIXct(breaks, tz = "UTC")
  } else {
    breaks <- seq(from = start_date, to = end_date, by = "12 hours")
  }
  
  if (length(breaks) == 0 || tail(breaks, 1) < end_date) {
    breaks <- c(breaks, end_date)
  }
  
  windows <- list()
  
  if (length(breaks) == 1) {
    windows[[1]] <- list(start = start_date, end = end_date)
    return(windows)
  }
  
  for (i in seq_len(length(breaks) - 1)) {
    win_start <- breaks[i]
    win_end   <- breaks[i + 1] - 1
    if (i == length(breaks) - 1) win_end <- end_date
    windows[[i]] <- list(start = win_start, end = win_end)
  }
  
  windows
}

fetch_dead_data_adaptive <- function(
    start_date,
    end_date,
    swlat,
    swlng,
    nelat,
    nelng,
    iconic_taxa = NULL,
    taxon_name = NULL,
    threatened_flag = FALSE,
    per_page = 200,
    progress = NULL
) {
  
  all_results <- list()
  result_index <- 1
  
  diagnostics <- list(
    weekly_windows = 0,
    weeks_subdivided_to_days = 0,
    days_subdivided_to_halfdays = 0,
    windows_hit_limit = 0,
    total_rows = 0,
    adaptive_used = FALSE
  )
  
  weekly_windows <- split_date_window(start_date, end_date, unit = "week")
  total_weeks <- length(weekly_windows)
  diagnostics$weekly_windows <- total_weeks
  
  for (i in seq_along(weekly_windows)) {
    wk <- weekly_windows[[i]]
    wk_start <- as.Date(wk$start)
    wk_end   <- as.Date(wk$end)
    
    if (!is.null(progress)) {
      progress$set(
        value = (i - 1) / total_weeks,
        message = glue("Live Query: week {i} of {total_weeks}"),
        detail = glue("{wk_start} to {wk_end}")
      )
    }
    
    week_res <- fetch_dead_data_once(
      swlat = swlat,
      swlng = swlng,
      nelat = nelat,
      nelng = nelng,
      start_date = wk_start,
      end_date = wk_end,
      iconic_taxa = iconic_taxa,
      taxon_name = taxon_name,
      threatened_flag = threatened_flag,
      per_page = per_page,
      progress = progress
    )
    
    if (isTRUE(week_res$hit_limit)) {
      diagnostics$weeks_subdivided_to_days <- diagnostics$weeks_subdivided_to_days + 1
      diagnostics$windows_hit_limit <- diagnostics$windows_hit_limit + 1
      diagnostics$adaptive_used <- TRUE
      
      if (!is.null(progress)) {
        progress$set(
          value = (i - 1) / total_weeks,
          message = glue("Week {i} returned {per_page}; subdividing to days"),
          detail = glue("{wk_start} to {wk_end}")
        )
      }
      
      daily_windows <- split_date_window(wk_start, wk_end, unit = "day")
      
      for (d in seq_along(daily_windows)) {
        dy <- daily_windows[[d]]
        dy_start <- as.Date(dy$start)
        dy_end   <- as.Date(dy$end)
        
        if (!is.null(progress)) {
          progress$set(
            value = (i - 1 + d / max(1, length(daily_windows))) / total_weeks,
            message = glue("Week {i} subdivided: day {d} of {length(daily_windows)}"),
            detail = glue("{dy_start} to {dy_end}")
          )
        }
        
        day_res <- fetch_dead_data_once(
          swlat = swlat,
          swlng = swlng,
          nelat = nelat,
          nelng = nelng,
          start_date = dy_start,
          end_date = dy_end,
          iconic_taxa = iconic_taxa,
          taxon_name = taxon_name,
          threatened_flag = threatened_flag,
          per_page = per_page,
          progress = progress
        )
        
        if (isTRUE(day_res$hit_limit)) {
          diagnostics$days_subdivided_to_halfdays <- diagnostics$days_subdivided_to_halfdays + 1
          diagnostics$windows_hit_limit <- diagnostics$windows_hit_limit + 1
          diagnostics$adaptive_used <- TRUE
          
          if (!is.null(progress)) {
            progress$set(
              value = (i - 1 + d / max(1, length(daily_windows))) / total_weeks,
              message = glue("Day {dy_start} returned {per_page}; subdividing to 12-hour windows"),
              detail = glue("{dy_start}")
            )
          }
          
          halfday_windows <- split_date_window(
            as.POSIXct(dy_start, tz = "UTC"),
            as.POSIXct(dy_end, tz = "UTC") + 86399,
            unit = "halfday"
          )
          
          for (h in seq_along(halfday_windows)) {
            hd <- halfday_windows[[h]]
            hd_start <- format(hd$start, "%Y-%m-%d")
            hd_end   <- format(hd$end, "%Y-%m-%d")
            
            half_res <- fetch_dead_data_once(
              swlat = swlat,
              swlng = swlng,
              nelat = nelat,
              nelng = nelng,
              start_date = hd_start,
              end_date = hd_end,
              iconic_taxa = iconic_taxa,
              taxon_name = taxon_name,
              threatened_flag = threatened_flag,
              per_page = per_page,
              progress = progress
            )
            
            all_results[[result_index]] <- half_res$data
            result_index <- result_index + 1
          }
          
        } else {
          all_results[[result_index]] <- day_res$data
          result_index <- result_index + 1
        }
      }
      
    } else {
      all_results[[result_index]] <- week_res$data
      result_index <- result_index + 1
    }
  }
  
  out <- dplyr::bind_rows(all_results)
  diagnostics$total_rows <- nrow(out)
  
  list(data = out, diagnostics = diagnostics)
}

getDeadVertebrates_dateRange <- function(
    start_date,
    end_date,
    swlat,
    swlng,
    nelat,
    nelng,
    iconic_taxa     = NULL,
    taxon_name      = NULL,
    threatened_flag = FALSE,
    exact_threat_status = "all",
    per_page        = 200,
    .shiny_progress = NULL,
    time_bin        = "day",
    metric_type     = "raw_counts",
    show_smoother   = TRUE
) {
  
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  adaptive_res <- fetch_dead_data_adaptive(
    start_date = start_date,
    end_date   = end_date,
    swlat      = swlat,
    swlng      = swlng,
    nelat      = nelat,
    nelng      = nelng,
    iconic_taxa = iconic_taxa,
    taxon_name  = taxon_name,
    threatened_flag = threatened_flag,
    per_page    = per_page,
    progress    = .shiny_progress
  )
  
  merged_df_all <- adaptive_res$data
  diagnostics   <- adaptive_res$diagnostics
  
  merged_df_all <- standardize_inat_columns(merged_df_all)
  
  merged_df_all <- filter_by_threat_status(
    merged_df_all,
    threat_status = exact_threat_status,
    require_iucn_for_exact = TRUE
  )
  
  diagnostics$total_rows <- nrow(merged_df_all)
  
  if (nrow(merged_df_all) == 0 || !"observed_on" %in% names(merged_df_all)) {
    placeholder_plot <- function(title) {
      ggplot() + labs(title = title, x = NULL, y = NULL) + theme_void()
    }
    
    return(list(
      merged_df_all    = merged_df_all,
      merged_df        = merged_df_all,
      daily_plot       = placeholder_plot("No 'Dead' Observations Found"),
      top_species_plot = placeholder_plot("No species data"),
      map_hotspots_gg  = placeholder_plot("No data for map"),
      daily_90th_quant = NA,
      diagnostics      = diagnostics
    ))
  }
  
  daily_plot <- make_daily_plot(
    merged_df_all,
    start_date = start_date,
    end_date = end_date,
    time_bin = time_bin,
    metric_type = metric_type,
    show_smoother = show_smoother
  )
  
  top_species_plot <- make_top_species_plot(merged_df_all)
  map_hotspots_gg <- make_hexbin_map(merged_df_all, start_date, end_date)
  
  hm <- get_high_mortality_days(merged_df_all, time_bin = time_bin)
  
  merged_high <- if (!is.null(hm$days)) {
    merged_df_all %>%
      mutate(obs_bin = make_time_bin(as.Date(observed_on), time_bin = time_bin)) %>%
      filter(obs_bin %in% hm$days)
  } else {
    merged_df_all
  }
  
  list(
    merged_df_all    = merged_df_all,
    merged_df        = merged_high,
    daily_plot       = daily_plot,
    top_species_plot = top_species_plot,
    map_hotspots_gg  = map_hotspots_gg,
    daily_90th_quant = if (!is.null(hm$quant)) hm$quant else NA,
    diagnostics      = diagnostics
  )
}

####################################################################################################
# UI
####################################################################################################

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .main-plot-panel { min-height: 700px; }
      .shiny-plot-output { overflow: visible !important; }
      .about-card {
        background: #fafafa;
        border: 1px solid #e3e3e3;
        border-radius: 10px;
        padding: 18px;
        margin-bottom: 14px;
      }
      .about-icon { font-size: 24px; margin-right: 8px; }
      .about-heading { font-weight: 700; margin-bottom: 10px; }
      .about-subtle { color: #666666; }
      .map-note { font-size: 12px; color: #666666; margin-bottom: 8px; }
      .note-box {
        background: #f8f9fb;
        border-left: 4px solid #6c8ebf;
        padding: 10px 12px;
        margin: 10px 0;
        font-size: 13px;
      }
      .silhouette-row {
        display: flex;
        gap: 18px;
        align-items: center;
        justify-content: center;
        margin: 10px 0 4px 0;
        opacity: 0.85;
        font-size: 32px;
      }
      .warning-box {
        background: #fff7e6;
        border-left: 4px solid #f0ad4e;
        padding: 10px 12px;
        margin: 10px 0;
        font-size: 13px;
      }
    "))
  ),
  
  fluidRow(
    column(
      width = 2,
      tags$img(
        src   = "all_logos.png",
        height = "110px",
        style = "max-width: 100%; height: auto; display: block; margin-top: 10px;"
      )
    ),
    column(
      width = 10,
      titlePanel("Dead Wildlife Observations from iNaturalist")
    )
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "sidebar_tabs",
        
        tabPanel(
          "Query",
          br(),
          
          radioButtons(
            "data_source",
            "Data Source:",
            choices = c(
              "Download Live from iNaturalist" = "live",
              "Archived Parquet File" = "archived"
            ),
            selected = "live"
          ),
          
          fileInput(
            "study_area_file",
            "Upload study area (.zip shapefile, zipped .gdb, .gpkg, or .geojson; max 100 MB)",
            accept = c(".zip", ".gpkg", ".geojson")
          ),
          
          tags$div(
            style = "margin-bottom:10px;",
            leafletOutput("select_map", height = "340px"),
            actionButton("clear_bbox", "Clear Bounding Box", icon = icon("eraser")),
            helpText("Draw a rectangle or upload a study area file. Only one region is used at a time.")
          ),
          
          verbatimTextOutput("bbox_coords"),
          
          dateRangeInput(
            "date_range",
            "Select Date Range:",
            start = Sys.Date() - 365,
            end   = Sys.Date(),
            min   = "2010-01-01",
            max   = Sys.Date()
          ),
          
          selectInput(
            "time_bin",
            "Time aggregation:",
            choices = c("Day" = "day", "Week" = "week", "Month" = "month"),
            selected = "day"
          ),
          
          radioButtons(
            "metric_type",
            "Daily plot metric:",
            choices = c(
              "Raw counts" = "raw_counts",
              "Unique observers" = "unique_observers",
              "Observations per observer" = "obs_per_observer"
            ),
            selected = "raw_counts"
          ),
          
          checkboxInput("show_smoother", "Show smoother", value = TRUE),
          
          hr(),
          
          radioButtons(
            "filter_mode",
            "Filter mode:",
            choices = c(
              "Taxonomy" = "taxonomy",
              "Conservation / threat status" = "status"
            ),
            selected = "taxonomy"
          ),
          
          conditionalPanel(
            condition = "input.filter_mode == 'taxonomy'",
            radioButtons(
              "query_type",
              "Query By:",
              choices = c(
                "Taxon Class" = "iconic",
                "Order" = "order",
                "Family" = "family",
                "Taxon Name" = "taxon"
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.filter_mode == 'taxonomy' && input.query_type == 'iconic'",
            selectInput(
              "iconic_taxon",
              "Select Taxon Class:",
              choices = c("Aves", "Mammalia", "Reptilia", "Amphibia", "Actinopterygii", "Mollusca", "Animalia"),
              selected = "Aves"
            )
          ),
          
          conditionalPanel(
            condition = "input.filter_mode == 'taxonomy' && input.query_type == 'order'",
            textInput("order_name", "Enter order name (e.g. Passeriformes)", "")
          ),
          
          conditionalPanel(
            condition = "input.filter_mode == 'taxonomy' && input.query_type == 'family'",
            textInput("family_name", "Enter family name (e.g. Anatidae)", "")
          ),
          
          conditionalPanel(
            condition = "input.filter_mode == 'taxonomy' && input.query_type == 'taxon'",
            textInput("taxon_name_input", "Enter taxon name (species, genus, family, or order)", "")
          ),
          
          conditionalPanel(
            condition = "input.filter_mode == 'status'",
            selectInput(
              "threat_status",
              "Threat status:",
              choices = c(
                "All" = "all",
                "Threatened and above (NT+)" = "threatened_plus",
                "Near Threatened (NT)" = "NT",
                "Vulnerable (VU)" = "VU",
                "Endangered (EN)" = "EN",
                "Critically Endangered (CR)" = "CR",
                "Extinct in the Wild (EW)" = "EW",
                "Extinct (EX)" = "EX",
                "Least Concern (LC)" = "LC",
                "Data Deficient (DD)" = "DD",
                "Not Evaluated (NE)" = "NE"
              ),
              selected = "threatened_plus"
            )
          ),
          
          conditionalPanel(
            condition = "input.data_source == 'archived' && input.filter_mode == 'status'",
            div(
              class = "warning-box",
              "Note: Threat-status filtering may be incomplete in archived data due to metadata limitations. Live mode is recommended for this filter."
            )
          ),
          
          div(
            class = "note-box",
            "Threat-status filtering is strict. Archived mode filters exact status categories using taxon.conservation_status.status_name when available. Live mode post-filters returned records using that same status field, so exact selections like CR are no longer broad threatened matches."
          ),
          
          actionButton("run_query", "Run Query", icon = icon("play")),
          
          hr(),
          
          downloadButton("downloadAll", "Download filtered data CSV"),
          br(), br(),
          downloadButton("downloadMetadata", "Download query metadata JSON")
        ),
        
        tabPanel(
          "About",
          
          div(class = "silhouette-row", "🦅", "🐢", "🦊", "🦋", "🐋", "🦎", "🪸"),
          
          div(
            class = "about-card",
            tags$div(class = "about-heading", icon("feather-alt", class = "about-icon"), "Why this tool exists"),
            tags$p(
              "This application supports near real-time ecological surveillance of wildlife mortality using participatory science observations from iNaturalist. Rather than treating mortality records as incidental, the app treats them as an ecological signal that can reveal emerging risks, hotspots, taxon-specific mortality patterns, and potential event clusters."
            ),
            tags$p(
              class = "about-subtle",
              "Contact information: diego.ellissoto@berkeley.edu, University of California Berkeley"
            ),
            tags$p(
              class = "about-subtle",
              "Diego Ellis-Soto,  Liam U. Taylor,  Elizabeth Edson,  Avery Hill,  Christopher J. Schell,  Carl Boettiger,  Rebecca F. Johnson"
            )
          ),
          
          div(
            class = "about-card",
            tags$div(class = "about-heading", icon("binoculars", class = "about-icon"), "Connection to the broader framework"),
            tags$p(
              "This app is aligned with the logic of near real-time ecological monitoring through participatory science. Public biodiversity platforms can function as distributed sensor networks for ecological disturbance, mortality pulses, unusual events, and biologically meaningful anomalies."
            ),
            tags$ul(
              tags$li("Mortality observations can accumulate quickly enough to identify events while they are still unfolding."),
              tags$li("Spatial clustering can reveal road mortality, window strikes, disease outbreaks, contamination events, and seasonal mortality pulses."),
              tags$li("Taxonomic and status-based filtering allow users to move from broad surveillance to more biologically targeted investigation."),
              tags$li("Archived and live workflows support both reproducible analysis and rapid exploratory querying.")
            )
          ),
          
          div(
            class = "about-card",
            tags$div(class = "about-heading", icon("sync-alt", class = "about-icon"), "Adaptive live querying and API handling"),
            tags$p(
              "Live queries are handled dynamically rather than with a fixed time step. The app begins with broader weekly windows for efficiency, but if a selected interval appears dense enough to hit the practical API ceiling for returned rows, the app automatically subdivides that period into smaller windows."
            ),
            tags$ul(
              tags$li("Queries begin with weekly intervals."),
              tags$li("If a week appears saturated, that week is automatically re-queried day by day."),
              tags$li("If a day still appears saturated, the app can subdivide further into 12-hour windows."),
              tags$li("The progress bar reports these transitions so the user can see when refinement is happening.")
            ),
            tags$p(
              class = "about-subtle",
              "This design favors completeness for dense queries while still keeping smaller queries efficient."
            )
          ),
          
          div(
            class = "about-card",
            tags$div(class = "about-heading", icon("exclamation-triangle", class = "about-icon"), "How vulnerability / threat status is handled"),
            tags$p(
              "Threat-status filtering is based on the flattened conservation fields returned in iNaturalist-style records, especially taxon.conservation_status.status_name. The app standardizes those values to a compact set of categories such as LC, NT, VU, EN, and CR."
            ),
            tags$ul(
              tags$li("Live mode is the most reliable setting for threat-status filtering."),
              tags$li("Live mode may use a broad threatened flag only to widen candidate retrieval when needed."),
              tags$li("After live retrieval, the app post-filters records using taxon.conservation_status.status_name, which prevents exact user choices such as CR from collapsing into a broader threatened pool."),
              tags$li("Where authority is present, exact categories can be restricted to IUCN-style assessments to reduce mixing with other systems.")
            ),
            div(
              class = "note-box",
              tags$b("Important limitation:"),
              tags$p(
                "Conservation / threat-status filtering is fully supported in Live mode using standardized fields derived from taxon.conservation_status.status_name. ",
                "In Archived mode, filtering may be incomplete or unavailable because the archived dataset was generated with a different metadata structure and does not consistently retain comparable conservation-status fields."
              )
            )
          ),
          
          div(
            class = "about-card",
            tags$div(class = "about-heading", icon("chart-line", class = "about-icon"), "Interpreting counts and observer effort"),
            tags$p(
              "Observation totals reflect both ecological signal and the observation process. A spike in records can indicate a true mortality event, but it can also reflect increased observer effort, seasonality, detectability, or platform activity. That is why the app includes raw counts, unique observers, and observations per observer."
            ),
            tags$ul(
              tags$li("Raw counts are the most direct signal but remain effort-sensitive."),
              tags$li("Unique observers provide a rough proxy for observer effort."),
              tags$li("Observations per observer can help contextualize spikes, though it is not a formal bias-corrected rate."),
              tags$li("Query diagnostics document when adaptive subdivision was required to reduce truncation risk.")
            )
          ),
          
          div(
            class = "about-card",
            tags$div(class = "about-heading", icon("shield-alt", class = "about-icon"), "Responsible interpretation"),
            tags$ul(
              tags$li("Observation effort is uneven across space, taxa, and time."),
              tags$li("Some coordinates may be obscured or generalized for privacy."),
              tags$li("Conservation-status fields may come from different authorities."),
              tags$li("Strong signals should be interpreted with ecological context and, where possible, independent validation.")
            )
          ),
          
          div(
            class = "about-card",
            tags$div(class = "about-heading", icon("book", class = "about-icon"), "Citation"),
            tags$p(
              "Please cite: Ellis-Soto, Diego, et al. \"Global monitoring of wildlife mortality through participatory science in near-real time.\" bioRxiv (2025): 2025-08."
            )
          )
        ),
        
        tabPanel(
          "How to Use",
          
          div(class = "silhouette-row", "🦅", "🐢", "🦊", "🦋", "🐋", "🦎", "🪸"),
          
          tags$h3("Quick Start Guide"),
          
          tags$ol(
            tags$li("Upload a study area file (.zip shapefile, zipped .gdb, .gpkg, or .geojson) or draw a rectangle on the map."),
            tags$li("Set your date range."),
            tags$li("Choose the time aggregation: day, week, or month."),
            tags$li("Choose whether you want to inspect raw mortality counts, unique observers, or observations per observer."),
            tags$li("Choose a filter mode: taxonomy or conservation / threat status."),
            tags$li("Pick Live for current records or Archived for faster reproducible querying."),
            tags$li("Run the query and inspect the plots, diagnostics, maps, and data table."),
            tags$li("Download the filtered records and metadata if needed.")
          ),
          
          tags$h4("How live querying works"),
          tags$p(
            "In Live mode, the app does not assume a single fixed time chunk. It starts with weekly windows because those are usually fast and efficient for moderate-sized queries."
          ),
          tags$ul(
            tags$li("If a weekly query looks manageable, the app keeps that weekly result."),
            tags$li("If a weekly window looks dense enough to hit the practical return ceiling, it is automatically subdivided into day windows."),
            tags$li("If a day still looks dense, it can be subdivided further into 12-hour windows."),
            tags$li("This is especially useful for broad regions, taxonomically broad searches, or bursts of mortality reporting.")
          ),
          
          tags$h4("How records are handled"),
          tags$ul(
            tags$li("All plots, maps, tables, and downloads are based on the filtered query result, not the full archive."),
            tags$li("The CSV download contains the filtered records returned by the current query."),
            tags$li("The metadata JSON stores the bounding box, date range, filtering mode, metric choices, and diagnostics."),
            tags$li("The diagnostics panel helps document whether adaptive query subdivision was triggered.")
          ),
          
          tags$h4("How conservation / threat filtering works"),
          tags$p(
            "Threat-status filtering is based on taxon.conservation_status.status_name. The app standardizes status labels to a compact code set before filtering."
          ),
          tags$ul(
            tags$li("Live mode applies the most reliable threat-status logic."),
            tags$li("Archived mode can use threat-status filtering when comparable metadata are present, but archived metadata may be incomplete or inconsistent."),
            tags$li("Live mode may initially query a broader threatened pool when needed, but then post-filters the returned records using the same standardized status field."),
            tags$li("This means that choosing Critically Endangered (CR) should return CR-standardized rows only, rather than a mixture of EN, VU, or non-IUCN entries.")
          ),
          tags$li(
            "Threat-status filtering works most reliably in Live mode. Archived data may not include consistent conservation-status fields due to differences in how metadata were originally stored."
          ),
          
          tags$h4("Practical interpretation tips"),
          tags$ul(
            tags$li("Compare raw counts against unique observers when interpreting spikes."),
            tags$li("Use observations per observer as a simple effort-sensitive contextual metric."),
            tags$li("Check the All Data Table to inspect status authority, status code, and status name directly."),
            tags$li("Use the static hexbin map for overview patterns and the interactive map for quick spatial exploration.")
          )
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Daily Time Series",
          div(class = "note-box", textOutput("samplingNote")),
          fluidRow(
            column(
              width = 8,
              div(
                class = "main-plot-panel",
                withSpinner(plotOutput("dailyPlot", height = "650px"), type = 6)
              )
            ),
            column(
              width = 4,
              verbatimTextOutput("dailySummary"),
              br(),
              verbatimTextOutput("queryDiagnostics")
            )
          )
        ),
        
        tabPanel(
          "Top Species",
          div(
            class = "main-plot-panel",
            withSpinner(plotOutput("speciesPlot", height = "700px"), type = 6)
          )
        ),
        
        tabPanel(
          "Interactive Hotspot Map",
          tags$div(class = "map-note", "This map uses clustered markers for speed and broad exploratory use."),
          div(
            class = "main-plot-panel",
            withSpinner(leafletOutput("hotspotLeaflet", height = "760px"), type = 6)
          )
        ),
        
        tabPanel(
          "Hexbin Map (Static)",
          div(
            class = "main-plot-panel",
            withSpinner(plotOutput("hotspotMap", height = "820px"), type = 6)
          )
        ),
        
        tabPanel(
          "All Data Table",
          withSpinner(DT::dataTableOutput("dataTable"), type = 6)
        )
      )
    )
  )
)

####################################################################################################
# SERVER
####################################################################################################

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    bbox = NULL,
    uploaded_study_area = NULL,
    query_token = 0,
    metadata = NULL
  )
  
  ##################################################################################################
  # BBOX SELECTION MAP
  ##################################################################################################
  output$select_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -95, lat = 40, zoom = 3) %>%
      addDrawToolbar(
        targetGroup = "drawn_bboxes",
        rectangleOptions = drawRectangleOptions(repeatMode = FALSE),
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = FALSE,
        editOptions = editToolbarOptions()
      )
  })
  
  observeEvent(input$select_map_draw_new_feature, {
    feat <- input$select_map_draw_new_feature
    if (!is.null(feat$geometry) && feat$geometry$type == "Polygon") {
      coords <- feat$geometry$coordinates[[1]]
      lngs <- vapply(coords, function(x) x[[1]], numeric(1))
      lats <- vapply(coords, function(x) x[[2]], numeric(1))
      rv$bbox <- c(min(lats), min(lngs), max(lats), max(lngs))
      rv$query_token <- rv$query_token + 1
    }
  })
  
  observeEvent(input$select_map_draw_deleted_features, {
    rv$bbox <- NULL
    rv$query_token <- rv$query_token + 1
  })
  
  observeEvent(input$select_map_draw_edited_features, {
    if (!is.null(input$select_map_draw_all_features)) {
      feats <- input$select_map_draw_all_features$features
      if (length(feats) > 0) {
        feat <- feats[[length(feats)]]
        if (!is.null(feat$geometry) && feat$geometry$type == "Polygon") {
          coords <- feat$geometry$coordinates[[1]]
          lngs <- vapply(coords, function(x) x[[1]], numeric(1))
          lats <- vapply(coords, function(x) x[[2]], numeric(1))
          rv$bbox <- c(min(lats), min(lngs), max(lats), max(lngs))
          rv$query_token <- rv$query_token + 1
        }
      }
    }
  })
  
  observeEvent(input$clear_bbox, {
    rv$bbox <- NULL
    rv$uploaded_study_area <- NULL
    rv$query_token <- rv$query_token + 1
    
    leafletProxy("select_map") %>%
      clearGroup("drawn_bboxes") %>%
      clearGroup("search_bbox")
    
    shinyjs::reset("study_area_file")
  })
  
  ##################################################################################################
  # UPLOADED STUDY AREA
  ##################################################################################################
  observeEvent(input$study_area_file, {
    req(input$study_area_file)
    
    uploaded_file <- input$study_area_file$datapath
    uploaded_name <- input$study_area_file$name
    
    study_area <- tryCatch(
      read_uploaded_study_area(uploaded_file, uploaded_name),
      error = function(e) {
        showNotification(paste("Could not read study area:", e$message), type = "error", duration = 8)
        return(NULL)
      }
    )
    
    if (is.null(study_area) || nrow(study_area) == 0) return()
    
    study_area <- prepare_uploaded_study_area(study_area)
    
    if (is.null(study_area)) {
      showNotification("Could not transform study area to WGS84.", type = "error", duration = 8)
      return()
    }
    
    bbox <- bbox_from_sf(study_area)
    
    leafletProxy("select_map") %>%
      clearGroup("search_bbox") %>%
      addRectangles(
        lng1 = bbox[2],
        lat1 = bbox[1],
        lng2 = bbox[4],
        lat2 = bbox[3],
        fillColor = "red",
        fillOpacity = 0.1,
        color = "red",
        group = "search_bbox"
      ) %>%
      fitBounds(
        lng1 = bbox[2],
        lat1 = bbox[1],
        lng2 = bbox[4],
        lat2 = bbox[3]
      )
    
    rv$bbox <- bbox
    rv$uploaded_study_area <- study_area
    rv$query_token <- rv$query_token + 1
    
    showNotification("Study area uploaded successfully.", type = "message", duration = 4)
  })
  
  output$bbox_coords <- renderText({
    if (is.null(rv$bbox)) {
      "No bounding box defined yet. Upload a study area or draw a rectangle."
    } else {
      paste0(
        "Bounding box:\nSW: (", round(rv$bbox[1], 4), ", ", round(rv$bbox[2], 4),
        ")\nNE: (", round(rv$bbox[3], 4), ", ", round(rv$bbox[4], 4), ")"
      )
    }
  })
  
  ##################################################################################################
  # MAIN RESULT STORE
  ##################################################################################################
  result_data <- reactiveVal(NULL)
  
  ##################################################################################################
  # RUN QUERY
  ##################################################################################################
  observeEvent(input$run_query, {
    req(input$date_range)
    req(rv$bbox)
    
    start_date <- as.Date(input$date_range[1])
    end_date   <- as.Date(input$date_range[2])
    
    swlat <- rv$bbox[1]
    swlng <- rv$bbox[2]
    nelat <- rv$bbox[3]
    nelng <- rv$bbox[4]
    
    diagnostics <- list(
      weekly_windows = 0,
      weeks_subdivided_to_days = 0,
      days_subdivided_to_halfdays = 0,
      windows_hit_limit = 0,
      total_rows = 0,
      adaptive_used = FALSE
    )
    
    ################################################################################################
    # ARCHIVED MODE
    ################################################################################################
    if (input$data_source == "archived") {
      
      inat_all_raw <- arrow::read_parquet(parquet_path)
      
      inat_all <- inat_all_raw %>%
        filter(!is.na(latitude) & !is.na(longitude)) %>%
        filter(
          latitude  >= swlat,
          latitude  <= nelat,
          longitude >= swlng,
          longitude <= nelng
        ) |>
        collect()
      
      inat_all <- standardize_inat_columns(inat_all)
      
      if ("observed_on" %in% names(inat_all)) {
        inat_all <- inat_all %>%
          filter(!is.na(observed_on)) %>%
          filter(as.Date(observed_on) >= start_date, as.Date(observed_on) <= end_date)
      }
      
      if (input$filter_mode == "taxonomy") {
        inat_all <- filter_by_taxonomy(
          df               = inat_all,
          query_type       = input$query_type,
          iconic_taxon     = input$iconic_taxon,
          order_name       = input$order_name,
          family_name      = input$family_name,
          taxon_name_input = input$taxon_name_input
        )
      } else {
        inat_all <- filter_by_threat_status(
          inat_all,
          threat_status = input$threat_status,
          require_iucn_for_exact = TRUE
        )
      }
      
      hm <- get_high_mortality_days(inat_all, time_bin = input$time_bin)
      
      merged_high <- if (!is.null(hm$days)) {
        inat_all %>%
          mutate(obs_bin = make_time_bin(as.Date(observed_on), time_bin = input$time_bin)) %>%
          filter(obs_bin %in% hm$days)
      } else {
        inat_all
      }
      
      diagnostics$total_rows <- nrow(inat_all)
      
      query_res <- list(
        merged_df_all    = inat_all,
        merged_df        = merged_high,
        daily_plot       = make_daily_plot(
          inat_all,
          start_date = start_date,
          end_date = end_date,
          time_bin = input$time_bin,
          metric_type = input$metric_type,
          show_smoother = input$show_smoother
        ),
        top_species_plot = make_top_species_plot(inat_all),
        map_hotspots_gg  = make_hexbin_map(inat_all, start_date, end_date),
        diagnostics      = diagnostics,
        threat_mode_label = make_filter_status_mode_label("archived", input$filter_mode)
      )
      
      result_data(query_res)
      rv$metadata <- make_query_metadata(input, rv$bbox, "archived", diagnostics, nrow(inat_all))
    }
    
    ################################################################################################
    # LIVE MODE
    ################################################################################################
    if (input$data_source == "live") {
      
      iconic_val <- NULL
      taxon_val <- NULL
      threatened_flag <- FALSE
      
      if (input$filter_mode == "taxonomy") {
        iconic_val <- if (input$query_type == "iconic") input$iconic_taxon else NULL
        if (input$query_type == "order"  && nzchar(input$order_name))       taxon_val <- input$order_name
        if (input$query_type == "family" && nzchar(input$family_name))      taxon_val <- input$family_name
        if (input$query_type == "taxon"  && nzchar(input$taxon_name_input)) taxon_val <- input$taxon_name_input
      } else {
        if (input$threat_status %in% c("threatened_plus", "NT", "VU", "EN", "CR", "EW", "EX")) {
          threatened_flag <- TRUE
        }
      }
      
      showNotification(
        "Live Mode: starting adaptive query. Returned records will then be strictly post-filtered using taxon.conservation_status.status_name.",
        duration = 8,
        type = "message"
      )
      
      progress <- shiny::Progress$new()
      on.exit(progress$close(), add = TRUE)
      
      progress$set(
        message = "Live Query: starting adaptive query",
        value   = 0
      )
      
      query_res <- getDeadVertebrates_dateRange(
        start_date      = start_date,
        end_date        = end_date,
        swlat           = swlat,
        swlng           = swlng,
        nelat           = nelat,
        nelng           = nelng,
        iconic_taxa     = iconic_val,
        taxon_name      = taxon_val,
        threatened_flag = threatened_flag,
        exact_threat_status = if (input$filter_mode == "status") input$threat_status else "all",
        per_page        = 200,
        .shiny_progress = progress,
        time_bin        = input$time_bin,
        metric_type     = input$metric_type,
        show_smoother   = input$show_smoother
      )
      
      query_res$threat_mode_label <- make_filter_status_mode_label("live", input$filter_mode)
      
      result_data(query_res)
      rv$metadata <- make_query_metadata(input, rv$bbox, "live", query_res$diagnostics, nrow(query_res$merged_df_all))
    }
  })
  
  ##################################################################################################
  # OUTPUTS
  ##################################################################################################
  output$samplingNote <- renderText({
    make_sampling_note(input$metric_type)
  })
  
  output$dailyPlot <- renderPlot({
    req(result_data())
    result_data()$daily_plot
  }, res = 140)
  
  output$dailySummary <- renderText({
    req(result_data())
    make_summary_text(result_data()$merged_df_all)
  })
  
  output$queryDiagnostics <- renderText({
    req(result_data())
    make_diagnostics_text(
      result_data()$diagnostics,
      threat_mode_label = result_data()$threat_mode_label
    )
  })
  
  output$speciesPlot <- renderPlot({
    req(result_data())
    result_data()$top_species_plot
  }, res = 140)
  
  output$hotspotMap <- renderPlot({
    req(result_data())
    result_data()$map_hotspots_gg
  }, res = 160)
  
  output$hotspotLeaflet <- renderLeaflet({
    req(result_data())
    make_leaflet_hotspot_map(
      df   = result_data()$merged_df_all,
      bbox = rv$bbox
    )
  })
  
  output$dataTable <- DT::renderDataTable({
    req(result_data())
    df <- result_data()$merged_df_all
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No records found"), options = list(pageLength = 20)))
    }
    
    df <- standardize_inat_columns(df)
    
    if ("id" %in% names(df)) {
      df$inat_link <- paste0(
        "<a href='https://www.inaturalist.org/observations/", df$id, "' target='_blank'>", df$id, "</a>"
      )
    } else {
      df$inat_link <- NA
    }
    
    df$image_thumb <- "No Img"
    
    if ("image_url" %in% names(df)) {
      df$image_thumb <- ifelse(
        !is.na(df$image_url) & df$image_url != "",
        paste0("<img src='", df$image_url, "' width='50'/>"),
        "No Img"
      )
    } else if ("taxon.default_photo.square_url" %in% names(df)) {
      df$image_thumb <- ifelse(
        !is.na(df$`taxon.default_photo.square_url`) & df$`taxon.default_photo.square_url` != "",
        paste0("<img src='", df$`taxon.default_photo.square_url`, "' width='50'/>"),
        "No Img"
      )
    }
    
    preferred_species_col <- if ("scientific_name_std" %in% names(df)) "scientific_name_std" else NULL
    
    show_cols <- c(
      "inat_link",
      "image_thumb",
      preferred_species_col,
      intersect(
        c(
          "common_name_std",
          "taxon_id_std",
          "taxon.conservation_status.authority",
          "taxon.conservation_status.status",
          "taxon.conservation_status.status_name",
          "threat_status_std",
          "threat_authority_raw",
          "threat_status_code_raw",
          "threat_status_raw"
        ),
        names(df)
      ),
      intersect(
        c(
          "observed_on",
          "quality_grade",
          "place_guess",
          "positional_accuracy",
          "user.id",
          "user.login",
          "user.name",
          "user_id",
          "user_login",
          "user_name",
          "observer_std",
          "latitude",
          "longitude"
        ),
        names(df)
      )
    )
    
    other_cols <- setdiff(names(df), c(show_cols, "taxon", "lat_str", "lon_str"))
    final_cols <- unique(c(show_cols, other_cols))
    final_cols <- final_cols[final_cols %in% names(df)]
    
    DT::datatable(
      df[, final_cols, drop = FALSE],
      escape = FALSE,
      options = list(
        pageLength = 20,
        autoWidth  = TRUE,
        scrollX    = TRUE
      )
    )
  })
  
  output$downloadAll <- downloadHandler(
    filename = function() paste0("inat_dead_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      req(result_data())
      readr::write_csv(result_data()$merged_df_all, file)
    }
  )
  
  output$downloadMetadata <- downloadHandler(
    filename = function() paste0("inat_query_metadata_", Sys.Date(), ".json"),
    content = function(file) {
      req(rv$metadata)
      jsonlite::write_json(rv$metadata, path = file, pretty = TRUE, auto_unbox = TRUE)
    }
  )
}

####################################################################################################
# RUN APP
####################################################################################################

shinyApp(ui = ui, server = server)