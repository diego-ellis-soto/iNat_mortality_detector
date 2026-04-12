
####################################################################################################
# app.R
#
# Dead Wildlife Observations from iNaturalist
# --------------------------------------------------------------------------------------------------
# PURPOSE
# --------------------------------------------------------------------------------------------------
# This application supports exploration of dead wildlife observations from iNaturalist in both:
#   1) archived mode (remote parquet snapshot)
#   2) live mode (dynamic iNaturalist API queries)
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
library(bslib)
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

escape_regex_literal <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

####################################################################################################
# COLUMN STANDARDIZATION
####################################################################################################

standardize_taxonomy_columns <- function(df) {
  
  scientific_col <- detect_first_existing_col(
    df,
    c(
      "taxon.name",
      "scientific_name",
      "taxon_name"
    )
  )
  
  family_col <- detect_first_existing_col(
    df,
    c(
      "taxon_family_name",
      "family",
      "family_name",
      "taxon.family_name"
    )
  )
  
  order_col <- detect_first_existing_col(
    df,
    c(
      "taxon_order_name",
      "order",
      "order_name",
      "taxon.order_name"
    )
  )
  
  rank_col <- detect_first_existing_col(
    df,
    c(
      "taxon.rank",
      "taxon_rank",
      "rank"
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
  
  ancestor_ids_col <- detect_first_existing_col(
    df,
    c(
      "taxon.ancestor_ids",
      "ancestor_ids",
      "taxon_ancestor_ids"
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
  
  if (!is.null(rank_col) && !"taxon_rank_std" %in% names(df)) {
    df$taxon_rank_std <- as.character(df[[rank_col]])
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
    df$taxon_id_std <- suppressWarnings(as.integer(df[[taxon_id_col]]))
  }
  
  if (!is.null(ancestor_ids_col) && !"taxon_ancestor_ids_std" %in% names(df)) {
    df$taxon_ancestor_ids_std <- vapply(df[[ancestor_ids_col]], function(x) {
      if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) return(NA_character_)
      if (is.atomic(x) && length(x) > 1) return(paste(x, collapse = ","))
      if (is.list(x)) {
        flat <- unlist(x, use.names = FALSE)
        if (length(flat) == 0) return(NA_character_)
        return(paste(flat, collapse = ","))
      }
      as.character(x)
    }, character(1))
  }
  
  df
}

has_selected_taxon_ancestor <- function(df, selected_taxon_id) {
  if (is.null(selected_taxon_id) || is.na(selected_taxon_id)) {
    return(rep(FALSE, nrow(df)))
  }
  
  df <- standardize_taxonomy_columns(df)
  sid <- as.character(selected_taxon_id)
  
  by_id <- if ("taxon_id_std" %in% names(df)) {
    suppressWarnings(as.character(df$taxon_id_std) == sid)
  } else {
    rep(FALSE, nrow(df))
  }
  
  by_ancestor <- if ("taxon_ancestor_ids_std" %in% names(df)) {
    vals <- as.character(df$taxon_ancestor_ids_std)
    vals[is.na(vals)] <- ""
    stringr::str_detect(vals, stringr::regex(paste0("(^|,)", sid, "(,|$)")))
  } else {
    rep(FALSE, nrow(df))
  }
  
  by_id | by_ancestor
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
          limits = c(as.Date(start_date), as.Date(end_date)),
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = expansion(mult = c(0.01, 0.02)),
          minor_breaks = NULL
        )
      )
    } else {
      return(
        scale_x_date(
          limits = c(as.Date(start_date), as.Date(end_date)),
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = expansion(mult = c(0.01, 0.02)),
          minor_breaks = NULL
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
          limits = c(as.Date(start_date), as.Date(end_date)),
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = expansion(mult = c(0.01, 0.02)),
          minor_breaks = NULL
        )
      )
    } else {
      return(
        scale_x_date(
          limits = c(as.Date(start_date), as.Date(end_date)),
          date_labels = "%b %Y",
          date_breaks = "1 month",
          expand = expansion(mult = c(0.01, 0.02)),
          minor_breaks = NULL
        )
      )
    }
  }
  
  span_days <- as.numeric(as.Date(end_date) - as.Date(start_date))
  if (is.na(span_days)) span_days <- 30
  
  if (span_days <= 10) {
    return(
      scale_x_date(
        limits = c(as.Date(start_date), as.Date(end_date)),
        date_labels = "%b %Y",
        date_breaks = "1 month",
        expand = expansion(mult = c(0.01, 0.02)),
        minor_breaks = NULL
      )
    )
  }
  if (span_days <= 45) {
    return(
      scale_x_date(
        limits = c(as.Date(start_date), as.Date(end_date)),
        date_labels = "%b %Y",
        date_breaks = "1 month",
        expand = expansion(mult = c(0.01, 0.02)),
        minor_breaks = NULL
      )
    )
  }
  if (span_days <= 120) {
    return(
      scale_x_date(
        limits = c(as.Date(start_date), as.Date(end_date)),
        date_labels = "%b %Y",
        date_breaks = "1 month",
        expand = expansion(mult = c(0.01, 0.02)),
        minor_breaks = NULL
      )
    )
  }
  if (span_days <= 400) {
    return(
      scale_x_date(
        limits = c(as.Date(start_date), as.Date(end_date)),
        date_labels = "%b %Y",
        date_breaks = "1 month",
        expand = expansion(mult = c(0.01, 0.02)),
        minor_breaks = NULL
      )
    )
  }
  if (span_days <= 1095) {
    return(
      scale_x_date(
        limits = c(as.Date(start_date), as.Date(end_date)),
        date_labels = "%b %Y",
        date_breaks = "1 month",
        expand = expansion(mult = c(0.01, 0.02)),
        minor_breaks = NULL
      )
    )
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
  
  out <- tryCatch(
    {
      if (is.na(sf::st_crs(study_area))) {
        sf::st_set_crs(study_area, 4326)
      } else {
        sf::st_transform(study_area, 4326)
      }
    },
    error = function(e) NULL
  )
  
  if (is.null(out) || nrow(out) == 0) return(NULL)
  
  out <- tryCatch(sf::st_make_valid(out), error = function(e) out)
  out <- out[!sf::st_is_empty(out), , drop = FALSE]
  
  if (nrow(out) == 0) return(NULL)
  
  out
}

# bbox_from_sf <- function(sf_obj) {
#   bb <- sf::st_bbox(sf_obj)
#   c(
#     as.numeric(bb["ymin"]),
#     as.numeric(bb["xmin"]),
#     as.numeric(bb["ymax"]),
#     as.numeric(bb["xmax"])
#   )
# }

WORLD_EPS <- 1e-6

normalize_longitude <- function(lon) {
  lon_in <- as.numeric(lon)
  lon_out <- ((lon_in + 180) %% 360) - 180

  lon_out[!is.na(lon_in) & abs(lon_in - 180) < WORLD_EPS] <- 180
  lon_out[!is.na(lon_in) & abs(lon_in + 180) < WORLD_EPS] <- -180

  lon_out
}

normalize_latitude <- function(lat) {
  pmax(-90, pmin(90, as.numeric(lat)))
}

validate_bbox <- function(bbox) {
  if (!is.list(bbox)) {
    stop("bbox must be a named list.")
  }
  
  required_names <- c(
    "swlat", "swlng", "nelat", "nelng",
    "crosses_antimeridian", "is_near_global", "lon_span"
  )
  
  if (!all(required_names %in% names(bbox))) {
    stop("bbox is missing required names.")
  }
  
  numeric_fields <- c("swlat", "swlng", "nelat", "nelng")
  
  if (any(!vapply(bbox[numeric_fields], is.numeric, logical(1)))) {
    stop("bbox coordinates must be numeric.")
  }
  
  if (any(!is.finite(unlist(bbox[numeric_fields])))) {
    stop("bbox coordinates must be finite.")
  }
  
  if (bbox$swlat < -90 || bbox$swlat > 90 || bbox$nelat < -90 || bbox$nelat > 90) {
    stop("Latitude values must be within [-90, 90].")
  }
  
  if (bbox$swlng < -180 || bbox$swlng > 180 || bbox$nelng < -180 || bbox$nelng > 180) {
    stop("Longitude values must be within [-180, 180].")
  }
  
  if (bbox$swlat > bbox$nelat) {
    stop("SW latitude must be <= NE latitude.")
  }
  
  TRUE
}

canonicalize_bbox <- function(lats, lngs) {
  lats <- normalize_latitude(lats)
  lngs <- normalize_longitude(lngs)
  
  if (length(lats) == 0 || length(lngs) == 0 || all(is.na(lats)) || all(is.na(lngs))) {
    stop("Cannot construct bounding box from empty coordinates.")
  }
  
  swlat <- min(lats, na.rm = TRUE)
  nelat <- max(lats, na.rm = TRUE)
  
  raw_min <- min(lngs, na.rm = TRUE)
  raw_max <- max(lngs, na.rm = TRUE)
  raw_span <- raw_max - raw_min
  
  lngs360 <- ifelse(lngs < 0, lngs + 360, lngs)
  wrap_min <- min(lngs360, na.rm = TRUE)
  wrap_max <- max(lngs360, na.rm = TRUE)
  wrapped_span <- 360 - (wrap_max - wrap_min)
  
  effective_span <- min(raw_span, wrapped_span)
  is_near_global <- raw_span >= (360 - WORLD_EPS) || effective_span >= 355
  
  if (is_near_global) {
    bbox <- list(
      swlat = swlat,
      swlng = -180,
      nelat = nelat,
      nelng = 180,
      crosses_antimeridian = FALSE,
      is_near_global = TRUE,
      lon_span = 360
    )
  } else if (raw_span <= 180) {
    bbox <- list(
      swlat = swlat,
      swlng = raw_min,
      nelat = nelat,
      nelng = raw_max,
      crosses_antimeridian = FALSE,
      is_near_global = FALSE,
      lon_span = raw_span
    )
  } else {
    bbox <- list(
      swlat = swlat,
      swlng = normalize_longitude(wrap_max),
      nelat = nelat,
      nelng = normalize_longitude(wrap_min),
      crosses_antimeridian = TRUE,
      is_near_global = FALSE,
      lon_span = wrapped_span
    )
  }
  
  validate_bbox(bbox)
  bbox
}

bbox_from_sf <- function(sf_obj) {
  bb <- sf::st_bbox(sf_obj)
  
  bbox <- canonicalize_bbox(
    lats = c(as.numeric(bb["ymin"]), as.numeric(bb["ymax"])),
    lngs = c(as.numeric(bb["xmin"]), as.numeric(bb["xmax"]))
  )
  
  validate_bbox(bbox)
  bbox
}

bbox_to_query_windows <- function(bbox, eps = 1e-6) {
  validate_bbox(bbox)

  windows <- if (isTRUE(bbox$is_near_global)) {
    list(
      list(swlng = -180,     nelng = 0),
      list(swlng = 0 + eps,  nelng = 180)
    )
  } else if (isTRUE(bbox$crosses_antimeridian)) {
    list(
      list(swlng = bbox$swlng, nelng = 180),
      list(swlng = -180,       nelng = bbox$nelng)
    )
  } else {
    list(list(swlng = bbox$swlng, nelng = bbox$nelng))
  }

  lapply(windows, function(w) {
    list(
      swlat = bbox$swlat,
      swlng = w$swlng,
      nelat = bbox$nelat,
      nelng = w$nelng
    )
  })
}


map_bounds_to_bbox <- function(map_bounds) {
  if (is.null(map_bounds)) return(NULL)
  required <- c("south", "west", "north", "east")
  if (!all(required %in% names(map_bounds))) return(NULL)

  tryCatch(
    canonicalize_bbox(
      lats = c(map_bounds$south, map_bounds$north),
      lngs = c(map_bounds$west, map_bounds$east)
    ),
    error = function(e) NULL
  )
}

extract_bbox_from_draw_feature <- function(feature, map_bounds = NULL) {
  if (is.null(feature$geometry) || feature$geometry$type != "Polygon") {
    stop("Drawn feature is not a polygon.")
  }

  coords <- feature$geometry$coordinates[[1]]
  lngs <- vapply(coords, function(x) as.numeric(x[[1]]), numeric(1))
  lats <- vapply(coords, function(x) as.numeric(x[[2]]), numeric(1))

  raw_bbox <- canonicalize_bbox(lats = lats, lngs = lngs)
  map_bbox <- map_bounds_to_bbox(map_bounds)

  raw_lon_span <- diff(range(lngs, na.rm = TRUE))
  raw_lat_span <- diff(range(lats, na.rm = TRUE))
  distinct_lngs <- length(unique(round(lngs, 6)))

  # Leaflet rectangle drawing on wrapped global maps can occasionally collapse a nearly
  # global rectangle to a thin strip near the dateline. When the current viewport is
  # essentially global and the drawn geometry is implausibly narrow in longitude but
  # very tall in latitude, interpret it as a global rectangle over the drawn latitude span.
  if (!is.null(map_bbox) &&
      isTRUE(map_bbox$is_near_global) &&
      !isTRUE(raw_bbox$is_near_global) &&
      raw_lon_span <= 20 &&
      raw_lat_span >= 45 &&
      distinct_lngs <= 3) {
    bbox <- list(
      swlat = min(lats, na.rm = TRUE),
      swlng = -180,
      nelat = max(lats, na.rm = TRUE),
      nelng = 180,
      crosses_antimeridian = FALSE,
      is_near_global = TRUE,
      lon_span = 360
    )
    validate_bbox(bbox)
    return(bbox)
  }

  raw_bbox
}

fit_leaflet_to_bbox <- function(map_obj, bbox) {
  validate_bbox(bbox)
  
  if (isTRUE(bbox$is_near_global)) {
    return(map_obj %>% fitBounds(
      lng1 = -180,
      lat1 = bbox$swlat,
      lng2 = 180,
      lat2 = bbox$nelat
    ))
  }
  
  if (!isTRUE(bbox$crosses_antimeridian)) {
    return(map_obj %>% fitBounds(
      lng1 = bbox$swlng,
      lat1 = bbox$swlat,
      lng2 = bbox$nelng,
      lat2 = bbox$nelat
    ))
  }
  
  map_obj %>% fitBounds(
    lng1 = -180,
    lat1 = bbox$swlat,
    lng2 = 180,
    lat2 = bbox$nelat
  )
}

coerce_bbox_to_polygon <- function(bbox) {
  validate_bbox(bbox)

  if (isTRUE(bbox$is_near_global)) {
    return(sf::st_as_sfc(sf::st_bbox(c(
      xmin = -180,
      ymin = bbox$swlat,
      xmax = 180,
      ymax = bbox$nelat
    ), crs = sf::st_crs(4326))))
  }

  if (!isTRUE(bbox$crosses_antimeridian)) {
    return(sf::st_as_sfc(sf::st_bbox(c(
      xmin = bbox$swlng,
      ymin = bbox$swlat,
      xmax = bbox$nelng,
      ymax = bbox$nelat
    ), crs = sf::st_crs(4326))))
  }

  left_poly <- sf::st_as_sfc(sf::st_bbox(c(
    xmin = bbox$swlng,
    ymin = bbox$swlat,
    xmax = 180,
    ymax = bbox$nelat
  ), crs = sf::st_crs(4326)))

  right_poly <- sf::st_as_sfc(sf::st_bbox(c(
    xmin = -180,
    ymin = bbox$swlat,
    xmax = bbox$nelng,
    ymax = bbox$nelat
  ), crs = sf::st_crs(4326)))

  sf::st_union(left_poly, right_poly)
}

filter_points_to_study_area <- function(df, study_area = NULL) {
  if (is.null(study_area)) return(df)
  if (is.null(df) || nrow(df) == 0) return(df)

  df <- standardize_inat_columns(df)

  if (!all(c("latitude", "longitude") %in% names(df))) {
    return(df[0, , drop = FALSE])
  }

  point_df <- df %>%
    dplyr::filter(!is.na(latitude), !is.na(longitude))

  if (nrow(point_df) == 0) {
    return(df[0, , drop = FALSE])
  }

  study_area <- tryCatch(prepare_uploaded_study_area(study_area), error = function(e) NULL)
  if (is.null(study_area) || nrow(study_area) == 0) {
    return(df[0, , drop = FALSE])
  }

  pts_sf <- tryCatch(
    sf::st_as_sf(point_df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE),
    error = function(e) NULL
  )

  if (is.null(pts_sf)) {
    return(df[0, , drop = FALSE])
  }

  hit <- tryCatch(
    lengths(sf::st_intersects(pts_sf, study_area)) > 0,
    error = function(e) {
      study_area_valid <- tryCatch(sf::st_make_valid(study_area), error = function(e2) study_area)
      lengths(sf::st_intersects(pts_sf, study_area_valid)) > 0
    }
  )

  point_df[hit, , drop = FALSE]
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
    taxon_name_input = NULL,
    selected_taxon_id = NULL,
    selected_taxon_name = NULL,
    selected_taxon_rank = NULL,
    selected_taxon_label = NULL
) {
  
  df <- standardize_taxonomy_columns(df)
  
  if (query_type == "iconic" && !is.null(iconic_taxon) && nzchar(iconic_taxon) && "iconic_taxon_name_std" %in% names(df)) {
    df <- df %>% filter(str_to_lower(iconic_taxon_name_std) == str_to_lower(iconic_taxon))
    return(df)
  }
  
  selected_taxon_name <- trimws(as.character(selected_taxon_name %||% ""))
  selected_taxon_rank <- trimws(as.character(selected_taxon_rank %||% ""))
  selected_taxon_id <- suppressWarnings(as.integer(selected_taxon_id))
  
  if (!is.na(selected_taxon_id)) {
    descendant_hit <- has_selected_taxon_ancestor(df, selected_taxon_id)
    if (any(descendant_hit, na.rm = TRUE)) {
      df <- df[descendant_hit, , drop = FALSE]
      return(df)
    }
  }
  
  if (nzchar(selected_taxon_name)) {
    if (selected_taxon_rank == "order" && "order_name_std" %in% names(df)) {
      df <- df %>% filter(str_to_lower(order_name_std) == str_to_lower(selected_taxon_name))
      return(df)
    }
    
    if (selected_taxon_rank == "family" && "family_name_std" %in% names(df)) {
      df <- df %>% filter(str_to_lower(family_name_std) == str_to_lower(selected_taxon_name))
      return(df)
    }
    
    if (selected_taxon_rank == "genus" && "scientific_name_std" %in% names(df)) {
      genus_pat <- paste0("^", escape_regex_literal(selected_taxon_name), "(\\s|$)")
      df <- df %>% filter(stringr::str_detect(scientific_name_std, regex(genus_pat, ignore_case = TRUE)))
      return(df)
    }
    
    if (selected_taxon_rank %in% c("species", "subspecies", "variety", "form", "hybrid") && "scientific_name_std" %in% names(df)) {
      sci_pat <- paste0("^", escape_regex_literal(selected_taxon_name), "(\\s|$)")
      df <- df %>% filter(stringr::str_detect(scientific_name_std, regex(sci_pat, ignore_case = TRUE)))
      return(df)
    }
  }
  
  if (query_type == "order" && !is.null(order_name) && nzchar(order_name) && "order_name_std" %in% names(df)) {
    df <- df %>% filter(str_to_lower(order_name_std) == str_to_lower(order_name))
  }
  
  if (query_type == "family" && !is.null(family_name) && nzchar(family_name) && "family_name_std" %in% names(df)) {
    df <- df %>% filter(str_to_lower(family_name_std) == str_to_lower(family_name))
  }
  
  if (query_type == "taxon" && !is.null(taxon_name_input) && nzchar(taxon_name_input) && "scientific_name_std" %in% names(df)) {
    taxon_pat <- paste0("^", escape_regex_literal(trimws(taxon_name_input)), "(\\s|$)")
    df <- df %>% filter(stringr::str_detect(scientific_name_std, regex(taxon_pat, ignore_case = TRUE)))
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
  validate_bbox(bbox)
  
  list(
    timestamp = as.character(Sys.time()),
    data_source = mode,
    bbox = list(
      swlat = bbox$swlat,
      swlng = bbox$swlng,
      nelat = bbox$nelat,
      nelng = bbox$nelng,
      crosses_antimeridian = bbox$crosses_antimeridian
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
    selected_taxon_id = if (input$filter_mode == "taxonomy") suppressWarnings(as.integer(input$order_taxon_choice %||% input$family_taxon_choice %||% input$taxon_autocomplete_choice)) else NULL,
    threat_status = if (input$filter_mode == "status") input$threat_status else NULL,
    returned_records = n_records,
    threat_status_mode = make_filter_status_mode_label(mode, input$filter_mode),
    diagnostics = diagnostics
  )
}

make_diagnostics_text <- function(diag, threat_mode_label = NULL) {
  paste0(
    "Query diagnostics:
",
    "- Total results reported by API: ", diag$total_results_reported %||% NA, "
",
    "- Paging used: ", ifelse(isTRUE(diag$paging_used), "Yes", "No"), "
",
    "- Pages fetched: ", diag$pages_fetched %||% 0, "
",
    "- ID batching used: ", ifelse(isTRUE(diag$used_id_batching), "Yes", "No"), "
",
    "- Weekly windows queried: ", diag$weekly_windows %||% 0, "
",
    "- Weeks subdivided to days: ", diag$weeks_subdivided_to_days %||% 0, "
",
    "- Days subdivided to half-days: ", diag$days_subdivided_to_halfdays %||% 0, "
",
    "- Windows that returned API cap (200): ", diag$windows_hit_limit %||% 0, "
",
    "- Final records returned: ", diag$total_rows %||% 0, "
",
    "- Adaptive querying used: ", ifelse(isTRUE(diag$adaptive_used), "Yes", "No"), "
",
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
    plot_df <- counts_by_bin %>% transmute(obs_bin, value = observers, Window = format(obs_bin, "%Y"))
    y_lab <- "Number of unique observers"
    plot_title <- "Observer Effort Through Time"
  } else if (metric_type == "obs_per_observer") {
    plot_df <- counts_by_bin %>% transmute(obs_bin, value = obs_per_observer, Window = format(obs_bin, "%Y"))
    y_lab <- "Dead wildlife observations per observer"
    plot_title <- "Dead Wildlife Observations Per Observer"
  } else {
    plot_df <- counts_by_bin %>% transmute(obs_bin, value = dead_obs, Window = format(obs_bin, "%Y"))
    y_lab <- "Number of dead wildlife observations"
    plot_title <- "Daily Dead Wildlife Observations"
  }
  
  plot_df <- plot_df %>% filter(!is.na(value))
  if (nrow(plot_df) == 0) return(ggplot() + theme_void() + labs(title = "No data available for selected metric"))
  
  anomaly_cutoff <- quantile(plot_df$value, probs = 0.90, na.rm = TRUE)
  anomaly_df <- plot_df %>% filter(value >= anomaly_cutoff)
  
  p <- ggplot(plot_df, aes(x = obs_bin, y = value, color = Window, group = Window)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.5) +
    geom_point(data = anomaly_df, aes(x = obs_bin, y = value), color = "red3", size = 2.6, inherit.aes = FALSE) +
    scale_color_viridis_d(option = "D", end = 0.9) +
    make_adaptive_date_scale(start_date, end_date, time_bin = time_bin) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.04))) +
    labs(
      title    = plot_title,
      subtitle = glue("{start_date} to {end_date}"),
      x        = "Observation date",
      y        = y_lab
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title        = element_text(face = "bold", size = 20),
      plot.subtitle     = element_text(color = "grey30", size = 12),
      axis.text.x       = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        color = "black",
        margin = margin(t = 8)
      ),
      axis.text.y       = element_text(color = "black"),
      axis.title.x      = element_text(face = "bold", margin = margin(t = 22)),
      axis.title.y      = element_text(face = "bold", margin = margin(r = 22)),
      panel.grid.minor  = element_blank(),
      plot.margin       = margin(t = 12, r = 24, b = 80, l = 55)
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
  
  # n_bins <- dplyr::case_when(
  #   nrow(df_coords) < 150   ~ 12,
  #   nrow(df_coords) < 500   ~ 18,
  #   nrow(df_coords) < 2000  ~ 25,
  #   nrow(df_coords) < 10000 ~ 35,
  #   TRUE ~ 45
  # )
  
  lon_range <- diff(range(df_coords$longitude, na.rm = TRUE))
  lat_range <- diff(range(df_coords$latitude, na.rm = TRUE))
  
  binwidth_lon <- max(0.08, lon_range / 50)
  binwidth_lat <- max(0.08, lat_range / 50)
  
  main_map <- ggplot() +
    geom_polygon(
      data = world_df,
      aes(x = long, y = lat, group = group),
      fill = "grey97",
      color = "grey88",
      linewidth = 0.12
    ) +
    # stat_bin_hex(
    #   data = df_coords,
    #   aes(x = longitude, y = latitude),
    #   bins = n_bins,
    #   linewidth = 0.08,
    #   color = scales::alpha("black", 0.18)
    # )
    # 
    stat_bin_hex(
      data = df_coords,
      aes(x = longitude, y = latitude),
      binwidth = c(binwidth_lon, binwidth_lat),
      linewidth = 0.08,
      color = scales::alpha("white", 0.18)
    ) +
    # scale_fill_viridis_c(option = "C", trans = "sqrt", labels = scales::comma, name = "Records") +
    scale_fill_viridis_c(
      option = "B",
      trans = "sqrt",
      labels = scales::comma,
      name = "Records"
    )+
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
  if (!all(c("latitude", "longitude") %in% names(df))) {
    return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron))
  }
  
  map_df <- df %>%
    standardize_inat_columns() %>%
    filter(!is.na(latitude), !is.na(longitude))
  
  if (nrow(map_df) == 0) {
    return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron))
  }
  
  if (nrow(map_df) > max_points) {
    set.seed(1)
    map_df <- dplyr::slice_sample(map_df, n = max_points)
  }
  
  sci_name <- if ("scientific_name_std" %in% names(map_df)) {
    map_df$scientific_name_std
  } else {
    rep(NA_character_, nrow(map_df))
  }
  
  common_name <- if ("common_name_std" %in% names(map_df)) {
    map_df$common_name_std
  } else {
    rep(NA_character_, nrow(map_df))
  }
  
  obs_date <- if ("observed_on" %in% names(map_df)) {
    as.character(map_df$observed_on)
  } else {
    rep(NA_character_, nrow(map_df))
  }
  
  popup_text <- if ("id" %in% names(map_df)) {
    paste0(
      "<b>", ifelse(!is.na(sci_name) & nzchar(sci_name), sci_name, "iNaturalist record"), "</b><br/>",
      ifelse(!is.na(common_name) & nzchar(common_name),
             paste0("Common name: ", common_name, "<br/>"),
             ""),
      ifelse(!is.na(obs_date) & nzchar(obs_date),
             paste0("Observed on: ", obs_date, "<br/>"),
             ""),
      "<a href='https://www.inaturalist.org/observations/", map_df$id,
      "' target='_blank'>Open this record on iNaturalist</a>"
    )
  } else {
    paste0(
      "<b>", ifelse(!is.na(sci_name) & nzchar(sci_name), sci_name, "iNaturalist record"), "</b><br/>",
      ifelse(!is.na(common_name) & nzchar(common_name),
             paste0("Common name: ", common_name, "<br/>"),
             ""),
      ifelse(!is.na(obs_date) & nzchar(obs_date),
             paste0("Observed on: ", obs_date, "<br/>"),
             "")
    )
  }
  
  m <- leaflet(map_df) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = 7,
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
    m <- fit_leaflet_to_bbox(m, bbox)
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
# TAXON RESOLUTION HELPERS
####################################################################################################

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

search_inat_taxa_autocomplete <- function(query, rank = NULL, per_page = 30) {
  query <- trimws(as.character(query %||% ""))
  if (!nzchar(query) || nchar(query) < 2) {
    return(tibble::tibble())
  }
  
  base_url <- "https://api.inaturalist.org/v1/taxa/autocomplete"
  req <- list(
    q = query,
    per_page = per_page,
    is_active = "true",
    all_names = "true"
  )
  
  if (!is.null(rank) && nzchar(rank)) {
    req$rank <- rank
  }
  
  resp <- tryCatch(httr::GET(base_url, query = req), error = function(e) NULL)
  if (is.null(resp) || httr::http_error(resp)) {
    return(tibble::tibble())
  }
  
  parsed <- tryCatch(
    httr::content(resp, as = "text", encoding = "UTF-8") %>% jsonlite::fromJSON(flatten = TRUE),
    error = function(e) NULL
  )
  
  if (is.null(parsed) || is.null(parsed$results) || length(parsed$results) == 0) {
    return(tibble::tibble())
  }
  
  out <- tibble::as_tibble(parsed$results)
  
  keep_cols <- intersect(c(
    "id",
    "name",
    "preferred_common_name",
    "rank",
    "rank_level",
    "matched_term",
    "iconic_taxon_name",
    "default_photo.medium_url"
  ), names(out))
  out <- out[, keep_cols, drop = FALSE]
  
  if (!"preferred_common_name" %in% names(out)) out$preferred_common_name <- NA_character_
  if (!"rank" %in% names(out)) out$rank <- NA_character_
  if (!"matched_term" %in% names(out)) out$matched_term <- NA_character_
  if (!"iconic_taxon_name" %in% names(out)) out$iconic_taxon_name <- NA_character_
  
  out %>%
    mutate(
      id = as.integer(id),
      label = dplyr::case_when(
        !is.na(preferred_common_name) & nzchar(preferred_common_name) ~ paste0(name, " (", preferred_common_name, ") · ", rank, " · id=", id),
        TRUE ~ paste0(name, " · ", rank, " · id=", id)
      )
    ) %>%
    distinct(id, .keep_all = TRUE)
}

choices_from_taxa <- function(taxa_df) {
  if (is.null(taxa_df) || nrow(taxa_df) == 0) return(setNames(character(0), character(0)))
  vals <- as.character(taxa_df$id)
  stats::setNames(vals, taxa_df$label)
}

get_selected_taxon_record <- function(choice_value, options_df) {
  choice_value <- as.character(choice_value %||% "")
  if (!nzchar(choice_value) || is.null(options_df) || nrow(options_df) == 0) return(NULL)
  hit <- options_df %>% filter(as.character(id) == choice_value)
  if (nrow(hit) == 0) return(NULL)
  hit[1, , drop = FALSE]
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
    taxon_id    = NULL,
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
  if (!is.null(taxon_id) && nzchar(as.character(taxon_id))) q_parts <- c(q_parts, glue("taxon_id={taxon_id}"))
  if ((is.null(taxon_id) || !nzchar(as.character(taxon_id))) && !is.null(taxon_name)  && nzchar(taxon_name))  q_parts <- c(q_parts, glue("taxon_name={URLencode(taxon_name)}"))
  if (isTRUE(threatened_flag)) q_parts <- c(q_parts, "threatened=true")
  
  query_params <- paste(q_parts, collapse = "&")
  
  loc_part <- if (
    !is.null(swlat) && !is.null(nelat) &&
    !is.null(swlng) && !is.null(nelng)
  ) {
    glue("&nelat={nelat}&nelng={nelng}&swlat={swlat}&swlng={swlng}")
  } else if (!is.null(swlat) && !is.null(nelat)) {
    glue("&nelat={nelat}&swlat={swlat}")
  } else {
    ""
  }
  
  if (!is.null(progress)) {
    progress$set(detail = glue("Fetching {start_date} to {end_date}"), value = NULL)
  }
  
  query_url <- paste0(base_url, "?", query_params, loc_part)
  resp <- httr::GET(query_url)
  
  Sys.sleep(1.2)
  
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

fetch_total_observation_count <- function(
    swlat,
    swlng,
    nelat,
    nelng,
    start_date,
    end_date,
    iconic_taxa = NULL,
    taxon_id = NULL,
    taxon_name = NULL,
    threatened_flag = FALSE
) {
  
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  q_parts <- list(
    "term_id=17",
    "term_value_id=19",
    glue("d1={start_date}"),
    glue("d2={end_date}"),
    "verifiable=true",
    "per_page=0"
  )
  
  if (!is.null(iconic_taxa) && nzchar(iconic_taxa)) {
    q_parts <- c(q_parts, glue("iconic_taxa={iconic_taxa}"))
  }
  
  if (!is.null(taxon_id) && nzchar(as.character(taxon_id))) {
    q_parts <- c(q_parts, glue("taxon_id={taxon_id}"))
  } else if (!is.null(taxon_name) && nzchar(taxon_name)) {
    q_parts <- c(q_parts, glue("taxon_name={URLencode(taxon_name)}"))
  }
  
  if (isTRUE(threatened_flag)) {
    q_parts <- c(q_parts, "threatened=true")
  }
  
  loc_part <- if (
    !is.null(swlat) && !is.null(nelat) &&
    !is.null(swlng) && !is.null(nelng)
  ) {
    glue("&swlat={swlat}&swlng={swlng}&nelat={nelat}&nelng={nelng}")
  } else if (!is.null(swlat) && !is.null(nelat)) {
    glue("&swlat={swlat}&nelat={nelat}")
  } else {
    ""
  }
  
  query_url <- paste0(
    base_url, "?",
    paste(q_parts, collapse = "&"),
    loc_part
  )
  
  resp <- httr::GET(query_url)
  
  if (httr::http_error(resp)) {
    warning("HTTP error while fetching total observation count: ", httr::status_code(resp))
    return(NA_integer_)
  }
  
  parsed <- httr::content(resp, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)
  
  parsed$total_results
}

fetch_dead_data_paged <- function(
    swlat,
    swlng,
    nelat,
    nelng,
    start_date,
    end_date,
    iconic_taxa = NULL,
    taxon_id = NULL,
    taxon_name = NULL,
    threatened_flag = FALSE,
    per_page = 200,
    max_pages_per_batch = 50,
    progress = NULL,
    progress_prefix = NULL
) {
  
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  base_query <- list(
    term_id = 17,
    term_value_id = 19,
    verifiable = "true",
    d1 = as.character(start_date),
    d2 = as.character(end_date),
    order = "asc",
    order_by = "id",
    per_page = per_page
  )
  
  if (!is.null(iconic_taxa) && nzchar(iconic_taxa)) {
    base_query$iconic_taxa <- iconic_taxa
  }
  
  if (!is.null(taxon_id) && nzchar(as.character(taxon_id))) {
    base_query$taxon_id <- as.character(taxon_id)
  } else if (!is.null(taxon_name) && nzchar(taxon_name)) {
    base_query$taxon_name <- taxon_name
  }
  
  if (isTRUE(threatened_flag)) {
    base_query$threatened <- "true"
  }
  
  if (!is.null(swlat) && !is.null(nelat)) {
    base_query$swlat <- swlat
    base_query$nelat <- nelat
  }
  
  if (!is.null(swlng) && !is.null(nelng)) {
    base_query$swlng <- swlng
    base_query$nelng <- nelng
  }
  
  count_query <- base_query
  count_query$per_page <- 0
  
  count_resp <- httr::GET(base_url, query = count_query)
  if (httr::http_error(count_resp)) {
    warning("HTTP error while fetching total count: ", httr::status_code(count_resp))
    return(list(
      data = tibble::tibble(),
      total_results = NA_integer_,
      total_pages = NA_integer_,
      pages_fetched = 0L,
      used_id_batching = FALSE
    ))
  }
  
  count_parsed <- httr::content(count_resp, as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON(flatten = TRUE)
  
  total_results <- count_parsed$total_results %||% 0L
  total_pages <- ceiling(total_results / per_page)
  
  if (total_results == 0) {
    return(list(
      data = tibble::tibble(),
      total_results = 0L,
      total_pages = 0L,
      pages_fetched = 0L,
      used_id_batching = FALSE
    ))
  }
  
  all_pages <- list()
  out_i <- 1L
  pages_fetched <- 0L
  id_above <- NULL
  used_id_batching <- FALSE
  
  repeat {
    page <- 1L
    pages_in_this_batch <- 0L
    last_id <- NULL
    got_any_results <- FALSE
    
    repeat {
      if (pages_in_this_batch >= max_pages_per_batch) break
      
      if (!is.null(progress)) {
        detail_text <- paste0(
          if (!is.null(progress_prefix)) paste0(progress_prefix, " · ") else "",
          "page ", pages_fetched + 1L, " of ~", total_pages
        )
        progress$set(detail = detail_text, value = NULL)
      }
      
      q <- base_query
      q$page <- page
      if (!is.null(id_above)) q$id_above <- id_above
      
      resp <- httr::GET(base_url, query = q)
      Sys.sleep(1.0)
      
      if (httr::http_error(resp)) {
        warning("HTTP error while fetching observations page: ", httr::status_code(resp))
        break
      }
      
      parsed <- httr::content(resp, as = "text", encoding = "UTF-8") |>
        jsonlite::fromJSON(flatten = TRUE)
      
      results_df <- tibble::as_tibble(parsed$results)
      if (nrow(results_df) == 0) break
      
      got_any_results <- TRUE
      all_pages[[out_i]] <- results_df
      out_i <- out_i + 1L
      
      last_id <- max(results_df$id, na.rm = TRUE)
      page <- page + 1L
      pages_in_this_batch <- pages_in_this_batch + 1L
      pages_fetched <- pages_fetched + 1L
    }
    
    if (!got_any_results) break
    if (pages_in_this_batch < max_pages_per_batch) break
    
    id_above <- last_id
    used_id_batching <- TRUE
  }
  
  out <- dplyr::bind_rows(all_pages) |>
    dplyr::distinct(id, .keep_all = TRUE)
  
  list(
    data = out,
    total_results = total_results,
    total_pages = total_pages,
    pages_fetched = pages_fetched,
    used_id_batching = used_id_batching
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
    taxon_id = NULL,
    taxon_name = NULL,
    threatened_flag = FALSE,
    per_page = 200,
    progress = NULL,
    tile_index = 1,
    total_tiles = 1
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

  progress_value <- function(tile_index, total_tiles, step_value) {
    ((tile_index - 1) + step_value) / max(1, total_tiles)
  }
  
  for (i in seq_along(weekly_windows)) {
    wk <- weekly_windows[[i]]
    wk_start <- as.Date(wk$start)
    wk_end   <- as.Date(wk$end)
    
    if (!is.null(progress)) {
      progress$set(
        value = progress_value(tile_index, total_tiles, (i - 1) / max(1, total_weeks)),
        message = glue("Live Query: tile {tile_index} of {total_tiles} · week {i} of {total_weeks}"),
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
      taxon_id = taxon_id,
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
          value = progress_value(tile_index, total_tiles, (i - 1) / max(1, total_weeks)),
          message = glue("Live Query: tile {tile_index} of {total_tiles} · week {i} returned {per_page}; subdividing to days"),
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
            value = progress_value(tile_index, total_tiles, (i - 1 + d / max(1, length(daily_windows))) / max(1, total_weeks)),
            message = glue("Live Query: tile {tile_index} of {total_tiles} · week {i} · day {d} of {length(daily_windows)}"),
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
          taxon_id = taxon_id,
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
              value = progress_value(tile_index, total_tiles, (i - 1 + d / max(1, length(daily_windows))) / max(1, total_weeks)),
              message = glue("Live Query: tile {tile_index} of {total_tiles} · day {dy_start} returned {per_page}; subdividing to 12-hour windows"),
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
              taxon_id = taxon_id,
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
    taxon_id        = NULL,
    taxon_name      = NULL,
    threatened_flag = FALSE,
    exact_threat_status = "all",
    per_page        = 200,
    .shiny_progress = NULL,
    time_bin        = "day",
    metric_type     = "raw_counts",
    show_smoother   = TRUE,
    tile_index      = 1,
    total_tiles     = 1
) {
  
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  total_n <- fetch_total_observation_count(
    swlat = swlat,
    swlng = swlng,
    nelat = nelat,
    nelng = nelng,
    start_date = start_date,
    end_date = end_date,
    iconic_taxa = iconic_taxa,
    taxon_id = taxon_id,
    taxon_name = taxon_name,
    threatened_flag = threatened_flag
  )
  
  diagnostics <- list(
    weekly_windows = 0,
    weeks_subdivided_to_days = 0,
    days_subdivided_to_halfdays = 0,
    windows_hit_limit = 0,
    total_rows = 0,
    adaptive_used = FALSE,
    total_results_reported = total_n,
    paging_used = FALSE,
    pages_fetched = 0,
    used_id_batching = FALSE
  )
  
  if (!is.na(total_n) && total_n <= 250000) {
    paged_res <- fetch_dead_data_paged(
      swlat = swlat,
      swlng = swlng,
      nelat = nelat,
      nelng = nelng,
      start_date = start_date,
      end_date = end_date,
      iconic_taxa = iconic_taxa,
      taxon_id = taxon_id,
      taxon_name = taxon_name,
      threatened_flag = threatened_flag,
      per_page = per_page,
      max_pages_per_batch = 50,
      progress = .shiny_progress,
      progress_prefix = paste0("tile ", tile_index, " of ", total_tiles)
    )
    
    merged_df_all <- paged_res$data
    diagnostics$paging_used <- TRUE
    diagnostics$pages_fetched <- paged_res$pages_fetched
    diagnostics$used_id_batching <- paged_res$used_id_batching
    diagnostics$total_results_reported <- paged_res$total_results
    
  } else {
    adaptive_res <- fetch_dead_data_adaptive(
      start_date = start_date,
      end_date   = end_date,
      swlat      = swlat,
      swlng      = swlng,
      nelat      = nelat,
      nelng      = nelng,
      iconic_taxa = iconic_taxa,
      taxon_id    = taxon_id,
      taxon_name  = taxon_name,
      threatened_flag = threatened_flag,
      per_page    = per_page,
      progress    = .shiny_progress,
      tile_index  = tile_index,
      total_tiles = total_tiles
    )
    
    merged_df_all <- adaptive_res$data
    diagnostics[names(adaptive_res$diagnostics)] <- adaptive_res$diagnostics
  }
  
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
  # theme = shinytheme("cosmo"),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C7FB8",
    base_font = font_google("Inter")
  ),
  useShinyjs(),
  
  tags$head(
    # tags$style(HTML("
    #   .main-plot-panel { min-height: 700px; }
    #   .shiny-plot-output { overflow: visible !important; }
    #   .about-card {
    #     background: #fafafa;
    #     border: 1px solid #e3e3e3;
    #     border-radius: 10px;
    #     padding: 18px;
    #     margin-bottom: 14px;
    #   }
    #   .about-icon { font-size: 24px; margin-right: 8px; }
    #   .about-heading { font-weight: 700; margin-bottom: 10px; }
    #   .about-subtle { color: #666666; }
    #   .map-note { font-size: 12px; color: #666666; margin-bottom: 8px; }
    #   .note-box {
    #     background: #f8f9fb;
    #     border-left: 4px solid #6c8ebf;
    #     padding: 10px 12px;
    #     margin: 10px 0;
    #     font-size: 13px;
    #   }
    #   .silhouette-row {
    #     display: flex;
    #     gap: 18px;
    #     align-items: center;
    #     justify-content: center;
    #     margin: 10px 0 4px 0;
    #     opacity: 0.85;
    #     font-size: 32px;
    #   }
    #   .warning-box {
    #     background: #fff7e6;
    #     border-left: 4px solid #f0ad4e;
    #     padding: 10px 12px;
    #     margin: 10px 0;
    #     font-size: 13px;
    #   }
    # "))
    tags$style(HTML("
  body {
    background-color: #F7F9FB;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial;
  }
  
  .main-plot-panel {
  min-height: 850px;
  background: white;
  border-radius: 8px;
  padding: 25px 25px 60px 25px;
  box-shadow: 0 2px 6px rgba(0,0,0,0.08);
  overflow: visible !important;
}

  # .main-plot-panel {
  #   min-height: 700px;
  #   background: white;
  #   border-radius: 8px;
  #   padding: 15px;
  #   box-shadow: 0 2px 6px rgba(0,0,0,0.08);
  # }

  .shiny-plot-output { overflow: visible !important; }

  .about-card {
    background: #FFFFFF;
    border: 1px solid #E5E5E5;
    border-radius: 10px;
    padding: 18px;
    margin-bottom: 14px;
    box-shadow: 0 1px 4px rgba(0,0,0,0.05);
  }

  .about-heading {
    font-weight: 600;
    margin-bottom: 10px;
    color: #1F1F1F;
  }

  .about-subtle { color: #666666; }

  .map-note {
    font-size: 12px;
    color: #666666;
    margin-bottom: 8px;
  }

  .note-box {
    background: #F5F7FA;
    border-left: 4px solid #2C7FB8;
    padding: 10px 12px;
    margin: 10px 0;
    font-size: 13px;
  }

  .warning-box {
    background: #FFF4E5;
    border-left: 4px solid #F0AD4E;
    padding: 10px 12px;
    margin: 10px 0;
    font-size: 13px;
  }

  .nav-tabs > li > a {
    font-weight: 500;
    color: #555;
  }

  .nav-tabs > li.active > a {
    border-bottom: 3px solid #2C7FB8;
    color: #2C7FB8;
    background-color: transparent;
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
      # titlePanel("Dead Wildlife Observations from iNaturalist")
      div(
        style = "padding-top: 20px;",
        h2("Dead Wildlife Observations from iNaturalist", style = "font-weight:600;")
      )
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
              choices = 
                c(
                  "Aves", "Mammalia", "Reptilia", "Amphibia",
                  "Actinopterygii", "Chondrichthyes",
                  "Insecta", "Arachnida", "Malacostraca", "Chilopoda", "Diplopoda",
                  "Gastropoda", "Bivalvia", "Cephalopoda", "Polyplacophora",
                  "Clitellata",
                  "Anthozoa", "Hydrozoa", "Scyphozoa",
                  "Asteroidea", "Echinoidea", "Holothuroidea",
                  "Demospongiae"
                ),
              selected = "Aves"
            )
          ),
          
          conditionalPanel(
            condition = "input.filter_mode == 'taxonomy' && input.query_type == 'order'",
            tagList(
              textInput("order_name", "Search order name", ""),
              selectInput("order_taxon_choice", "Select resolved order", choices = character(0)),
              helpText("The app resolves the typed text against iNaturalist taxa and uses the selected taxon ID for live queries.")
            )
          ),
          
          conditionalPanel(
            condition = "input.filter_mode == 'taxonomy' && input.query_type == 'family'",
            tagList(
              textInput("family_name", "Search family name", ""),
              selectInput("family_taxon_choice", "Select resolved family", choices = character(0)),
              helpText("Using the iNaturalist family taxon ID retrieves descendant taxa in live mode.")
            )
          ),
          
          conditionalPanel(
            condition = "input.filter_mode == 'taxonomy' && input.query_type == 'taxon'",
            tagList(
              textInput("taxon_name_input", "Search taxon (species, genus, family, order, or common name)", ""),
              selectInput("taxon_autocomplete_choice", "Select resolved taxon", choices = character(0)),
              helpText("Select the intended iNaturalist taxon so live queries use taxon_id rather than ambiguous free text.")
            )
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
            "In Live mode, the app first asks the API how many records exist for the full query window. Smaller result sets are then retrieved by paging across the full range instead of looping through empty weekly windows."
          ),
          tags$ul(
            tags$li("If the full query is modest in size, the app fetches all pages directly across the whole date range."),
            tags$li("This means small places or sparse taxa can often be retrieved with one count call plus a small number of page calls, rather than one call per week."),
            tags$li("If the total result set is large, the app falls back to adaptive week -> day -> 12-hour subdivision."),
            tags$li("For most live queries, the app now stays in page-based retrieval and only falls back to temporal subdivision for extremely large result sets. Page batches continue with ascending IDs so retrieval remains complete beyond the first 10,000 records.")
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
          tags$h4("How bounding boxes work"),
          tags$p(
            "Bounding boxes are automatically standardized to valid geographic coordinates (−180° to 180° longitude) to ensure consistent global querying."
          ),
          tags$ul(
            tags$li("Coordinates returned by the map are normalized to standard WGS84 longitude ranges before use."),
            tags$li("If a selected region crosses the 180° meridian (Pacific region), the app will indicate this (e.g., 'Crosses antimeridian: Yes')."),
            tags$li("In these cases, the query is internally split and handled correctly to avoid missing records."),
            tags$li("This ensures accurate results for regions such as Australia, New Zealand, and Indo-Pacific islands.")
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
    
    # mainPanel(
    mainPanel(
      style = "padding: 20px;",
      tabsetPanel(
        tabPanel(
          "Daily Time Series",
          div(class = "note-box", textOutput("samplingNote")),
          fluidRow(
            column(
              width = 8,
              div(
                class = "main-plot-panel",
                # Added total number of observations
                div(
                  style = "margin-bottom: 8px; font-weight: 600;",
                  textOutput("total_inat_text")
                ),
                # withSpinner(plotOutput("dailyPlot", height = "650px"), type = 6)
                withSpinner(plotOutput("dailyPlot", height = "850px"), type = 6)
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
    metadata = NULL,
    total_obs_count = NA_integer_,
    order_taxa_options = tibble::tibble(),
    family_taxa_options = tibble::tibble(),
    general_taxa_options = tibble::tibble()
  )
  
  order_search_text <- shiny::debounce(reactive(trimws(input$order_name %||% "")), 500)
  family_search_text <- shiny::debounce(reactive(trimws(input$family_name %||% "")), 500)
  taxon_search_text <- shiny::debounce(reactive(trimws(input$taxon_name_input %||% "")), 500)
  
  observeEvent(order_search_text(), {
    q <- order_search_text()
    if (!nzchar(q) || nchar(q) < 2) {
      rv$order_taxa_options <- tibble::tibble()
      updateSelectInput(session, "order_taxon_choice", choices = character(0), selected = character(0))
      return()
    }
    out <- search_inat_taxa_autocomplete(q, rank = "order", per_page = 20)
    rv$order_taxa_options <- out
    updateSelectInput(session, "order_taxon_choice", choices = choices_from_taxa(out), selected = if (nrow(out) > 0) as.character(out$id[[1]]) else character(0))
  }, ignoreInit = TRUE)
  
  observeEvent(family_search_text(), {
    q <- family_search_text()
    if (!nzchar(q) || nchar(q) < 2) {
      rv$family_taxa_options <- tibble::tibble()
      updateSelectInput(session, "family_taxon_choice", choices = character(0), selected = character(0))
      return()
    }
    out <- search_inat_taxa_autocomplete(q, rank = "family", per_page = 20)
    rv$family_taxa_options <- out
    updateSelectInput(session, "family_taxon_choice", choices = choices_from_taxa(out), selected = if (nrow(out) > 0) as.character(out$id[[1]]) else character(0))
  }, ignoreInit = TRUE)
  
  observeEvent(taxon_search_text(), {
    q <- taxon_search_text()
    if (!nzchar(q) || nchar(q) < 2) {
      rv$general_taxa_options <- tibble::tibble()
      updateSelectInput(session, "taxon_autocomplete_choice", choices = character(0), selected = character(0))
      return()
    }
    out <- search_inat_taxa_autocomplete(q, rank = NULL, per_page = 20)
    rv$general_taxa_options <- out
    updateSelectInput(session, "taxon_autocomplete_choice", choices = choices_from_taxa(out), selected = if (nrow(out) > 0) as.character(out$id[[1]]) else character(0))
  }, ignoreInit = TRUE)
  
  selected_taxonomy <- reactive({
    if (input$filter_mode != "taxonomy") {
      return(list(id = NULL, name = NULL, rank = NULL, label = NULL))
    }
    if (input$query_type == "iconic") {
      return(list(id = NULL, name = input$iconic_taxon, rank = "iconic", label = input$iconic_taxon))
    }
    if (input$query_type == "order") {
      hit <- get_selected_taxon_record(input$order_taxon_choice, rv$order_taxa_options)
    } else if (input$query_type == "family") {
      hit <- get_selected_taxon_record(input$family_taxon_choice, rv$family_taxa_options)
    } else {
      hit <- get_selected_taxon_record(input$taxon_autocomplete_choice, rv$general_taxa_options)
    }
    if (is.null(hit) || nrow(hit) == 0) {
      return(list(id = NULL, name = NULL, rank = NULL, label = NULL))
    }
    list(
      id = as.integer(hit$id[[1]]),
      name = as.character(hit$name[[1]] %||% NA_character_),
      rank = as.character(hit$rank[[1]] %||% NA_character_),
      label = as.character(hit$label[[1]] %||% hit$name[[1]])
    )
  })

  ##################################################################################################
  # BBOX SELECTION MAP
  ##################################################################################################
  output$select_map <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        worldCopyJump = FALSE,
        minZoom = 1,
        maxBoundsViscosity = 1
      )
    ) %>%
      addProviderTiles(
        providers$OpenStreetMap,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      fitBounds(lng1 = -179.999, lat1 = -85, lng2 = 179.999, lat2 = 85) %>%
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
      bbox <- tryCatch(
        {
          bb <- extract_bbox_from_draw_feature(
            feature = feat,
            map_bounds = input$select_map_bounds
          )
          validate_bbox(bb)
          bb
        },
        error = function(e) {
          showNotification(
            paste("Invalid bounding box:", e$message),
            type = "error",
            duration = 8
          )
          return(NULL)
        }
      )
      
      if (!is.null(bbox)) {
        rv$bbox <- bbox
        rv$query_token <- rv$query_token + 1
      }
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
          bbox <- tryCatch(
            {
              bb <- extract_bbox_from_draw_feature(
                feature = feat,
                map_bounds = input$select_map_bounds
              )
              validate_bbox(bb)
              bb
            },
            error = function(e) {
              showNotification(
                paste("Invalid edited bounding box:", e$message),
                type = "error",
                duration = 8
              )
              return(NULL)
            }
          )
          
          if (!is.null(bbox)) {
            rv$bbox <- bbox
            rv$query_token <- rv$query_token + 1
          }
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
    
    # Simplify study area ####
    # study_area = sf::st_simplify(
    #   study_area,
    #   dTolerance = 0.0001,
    #   preserveTopology = TRUE
    # )
    # Re-validate after simplify, just to be safe
    study_area <- sf::st_make_valid(study_area)
    
    if (is.null(study_area) || nrow(study_area) == 0) return()
    
    study_area <- prepare_uploaded_study_area(study_area)
    
    if (is.null(study_area)) {
      showNotification("Could not transform study area to WGS84.", type = "error", duration = 8)
      return()
    }
    
    bbox <- bbox_from_sf(study_area)
    
    map_proxy <- leafletProxy("select_map") %>%
      clearGroup("search_bbox") %>%
      addPolygons(
        data = study_area,
        fillColor = "red",
        fillOpacity = 0.1,
        color = "red",
        weight = 2,
        group = "search_bbox"
      )

    fit_leaflet_to_bbox(map_proxy, bbox)
    
    rv$bbox <- bbox
    rv$uploaded_study_area <- study_area
    rv$query_token <- rv$query_token + 1
    
    showNotification("Study area uploaded successfully.", type = "message", duration = 4)
  })
  
  output$bbox_coords <- renderText({
    if (is.null(rv$bbox)) {
      return("No bounding box selected.")
    }
    
    bb <- rv$bbox
    
    paste0(
      "Bounding box:\n",
      "SW: (", round(as.numeric(bb$swlat), 4), ", ", round(as.numeric(bb$swlng), 4), ")\n",
      "NE: (", round(as.numeric(bb$nelat), 4), ", ", round(as.numeric(bb$nelng), 4), ")\n"
      # ,
      # "Crosses antimeridian: ", ifelse(isTRUE(bb$crosses_antimeridian), "Yes", "No"), "\n",
      # "Near-global extent: ", ifelse(isTRUE(bb$is_near_global), "Yes", "No")
    )
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
    
    bb <- rv$bbox
    validate_bbox(bb)
    
    swlat <- bb$swlat
    swlng <- bb$swlng
    nelat <- bb$nelat
    nelng <- bb$nelng
    crosses_antimeridian <- isTRUE(bb$crosses_antimeridian)
    is_near_global <- isTRUE(bb$is_near_global)
    query_windows <- bbox_to_query_windows(bb)
    
    
    diagnostics <- list(
      weekly_windows = 0,
      weeks_subdivided_to_days = 0,
      days_subdivided_to_halfdays = 0,
      windows_hit_limit = 0,
      total_rows = 0,
      adaptive_used = FALSE
    )
    
    # NEW: total iNat observation count
    taxonomy_sel <- selected_taxonomy()
    if (input$filter_mode == "taxonomy" && input$query_type != "iconic" && is.null(taxonomy_sel$id)) {
      showNotification(
        "Please select a resolved iNaturalist taxon from the autocomplete dropdown before running the query.",
        type = "error",
        duration = 8
      )
      return(NULL)
    }
    iconic_for_count <- NULL
    taxon_id_for_count <- NULL
    taxon_for_count <- NULL
    threatened_for_count <- FALSE
    
    if (input$filter_mode == "taxonomy") {
      if (input$query_type == "iconic") {
        iconic_for_count <- input$iconic_taxon
      } else {
        taxon_id_for_count <- taxonomy_sel$id
        taxon_for_count <- taxonomy_sel$name
      }
    } else {
      if (input$threat_status %in% c("threatened_plus", "NT", "VU", "EN", "CR", "EW", "EX")) {
        threatened_for_count <- TRUE
      }
    }
    
    # rv$total_obs_count <- fetch_total_observation_count(
    #   swlat = swlat,
    #   swlng = swlng,
    #   nelat = nelat,
    #   nelng = nelng,
    #   start_date = start_date,
    #   end_date = end_date,
    #   iconic_taxa = iconic_for_count,
    #   taxon_name = taxon_for_count,
    #   threatened_flag = threatened_for_count
    # )
    
    rv$total_obs_count <- sum(vapply(
      query_windows,
      function(w) {
        fetch_total_observation_count(
          swlat = w$swlat,
          swlng = w$swlng,
          nelat = w$nelat,
          nelng = w$nelng,
          start_date = start_date,
          end_date = end_date,
          iconic_taxa = iconic_for_count,
          taxon_id = taxon_id_for_count,
          taxon_name = taxon_for_count,
          threatened_flag = threatened_for_count
        )
      },
      numeric(1)
    ), na.rm = TRUE)
    
    ################################################################################################
    # ARCHIVED MODE
    ################################################################################################
    if (input$data_source == "archived") {
      
      inat_all_raw <- arrow::read_parquet(parquet_path)

      
      archived_parts <- lapply(query_windows, function(w) {
        inat_all_raw %>%
          filter(!is.na(latitude) & !is.na(longitude)) %>%
          filter(
            latitude  >= w$swlat,
            latitude  <= w$nelat,
            longitude >= w$swlng,
            longitude <= w$nelng
          ) |>
          collect()
      })

      inat_all <- bind_rows(archived_parts) %>%
        distinct(id, .keep_all = TRUE)

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
          taxon_name_input = input$taxon_name_input,
          selected_taxon_id = taxonomy_sel$id,
          selected_taxon_name = taxonomy_sel$name,
          selected_taxon_rank = taxonomy_sel$rank,
          selected_taxon_label = taxonomy_sel$label
        )
      } else {
        inat_all <- filter_by_threat_status(
          inat_all,
          threat_status = input$threat_status,
          require_iucn_for_exact = TRUE
        )
      }
if (!is.null(rv$uploaded_study_area)) {
  inat_all <- filter_points_to_study_area(
    df = inat_all,
    study_area = rv$uploaded_study_area
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
      taxon_id_val <- NULL
      taxon_val <- NULL
      threatened_flag <- FALSE
      
      if (input$filter_mode == "taxonomy") {
        iconic_val <- if (input$query_type == "iconic") input$iconic_taxon else NULL
        if (input$query_type != "iconic") {
          taxon_id_val <- taxonomy_sel$id
          taxon_val <- taxonomy_sel$name
        }
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
        message = glue("Live Query: starting adaptive query across {length(query_windows)} spatial tile(s)"),
        value   = 0
      )
      
      # query_res <- getDeadVertebrates_dateRange(
      #   start_date      = start_date,
      #   end_date        = end_date,
      #   swlat           = swlat,
      #   swlng           = swlng,
      #   nelat           = nelat,
      #   nelng           = nelng,
      #   iconic_taxa     = iconic_val,
      #   taxon_name      = taxon_val,
      #   threatened_flag = threatened_flag,
      #   exact_threat_status = if (input$filter_mode == "status") input$threat_status else "all",
      #   per_page        = 200,
      #   .shiny_progress = progress,
      #   time_bin        = input$time_bin,
      #   metric_type     = input$metric_type,
      #   show_smoother   = input$show_smoother
      # )
      
      
      query_parts <- lapply(seq_along(query_windows), function(tile_i) {
        w <- query_windows[[tile_i]]
        getDeadVertebrates_dateRange(
          start_date      = start_date,
          end_date        = end_date,
          swlat           = w$swlat,
          swlng           = w$swlng,
          nelat           = w$nelat,
          nelng           = w$nelng,
          iconic_taxa     = iconic_val,
          taxon_id        = taxon_id_val,
          taxon_name      = taxon_val,
          threatened_flag = threatened_flag,
          exact_threat_status = if (input$filter_mode == "status") input$threat_status else "all",
          per_page        = 200,
          .shiny_progress = progress,
          time_bin        = input$time_bin,
          metric_type     = input$metric_type,
          show_smoother   = input$show_smoother,
          tile_index      = tile_i,
          total_tiles     = length(query_windows)
        )
      })
      
      if (length(query_parts) == 1) {
        query_res <- query_parts[[1]]
      } else {
        merged_df_all <- bind_rows(lapply(query_parts, function(x) x$merged_df_all)) %>%
          distinct(id, .keep_all = TRUE)
        
        merged_df <- bind_rows(lapply(query_parts, function(x) x$merged_df)) %>%
          distinct(id, .keep_all = TRUE)
        
        combined_diagnostics <- list(
          weekly_windows = sum(vapply(query_parts, function(x) x$diagnostics$weekly_windows, numeric(1)), na.rm = TRUE),
          weeks_subdivided_to_days = sum(vapply(query_parts, function(x) x$diagnostics$weeks_subdivided_to_days, numeric(1)), na.rm = TRUE),
          days_subdivided_to_halfdays = sum(vapply(query_parts, function(x) x$diagnostics$days_subdivided_to_halfdays, numeric(1)), na.rm = TRUE),
          windows_hit_limit = sum(vapply(query_parts, function(x) x$diagnostics$windows_hit_limit, numeric(1)), na.rm = TRUE),
          total_rows = nrow(merged_df_all),
          adaptive_used = any(vapply(query_parts, function(x) isTRUE(x$diagnostics$adaptive_used), logical(1)))
        )
        
        query_res <- list(
          merged_df_all    = merged_df_all,
          merged_df        = merged_df,
          daily_plot       = make_daily_plot(
            merged_df_all,
            start_date = start_date,
            end_date = end_date,
            time_bin = input$time_bin,
            metric_type = input$metric_type,
            show_smoother = input$show_smoother
          ),
          top_species_plot = make_top_species_plot(merged_df_all),
          map_hotspots_gg  = make_hexbin_map(merged_df_all, start_date, end_date),
          daily_90th_quant = NA,
          diagnostics      = combined_diagnostics
        )
      }
      
      # if (!crosses_antimeridian) {
      #   query_res <- getDeadVertebrates_dateRange(
      #     start_date      = start_date,
      #     end_date        = end_date,
      #     swlat           = swlat,
      #     swlng           = swlng,
      #     nelat           = nelat,
      #     nelng           = nelng,
      #     iconic_taxa     = iconic_val,
      #     taxon_name      = taxon_val,
      #     threatened_flag = threatened_flag,
      #     exact_threat_status = if (input$filter_mode == "status") input$threat_status else "all",
      #     per_page        = 200,
      #     .shiny_progress = progress,
      #     time_bin        = input$time_bin,
      #     metric_type     = input$metric_type,
      #     show_smoother   = input$show_smoother
      #   )
      # } else {
      #   query_res_left <- getDeadVertebrates_dateRange(
      #     start_date      = start_date,
      #     end_date        = end_date,
      #     swlat           = swlat,
      #     swlng           = swlng,
      #     nelat           = nelat,
      #     nelng           = 180,
      #     iconic_taxa     = iconic_val,
      #     taxon_name      = taxon_val,
      #     threatened_flag = threatened_flag,
      #     exact_threat_status = if (input$filter_mode == "status") input$threat_status else "all",
      #     per_page        = 200,
      #     .shiny_progress = progress,
      #     time_bin        = input$time_bin,
      #     metric_type     = input$metric_type,
      #     show_smoother   = input$show_smoother
      #   )
      #   
      #   query_res_right <- getDeadVertebrates_dateRange(
      #     start_date      = start_date,
      #     end_date        = end_date,
      #     swlat           = swlat,
      #     swlng           = -180,
      #     nelat           = nelat,
      #     nelng           = nelng,
      #     iconic_taxa     = iconic_val,
      #     taxon_name      = taxon_val,
      #     threatened_flag = threatened_flag,
      #     exact_threat_status = if (input$filter_mode == "status") input$threat_status else "all",
      #     per_page        = 200,
      #     .shiny_progress = progress,
      #     time_bin        = input$time_bin,
      #     metric_type     = input$metric_type,
      #     show_smoother   = input$show_smoother
      #   )
      #   
      #   merged_df_all <- bind_rows(
      #     query_res_left$merged_df_all,
      #     query_res_right$merged_df_all
      #   ) %>%
      #     distinct(id, .keep_all = TRUE)
      #   
      #   merged_df <- bind_rows(
      #     query_res_left$merged_df,
      #     query_res_right$merged_df
      #   ) %>%
      #     distinct(id, .keep_all = TRUE)
      #   
      #   combined_diagnostics <- list(
      #     weekly_windows = query_res_left$diagnostics$weekly_windows + query_res_right$diagnostics$weekly_windows,
      #     weeks_subdivided_to_days = query_res_left$diagnostics$weeks_subdivided_to_days + query_res_right$diagnostics$weeks_subdivided_to_days,
      #     days_subdivided_to_halfdays = query_res_left$diagnostics$days_subdivided_to_halfdays + query_res_right$diagnostics$days_subdivided_to_halfdays,
      #     windows_hit_limit = query_res_left$diagnostics$windows_hit_limit + query_res_right$diagnostics$windows_hit_limit,
      #     total_rows = nrow(merged_df_all),
      #     adaptive_used = isTRUE(query_res_left$diagnostics$adaptive_used) || isTRUE(query_res_right$diagnostics$adaptive_used)
      #   )
      #   
      #   query_res <- list(
      #     merged_df_all    = merged_df_all,
      #     merged_df        = merged_df,
      #     daily_plot       = make_daily_plot(
      #       merged_df_all,
      #       start_date = start_date,
      #       end_date = end_date,
      #       time_bin = input$time_bin,
      #       metric_type = input$metric_type,
      #       show_smoother = input$show_smoother
      #     ),
      #     top_species_plot = make_top_species_plot(merged_df_all),
      #     map_hotspots_gg  = make_hexbin_map(merged_df_all, start_date, end_date),
      #     daily_90th_quant = NA,
      #     diagnostics      = combined_diagnostics
      #   )
      # }
      # 
if (!is.null(rv$uploaded_study_area)) {
  query_res$merged_df_all <- filter_points_to_study_area(
    df = query_res$merged_df_all,
    study_area = rv$uploaded_study_area
  )

  hm_live <- get_high_mortality_days(query_res$merged_df_all, time_bin = input$time_bin)

  query_res$merged_df <- if (!is.null(hm_live$days)) {
    query_res$merged_df_all %>%
      mutate(obs_bin = make_time_bin(as.Date(observed_on), time_bin = input$time_bin)) %>%
      filter(obs_bin %in% hm_live$days)
  } else {
    query_res$merged_df_all
  }

  query_res$daily_plot <- make_daily_plot(
    query_res$merged_df_all,
    start_date = start_date,
    end_date = end_date,
    time_bin = input$time_bin,
    metric_type = input$metric_type,
    show_smoother = input$show_smoother
  )

  query_res$top_species_plot <- make_top_species_plot(query_res$merged_df_all)
  query_res$map_hotspots_gg  <- make_hexbin_map(query_res$merged_df_all, start_date, end_date)
  query_res$diagnostics$total_rows <- nrow(query_res$merged_df_all)
}

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
  
  output$total_inat_text <- renderText({
    req(rv$total_obs_count)
    
    paste0(
      "Total iNaturalist observations in this query window: ",
      format(rv$total_obs_count, big.mark = ",")
    )
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