# --------------------------------------------------
# Additional Packages
# --------------------------------------------------
# install.packages("tidycensus")
# install.packages("sf")
# library(tidycensus)
# library(sf)

# --------------------------------------------------
# 1) Define Counties of Interest
# --------------------------------------------------
counties_list <- c(
  "Los Angeles",
  "San Diego",
  # SF Bay Area:
  "San Francisco",
  "Marin",
  "Sonoma",
  "Napa",
  "Solano",
  "Contra Costa",
  "Alameda",
  "Santa Clara",
  "San Mateo"
)

# --------------------------------------------------
# 2) Helper to Get BBox for a Single County
# --------------------------------------------------
get_county_bbox <- function(county_name, state_abbr = "CA") {
  # We can retrieve geometry for the county using tidycensus::get_acs,
  # or get_estimates, or get_decennial. The variable doesn't matter much
  # because we just need geometry.
  geo <- get_acs(
    geography = "county",
    variables = "B19013_001",  # any single variable
    state     = state_abbr,
    county    = county_name,
    geometry  = TRUE
  )
  
  # Now, 'geo' is an sf object with geometry for that county
  # We can get the bounding box
  bb <- st_bbox(geo)
  
  # st_bbox returns c(xmin, ymin, xmax, ymax) => (lon_min, lat_min, lon_max, lat_max)
  # iNat expects c(lat_min, lng_min, lat_max, lng_max)
  # So reorder:
  bounds <- c(bb$ymin, bb$xmin, bb$ymax, bb$xmax)
  
  return(bounds)
}

# --------------------------------------------------
# 3) Loop Over Counties, Collect Monthly Data
# --------------------------------------------------
all_county_data <- list()

for (cnty in counties_list) {
  # Get bounding box
  cnty_bbox <- get_county_bbox(cnty, "CA")
  
  # Retrieve data month-by-month
  inat_cnty <- get_inat_monthly(
    bounds     = cnty_bbox,
    start_date = as.Date("2008-01-01"),
    end_date   = Sys.Date()
  )
  
  # Tag with county name (optional)
  if (!is.null(inat_cnty) && nrow(inat_cnty) > 0) {
    inat_cnty$county_name <- cnty
    all_county_data[[length(all_county_data) + 1]] <- inat_cnty
  }
}

# Combine everything
inat_all_ca_counties <- bind_rows(all_county_data)

# Quick look
dim(inat_all_ca_counties)
head(inat_all_ca_counties)

# Optionally save to disk
# saveRDS(inat_all_ca_counties, "inat_all_ca_counties.rds")
