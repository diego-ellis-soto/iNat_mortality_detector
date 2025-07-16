##################################################################
# Single R Script: Shiny + iNaturalist "Dead" Wildlife + Comparison
##################################################################

########################
# 1) Install/Load Packages
########################
required_packages <- c(
  "httr", "jsonlite", "tidyverse", "glue", "lubridate",
  "wesanderson", "viridis", "shiny", "shinycssloaders",
  "DT", "maps", "mapdata", "leaflet", "leaflet.extras"
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
library(wesanderson)
library(viridis)
library(shiny)
library(shinycssloaders)
library(DT)
library(maps)       # for borders() if using ggplot world map
library(mapdata)
library(leaflet)
library(leaflet.extras)

########################################################
# 2) Helper Functions to Fetch Data from iNaturalist
########################################################

# A) fetch_inat_data_once(): Single date-range for "All" iNat obs
#    - No annotation constraints
#    - iconic_taxa optional if you want to filter by "Aves"/"Mammalia" only
fetch_inat_data_once <- function(
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    start_date,
    end_date,
    iconic_taxa = NULL,  # or "Aves"/"Mammalia" if desired
    per_page    = 200,
    max_pages   = 50
) {
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  # Build query for general iNat observations (verifiable = true)
  q_iconic <- if (!is.null(iconic_taxa)) paste0("iconic_taxa=", iconic_taxa, "&") else ""
  
  query_params <- glue(
    "{q_iconic}",
    "verifiable=true&",
    "d1={start_date}&d2={end_date}&",
    "order=desc&order_by=created_at&",
    "per_page={per_page}"
  )
  
  # Build location part
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
    
    message(" [All] Page ", current_page,
            " [", start_date, " to ", end_date, "]:\n", query_url)
    resp <- GET(query_url)
    if (http_error(resp)) {
      warning("HTTP error on page ", current_page, ": ", status_code(resp))
      break
    }
    parsed <- content(resp, as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE)
    
    if (length(parsed$results) == 0) {
      message("No more ALL-observations results at page ", current_page)
      break
    }
    
    obs_page_df <- as_tibble(parsed$results)
    observations_list[[current_page]] <- obs_page_df
    
    if (nrow(obs_page_df) < per_page) {
      message("Reached last page for ALL-observations at page ", current_page)
      break
    }
    
    current_page <- current_page + 1
    Sys.sleep(1)  # polite pause
  }
  
  return(bind_rows(observations_list))
}


# B) fetch_dead_data_once(): Single date-range for "Dead" obs
fetch_dead_data_once <- function(
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    start_date,
    end_date,
    iconic_taxa = "Aves",
    per_page    = 200,
    max_pages   = 50
) {
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  # Build query for "Dead" observations
  # "q=Dead" is optional, but main filter is term_id=17&term_value_id=19
  q_iconic <- if (!is.null(iconic_taxa)) paste0("iconic_taxa=", iconic_taxa, "&") else ""
  
  query_params <- glue(
    "{q_iconic}",
    "term_id=17&",
    "term_value_id=19&",
    "verifiable=true&",
    "d1={start_date}&d2={end_date}&",
    "order=desc&order_by=created_at&",
    "per_page={per_page}"
  )
  
  # Location part
  loc_part <- ""
  if (!is.null(place_id)) {
    loc_part <- glue("&place_id={place_id}")
  } else if (!is.null(swlat) && !is.null(swlng) &&
             !is.null(nelat) && !is.null(nelng)) {
    loc_part <- glue("&nelat={nelat}&nelng={nelng}&swlat={swlat}&swlng={swlng}")
  } else {
    stop("Must provide either 'place_id' OR bounding box.")
  }
  
  observations_list <- list()
  current_page <- 1
  
  while (current_page <= max_pages) {
    query_url <- paste0(base_url, "?", query_params,
                        "&page=", current_page, loc_part)
    
    message(" [Dead] Page ", current_page,
            " [", start_date, " to ", end_date, "]:\n", query_url)
    resp <- GET(query_url)
    if (http_error(resp)) {
      warning("HTTP error on page ", current_page, ": ", status_code(resp))
      break
    }
    parsed <- content(resp, as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE)
    
    if (length(parsed$results) == 0) {
      message("No more DEAD-observations results at page ", current_page)
      break
    }
    
    obs_page_df <- as_tibble(parsed$results)
    observations_list[[current_page]] <- obs_page_df
    
    if (nrow(obs_page_df) < per_page) {
      message("Reached last page for DEAD-observations at page ", current_page)
      break
    }
    
    current_page <- current_page + 1
    Sys.sleep(1)
  }
  
  return(bind_rows(observations_list))
}


# C) fetch_inat_data_monthly() - For "All" iNat
fetch_inat_data_monthly <- function(
    year,
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    iconic_taxa = NULL
) {
  monthly_list <- list()
  for (m in 1:12) {
    start_date <- as.Date(glue("{year}-{sprintf('%02d', m)}-01"))
    end_date   <- start_date %m+% months(1) %m-% days(1)
    if (year(start_date) != year) break
    
    df_m <- fetch_inat_data_once(
      place_id   = place_id,
      swlat      = swlat,
      swlng      = swlng,
      nelat      = nelat,
      nelng      = nelng,
      start_date = start_date,
      end_date   = end_date,
      iconic_taxa= iconic_taxa
    )
    monthly_list[[m]] <- df_m
  }
  bind_rows(monthly_list)
}


# D) fetch_dead_data_monthly() - For "Dead" iNat
fetch_dead_data_monthly <- function(
    year,
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    iconic_taxa = "Aves"
) {
  monthly_list <- list()
  for (m in 1:12) {
    start_date <- as.Date(glue("{year}-{sprintf('%02d', m)}-01"))
    end_date   <- start_date %m+% months(1) %m-% days(1)
    if (year(start_date) != year) break
    
    df_m <- fetch_dead_data_once(
      place_id   = place_id,
      swlat      = swlat,
      swlng      = swlng,
      nelat      = nelat,
      nelng      = nelng,
      start_date = start_date,
      end_date   = end_date,
      iconic_taxa= iconic_taxa
    )
    monthly_list[[m]] <- df_m
  }
  bind_rows(monthly_list)
}


##################################################################
# 3) getDeadVertebrates_monthlyLoop(): Returns Dead data + plots
#    (Revised to handle empty / invalid location gracefully)
##################################################################
getDeadVertebrates_monthlyLoop <- function(
    years       = c(2021, 2022),
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    iconic_taxa = "Aves",
    outdir      = NULL
) {
  # Gather monthly data across selected years
  all_years_list <- list()
  for (yr in years) {
    message("\n=== Dead Obs: YEAR ", yr, " ===\n")
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
  merged_df_all <- bind_rows(all_years_list)
  
  # If no data at all
  if (!"created_at_details.date" %in% names(merged_df_all) ||
      nrow(merged_df_all) == 0) {
    daily_plot <- ggplot() +
      labs(title = "No 'Dead' Observations Found") +
      theme_void()
    
    empty_plot <- ggplot() + theme_void()
    
    return(list(
      merged_df        = merged_df_all,
      daily_plot       = daily_plot,
      top_species_plot = empty_plot,
      map_hotspots_gg  = empty_plot
    ))
  }
  
  # Optionally save raw data
  if (!is.null(outdir)) {
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    readr::write_csv(merged_df_all, file.path(outdir, "merged_df_top_all_data.csv"))
  }
  
  # 1) Daily time series
  counts_by_day <- merged_df_all %>%
    mutate(obs_date = as.Date(`created_at_details.date`)) %>%
    group_by(Window, obs_date) %>%
    summarise(n = n(), .groups = "drop")
  
  # iNaturalist color palette
  # (e.g., #74AC00 is iNat green, #1E5713 is dark green)
  iNat_colors <- c("#74AC00", "#1E5713", "#91D27F", "#5A7B36")
  # We'll pick as many as needed:
  color_count <- length(unique(counts_by_day$Window))
  chosen_cols <- if (color_count <= length(iNat_colors)) {
    iNat_colors[1:color_count]
  } else {
    rep(iNat_colors, length.out = color_count)
  }
  
  daily_plot <- ggplot(counts_by_day, aes(x = obs_date, y = n, color = Window)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = chosen_cols) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(
      title = paste("Daily 'Dead' Observations (", paste(years, collapse=", "), ")"),
      x     = "Date",
      y     = "Number of Observations",
      color = "Year"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 2) Top-20 species
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
      scale_fill_manual(values = chosen_cols) +
      labs(
        title = "Top 20 Species with 'Dead' Observations",
        x     = "Species",
        y     = "Number of Dead Observations",
        fill  = "Year"
      ) +
      theme_minimal(base_size = 14)
  } else {
    top_species_plot <- ggplot() + labs(title="No 'taxon.name' column found") + theme_void()
  }
  
  # 3) Identify top 90th percentile daily mortality => "hotspots"
  daily_quantile <- quantile(counts_by_day$n, probs = 0.90, na.rm = TRUE)
  high_mortality_days <- counts_by_day %>%
    filter(n >= daily_quantile) %>%
    pull(obs_date)
  
  merged_high <- merged_df_all %>%
    mutate(obs_date = as.Date(`created_at_details.date`)) %>%
    filter(obs_date %in% high_mortality_days)
  
  if (nrow(merged_high) == 0) {
    map_hotspots_gg <- ggplot() +
      labs(title = "No data for top 90% mortality days") +
      theme_void()
    
    if (!is.null(outdir)) {
      readr::write_csv(merged_high, file.path(outdir, "merged_df_top90.csv"))
    }
    
    return(list(
      merged_df        = merged_high,
      daily_plot       = daily_plot,
      top_species_plot = top_species_plot,
      map_hotspots_gg  = map_hotspots_gg
    ))
  }
  
  # Convert location to character to avoid list issues
  if ("location" %in% names(merged_high)) {
    merged_high <- merged_high %>%
      mutate(location = as.character(location))
  }
  
  # Parse lat/lon if location is present
  if ("location" %in% names(merged_high)) {
    location_df <- merged_high %>%
      filter(!is.na(location) & location != "") %>%
      separate(location, into = c("lat_str", "lon_str"), sep = ",", remove = FALSE, extra = "drop") %>%
      mutate(
        latitude  = suppressWarnings(as.numeric(lat_str)),
        longitude = suppressWarnings(as.numeric(lon_str))
      ) %>%
      filter(!is.na(latitude) & !is.na(longitude))
    
    if (nrow(location_df) == 0) {
      map_hotspots_gg <- ggplot() +
        labs(title = "No valid lat/lon data for top 90% days") +
        theme_void()
    } else {
      min_lon <- min(location_df$longitude, na.rm = TRUE)
      max_lon <- max(location_df$longitude, na.rm = TRUE)
      min_lat <- min(location_df$latitude,  na.rm = TRUE)
      max_lat <- max(location_df$latitude,  na.rm = TRUE)
      
      map_hotspots_gg <- ggplot(location_df, aes(x = longitude, y = latitude, color = Window)) +
        borders("world", fill = "gray80", colour = "white") +
        geom_point(alpha = 0.6, size = 2) +
        scale_color_manual(values = chosen_cols) +
        coord_quickmap(xlim = c(min_lon, max_lon),
                       ylim = c(min_lat, max_lat),
                       expand = TRUE) +
        labs(
          title = paste("Top 90th percentile mortality days (", paste(years, collapse=", "), ")"),
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
  
  # 4) Save top-90% subset if outdir
  if (!is.null(outdir)) {
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    readr::write_csv(merged_high, file.path(outdir, "merged_df_top90.csv"))
    
    # Also save plots if desired
    ggsave(file.path(outdir, "daily_plot.png"),       daily_plot,       width=8, height=5, dpi=300)
    ggsave(file.path(outdir, "top_species_plot.png"), top_species_plot, width=7, height=7, dpi=300)
    ggsave(file.path(outdir, "map_hotspots.png"),     map_hotspots_gg,  width=8, height=5, dpi=300)
  }
  
  list(
    merged_df        = merged_high,
    daily_plot       = daily_plot,
    top_species_plot = top_species_plot,
    map_hotspots_gg  = map_hotspots_gg,
    daily_90th_quant = daily_quantile
  )
}


##################################################################
# 4) getAllInatRecords_monthlyLoop():
#    - Similar to above but fetches "All" iNat data (no 'Dead' annotation)
#    - We'll use it for the comparison plot: "All Observations" vs "Dead"
##################################################################
getAllInatRecords_monthlyLoop <- function(
    years       = c(2021, 2022),
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    iconic_taxa = NULL  # If you'd like to filter by Aves/Mammalia
) {
  all_years_list <- list()
  for (yr in years) {
    message("\n=== All Obs: YEAR ", yr, " ===\n")
    yr_df <- fetch_inat_data_monthly(
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
  bind_rows(all_years_list)
}

##################################################################
# 5) Shiny App (UI + Server)
##################################################################

ui <- fluidPage(
  titlePanel("iNaturalist 'Dead' Wildlife vs. All Observations"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("query_mode", "Choose Query Mode:",
                   choices = c("Use place_id" = "place",
                               "Draw bounding box on map" = "draw")),
      
      # place_id input
      conditionalPanel(
        condition = "input.query_mode == 'place'",
        numericInput("place_id", "place_id (e.g. 1 for USA, 6712 for Canada)",
                     value = 1, min = 1, max = 999999)
      ),
      
      # Leaflet bounding box
      conditionalPanel(
        condition = "input.query_mode == 'draw'",
        helpText("Draw a rectangle on the map to define your bounding box."),
        leafletOutput("map_draw", height = "300px"),
        verbatimTextOutput("bbox_coords")
      ),
      
      # Years
      checkboxGroupInput("years", "Select Year(s):",
                         choices = 2021:2025,
                         selected = c(2021,2022)),
      
      # Iconic Taxon
      # If you want to limit "All" queries to a certain group
      # or just set it to "NULL" to get truly all
      selectInput("iconic_taxon", "Taxon Filter:",
                  choices = c("No filter" = "NULL",
                              "Aves"       = "Aves",
                              "Mammalia"   = "Mammalia"),
                  selected = "Aves"),
      
      actionButton("run_query", "Run Query"),
      
      hr(),
      downloadButton("downloadData", "Download Top-90% 'Dead' CSV"),
      br(), br(),
      helpText("Example iNaturalist record ID: 126811566 → ",
               a("View on iNaturalist",
                 href = "https://www.inaturalist.org/observations/126811566",
                 target = "_blank"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Daily 'Dead' Time Series",
                 withSpinner(plotOutput("dailyPlot"), type = 6)),
        tabPanel("Top 'Dead' Species",
                 withSpinner(plotOutput("speciesPlot"), type = 6)),
        tabPanel("Hotspots Map (Dead, 90th%)",
                 withSpinner(plotOutput("hotspotMap"), type = 6)),
        tabPanel("Data Table (Dead, Top-90%)",
                 withSpinner(DT::dataTableOutput("dataTable"), type = 6)),
        tabPanel("Daily Compare: All vs. Dead",
                 withSpinner(plotOutput("comparePlot"), type = 6))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # For bounding-box drawing
  rv <- reactiveValues(bbox = NULL)
  
  output$map_draw <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -100, lat = 40, zoom = 4) %>%
      addDrawToolbar(
        targetGroup = "drawn_bbox",
        polylineOptions = FALSE, polygonOptions = FALSE,
        circleOptions   = FALSE, markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = drawRectangleOptions(
          shapeOptions = drawShapeOptions(clickable = FALSE)
        ),
        editOptions = editToolbarOptions(edit = FALSE),
        singleFeature = TRUE
      )
  })
  
  # Observe Leaflet draw
  observeEvent(input$map_draw_draw_new_feature, {
    feature <- input$map_draw_draw_new_feature
    if (is.null(feature)) return()
    
    coords <- feature$geometry$coordinates[[1]]
    lngs <- sapply(coords, `[`, 1)
    lats <- sapply(coords, `[`, 2)
    
    lat_min <- min(lats)
    lat_max <- max(lats)
    lng_min <- min(lngs)
    lng_max <- max(lngs)
    
    rv$bbox <- c(lat_min, lng_min, lat_max, lng_max)
  })
  
  output$bbox_coords <- renderText({
    if (is.null(rv$bbox)) {
      "No bounding box drawn yet."
    } else {
      paste0(
        "Bounding box:\n",
        "SW corner: (", rv$bbox[1], ", ", rv$bbox[2], ")\n",
        "NE corner: (", rv$bbox[3], ", ", rv$bbox[4], ")"
      )
    }
  })
  
  # Reactive values for results
  dead_data <- reactiveVal(NULL)  # 'Dead' data + plots
  all_data  <- reactiveVal(NULL)  # 'All' data, to compare
  
  # Observe "Run Query"
  observeEvent(input$run_query, {
    validate(need(input$years, "Please select at least one year."))
    yrs <- as.numeric(input$years)
    
    # Decide bounding box or place
    if (input$query_mode == "place") {
      place_val <- input$place_id
      swlat_val <- swlng_val <- nelat_val <- nelng_val <- NULL
    } else {
      validate(need(!is.null(rv$bbox), "Please draw a bounding box first."))
      place_val <- NULL
      swlat_val <- rv$bbox[1]
      swlng_val <- rv$bbox[2]
      nelat_val <- rv$bbox[3]
      nelng_val <- rv$bbox[4]
    }
    
    # Iconic taxon logic
    # If user selected "No filter", we pass NULL
    taxon_choice <- if (input$iconic_taxon == "NULL") NULL else input$iconic_taxon
    
    withProgress(message = "Fetching data...", value = 0, {
      incProgress(0.1)
      
      # 1) Dead data
      dd <- getDeadVertebrates_monthlyLoop(
        years       = yrs,
        place_id    = place_val,
        swlat       = swlat_val,
        swlng       = swlng_val,
        nelat       = nelat_val,
        nelng       = nelng_val,
        iconic_taxa = taxon_choice
      )
      dead_data(dd)
      
      incProgress(0.5)
      
      # 2) All data
      aa <- getAllInatRecords_monthlyLoop(
        years       = yrs,
        place_id    = place_val,
        swlat       = swlat_val,
        swlng       = swlng_val,
        nelat       = nelat_val,
        nelng       = nelng_val,
        iconic_taxa = taxon_choice
      )
      all_data(aa)
      
      incProgress(0.9)
    })
  })
  
  # ---- Outputs ----
  
  # (A) "Dead" daily plot
  output$dailyPlot <- renderPlot({
    req(dead_data())
    dead_data()$daily_plot
  })
  
  # (B) "Dead" top species
  output$speciesPlot <- renderPlot({
    req(dead_data())
    dead_data()$top_species_plot
  })
  
  # (C) "Dead" hotspot map
  output$hotspotMap <- renderPlot({
    req(dead_data())
    dead_data()$map_hotspots_gg
  })
  
  # (D) "Dead" top-90% data table
  output$dataTable <- DT::renderDataTable({
    req(dead_data())
    
    df <- dead_data()$merged_df
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message="No 'Dead' records found!")))
    }
    
    # Add clickable link & thumbnail
    df <- df %>%
      mutate(
        inat_link = paste0(
          "<a href='https://www.inaturalist.org/observations/", id,
          "' target='_blank'>", id, "</a>"
        )
      )
    
    # If we have a photo column
    photo_col <- "taxon.default_photo.square_url"
    if (photo_col %in% names(df)) {
      df$image_thumb <- ifelse(
        !is.na(df[[photo_col]]) & df[[photo_col]] != "",
        paste0("<img src='", df[[photo_col]], "' width='50'/>"),
        "No Img"
      )
    } else {
      df$image_thumb <- "No Img"
    }
    
    # Reorder columns
    show_cols <- c("inat_link", "image_thumb", "taxon.name", "created_at_details.date",
                   setdiff(names(df), c("inat_link","image_thumb","taxon.name","created_at_details.date")))
    
    DT::datatable(
      df[, show_cols, drop=FALSE],
      escape=FALSE,
      options = list(pageLength=10, autoWidth=TRUE)
    )
  })
  
  # (E) Compare "All" vs "Dead"
  output$comparePlot <- renderPlot({
    req(dead_data(), all_data())
    
    # Build daily counts for "Dead"
    dd <- dead_data()$merged_df
    if ("created_at_details.date" %in% names(dd) && nrow(dd) > 0) {
      d_daily <- dd %>%
        mutate(date = as.Date(`created_at_details.date`)) %>%
        group_by(date) %>%
        summarise(dead_count = n(), .groups="drop")
    } else {
      d_daily <- tibble(date=as.Date(character()), dead_count=integer())
    }
    
    # Build daily counts for "All"
    aa <- all_data()
    # "created_at_details.date" might be nested in columns
    if ("created_at_details.date" %in% names(aa) && nrow(aa) > 0) {
      a_daily <- aa %>%
        mutate(date = as.Date(`created_at_details.date`)) %>%
        group_by(date) %>%
        summarise(all_count = n(), .groups="drop")
    } else {
      a_daily <- tibble(date=as.Date(character()), all_count=integer())
    }
    
    # Merge the two
    combined <- full_join(d_daily, a_daily, by="date") %>%
      mutate(
        dead_count = replace_na(dead_count, 0),
        all_count  = replace_na(all_count, 0)
      )
    
    if (nrow(combined) == 0) {
      return(ggplot() + labs(title="No data to compare") + theme_void())
    }
    
    # Pivot longer for easy plotting
    plot_data <- combined %>%
      pivot_longer(cols=c("dead_count","all_count"),
                   names_to="Type", values_to="Count")
    
    # iNaturalist-inspired colors: "All" = #1E5713 (dark green), "Dead" = #74AC00 (lighter green)
    compare_colors <- c("all_count" = "#1E5713", "dead_count" = "#74AC00")
    compare_labels <- c("all_count" = "All Observations", "dead_count" = "Dead Observations")
    
    ggplot(plot_data, aes(x=date, y=Count, color=Type)) +
      geom_line(size=1.2) +
      geom_point(size=2) +
      scale_color_manual(values=compare_colors, labels=compare_labels) +
      labs(
        title="Daily Comparison: All vs. Dead Observations",
        x = "Date",
        y = "Count of Observations",
        color = NULL
      ) +
      theme_minimal(base_size=14) +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  })
  
  # Download top-90% CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("inat_dead_top90_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(dead_data())
      readr::write_csv(dead_data()$merged_df, file)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
