# Next steps: Add daily iNat records in general in that bounding box ! 
# 

##################################################################
# Single R Script: Mortality Analysis + Shiny + Leaflet-draw
##################################################################

### 1) Install/Load Required Packages ####
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
library(maps)       # for borders() if used in ggplot
library(mapdata)    # extra map data
library(leaflet)
library(leaflet.extras)

##################################################################
# 2) Mortality-Analysis Functions
##################################################################

#########################################################
# A) fetch_dead_data_once(): Single date-range query
#    Uses iNaturalist API parameters:
#      term_id=17, term_value_id=19 => "Dead" annotation
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
  # "q=Dead" is optional. We'll rely primarily on annotation (term_id=17, term_value_id=19).
  query_params <- glue(
    "iconic_taxa={iconic_taxa}&",
    # "q=Dead&",  # optional search keyword
    "term_id=17&",       # "Alive or Dead" annotation
    "term_value_id=19&", # "Dead" value
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
            " [", start_date, " to ", end_date, "]:\n", query_url)
    
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
    
    # If fewer rows than per_page, we've reached last page
    if (nrow(obs_page_df) < per_page) {
      message("Reached last page of results at page ", current_page)
      break
    }
    
    current_page <- current_page + 1
    Sys.sleep(1)  # polite pause for the API
  }
  
  observations_all <- bind_rows(observations_list)
  return(observations_all)
}

#########################################################
# B) fetch_dead_data_monthly():
#    For one YEAR, split into monthly intervals.
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
    
    message("\n--- Querying ", year, ", month ", month_i, " ---")
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
#    - loops over selected years
#    - merges data
#    - creates:
#      * daily_plot
#      * top_species_plot
#      * map_hotspots_gg (90th percentile days)
#      * returns merged_df (top 90% days)
#########################################################
getDeadVertebrates_monthlyLoop <- function(
    years       = c(2022, 2023),
    # Either place_id OR bounding box
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    iconic_taxa = "Aves",
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
  
  merged_df_all <- bind_rows(all_years_list)
  
  # If no data, return empty results
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
      merged_df        = merged_df_all,
      daily_plot       = daily_plot,
      top_species_plot = top_species_plot,
      map_hotspots_gg  = map_hotspots_gg
    ))
  }
  
  # Optionally save raw data
  if (!is.null(outdir)) {
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    readr::write_csv(merged_df_all, file.path(outdir, "merged_df_top_all_data.csv"))
  }
  
  # 2) Daily time series
  counts_by_day <- merged_df_all %>%
    mutate(obs_date = as.Date(`created_at_details.date`)) %>%
    group_by(Window, obs_date) %>%
    summarise(n = n(), .groups = "drop")
  
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
  
  # 5) If outdir specified, save files
  if (!is.null(outdir)) {
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    
    # Save the top-90% CSV
    readr::write_csv(merged_high, file.path(outdir, "merged_df_top90.csv"))
    
    # Save daily_plot
    ggsave(file.path(outdir, "daily_plot.png"),
           daily_plot, width = 8, height = 5, dpi = 300)
    # Save top_species_plot
    ggsave(file.path(outdir, "top_species_plot.png"),
           top_species_plot, width = 7, height = 7, dpi = 300)
    # Save map_hotspots_gg
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

##################################################################
# 3) Shiny App: UI + Server
##################################################################

ui <- fluidPage(
  titlePanel("Dead Wildlife Observations from iNaturalist"),
  sidebarLayout(
    sidebarPanel(
      # Query mode
      radioButtons("query_mode", "Choose Query Mode:",
                   choices = c("Use place_id" = "place",
                               "Draw bounding box on map" = "draw")),
      
      # place_id input
      conditionalPanel(
        condition = "input.query_mode == 'place'",
        numericInput("place_id", "place_id (e.g. 1 for USA, 6712 for Canada)",
                     value = 1, min = 1, max = 999999, step = 1)
      ),
      
      # Leaflet map for bounding box (only shows if user chooses 'draw')
      conditionalPanel(
        condition = "input.query_mode == 'draw'",
        helpText("Draw a rectangle on the map to define your bounding box."),
        leafletOutput("map_draw", height = "300px"),
        verbatimTextOutput("bbox_coords")
      ),
      
      # Years (multiple selection)
      checkboxGroupInput("years", "Select Year(s):",
                         choices = 2021:2025,
                         selected = c(2021, 2022, 2023)),
      
      # Iconic Taxon
      selectInput("iconic_taxon", "Select Taxonomic Group:",
                  choices = c("Aves", "Mammalia"),
                  selected = "Aves"),
      
      # Action Button to run the query
      actionButton("run_query", "Run Query"),
      
      hr(),
      
      # Download button for top-90% CSV
      downloadButton("downloadData", "Download Top-90% CSV"),
      
      br(), br(),
      helpText("Example iNaturalist record ID: 126811566 → ",
               a("View on iNaturalist",
                 href = "https://www.inaturalist.org/observations/126811566",
                 target = "_blank"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Daily Time Series",
                 withSpinner(plotOutput("dailyPlot"), type = 6)),
        tabPanel("Top Species",
                 withSpinner(plotOutput("speciesPlot"), type = 6)),
        tabPanel("Hotspots Map (90th%)",
                 withSpinner(plotOutput("hotspotMap"), type = 6)),
        tabPanel("Data Table (Top-90%)",
                 withSpinner(DT::dataTableOutput("dataTable"), type = 6))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values for bounding box
  rv <- reactiveValues(
    bbox = NULL  # will be c(lat_min, lng_min, lat_max, lng_max)
  )
  
  # Initialize the Leaflet map for bounding box drawing
  output$map_draw <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -100, lat = 40, zoom = 4) %>%  # a broad US-centric view
      addDrawToolbar(
        targetGroup  = "drawn_bbox",
        polylineOptions = FALSE,
        polygonOptions  = FALSE,
        circleOptions   = FALSE,
        markerOptions   = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = drawRectangleOptions(
          shapeOptions = drawShapeOptions(clickable = FALSE)
        ),
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = FALSE),
        singleFeature = TRUE  # only one shape at a time
      )
  })
  
  # Observe new shapes (rectangle) drawn by the user
  observeEvent(input$map_draw_draw_new_feature, {
    # The drawn feature is in GeoJSON format:
    feature <- input$map_draw_draw_new_feature
    if (is.null(feature)) return()
    
    # For a rectangle, coordinates come as a list of four corners:
    # feature$geometry$coordinates[[1]] is a list of [lng, lat] pairs
    coords <- feature$geometry$coordinates[[1]]
    lngs <- sapply(coords, "[", 1)
    lats <- sapply(coords, "[", 2)
    
    lat_min <- min(lats)
    lat_max <- max(lats)
    lng_min <- min(lngs)
    lng_max <- max(lngs)
    
    rv$bbox <- c(lat_min, lng_min, lat_max, lng_max)
  })
  
  # Show the user the bounding box they drew
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
  
  # Reactive value to store the analysis result
  result_data <- reactiveVal(NULL)
  
  # Observe "Run Query" button
  observeEvent(input$run_query, {
    req(input$years)
    validate(need(length(input$years) > 0, "Please select at least one year"))
    
    yrs <- as.numeric(input$years)
    
    # Show progress
    withProgress(message = 'Fetching data from iNaturalist...', value = 0, {
      incProgress(0.1)
      
      if (input$query_mode == "place") {
        # place_id mode
        place_id_val <- input$place_id
        swlat_val <- NULL
        swlng_val <- NULL
        nelat_val <- NULL
        nelng_val <- NULL
        
      } else {
        # "draw bounding box" mode
        validate(need(!is.null(rv$bbox), "Please draw a bounding box on the map first."))
        
        # rv$bbox = c(lat_min, lng_min, lat_max, lng_max)
        place_id_val <- NULL
        swlat_val <- rv$bbox[1]
        swlng_val <- rv$bbox[2]
        nelat_val <- rv$bbox[3]
        nelng_val <- rv$bbox[4]
      }
      
      # Run the main function
      query_res <- getDeadVertebrates_monthlyLoop(
        years       = yrs,
        place_id    = place_id_val,
        swlat       = swlat_val,
        swlng       = swlng_val,
        nelat       = nelat_val,
        nelng       = nelng_val,
        iconic_taxa = input$iconic_taxon,
        outdir      = NULL
      )
      
      # Store in reactiveVal
      result_data(query_res)
      
      incProgress(0.9)
    })
  })
  
  # (a) Daily Plot
  output$dailyPlot <- renderPlot({
    req(result_data())
    result_data()$daily_plot
  })
  
  # (b) Top Species Plot
  output$speciesPlot <- renderPlot({
    req(result_data())
    result_data()$top_species_plot
  })
  
  # (c) Hotspots Map
  output$hotspotMap <- renderPlot({
    req(result_data())
    result_data()$map_hotspots_gg
  })
  
  # (d) Data Table (top-90% days) with clickable links & thumbnails
  output$dataTable <- DT::renderDataTable({
    req(result_data())
    
    df <- result_data()$merged_df
    
    # If there's no data, just display an empty table
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No records found"), options = list(pageLength = 5)))
    }
    
    # Add a clickable link to iNaturalist
    df <- df %>%
      mutate(
        inat_link = paste0(
          "<a href='https://www.inaturalist.org/observations/",
          id, "' target='_blank'>", id, "</a>"
        )
      )
    
    # Add a small thumbnail if available
    # Checking if "taxon.default_photo.square_url" column exists:
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
    
    # Re-order columns for clarity
    # Put link and image up front
    show_cols <- c("inat_link", "image_thumb", "taxon.name", "created_at_details.date",
                   setdiff(names(df), c("inat_link", "image_thumb", "taxon.name", "created_at_details.date")))
    
    DT::datatable(
      df[ , show_cols, drop = FALSE],
      escape = FALSE,   # allow HTML in cells
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      )
    )
  })
  
  # Download handler for top-90% CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("inat_dead_top90_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(result_data())
      readr::write_csv(result_data()$merged_df, file)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
