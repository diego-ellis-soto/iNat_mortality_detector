##################################################################
# Single R Script: Mortality Analysis + Shiny - Two-Click BBox
##################################################################

### 1) Install/Load Required Packages ####
required_packages <- c(
  "httr", "jsonlite", "tidyverse", "glue", "lubridate",
  "wesanderson", "viridis", "shiny", "shinycssloaders",
  "DT", "maps", "mapdata", "leaflet", "leaflet.extras",
  "shinythemes"
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
library(maps)
library(mapdata)
library(leaflet)
library(leaflet.extras)
library(shinythemes)


##################################################################
# 2) Mortality-Analysis Functions
##################################################################

fetch_dead_data_once <- function(
    place_id    = NULL,
    swlat       = NULL,
    swlng       = NULL,
    nelat       = NULL,
    nelng       = NULL,
    start_date,
    end_date,
    iconic_taxa = "Aves",
    per_page    = 1000,
    max_pages   = 50
) {
  base_url <- "https://api.inaturalist.org/v1/observations"
  
  query_params <- glue(
    "iconic_taxa={iconic_taxa}&",
    "term_id=17&",
    "term_value_id=19&",
    "verifiable=true&",
    "d1={start_date}&d2={end_date}&",
    "order=desc&order_by=created_at&",
    "per_page={per_page}"
  )
  
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


getDeadVertebrates_monthlyLoop <- function(
    years       = c(2022, 2023),
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
  
  if (!is.null(outdir)) {
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    readr::write_csv(merged_df_all, file.path(outdir, "merged_df_top_all_data.csv"))
  }
  
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
  
  daily_quantile <- quantile(counts_by_day$n, probs = 0.90, na.rm = TRUE)
  high_mortality_days <- counts_by_day %>%
    filter(n >= daily_quantile) %>%
    pull(obs_date)
  
  merged_high <- merged_df_all %>%
    mutate(obs_date = as.Date(`created_at_details.date`)) %>%
    filter(obs_date %in% high_mortality_days)
  
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
  
  if (!is.null(outdir)) {
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
    
    readr::write_csv(merged_high, file.path(outdir, "merged_df_top90.csv"))
    ggsave(file.path(outdir, "daily_plot.png"),
           daily_plot, width = 8, height = 5, dpi = 300)
    ggsave(file.path(outdir, "top_species_plot.png"),
           top_species_plot, width = 7, height = 7, dpi = 300)
    ggsave(file.path(outdir, "map_hotspots.png"),
           map_hotspots_gg, width = 8, height = 5, dpi = 300)
  }
  
  return(list(
    merged_df        = merged_high,
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
  theme = shinytheme("flatly"),
  
  # -- Logo and Title at the top --
  fluidRow(
    column(
      width = 2,
      # .all_logos.png in www folder
      tags$img(src = "www.all_logos.png", height = "400px")
    ),
    column(
      width = 10,
      titlePanel("Dead Wildlife Observations from iNaturalist")
    )
  ),
  hr(),
  
  # Layout with a sidebar containing TABS: Query, About, Participatory, How to Use
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "sidebar_tabs",
        
        # == Query Panel ==
        tabPanel(
          title = "Query",
          br(),
          radioButtons("query_mode", "Choose Query Mode:",
                       choices = c("Use place_id" = "place",
                                   "Two clicks to define bounding box" = "twoclick")),
          
          # If place ID chosen, show numeric input
          conditionalPanel(
            condition = "input.query_mode == 'place'",
            numericInput("place_id", "place_id (e.g. 1 for USA, 6712 for Canada)",
                         value = 1, min = 1, max = 999999, step = 1)
          ),
          
          # If two-click BBox chosen, show Leaflet & bounding box info
          conditionalPanel(
            condition = "input.query_mode == 'twoclick'",
            helpText("Left-click once for the first corner, and once more for the opposite corner. 
                      The SW and NE corners will be automatically computed."),
            leafletOutput("map_two_click", height = "300px"),
            br(),
            actionButton("clear_bbox", "Clear bounding box"),
            br(), br(),
            verbatimTextOutput("bbox_coords")
          ),
          
          checkboxGroupInput("years", "Select Year(s):",
                             choices = 2021:2025,
                             selected = c(2021, 2022, 2023)),
          
          selectInput("iconic_taxon", "Select Taxonomic Group:",
                      choices = c("Aves", "Mammalia", "Reptilia", "Amphibia", "Actinopterygii", "Mollusca", "Animalia"),
                      selected = "Aves"),
          
          actionButton("run_query", "Run Query", icon = icon("play")),
          hr(),
          downloadButton("downloadData", "Download Top-90% CSV", icon = icon("download"))
        ),
        
        # == About Panel ==
        tabPanel(
          title = "About",
          br(),
          p("This Shiny application was created by Diego Ellis Soto (UC Berkeley). 
            It queries iNaturalist for observations that have been annotated as 'Dead' wildlife (term_id=17, term_value_id=19). 
            The data is fetched via the iNaturalist API and summarized here for scientific or conservation purposes.")
        ),
        
        # == Participatory Science Panel ==
        tabPanel(
          title = "Participatory Science",
          br(),
          p("Citizen science platforms like iNaturalist allow everyday people to collect and share data about local biodiversity. 
            Recording observations of dead wildlife can help track mortality events, disease spread, and other factors affecting animal populations."),
          p("We encourage everyone to contribute their sightings responsibly, ensuring that any data on roadkill or other mortalities can help conservation efforts and 
            raise public awareness.")
        ),
        
        # == How To Use Panel ==
        tabPanel(
          title = "How to Use",
          br(),
          p("This application lets you retrieve data about dead wildlife observations from iNaturalist. 
            You can either specify a place_id (e.g., country or region) or define a custom bounding box with two clicks on the map. 
            After choosing which years and taxonomic group to query, press 'Run Query.'"),
          p("These data are critical for understanding patterns of wildlife mortality, identifying hotspots of roadkill or disease, and informing conservation actions. 
            By systematically collecting and analyzing these records, conservation biologists and policymakers can make evidence-based decisions to protect wildlife populations.")
        )
      )
    ),
    
    # Main panel with tabbed outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Daily Time Series",    withSpinner(plotOutput("dailyPlot"), type = 6)),
        tabPanel("Top Species",          withSpinner(plotOutput("speciesPlot"), type = 6)),
        tabPanel("Hotspots Map (90th%)", withSpinner(plotOutput("hotspotMap"), type = 6)),
        tabPanel("Data Table (Top-90%)", withSpinner(DT::dataTableOutput("dataTable"), type = 6))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store bounding box corners from two clicks
  rv <- reactiveValues(
    corner1 = NULL,  # first click: (lat, lng)
    corner2 = NULL,  # second click: (lat, lng)
    bbox    = NULL   # computed bounding box: c(swlat, swlng, nelat, nelng)
  )
  
  # Render a simple Leaflet map (no draw toolbar).
  output$map_two_click <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -100, lat = 40, zoom = 4)
  })
  
  # Observe map clicks when "Two clicks to define bounding box" is selected
  observeEvent(input$map_two_click_click, {
    req(input$query_mode == "twoclick")
    
    click <- input$map_two_click_click
    if (is.null(click)) return()
    
    lat_clicked <- click$lat
    lng_clicked <- click$lng
    
    # If corner1 is NULL, store the first corner
    if (is.null(rv$corner1)) {
      rv$corner1 <- c(lat_clicked, lng_clicked)
      showNotification("First corner set. Now click for the opposite corner.")
      
      # Add marker for the first corner
      leafletProxy("map_two_click") %>%
        clearMarkers() %>%  # remove any old markers
        addMarkers(lng = lng_clicked, lat = lat_clicked,
                   popup = "Corner 1")
      
      rv$corner2 <- NULL
      rv$bbox <- NULL
      
    } else {
      # This is the second corner
      rv$corner2 <- c(lat_clicked, lng_clicked)
      
      # We'll compute bounding box from the two corners
      lat_min <- min(rv$corner1[1], rv$corner2[1])
      lat_max <- max(rv$corner1[1], rv$corner2[1])
      lng_min <- min(rv$corner1[2], rv$corner2[2])
      lng_max <- max(rv$corner1[2], rv$corner2[2])
      
      rv$bbox <- c(lat_min, lng_min, lat_max, lng_max)
      
      showNotification("Second corner set. Bounding box defined!", duration = 2)
      
      # Add marker for the second corner & a rectangle to visualize the bounding box
      leafletProxy("map_two_click") %>%
        clearMarkers() %>%
        addMarkers(lng = rv$corner1[2], lat = rv$corner1[1],
                   popup = "Corner 1") %>%
        addMarkers(lng = rv$corner2[2], lat = rv$corner2[1],
                   popup = "Corner 2") %>%
        # draw rectangle
        clearShapes() %>%
        addRectangles(
          lng1 = lng_min, lat1 = lat_min,
          lng2 = lng_max, lat2 = lat_max,
          fillColor = "red", fillOpacity = 0.2,
          color = "red"
        )
    }
  })
  
  # Button to clear bounding box and reset corners
  observeEvent(input$clear_bbox, {
    rv$corner1 <- NULL
    rv$corner2 <- NULL
    rv$bbox    <- NULL
    
    leafletProxy("map_two_click") %>%
      clearMarkers() %>%
      clearShapes()
  })
  
  # Show the user the bounding box
  output$bbox_coords <- renderText({
    req(input$query_mode == "twoclick")
    
    if (is.null(rv$bbox)) {
      "No bounding box defined yet."
    } else {
      paste0(
        "Bounding box:\n",
        "SW corner: (", rv$bbox[1], ", ", rv$bbox[2], ")\n",
        "NE corner: (", rv$bbox[3], ", ", rv$bbox[4], ")"
      )
    }
  })
  
  # Reactive value to store final query results
  result_data <- reactiveVal(NULL)
  
  # "Run Query" button
  observeEvent(input$run_query, {
    req(input$years)
    validate(need(length(input$years) > 0, "Please select at least one year"))
    
    yrs <- as.numeric(input$years)
    
    withProgress(message = 'Fetching data from iNaturalist...', value = 0, {
      incProgress(0.3)
      
      if (input$query_mode == "place") {
        # place_id mode
        place_id_val <- input$place_id
        swlat_val <- NULL
        swlng_val <- NULL
        nelat_val <- NULL
        nelng_val <- NULL
        
      } else {
        # two-click bounding box
        validate(need(!is.null(rv$bbox), "Please click twice on the map to define bounding box."))
        
        place_id_val <- NULL
        swlat_val <- rv$bbox[1]
        swlng_val <- rv$bbox[2]
        nelat_val <- rv$bbox[3]
        nelng_val <- rv$bbox[4]
      }
      
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
      
      result_data(query_res)
      incProgress(1)
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
  
  # (d) Data Table (top-90% days)
  output$dataTable <- DT::renderDataTable({
    req(result_data())
    df <- result_data()$merged_df
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No records found"), options = list(pageLength = 5)))
    }
    
    df <- df %>%
      mutate(
        inat_link = paste0(
          "<a href='https://www.inaturalist.org/observations/",
          id, "' target='_blank'>", id, "</a>"
        )
      )
    
    # If photos exist
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
    
    show_cols <- c(
      "inat_link", "image_thumb", "taxon.name", "created_at_details.date",
      setdiff(names(df), c("inat_link", "image_thumb", "taxon.name", "created_at_details.date"))
    )
    
    DT::datatable(
      df[ , show_cols, drop = FALSE],
      escape = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  # Download CSV handler
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
