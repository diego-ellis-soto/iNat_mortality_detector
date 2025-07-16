##################################################################
# Dead Wildlife iNaturalist Shiny – Professional Version (Live/Archive, All Data Table Fixed)
##################################################################

# Install and load required packages
required_packages <- c(
  "httr", "jsonlite", "tidyverse", "glue", "lubridate",
  "wesanderson", "viridis", "hexbin", "shinycssloaders",
  "DT", "maps", "mapdata", "leaflet", "leaflet.extras",
  "shinythemes", "shiny", "arrow"
)
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) install.packages(pkg, dependencies = TRUE)
}
library(httr)
library(jsonlite)
library(tidyverse)
library(glue)
library(lubridate)
library(wesanderson)
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

# Path to your archived parquet file (update if needed)
# parquet_path <- "~/inat_all_Apr122025.parquet"
parquet_path <- "https://huggingface.co/datasets/diegoellissoto/iNaturalist_mortality_records_12Apr2025/resolve/main/inat_all_Apr122025.parquet"
# ------------------- Helper Functions ---------------------
make_daily_plot <- function(df, start_date, end_date) {
  if (!"observed_on" %in% names(df)) return(ggplot() + theme_void() + labs(title = "No date info"))
  if (nrow(df) == 0) return(ggplot() + theme_void() + labs(title = "No data"))
  df <- df %>%
    mutate(obs_date = as.Date(observed_on),
           Window   = format(obs_date, "%Y")) %>%
    filter(!is.na(obs_date))
  counts_by_day <- df %>%
    group_by(Window, obs_date) %>%
    summarise(n = n_distinct(id), .groups = "drop")
  y_max_value <- max(counts_by_day$n, na.rm = TRUE)
  ggplot(counts_by_day, aes(x = obs_date, y = n, color = Window)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
    scale_y_continuous(limits = c(0, y_max_value)) +
    labs(
      title = glue("Daily 'Dead' Observations ({start_date} to {end_date})"),
      x     = "Date",
      y     = "Number of Observations",
      color = "Year"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

make_top_species_plot <- function(df) {
  if (!"scientific_name" %in% names(df)) return(ggplot() + theme_void() + labs(title = "No species info"))
  if (nrow(df) == 0) return(ggplot() + theme_void() + labs(title = "No data"))
  df <- df %>%
    mutate(obs_date = as.Date(observed_on),
           Window   = format(obs_date, "%Y"))
  species_counts <- df %>%
    filter(!is.na(scientific_name)) %>%
    group_by(Window, scientific_name) %>%
    summarise(dead_count = n(), .groups = "drop")
  top_species_overall <- species_counts %>%
    group_by(scientific_name) %>%
    summarise(total_dead = sum(dead_count)) %>%
    arrange(desc(total_dead)) %>%
    slice_head(n = 20)
  species_top20 <- species_counts %>%
    filter(scientific_name %in% top_species_overall$scientific_name)
  ggplot(species_top20, aes(
    x   = reorder(scientific_name, -dead_count),
    y   = dead_count,
    fill= Window
  )) +
    geom_col(position = position_dodge(width = 0.7)) +
    coord_flip() +
    labs(
      title = "Top 20 Species with 'Dead' Observations",
      x     = "Species",
      y     = "Number of Dead Observations",
      fill  = "Year"
    ) +
    theme_minimal(base_size = 14)
}

make_hexbin_map <- function(df, start_date, end_date) {
  if (!("latitude" %in% names(df) && "longitude" %in% names(df))) {
    return(ggplot() + labs(title = "No spatial data available for map") + theme_void())
  }
  df <- df %>% filter(!is.na(latitude) & !is.na(longitude))
  if (nrow(df) == 0) {
    return(ggplot() + labs(title = "No spatial data available for map") + theme_void())
  }
  x_limits <- range(df$longitude, na.rm = TRUE)
  y_limits <- range(df$latitude,  na.rm  = TRUE)
  ggplot() +
    borders("world", fill = "gray80", colour = "white") +
    stat_bin_hex(
      data  = df,
      aes(x = longitude, y = latitude),
      bins  = 500,
      color = "black",
      alpha = 0.8
    ) +
    # scale_fill_viridis_c(option = "plasma", name = "Observation Count") +
    scale_fill_viridis_c(option = "magma", name = "Obs. Count", trans="sqrt") +
    coord_quickmap(xlim = x_limits, ylim = y_limits, expand = TRUE) +
    labs(
      title = glue("'Dead' Wildlife Hexbin Map ({start_date} to {end_date})"),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_classic(base_size = 14) +
    theme(
      axis.text  = element_text(face = "bold", size = 14, colour = "black"),
      axis.title = element_text(face = "bold", size = 16, colour = "black")
    )
}

get_high_mortality_days <- function(df) {
  if (!"observed_on" %in% names(df)) return(NULL)
  df <- df %>% mutate(obs_date = as.Date(observed_on))
  counts_by_day <- df %>%
    group_by(obs_date) %>%
    summarise(n = n_distinct(id), .groups = "drop")
  if (nrow(counts_by_day) == 0) return(NULL)
  daily_quantile <- quantile(counts_by_day$n, probs = 0.90, na.rm = TRUE)
  high_days <- counts_by_day %>% filter(n >= daily_quantile) %>% pull(obs_date)
  list(days = high_days, quant = daily_quantile)
}

# -- API Fetch/Progress bar logic (Live mode) --
fetch_dead_data_once <- function(
    swlat, swlng, nelat, nelng,
    start_date, end_date,
    iconic_taxa = NULL, taxon_name = NULL,
    per_page = 200, max_pages = 200, progress = NULL
) {
  base_url <- "https://api.inaturalist.org/v1/observations"
  q_parts <- list(
    "term_id=17", "term_value_id=19", "verifiable=true",
    glue("d1={start_date}"), glue("d2={end_date}"),
    "order=desc", "order_by=created_at", glue("per_page={per_page}")
  )
  if (!is.null(iconic_taxa) && iconic_taxa != "") q_parts <- c(q_parts, glue("iconic_taxa={iconic_taxa}"))
  if (!is.null(taxon_name) && taxon_name != "") q_parts <- c(q_parts, glue("taxon_name={URLencode(taxon_name)}"))
  query_params <- paste(q_parts, collapse = "&")
  loc_part <- glue("&nelat={nelat}&nelng={nelng}&swlat={swlat}&swlng={swlng}")
  observations_list <- list()
  current_page <- 1
  while (current_page <= max_pages) {
    if (!is.null(progress)) progress$set(detail = glue("API page {current_page}"), value = NULL)
    query_url <- paste0(base_url, "?", query_params, "&page=", current_page, loc_part)
    resp <- GET(query_url)
    if (http_error(resp)) {
      warning("HTTP error on page ", current_page, ": ", status_code(resp))
      break
    }
    parsed <- content(resp, as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE)
    if (length(parsed$results) == 0) break
    obs_page_df <- as_tibble(parsed$results)
    observations_list[[current_page]] <- obs_page_df
    if (nrow(obs_page_df) < per_page) break
    current_page <- current_page + 1
    Sys.sleep(1.4)
  }
  bind_rows(observations_list)
}

getDeadVertebrates_dateRange <- function(
    start_date, end_date,
    swlat, swlng, nelat, nelng,
    iconic_taxa = NULL, taxon_name = NULL,
    per_page = 500, max_pages = 500,
    .shiny_progress = NULL
) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  week_starts <- seq.Date(start_date, end_date, by = "1 week")
  all_weeks_list <- list()
  for (i in seq_along(week_starts)) {
    st <- week_starts[i]
    ed <- if (i < length(week_starts)) week_starts[i + 1] - 1 else end_date
    if (!is.null(.shiny_progress)) {
      .shiny_progress$set(
        value = (i-1)/length(week_starts),
        message = glue("Live Query: Fetching week {i} of {length(week_starts)}"),
        detail = glue("Dates: {st} to {ed}")
      )
    }
    df_week <- fetch_dead_data_once(
      swlat, swlng, nelat, nelng,
      start_date = st, end_date = ed,
      iconic_taxa = iconic_taxa, taxon_name = taxon_name,
      per_page = per_page, max_pages = max_pages,
      progress = .shiny_progress
    )
    all_weeks_list[[i]] <- df_week
    Sys.sleep(1.4)
  }
  merged_df_all <- bind_rows(all_weeks_list)
  # Everything else unchanged (see previous versions, e.g. make plots, high mortality)
  if (!"created_at_details.date" %in% names(merged_df_all) || nrow(merged_df_all) == 0) {
    placeholder_plot <- function(title) {
      ggplot() + labs(title = title, x = NULL, y = NULL) + theme_void()
    }
    return(list(
      merged_df_all    = merged_df_all,
      merged_df        = merged_df_all,
      daily_plot       = placeholder_plot("No 'Dead' Observations Found"),
      top_species_plot = placeholder_plot("No species data"),
      map_hotspots_gg  = placeholder_plot("No data for map"),
      daily_90th_quant = NA
    ))
  }
  merged_df_all <- merged_df_all %>%
    mutate(obs_date = as.Date(observed_on),
           Window   = format(obs_date, "%Y"))
  counts_by_day <- merged_df_all %>%
    group_by(Window, obs_date) %>%
    summarise(n = n_distinct(id), .groups = "drop")
  y_max_value <- max(counts_by_day$n, na.rm = TRUE)
  daily_plot <- ggplot(counts_by_day, aes(x = obs_date, y = n, color = Window)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
    scale_y_continuous(limits = c(0, y_max_value)) +
    labs(
      title = glue("Daily 'Dead' Observations ({start_date} to {end_date})"),
      x     = "Date",
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
    filter(obs_date %in% high_mortality_days)
  if ("location" %in% names(merged_df_all)) {
    location_df_all <- merged_df_all %>%
      filter(!is.na(location) & location != "") %>%
      separate(location, into = c("lat_str", "lon_str"), sep = ",", remove = FALSE) %>%
      mutate(latitude = as.numeric(lat_str), longitude = as.numeric(lon_str))
    if (nrow(location_df_all) == 0) {
      map_hotspots_gg <- ggplot() +
        labs(title = "No spatial data available for map") +
        theme_void()
    } else {
      x_limits <- range(location_df_all$longitude, na.rm = TRUE)
      y_limits <- range(location_df_all$latitude,  na.rm  = TRUE)
      map_hotspots_gg <- ggplot() +
        borders("world", fill = "gray80", colour = "white") +
        stat_bin_hex(
          data  = location_df_all,
          aes(x = longitude, y = latitude),
          bins  = 500,
          color = "black",
          alpha = 0.8
        ) +
        scale_fill_viridis_c(option = "plasma", name = "Observation Count") +
        coord_quickmap(xlim = x_limits, ylim = y_limits, expand = TRUE) +
        labs(
          title = glue("'Dead' Wildlife Hexbin Map ({start_date} to {end_date})"),
          x = "Longitude",
          y = "Latitude"
        ) +
        theme_classic(base_size = 14) +
        theme(
          axis.text  = element_text(face = "bold", size = 14, colour = "black"),
          axis.title = element_text(face = "bold", size = 16, colour = "black")
        )
    }
  } else {
    map_hotspots_gg <- ggplot() +
      labs(title = "No 'location' column for map") +
      theme_void()
  }
  return(list(
    merged_df_all    = merged_df_all,
    merged_df        = merged_high,
    daily_plot       = daily_plot,
    top_species_plot = top_species_plot,
    map_hotspots_gg  = map_hotspots_gg,
    daily_90th_quant = daily_quantile
  ))
}
# ------------------------ UI --------------------------
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  fluidRow(
    column(width = 2, tags$img(src = "www/all_logos.png", height = "400px")),
    column(width = 10, titlePanel("Dead Wildlife Observations from iNaturalist"))
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "sidebar_tabs",
                  tabPanel("Query",
                           br(),
                           radioButtons(
                             "data_source", "Data Source:",
                             choices = c("Download Live from iNaturalist" = "live", "Archived Parquet File" = "archived"),
                             selected = "live"
                           ),
                           tags$div(
                             style="margin-bottom:8px;",
                             textInput("region_search", "Find place (type and click Search)", value = "", placeholder = "e.g. California, Uruguay, Yellowstone"),
                             actionButton("region_search_btn", "Search", icon=icon("search"))
                           ),
                           tags$div(
                             style = "margin-bottom:10px;",
                             leafletOutput("select_map", height = "340px"),
                             actionButton("clear_bbox", "Clear Bounding Box", icon = icon("eraser")),
                             helpText("Draw a rectangle or search for a place. Only one region (rectangle) is used at a time.")
                           ),
                           verbatimTextOutput("bbox_coords"),
                           dateRangeInput("date_range", "Select Date Range:",
                                          start  = Sys.Date() - 365,
                                          end    = Sys.Date(),
                                          min    = "2010-01-01",
                                          max    = Sys.Date()),
                           radioButtons("query_type", "Query By:",
                                        choices = c("Taxon Class" = "iconic", "Exact Species Name" = "species")),
                           conditionalPanel(
                             condition = "input.query_type == 'iconic'",
                             selectInput("iconic_taxon", "Select Taxon Class:",
                                         choices = c("Aves", "Mammalia", "Reptilia", "Amphibia", "Actinopterygii", "Mollusca", "Animalia"),
                                         selected = "Aves")
                           ),
                           conditionalPanel(
                             condition = "input.query_type == 'species'",
                             textInput("species_name", "Enter exact species name (e.g. Puma concolor)", "")
                           ),
                           actionButton("run_query", "Run Query", icon = icon("play")),
                           hr(),
                           downloadButton("downloadAll", "Download ALL Data CSV", icon = icon("download"))
                  ),
                  tabPanel("About",
                           tags$h3("iNaturalist, Dead Wildlife, and Participatory Science"),
                           tags$p("iNaturalist is a global biodiversity platform powered by a vibrant community of naturalists, scientists, students, and citizens. Its open data and easy smartphone app allow anyone to record nature and contribute to science."),
                           tags$h4("Why observe dead wildlife?"),
                           tags$ul(
                             tags$li("Track disease outbreaks and mass die-offs (e.g. avian influenza, amphibian disease)."),
                             tags$li("Identify human-wildlife conflicts (e.g. roadkill, window strikes)."),
                             tags$li("Detect range shifts and rare events."),
                             tags$li("Monitor mortality of threatened or sensitive species.")
                           ),
                           tags$p("Documenting dead wildlife—even if unpleasant—can save species by detecting threats early."),
                           tags$h4("About this App"),
                           tags$p("This app was created by Diego Ellis-Soto (UC Berkeley) and colleagues to empower rapid, open exploration of wildlife mortality patterns worldwide. It is open source and intended for research, conservation, and education."),
                           tags$blockquote("Ellis-Soto D., Taylor L., Edson E., Schell C., Boettiger C., Johnson R. (2024). Global, near real-time ecological forecasting of mortality events through participatory science
 
                                           https://github.com/diego-ellis-soto/iNat_mortality_detector"),
                           tags$h4("Technical Info"),
                           tags$ul(
                             tags$li("iNaturalist API v1 (Live Mode) and Parquet snapshot (Archive Mode).")
                           ),
                           tags$h4("FAQ"),
                           tags$dl(
                             tags$dt("Can I use this data for research/publication?"),
                             tags$dd("Yes! Always credit iNaturalist and respect original content licenses. See iNaturalist's Data Use Policy."),
                             tags$dt("Why is the map sometimes empty?"),
                             tags$dd("Some species/locations are obscured for privacy, or there may be no recent observations in your selected area and time."),
                             tags$dt("Are locations accurate?"),
                             tags$dd("Coordinate accuracy varies by observer and privacy settings."),
                             tags$dt("Can I see private/sensitive records?"),
                             tags$dd("No—privacy and ethical protection is strictly respected by the iNaturalist API and this app.")
                           ),
                           tags$h4("Responsible Use"),
                           tags$p("Never disturb wildlife for photos. Be cautious with sensitive data. Community-driven science works best when it's ethical and transparent."),
                           tags$h4("Get Involved!"),
                           tags$p("Join iNaturalist, share your own records, or help identify others' observations. Every data point helps conservation.")
                  ),
                  tabPanel("How to Use",
                           tags$h3("Quick Start Guide"),
                           tags$ol(
                             tags$li("Search for a place or draw a rectangle on the map (one region at a time)."),
                             tags$li("Set your date range. For best speed, keep queries focused."),
                             tags$li("Choose a taxon class or enter a species name."),
                             tags$li("Pick 'Live' for the latest data (slower, but up-to-date) or 'Archive' for instant results (fixed snapshot)."),
                             tags$li("Click Run Query. Visualizations and tables will update below!"),
                             tags$li("Download the full results table as CSV for further analysis.")
                           ),
                           tags$h4("Tips"),
                           tags$ul(
                             tags$li("Use Archive mode for large or exploratory queries—it is much faster."),
                             tags$li("Live mode fetches week-by-week and may take minutes for big regions or long periods (a progress bar helps you track progress)."),
                             tags$li("To reset your selected area, click 'Clear Bounding Box'.")
                           ),
                           tags$h4("Contact & Support"),
                           tags$p("For questions or feedback, visit our GitHub repository or email the authors.")
                  )
      )
    ),
    mainPanel(
      tabsetPanel(
        # ----------- CHANGED: summary beside plot -----------
        tabPanel("Daily Time Series",
                 fluidRow(
                   column(width = 8, withSpinner(plotOutput("dailyPlot"), type = 6)),
                   column(width = 4, verbatimTextOutput("dailySummary"))
                 )
        ),
        tabPanel("Top Species",           withSpinner(plotOutput("speciesPlot"), type = 6)),
        tabPanel("Hexbin Map (All Data)", withSpinner(plotOutput("hotspotMap"),  type = 6)),
        tabPanel("All Data Table",        withSpinner(DT::dataTableOutput("dataTable"), type = 6))
      )
    )
  )
)
# -------------------- SERVER --------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(bbox = NULL)
  output$select_map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = -95, lat = 40, zoom = 3) %>%
      addDrawToolbar(
        targetGroup = "drawn_bboxes",
        rectangleOptions = drawRectangleOptions(repeatMode = FALSE),
        polylineOptions = FALSE, circleOptions = FALSE,
        markerOptions = FALSE, circleMarkerOptions = FALSE,
        polygonOptions = FALSE, editOptions = editToolbarOptions()
      )
  })
  observeEvent(input$select_map_draw_new_feature, {
    feat <- input$select_map_draw_new_feature
    if (!is.null(feat$geometry) && feat$geometry$type == "Polygon") {
      coords <- feat$geometry$coordinates[[1]]
      lngs <- vapply(coords, function(x) x[[1]], numeric(1))
      lats <- vapply(coords, function(x) x[[2]], numeric(1))
      rv$bbox <- c(min(lats), min(lngs), max(lats), max(lngs))
    }
  })
  observeEvent(input$select_map_draw_deleted_features, { rv$bbox <- NULL })
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
        }
      }
    }
  })
  observeEvent(input$clear_bbox, {
    rv$bbox <- NULL
    leafletProxy("select_map") %>%
      clearGroup("drawn_bboxes") %>%
      clearGroup("search_bbox")
  })
  observeEvent(input$region_search_btn, {
    loc <- input$region_search
    if (!is.null(loc) && nzchar(loc)) {
      url <- paste0("https://nominatim.openstreetmap.org/search?format=json&q=", URLencode(loc))
      res <- tryCatch(jsonlite::fromJSON(url), error=function(e) NULL)
      if (!is.null(res) && nrow(res) >= 1 && !is.null(res$boundingbox[1])) {
        bbox_bb <- res$boundingbox[1][[1]]
        if (is.character(bbox_bb) && length(bbox_bb) == 4) {
          bbox_raw <- as.numeric(bbox_bb)
        } else if (is.list(bbox_bb) && length(bbox_bb) == 4) {
          bbox_raw <- as.numeric(unlist(bbox_bb))
        } else if (is.character(res$boundingbox[1])) {
          bbox_raw <- as.numeric(unlist(strsplit(res$boundingbox[1], ",")))
        } else {
          bbox_raw <- NULL
        }
        if (!is.null(bbox_raw) && length(bbox_raw) == 4 && all(!is.na(bbox_raw))) {
          bbox <- c(bbox_raw[1], bbox_raw[3], bbox_raw[2], bbox_raw[4])
          leafletProxy("select_map") %>%
            clearGroup("search_bbox") %>%
            addRectangles(
              lng1 = bbox[2], lat1 = bbox[1], lng2 = bbox[4], lat2 = bbox[3],
              fillColor = "red", fillOpacity = 0.1, color = "red", group = "search_bbox"
            ) %>%
            fitBounds(lng1 = bbox[2], lat1 = bbox[1], lng2 = bbox[4], lat2 = bbox[3])
          rv$bbox <- bbox
        } else {
          showNotification("Unexpected bounding box format from geocoder.", type = "error", duration = 6)
        }
      } else {
        showNotification("Could not geocode this place. Try a different name.", type = "warning", duration = 5)
      }
    }
  })
  output$bbox_coords <- renderText({
    if (is.null(rv$bbox)) "No bounding box defined yet. Search for a place or draw a rectangle." else paste0(
      "Bounding box:\nSW: (", round(rv$bbox[1], 4), ", ", round(rv$bbox[2], 4), ")\nNE: (", round(rv$bbox[3], 4), ", ", round(rv$bbox[4], 4), ")")
  })
  result_data <- reactiveVal(NULL)
  observeEvent(input$run_query, {
    req(input$date_range)
    req(rv$bbox)
    start_date <- as.Date(input$date_range[1])
    end_date   <- as.Date(input$date_range[2])
    swlat <- rv$bbox[1]; swlng <- rv$bbox[2]; nelat <- rv$bbox[3]; nelng <- rv$bbox[4]
    if (input$data_source == "archived") {
      # req(file.exists(parquet_path))
      inat_all_raw <- arrow::read_parquet(parquet_path)#  %>% as_tibble()
      inat_all <- inat_all_raw %>%
        filter(!is.na(latitude) & !is.na(longitude)) %>%
        filter(latitude  >= swlat, latitude  <= nelat,
               longitude >= swlng, longitude <= nelng) |> collect()
      if ("observed_on" %in% names(inat_all)) {
        inat_all <- inat_all %>%
          filter(!is.na(observed_on)) %>%
          filter(as.Date(observed_on) >= start_date, as.Date(observed_on) <= end_date)
      }
      if (input$query_type == "iconic" && !is.null(input$iconic_taxon) && input$iconic_taxon != "" &&
          "iconic_taxon_name" %in% names(inat_all)) {
        inat_all <- inat_all %>% filter(iconic_taxon_name == input$iconic_taxon)
      }
      if (input$query_type == "species" && !is.null(input$species_name) && input$species_name != "" &&
          "scientific_name" %in% names(inat_all)) {
        inat_all <- inat_all %>% filter(scientific_name == input$species_name)
      }
      hm <- get_high_mortality_days(inat_all)
      merged_high <- if (!is.null(hm$days)) inat_all %>% filter(as.Date(observed_on) %in% hm$days) else inat_all
      query_res <- list(
        merged_df_all    = inat_all,
        merged_df        = merged_high,
        daily_plot       = make_daily_plot(inat_all, start_date, end_date),
        top_species_plot = make_top_species_plot(inat_all),
        map_hotspots_gg  = make_hexbin_map(inat_all, start_date, end_date),
        daily_90th_quant = if (!is.null(hm$quant)) hm$quant else NA
      )
      result_data(query_res)
    } else {
      iconic_val  <- if (input$query_type == "iconic") input$iconic_taxon  else NULL
      species_val <- if (input$query_type == "species") input$species_name else NULL
      week_starts <- seq.Date(start_date, end_date, by = "1 week")
      showNotification(
        paste("Live Mode: About to fetch", length(week_starts),
              "weeks from iNaturalist API. This may take several minutes for large queries."),
        duration = 7, type = "warning"
      )
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = paste("Live Query: Fetching", length(week_starts), "weeks"), value = 0)
      query_res <- getDeadVertebrates_dateRange(
        start_date    = start_date,
        end_date      = end_date,
        swlat         = swlat,
        swlng         = swlng,
        nelat         = nelat,
        nelng         = nelng,
        iconic_taxa   = iconic_val,
        taxon_name    = species_val,
        .shiny_progress = progress
      )
      result_data(query_res)
    }
  })
  output$dailyPlot   <- renderPlot({ req(result_data()); result_data()$daily_plot })
  
  output$dailySummary <- renderText({
    req(result_data())
    df <- result_data()$merged_df_all
    if (nrow(df) == 0 || !"observed_on" %in% names(df)) return("No data available.")
    df <- df %>% mutate(obs_date = as.Date(observed_on)) %>% filter(!is.na(obs_date))
    n_obs <- nrow(df)
    n_days <- n_distinct(df$obs_date)
    span_days <- if (n_days > 1) paste0(range(df$obs_date, na.rm=TRUE), collapse=" to ") else as.character(unique(df$obs_date))
    counts_by_day <- df %>% count(obs_date)
    peak <- counts_by_day %>% filter(n == max(n)) %>% pull(obs_date)
    peak_val <- max(counts_by_day$n)
    avg_day <- round(mean(counts_by_day$n), 2)
    paste0(
      "Summary:\n",
      "- Total mortality records: ", n_obs, "\n",
      "- Date range: \n", span_days, "\n",
      "- Days with data: ", n_days, "\n",
      "- Average per day: ", avg_day, "\n",
      "- Peak day: ", paste(peak, collapse = ", "), " (", peak_val, " records)\n",
      if (peak_val > avg_day*2) "- Spike in mortality observations" else ""
    )
    
  })
  
  output$speciesPlot <- renderPlot({ req(result_data()); result_data()$top_species_plot })
  output$hotspotMap  <- renderPlot({ req(result_data()); result_data()$map_hotspots_gg })
  output$dataTable <- DT::renderDataTable({
    req(result_data())
    df <- result_data()$merged_df_all
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No records found"), options = list(pageLength = 20)))
    }
    # --- Fix column naming for scientific_name ---
    if (!"scientific_name" %in% names(df) && "taxon.name" %in% names(df)) {
      df$scientific_name <- df$taxon.name
    }
    # --- Add inat_link ---
    if ("id" %in% names(df)) {
      df$inat_link <- paste0("<a href='https://www.inaturalist.org/observations/", df$id, "' target='_blank'>", df$id, "</a>")
    } else {
      df$inat_link <- NA
    }
    # --- Fix image column (robust to both sources) ---
    df$image_thumb <- "No Img"
    if ("image_url" %in% names(df)) {
      df$image_thumb <- ifelse(!is.na(df$image_url) & df$image_url != "", paste0("<img src='", df$image_url, "' width='50'/>"), "No Img")
    } else if ("taxon.default_photo.square_url" %in% names(df)) {
      df$image_thumb <- ifelse(!is.na(df$taxon.default_photo.square_url) & df$taxon.default_photo.square_url != "", paste0("<img src='", df$taxon.default_photo.square_url, "' width='50'/>"), "No Img")
    } else if ("taxon" %in% names(df)) {
      taxon_photo <- sapply(df$taxon, function(x) {
        if (is.list(x) && "default_photo" %in% names(x) && !is.null(x$default_photo$square_url)) x$default_photo$square_url else NA
      })
      df$image_thumb <- ifelse(!is.na(taxon_photo) & taxon_photo != "", paste0("<img src='", taxon_photo, "' width='50'/>"), "No Img")
    }
    # --- Show columns ---
    show_cols <- c(
      "inat_link", "image_thumb", 
      "scientific_name",
      intersect(c("observed_on", "created_at_details.date"), names(df)),
      "latitude", "longitude",
      setdiff(names(df), c("inat_link", "image_thumb", "scientific_name", "observed_on", "created_at_details.date", "latitude", "longitude"))
    )
    DT::datatable(df[, show_cols[show_cols %in% names(df)], drop = FALSE], escape = FALSE,
                  options = list(pageLength = 20, autoWidth = TRUE))
  })
  output$downloadAll <- downloadHandler(
    filename = function() paste0("inat_dead_ALL_", Sys.Date(), ".csv"),
    content = function(file) { req(result_data()); readr::write_csv(result_data()$merged_df_all, file) }
  )
}

shinyApp(ui = ui, server = server)
