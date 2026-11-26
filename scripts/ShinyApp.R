# Load libraries
packages <- c(
  "shiny", "rsconnect", "googlesheets4", "leaflet",
  "htmltools", "viridisLite", "dplyr", "DT", "curl"# Load libraries
packages <- c(
  "shiny", "rsconnect", "googlesheets4", "leaflet",
  "htmltools", "viridisLite", "dplyr", "DT", "curl"
)

install.packages(
  setdiff(packages, rownames(installed.packages())),
  repos = "https://cloud.r-project.org"
)

lapply(packages, library, character.only = TRUE)

# ================================================================
# Configuration
# ================================================================

sheet_url <- "https://docs.google.com/spreadsheets/d/1FXjSm275heZTNx_I0t72BC-cqbDaC_xjY8iOcAmEE_Y/edit?gid=0#gid=0"

# For a publicly readable Google Sheet
gs4_deauth()

# These columns are excluded from the general filter dropdowns.
# They are handled separately with sliders or search.
excluded_filter_columns <- c(
  "Sample_name",
  "Sample_name_initial",
  "Sample_lat",
  "Sample_lon",
  "Sample_depth",
  "Sample_year",
  "Sample_month",
  "Sample_day",
  "Sample_date",
  "Specimen_length",
  "Specimen_weight"
)

# Account information (for private google sheets)
#rsconnect::setAccountInfo(name='xxx',
#                          token='xx',
#                          secret='xx')

# ================================================================
# Helper functions
# ================================================================

as_clean_character <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

display_values_per_row <- function(x) {
  x <- as_clean_character(x)
  x[x == ""] <- "(blank)"
  x
}

display_values <- function(x) {
  unique(display_values_per_row(x))
}

valid_numeric_values <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x)]
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Parse several possible date formats:
# 20240816
# 2024-08-16
# 2024/08/16
# Google Sheets numeric date serials
parse_sample_dates <- function(x) {
  
  if (inherits(x, "Date")) {
    return(x)
  }
  
  x_character <- as_clean_character(x)
  result <- rep(as.Date(NA), length(x_character))
  
  # Format: YYYYMMDD
  is_yyyymmdd <- grepl(
    "^[0-9]{8}$",
    x_character
  )
  
  result[is_yyyymmdd] <- as.Date(
    x_character[is_yyyymmdd],
    format = "%Y%m%d"
  )
  
  # Format: YYYY-MM-DD
  still_missing <- is.na(result) & x_character != ""
  
  result[still_missing] <- as.Date(
    x_character[still_missing],
    format = "%Y-%m-%d"
  )
  
  # Format: YYYY/MM/DD
  still_missing <- is.na(result) & x_character != ""
  
  result[still_missing] <- as.Date(
    x_character[still_missing],
    format = "%Y/%m/%d"
  )
  
  # Handle numeric Google Sheets date serials
  numeric_values <- suppressWarnings(
    as.numeric(x_character)
  )
  
  is_possible_serial <- is.na(result) &
    !is.na(numeric_values) &
    numeric_values > 20000 &
    numeric_values < 100000
  
  result[is_possible_serial] <- as.Date(
    numeric_values[is_possible_serial],
    origin = "1899-12-30"
  )
  
  result
}

# Create a date from year/month/day if Sample_date is missing
construct_fallback_date <- function(dat) {
  
  if (!"Sample_date" %in% names(dat)) {
    dat$Sample_date <- as.Date(NA)
  } else {
    dat$Sample_date <- parse_sample_dates(
      dat$Sample_date
    )
  }
  
  required_columns <- c(
    "Sample_year",
    "Sample_month",
    "Sample_day"
  )
  
  if (all(required_columns %in% names(dat))) {
    
    year_values <- suppressWarnings(
      as.integer(as_clean_character(dat$Sample_year))
    )
    
    month_values <- suppressWarnings(
      as.integer(as_clean_character(dat$Sample_month))
    )
    
    day_values <- suppressWarnings(
      as.integer(as_clean_character(dat$Sample_day))
    )
    
    fallback_dates <- as.Date(
      paste(
        year_values,
        month_values,
        day_values,
        sep = "-"
      ),
      format = "%Y-%m-%d"
    )
    
    missing_date <- is.na(dat$Sample_date)
    
    dat$Sample_date[missing_date] <-
      fallback_dates[missing_date]
  }
  
  dat
}

# Make HTML popup for one sample
make_popup <- function(row) {
  
  row <- as.data.frame(
    row,
    stringsAsFactors = FALSE
  )
  
  popup_rows <- vapply(
    names(row),
    function(column_name) {
      
      value <- row[[column_name]][1]
      
      if (inherits(value, "Date")) {
        value <- format(value, "%Y-%m-%d")
      }
      
      if (is.na(value)) {
        value <- ""
      }
      
      paste0(
        "<tr>",
        "<th style='text-align:left; padding-right:12px;'>",
        htmlEscape(column_name),
        "</th>",
        "<td>",
        htmlEscape(as.character(value)),
        "</td>",
        "</tr>"
      )
    },
    character(1)
  )
  
  paste0(
    "<table>",
    paste0(popup_rows, collapse = ""),
    "</table>"
  )
}

# ================================================================
# Read data from Google Sheets
# ================================================================

read_sample_data <- function() {
  
  dat <- read_sheet(
    ss = sheet_url,
    sheet = 1
  )
  
  required_columns <- c(
    "Sample_lat",
    "Sample_lon"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(dat)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  # Convert latitude and longitude to numeric.
  # This removes the units row because it contains no numeric coordinates.
  dat <- dat %>%
    mutate(
      Sample_lat = suppressWarnings(
        as.numeric(as.character(Sample_lat))
      ),
      Sample_lon = suppressWarnings(
        as.numeric(as.character(Sample_lon))
      )
    ) %>%
    filter(
      !is.na(Sample_lat),
      !is.na(Sample_lon)
    )
  
  if ("Sample_depth" %in% names(dat)) {
    dat$Sample_depth <- suppressWarnings(
      as.numeric(as.character(dat$Sample_depth))
    )
  }
  
  for (column in c("Specimen_length", "Specimen_weight")) {
    if (column %in% names(dat)) {
      dat[[column]] <- suppressWarnings(
        as.numeric(as.character(dat[[column]]))
      )
    }
  }
  
  dat <- construct_fallback_date(dat)
  
  if (!"Sample_name" %in% names(dat)) {
    dat$Sample_name <- paste0(
      "Sample_",
      seq_len(nrow(dat))
    )
  }
  
  dat
}

# ================================================================
# User interface
# ================================================================

ui <- fluidPage(
  
  titlePanel("Sample collection map"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Data refresh"),
      
      actionButton(
        inputId = "refresh_now",
        label = "Refresh data now",
        icon = icon("refresh")
      ),
      
      br(),
      br(),
      
      textOutput("last_updated"),
      textOutput("refresh_status"),
      
      hr(),
      
      h4("Search samples"),
      
      textInput(
        inputId = "sample_search",
        label = "Search Sample_name:",
        placeholder = "Enter part of a sample name"
      ),
      
      h4("Filter by date"),
      
      sliderInput(
        inputId = "date_range",
        label = "Sample date:",
        min = as.Date("2020-01-01"),
        max = as.Date("2030-01-01"),
        value = as.Date(
          c("2020-01-01", "2030-01-01")
        ),
        timeFormat = "%Y-%m-%d"
      ),
      
      h4("Filter by latitude"),
      
      sliderInput(
        inputId = "lat_range",
        label = "Sample latitude:",
        min = 0,
        max = 90,
        value = c(0, 90),
        step = 0.000001
      ),
      
      h4("Filter by longitude"),
      
      sliderInput(
        inputId = "lon_range",
        label = "Sample longitude:",
        min = -180,
        max = 180,
        value = c(-180, 180),
        step = 0.000001
      ),
      
      h4("Filter by depth"),
      
      sliderInput(
        inputId = "depth_range",
        label = "Sample depth:",
        min = 0,
        max = 1000,
        value = c(0, 1000),
        step = 1
      ),
      
      h4("Filter by specimen length"),
      
      sliderInput(
        inputId = "length_range",
        label = "Specimen length:",
        min = 0,
        max = 1000,
        value = c(0, 1000),
        step = 0.01
      ),
      
      h4("Filter by specimen weight"),
      
      sliderInput(
        inputId = "weight_range",
        label = "Specimen weight:",
        min = 0,
        max = 1000,
        value = c(0, 1000),
        step = 0.01
      ),
      
      hr(),
      
      h4("Metadata filters"),
      
      p(
        "Select values in one or more fields. ",
        "Leave a field empty to include all values."
      ),
      
      uiOutput("filter_controls"),
      
      hr(),
      
      h4("Map display"),
      
      selectInput(
        inputId = "colour_variable",
        label = "Colour points by:",
        choices = NULL
      ),
      
      checkboxInput(
        inputId = "show_labels",
        label = "Show sample names on map",
        value = FALSE
      ),
      
      checkboxInput(
        inputId = "enable_clustering",
        label = "Cluster nearby samples",
        value = TRUE
      ),
      
      hr(),
      
      h4("Current selection"),
      
      textOutput("number_of_samples"),
      tableOutput("summary_table"),
      
      downloadButton(
        outputId = "download_data",
        label = "Download filtered data"
      ),
      
      width = 3
    ),
    
    mainPanel(
      
      leafletOutput(
        outputId = "sample_map",
        height = "700px"
      ),
      
      br(),
      
      h4("Filtered sample table"),
      
      DTOutput("sample_table"),
      
      width = 9
    )
  )
)

# ================================================================
# Server
# ================================================================

server <- function(input, output, session) {
  
  sample_data <- reactiveVal(NULL)
  last_updated <- reactiveVal(NULL)
  refresh_status <- reactiveVal("")
  
  # --------------------------------------------------------------
  # Refresh data
  # --------------------------------------------------------------
  
  refresh_data <- function() {
    
    tryCatch(
      
      {
        new_data <- read_sample_data()
        
        sample_data(new_data)
        last_updated(Sys.time())
        refresh_status("Data loaded successfully.")
      },
      
      error = function(e) {
        
        refresh_status(
          paste(
            "Refresh failed:",
            conditionMessage(e)
          )
        )
      }
    )
  }
  
  # Initial load
  refresh_data()
  
  # Manual refresh
  observeEvent(input$refresh_now, {
    refresh_data()
  })
  
  # Automatic refresh every 24 hours
  refresh_timer <- reactiveTimer(
    intervalMs = 24 * 60 * 60 * 1000
  )
  
  observe({
    refresh_timer()
    refresh_data()
  })
  
  # --------------------------------------------------------------
  # Last-updated information
  # --------------------------------------------------------------
  
  output$last_updated <- renderText({
    
    timestamp <- last_updated()
    
    if (is.null(timestamp)) {
      return("Last updated: not yet")
    }
    
    paste(
      "Last updated:",
      format(
        timestamp,
        "%Y-%m-%d %H:%M:%S",
        tz = Sys.timezone()
      )
    )
  })
  
  output$refresh_status <- renderText({
    refresh_status()
  })
  
  # --------------------------------------------------------------
  # Available filter variables
  # --------------------------------------------------------------
  
  filter_variables <- reactive({
    
    dat <- sample_data()
    req(dat)
    
    setdiff(
      names(dat),
      excluded_filter_columns
    )
  })
  
  # --------------------------------------------------------------
  # Dynamically generate multiple filters
  # --------------------------------------------------------------
  
  output$filter_controls <- renderUI({
    
    dat <- sample_data()
    variables <- filter_variables()
    
    req(dat, variables)
    
    controls <- lapply(
      variables,
      function(variable) {
        
        values <- display_values(
          dat[[variable]]
        )
        
        selectizeInput(
          inputId = paste0(
            "filter_",
            make.names(variable)
          ),
          label = variable,
          choices = values,
          multiple = TRUE,
          options = list(
            placeholder = paste(
              "Select",
              variable
            ),
            maxOptions = 1000
          )
        )
      }
    )
    
    tagList(controls)
  })
  
  # --------------------------------------------------------------
  # Colour-variable dropdown
  # --------------------------------------------------------------
  
  observe({
    
    variables <- filter_variables()
    
    req(variables)
    
    preferred_variable <- if (
      "Sample_owner" %in% variables
    ) {
      "Sample_owner"
    } else {
      variables[1]
    }
    
    updateSelectInput(
      session = session,
      inputId = "colour_variable",
      choices = variables,
      selected = preferred_variable
    )
  })
  
  # --------------------------------------------------------------
  # Date slider limits
  # --------------------------------------------------------------
  
  observeEvent(sample_data(), {
    
    dat <- sample_data()
    req(dat)
    
    valid_dates <- dat$Sample_date[
      !is.na(dat$Sample_date)
    ]
    
    if (length(valid_dates) == 0) {
      return()
    }
    
    minimum_date <- min(valid_dates)
    maximum_date <- max(valid_dates)
    
    if (minimum_date == maximum_date) {
      minimum_date <- minimum_date - 1
      maximum_date <- maximum_date + 1
    }
    
    updateSliderInput(
      session = session,
      inputId = "date_range",
      min = minimum_date,
      max = maximum_date,
      value = c(
        minimum_date,
        maximum_date
      )
    )
  })
  
  # --------------------------------------------------------------
  # Latitude, longitude, and depth slider limits
  # --------------------------------------------------------------
  
  observeEvent(sample_data(), {
    
    dat <- sample_data()
    req(dat)
    
    # Latitude limits
    latitude_values <- valid_numeric_values(
      dat$Sample_lat
    )
    
    if (length(latitude_values) > 0) {
      
      minimum_latitude <- min(latitude_values)
      maximum_latitude <- max(latitude_values)
      
      if (minimum_latitude == maximum_latitude) {
        minimum_latitude <- minimum_latitude - 0.01
        maximum_latitude <- maximum_latitude + 0.01
      }
      
      updateSliderInput(
        session = session,
        inputId = "lat_range",
        min = minimum_latitude,
        max = maximum_latitude,
        value = c(
          minimum_latitude,
          maximum_latitude
        ),
        step = 0.000001
      )
    }
    
    # Longitude limits
    longitude_values <- valid_numeric_values(
      dat$Sample_lon
    )
    
    if (length(longitude_values) > 0) {
      
      minimum_longitude <- min(longitude_values)
      maximum_longitude <- max(longitude_values)
      
      if (minimum_longitude == maximum_longitude) {
        minimum_longitude <- minimum_longitude - 0.01
        maximum_longitude <- maximum_longitude + 0.01
      }
      
      updateSliderInput(
        session = session,
        inputId = "lon_range",
        min = minimum_longitude,
        max = maximum_longitude,
        value = c(
          minimum_longitude,
          maximum_longitude
        ),
        step = 0.000001
      )
    }
    
    # Depth limits
    if ("Sample_depth" %in% names(dat)) {
      
      depth_values <- valid_numeric_values(
        dat$Sample_depth
      )
      
      if (length(depth_values) > 0) {
        
        minimum_depth <- min(depth_values)
        maximum_depth <- max(depth_values)
        
        if (minimum_depth == maximum_depth) {
          minimum_depth <- max(
            0,
            minimum_depth - 1
          )
          maximum_depth <- maximum_depth + 1
        }
        
        depth_step <- if (
          all(depth_values == floor(depth_values))
        ) {
          1
        } else {
          0.01
        }
        
        updateSliderInput(
          session = session,
          inputId = "depth_range",
          min = minimum_depth,
          max = maximum_depth,
          value = c(
            minimum_depth,
            maximum_depth
          ),
          step = depth_step
        )
      }
    }
    
    # Specimen length limits
    if ("Specimen_length" %in% names(dat)) {
      
      length_values <- valid_numeric_values(
        dat$Specimen_length
      )
      
      if (length(length_values) > 0) {
        
        minimum_length <- min(length_values)
        maximum_length <- max(length_values)
        
        # A slider cannot have identical minimum and maximum values
        if (minimum_length == maximum_length) {
          minimum_length <- minimum_length - 0.01
          maximum_length <- maximum_length + 0.01
        }
        
        updateSliderInput(
          session = session,
          inputId = "length_range",
          min = minimum_length,
          max = maximum_length,
          value = c(
            minimum_length,
            maximum_length
          ),
          step = 0.01
        )
      }
    }
    
    # Specimen weight limits
    if ("Specimen_weight" %in% names(dat)) {
      
      weight_values <- valid_numeric_values(
        dat$Specimen_weight
      )
      
      if (length(weight_values) > 0) {
        
        minimum_weight <- min(weight_values)
        maximum_weight <- max(weight_values)
        
        # A slider cannot have identical minimum and maximum values
        if (minimum_weight == maximum_weight) {
          minimum_weight <- minimum_weight - 0.01
          maximum_weight <- maximum_weight + 0.01
        }
        
        updateSliderInput(
          session = session,
          inputId = "weight_range",
          min = minimum_weight,
          max = maximum_weight,
          value = c(
            minimum_weight,
            maximum_weight
          ),
          step = 0.01
        )
      }
    }
  })
  
  # --------------------------------------------------------------
  # Apply all filters
  # --------------------------------------------------------------
  
  filtered_data <- reactive({
    
    dat <- sample_data()
    req(dat)
    
    # Sample-name search
    search_text <- tolower(
      trimws(input$sample_search %||% "")
    )
    
    if (
      nzchar(search_text) &&
      "Sample_name" %in% names(dat)
    ) {
      
      sample_names <- tolower(
        as_clean_character(dat$Sample_name)
      )
      
      dat <- dat[
        grepl(
          search_text,
          sample_names,
          fixed = TRUE
        ),
        ,
        drop = FALSE
      ]
    }
    
    # Date range filter
    if (
      !is.null(input$date_range) &&
      length(input$date_range) == 2 &&
      "Sample_date" %in% names(dat)
    ) {
      
      date_values <- dat$Sample_date
      
      dat <- dat[
        is.na(date_values) |
          (
            date_values >= input$date_range[1] &
              date_values <= input$date_range[2]
          ),
        ,
        drop = FALSE
      ]
    }
    
    # Latitude range filter
    if (
      !is.null(input$lat_range) &&
      length(input$lat_range) == 2 &&
      "Sample_lat" %in% names(dat)
    ) {
      
      latitude_values <- dat$Sample_lat
      
      dat <- dat[
        is.na(latitude_values) |
          (
            latitude_values >= input$lat_range[1] &
              latitude_values <= input$lat_range[2]
          ),
        ,
        drop = FALSE
      ]
    }
    
    # Longitude range filter
    if (
      !is.null(input$lon_range) &&
      length(input$lon_range) == 2 &&
      "Sample_lon" %in% names(dat)
    ) {
      
      longitude_values <- dat$Sample_lon
      
      dat <- dat[
        is.na(longitude_values) |
          (
            longitude_values >= input$lon_range[1] &
              longitude_values <= input$lon_range[2]
          ),
        ,
        drop = FALSE
      ]
    }
    
    # Depth range filter
    if (
      !is.null(input$depth_range) &&
      length(input$depth_range) == 2 &&
      "Sample_depth" %in% names(dat)
    ) {
      
      depth_values <- dat$Sample_depth
      
      dat <- dat[
        is.na(depth_values) |
          (
            depth_values >= input$depth_range[1] &
              depth_values <= input$depth_range[2]
          ),
        ,
        drop = FALSE
      ]
    }
    
    # Specimen length range filter
    if (
      !is.null(input$length_range) &&
      length(input$length_range) == 2 &&
      "Specimen_length" %in% names(dat)
    ) {
      
      length_values <- dat$Specimen_length
      
      dat <- dat[
        is.na(length_values) |
          (
            length_values >= input$length_range[1] &
              length_values <= input$length_range[2]
          ),
        ,
        drop = FALSE
      ]
    }
    
    # Specimen weight range filter
    if (
      !is.null(input$weight_range) &&
      length(input$weight_range) == 2 &&
      "Specimen_weight" %in% names(dat)
    ) {
      
      weight_values <- dat$Specimen_weight
      
      dat <- dat[
        is.na(weight_values) |
          (
            weight_values >= input$weight_range[1] &
              weight_values <= input$weight_range[2]
          ),
        ,
        drop = FALSE
      ]
    }
    
    # Metadata filters
    variables <- filter_variables()
    
    for (variable in variables) {
      
      filter_id <- paste0(
        "filter_",
        make.names(variable)
      )
      
      selected_values <- input[[filter_id]]
      
      if (
        !is.null(selected_values) &&
        length(selected_values) > 0
      ) {
        
        values_for_comparison <- display_values_per_row(
          dat[[variable]]
        )
        
        dat <- dat[
          values_for_comparison %in% selected_values,
          ,
          drop = FALSE
        ]
      }
    }
    
    dat
  })
  
  # --------------------------------------------------------------
  # Summary
  # --------------------------------------------------------------
  
  output$number_of_samples <- renderText({
    
    dat <- filtered_data()
    
    paste(
      "Samples shown:",
      nrow(dat)
    )
  })
  
  output$summary_table <- renderTable({
    
    dat <- filtered_data()
    req(dat)
    
    count_unique <- function(column_name) {
      
      if (!column_name %in% names(dat)) {
        return(NA_integer_)
      }
      
      length(unique(
        dat[[column_name]][
          !is.na(dat[[column_name]]) &
            as_clean_character(dat[[column_name]]) != ""
        ]
      ))
    }
    
    data.frame(
      Measure = c(
        "Samples",
        "Owners",
        "Species",
        "Treatments",
        "Years"
      ),
      Value = c(
        nrow(dat),
        count_unique("Sample_owner"),
        count_unique("Species"),
        count_unique("Sample_treatment"),
        count_unique("Sample_year")
      ),
      check.names = FALSE
    )
  })
  
  # --------------------------------------------------------------
  # Map
  # --------------------------------------------------------------
  
  output$sample_map <- renderLeaflet({
    
    dat <- filtered_data()
    req(dat)
    
    base_map <- leaflet() %>%
      addProviderTiles(
        providers$Esri.WorldTopoMap
      )
    
    if (nrow(dat) == 0) {
      
      return(
        base_map %>%
          addControl(
            "No samples match the selected filters.",
            position = "topright"
          )
      )
    }
    
    colour_variable <- input$colour_variable
    
    if (
      is.null(colour_variable) ||
      !colour_variable %in% names(dat)
    ) {
      colour_variable <- "Sample_owner"
    }
    
    dat$colour_value <- display_values_per_row(
      dat[[colour_variable]]
    )
    
    # Create layers based on Sample_status
    if ("Sample_status" %in% names(dat)) {
      
      dat$status_layer <- display_values_per_row(
        dat$Sample_status
      )
      
    } else {
      
      dat$status_layer <- rep(
        "Samples",
        nrow(dat)
      )
    }
    
    colour_levels <- unique(
      dat$colour_value
    )
    
    point_palette <- colorFactor(
      palette = viridis(
        max(1, length(colour_levels))
      ),
      domain = colour_levels
    )
    
    dat$popup_text <- vapply(
      seq_len(nrow(dat)),
      function(i) {
        
        popup_data <- dat[
          i,
          setdiff(
            names(dat),
            c(
              "colour_value",
              "status_layer",
              "popup_text"
            )
          ),
          drop = FALSE
        ]
        
        make_popup(popup_data)
      },
      character(1)
    )
    
    status_levels <- unique(
      dat$status_layer
    )
    
    map <- base_map
    
    # Add one map layer per Sample_status
    for (status in status_levels) {
      
      status_data <- dat[
        dat$status_layer == status,
        ,
        drop = FALSE
      ]
      
      marker_arguments <- list(
        map = map,
        data = status_data,
        lng = ~Sample_lon,
        lat = ~Sample_lat,
        radius = 7,
        stroke = TRUE,
        weight = 1,
        color = ~point_palette(colour_value),
        fillColor = ~point_palette(colour_value),
        fillOpacity = 0.8,
        popup = ~popup_text,
        group = status
      )
      
      if (isTRUE(input$show_labels)) {
        marker_arguments$label <-
          ~as.character(Sample_name)
      }
      
      if (isTRUE(input$enable_clustering)) {
        marker_arguments$clusterOptions <-
          markerClusterOptions()
      }
      
      map <- do.call(
        addCircleMarkers,
        marker_arguments
      )
    }
    
    # Fit map to the selected samples
    min_lat <- min(
      dat$Sample_lat,
      na.rm = TRUE
    )
    
    max_lat <- max(
      dat$Sample_lat,
      na.rm = TRUE
    )
    
    min_lon <- min(
      dat$Sample_lon,
      na.rm = TRUE
    )
    
    max_lon <- max(
      dat$Sample_lon,
      na.rm = TRUE
    )
    
    # Add a small margin so points are not exactly at the edge
    lat_margin <- max(
      0.01,
      (max_lat - min_lat) * 0.05
    )
    
    lon_margin <- max(
      0.01,
      (max_lon - min_lon) * 0.05
    )
    
    map <- map %>%
      fitBounds(
        lng1 = min_lon - lon_margin,
        lat1 = min_lat - lat_margin,
        lng2 = max_lon + lon_margin,
        lat2 = max_lat + lat_margin
      ) %>%
      addLegend(
        position = "bottomright",
        pal = point_palette,
        values = dat$colour_value,
        title = colour_variable,
        opacity = 1
      )
    
    # Add the Sample_status tick-box control
    if (length(status_levels) > 1) {
      
      map <- map %>%
        addLayersControl(
          overlayGroups = status_levels,
          options = layersControlOptions(
            collapsed = FALSE
          )
        )
    }
    
    map
  })
  
  # --------------------------------------------------------------
  # Data table
  # --------------------------------------------------------------
  
  output$sample_table <- renderDT(
    {
      dat <- filtered_data()
      req(dat)
      
      dat_for_display <- dat
      
      if ("Sample_date" %in% names(dat_for_display)) {
        dat_for_display$Sample_date <- format(
          dat_for_display$Sample_date,
          "%Y-%m-%d"
        )
      }
      
      datatable(
        dat_for_display,
        rownames = FALSE,
        filter = "top",
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "Bfrtip",
          
          buttons = list(
            list(
              extend = "copy",
              exportOptions = list(
                modifier = list(
                  page = "all",
                  search = "applied"
                )
              )
            ),
            list(
              extend = "csv",
              exportOptions = list(
                modifier = list(
                  page = "all",
                  search = "applied"
                )
              )
            ),
            list(
              extend = "excel",
              exportOptions = list(
                modifier = list(
                  page = "all",
                  search = "applied"
                )
              )
            )
          )
        )
      )
    },
    server = FALSE
  )

  # --------------------------------------------------------------
  # Download filtered data
  # --------------------------------------------------------------
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "filtered_samples_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      dat <- filtered_data()
      req(dat)
      
      write.csv(
        dat,
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
}

# ================================================================
# Run the application
# ================================================================

shiny::runApp(
  shiny::shinyApp(
    ui = ui,
    server = server
  )
)
