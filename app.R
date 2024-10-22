library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(stringr)
library(xts)
library(dygraphs)
library(glue)
library(lubridate)

units <- list(
  CC = list(name = "Cloud cover", unit = "Oktas", scale = 1),
  WD = list(name = "Wind Direction", unit = "Degrees", scale = 1),
  FG = list(name = "Wind Speed", unit = "Speed (m/s)", scale = 10),
  FX = list(name = "Wind Gust", unit = "Speed (m/s)", scale = 10),
  HU = list(name = "Humidity", unit = "%", scale = 1),
  PP = list(name = "Pressure", unit = "hPa", scale = 10),
  RR = list(name = "Precipitation", unit = "Millimeters (mm)", scale = 10),
  SD = list(name = "Snow Depth", unit = "Centimeters (cm)", scale = 1),
  SS = list(name = "Sunshine", unit = "Hours", scale = 10),
  TG = list(name = "Mean Temperature", unit = "Celsius Degrees (C)", scale = 10),
  TN = list(name = "Min Temperature", unit = "Celsius Degrees (C)", scale = 10),
  TX = list(name = "Max Temperature", unit = "Celsius Degrees (C)", scale = 10),
  QQ = list(name = "Solar radiation", unit = "W/m2", scale = 1)
)

# Function used to read local data 
read_station_data <- function(filename) {
  nskips <- 20
  if (str_sub(filename, 1, 2) == "RR") {
    nskips <- 21
  }

  stn_data <- read_csv(filename,
    skip = nskips,
    col_types = cols(
      STAID = col_character(),
      SOUID = col_character(),
      DATE = col_date(format = "%Y%m%d")
    )
  ) %>%
    select(-starts_with("Q_")) %>%
    gather(variable, value, -STAID, -SOUID, -DATE) %>%
    dplyr::filter(value > -9999)
  return(stn_data)
}

### USER INTERFACE ------------------------------------------------------
ui <- fluidPage(
  titlePanel("European Climate Assessment & Dataset (ECA&D) stations data browser"),
  tags$head(
    # Include custom CSS
    includeCSS("styles.css")
  ),

  fluidRow(
    column(
      3,
      p("Browse the available stations in the Blended ECA Dataset. Click on each marker to see the time-series and the metadata."),
      tags$p(
        "Data from the European Climate Assessment & Dataset project. ",
        "Processed data retrieved from ",
        a("KNMI Climate Explorer", href = "https://climexp.knmi.nl/start.cgi"),
        # "Time-series available until July 2020.",
        a("Data description & policy", href = "https://www.ecad.eu//dailydata/index.php")
      ),
      tags$p(
        "Developed by ",
        a("Matteo De Felice", href = "http://matteodefelice.name"),
        "(the author is not involved in the ECA&D project).",
        "The code is ",
        a("available on Github", href = "https://github.com/matteodefelice/ECAD-data-browser"),
        "Thanks to Geert Jan van Oldenburgh and the KNMI (Koninklijk Nederlands Meteorologisch Instituut) for the data access"
      ),
      wellPanel(
        h4("Filter"),
        # Select the minimum number of years
        sliderInput("years", "Minimum number of years",
          0, 80, 10,
          step = 10
        ),
        # choose betwee the 'any' and the 'all' condition
        radioButtons("any_all",
          label = "Filter all the stations with",
          choices = c("any", "all"), inline = TRUE
        ),
        # Choose the variables
        checkboxGroupInput("elems",
          label = "the following elements:",
          choices = c(
            "Cloud cover (CC)" = "CC", # climexp
            # "Wind Direction (DD)" = "DD",
            "Wind Speed (FG)" = "FG",
            "Wind Gust (FX)" = "FX",
            # "Humidity (HU)" = "HU",
            "Pressure (PP)" = "PP", # climexp
            "Precipitation (RR)" = "RR", # climexp
            "Snow Depth (SD)" = "SD", # climexp
            # "Sunshine (SS)" = "SS",
            "Mean Temperature (TG)" = "TG", # climexp
            "Min Temperature (TN)" = "TN", # climexp
            "Max Temperature (TX)" = "TX", # climexp
            "Solar Radiation (QQ)" = "QQ"
          ),
          selected = "TG"
        ),
        actionButton("selectall", label = "Select all")
      )
    ),
    column(
      9,
      leafletOutput("stations_map", height = "300px"),
      tags$p("Click on a marker to show daily time-series data."),
      tags$p("The horizontal lines are the 5th and 95th percentiles for the entire time-series. ",
        "Change the number in the bottom left to apply a rolling average (default one day, i.e. no average)"
        ),
      dygraphOutput("dyts", height = "400px")
    )
  )
)

### Server side ------------------------------------------------------
server <- function(input, output, session) {
  # Load the stations metadata saved as a serialised object
  eobs_processed <- read_rds("eobs-database-stations.rds") %>%
    ungroup()
  # Return the list of selected stations
  selected_stations <- reactive({
    r <- subset(eobs_processed, years_length >= input$years) %>%
      dplyr::filter(base_ELEI %in% input$elems)
    if (input$any_all == "all") {
      r <- r %>%
        group_by(STAID) %>%
        mutate(n = n()) %>%
        dplyr::filter(n == length(input$elems)) %>%
        ungroup()
    }
    return(r)
  })
  # Render the map
  output$stations_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 10, lat = 50, zoom = 4) %>%
      addMarkers(
        data = selected_stations(),
        layerId = ~filename,
        ~lon_dec, ~lat_dec,
        popup = ~ paste0(
          "<h4>", STANAME, " (", CN, ")</h2><br/>",
          "Station id: ", STAID, "<br/>",
          "Station height: ", HGHT, "<br/>",
          "Variable: ", base_ELEI, "<br/>",
          "Number of sources used for blending: ", n_sources, "<br/>",
          "Time-series range from ", START, " to ", STOP, "<br/>",
          "Filename: <code>", filename, "</code>",
          " available downloading <code>ECA_blended_", tolower(base_ELEI),
          '.zip</code> from <a href="https://www.ecad.eu/dailydata/predefinedseries.php">this page</a><br/>'
        ),
        popupOptions = popupOptions(textsize = "15px", opacity = 0.7),
        label = ~ as.character(paste(
          STANAME, "(", STAID, ")",
          base_ELEI
        )),
        labelOptions = labelOptions(textsize = "15px", opacity = 0.7),
        clusterOptions = markerClusterOptions()
      )
  })
  # Time-series

  output$dyts <- renderDygraph({
    if (!is.null(input$stations_map_marker_click)) {
      # Code used to read static files in 'data/'
      # toplot <- read_station_data(paste0(
      #   "data/",
      #   input$stations_map_marker_click$id
      # )) %>%
      #   select(date = DATE, value)

      VAR <- str_sub(input$stations_map_marker_click$id, 1, 2) %>%
        str_to_lower()
      ID <- str_sub(input$stations_map_marker_click$id, 9, 14)
      URL <- glue("https://climexp.knmi.nl/ECAData/data/b{VAR}{ID}.dat.gz")

      toplot <- read_table(URL, skip = 1, col_names = c("year", "month", "day", "value")) %>%
        mutate(
          date = make_date(year, month, day)
        ) %>%
        select(
          date, value
        )

      # Load unit measure
      this_unit <- units[[str_sub(input$stations_map_marker_click$id, 1, 2)]]

      # Scale value (ONLY FOR FILES)
      # toplot$value <- toplot$value / this_unit$scale

      # Get name
      sel_stn <- eobs_processed %>%
        dplyr::filter(filename == input$stations_map_marker_click$id) %>%
        select(STANAME, CN)
      # Create XTS
      toplot_xts <- xts::xts(x = toplot$value, order.by = toplot$date)

      dy <- dygraph(toplot_xts,
        main = paste0(this_unit$name, " - ", sel_stn$STANAME, " (", sel_stn$CN, ")"),
        ylab = this_unit$unit
      ) %>%
        dyRangeSelector(dateWindow = (toplot %>% tail(365) %>% pull(date))[c(1, 365)]) %>%
        dyRoller() %>%
        dyLimit(quantile(toplot$value, 0.05, na.rm = TRUE) %>% as.numeric(), color = "blue") %>%
        dyLimit(quantile(toplot$value, 0.95, na.rm = TRUE) %>% as.numeric(), color = "red")
      dy
    }
  })

  # Observe part
  observe({
    if (input$selectall > 0) {
      opts <- list(
        "Cloud cover (CC)" = "CC",
        "Wind Direction (DD)" = "DD",
        "Wind Speed (FG)" = "FG",
        "Wind Gust (FX)" = "FX",
        "Humidity (HU)" = "HU",
        "Pressure (PP)" = "PP",
        "Precipitation (RR)" = "RR",
        "Snow Depth (SD)" = "SD",
        "Sunshine (SS)" = "SS",
        "Mean Temperature (TG)" = "TG",
        "Min Temperature (TN)" = "TN",
        "Max Temperature (TX)" = "TX"
      )
      updateCheckboxGroupInput(
        session = session,
        inputId = "elems", label = "the following elements:",
        choices = opts,
        selected = unlist(opts)
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
