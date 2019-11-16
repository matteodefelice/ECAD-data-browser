library(shiny)
library(leaflet)
library(tidyverse)
library(highcharter)
read_station_data <- function(filename) {
  nskips = 20
  if (str_sub(filename, 1,2) == 'RR') {
    nskips = 21
  }
  
  stn_data <- read_csv(filename, skip = nskips, 
                       col_types = cols(STAID = col_character(),
                                        SOUID = col_character(),
                                        DATE = col_date(format = "%Y%m%d"))) %>% 
    select(-starts_with('Q_')) %>%
    gather(variable, value, -STAID, -SOUID, -DATE) %>%
    filter(value > -9999)
  return(stn_data)
}

### USER INTERFACE ------------------------------------------------------
ui <- fluidPage(
  titlePanel("European Climate Assessment & Dataset (ECA&D) stations metadata browser"),
  tags$head(
    # Include custom CSS
    includeCSS("styles.css")
  ),

  fluidRow(
    column(
      3,
      p("Browse the available stations in the Blended ECA Dataset. Click on each marker to see how to access the data."),
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
          ),
          selected = "TG"
        ),
        actionButton("selectall", label = "Select all")
      )
    ),
    column(
      9,
      leafletOutput("stations_map", height = "600px"),
      highchartOutput("hcontainer", height = "600px"),
      # textOutput('temp'),
      wellPanel(
        tags$p(paste0(
          "Data from the European Climate Assessment & Dataset project. ",
          "Updated August 2018. "
        ), a("Data description & policy", href = "https://www.ecad.eu//dailydata/index.php")),
        tags$p("Developed by ", 
               a("Matteo De Felice", href = "http://matteodefelice.name"),
               "(the author is not involved in the ECA&D project).",
               "The code is ", 
               a("available on Github", href = "https://github.com/matteodefelice/ECAD-data-browser"),
               "with",
               a("CC BY 4.0", href = "https://creativecommons.org/licenses/by/4.0/"),
               "license"
               )
      )
    )
  )
)

### Server side ------------------------------------------------------
server <- function(input, output, session) {
  # Load the stations metadata saved as a serialised object 
  eobs_processed <- read_rds("eobs-database-stations.rds")
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
        popup = ~paste0("<h4>", STANAME , " (", CN, ")</h2><br/>",
                       "Station id: ", STAID, "<br/>",
                       "Station height: ", HGHT, "<br/>",
                       "Variable: ", base_ELEI, "<br/>",
                       "Number of sources used for blending: ", n_sources, "<br/>",
                       "Time-series range from ", START, " to ", STOP, "<br/>",
                       "Filename: <code>", filename, "</code>",
                       ' available downloading <code>ECA_blended_', tolower(base_ELEI),
                       '.zip</code> from <a href="https://www.ecad.eu/dailydata/predefinedseries.php">this page</a><br/>'
            ),
        popupOptions = popupOptions(textsize = "15px", opacity = 0.7),
        label = ~as.character(paste(STANAME, '(', STAID, ')', 
                                    base_ELEI)),
        labelOptions = labelOptions(textsize = "15px", opacity = 0.7),
        clusterOptions = markerClusterOptions()
      ) 
  })
  #Time-series
  output$hcontainer <- renderHighchart({
    if (!is.null(input$stations_map_marker_click)) {
      toplot = read_station_data(paste0('data/', 
                                        input$stations_map_marker_click$id)) %>%
        select(date = DATE, value) 
      
      toplot_xts = xts::xts(x = toplot$value, order.by = toplot$date)
      # hc = hchart(toplot, 'line', hcaes(x = DATE, y = value),
      #             range = 10
      #             ) %>%
      #   hc_chart(zoomType = 'x')
      hc = highchart(type = "stock") %>%
        hc_add_series(data = toplot_xts,type = 'line')
      
      hc
    }
    
    
  })

  
  # output$temp <- renderText({
  #   if (!is.null(input$stations_map_marker_click)) {
  #     
  #     input$stations_map_marker_click$id
  #   }
  # })
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
