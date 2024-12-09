#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

dat <- readr::read_csv("ACRN_Database_old.csv") %>%
  janitor::clean_names() %>%
  dplyr::select(-c("publication_url", "project_url")) %>%
  dplyr::mutate(latitude = round(latitude, 2),
                longitude = round(longitude, 2))



# Define UI for application that draws a histogram
ui <- fluidPage(title = "Ana Amazing App (AAA)", # Application title
                theme = bslib::bs_theme(
                  version = 5,
                  bootswatch = "flatly"),
                shiny::h1("Ana's App"),
                shiny::br(),
                shiny::br(),
                shiny::fluidRow(
                  shiny::column(width = 4,
                                shiny::selectizeInput(inputId = "habitat",
                                                      label = "Habitat",
                                                      selected = "Mangrove",
                                                      choices = unique(dat$habitat_type),
                                                      width = "100%",
                                                      options = list(dropdownParent = 'body'),
                                                      multiple = TRUE
                                ), # selectizeInput
                  ), # column
                  shiny::column(width = 4,
                                shiny::selectizeInput(inputId = "country",
                                                      label = "Country",
                                                      selected = "Australia",
                                                      choices = unique(dat$country),
                                                      width = "100%",
                                                      options = list(dropdownParent = 'body'),
                                                      multiple = TRUE,

                                ), # selectizeInput
                  ), # column
                ), # fluidRow


                shiny::tabsetPanel(
                  shiny::tabPanel(title = "Map",
                                  shiny::fluidRow(
                                    leaflet::leafletOutput("dat_map", width = "100%", height = "800px"),
                                  ),
                  ),
                  shiny::tabPanel(title = "Data",
                                  shiny::fluidRow(
                                    shiny::br(),
                                    DT::DTOutput("dat_table"), # DTOutput
                                  ), # fluidRow
                  ) # tabpanel

                ) # navbarPage

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## TAB 1 ------------

  datr <- reactive({

    dat %>%
      dplyr::filter(habitat_type %in% input$habitat &
                      country %in% input$country)

  }) #%>% bindCache(input$species)

  # Render basemap
  output$dat_map <- leaflet::renderLeaflet({


    leaflet::leaflet(dat %>%
                       dplyr::distinct(latitude, longitude)) %>%
      leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>%
      # leaflet::setMaxBounds(~110, ~-45, ~160, ~-10) %>%
      leaflet::addCircleMarkers(lng = ~ longitude,
                                lat = ~ latitude,
                                color = "grey",
                                opacity = 1,
                                fillOpacity = 1,
                                radius = 2)

  })


  # Add points for chosen larval fish
  shiny::observe({
    labs_dat <- lapply(seq(nrow(datr())), function(i) {
      paste("<strong>Latitude:</strong>", datr()$latitude[i], "<br>",
            "<strong>Longitude:</strong>", datr()$longitude[i], "<br>",
            "<strong>Location:</strong>", datr()$location[i], "<br>",
            "<strong>Species:</strong>", datr()$species[i], "<br>"
      )})

    leaflet::leafletProxy("dat_map", data = datr) %>%
      # leaflet::setMaxBounds(~110, ~-45, ~160, ~-10) %>%
      leaflet::clearGroup("Present") %>%
      leaflet::addCircleMarkers(data = datr(),
                                lng = ~ longitude,
                                lat = ~ latitude,
                                color = "blue",
                                opacity = 1,
                                fillOpacity = 1,
                                radius = 5,
                                group = "Present",
                                label = lapply(labs_dat, htmltools::HTML))
  })





  ## TAB 2 ------------

  output$dat_table <- DT::renderDT(
    datr(),
    filter = "top",
    options = list(
      pageLength = 100))  # renderDT


  # Download -------------------------------------------------------
  # output$downloadData1 <- fDownloadButtonServer(input, dat_tabler,
  #                                               stringr::str_remove_all(input$species, ":|\\(|\\)") %>%
  #                                                 stringr::str_replace_all(" ", "_")) # Download csv of data

}

# Run the application
shinyApp(ui = ui, server = server)
