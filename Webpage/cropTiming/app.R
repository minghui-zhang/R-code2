library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(raster)
library(leafpop)
library(ggplot2)

# TUTORIAL: https://rstudio.github.io/leaflet/shiny.html

df <- readRDS("./cell_spdf.rds")
MT_outline <- readOGR(dsn = './MatoGrossoOutline', layer = 'MatoGrossoOutline')
crs(MT_outline) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# years, for drop-downs
vars <- c(
  "2014" = 2014,
  "2013" = 2013,
  "2012" = 2012,
  "2011" = 2011,
  "2010" = 2010,
  "2009" = 2009,
  "2008" = 2008,
  "2007" = 2007,
  "2006" = 2006,
  "2005" = 2005,
  "2004" = 2004
)

ui <- navbarPage("Crop Timing", id = "nav",
           
  # map tab -------------------------------------------------------------------------------------      
  tabPanel("Interactive Map",
           
    # side bar
    sidebarLayout(
      sidebarPanel(
      radioButtons("user_intensity", "Cropping intensity:",
                               c("Single Cropping" = "SC",
                                 "Double Cropping" = "DC")),
                  
      selectInput("user_year", "Year:", vars)
      ),
    
    # main bar
    mainPanel(
      leafletOutput("mymap",height = 1000)
    )
    
    ) # end sidebarLayout
  ), # end interactive map tab
  
  # how data created tab -----------------------------------------------------------------------
  tabPanel("How this data was created")
)

server <- function(input, output) {
  
  filteredData <- reactive({
    df[df$intensity == input$user_intensity & df$year == input$user_year,]
  })
  
  filteredData_allYears <- reactive({
    df[df$intensity == input$user_intensity,]

  })
  

  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = df$plant
  )
  
  output$mymap <- renderLeaflet({
    
    leaflet(data = MT_outline) %>%
      #addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = MT_outline, weight = 2, stroke = TRUE, color = "black", fillColor = "white")
  })
  
  # observeEvent(input$mymap_shape_click, { # update the location selectInput on map clicks
  #   point_lat <- input$mymap_shape_click$lat
  #   point_lon <- input$mymap_shape_click$lon
  #   point_chosen <- SpatialPoints(matrix(c(point_lon,point_lat), nrow = 1))
  #   crs(point_chosen) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #   
  #   filteredData_allYears_cell <- reactive({
  #     over(point_chosen, filteredData_allYears())
  #   })
  # 
  #   print(filteredData_allYears_cell())
  # })

  
  
  observe({

    graph_popup <- function(cell_ID_value) {
      print('cell_ID_value')
      print(cell_ID_value)
      cell_df <- as.data.frame(filteredData_allYears())[filteredData_allYears()$cell_ID == cell_ID_value,]
      print(cell_df)
      return(
        ggplot(cell_df, aes(x = year, y = plant)) +
                            geom_point(size = 1) +
                            theme_bw()
      )
    }
    
    #FOLLOW THIS https://stackoverflow.com/questions/32352539/plotting-barchart-in-popup-using-leaflet-library
    
    leafletProxy("mymap", data = filteredData()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = MT_outline, weight = 2, stroke = TRUE, color = "black", fillColor = "white") %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0, fillOpacity = 1,
                  fillColor = ~pal(plant),
                  #fillColor = ~colorQuantile("YlOrRd", plant)(plant),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = paste("Plant: ", round(df$plant), " days after Aug 1 <br>", "Onset: ", df$onset, "days after Aug 1")
                  #popup = (cell_ID <- df$cell_ID) #popupGraph(graph_popup(df$cell_ID))
      ) %>%
      addLegend("bottomright", pal = pal, values = ~plant,
                title = "Planting date (DOY after Aug 1)",
                opacity = 1
      ) 
    # dist <- 0.5
    # lng <- data()$long
    # lat <- data()$lat
    # 
    # leafletProxy("map") %>%
    #    fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })

  
}

shinyApp(ui = ui, server = server)