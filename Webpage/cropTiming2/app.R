library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(raster)
library(leafpop)
library(ggplot2)
library(spatialEco)

# TUTORIAL: https://rstudio.github.io/leaflet/shiny.html

df <- readRDS("./cell_spdf_plant_harvest.rds")
df <- df[df$plant_stat_type == "median", ] # show median planting date only
to_download_csv <- as.data.frame(df) %>%
  dplyr::select(-c("SC_delay", "DC_area_km", "label", "Muni_code", "SC_area_km",
            "onset_rang", "DC_delay", "plant_stat_type", "cell_ID",  
            "year_index", "year_factor", "plantYear", "date_monthday", "onset_monthday", "date", "onset")) %>%
  dplyr::rename(soy_area = soy_area_k,
                crop_date = date_formatted,
                onset_date = onset_date_formatted)

# writes out a shp file for download
#writeOGR(obj=df, dsn="cell_data", layer="cell_data", driver="ESRI Shapefile")

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

documentation_table <- data.frame(
  Variable = c('year', 'crop_date','plant_or_harv','onset_date', 'intensity', 'soy_area', 'lon', 'lat'),
  Description = c("Year of harvest (2014 refers to Aug 1, 2013 to July 31, 2014)",
                  "Date (Planting or harvest date)",
                  "Indicator for whether the date is planting or harvest date",
                  "Wet season onset date",
                  "Cropping intensity (SC = single cropped, DC = double cropped)",
                  "Area of soy [square km]",
                  "Longitude of cell center",
                  "Latitude of cell center")
)

###################################################################################################
# UI
###################################################################################################

ui <- navbarPage("Crop Timing", id = "nav",
           
  # # intro tab -----------------------------------------------------------------------------------
  # tabPanel("Welcome",
  #   
  #     h1("This is a dataset of soybean planting dates estimated over Mato Grosso, Brazil from 2004 to 2014."),
  #     
  #     h2("Click on 'Interactive Map' to explore the data."),
  #     
  #     h2("Please see [link to paper and citation] for details on how this dataset was created."),
  #     
  #     h3("To cite this dataset, [citation for paper]")
  #     
  # ), # end intro tab
  
  
  # map tab -------------------------------------------------------------------------------------      
  tabPanel("Interactive Map",
           
    # side bar
    sidebarLayout(
      sidebarPanel(

      radioButtons("user_plant_harv", "Planting or harvest date:",
                     c("Planting date" = "plant",
                       "Harvest date" = "harvest")),
        
      radioButtons("user_intensity", "Cropping intensity:",
                               c("Single Cropping" = "SC",
                                 "Double Cropping" = "DC")),
                  
      #selectInput("user_year", "Year:", vars)
      sliderInput(inputId = "user_year", 
                    label = "Year",
                    value = 2014, min = 2004, max = 2014, 
      	            step = 1, round = TRUE, sep = '', ticks = FALSE),
      
      h6("Click a cell to view its planting and wet season onset trajectory."),
      
      leafletOutput("mymap",height = 1000),

      width = 7
      ),
    
    # main bar
    mainPanel(
      plotOutput("timeseries"),
      plotOutput("histogram_SC"),
      plotOutput("histogram_DC"),
      width = 5
    )
    
    ) # end sidebarLayout
  ), # end interactive map tab
  
  # how data created tab -----------------------------------------------------------------------
  tabPanel("Download Data",
           h3('Click the link below to download planting and harvest date estimates for 2004-2014 across Mato Grosso, Brazil.'),
           tableOutput('documentation'),
           downloadLink('data_csv', 'Download as CSV'),
           h3('Please cite the dataset as: [...]')
           #,downloadLink('data_shp', 'Download as SHP')
           )
)

###################################################################################################
# SERVER
###################################################################################################

#' Title
#'
#' @param input 
#' @param output 
#'
#' @return
#' @export
#'
#' @examples
server <- function(input, output) {
  
  filteredData <- reactive({
    df[df$intensity == input$user_intensity & df$year == input$user_year & df$plant_or_harv == input$user_plant_harv,]
  })
  
  filteredData_userYearIntensity_plant <- reactive({
    df[df$intensity == input$user_intensity & df$year == input$user_year & df$plant_or_harv == "plant",]
  })
  
  filteredData_userYearIntensity_harvest <- reactive({
    df[df$intensity == input$user_intensity & df$year == input$user_year & df$plant_or_harv == "harvest",]
  })
  
  filteredData_allYears <- reactive({
    df[df$intensity == input$user_intensity,]
  })
  
  filteredData_SC_userYear <- reactive({
    df[df$intensity == "SC" & df$year == input$user_year,]
  })
  
  filteredData_DC_userYear <- reactive({
    df[df$intensity == "DC" & df$year == input$user_year,]
  })

  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = df$date
  )
  
  output$mymap <- renderLeaflet({

    leaflet(data = MT_outline) %>%
      #addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(minZoom = 4, maxZoom = 8)) %>%
      addPolygons(data = MT_outline, weight = 2, stroke = TRUE, color = "black", fillColor = "white")
    
  })
  
  # observeEvent for slider change: this is repeat of the observeEvent for clicking a map but with dummy point
  # if don't have this, when only change the user_year with slider, the plot will try to update but it 
  # won't have the new year. if a cell that existed in 2014 but doesn't exist in 2004 is chosen, get error in plot
  observeEvent(input$user_year, {
    #print('slider changed')
    
    point_lat <- max(c(-80,input$mymap_shape_click$lat), na.rm = TRUE) # dummy point
    point_lon <- max(c(-80,input$mymap_shape_click$lng), na.rm = TRUE)
    
    
    point_chosen <- SpatialPoints(matrix(c(point_lon,point_lat), nrow = 1))
    crs(point_chosen) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    filteredData_allYears_cell <- reactive({
      over(point_chosen, df, returnList = TRUE) #filteredData_allYears() if want one intensity; use df if want both intensities
    })
    
    #print(filteredData_allYears_cell()$"1")
    
    cell_data <- as.data.frame(filteredData_allYears_cell()$"1")
    #print('cell_data for all years')
    #print(cell_data)
    
    if (nrow(cell_data) >= 1) {
      output$timeseries <- renderPlot({
        ggplot(cell_data, mapping = aes(x = year)) +
          geom_line(mapping = aes(y = date_monthday, col = intensity, linetype = plant_or_harv), size = 1) +
          xlab("Year") +
          ylab("Date") +
          geom_line(mapping = aes(y = onset_monthday, color = "zonset")) +
          scale_color_manual(name = "", 
                             labels = c("Double cropped", "Single cropped", "Wet season onset"),
                             values = c("DC" = "darkgreen", "SC" = "green", "zonset" = "darkblue")) +
          scale_linetype_manual(name = "", labels = c("Harvest date", "Planting date"),
                                values = c("dotdash", "solid")) +
          scale_y_date(date_labels = "%m-%d") +
          labs(linetype = "") +
          ggtitle("Timeseries at chosen cell") +
          theme_bw()
        
      })
    }
    
    if (nrow(cell_data) == 0) {
      output$timeseries <- renderPlot({
        ggplot(as.data.frame(filteredData_allYears()), mapping = aes(x = year)) +
          geom_point(mapping = aes(y = date_monthday, col = plant_or_harv)) +
          xlab("Year") +
          ylab("Date") +
          scale_color_manual(name = "", 
                             labels = c("Harvest date", "Planting date"),
                             values = c("plant" = "darkgreen", "harvest" = "darkred")) +
          scale_y_date(date_labels = "%m-%d") +
          labs(shape = "") +
          ggtitle("Invalid cell chosen. Showing timeseries at all cells") +
          theme_bw()
      })
    }
    
    #print(as.data.frame(filteredData_SC_userYear())$plant)
    
    # histogram for ONE year, one histogram per intensity
    # if cell exists in chosen year
    if (nrow(cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC",]) >= 1) {
      
      output$histogram_SC <- renderPlot({
        ggplot(as.data.frame(filteredData_SC_userYear()), aes(x = date_formatted, fill = plant_or_harv)) +
          geom_histogram(alpha = 0.3) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkgreen", size = 1) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkred", size = 1) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkblue", size = 1) +
          
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "date_formatted"],
          #            col = "darkgreen", size = 2) + # get plant date for specific year +
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
          #            col = "darkred", size = 2) + # get harvest date for specific year +
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
          #            col = "darkblue", size = 2) + # get onset for specific year
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "date_formatted"] + 5, y=120,
                    label="SC \n planting \n date", color = "darkgreen") +
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "harvest", "date_formatted"], y=120,
                    label="SC \n harvest \n date", color = "darkred") +
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"] , y=120,
                    label="Wet \n season \n onset", color = "darkblue") +
          ggtitle(paste("Single cropped for", input$user_year)) +
          xlab("Date") +
          ylab("Histogram") +
          scale_x_date(date_breaks="3 month", limits = as.Date(c(paste0(input$user_year - 1, '-08-01'),paste0(input$user_year, '-07-31')))) +
          scale_fill_manual(values=c("red", "darkgreen")) +
          theme_bw() +
          labs(fill = "") +
          ylim(0, 130)
      })
    }
    
    # if cell doesn't exist in chosen year
    #print('user year')
    #user_year <- reactive(input$user_year)
    #print(input$user_year)
    #print('new cell_data to check')
    #print(cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC",])
    
    if (nrow(cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC",]) == 0) {
      
      
      output$histogram_SC <- renderPlot({
        ggplot(as.data.frame(filteredData_SC_userYear()), aes(x = date_formatted, fill = plant_or_harv)) +
          geom_histogram(alpha = 0.5) +
          ggtitle(paste("Single cropped for", input$user_year)) +
          xlab("Date") +
          ylab("Histogram") +
          scale_fill_manual(values=c("red", "darkgreen")) +
          theme_bw() +
          labs(fill = "") +
          scale_x_date(date_breaks="3 month", limits = as.Date(c(paste0(input$user_year - 1, '-08-01'),paste0(input$user_year, '-07-31')))) +
          ylim(0, 130)
      })
    }
    
    # if cell exists in chosen year
    if (nrow(cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC",]) >= 1) {
      output$histogram_DC <- renderPlot({
        ggplot(as.data.frame(filteredData_DC_userYear()), aes(x = date_formatted, fill = plant_or_harv)) +
          geom_histogram(alpha = 0.3) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkgreen", size = 1) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkred", size = 1) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkblue", size = 1) +
          
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "date_formatted"],
          #            col = "darkgreen", size = 2) + # get plant date for specific year +
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
          #            col = "darkred", size = 2) + # get harvest date for specific year +
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
          #            col = "darkblue", size = 2) + # get onset for specific year
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "date_formatted"] + 5, y=120,
                    label="DC \n planting \n date", color = "darkgreen") +
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "harvest", "date_formatted"], y=120,
                    label="DC \n harvest \n date", color = "darkred") +
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"] , y=120,
                    label="Wet \n season \n onset", color = "darkblue") +
          ggtitle(paste("Double cropped for", input$user_year)) +
          xlab("Date") +
          ylab("Histogram") +
          scale_x_date(date_breaks="3 month", limits = as.Date(c(paste0(input$user_year - 1, '-08-01'),paste0(input$user_year, '-07-31')))) +
          scale_fill_manual(values=c("red", "darkgreen")) +
          theme_bw() +
          labs(fill = "") +
          ylim(0, 130)
      })
    }
    
    # if cell doesn't exist in chosen year
    if (nrow(cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC",]) == 0) {
      output$histogram_DC <- renderPlot({
        ggplot(as.data.frame(filteredData_DC_userYear()), aes(x = date_formatted, fill = plant_or_harv)) +
          geom_histogram(alpha = 0.5) +
          ggtitle(paste("Double cropped for", input$user_year)) +
          xlab("Date") +
          ylab("Histogram") +
          scale_fill_manual(values=c("red", "darkgreen")) +
          theme_bw() +
          labs(fill = "") +
          scale_x_date(date_breaks="3 month", limits = as.Date(c(paste0(input$user_year - 1, '-08-01'),paste0(input$user_year, '-07-31')))) +
          ylim(0,130)
      })
    }
    
    
  })
  
  # observeEvent for map click
  observeEvent(input$mymap_shape_click, { # update the location selectInput on map clicks

    point_lat <- input$mymap_shape_click$lat
    point_lon <- input$mymap_shape_click$lng

    point_chosen <- SpatialPoints(matrix(c(point_lon,point_lat), nrow = 1))
    crs(point_chosen) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

    filteredData_allYears_cell <- reactive({
      over(point_chosen, df, returnList = TRUE) #filteredData_allYears() if want one intensity; use df if want both intensities
    })

    #print(filteredData_allYears_cell()$"1")
    
    cell_data <- as.data.frame(filteredData_allYears_cell()$"1")

    
    if (nrow(cell_data) >= 1) {
      output$timeseries <- renderPlot({
        ggplot(cell_data, mapping = aes(x = year)) +
                                geom_line(mapping = aes(y = date_monthday, col = intensity, linetype = plant_or_harv), size = 1) +
                                xlab("Year") +
                                ylab("Date") +
                                geom_line(mapping = aes(y = onset_monthday, color = "zonset")) +
                                scale_color_manual(name = "",
                                                   labels = c("Double cropped", "Single cropped", "Wet season onset"),
                                                   values = c("DC" = "darkgreen", "SC" = "green", "zonset" = "darkblue")) +
                                scale_linetype_manual(name = "", labels = c("Harvest date", "Planting date"),
                                                                  values = c("dotdash", "solid")) +
                                scale_y_date(date_labels = "%m-%d") +
                                labs(linetype = "") +
                                ggtitle("Timeseries at chosen cell") +
                                theme_bw()
      })
    }

    if (nrow(cell_data) == 0) {
      output$timeseries <- renderPlot({
        ggplot(as.data.frame(filteredData_allYears()), mapping = aes(x = year)) +
          geom_point(mapping = aes(y = date_monthday, col = plant_or_harv)) +
          xlab("Year") +
          ylab("Date") +
          scale_color_manual(name = "", 
                             labels = c("Harvest date", "Planting date"),
                             values = c("plant" = "darkgreen", "harvest" = "darkred")) +
          scale_y_date(date_labels = "%m-%d") +
          labs(shape = "") +
          ggtitle("Invalid cell chosen. Showing timeseries at all cells") +
          theme_bw()
      })
    }
    
    #print(as.data.frame(filteredData_SC_userYear())$plant)
    
    # histogram for ONE year, one histogram per intensity
    # if cell exists in chosen year
    if (nrow(cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC",]) >= 1) {
      
      
      output$histogram_SC <- renderPlot({
        ggplot(as.data.frame(filteredData_SC_userYear()), aes(x = date_formatted, fill = plant_or_harv)) +
          geom_histogram(alpha = 0.3) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkgreen", size = 1) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkred", size = 1) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkblue", size = 1) +
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "date_formatted"],
          #           col = "darkgreen", size = 1) + # get plant date for specific year +
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
          #            col = "darkred", size = 1) + # get harvest date for specific year +
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
          #            col = "darkblue", size = 1) + # get onset for specific year
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "date_formatted"] + 5, y=120,
                    label="SC \n planting \n date", color = "darkgreen") +
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "harvest", "date_formatted"], y=120,
                    label="SC \n harvest \n date", color = "darkred") +
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"] , y=120,
                    label="Wet \n season \n onset", color = "darkblue") +
          ggtitle(paste("Single cropped for", input$user_year)) +
          xlab("Date") +
          ylab("Histogram") +
          scale_fill_manual(values=c("red", "darkgreen")) +
          theme_bw() +
          labs(fill = "") +
          scale_x_date(date_breaks="3 month", limits = as.Date(c(paste0(input$user_year - 1, '-08-01'),paste0(input$user_year, '-07-31')))) +
          ylim(0, 130)
      })
    }
    
    # if cell doesn't exist in chosen year
    if (nrow(cell_data[cell_data$year == input$user_year & cell_data$intensity == "SC",]) == 0) {
      

      output$histogram_SC <- renderPlot({
        ggplot(as.data.frame(filteredData_SC_userYear()), aes(x = date_formatted, fill = plant_or_harv)) +
          geom_histogram(alpha = 0.5) +
          ggtitle(paste("Single cropped for", input$user_year)) +
          xlab("Date") +
          ylab("Histogram") +
          scale_fill_manual(values=c("red", "darkgreen")) +
          theme_bw() +
          labs(fill = "") +
          scale_x_date(date_breaks="3 month", limits = as.Date(c(paste0(input$user_year - 1, '-08-01'),paste0(input$user_year, '-07-31')))) +
          ylim(0, 130)
      })
    }
    
    # if cell exists in chosen year
    if (nrow(cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC",]) >= 1) {
      output$histogram_DC <- renderPlot({
        ggplot(as.data.frame(filteredData_DC_userYear()), aes(x = date_formatted, fill = plant_or_harv)) +
          geom_histogram(alpha = 0.3) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkgreen", size = 1) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkred", size = 1) +
          geom_segment(x = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
                       xend = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
                       y = 110,
                       yend = 0,
                       arrow = grid::arrow(),
                       col = "darkblue", size = 1) +
          
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "date_formatted"],
          #            col = "darkgreen", size = 2) + # get plant date for specific year +
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "harvest", "date_formatted"],
          #            col = "darkred", size = 2) + # get harvest date for specific year +
          # geom_vline(xintercept = cell_data[cell_data$year == input$user_year & cell_data$plant_or_harv == "plant", "onset_date_formatted"],
          #            col = "darkblue", size = 2) + # get onset for specific year
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "date_formatted"] + 5, y=120,
                    label="DC \n planting \n date", color = "darkgreen") +
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "harvest", "date_formatted"], y=120,
                    label="DC \n harvest \n date", color = "darkred") +
          geom_text(x=cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC" & cell_data$plant_or_harv == "plant", "onset_date_formatted"] , y=120,
                    label="Wet \n season \n onset", color = "darkblue") +
          ggtitle(paste("Double cropped for", input$user_year)) +
          xlab("Date") +
          ylab("Histogram") +
          scale_fill_manual(values=c("red", "darkgreen")) +
          theme_bw() +
          labs(fill = "") +
          scale_x_date(date_breaks="3 month", limits = as.Date(c(paste0(input$user_year - 1, '-08-01'),paste0(input$user_year, '-07-31')))) +
          ylim(0, 130)
      })
    }
    
    # if cell doesn't exist in chosen year
    if (nrow(cell_data[cell_data$year == input$user_year & cell_data$intensity == "DC",]) == 0) {
      output$histogram_DC <- renderPlot({
        ggplot(as.data.frame(filteredData_DC_userYear()), aes(x = date_formatted, fill = plant_or_harv)) +
          geom_histogram(alpha = 0.5) +
          ggtitle(paste("Double cropped for", input$user_year)) +
          xlab("Date") +
          ylab("Histogram") +
          scale_fill_manual(values=c("red", "darkgreen")) +
          theme_bw() +
          labs(fill = "") +
          scale_x_date(date_breaks="3 month", limits = as.Date(c(paste0(input$user_year - 1, '-08-01'),paste0(input$user_year, '-07-31')))) +
          ylim(0,130)
      })
    }
  })

  
  
  observe({

    leafletProxy("mymap", data = filteredData()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = MT_outline, weight = 2, stroke = TRUE, color = "black", fillColor = "white") %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 0, fillOpacity = 1,
                  fillColor = ~pal(date),
                  #fillColor = ~colorQuantile("YlOrRd", plant)(plant),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  popup = paste("Plant: ", filteredData_userYearIntensity_plant()$date_formatted, 
                                "<br>", 
                                "Harvest: ", filteredData_userYearIntensity_harvest()$date_formatted, 
                                "<br>",
                                "Onset: ", filteredData()$onset_date_formatted)
                  #popup = popupGraph(graphs_list) #(cell_ID <- df$cell_ID) # graph_popup(df$cell_ID)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~date,
                title = "Date (DOY after Aug 1)",
                opacity = 1
      )
    
    })

  output$documentation <- renderTable({
    documentation_table
  })
  
  output$data_csv <- downloadHandler('data.csv',
    content = function(file) {
      write.csv(to_download_csv, file, row.names = FALSE)
    }
  )
  
  output$data_shp <- downloadHandler(
    filename = 'cell_data.zip',
    content = function(file) {
      if (length(Sys.glob("cell_data.*"))>0){
        file.remove(Sys.glob("cell_data.*"))
      }
      writeOGR(df, dsn="cell_data.shp", layer="cell_data", driver="ESRI Shapefile")
      zip(zipfile='cell_data.zip', files=Sys.glob("cell_data.*"))
      file.copy("cell_data.zip", file)
      if (length(Sys.glob("cell_data.*"))>0){
        file.remove(Sys.glob("cell_data.*"))
      }
    }
  )
  
}

shinyApp(ui = ui, server = server)