library(shiny)
library(shinyWidgets)
library(leaflet)
library(devtools)
#install_github("CIAT-DAPA/analogues")
library(analogues)
library(maptools)
data("wrld_simpl")
library(leaflet.extras)

temp_prec_list <- list(
  "Temp"  = 1, 
  "90%T/10%P" = 0.9,
  "80%T/20%P" = 0.8,
  "70%T/30%P" = 0.7,
  "60%T/40%P" = 0.6,
  "50%T/50%P" = 0.5,
  "40%T/60%P" = 0.4,
  "30%T/70%P" = 0.3,
  "20%T/80%P" = 0.2,
  "10%T/90%P" = 0.1,
  "Precipitation" = 0
)

ui <- fluidPage(
  
  # Intro
  div(id="intro", HTML("<h2>Future Climate Analogs Map</h2><br><p>
  <h4>This site lets you view a map showing locations that are currently
  similar to what your location might look like in the future.
  To use, select how far into future you want to look, and select whether
  you want to compare based on temperature, precipitation, or both.</h4><br></p>")),
  
  # Year, similarity, season options
  div(style="display:inline-block",
      selectInput("startgrow", "Start of the growing season", choices=c("Jan" = 1,"Feb" = 2,"Mar" = 3, 
                                                                        "Apr" = 4, "May" = 5, "Jun" = 6, 
                                                                        "Jul" = 7, "Aug" = 8, "Sep" = 9, 
                                                                        "Oct" = 10, "Nov" = 11, "Dec" = 12), selected = 1)),
  div(style="display:inline-block",
      selectInput("endgrow","End of the growing season",choices=c("Jan" = 1,"Feb" = 2,"Mar" = 3, 
                                                                  "Apr" = 4, "May" = 5, "Jun" = 6, 
                                                                  "Jul" = 7, "Aug" = 8, "Sep" = 9, 
                                                                  "Oct" = 10, "Nov" = 11, "Dec" = 12), selected = 12)),
  div(HTML("<br>")),        
  div(style="display:inline-block",
      sliderTextInput("weight","Look for similarities based on:",choices = names(temp_prec_list))),
  div(style="display:inline-block",
      selectInput("year", "Future Year", choices=c("2030","2050","2070","2080"))),
  
  
  # Map to select location to match
  div(id="sel_map", HTML("<p>Zoom and click on the map to select the location you want to look for analogs for</p>
                         <p style='color:red;'><b>NOTE:</b> Even if you use the address bar to navigate, you must click on the map to set your location</p>")),
  leafletOutput("mymap", width = "80%"),
  div(HTML("<br>")),
  actionButton("submit_button","Create Climate Analog Map"),
  
  div(HTML("<p><br></p>")),
  
  # Info links
  div(HTML("<a href='https://github.com/jaredzystro/ClimateMap'>Code</a> 
            based on <a href='https://github.com/CIAT-DAPA/analogues'>analogues</a> 
            R package from <a href='https://ccafs.cgiar.org/people/julian-ramirez-villegas'>Julian Ramirez</a> 
            of CIAT, using current data from <a href='https://www.worldclim.org/'>worldclim</a>, 
            future data from <a href='https://pcmdi.llnl.gov/mips/cmip5/'>CMIP5</a>, 
            using the 8.5 relative concentration <a href='https://www.nature.com/articles/s41597-019-0343-8/tables/2'>BCC-CSM1.1 climate model</a>"))
)

server <- function(input, output, session) {
  
  # Base map for selecting location
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      fitBounds(-124.7844, 24.7433, -66.9514, 49.3458) %>% 
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE))
    
  })
  
  # Observe mouse clicks and add marker where clicked 
  observeEvent(input$mymap_click, {
    click = input$mymap_click
    leafletProxy('mymap') %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat)
  })
  
  observeEvent(input$submit_button, {
    
    # Isolate variables
    temp_weight <- isolate(as.numeric(temp_prec_list[input$weight]))
    prec_weight <- isolate(1-as.numeric(temp_prec_list[input$weight]))
    year <- isolate(input$year)
    lat <- isolate(input$mymap_click$lat)
    lng <- isolate(input$mymap_click$lng)
    startgrow <- isolate(input$startgrow)
    endgrow <- isolate(input$endgrow)
    
    # Wait for map click  
    validate(need(input$submit_button, 'Map will load after location, date, and comparison type are chosen'))
    validate(need(lat, 'Please click on a point on the map and then click the Create Analog Map button'))
    withProgress(message = "Creating analog map: ", value = 0, {
      
      # Get precipitation data
      incProgress(0.333, detail = "Loading data")
      cur_prec <- raster::getData("worldclim", res=10, var="prec", path=".")
      fut_prec <- getCMIP5(var="prec", rcp=8.5, model=1, year=as.numeric(year), res=10, path='.')
      
      cur_temp <- raster::getData("worldclim", res=10, var="tmean", path=".")
      fut_temp <- getCMIP5(var="tmean", rcp=8.5, model=1, year=as.numeric(year), res=10, path='.')
      
      
      # Match up coordinates
      crs(fut_temp) <- crs(fut_prec) <- cur_prec
      
      # Create parameter object
      incProgress(0.333, detail = "Creating model")
      params <- createParameters(x=lng, y=lat, vars=c("prec","tmean"),weights=c(prec_weight,temp_weight),
                                 ndivisions=c(12,12),growing.season=c(startgrow,endgrow),rotation="tmean",threshold=1,
                                 env.data.ref=list(fut_prec,fut_temp), env.data.targ=list(cur_prec,cur_temp),
                                 outfile="~/.",fname=NA,writefile=FALSE)
      
      
      # Calculate similarity
      sim_out <- calc_similarity(params)
      
      # Attempt to shrink sim_out for shinyapps.io memory limit
#      sim_out <- aggregate(sim_out, fact=3)
      
      # Clear space for shinyapps.io memory limit
      rm(list = c("cur_prec","cur_temp","fut_prec","fut_temp"))
      
      # Plot the result
      incProgress(0.333, detail = "Drawing map")
      leafletProxy('mymap') %>%
        clearImages() %>%
        addRasterImage(sim_out, colors = rev(terrain.colors(10)), opacity = 0.8) %>%
        
        fitBounds(-180, -55, 180, 75)
      
      
    })
    
  })
  
}


shinyApp(ui, server)
