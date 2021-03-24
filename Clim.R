library(shiny)
library(leaflet)
library(devtools)
#install_github("CIAT-DAPA/analogues")
library(analogues)
library(maptools)
data("wrld_simpl")
library(leaflet.extras)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  div(id="intro", HTML("<h2>Future Climate Analogs Map</h2><br><p>
  <h4>This site lets you view a map showing locations that are currently
  similar to what your location in the future.
  To use, select how far into future you want to look, and select whether
  you want to compare based on temperature, precipitation, or both.</h4><br></p>")),
  div(style="display:inline-block",selectInput("year", "Future Year", choices=c("2030","2050","2070"))),
  div(style="display:inline-block",selectInput("startgrow", "Start of the growing season", choices=c("Jan" = 1,"Feb" = 2,"Mar" = 3, 
                                                                      "Apr" = 4, "May" = 5, "Jun" = 6, 
                                                                      "Jul" = 7, "Aug" = 8, "Sep" = 9, 
                                                                      "Oct" = 10, "Nov" = 11, "Dec" = 12), selected = 1)),
  div(HTML("<br>")),
    div(style="display:inline-block",selectInput("compare","Look for Similarities based on",c("Temperature","Precipitation","Both"))),
    div(style="display:inline-block",selectInput("endgrow","End of the growing season",choices=c("Jan" = 1,"Feb" = 2,"Mar" = 3, 
                                                                "Apr" = 4, "May" = 5, "Jun" = 6, 
                                                                "Jul" = 7, "Aug" = 8, "Sep" = 9, 
                                                                "Oct" = 10, "Nov" = 11, "Dec" = 12), selected = 12)),
  
  div(id="sel_map", HTML("<p>Zoom and click on the map to select the location you want to look for analogs for</p>
                         <p style='color:red;'><b>NOTE:</b> Even if you use the address bar to navigate, you must click on the map to set your location</p>")),
  leafletOutput("mymap", width = "100%", height = "300px"),
  div(HTML("<br>")),
  actionButton("submit_button","Create Analog Map"),
 # textOutput("lat"),
  div(id="disp_map", HTML("<br><p><b>A map will load below with the locations that have current climates similar to your future climate. 
                          <br>Green and yellow shading are locations that are more similar.</b></p><br>")),
  plotOutput("AnalogPlot",width = "100%", height = "800px"),
  div(HTML("Code based on analogues R package from Julian Ramirez of CIAT, 
            using current data from worldclim, future data from CMIP5, 
           using the 8.5 relative concentration BCC-CSM1.1 climate model"))
)

server <- function(input, output, session) {
  
  # map for selecting location
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      fitBounds(-124.7844, 24.7433, -66.9514, 49.3458) %>% 
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE))
    
  })

  observeEvent(input$mymap_click, {
    click = input$mymap_click
    leafletProxy('mymap') %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat)
  })
  
  # output$lat <- renderText({
  #   
  #   print(c("Latitude: ",input$mymap_click$lat,", Longitude:",input$mymap_click$lng))
  #   
  # })

 output$AnalogPlot <- renderPlot({
   
   input$submit_button
   
   # Isolate variables
   compare <- isolate(input$compare)
   year <- isolate(input$year)
   lat <- isolate(input$mymap_click$lat)
   lng <- isolate(input$mymap_click$lng)
   startgrow <- isolate(input$startgrow)
   endgrow <- isolate(input$endgrow)

  # wait for map click  
   validate(need(input$submit_button, 'Map will load after location, date, and comparison type are chosen'))
   validate(need(lat, 'Please click on a point on the map and then click the Create Analog Map button'))
   withProgress(message = "Creating analog map: ", value = 0, {
     
    # get precipitation data
    incProgress(0.333, detail = "Loading data")
    if (compare == "Precipitation" ||  compare == "Both") {
      cur_prec <- raster::getData("worldclim", res=10, var="prec", path=".")
      fut_prec <- getCMIP5(var="prec", rcp=8.5, model=1, year=as.numeric(year), res=10, path='.')
   }
   
    if (compare == "Temperature" ||  compare == "Both") {
    cur_temp <- raster::getData("worldclim", res=10, var="tmean", path=".")
     fut_temp <- getCMIP5(var="tmean", rcp=8.5, model=1, year=as.numeric(year), res=10, path='.')
   }
  
  # match up coordinates
  
     if (compare == "Temperature") {
       crs(fut_temp) <- crs(cur_temp) 
       }
     
     if (compare == "Precipitation") {
       crs(fut_prec) <- crs(cur_prec) 
       }
     
     if (compare == "Both") {
       crs(fut_temp) <- crs(fut_prec) <- cur_prec
       }
  
  # create parameter object
    incProgress(0.333, detail = "Creating model")
    if (compare == "Precipitation") {
      params <- createParameters(x=lng, y=lat, vars=c("prec"),weights=c(1),
                                ndivisions=c(12,12),growing.season=c(startgrow,endgrow),rotation="prec",threshold=1,
                                 env.data.ref=list(cur_prec), env.data.targ=list(fut_prec),
                                 outfile="~/.",fname=NA,writefile=FALSE)
    }
  
    if (compare == "Temperature") {
      params <- createParameters(x=lng, y=lat, vars=c("tmean"),weights=c(1),
                                 ndivisions=c(12,12),growing.season=c(startgrow,endgrow),rotation="tmean",threshold=1,
                                 env.data.ref=list(cur_temp), env.data.targ=list(fut_temp),
                                 outfile="~/.",fname=NA,writefile=FALSE)
    }
    
    if (compare == "Both") {
      params <- createParameters(x=lng, y=lat, vars=c("prec","tmean"),weights=c(0.5,0.5),
                                 ndivisions=c(12,12),growing.season=c(startgrow,endgrow),rotation="tmean",threshold=1,
                                 env.data.ref=list(cur_prec,cur_temp), env.data.targ=list(fut_prec,fut_temp),
                                 outfile="~/.",fname=NA,writefile=FALSE)
    }
    
  
    #calculate similarity
    sim_out <- calc_similarity(params)
    
    # plot the result
    incProgress(0.333, detail = "Drawing map")
    
    plot(sim_out, col = rev(terrain.colors(10)))
    plot(wrld_simpl, add=T)
    
   })

 })

}


shinyApp(ui, server)
