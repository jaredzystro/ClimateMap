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
  This site lets you view a map showing locations that are currently
  similar to what your location in the future.<br>
  To use, select how far into future you want to look, and select whether
  you want to compare based on temperature, precipitation, or both.<br>
  Note, the map will take a while to load. Please be patient.</p>")),
  radioButtons("year","Future Year",c("2030","2050","2070")),
  radioButtons("compare","Look for Similarities based on",c("Temperature","Precipitation","Both")),
  div(id="sel_map", HTML("<p>Zoom and click on the map to select the location you want to look for analogs for</p>
                         <p style='color:red;'><b>NOTE:</b> Even if you use the address bar to navigate to a location, you must click on the map to set your location</p>")),
  leafletOutput("mymap"),
  submitButton("Create Map"),
  textOutput("lat"),
  div(id="disp_map", HTML("<p>A large map will load below with the locations that have current climates similar to your future climate. 
                          Green and yellow shading are locations that are more similar<b>")),
  plotOutput("AnalogPlot",width=2000,height=2000)
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
  
  output$lat <- renderText({
    
    print(c("Latitude: ",input$mymap_click$lat,", Longitude:",input$mymap_click$lng))
    
  })

 output$AnalogPlot <- renderPlot({
   
   # wait for map click  
   validate(need(input$mymap_click, 'Map will load after location, date, and comparison type are chosen'))
#   validate(need(input$mymap_click, 'Please click on a point on the map'))
   withProgress(message = "Creating analog map: ", value = 0, {
     
    # get precipitation data
    incProgress(0.333, detail = "Loading data")
    if (input$compare == "Precipitation" ||  input$compare == "Both") {
      cur_prec <- raster::getData("worldclim", res=10, var="prec", path=".")
      fut_prec <- getCMIP5(var="prec", rcp=8.5, model=1, year=as.numeric(input$year), res=10, path='.')
   }
   
    if (input$compare == "Temperature" ||  input$compare == "Both") {
    cur_temp <- raster::getData("worldclim", res=10, var="tmean", path=".")
     fut_temp <- getCMIP5(var="tmean", rcp=8.5, model=1, year=as.numeric(input$year), res=10, path='.')
   }
  
  # match up coordinates
  
     if (input$compare == "Temperature") {
       crs(fut_temp) <- crs(cur_temp) 
       }
     
     if (input$compare == "Precipitation") {
       crs(fut_prec) <- crs(cur_prec) 
       }
     
     if (input$compare == "Both") {
       crs(fut_temp) <- crs(fut_prec) <- cur_prec
       }
  
  # create parameter object
    incProgress(0.333, detail = "Creating model")
    if (input$compare == "Precipitation") {
      params <- createParameters(x=input$mymap_click$lng, y=input$mymap_click$lat, vars=c("prec"),weights=c(1),
                                ndivisions=c(12,12),growing.season=c(1,12),rotation="prec",threshold=1,
                                 env.data.ref=list(cur_prec), env.data.targ=list(fut_prec),
                                 outfile="~/.",fname=NA,writefile=FALSE)
    }
  
    if (input$compare == "Temperature") {
      params <- createParameters(x=input$mymap_click$lng, y=input$mymap_click$lat, vars=c("tmean"),weights=c(1),
                                 ndivisions=c(12,12),growing.season=c(1,12),rotation="tmean",threshold=1,
                                 env.data.ref=list(cur_temp), env.data.targ=list(fut_temp),
                                 outfile="~/.",fname=NA,writefile=FALSE)
    }
    
    if (input$compare == "Both") {
      params <- createParameters(x=input$mymap_click$lng, y=input$mymap_click$lat, vars=c("prec","tmean"),weights=c(0.5,0.5),
                                 ndivisions=c(12,12),growing.season=c(1,12),rotation="tmean",threshold=1,
                                 env.data.ref=list(cur_prec,cur_temp), env.data.targ=list(fut_prec,fut_temp),
                                 outfile="~/.",fname=NA,writefile=FALSE)
    }
    
  
    #calculate similarity
    sim_out <- calc_similarity(params)
    
    # plot the result
    incProgress(0.333, detail = "Drawing map")
    
    plot(sim_out, zlim=c(0,1), col = rev(terrain.colors(10)))
    plot(wrld_simpl, add=T)
    
   })

 })

}


shinyApp(ui, server)
