library(shiny)
library(leaflet)
library(devtools)
install_github("CIAT-DAPA/analogues")
library(analogues)
library(maptools)
data("wrld_simpl")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  radioButtons("year","Future Year",c("2030","2050","2070")),
  textOutput("lat"),
  plotOutput("AnalogPlot",width=2000,height=2000)
  # plotOutput("AnalogPlot")
)

server <- function(input, output, session) {
  
  # map for selecting location
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      fitBounds(lng2 = -167.27, lng1 = -52.233, lat2 = 5.499, lat1=83.162)
  })

  
  
  output$lat <- renderText({
    
    print(c(input$mymap_click$lat," , ",input$mymap_click$lng))
    
  })

 output$AnalogPlot <- renderPlot({
   
   # wait for map click  
   validate(need(input$mymap_click, 'Click on the map to view analogues'))

   # get current data
   cur_prec <- raster::getData("worldclim", res=10, var="prec", path=".")
   cur_temp <- raster::getData("worldclim", res=10, var="tmean", path=".")
   
   # get future data
  fut_prec <- getCMIP5(var="prec", rcp=8.5, model=1, year=as.numeric(input$year), res=10, path='.')
  fut_temp <- getCMIP5(var="tmean", rcp=8.5, model=1, year=as.numeric(input$year), res=10, path='.')

  # match up coordinates
  crs(fut_prec) <- crs(fut_temp) <- cur_prec
  
  # create parameter ibject
  params <- createParameters(x=input$mymap_click$lng, y=input$mymap_click$lat, vars=c("prec","tmean"),weights=c(0.5,0.5),
                             ndivisions=c(12,12),growing.season=c(1,12),rotation="tmean",threshold=1,
                             env.data.ref=list(cur_prec,cur_temp), env.data.targ=list(fut_prec,fut_temp),
                             outfile="~/.",fname=NA,writefile=FALSE)
  
  # params <- createParameters(x=4.62, y=52.27, vars=c("prec","tmean"),weights=c(0.5,0.5),
  #                            ndivisions=c(12,12),growing.season=c(1,12),rotation="tmean",threshold=1,
  #                            env.data.ref=list(cur_prec,cur_temp), env.data.targ=list(fut_prec,fut_temp),
  #                            outfile=".",fname=NA,writefile=FALSE)
  # 
  #calculate similarity
  sim_out <- calc_similarity(params)
  
  # plot the result
  plot(sim_out, zlim=c(0,1), col = rev(terrain.colors(10)))
  plot(wrld_simpl, add=T)

 })

}


shinyApp(ui, server)
