##################################################### Integration ###########################################################
#load package
library(shiny)
library(rgdal)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(tibble)


# server()
server <- function(input, output, session) { 
  
  rsp_F <- reactive({
    req(input$dens)
    densi<-read.csv(input$dens$datapath, header=TRUE)
    rs <- raster::raster(raster::extent(as.numeric(input$lw),as.numeric(input$le),as.numeric(input$ls),as.numeric(input$ln)), nrows=as.numeric(input$nrows),ncols=as.numeric(input$ncols))
    rs[] <- 1:raster::ncell(rs)
    rsp <- raster::rasterToPolygons(rs)
    sp::merge(rsp,densi,by.x="layer",by.y="X", all.x=FALSE)
  })
  
  costi <- reactive({
    req(input$cost)
    read.csv(input$cost$datapath, header=TRUE)
  })
  
  binpal <- colorBin("Reds", c(0:14), 9, pretty = FALSE)
  
  output$map <- renderLeaflet({  
    
    rsp_x<-rsp_F()
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = rsp_x, 
                  fillColor = binpal(rsp_x$Predicted), 
                  fillOpacity = 0.3, 
                  color = "black", 
                  stroke = T, 
                  weight = 1, 
                  layerId = rsp_x$layer, 
                  group = "clickedIds", 
                  #label = rsp_x$Predicted
      )
    
    
  }) #END RENDER LEAFLET
  
  clickedIds <- reactiveValues(ids = vector())
  
  
  
  makeReactiveBinding("clickedPoly")
  
  observeEvent(input$map_shape_click, {
    
    rsp_x<-rsp_F()
    
    tibble::add_column(rsp_x@data, X.1 = "A", .after = 1)
    
    rsp_x$X.1 <- paste(rsp_x$X.1, rsp_x$layer)
    
    #create object for clicked polygon
    click <- input$map_shape_click
    
    #define leaflet proxy for second regional level map
    proxy <- leafletProxy("map")
    
    #append all click ids in empty vector 
    clickedIds$ids <- c(clickedIds$ids, click$id)
    
    #shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
    clickedPolys <- rsp_x[rsp_x$layer %in% clickedIds$ids, ]
    
    #if the current click ID [from CC_1] exists in the clicked polygon (if it has been clicked twice)
    if(click$id %in% clickedPolys$X.1){
      
      #define vector that subsets NAME that matches CC_1 click ID
      nameMatch <- clickedPolys$layer[clickedPolys$X.1 == click$id]
      
      #remove the current click$id AND its name match from the clickedPolys shapefile
      clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% click$id] 
      clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% nameMatch]
      
      #remove that highlighted polygon from the map
      proxy %>% removeShape(layerId = click$id)
      
    } else {
      
      #map highlighted polygons
      proxy %>% addPolygons(data = clickedPolys,
                            fillColor = "black",
                            fillOpacity = 0.5,
                            weight = 1,
                            color = "black",
                            stroke = T,
                            #label = clickedPolys$Predicted, 
                            layerId = clickedPolys$X.1)
    } #END CONDITIONAL
    
    clickedPoly <<- subset(clickedPolys, clickedPolys$X.1!=click$id)
    
    average<-mean(clickedPoly$Predicted)
    
    output$aver<-renderText({
      
      average
      
    })
    
  }) #END OBSERVE EVENT
  
  
  observeEvent(input$calculate, {
    
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
  
    rsp_n<-rsp_F()
    
##################################################### Function ###########################################################
## cost effectiveness management methods for covariates
    Capacity<-function(rate, ly, expect, time){
      co<-costi()
      r<-rate
      p<-rsp_n@data
      p<-p[,c("layer","Predicted","SE","lower","upper",as.character(co[,1]))]
      ply<-subset(p,p$layer==ly)
      N0=ply$Predicted
      Nt<-expect
      T<-time
      K<-(N0*(exp(r*T)-1))*Nt/(N0*exp(r*T)-Nt)  
      Delta<-log(N0)-log(K)
      pl<-length(co[,1])+5

      DFF<-data.frame("item"=co[,1], "weight"=co[,2], "unitvalue"=c(co[,3]*T+co[,4]), "fixedvalue"=c(co[,5]*T+co[,6]), "pieces"=as.numeric(ply[,6:pl]))  
      
      
      fitness= function(x= rep(1, nrow(DFF))){
        total_value= sum(DFF$unitvalue * x + DFF$fixedvalue*round(x/(x+0.00001)))
        total_weight= sum(DFF$weight * x)
        ifelse(total_weight >= Delta, total_value, Inf)
      }
      
      allowed= matrix(c(rep(0, nrow(DFF)), DFF$pieces), ncol = 2)
      set.seed(42)
      evolution= rgenoud::genoud(fn= fitness, 
                                 nvars= nrow(allowed), 
                                 max= FALSE,
                                 pop.size= 10000,
                                 data.type.int= TRUE, 
                                 Domains= allowed)
      
      cat("Value: ", evolution$value, "\n")
      cat("Weight:", sum(DFF$weight * evolution$par), "dag", "\n")
      best<-as.integer(evolution$par)
      besti<-c(ly, evolution$value, best)
      return(besti)
    }#######End function Capacity
    

    
##################################################### ui_Output ###########################################################
    co<-costi()
    
    out_put<-data.frame(matrix(ncol = 2+length(co[,1]), nrow = 0))
    
    names(out_put)<-c("layer","value", as.character(co[,1]))
    
    out_put_c<-data.frame(layer=numeric(),value=numeric())
    
    HandledPoly<-subset(clickedPoly, clickedPoly$Predicted>=as.numeric(input$expct))

    mult_cell <- HandledPoly$layer
    
    n.mult_cell <- length(mult_cell)
    
    #loop_capacity
    for (i in 1:n.mult_cell){
      
      out_put[i,] <- Capacity(rate = as.numeric(input$gr), ly = mult_cell[i], expect = as.numeric(input$expct), time = as.numeric(input$mth))
      
    }
    
    sum_val<-sum(out_put$value)
    
    output$sum_v<-renderText({
      
      sum_val
      
    })#End loop_capacity
    

    
    removeModal()
    
    #convert dataframe to spatialpolygondataframe
    rs <- raster::raster(raster::extent(as.numeric(input$lw),as.numeric(input$le),as.numeric(input$ls),as.numeric(input$ln)), nrows=as.numeric(input$nrows),ncols=as.numeric(input$ncols))
    rs[] <- 1:raster::ncell(rs)
    rsp <- raster::rasterToPolygons(rs)
    polycap<-sp::merge(rsp,out_put,by.x="layer",by.y="layer", all.x=FALSE)
    
    ####downloadButton
    output$downloadData<- downloadHandler(
      filename = function() {
        paste('data','.kml',sep='')
      },
      content = function(file) {
        rgdal::writeOGR(polycap,file,layer="Management",driver="KML")
      }
    )#end downloadbutton
    
    
  })#END OBSERVE EVENT
  
}

