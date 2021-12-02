##################################################### Integration ###########################################################
#load package
library(shiny)
library(rgdal)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(tibble)
library(unmarked)
library(DT)
library(data.table)
library(xlsx)

# server()
server <- function(input, output, session) { 
  
  distd <- reactive({
    req(input$distdata)
    distd <- read.csv(input$distdata$datapath, header=TRUE)
    yDat <- formatDistData(distd, distCol="distance",
                           transectNameCol="transect", dist.breaks=as.numeric(unlist(strsplit(input$distbreak, ","))))
    req(input$covariates)
    covd <- read.csv(input$covariates$datapath, header=TRUE)
    umf <- unmarkedFrameDS(y=as.matrix(yDat), siteCovs=covd, survey=input$surveydist,
                           dist.breaks=as.numeric(unlist(strsplit(input$distbreak, ","))), tlength=as.numeric(unlist(strsplit(input$tlength, ","))), unitsIn="m")
    umf
    })  

  pcountd <- reactive({
    req(input$pcdata)
    pcountd <- read.csv(input$pcdata$datapath, header=TRUE)
    obsnames <- unique(sub("\\..*", "", unlist(strsplit(input$obscol, ","))))
    obscovs <- list()
    for (i in 1:length(obsnames)){
      obscovs$X <- pcountd[ ,grepl(obsnames[i], names(pcountd))]
      names(obscovs)[names(obscovs)=="X"] <- paste(obsnames[i])
    }
    umf <- unmarkedFramePCount(y=pcountd[,unlist(strsplit(input$countcol, ","))], 
                          siteCovs=pcountd[,unlist(strsplit(input$sitecol, ","))],
                          obsCovs=obscovs)
    umf
  })
  
  
  mnd <- reactive({
    req(input$mndata)
    mnd <- read.csv(input$mndata$datapath, header=TRUE)
    obsnamesm <- unique(sub("\\..*", "", unlist(strsplit(input$obscolm, ","))))
    
    
    if(length(obsnamesm)!=0){
    obscovsm <- list()
    for (i in 1:length(obsnamesm)){
      obscovsm$X <- mnd[ ,grepl(obsnamesm[i], names(mnd))]
      names(obscovsm)[names(obscovsm)=="X"] <- paste(obsnamesm[i])
    }
    } else {obscovsm <- NULL}
    
    umf <- unmarkedFrameMPois(y=mnd[,as.character(unlist(strsplit(input$countcolm, ",")))], 
                               siteCovs=mnd[,unlist(strsplit(input$sitecolm, ","))],
                               obsCovs=obscovsm, type = input$mntype)
    umf
  })
  

  observeEvent(input$detect, {
    
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    umf <- distd()
    detect_fun<-list()
    detect_fun$halfnorm <- distsamp(~1~1, umf, keyfun="halfnorm")
    detect_fun$exp <- distsamp(~1~1, umf, keyfun="exp")
    detect_fun$hazard <- distsamp(~1~1, umf, keyfun="hazard")
    detect_fun$uniform <- distsamp(~1~1, umf, keyfun="uniform")
    fitsP <- fitList(fits=detect_fun)
    (msP<-modSel(fitsP))
    detect_df <- data.frame(msP@Full$model, msP@Full$nPars, msP@Full$AIC, msP@Full$delta, msP@Full$AICwt, msP@Full$cumltvWt)
    names(detect_df) <- c("model","nPars","AIC","delta","AICwt","cumltvWt")
    removeModal()
    output$detect_fun<-renderDataTable({
      
      detect_df
      
    })
    
  })
  
  
  observeEvent(input$mixture_in, {
    
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    umf <- pcountd()
    mixture_fun<-list()
    mixture_fun$P <- pcount(~1~1, umf, mixture="P")
    mixture_fun$NB <- pcount(~1~1, umf, mixture="NB")
    mixture_fun$ZIP <- pcount(~1~1, umf, mixture="ZIP")
    fitsPx <- fitList(fits=mixture_fun)
    (msPx<-modSel(fitsPx))
    mixture_df <- data.frame(msPx@Full$model, msPx@Full$nPars, msPx@Full$AIC, msPx@Full$delta, msPx@Full$AICwt, msPx@Full$cumltvWt)
    names(mixture_df) <- c("model","nPars","AIC","delta","AICwt","cumltvWt")
    removeModal()
    output$mixture_fun<-renderDataTable({
      
      mixture_df
      
    })
    
  })
  

  makeReactiveBinding("covmodel")
  
  #distance sampling covmodel calculation
  observeEvent(input$ModelCovs_distsamp, {
    
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    umf <- distd()
    covmodels<-list()
    for (i in as.character(unlist(strsplit(input$det, ",")))){
      for (j in as.character(unlist(strsplit(input$state, ",")))) {
        covmodels$X <- distsamp(formula(paste("~",i,"~",j, sep="")), umf, keyfun = as.character(input$keyfun), output = "density", unitsOut = "ha")
        names(covmodels)[names(covmodels)=="X"] <- paste(i,j,sep="_")
      }
    }
    fitsPC <- fitList(fits=covmodels)
    (msPC<-modSel(fitsPC))
    covmodels_df <- data.frame(msPC@Full$model, msPC@Full$nPars, msPC@Full$AIC, msPC@Full$delta, msPC@Full$AICwt, msPC@Full$cumltvWt)
    names(covmodels_df) <- c("model","nPars","AIC","delta","AICwt","cumltvWt")
    removeModal()
    covmodel <<- covmodels
    output$covmodels_distsamp<-renderDataTable({
      
      covmodels_df
      
    })
    
  })
  
  #repeated count covmodel calculation
  observeEvent(input$ModelCovs_pcount, {
    
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    umf <- pcountd()
    covmodels<-list()
    
    for (i in as.character(unlist(strsplit(input$det_pc, ",")))){
      for (j in as.character(unlist(strsplit(input$state_pc, ",")))) {
        covmodels$X <- pcount(formula(paste("~",i,"~",j, sep="")), umf, mixture = as.character(input$mixture))
        names(covmodels)[names(covmodels)=="X"] <- paste(i,j,sep="_")
      }
    }
    fitsPC <- fitList(fits=covmodels)
    (msPC<-modSel(fitsPC))
    covmodels_df <- data.frame(msPC@Full$model, msPC@Full$nPars, msPC@Full$AIC, msPC@Full$delta, msPC@Full$AICwt, msPC@Full$cumltvWt)
    names(covmodels_df) <- c("model","nPars","AIC","delta","AICwt","cumltvWt")
    removeModal()
    covmodel <<- covmodels
    output$covmodels_pcount<-renderDataTable({
      
      covmodels_df
      
    })
    
  })
  
  #multinomial covmodel calculation
  observeEvent(input$ModelCovs_mn, {
    
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    umf <- mnd()
    
    
    covmodels<-list()
    for (i in as.character(unlist(strsplit(input$det_mn, ",")))){
      for (j in as.character(unlist(strsplit(input$state_mn, ",")))) {
        covmodels$X <- multinomPois(formula(paste("~",i,"~",j, sep="")), umf)
        names(covmodels)[names(covmodels)=="X"] <- paste(i,j,sep="_")
      }
    }
    
   
    fitsPC <- fitList(fits=covmodels)
    (msPC<-modSel(fitsPC))
    covmodels_df <- data.frame(msPC@Full$model, msPC@Full$nPars, msPC@Full$AIC, msPC@Full$delta, msPC@Full$AICwt, msPC@Full$cumltvWt)
    names(covmodels_df) <- c("model","nPars","AIC","delta","AICwt","cumltvWt")
    removeModal()
    covmodel <<- covmodels
    output$covmodels_mn<-renderDataTable({
      
      covmodels_df
      
    })
    
  })
  
  
  #Density estimation
  rsp_F <- reactive({
    req(input$newdata)
    newdata <- read.csv(input$newdata$datapath, header=TRUE)
    newdatax <- newdata
    newdatax[,as.character(unlist(strsplit(input$mcovs, ",")))] <- 0
    P_dens <- predict(covmodel[[as.character(input$best)]], type = "state", newdata = newdata, appendData = TRUE)
    P_densx <- predict(covmodel[[as.character(input$best)]], type = "state", newdata = newdatax)
    P_densx[,2] <- P_dens[,1]*exp(as.numeric(input$gr)*as.numeric(input$mth))*P_densx[,1]/(P_dens[,1]*exp(as.numeric(input$gr)*as.numeric(input$mth))+P_densx[,1]-P_dens[,1])
    P_dens[,1] <- P_dens[,1]/as.numeric(input$area)
    P_densx <- P_densx[,2]/as.numeric(input$area)
    P_dens <- cbind(P_dens,P_densx)
    rs <- raster::raster(raster::extent(as.numeric(input$lw),as.numeric(input$le),as.numeric(input$ls),as.numeric(input$ln)), nrows=as.numeric(input$nrows),ncols=as.numeric(input$ncols))
    rs[] <- 1:raster::ncell(rs)
    rsp <- raster::rasterToPolygons(rs)
    sp::merge(rsp,P_dens,by.x="layer",by.y="layer", all.x=FALSE)
  })
  

  
  #check the score of each model
  

  costi <- reactive({
    req(input$cost)
    costs <- read.csv(input$cost$datapath, header=TRUE)
    bestmodels <- covmodel[names(covmodel) == input$best]
    coeffs <- as.data.frame(bestmodels[[1]]@estimates@estimates$state@estimates)
    name_coeffs <- as.data.frame(names(bestmodels[[1]]@estimates@estimates$state@estimates))
    row.names(coeffs) <- NULL
    coeffs <- cbind(name_coeffs, coeffs)
    names(coeffs) <- c("X", "coefficient")
    merge(coeffs, costs, by.X = "X", by.y = "X")
  })
  
  binpal <- colorBin("Reds", c(0:14), 9, pretty = FALSE)
  
  output$map <- renderLeaflet({  
    
    rsp_x<-rsp_F()
    
    labels <- sprintf(
      "<strong>Estimated density: %s</strong><br/>Density exhausting manageable covariates: %g",
      rsp_x$Predicted, rsp_x$P_densx
    ) %>% lapply(htmltools::HTML)
    
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
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
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
      ps<-subset(p,p$layer==ly)
      ply<-ps[,c("layer","Predicted","SE","lower","upper",as.character(co[,1]))]
      N0=ply$Predicted*as.numeric(input$area)
      Nt<-expect*as.numeric(input$area)
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
      
      list_covs <- list()
      for (i in as.character(co[,1])){
        
      list_covs$X <- c(0, ply[1,i])  
      names(list_covs)[names(list_covs)=="X"] <- i
        
      }
      qq <- expand.grid(list_covs)
      
      list_names <- list()
      for (i in as.character(co[,1])){
        
        list_names$X <- c(0,i)
        names(list_names)[names(list_names)=="X"] <- i
        
      }
      qq_names <- do.call(paste, c(expand.grid(list_names), sep="+"))
      qq_names <- c(qq_names,"best")
      
      qqq <- rbind(qq, as.numeric(evolution$par))
      
      qqqq <- 0 - qqq
      
      psn <- Filter(is.numeric, ps)
      psc <- Filter(is.character, ps)
      names(qqqq) <- as.character(co[,1])
      qqn <- cbind(qqqq, psn)
      qqn <- t(rowsum(t(qqn), group = colnames(qqn), na.rm = T))
      newdatar <- cbind(qqn,psc)
      
      P_densr <- predict(covmodel[[as.character(input$best)]], type = "state", newdata = newdatar)
      P_densr[,2] <- P_densr[1,1]*exp(r*T)*P_densr[,1]/(P_densr[1,1]*exp(r*T)+P_densr[,1]-P_densr[1,1])
      
      q_values <- qqq
      
      for (i in as.character(co[,1])){
        
        q_values[,i] <- qqq[,i]*DFF[DFF$item==i,3] + round(qqq[,i]/(qqq[,i]+0.00001))*DFF[DFF$item==i,4]  
        
      }
      
      final <- cbind(qq_names, rowSums(q_values))
      final <- cbind(final, P_densr[1,1])
      final <- cbind(final, P_densr[,2])
      final <- cbind(final, qqq)
      final <- subset(final, final[,2]!=0)
      colnames(final)[1:4] <- c("Scenarios","Costs","Pre-management","Post-management")
      
      return(final)
    }#######End function Capacity
    

    
##################################################### ui_Output ###########################################################
    
    co<-costi()
    
    HandledPoly<-subset(clickedPoly, clickedPoly$Predicted>=as.numeric(input$expct))

    mult_cell <- HandledPoly$layer
    
    n.mult_cell <- length(mult_cell)
    
    #loop_capacity
    out_put <- list()
    
    for (i in 1:n.mult_cell){
      
      out_put$X <- Capacity(rate = as.numeric(input$gr), ly = mult_cell[i], expect = as.numeric(input$expct), time = as.numeric(input$mth))
      
      rownames(out_put$X) <- out_put$X$Scenarios
      
      names(out_put)[names(out_put)=="X"] <- mult_cell[i]
      
    }
    #End loop_capacity
    
    result = lapply(out_put, "[", "best", )
    
    finaldf <- do.call(rbind, result)
    
    finaldf$layer <- rownames(finaldf)
    
    sum_val<-sum(as.numeric(finaldf[,2]))
    
    output$sum_v<-renderText({
      
      sum_val
      
    })
    
    
    removeModal()
  
    #convert dataframe to spatialpolygondataframe
    rs <- raster::raster(raster::extent(as.numeric(input$lw),as.numeric(input$le),as.numeric(input$ls),as.numeric(input$ln)), nrows=as.numeric(input$nrows),ncols=as.numeric(input$ncols))
    rs[] <- 1:raster::ncell(rs)
    rsp <- raster::rasterToPolygons(rs)
    polycap<-sp::merge(rsp,finaldf,by.x="layer",by.y="layer", all.x=FALSE)
    
    ####download map button
    output$downloadMap<- downloadHandler(
      filename = function() {
        paste('data','.kml',sep='')
      },
      content = function(file) {
        rgdal::writeOGR(polycap,file,layer="",driver="KML")
      }
    )#end download map button
    
    ####download excel button
    output$downloadPer<- downloadHandler(
      filename = function() {
        paste('data','.xlsx',sep='')
      },
      content = function(file) {
      write.xlsx(finaldf, file, sheetName = "summary_best", row.names = FALSE, append = FALSE)
      
      for(i in 1:length(out_put)) {
        write.xlsx(out_put[[i]], file, 
                   sheetName = names(out_put)[i], row.names = FALSE, append = TRUE)}
      
      }
    )#end downloadbutton
    
    
  })#END OBSERVE EVENT
  
}
