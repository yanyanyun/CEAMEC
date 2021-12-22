library(unmarked) ##load R package "unmarked"

distd <- read.csv("distdata.csv", header=TRUE) ## read in the survey data

covd <- read.csv("covs.csv", header=TRUE) ## read in the site covariates

## generate input for hierarchical modelling
yDat <- formatDistData(distd, distCol="distance",
                       transectNameCol="transect", 
                       dist.breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200))

umf <- unmarkedFrameDS(y=as.matrix(yDat), siteCovs=covd, survey="point",
                       dist.breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200),unitsIn="m")

## compare detection functions (in the example "hazard" is the best)
detect_fun<-list()
detect_fun$halfnorm <- distsamp(~1~1, umf, keyfun="halfnorm")
detect_fun$exp <- distsamp(~1~1, umf, keyfun="exp")
detect_fun$hazard <- distsamp(~1~1, umf, keyfun="hazard")
detect_fun$uniform <- distsamp(~1~1, umf, keyfun="uniform")
fitsP <- fitList(fits=detect_fun)
(msP<-modSel(fitsP))## here to call to display results of model comparisons

## hierachical modelling with different combinations of site covariates
covmodels<-list()
covmodels$Null<-distsamp(~1~1, umf, keyfun="hazard",output = "density", unitsOut = "ha" )
covmodels$A<-distsamp(~1~FI, umf, keyfun="hazard",output = "density", unitsOut = "ha" )
covmodels$B<-distsamp(~FI~1, umf, keyfun="hazard",output = "density", unitsOut = "ha" )
covmodels$C<-distsamp(~FI~FI+EE, umf, keyfun="hazard",output = "density", unitsOut = "ha" )
###......as many as you want####
covmodels$Z<-distsamp(~FI~FI+LU+V+EE+BS+OP, umf, keyfun="hazard",output = "density", unitsOut = "ha" )
fitsP <- fitList(fits=covmodels)
(msP<-modSel(fitsP))## here to call to display results of model comparisons (assuming covmodels$Z is the best model)

##estimate population density using the best model "covmodels$Z"
newdata <- read.csv("newdata.csv", header=TRUE) ## read in environemntal variables for density eestimation
newdatax <- newdata 
newdatax[,c("FI","EE","BS","OP")] <- 0 ## input the covariates to be managed here to generate dataframe for background density calculation
P_dens <- predict(covmodels$Z, type = "state", newdata = newdata, appendData = TRUE) ## estimate density
P_densx <- predict(covmodels$Z, type = "state", newdata = newdatax) ## estimate background density
## calculate realistic goal based on background density ("0.02775" is the population growth rate, "24" is period of management, )
P_densx[,2] <- P_dens[,1]*exp(0.02775*24)*P_densx[,1]/(P_dens[,1]*exp(0.02775*24)+P_densx[,1]-P_dens[,1]) 
## if survey other than distance sampling, predict() gives abundance, remember to divide area of survey to get density
## for example (P_dens[,1] <- P_dens[,1]/5)
P_dens[,1] <- P_dens[,1]/1
P_densx <- P_densx[,2]/1
P_dens <- cbind(P_dens,P_densx)
write.csv(P_dens, "P_dens.csv") ## output a table of density estimation here

####################Optional code for exporting density estimate as spatial file#######################
rs <- raster::raster(raster::extent(103.6051,104.0364,1.219747,1.472969), nrows=56,ncols=96) ##create blank raster over the area of study
rs[] <- 1:raster::ncell(rs)
rsp <- raster::rasterToPolygons(rs) ## convert the raster to spatial polygons dataframe
rsp_F <- sp::merge(rsp,P_dens,by.x="layer",by.y="layer", all.x=FALSE) ## fill in density estimates on the spatial polygons dataframe
rgdal::writeOGR(rsp_F,"density_estimate",layer="",driver="KML") ## output as kml (other format, e.g. shp, is also doable)
####################End of the Optional code for visualizing density estimate#########################

costs <- read.csv("cost.csv") ## import the table of unit costs

## Select management unit by sub-setting, for example select all management units of density is above 4
selected <- subset(P_dens, P_dens$Predicted >= 4) 

selected <- subset(selected, selected$Predicted >= 2) ## filter management units have lower density than management target

selected <- subset(selected, selected$P_densx <= 2) ## filter management units have higher background density than management target

mult_cell <- selected$layer

n.mult_cell <- length(mult_cell)

out_put <- list()

## Loop the function for all selected management units for cost-effectiveness computation
for (i in 1:n.mult_cell){
  
  out_put$X <- Capacity(bestmodel = covmodels$Z, costs = costs, rate = 0.02775, P_dens = P_dens, ly = mult_cell[i], expect = 2, area = 1, time = 24)
  
  rownames(out_put$X) <- out_put$X$Scenarios
  
  names(out_put)[names(out_put)=="X"] <- mult_cell[i]
  
}

result = lapply(out_put, "[", "best", )

finaldf <- do.call(rbind, result)

finaldf$layer <- rownames(finaldf)

## Export and visualize best management scenarios to a kml file
rs <- raster::raster(raster::extent(103.6051,104.0364,1.219747,1.472969), nrows=56,ncols=96) ##create blank raster over the area of study
rs[] <- 1:raster::ncell(rs)
rsp <- raster::rasterToPolygons(rs) ## convert the raster to spatial polygons dataframe
polycap<-sp::merge(rsp,finaldf,by.x="layer",by.y="layer", all.x=FALSE)
rgdal::writeOGR(polycap,"best.kml",layer="",driver="KML")

## Export results as per management unit management scenario comparisons to an excel file
xlsx::write.xlsx(finaldf, "suggestions.xlsx", sheetName = "summary_best", row.names = FALSE, append = FALSE)
for(i in 1:length(out_put)) {
  xlsx::write.xlsx(out_put[[i]], "suggestions.xlsx", 
             sheetName = names(out_put)[i], row.names = FALSE, append = TRUE)}






