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
library(plyr)
library(rgeos)

# server()
server <- function(input, output, session) { 
  
  distd <- reactive({
    req(input$distdata)
    distd <- read.csv(input$distdata$datapath, header=TRUE)
    yDat <- formatDistData(distd, distCol="distance",
                           transectNameCol="transect", dist.breaks=seq(0, round_any(max(distd[,1], na.rm = TRUE), as.numeric(input$binsize), f = ceiling), as.numeric(input$binsize)))
    req(input$covariates)
    covd <- read.csv(input$covariates$datapath, header=TRUE)
    umf <- unmarkedFrameDS(y=as.matrix(yDat), siteCovs=covd[,1:(ncol(covd)-1)], survey=input$surveydist,
                           dist.breaks=seq(0, round_any(max(distd[,1], na.rm = TRUE), as.numeric(input$binsize), f = ceiling), as.numeric(input$binsize)), tlength=covd[,ncol(covd)], unitsIn="m")
    umf
  })  
  
  output$summary_distsamp <- renderPrint({
    req(input$distdata, input$covariates, input$binsize)
    summary(distd())
  })
  
  output$Hist <- renderPlot({
    req(input$distdata, input$covariates, input$binsize)
    unmarked::hist(distd())
  })
  
  output$countcol <- renderUI({
    req(input$pcdata)
    colnames_pc <- colnames(read.csv(input$pcdata$datapath, header=TRUE))
    checkboxGroupInput("countcol", "Column names for counts", colnames_pc, inline = TRUE)
  })

    output$sitecol <- renderUI({
    req(input$pcdata)
    colnames_pc <- colnames(read.csv(input$pcdata$datapath, header=TRUE))
    checkboxGroupInput("sitecol", "Column names for site covariates", colnames_pc, inline = TRUE)
  })

  output$obscol <- renderUI({
    req(input$pcdata)
    colnames_pc <- colnames(read.csv(input$pcdata$datapath, header=TRUE))
    checkboxGroupInput("obscol", "Column names for observation covariates", colnames_pc, inline = TRUE)
  })
  
  pcountd <- reactive({
    req(input$countcol, input$sitecol, input$obscol)
    pcountd <- read.csv(input$pcdata$datapath, header=TRUE)
    obsnames <- unique(sub("\\..*", "", input$obscol, ","))
    obscovs <- list()
    for (i in 1:length(obsnames)){
      obscovs$X <- pcountd[ ,grepl(obsnames[i], names(pcountd))]
      names(obscovs)[names(obscovs)=="X"] <- paste(obsnames[i])
    }
    umf <- unmarkedFramePCount(y=pcountd[, input$countcol], 
                               siteCovs=pcountd[, input$sitecol],
                               obsCovs=obscovs)
    umf
  })
  
  output$summary_pcount <- renderPrint({
    req(input$countcol, input$sitecol, input$obscol)
    summary(pcountd())
  })
  
  output$countcolm <- renderUI({
    req(input$mndata)
    colnames_mn <- colnames(read.csv(input$mndata$datapath, header=TRUE))
    checkboxGroupInput("countcolm", "Column names for counts", colnames_mn, inline = TRUE)
  })

    output$sitecolm <- renderUI({
    req(input$mndata)
    colnames_mn <- colnames(read.csv(input$mndata$datapath, header=TRUE))
    checkboxGroupInput("sitecolm", "Column names for site covariates", colnames_mn, inline = TRUE)
  })

  output$obscolm <- renderUI({
    req(input$mndata)
    colnames_mn <- colnames(read.csv(input$mndata$datapath, header=TRUE))
    checkboxGroupInput("obscolm", "Column names for observation covariates", colnames_mn, inline = TRUE)
  })
  
  mnd <- reactive({
    req(input$countcolm, input$sitecolm)
    mnd <- read.csv(input$mndata$datapath, header=TRUE)
    obsnamesm <- unique(sub("\\..*", "", input$obscolm, ","))
    if(length(obsnamesm)!=0){
      obscovsm <- list()
      for (i in 1:length(obsnamesm)){
        obscovsm$X <- mnd[ ,grepl(obsnamesm[i], names(mnd))]
        names(obscovsm)[names(obscovsm)=="X"] <- paste(obsnamesm[i])
      }
    } else {obscovsm <- NULL}
    
    umf <- unmarkedFrameMPois(y=mnd[, input$countcolm], 
                              siteCovs=mnd[, input$sitecolm],
                              obsCovs=obscovsm, type = input$mntype)
    umf
  })
  
  output$summary_mn <- renderPrint({
    req(input$countcolm, input$sitecolm)
    summary(mnd())
  })
  
  observeEvent(input$detect, {
    req(input$distdata, input$covariates, input$binsize)
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
    output$detect_fun<-renderDT(
      
      datatable(detect_df, caption = "Hint: Best detection function is with the lowest AIC") 
      
    )
    
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
    output$mixture_fun<-renderDT(
      
      datatable(mixture_df, caption = "Hint: Best model for abundance distribution is with the lowest AIC") 
      
    )
    
  })
  
  observeEvent(input$det, {
    
    req(input$covariates, input$det)
    if(all(unique(unlist(strsplit(gsub("\\+", ",", gsub(" ", "", input$det)), ","))) 
        %in% append(head(colnames(read.csv(input$covariates$datapath, header=TRUE)), -1), "1"))) {
      shinyjs::hide("message_det")
    } else {
      shinyjs::show("message_det")
    }
    
  })
  
  observeEvent(input$state, {
    
    req(input$covariates, input$state)
    
    if(all(unique(unlist(strsplit(gsub("\\+", ",", gsub(" ", "", input$state)), ","))) 
           %in% append(head(colnames(read.csv(input$covariates$datapath, header=TRUE)), -1), "1"))) {
      shinyjs::hide("message_state")
    } else {
      shinyjs::show("message_state")
    }
    
  })
  
  makeReactiveBinding("covmodel")
  
  #distance sampling covmodel calculation
  observeEvent(input$ModelCovs_distsamp, {
    req(input$det, input$state, input$keyfun)
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    umf <- distd()
    covmodels<-list()
    for (i in as.character(unlist(strsplit(gsub(" ", "", input$det), ",")))){
      for (j in as.character(unlist(strsplit(gsub(" ", "", input$state), ",")))) {
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
    output$covmodels_distsamp<-renderDT(
      
      datatable(covmodels_df, caption = "Hint: Best model is with the lowest AIC") 
      
    )
    
  })
  
  output$bestmodels_distsamp <- renderUI({
    req(input$ModelCovs_distsamp)
    fitsPC <- fitList(fits=covmodel)
    (msPC<-modSel(fitsPC))
    modelnames <- msPC@Full$model
    selectInput("best_distsamp", "Select the best model", modelnames)
  })

    
  observeEvent(input$det_pc, {
    
    req(input$sitecol, input$det_pc)
    if(all(unique(unlist(strsplit(gsub("\\+", ",", gsub(" ", "", input$det_pc)), ","))) 
           %in% append(append(input$sitecol, sub("\\..", "", input$obscol)), "1"))) {
      shinyjs::hide("message_det_pc")
    } else {
      shinyjs::show("message_det_pc")
    }
    
  })
  
  observeEvent(input$state_pc, {
    
    req(input$sitecol, input$state_pc)
    
    if(all(unique(unlist(strsplit(gsub("\\+", ",", gsub(" ", "", input$state_pc)), ","))) 
           %in% append(input$sitecol, "1"))) {
      shinyjs::hide("message_state_pc")
    } else {
      shinyjs::show("message_state_pc")
    }
    
  })  
  
  #repeated count covmodel calculation
  observeEvent(input$ModelCovs_pcount, {
    req(input$det_pc, input$state_pc, input$mixture)
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    umf <- pcountd()
    covmodels<-list()
    
    for (i in as.character(unlist(strsplit(gsub(" ", "", input$det_pc), ",")))){
      for (j in as.character(unlist(strsplit(gsub(" ", "", input$state_pc), ",")))) {
        covmodels$X <- pcount(formula(paste("~",i,"~",j, sep="")), umf, mixture = input$mixture)
        names(covmodels)[names(covmodels)=="X"] <- paste(i,j,sep="_")
      }
    }
    fitsPC <- fitList(fits=covmodels)
    (msPC<-modSel(fitsPC))
    covmodels_df <- data.frame(msPC@Full$model, msPC@Full$nPars, msPC@Full$AIC, msPC@Full$delta, msPC@Full$AICwt, msPC@Full$cumltvWt)
    names(covmodels_df) <- c("model","nPars","AIC","delta","AICwt","cumltvWt")
    removeModal()
    covmodel <<- covmodels
    output$covmodels_pcount<-renderDT(
      
      datatable(covmodels_df, caption = "Hint: Best model is with the lowest AIC") 
      
    )
    
  })
  
  output$bestmodels_pcount <- renderUI({
    req(input$ModelCovs_pcount)
    fitsPC <- fitList(fits=covmodel)
    (msPC<-modSel(fitsPC))
    modelnames <- msPC@Full$model
    selectInput("best_pcount", "Select the best model", modelnames)
  })  
  
  #multinomial covmodel calculation
  
  observeEvent(input$det_mn, {
    
    req(input$sitecolm, input$det_mn)
    if(all(unique(unlist(strsplit(gsub("\\+", ",", gsub(" ", "", input$det_mn)), ","))) 
           %in% append(append(input$sitecolm, sub("\\..", "", input$obscolm)), "1"))) {
      shinyjs::hide("message_det_mn")
    } else {
      shinyjs::show("message_det_mn")
    }
    
  })
  
  observeEvent(input$state_mn, {
    
    req(input$sitecolm, input$state_mn)
    
    if(all(unique(unlist(strsplit(gsub("\\+", ",", gsub(" ", "", input$state_mn)), ","))) 
           %in% append(input$sitecolm, "1"))) {
      shinyjs::hide("message_state_mn")
    } else {
      shinyjs::show("message_state_mn")
    }
    
  })  
  
  observeEvent(input$ModelCovs_mn, {
    req(input$det_mn, input$state_mn)
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    umf <- mnd()
    
    
    covmodels<-list()
    for (i in as.character(unlist(strsplit(gsub(" ", "", input$det_mn), ",")))){
      for (j in as.character(unlist(strsplit(gsub(" ", "", input$state_mn), ",")))) {
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
    output$covmodels_mn<-renderDT(
      
      datatable(covmodels_df, caption = "Hint: Best model is with the lowest AIC") 
      
    )
    
  })
  
  output$bestmodels_mn <- renderUI({
    req(input$ModelCovs_mn)
    fitsPC <- fitList(fits=covmodel)
    (msPC<-modSel(fitsPC))
    modelnames <- msPC@Full$model
    selectInput("best_mn", "Select the best model", modelnames)
  })
  
  #model re-fit with parametric bootstraps (distance sampling)
  observeEvent(input$Parboot_distsamp, {
    
    req(input$best_distsamp, input$nsims_distsamp)
    
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    
    ##parboot function from unmarked
    fitstats <- function(fm) {
      observed <- getY(fm@data)
      expected <- fitted(fm)
      resids <- residuals(fm)
      sse <- sum(resids^2)
      chisq <- sum((observed - expected)^2 / expected)
      freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
      out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
      return(out)
    }#######End function fitstats
    formula <- covmodel[[input$best_distsamp]]@formula
    keyfun <- covmodel[[input$best_distsamp]]@keyfun
    umf <- distd()
    if (keyfun == "hazard") {
      xxx <- distsamp(formula, umf, keyfun = "hazard", output = "density", unitsOut = "ha")
    } else if (keyfun == "halfnorm") {
      xxx <- distsamp(formula, umf, keyfun = "halfnorm", output = "density", unitsOut = "ha")
    } else if (keyfun == "exp") {
      xxx <- distsamp(formula, umf, keyfun = "exp", output = "density", unitsOut = "ha")
    } else {
      xxx <- distsamp(formula, umf, keyfun = "uniform", output = "density", unitsOut = "ha")
    }
    pb <- parboot(xxx, fitstats, nsim=as.integer(input$nsims_distsamp))
    t.star <- pb@t.star
    t0 <- pb@t0
    nsim <- nrow(t.star)
    biasMat <- pMat <- matrix(NA, nsim, length(t0))
    for(i in 1:nsim) {
      biasMat[i,] <- t0 - t.star[i,]
      pMat[i,] <- abs(t.star[i,] - 1) > abs(t0 - 1)
    }
    bias <- colMeans(biasMat)
    bias.se <- apply(biasMat, 2, sd)
    p.val <- colSums(pMat) / (1 + nsim)
    stats <- data.frame("t0" = t0, "mean(t0 - t_B)" = bias,
                        "StdDev(t0 - t_B)" = bias.se, "Pr(t_B > t0)" = p.val,
                        check.names = FALSE)
    
    removeModal()
    
    output$parboot_distsamp<-renderDT(
      
      datatable(stats, caption = "Hint: t0 = Original statistic computed from data; t_B = Vector of bootstrap samples. Model is considered adequately fit when “Pr(t_B>t0)” is greater than 0.05") 
      
    )
    
  })

  #model re-fit with parametric bootstraps (repeat count)
  observeEvent(input$Parboot_pcount, {
    
    req(input$best_pcount, is.integer(input$nsims_pcount))
    
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    
    ##parboot function from unmarked
    fitstats <- function(fm) {
      observed <- getY(fm@data)
      expected <- fitted(fm)
      resids <- residuals(fm)
      sse <- sum(resids^2)
      chisq <- sum((observed - expected)^2 / expected)
      freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
      out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
      return(out)
    }#######End function fitstats
    
    formula <- covmodel[[input$best_pcount]]@formula
    mixture <- covmodel[[input$best_pcount]]@mixture
    umf <- pcountd()
    if (mixture == "P") {
      xxx <- pcount(formula, umf, mixture = "P")
    } else if (mixture == "NB") {
      xxx <- pcount(formula, umf, mixture = "NB")
    } else {
      xxx <- pcount(formula, umf, mixture = "ZIP")
    }
    pb <- parboot(xxx, fitstats, nsim=as.integer(input$nsims_pcount))
    t.star <- pb@t.star
    t0 <- pb@t0
    nsim <- nrow(t.star)
    biasMat <- pMat <- matrix(NA, nsim, length(t0))
    for(i in 1:nsim) {
      biasMat[i,] <- t0 - t.star[i,]
      pMat[i,] <- abs(t.star[i,] - 1) > abs(t0 - 1)
    }
    bias <- colMeans(biasMat)
    bias.se <- apply(biasMat, 2, sd)
    p.val <- colSums(pMat) / (1 + nsim)
    stats <- data.frame("t0" = t0, "mean(t0 - t_B)" = bias,
                        "StdDev(t0 - t_B)" = bias.se, "Pr(t_B > t0)" = p.val,
                        check.names = FALSE)
    
    removeModal()
    
    output$parboot_pcount<-renderDT(
      
      datatable(stats, caption = "Hint: t0 = Original statistic computed from data; t_B = Vector of bootstrap samples. Model is considered adequately fit when “Pr(t_B>t0)” is greater than 0.05") 
      
    )
    
  })
  
  #model re-fit with parametric bootstraps (multinomial)
  observeEvent(input$Parboot_mn, {
    
    showModal(modalDialog("Job Submitted, please wait...", footer=NULL))
    
    req(input$best_mn, is.integer(input$nsims_mn))
    
    ##parboot function from unmarked
    fitstats <- function(fm) {
      observed <- getY(fm@data)
      expected <- fitted(fm)
      resids <- residuals(fm)
      sse <- sum(resids^2)
      chisq <- sum((observed - expected)^2 / expected)
      freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
      out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
      return(out)
    }#######End function fitstats
    
    formula <- covmodel[[input$best_mn]]@formula
    umf <- mnd()
    xxx <- multinomPois(formula, umf)
    pb <- parboot(xxx, fitstats, nsim=as.integer(input$nsims_mn))
    t.star <- pb@t.star
    t0 <- pb@t0
    nsim <- nrow(t.star)
    biasMat <- pMat <- matrix(NA, nsim, length(t0))
    for(i in 1:nsim) {
      biasMat[i,] <- t0 - t.star[i,]
      pMat[i,] <- abs(t.star[i,] - 1) > abs(t0 - 1)
    }
    bias <- colMeans(biasMat)
    bias.se <- apply(biasMat, 2, sd)
    p.val <- colSums(pMat) / (1 + nsim)
    stats <- data.frame("t0" = t0, "mean(t0 - t_B)" = bias,
                        "StdDev(t0 - t_B)" = bias.se, "Pr(t_B > t0)" = p.val,
                        check.names = FALSE)
    
    removeModal()
    
    output$parboot_mn<-renderDT(
      
      datatable(stats, caption = "Hint: t0 = Original statistic computed from data; t_B = Vector of bootstrap samples. Model is considered adequately fit when “Pr(t_B>t0)” is greater than 0.05") 
      
    )
    
  })
  
  output$Managerables_distsamp <- renderUI({
    req(input$covariates)
    covnames_distsamp <- head(colnames(select_if(read.csv(input$covariates$datapath, header=TRUE), is.numeric)), -1)
    checkboxGroupInput("mcovs_distsamp", "Select covariates to be managed", covnames_distsamp, inline = TRUE)
  })
  
  output$Managerables_pcount <- renderUI({
    req(input$sitecol)
    covnames_pcount <- colnames(select_if(read.csv(input$pcdata$datapath, header=TRUE)[, input$sitecol], is.numeric))
    checkboxGroupInput("mcovs_pcount", "Select covariates to be managed", covnames_pcount, inline = TRUE)
  })
  
  output$Managerables_mn <- renderUI({
    req(input$sitecolm)
    covnames_mn <- colnames(select_if(read.csv(input$mndata$datapath, header=TRUE)[, input$sitecolm], is.numeric))
    checkboxGroupInput("mcovs_mn", "Select covariates to be managed", covnames_mn, inline = TRUE)
  })
  
  output$continue_distsamp <- renderUI({
    req(input$mcovs_distsamp, input$best_distsamp)
    p("Looks good. You may switch to the <CEAMEC> tab")
  })
  
  output$continue_pcount <- renderUI({
    req(input$mcovs_pcount, input$best_pcount)
    p("Looks good. You may switch to the <CEAMEC> tab")
  })
  
  output$continue_mn <- renderUI({
    req(input$mcovs_mn, input$best_mn)
    p("Looks good. You may switch to the <CEAMEC> tab")
  })
  
  #Density estimation
  
  observeEvent(input$expect, {
    
    req(input$expect)
    if(as.numeric(input$expect) >= as.numeric(output$mini)) {
      shinyjs::hide("message_expect")
    } else {
      shinyjs::show("message_expect")
    }
    
  })
  
  rsp_F <- reactive({
    req(input$newdata, input$gr, input$mth, input$le, input$lw, input$ln, input$ls, input$nrows, input$ncols)
    newdata <- read.csv(input$newdata$datapath, header=TRUE)
    if (is.null(input$mcovs_distsamp) == FALSE) {mcovs <- input$mcovs_distsamp} else 
      if (is.null(input$mcovs_pcount) == FALSE) {mcovs <- input$mcovs_pcount} else
      {mcovs <- input$mcovs_mn}
    newdatax <- newdata
    newdatax[,mcovs] <- 0
    if (is.null(input$best_distsamp) == FALSE) {best <- input$best_distsamp} else 
      if (is.null(input$best_pcount) == FALSE) {best <- input$best_pcount} else
      {best <- input$best_mn}
    if (input$area_pcount != "") {area <- as.numeric(input$area_pcount)} else 
      if (input$area_mn != "") {area <- as.numeric(input$area_mn)} else
      {area <- 1}
    P_dens <- predict(covmodel[[best]], type = "state", newdata = newdata, appendData = TRUE)
    P_densx <- predict(covmodel[[best]], type = "state", newdata = newdatax)
    P_densx[,2] <- P_dens[,1]*exp(as.numeric(input$gr)*as.numeric(input$mth))*P_densx[,1]/(P_dens[,1]*exp(as.numeric(input$gr)*as.numeric(input$mth))+P_densx[,1]-P_dens[,1])
    P_dens[,1] <- P_dens[,1]/area
    P_densx <- P_densx[,2]/area
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
    costs <- costs[,1:5]
    costs[is.na(costs)] = 0
    if (is.null(input$best_distsamp) == FALSE) {best <- input$best_distsamp} else 
      if (is.null(input$best_pcount) == FALSE) {best <- input$best_pcount} else
      {best <- input$best_mn}
    coeffs <- as.data.frame(covmodel[[best]]@estimates@estimates$state@estimates)
    name_coeffs <- as.data.frame(names(covmodel[[best]]@estimates@estimates$state@estimates))
    row.names(coeffs) <- NULL
    coeffs <- cbind(name_coeffs, coeffs)
    names(coeffs) <- c("X", "coefficient")
    merge(coeffs, costs, by.X = "X", by.y = "X")
  })
  
  observeEvent(input$cost, {
    
    req(input$cost)
    if (is.null(input$mcovs_distsamp) == FALSE) {mcovs <- input$mcovs_distsamp} else 
      if (is.null(input$mcovs_pcount) == FALSE) {mcovs <- input$mcovs_pcount} else
      {mcovs <- input$mcovs_mn}
    if(setequal(read.csv(input$cost$datapath, header=TRUE)[,1], mcovs)) {
      shinyjs::hide("message_costcov")
    } else {
      shinyjs::show("message_costcov")
    }
    
  })
  
  observeEvent(input$cost, {
    
    req(input$cost)
    if(is.numeric(unlist(read.csv(input$cost$datapath, header=TRUE)[,-1]))) {
      shinyjs::hide("message_costnum")
    } else {
      shinyjs::show("message_costnum")
    }
    
  })
  
  output$clickmap <- renderUI({
    req(input$newdata, input$nrows, input$ncols)
    p("Click to select/unselect cells for management units to be tested.")
  }) 

  binpal <- colorBin("Reds", c(0:14), 9, pretty = FALSE)
  
  output$map <- renderLeaflet({  
    req(rsp_F())
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
    
    maxback<-max(clickedPoly$P_densx)
  
    output$mini<-renderText({
      
      maxback
      
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
      if (input$area_pcount != "") {area <- as.numeric(input$area_pcount)} else 
        if (input$area_mn != "") {area <- as.numeric(input$area_mn)} else
        {area <- 1}
      N0=ply$Predicted*area
      Nt<-expect*area
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
      if (is.null(input$best_distsamp) == FALSE) {best <- input$best_distsamp} else 
        if (is.null(input$best_pcount) == FALSE) {best <- input$best_pcount} else
        {best <- input$best_mn}
      P_densr <- predict(covmodel[[best]], type = "state", newdata = newdatar)
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
