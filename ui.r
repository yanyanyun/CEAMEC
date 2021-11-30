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

# ui object

ui <- navbarPage("Cost-Effective Animal Management via Environmental Capacity",
  theme = shinytheme("united"),
  
  
  
  #Tab "Distance sampling"
  navbarMenu("Field data input",
             
    tabPanel("Distance sampling",
             
             tags$style(".first-p{
                    color: SaddleBrown;
                    font-size: 20px;
                    font-weight: bold;
                                 }"),
             
             tags$style(".second-p{
                    color: black;
                    font-size: 14px;
                    font-weight: bold;
                                 }"),
             
             
             h2 ("Hierarchical distance sampling with unmarked"),
             
             p(class = "first-p", "Distance sampling survey information"),
             
             flowLayout(
             
             fileInput(
               inputId = "distdata",
               label = "Upload distdata (csv file)",
               accept = c(".csv")
             ),
             
             textInput("surveydist", "Type of transects (point or line)", placeholder = "point")
             
             ),
             
             textInput("distbreak", "Distance cut-points delimiting distance classes in meters", placeholder = "0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200", width = "80%"),
             
             textInput("tlength", "Length of transects in meters (only applicable for line transects)", placeholder = "100,100,100,100,100,100,100", width = "80%"),
             
             
             fileInput(
               inputId = "covariates",
               label = "Upload covariates (csv file)",
               accept = c(".csv")
             ),
             
             actionButton("detect", "Check detection functions", class = "btn btn-primary"),
             
             br(),
             
             fluidRow(
               
               column(5,
                      dataTableOutput("detect_fun")
               )
             ),
             
             br(),
             
             p(class = "first-p", "Modelling with covariates"),
             
             textInput("det", "Detection covariates (comma delimited)", placeholder = "1,FI,LU,FI+LU", width = "80%"),   
             
             textInput("state", "Abundance covariates (comma delimited)", placeholder = "1,FI,FI+LU,FI+LU+EE+BS+OP+V", width = "80%"),
             
             textInput("keyfun", "detection function", placeholder = "hazard"),
             
             actionButton("ModelCovs_distsamp", "Start computing models", class = "btn btn-primary"),
             
             br(),
             
             br(),
            
             p(class = "first-p", "Models with covariates"),
             
             fluidRow(
               
               column(5,
                      dataTableOutput("covmodels_distsamp")
               )
             ),
               
               textInput("best", "Name of the best model", placeholder = "FI+LU_FI+LU+EE+BS+OP+V", width = "70%"),
             
               textInput("mcovs", "Identify covariates to be managed", placeholder = "FI,EE,BS,OP", width = "70%"),
               
             flowLayout( 
             
               textInput("gr", "Growth rate (per month)", placeholder = "0.02775"),
             
               textInput("mth", "Achieve target in____months", placeholder = "24")
               
             ),
             
             p(class = "first-p", "Extent and dimension of study area"),
             
             flowLayout(
               
               textInput("le", "Longitude (E)", placeholder = "104.0364"),   
               
               textInput("lw", "Longitude (W)", placeholder = "103.6051"),
               
               textInput("ln", "Latitude (N)", placeholder = "1.472969"),
               
               textInput("ls", "Latitude (S)", placeholder = "1.219747"),
               
               textInput("nrows", "Number of rows", placeholder = "56"),
               
               textInput("ncols", "Number of columns", placeholder = "96")
               
             ),
             
             fileInput(
               inputId = "newdata",
               label = "Upload covariates for prediction (csv file)",
               accept = c(".csv")
             )       
      
    ),
  #End Tab "Distance sampling"
  
  #Tab "Repeated count"
  
    tabPanel("Repeated count",
             
             h2 ("N-mixture model with unmarked"),
             
             fileInput(
               inputId = "pcdata",
               label = "Upload repeated count data (csv file)",
               accept = c(".csv")
             ),
             
             br(),
             
             p(class = "first-p", "Data file composition"),
             
             textInput("countcol", "Column names for counts (comma delimited)",  placeholder = "y.1,y.2,y.3", width = "70%"),   
             
             textInput("sitecol", "Column names for site covariates (comma delimited)", placeholder = "elev,length,forest", width = "70%"),
             
             textInput("obscol", "Column names for observation covariates (comma delimited)", placeholder = "ivel.1,ivel.2,ivel.3,date.1,date.2,date.3", width = "70%"),

             textInput("area", "Area of each survey site in hectare", "1"),
             
             actionButton("mixture_in", "Check abundance distribution", class = "btn btn-primary"),
             
             br(),
             
             fluidRow(
               
               column(5,
                      dataTableOutput("mixture_fun")
               )
             ),
             
             br(),
             
             p(class = "first-p", "Modelling with covariates"),
             
             textInput("det_pc", "Detection covariates (comma delimited)", placeholder = "1,date,ivel+date", width = "70%"),   
             
             textInput("state_pc", "Abundance covariates (comma delimited)", placeholder = "1,elev,length+forest", width = "70%"),
             
             textInput("mixture", "Latent abundance distribution (Poisson (P), negative binomial (NB) or zero-inflated  Poisson random variable (ZIP))",placeholder = "P", width = "70%"),
             
             actionButton("ModelCovs_pcount", "Start computing models", class = "btn btn-primary"),
             
             br(),
             
             br(),
             
             p(class = "first-p", "Models with covariates"),
             
             fluidRow(
               
               column(5,
                      dataTableOutput("covmodels_pcount")
               )
             ),
             
             textInput("best", "Name of the best model", placeholder = "time+date_length+forest", width = "70%"),
             
             textInput("mcovs", "Identify covariates to be managed", placeholder = "length,forest", width = "70%"),
             
             flowLayout(
             
             textInput("gr", "Growth rate (per month)",placeholder = "0.02775"),
             
             textInput("mth", "Achieve target in____months", placeholder = "24")
             
             ),
             
             p(class = "first-p", "Extent and dimension of study area"),
             
             flowLayout(
               
               textInput("le", "Longitude (E)", placeholder = "104.0364"),   
               
               textInput("lw", "Longitude (W)", placeholder = "103.6051"),
               
               textInput("ln", "Latitude (N)", placeholder = "1.472969"),
               
               textInput("ls", "Latitude (S)", placeholder = "1.219747"),
               
               textInput("nrows", "Number of rows", placeholder = "56"),
               
               textInput("ncols", "Number of columns", placeholder = "96")
               
             ),
             
             fileInput(
               inputId = "newdata",
               label = "Upload covariates for prediction (csv file)",
               accept = c(".csv")
             )       
             
    ),
    #End Tab "Repeated count"
  
  #Tab "Removal sampling or double observer sampling"
  
  tabPanel("Removal sampling or double observer sampling",
           
           h2 ("Multinomial-Poisson Mixture model with unmarked"),
           
           fileInput(
             inputId = "mndata",
             label = "Upload survey data (csv file)",
             accept = c(".csv")
           ),
           
           p(class = "first-p", "Data file composition"),
           
           textInput("countcolm", "Column names for counts (comma delimited)", "y.1,y.2,y.3", width = "70%"),   
           
           textInput("sitecolm", "Column names for site covariates (comma delimited)", "covA,covB", width = "70%"),
           
           textInput("obscolm", "Column names for observation covariates (comma delimited)", "obsA.1,obsA.2,obsB.1,obsB.2", width = "70%"),
           
           flowLayout(
           
           textInput("mntype", "Survey type (removal or double)", placeholder = "removal", width = "95%"),
           
           textInput("area", "Area of each survey site in hectare", "1")
           
           ),
           
           
           p(class = "first-p", "Modelling with covariates"),
           
           textInput("det_mn", "Detection covariates (comma delimited)", placeholder = "1,obsA,obsB,obsA+obsB", width = "70%"),   
           
           textInput("state_mn", "Abundance covariates (comma delimited)", placeholder = "1,covA,covB,covA+covB", width = "70%"),
           
           actionButton("ModelCovs_mn", "Start computing models", class = "btn btn-primary"),
           
           br(),
           
           br(),
           
           p(class = "first-p", "Models with covariates"),
           
           fluidRow(
             
             column(5,
                    dataTableOutput("covmodels_mn")
             )
           ),
           
           textInput("best", "Name of the best model", placeholder = "obsA_covA+covB", width = "70%"),
           
           textInput("mcovs", "Identify covariates to be managed", placeholder = "covA,covB", width = "70%"),
           
           flowLayout(
           
           textInput("gr", "Growth rate (per month)",placeholder = "0.02775"),
           
           textInput("mth", "Achieve target in____months", placeholder = "24")
           
           ),
           
           p(class = "first-p", "Extent and dimension of study area"),
           
           flowLayout(
             
             textInput("le", "Longitude (E)", placeholder = "104.0364"),   
             
             textInput("lw", "Longitude (W)", placeholder = "103.6051"),
             
             textInput("ln", "Latitude (N)", placeholder = "1.472969"),
             
             textInput("ls", "Latitude (S)", placeholder = "1.219747"),
             
             textInput("nrows", "Number of rows", placeholder = "56"),
             
             textInput("ncols", "Number of columns", placeholder = "96")
             
           ),
           
           fileInput(
             inputId = "newdata",
             label = "Upload covariates for prediction (csv file)",
             accept = c(".csv")
           )       
           
   )
  ),
  
  #End Tab "Removal sampling or double observer sampling"
  
  #Tab "CEAMEC"
  tabPanel("CEAMEC",
          
           h2 ("Cost-Effective Animal Management via Environmental Capacity"),
           
           leafletOutput(outputId = "map"),
           
           
           p(class = "second-p", "Average per hectare in selected cells"),
           verbatimTextOutput("aver", placeholder = TRUE),
      
           textInput("expct", "Density must under____ per ha", "5"),
          
           fileInput(
             inputId = "cost",
             label = "Upload cost (csv file)",
             multiple = TRUE,
             accept = c(".csv")
           ),
           
           
           actionButton("calculate", "Submit", class = "btn btn-primary"),
           
           br(),
           
           br(),
           
           p(class = "second-p", "Total cost"),
           verbatimTextOutput("sum_v", placeholder = TRUE),
           
           br(),
           
           br(),
           
           p(class = "second-p", "Optimalized management suggestion in a map:"),
           downloadButton("downloadMap",label = "Save as kml", class = "btn btn-primary"),
           
           br(),
           
           br(),
           
           p(class = "second-p", "Summary and per cell management suggestions:"),
           downloadButton("downloadPer",label = "Save as excel", class = "btn btn-primary")
  )
  #End Tab "CEAMEC"
  
################################  
  
  

  )#end UI