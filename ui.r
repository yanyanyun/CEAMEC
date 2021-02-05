##################################################### Integration ###########################################################
#load package
library(shiny)
library(rgdal)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(tibble)

# ui object

ui <- fluidPage(
  theme = shinytheme("lumen"),

  #title
  h2 ("Cost-Effective Animal Management via Environmental Capacity"),

  leafletOutput(outputId = "map"),
  
flowLayout(
  
    textInput("le", "Longitude (E)", "104.0364"),   
  
    textInput("lw", "Longitude (W)", "103.6051"),
  
    textInput("ln", "Latitude (N)", "1.472969"),
  
    textInput("ls", "Latitude (S)", "1.219747"),
  
    textInput("nrows", "Number of rows", "56"),
  
    textInput("ncols", "Number of columns", "96")
              
              ),
  
  fileInput(
    inputId = "dens",
    label = "Upload density (csv file)",
    accept = c(".csv")
  ),

   p("Average per hectare in selected cells"),
   verbatimTextOutput("aver", placeholder = TRUE),


flowLayout(  
  
    textInput("gr", "Growth rate (per month)","0.02775"),

    textInput("expct", "Density must under____ per ha", "5"),
  
    textInput("mth", "Achieve target in____months", "24")
  
    ),

fileInput(
  inputId = "cost",
  label = "Upload cost (csv file)",
  multiple = TRUE,
  accept = c(".csv")
),


actionButton("calculate", "Submit", class = "btn btn-primary"),

    
    p("Total cost"),
    verbatimTextOutput("sum_v", placeholder = TRUE),
  
    p("Detailed management suggestions:"),
    downloadButton("downloadData",label = "Save as KML", class = "btn btn-primary"),
 
    


  )#end UI

