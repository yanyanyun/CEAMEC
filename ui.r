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
  
    textInput("le", "Longitude (E)", placeholder="104.0364"),   
  
    textInput("lw", "Longitude (W)", placeholder="103.6051"),
  
    textInput("ln", "Latitude (N)", placeholder="1.472969"),
  
    textInput("ls", "Latitude (S)", placeholder="1.219747"),
  
    textInput("nrows", "Number of rows", placeholder="56"),
  
    textInput("ncols", "Number of columns", placeholder="96")
              
              ),
  
  fileInput(
    inputId = "dens",
    label = "Upload density (csv file)",
    accept = c(".csv")
  ),

   p("Average per hectare in selected cells"),
   verbatimTextOutput("aver", placeholder = TRUE),


flowLayout(  
  
    textInput("gr", "Growth rate (per month)",placeholder="0.02775"),

    textInput("expct", "Density must under____ per ha", placeholder="5"),
  
    textInput("mth", "Achieve target in____months", placeholder="24")
  
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
  
    p("Detailed management sugggestions:"),
    downloadButton("downloadData",label = "Save as KML", class = "btn btn-primary"),
 
    


  )#end UI
