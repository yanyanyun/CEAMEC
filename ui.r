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
library(htmltools)
library(bsplus)
library(dplyr)
library(shinycssloaders)
library(rgeos)

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
                                     
                                     use_bs_popover(),
                                     
                                     
                                       
                                       fileInput(
                                         inputId = "distdata",
                                         label = "Upload distdata (csv file)",
                                         accept = c(".csv")
                                       )%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "A csv format data frame where each row is a detected individual. Must have 2 columns.
One for distances and the other for transect names.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     
                                     fileInput(
                                       inputId = "covariates",
                                       label = "Upload covariates (csv file)",
                                       accept = c(".csv")
                                     )%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "A csv format data frame of environmental variables (covariates) that vary at the site level. Number of rows
must match number of transects. Number of columns should equal to number of covariates with one column per covariate. Append an additional column for the lengths of transects (put length as 0 if using point transects)", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                        
                                       selectInput("surveydist", "Type of transects", c("point", "line")),
                                    
                                     textInput("binsize", "Size of bin in meters", placeholder = "10", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Distance data is binned into discrete distance classes with the size of input.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     verbatimTextOutput("summary_distsamp"),
                                     plotOutput('Hist'),
                                     
                                     actionButton("detect", "Check detection functions", class = "btn btn-primary"),
                                     
                                     br(),
                                     
                                     fluidRow(
                                       
                                       column(5,
                                              dataTableOutput("detect_fun")
                                       )
                                     ),
                                     
                                     br(),
                                     
                                     p(class = "first-p", "Modelling with covariates"),
                                     
                                     textInput("det", "Detection covariates (comma delimited)", placeholder = "FI, FI+LU", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model detection (comma delimited). A combination of covariates is write as covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),   
                                     
                                     textInput("state", "Abundance covariates (comma delimited)", placeholder = "FI+LU, FI+LU+EE+BS+OP+V", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model abundance (comma delimited). A combination of covariates is write as covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     selectInput("keyfun", "Detection function", c("halfnorm", "hazard", "uniform", "exp"))%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "You could choose based on the table above if you ran check detection functions (normally the detection function with lowest AIC)", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     actionButton("ModelCovs_distsamp", "Start computing models", class = "btn btn-primary"),
                                     
                                     br(),
                                     
                                     br(),
                                     
                                     p(class = "first-p", "Models with covariates"),
                                     
                                     fluidRow(
                                       column(5,
                                              dataTableOutput("covmodels_distsamp")
                                       )
                                     ),
                                     
                                     uiOutput("bestmodels_distsamp"),
                                     
                                     textInput("nsims", "Number of bootstrap replicates", placeholder = "25", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Number of bootstrap replicates to check adequacy of model fit. Can be time consuming (>1 hour) if a large number (>100) is chosen.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     actionButton("Parboot_distsamp", "Re-fit model", class = "btn btn-primary"),
                                     
                                     fluidRow(
                                       
                                       column(5,
                                              dataTableOutput("parboot_distsamp")
                                       )
                                     ),
                                     
                                     br(),
                                     
                                     uiOutput("Managerables_distsamp"),
                                     
                                     br()
                            ),#End Tab "Distance sampling"
                            
                            #Tab "Repeated count"
                            
                            tabPanel("Repeated count",
                                     
                                     h2 ("N-mixture model with unmarked"),
                                     
                                     fileInput(
                                       inputId = "pcdata",
                                       label = "Upload repeated count data (csv file)",
                                       accept = c(".csv")
                                     )%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "A csv format data frame of the repeated count data with observation and site covariates appended. A transect per row. Columns contains counts (one session per column), observation covariates (one session per column) and site covariates (one covariate per column). Differnt seesions can be identified with '.#' (e.g. '.1','.2','.3') in the column names.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     br(),
                                     
                                     p(class = "first-p", "Data file composition"),
                                     
                                     uiOutput("countcol"),
                                     
                                     uiOutput("sitecol"),
                                     
                                     uiOutput("obscol"),
                                     
                                     textInput("area_pcount", "Area of each survey site in hectare", placeholder = "1")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "Normally to be the size of the transect. But if using traps, you may need to estimate the area that the trap may cover", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     verbatimTextOutput("summary_pcount"),
                                     
                                     actionButton("mixture_in", "Check abundance distribution", class = "btn btn-primary"),
                                     
                                     br(),
                                     
                                     fluidRow(
                                       
                                       column(5,
                                              dataTableOutput("mixture_fun")
                                       )
                                     ),
                                     
                                     br(),
                                     
                                     p(class = "first-p", "Modelling with covariates"),
                                     
                                     textInput("det_pc", "Detection covariates (comma delimited)", placeholder = "1,date,ivel+date", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model detection (comma delimited). A combination of covariates is write as covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),   
                                     
                                     textInput("state_pc", "Abundance covariates (comma delimited)", placeholder = "1,elev,length+forest", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model abundance (comma delimited). A combination of covariates is write as covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     selectInput("mixture", "Latent abundance distribution", c("Poisson (P)" = "P", "Negative binomial (NB)" = "NB", "Zero-inflated Poisson random variable (ZIP)" = "ZIP"))%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "You could choose based on the table above if you ran check abundance distribution (normally the mixture with lowest AIC)", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     actionButton("ModelCovs_pcount", "Start computing models", class = "btn btn-primary"),
                                     
                                     br(),
                                     
                                     br(),
                                     
                                     p(class = "first-p", "Models with covariates"),
                                     
                                     fluidRow(
                                       
                                       column(5,
                                              dataTableOutput("covmodels_pcount")
                                       )
                                     ),
                                     
                                     uiOutput("bestmodels_pcount"),
                                     
                                     textInput("nsims", "Number of bootstrap replicates to check adequacy of model fit", placeholder = "25", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Number of bootstrap replicates to check adequacy of model fit. Can be time consuming (>1 hour) if a large number (>100) is chosen.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     actionButton("Parboot_pcount", "Re-fit model", class = "btn btn-primary"),
                                     
                                     fluidRow(
                                       
                                       column(5,
                                              dataTableOutput("parboot_pcount")
                                       )
                                     ),
                                     
                                     br(),
                                     
                                     uiOutput("Managerables_pcount"),
                                     br()
                            ),
                            #End Tab "Repeated count"
                            
                            #Tab "Removal sampling or double observer sampling"
                            
                            tabPanel("Removal sampling or double observer sampling",
                                     
                                     h2 ("Multinomial-Poisson Mixture model with unmarked"),
                                     
                                     fileInput(
                                       inputId = "mndata",
                                       label = "Upload survey data (csv file)",
                                       accept = c(".csv")
                                     )%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "A csv format data frame of the count data with observation and site covariates appended. A transect per row. Columns contains counts (one session per column), observation covariates (one session per column) and site covariates (one covariate per column). Differnt seesions can be identified with '.#' (e.g. '.1','.2','.3') in the column names.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     p(class = "first-p", "Data file composition"),
                                     
                                     uiOutput("countcolm"),
                                     
                                     uiOutput("sitecolm"),
                                     
                                     uiOutput("obscolm"),
                                     
                                       selectInput("mntype", "Survey type", c("Removal sampling" = "removal", "Standard double observer sampling" = "double", "Dependent double observer sampling" = "depDouble")),
                                       
                                       textInput("area_mn", "Area of each survey site in hectare", placeholder = "1")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "Normally to be the size of the transect. But if using traps, you may need to estimate the area that the trap may cover", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     verbatimTextOutput("summary_mn"),
                                     
                                     br(),
                                     p(class = "first-p", "Modelling with covariates"),
                                     
                                     textInput("det_mn", "Detection covariates (comma delimited)", placeholder = "1,obsA,obsB,obsA+obsB", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model detection (comma delimited). A combination of covariates is write as covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     textInput("state_mn", "Abundance covariates (comma delimited)", placeholder = "1,covA,covB,covA+covB", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model abundance (comma delimited). A combination of covariates is write as covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     actionButton("ModelCovs_mn", "Start computing models", class = "btn btn-primary"),
                                     
                                     br(),
                                     
                                     br(),
                                     
                                     p(class = "first-p", "Models with covariates"),
                                     
                                     fluidRow(
                                       
                                       column(5,
                                              dataTableOutput("covmodels_mn")
                                       )
                                     ),
                                     
                                     uiOutput("bestmodels_mn"),
                                     
                                     textInput("nsims", "Number of bootstrap replicates to check adequacy of model fit", placeholder = "25", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Number of bootstrap replicates to check adequacy of model fit. Can be time consuming (>1 hour) if a large number (>100) is chosen.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     actionButton("Parboot_mn", "Re-fit model", class = "btn btn-primary"),
                                     
                                     fluidRow(
                                       
                                       column(5,
                                              dataTableOutput("parboot_mn")
                                       )
                                     ),
                                     
                                     br(),
                                     
                                     uiOutput("Managerables_mn"),
                                     br()
                            )
                 ),
                 
                 #End Tab "Removal sampling or double observer sampling"
                 
                 #Tab "CEAMEC"
                 tabPanel("CEAMEC",
                          
                          h2 ("Cost-Effective Animal Management via Environmental Capacity"),
                          
                          textInput("gr", "Growth rate (per month)", placeholder = "0.02775")%>%
                            shinyInput_label_embed(
                              shiny_iconlink() %>%
                                bs_embed_popover(
                                  content = "Growth rate can be estimated from reproductive experiments or field observations. If using anual growth rate please divide by 12.", placement = "right", trigger = "hover"
                                )
                            ),
                          
                          textInput("mth", "Achieve target in____months", placeholder = "24")%>%
                            shinyInput_label_embed(
                              shiny_iconlink() %>%
                                bs_embed_popover(
                                  content = "Length in months for the period of management", placement = "right", trigger = "hover"
                                )
                            ),
                          
                          fileInput(
                            inputId = "newdata",
                            label = "Upload covariates for prediction (csv file)",
                            accept = c(".csv")
                          )%>%
                            shinyInput_label_embed(
                              shiny_iconlink() %>%
                                bs_embed_popover(
                                  content = "A csv format data frame containing environmental variable of rasterized area of study. Each row represents each cell in the rasterized study area with cell ID indicated at the first column. All determinant covariates should be included with one covariate per column starting from the second column.", placement = "right", trigger = "hover"
                                )
                            ),   
                          
                          flowLayout(
                            
                            textInput("le", "Longitude (E)", placeholder = "104.0364"),   
                            
                            textInput("lw", "Longitude (W)", placeholder = "103.6051"),
                            
                            textInput("ln", "Latitude (N)", placeholder = "1.472969"),
                            
                            textInput("ls", "Latitude (S)", placeholder = "1.219747"),
                            
                            textInput("nrows", "Number of rows", placeholder = "56"),
                            
                            textInput("ncols", "Number of columns", placeholder = "96")
                            
                          ),
                          
                          leafletOutput(outputId = "map") %>% withSpinner(color="#DD4814"),
                          
                          
                          p(class = "second-p", "Average per hectare in selected cells"),
                          verbatimTextOutput("aver", placeholder = TRUE),
                          
                          p(class = "second-p", "Background density"),
                          
                          verbatimTextOutput("mini", placeholder = TRUE),
                          
                          textInput("expct", "Density must under____ per ha", placeholder = "5")%>%
                            shinyInput_label_embed(
                              shiny_iconlink() %>%
                                bs_embed_popover(
                                  content = "Don't set this value lower than the background density (see above). Background density of each cell can be seen with mouse hover over.", placement = "right", trigger = "hover"
                                )
                            ),
                          
                          fileInput(
                            inputId = "cost",
                            label = "Upload cost (csv file)",
                            multiple = TRUE,
                            accept = c(".csv")
                            )%>%
                              shinyInput_label_embed(
                                shiny_iconlink() %>%
                                  bs_embed_popover(
                                    content = "A csv format data frame with one covariate to be managed per row and one of the four unit costs (a, b, c, d) per column.", placement = "right", trigger = "hover"
                                  )
                              ),
                          
                          
                          actionButton("calculate", "Submit", class = "btn btn-primary"),
                          
                          br(),
                          
                          br(),
                          
                          p(class = "second-p", "Minimum cost to reach the managemnet target"),
                          verbatimTextOutput("sum_v", placeholder = TRUE),
                          
                          br(),
                          
                          br(),
                          
                          p(class = "second-p", "Optimalized management suggestion in a map:"),
                          downloadButton("downloadMap",label = "Save as kml", class = "btn btn-primary"),
                          
                          br(),
                          
                          br(),
                          
                          p(class = "second-p", "Summary and per cell management suggestions:"),
                          downloadButton("downloadPer",label = "Save as excel", class = "btn btn-primary"),
                          
                          br(),
                          br(),
                          br()
                 )
                 #End Tab "CEAMEC"
)#end UI
