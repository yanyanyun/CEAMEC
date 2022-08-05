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
library(shinyjs)

# ui object

ui <- navbarPage("Cost-Effective Animal Management via Environmental Capacity",
                 
                 useShinyjs(),
                 
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
                                     br(),
                                     br(),
                                     
                                     h2 ("Hierarchical distance sampling with unmarked"),
                                     
                                     p(class = "first-p", "Survey information"),
                                     
                                     use_bs_popover(),
                                     
                                     a(href="https://github.com/qt37t247/CEAMEC/blob/master/distdata.csv", "See an example of survey data file", style = "color: blue;"),
                                     
                                       fileInput(
                                         inputId = "distdata",
                                         label = "Upload survey data (csv file)",
                                         accept = c(".csv")
                                       )%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "A csv format data frame where each row is a detected individual. Must have two columns. One (named “distance”) 
                                               for distances to the detected individuals and the other (named “transect”) for transect names.", 
                                               placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     a(href="https://github.com/qt37t247/CEAMEC/blob/master/covs.csv", "See an example of covariate file", style = "color: blue;"),
                                     
                                     fileInput(
                                       inputId = "covariates",
                                       label = "Upload covariates (csv file)",
                                       accept = c(".csv")
                                     )%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "A csv format data frame of environmental variables (covariates) that vary at the site level. Number of rows must match number of transects. Number of columns should equal to number of covariates with one column per covariate. An additional column (named “length”) should be attached as the last column for the length of transects (in the unit of meter, put 0 if using point transects).", 
                                             placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                        
                                       selectInput("surveydist", "Type of transects", c("", "point", "line"), selected = NULL),
                                    
                                     textInput("binsize", "Size of bin in meters", placeholder = "10", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Distance data is binned into discrete distance classes with the size of input.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     br(),
                                     
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
                                             content = "List covariates and combinations of covariates to model detection (comma delimited). A combination of covariates is written as the names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     hidden(h5("Input do not match available covariates' names",
                                               id = "message_det",
                                               style = "font-weight:bold;color:red;")),
                                     
                                     textInput("state", "Abundance covariates (comma delimited)", placeholder = "FI+LU, FI+LU+EE+BS+OP+V", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List covariates and combinations of covariates to model abundance (comma delimited). A combination of covariates is written as the names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     hidden(h5("Input do not match available covariates' names",
                                               id = "message_state",
                                               style = "font-weight:bold;color:red;")),
                                     
                                     selectInput("keyfun", "Detection function", c("","halfnorm", "hazard", "uniform", "exp"), selected = NULL)%>%
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
                                     
                                     textInput("nsims_distsamp", "Number of bootstrap replicates", placeholder = "25", width = "70%")%>%
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
                                     
                                     uiOutput("continue_distsamp"),
                                     
                                     br()
                            ),#End Tab "Distance sampling"
                            
                            #Tab "Repeated count"
                            
                            tabPanel("Repeated count",
                                     
                                     br(),
                                     br(),
                                     
                                     h2 ("N-mixture model with unmarked"),
                                     
                                     p(class = "first-p", "Survey information"),
                                     
                                     a(href="https://github.com/qt37t247/CEAMEC/blob/master/mld_pcount.csv", "See an example of survey data file", style = "color: blue;"),
                                     
                                     fileInput(
                                       inputId = "pcdata",
                                       label = "Upload survey data (csv file)",
                                       accept = c(".csv")
                                     )%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "A csv format data frame of the repeated count data with observation and site covariates appended. A transect per row. Columns contains counts (one session per column), observation covariates (one session per column) and site covariates (one covariate per column). Differnt seesions can be identified with '.#' (e.g. '.1','.2','.3') in the column names.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     br(),
                                     
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
                                     
                                     textInput("det_pc", "Detection covariates (comma delimited)", placeholder = "1, date, ivel+date", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List covariates and combinations of covariates (can be either/both observation and site covariates) to model detection (comma delimited). A combination of covariates is written as the names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     hidden(h5("Input do not match available covariates' names",
                                               id = "message_det_pc",
                                               style = "font-weight:bold;color:red;")),
                                     
                                     textInput("state_pc", "Abundance covariates (comma delimited)", placeholder = "1, elev, length+forest", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List covariates and combinations of covariates (site covairates only) to model abundance (comma delimited). A combination of covariates is written as the names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     hidden(h5("Input do not match available covariates' names (site covariates only)",
                                               id = "message_state_pc",
                                               style = "font-weight:bold;color:red;")),
                                     
                                     selectInput("mixture", "Latent abundance distribution", c("", "Poisson (P)" = "P", "Negative binomial (NB)" = "NB", "Zero-inflated Poisson random variable (ZIP)" = "ZIP"), selected = NULL)%>%
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
                                     
                                     textInput("nsims_pcount", "Number of bootstrap replicates to check adequacy of model fit", placeholder = "25", width = "70%")%>%
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
                                     
                                     uiOutput("continue_pcount"),
                                     
                                     br()
                            ),
                            #End Tab "Repeated count"
                            
                            #Tab "Removal sampling or double observer sampling"
                            
                            tabPanel("Removal sampling or double observer sampling",
                                     
                                     br(),
                                     br(),
                                     
                                     h2 ("Multinomial-Poisson Mixture model with unmarked"),
                                     
                                     p(class = "first-p", "Survey information"),
                                     
                                     a(href="https://github.com/qt37t247/CEAMEC/blob/master/oven_removal.csv", "See an example of removal sampling survey data file", style = "color: blue;"),
                                     
                                     br(),
                                     
                                     a(href="https://github.com/qt37t247/CEAMEC/blob/master/fake_double.csv", "See an example of double observer sampling survey data file", style = "color: blue;"),
                                     
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
                                     
                                     uiOutput("countcolm"),
                                     
                                     uiOutput("sitecolm"),
                                     
                                     uiOutput("obscolm"),
                                     
                                       selectInput("mntype", "Survey type", c("", "Removal sampling" = "removal", "Standard double observer sampling" = "double", "Dependent double observer sampling" = "depDouble"), selected = NULL),
                                       
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
                                             content = "List covariates and combinations of covariates (can be either/both observation and site covariates) to model detection (comma delimited). A combination of covariates is written as the names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     hidden(h5("Input do not match available covariates' names",
                                               id = "message_det_mn",
                                               style = "font-weight:bold;color:red;")),
                                     
                                     textInput("state_mn", "Abundance covariates (comma delimited)", placeholder = "1,covA,covB,covA+covB", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List covariates and combinations of covariates (site covairates only) to model abundance (comma delimited). A combination of covariates is written as the names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     hidden(h5("Input do not match available covariates' names (site covariates only)",
                                               id = "message_state_mn",
                                               style = "font-weight:bold;color:red;")),
                                     
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
                                     
                                     textInput("nsims_mn", "Number of bootstrap replicates to check adequacy of model fit", placeholder = "25", width = "70%")%>%
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
                                     
                                     uiOutput("continue_mn"),
                                     
                                     br()
                            )
                 ),
                 
                 #End Tab "Removal sampling or double observer sampling"
                 
                 #Tab "CEAMEC"
                 tabPanel("CEAMEC",
                          
                          br(),
                          br(),
                          
                          h2 ("Cost-Effective Animal Management via Environmental Capacity"),
                          
                          textInput("gr", "Growth rate (per month)", "0.02775")%>%
                            shinyInput_label_embed(
                              shiny_iconlink() %>%
                                bs_embed_popover(
                                  content = "Growth rate can be estimated from reproductive experiments or field observations. If using anual growth rate please divide by 12.", placement = "right", trigger = "hover"
                                )
                            ),
                          
                          textInput("mth", "Achieve target in____months", "24")%>%
                            shinyInput_label_embed(
                              shiny_iconlink() %>%
                                bs_embed_popover(
                                  content = "Length in months for the period of management", placement = "right", trigger = "hover"
                                )
                            ),
                          
                          a(href="https://github.com/qt37t247/CEAMEC/blob/master/newdata.csv", "See an example of newdata file", style = "color: blue;"),
                          
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
                            
                            textInput("le", "Longitude (E)", "104.0364"),   
                            
                            textInput("lw", "Longitude (W)", "103.6051"),
                            
                            textInput("ln", "Latitude (N)", "1.472969"),
                            
                            textInput("ls", "Latitude (S)", "1.219747"),
                            
                            textInput("nrows", "Number of rows", "56"),
                            
                            textInput("ncols", "Number of columns", "96")
                            
                          ),
                          
                          uiOutput("clickmap"),
                          
                          leafletOutput(outputId = "map") %>% withSpinner(color="#DD4814"),
                          
                          br(),
                          
                          br(),
                          
                          p(class = "second-p", "Average per hectare in selected cells"),
                          verbatimTextOutput("aver", placeholder = TRUE),
                          
                          p(class = "second-p", "Background density"),
                          
                          verbatimTextOutput("mini", placeholder = TRUE),
                          
                          textInput("expct", "Density must under____ per ha", "5")%>%
                            shinyInput_label_embed(
                              shiny_iconlink() %>%
                                bs_embed_popover(
                                  content = "Don't set this value lower than the background density (see above). Background density of each cell can be seen with mouse hover over.", placement = "right", trigger = "hover"
                                )
                            ),
                          
                          hidden(h5("Target density too low! Please set a number higher than the background density",
                                    id = "message_expect",
                                    style = "font-weight:bold;color:red;")),
                          
                          a(href="https://github.com/qt37t247/CEAMEC/blob/master/cost.csv", "See an example of cost file", style = "color: blue;"),
                          
                          fileInput(
                            inputId = "cost",
                            label = "Upload cost (csv file)",
                            multiple = TRUE,
                            accept = c(".csv")
                            )%>%
                              shinyInput_label_embed(
                                shiny_iconlink() %>%
                                  bs_embed_popover(
                                    content = "A csv format data frame with one covariate to be managed per row and one of the four unit costs per column (in the order of a, b, c, d).", placement = "right", trigger = "hover"
                                  )
                              ),
                          
                          hidden(h5("Covariates' names in the file do not match selected covarites to be managed",
                                    id = "message_costcov",
                                    style = "font-weight:bold;color:red;")),
                          
                          hidden(h5("Non-numeric items in column 2 to 5, please check the input file",
                                    id = "message_costnum",
                                    style = "font-weight:bold;color:red;")),
                          
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
                 ),
                 #End Tab "CEAMEC"
                 
                 #Tab "Help"
                 tabPanel("Help",
                          
                          br(),
                          br(),
                          br(),
                          
                          a(href="https://youtu.be/mg-trms15hI", "1. I want to watch a video tutorial.", style = "font-size:25px;"),
                          
                          br(),
                          
                          a(href="https://github.com/qt37t247/CEAMEC/blob/master/User%20Manual.pdf", "2. I want to read the user manual.", style = "font-size:25px;"),
                          
                          br(),
                          
                          a(href="https://cran.r-project.org/web/packages/unmarked/index.html", "3. I want to know more about hierarchical modelling and unmarked.", style = "font-size:25px;"),
                          
                          br(),
                          
                          a(href="https://github.com/qt37t247/CEAMEC", "4. I want to report an issue.", style = "font-size:25px;"),
                          
                          br(),
                          
                          a(href="https://github.com/qt37t247/CEAMEC/blob/master/capacity.r", "5. My analysis is massive, I want script for non-interactive run with high-performance computer.", style = "font-size:25px;"),

                          br()
                          
                          
                 ),
                 #End Tab "Help"
    position = c("fixed-top")             
)#end UI
