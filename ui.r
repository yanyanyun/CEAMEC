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
library(htmltools)
library(bsplus)

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
                                               content = "A csv format data frame where each row is a detected individual. Must have two columns. One for distances to the detected individuals and the other for transect names.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                       
                                       textInput("surveydist", "Type of transects (point or line)", placeholder = "point", width = "70%")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "Either 'point' or 'line' for point- and line-transects.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                    
                                    
                                     
                                     textInput("distbreak", "Distance cut-points delimiting distance classes in meters", placeholder = "0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Numeric vector of distance cut-points delimiting the distance classes.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     textInput("tlength", "Length of transects in meters (only applicable for line transects)", placeholder = "100,100,100,100,100,100,100", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Number of trasect lengths should match number of transects. This is ignored if using point transects.", placement = "right", trigger = "hover"
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
                                             content = "A csv format data frame of environmental variables (covariates) that vary at the site level. Number of rows must match number of transects. Number of columns should equal to number of covariates with one column per covariate.", placement = "right", trigger = "hover"
                                           )
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
                                     
                                     textInput("det", "Detection covariates (comma delimited)", placeholder = "FI,FI+LU", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model detection (comma delimited). A combination of covariates is writen as names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),   
                                     
                                     textInput("state", "Abundance covariates (comma delimited)", placeholder = "FI+LU,FI+LU+EE+BS+OP+V", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model abundance (comma delimited). A combination of covariates is writen as names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     textInput("keyfun", "detection function", placeholder = "hazard")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "One of the four detection functions: 'halfnorm', 'hazard', 'exp', or 'uniform'. You could choose based on the result table if you ran check detection functions (normally the detection function with lowest AIC).", placement = "right", trigger = "hover"
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
                                     
                                     textInput("best", "Name of the best model", placeholder = "FI_FI+LU+EE+BS+OP+V", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "One of the models computed with covariates. You could choose based on the table above (normally the model with lowest AIC).As the model includes both detection model and abundance model, model name comprises detection model name followed by abundance model name connected with '_'.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
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
                                     
                                     br(),
                                     
                                     textInput("mcovs", "Identify covariates to be managed", placeholder = "FI,EE,BS,OP", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Names of covariates (comma delimited)", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                       textInput("gr", "Growth rate (per month)", placeholder = "0.02775")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Growth rate can be estimated from reproductive experiments or field observations. If using annual growth rate please divide by 12.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                       
                                       textInput("mth", "Achieve target in____months", placeholder = "24")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Length in months for the period of management", placement = "right", trigger = "hover"
                                           )
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
                                     )%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "A csv format data frame containing environmental variable of rasterized area of study. Each row represents each cell in the rasterized study area with cell ID indicated at the first column. All determinant covariates should be included with one covariate per column starting from the second column.", placement = "right", trigger = "hover"
                                           )
                                       )   
                                     
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
                                     
                                     textInput("countcol", "Column names for counts (comma delimited)",  placeholder = "y.1,y.2,y.3", width = "70%")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "All the names of columns contain counts of all sessions.", placement = "right", trigger = "hover"
                                             )
                                         ),   
                                     
                                     textInput("sitecol", "Column names for site covariates (comma delimited)", placeholder = "elev,length,forest", width = "70%")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "All the names of columns contain site covariates.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     textInput("obscol", "Column names for observation covariates (comma delimited)", placeholder = "ivel.1,ivel.2,ivel.3,date.1,date.2,date.3", width = "70%")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "All the names of columns contain observation covariates of all sessions.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     textInput("area", "Area of each survey site in hectare", "1")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "Normally to be the size of the transect. But if using traps, you may need to estimate the area that the trap covers", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
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
                                             content = "List all covariates or combinations of covariates to model abundance (comma delimited). A combination of covariates is writen as names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),   
                                     
                                     textInput("state_pc", "Abundance covariates (comma delimited)", placeholder = "1,elev,length+forest", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model abundance (comma delimited). A combination of covariates is writen as names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     textInput("mixture", "Latent abundance distribution (Poisson (P), negative binomial (NB) or zero-inflated  Poisson random variable (ZIP))",placeholder = "P", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "One of the three mixture: 'P' (Poisson), 'NB' (negative binomial) or 'ZIP' (zero-inflated Poisson). You could choose based on the table above if you ran check abundance distribution (normally the mixture with lowest AIC)", placement = "right", trigger = "hover"
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
                                     
                                     textInput("best", "Name of the best model", placeholder = "time+date_length+forest", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "One of the model computed with covariates. You could choose based on the table above (normally the model with lowest AIC).As the model includes both detection model and abundance model, model name comprises detection model name followed by abundance model name connected with '_'.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
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
                                     
                                     br(),
                                     
                                     textInput("mcovs", "Identify covariates to be managed", placeholder = "length,forest", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Names of covariates (comma delimited)", placement = "right", trigger = "hover"
                                           )
                                       ),
                                    
                                       textInput("gr", "Growth rate (per month)",placeholder = "0.02775")%>%
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
                                     )%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "A csv format data frame containing environmental variable of rasterized area of study. Each row represents each cell in the rasterized study area with cell ID indicated at the first column. All determinant covariates should be included with one covariate per column starting from the second column.", placement = "right", trigger = "hover"
                                           )       
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
                                     )%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "A csv format data frame of the count data with observation and site covariates appended. A transect per row. Columns contains counts (one session per column), observation covariates (one session per column) and site covariates (one covariate per column). Differnt seesions can be identified with '.#' (e.g. '.1','.2','.3') in the column names.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     p(class = "first-p", "Data file composition"),
                                     
                                     textInput("countcolm", "Column names for counts (comma delimited)", placeholder = "y.1,y.2,y.3", width = "70%")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "All the names of columns contain counts of all sessions.", placement = "right", trigger = "hover"
                                             )
                                         ),   
                                     
                                     textInput("sitecolm", "Column names for site covariates (comma delimited)", placeholder = "covA,covB", width = "70%")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "All the names of columns contain site covariates.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     textInput("obscolm", "Column names for observation covariates (comma delimited)", placeholder = "obsA.1,obsA.2,obsB.1,obsB.2", width = "70%")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "All the names of columns contain observation covariates of all sessions.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     
                                       
                                       textInput("mntype", "Survey type (removal, double or depDouble)", placeholder = "removal", width = "70%")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "Anyone of 'removal' for removal sampling, 'double' for standard double observer sampling, or 'depDouble' for dependent double observer sampling.", placement = "right", trigger = "hover"
                                             )
                                         ),
                                       
                                       textInput("area", "Area of each survey site in hectare", "1")%>%
                                         shinyInput_label_embed(
                                           shiny_iconlink() %>%
                                             bs_embed_popover(
                                               content = "Normally to be the size of the transect. But if using traps, you may need to estimate the area that the trap covers", placement = "right", trigger = "hover"
                                             )
                                         ),
                                     
                                     
                                     p(class = "first-p", "Modelling with covariates"),
                                     
                                     textInput("det_mn", "Detection covariates (comma delimited)", placeholder = "1,obsA,obsB,obsA+obsB", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model abundance (comma delimited). A combination of covariates is writen as names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
                                           )
                                       ),   
                                     
                                     textInput("state_mn", "Abundance covariates (comma delimited)", placeholder = "1,covA,covB,covA+covB", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "List all covariates or combinations of covariates to model abundance (comma delimited). A combination of covariates is writen as names of covariates connected with '+'. Consider non-interactive run if many combinations are tested, otherwise very time-consumming.", placement = "right", trigger = "hover"
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
                                     
                                     textInput("best", "Name of the best model", placeholder = "obsA_covA+covB", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "One of the models computed with covariates. You could choose based on the table above (normally the model with lowest AIC). As the model includes both detection model and abundance model, model name comprises detection model name followed by abundance model name connected with '_'.", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
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
                                     
                                     br(),
                                     
                                     textInput("mcovs", "Identify covariates to be managed", placeholder = "covA,covB", width = "70%")%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "Names of covariates (comma delimited)", placement = "right", trigger = "hover"
                                           )
                                       ),
                                     
                                     textInput("gr", "Growth rate (per month)",placeholder = "0.02775")%>%
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
                                     )%>%
                                       shinyInput_label_embed(
                                         shiny_iconlink() %>%
                                           bs_embed_popover(
                                             content = "A csv format data frame containing environmental variable of rasterized area of study. Each row represents each cell in the rasterized study area with cell ID indicated at the first column. All determinant covariates should be included with one covariate per column starting from the second column.", placement = "right", trigger = "hover"
                                           )
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
                          
                          p(class = "second-p", "Background density"),
                          
                          verbatimTextOutput("mini", placeholder = TRUE),
                          
                          textInput("expct", "Density must under____ per ha", "5")%>%
                            shinyInput_label_embed(
                              shiny_iconlink() %>%
                                bs_embed_popover(
                                  content = "Don't set this value lower than the background density (see above).", placement = "right", trigger = "hover"
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
                          downloadButton("downloadPer",label = "Save as excel", class = "btn btn-primary")
                 )
                 #End Tab "CEAMEC"
                 
                 ################################  
                 
                                
                 
)#end UI
