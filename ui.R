
# Loading packages --------------------------------------------------------
packages <- c("shiny", "mixOmics", "readxl", "DT", "ggrepel", "shinythemes", "shinyjs", "ggforce", "rstatix",
              "magrittr", "tidyverse", "agricolae", "ascii", "memisc", "shinycssloaders", "plyr", "splitTools", 
              "sjmisc", "pbapply")
lapply(packages, require, character.only = TRUE)

ui = fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  theme = shinytheme("simplex"),
  titlePanel("MetaboDatApp"),
  tabsetPanel(id = "Data",
              tabPanel("Import data", 
                       
                       # Loading data ------------------------------------------------------------
                       sidebarLayout(
                         sidebarPanel(
                           width = 3,
                           fileInput("loadData", "File input", buttonLabel = "Upload...", placeholder = "No file selected", accept = c(".xls", ".xlsx")),
                           textAreaInput("projectNotes", "Project notes"),
                           numericInput("columnsDisplay", "Columns to display", value = 20, min = 1, step = 1),
                           numericInput("rowsDisplay", "Rows to display", value = 10, min = 1, step = 1),
                           uiOutput("uiYselected"), # Select Y column
                           textOutput("selectColumns")
                         ),
                         mainPanel(
                           width = 9, style = "overflow-x:scroll;max-height:100%",
                           DT::dataTableOutput('summaryData'),
                           verbatimTextOutput("summaryDataReport"),
                         )
                       )
                       # 2 level: Pretreatment
                       # sidebarLayout(
                       #   sidebarPanel(
                       #     width = 3,
                       #     selectInput("MVI", "Missing Value imputation", choices = c("None", "RF", "PPCA", "svdImpute", "KNNImpute", "BPCA", "min",
                       #                                                                "halfmin", "mean", "zero"), selected = "None")
                       #     
                       #   ),
                       #   mainPanel(width = 9)
                       # )
              ),
              
              
              # Data exploration panel --------------------------------------------------
              tabPanel("Data exploration",
                       
                       
                       ## PCA scores plot ---------------------------------------------------------
                       tabsetPanel(
                         tabPanel("PCA scores plot",
                                  sidebarLayout(
                                    sidebarPanel(width = 3, position = "left", shinyjs::useShinyjs(), id = "loadingsPCAbar",
                                                 
                                                 h4("Figure settings", align = "center"), br(),
                                                 
                                                 fluidRow(
                                                   column(width = 6, numericInput("Xcomponent", "X component", min = 1, step = 1, value = 1)),
                                                   column(width = 6, numericInput("Ycomponent", "Y component", min = 1, step = 1, value = 2))
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(6, numericInput("heightPCAscores", "Height", value = 700, step = 100)), 
                                                   column(6, numericInput("widthPCAscores", "Width", value = 900, step = 100))
                                                 ),
                                                 
                                                 uiOutput("uiLabelsPCA"), # Select label to display
                                                 
                                                 fluidRow(
                                                   uiOutput("uilabelSizePCAscores"), # Size of labels if selected
                                                   uiOutput("uilineSizePCAscores") # Size of label lines if selected
                                                 ),
                                                 
                                                 fluidRow( # Obtain the class and levels of the classes selected in uiLabelsPCA
                                                   column(width = 6, uiOutput("uiRemoveClass")),
                                                   column(width = 6, uiOutput("uiRemoveLevel"))
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(6, numericInput("legendSizePCAscores", "Legend size", min = 1, max = 1000, value = 10, step = 0.2)),
                                                   column(6, numericInput("pointSizePCAscores", "Shapes size", min = 0.1, max = 10, value = 2, step = 0.2)),
                                                 ),
                                                 
                                                 br(),
                                                 actionButton("resetValuesPCAloadings", "Reset default values"), align = "center"
                                                 
                                    ),
                                    mainPanel(width = 9, 
                                              plotOutput("scoresPCA", width = 250, height = 250))
                                  )
                         ), 
                         
                         ## PCA Loadings plot  ------------------------------------------------------
                         tabPanel("PCA loadings plot",
                                  sidebarLayout( 
                                    sidebarPanel(shinyjs::useShinyjs(),
                                                 id = "loadingsPCAbar",
                                                 width = 3, position = "left",
                                                 h4("Figure settings", align = "center"), br(),
                                                 
                                                 fluidRow(
                                                   column(width = 6, numericInput("XcomponentLoadingsPCA", "X component", min = 1, step = 1, value = 1)),
                                                   column(width = 6, numericInput("YcomponentLoadingsPCA", "Y component", min = 1, step = 1, value = 2))
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(6, numericInput("heightPCAloadings", "Height", value = 700, step = 100)), 
                                                   column(6, numericInput("widthPCAloadings", "Width", value = 725, step = 100))
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(4, numericInput("legendSizePCAloadings", "Legend size", min = 1, max = 1000, value = 10, step = 0.1)),
                                                   column(4, numericInput("pointSizePCAloadings", "Shapes size", min = 0.1, max = 10, value = 2, step = 0.1)),
                                                   column(4, numericInput("labelSizePCAloadings", "Labels size", min = 0.1, max = 10, value = 2.5, step = 0.1))
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(4, checkboxInput("showLabelsLoadingsPCA", "Show labels", value = F), align = "left"),
                                                   column(4, uiOutput("uideclutterLabels"))
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(4, checkboxInput("correlationPlotPCA", "Correlation plot", value = F), align = "left"),
                                                   column(4, uiOutput("uiradInn")),
                                                   column(4, uiOutput("uifilterVar"))
                                                 ),
                                                 
                                                 br(),
                                                 actionButton("resetValuesPCAloadings", "Reset default values"), align = "center"
                                    ),
                                    mainPanel(width = 9,
                                              plotOutput("loadingsPCA", width = 250, height = 250)
                                    )     
                                  )
                         ),
                         
                         ## Heatmap subpanel --------------------------------------------------------
                         tabPanel("Heat map",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         )
                       ),
              ),
              
              # Univariate analysis -----------------------------------------------------              
              tabPanel("Univariate analysis",
                       tabsetPanel(
                         
                         ## ANOVA -------------------------------------------------------------------                         
                         tabPanel("ANOVA",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3, 
                                                 uiOutput("uiFactorsANOVA"),
                                                 checkboxInput("doInteractions", "Perform interactions", value = T),
                                                 checkboxInput("originalDataANOVA", label = "Use original data", value = T),
                                                 checkboxInput("showSDsANOVA", "Include SD", value = F),
                                                 checkboxInput("doFoldChange", "Foldchange", value = F),
                                                 actionButton("runANOVA", "Run ANOVA"), align = "center"
                                                 
                                    ),
                                    mainPanel(width = 9, #style = "overflow-x:scroll;max-width:100%", style = "overflow-y:scroll;max-height:100%",
                                              fluidRow(
                                                # column(1, DTOutput("decimalsANOVA")),
                                                # column(9, withSpinner(tableOutput('ANOVAautomatic'), type = 4, size = 1))
                                                column(9, withSpinner(DTOutput("decimalsANOVA"), type = 4, size = 1))
                                              )
                                    )     
                                  )
                         ),
                         
                         ## Correlations ------------------------------------------------------------
                         tabPanel("Correlations",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         )
                       )
              ),
              
              # Univariate visualizations -----------------------------------------------
              tabPanel("Univariate visualization",
                       tabsetPanel(
                         
                         ## Box/violin plot ---------------------------------------------------------
                         tabPanel("Box/violin plot",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3,
                                                 h4("Figure settings", align = "center"), br(),
                                                 
                                                 fluidRow(
                                                   column(6, uiOutput("uiFactorsBP")),
                                                   column(6, uiOutput("uiVariablesBP"))
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(4, numericInput("sizeAxisBoxplot", "Size axis", value = 8, step = 0.5)), # Size axis
                                                   column(4, numericInput("sizeLabelAxisBoxplot", "Size axis label", value = 10, step = 0.5)), # Size axis label
                                                   column(4, numericInput("XlabelAngleBoxplot", "Angle X label", value = 0, min = 0, step = 5)) # Width
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(4, numericInput("heightBoxplot", "Height", value = 600, step = 100)), # Size axis
                                                   column(4, numericInput("widthBoxplot", "Width", value = 600, step = 100)) # Size axis label
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(4, checkboxInput("rotateBoxplot", "Rotate box plot?", value = F)), # Rotate,
                                                   column(6,  selectInput("orderBoxplot", "Order of sample groups", multiple = F, choices = c("increasing", "alphabetic"), 
                                                                          selected = "increasing")) # Order of the sample groups), 
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(6, checkboxInput("violinPlot", "Violin plot")),
                                                   column(6, selectInput("dotPlot", "Include dots", choices = c("None", "Dot", "Jitter"), selected = "none"))
                                                 )
                                                 
                                    ),                      
                                    
                                    mainPanel(width = 9,
                                              plotOutput("boxPlot"))  
                                  )
                         ),
                         
                         ## Histogram ---------------------------------------------------------------
                         tabPanel("Bar plot",
                                  barPlotUI("barPlot")
                         ),
                         
                         ## Density plot ------------------------------------------------------------
                         tabPanel("Histogram",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         ),
                         
                         ## Density plot ------------------------------------------------------------
                         tabPanel("Density plot",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                                 
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         ),
                       )
              ),
              
              # Classification models ---------------------------------------------------              
              tabPanel("Classification models",
                       tabsetPanel(
                         
                         
                         ## LDA ---------------------------------------------------------------------
                         # tabPanel("LDA",
                         #          sidebarLayout( 
                         #            sidebarPanel(width = 3
                         #            ),
                         #            mainPanel(width = 9        
                         #            )     
                         #          )
                         # ),
                         
                         
                         ## PLS-DA ------------------------------------------------------------------
                         tabPanel("PLS-DA",
                                  plsdaUI("plsdaScores")
                         ),
                         
                         ## SVM ---------------------------------------------------------------------
                         tabPanel("Support Vector Machines",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         )
                       )
              ),
              
              # Regression models -------------------------------------------------------              
              tabPanel("Regression models",
                       tabsetPanel(
                         
                         ## PCR ---------------------------------------------------------------------                         
                         tabPanel("PCR",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         ),
                         
                         ## PLSR --------------------------------------------------------------------
                         tabPanel("PLSR",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         ),
                         
                         ## SVMR --------------------------------------------------------------------
                         tabPanel("SVMR",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         )
                       ),
              ),
              
              # Template ----------------------------------------------------------------
              tabPanel("Template",
                       tabsetPanel(
                         tabPanel("Template",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9,
                                              tableOutput("tableresults")
                                    )     
                                  )
                         )
                       )
              )
  )
)
