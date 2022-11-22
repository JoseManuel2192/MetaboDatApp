library(shiny); library(mixOmics); library(readxl); library(dplyr); library(DT); library(ggrepel); library(shinythemes); library(shinyjs); library(ggforce)
library(shinycssloaders)

#################################################################################################################################
#################################################################################################################################
#####  USER INTERFACE
#################################################################################################################################
#################################################################################################################################
ui = fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  theme = shinytheme("simplex"),
  titlePanel("MetaboDatApp"),
  tabsetPanel(id = "Data",
              tabPanel("Import data", 
                       
                       ########################
                       # UI: Loading data
                       ########################
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
              
              # Data exploration panel
              tabPanel("Data exploration",
                       
                       ########################
                       # UI: PCA scores plot
                       ########################
                       tabsetPanel(
                         tabPanel("PCA scores plot",
                                  sidebarLayout(
                                    sidebarPanel(width = 3, position = "left", shinyjs::useShinyjs(), id = "loadingsPCAbar",
                                                 
                                                 h4("Figure settings", align = "center"), br(),
                                                 
                                                 fluidRow(
                                                   column(width = 6, numericInput("Xcomponent", "X component", min = 1, step = 1, value = 1)),
                                                   column(width = 6, numericInput("Ycomponent", "Y component", min = 1, step = 1, value = 2))
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
                                                   column(6, sliderInput("heightPCAscores", "Height plot", min = 400, max = 2000, value = 710)),
                                                   column(6, sliderInput("widthPCAscores", "Width plot", min = 400, max = 2000, value = 900))
                                                 ),
                                                 
                                                 fluidRow(
                                                   column(6, numericInput("legendSizePCAscores", "Legend size", min = 1, max = 1000, value = 10, step = 0.1)),
                                                   column(6, numericInput("pointSizePCAscores", "Shapes size", min = 0.1, max = 10, value = 2, step = 0.1)),
                                                 ),
                                                 
                                                 br(),
                                                 actionButton("resetValuesPCAloadings", "Reset default values"), align = "center"
                                                 
                                    ),
                                    mainPanel(width = 9, style = "overflow-y:scroll;min-height:700px",
                                              plotOutput("scoresPCA", width = 250, height = 250))
                                  )
                         ), 
                         #########################
                         # UI: PCA Loadings plot 
                         #########################
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
                                                   column(6, sliderInput("heightPCAloadings", "Height plot", min = 400, max = 2000, value = 700)),
                                                   column(6, sliderInput("widthPCAloadings", "Width plot", min = 400, max = 2000, value = 725)),
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
                         
                         #########################
                         # UI: Heatmap subpanel
                         #########################
                         tabPanel("Heat map",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         )
                       ),
                       # 2 level: PCA loadings
              ),
              #############
              tabPanel("Univariate analysis",
                       tabsetPanel(
                         
                         #############
                         # UI: ANOVA
                         #############
                         tabPanel("ANOVA",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3,
                                                 column(8, uiOutput("uiFactorsANOVA")),
                                                 column(4, checkboxInput("doInteractions", "Perform interactions", value = T)),
                                                 checkboxInput("originalDataANOVA", label = "Use original data", value = T),
                                                 checkboxInput("showSDsANOVA", "Include SD", value = F),
                                                 actionButton("runANOVA", "Run ANOVA"), align = "center"
                                                 
                                    ),
                                    mainPanel(width = 9,
                                              withSpinner(tableOutput('ANOVAautomatic'), type = 4, size = 1)
                                    )     
                                  )
                         ),
                         ##################
                         # UI: Correlations
                         ##################
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
              ##################
              tabPanel("Univariate visualization",
                       tabsetPanel(
                         #######################
                         # UI: Box/violin plot
                         #######################
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
                         
                         ##################
                         # UI: Histogram
                         ##################
                         tabPanel("Histogram",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         ),
                         
                         ##################
                         # UI: Density plot
                         ##################
                         tabPanel("Density plot",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         )
                       )
              ),
              
              tabPanel("Classification models",
                       tabsetPanel(
                         
                         ##################
                         # UI: LDA
                         ##################
                         tabPanel("LDA",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         ),
                         
                         ##################
                         # UI: PLS-DA
                         ##################
                         tabPanel("PLS-DA",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         ),
                         
                         ##################
                         # UI: SVM
                         ##################
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
              
              tabPanel("Regression models",
                       tabsetPanel(
                         
                         ##################
                         # UI: PCR
                         ##################
                         tabPanel("PCR",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         ),
                         
                         ##################
                         # UI: PLSR
                         ##################
                         tabPanel("PLSR",
                                  sidebarLayout( 
                                    sidebarPanel(width = 3
                                    ),
                                    mainPanel(width = 9        
                                    )     
                                  )
                         ),
                         
                         ##################
                         # UI: SVMR
                         ##################
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
              
              ##################
              # UI: Template
              ##################
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

#################################################################################################################################
#################################################################################################################################
#####  SERVER
#################################################################################################################################
#################################################################################################################################
server = function(input, output){
  
  ################################################
  # REACTIVE: Input data (before removing samples)
  ################################################
  inputData  <- reactive({
    # inFile <- req(input$loadData)
    # if(is.null(inFile)) return(NULL)
    # data <- read_excel(paste(inFile$datapath, sep=""), 1) %>% as.data.frame()
    data <- read_excel("./data.xlsx") %>% as.data.frame()
    return(data)
  })
  
  ##############################################
  # REACTIVE: factors excluded from the X matrix
  ##############################################
  factors <- reactive({
    Yfactors <- inputData() %>% dplyr::select(input$summaryData_columns_selected) %>% as.data.frame()
  })
  
  ####################################################
  # REACTIVE: Levels removed from the factor selected
  ####################################################
  levelsRM <- reactive({
    levels <- 10000000
    if (isTruthy(input$removeLevel)) {
      if (input$removeClass == "None") {levels <- 10000000}else{
        factor <- inputData() %>% dplyr::select(input$removeClass) %>% unlist() %>% as.character()
        levels <- which(factor %in% input$removeLevel)
      }
      if (length(levels) == 0){
        levels <- 10000000
      }
    }
    return(levels)
  })
  
  ####################
  # REACTIVE: X matrix
  ####################
  X <- reactive({
    X <- inputData() %>% dplyr::select(-c(input$summaryData_columns_selected))
  })
  
  ####################
  # REACTIVE: Y matrix
  ####################
  Y <- reactive({
    Yposition <- which(colnames(inputData()) == input$Yselected)
    inputData()[,Yposition] 
  })
  
  ####################
  # REACTIVE: col.pch
  ####################
  col.pch = reactive({
    levels = length(levels(as.factor(Y())))
    col = c("darkblue","red4","darkgreen", "darkorange","3", "4", "5", "1", paste0(8:200))[1:levels] #Ajustar colores si hay mas de tres grupos
    pch = c(16,17,18,15,8, 9,10, 11, 12,13,14,19:200)[1:levels] #Ajusta las formas de los scores
    results = list("col" = col, "pch" = pch)
    return(results)
  })
  
  ##############################
  # REACTIVE: constant variables
  ##############################
  constantVars <- reactive({
    Xrml <- X()[-levelsRM(),]
    all.sd <- apply(Xrml, 2, function(X) sd(X))
    all.sd[all.sd == 0] %>% names()
  })
  
  ###############
  # REACTIVE: PCA
  ###############
  my.pca <- reactive({
    Xpca <- X()[-levelsRM(),] %>% dplyr::select(-any_of(constantVars())) 
    ncomp <- ifelse(Xpca %>% dim() %>% min() < 20, Xpca %>% dim() %>% min() %>% as.numeric(), 20)
    my.pca <- mixOmics::pca(Xpca, ncomp = 5, center = TRUE, scale = TRUE)
    return(my.pca)
  })
  
  #################################  #################################  #################################  #################################
  
  ################################################################
  # RENDER USER INTERFACE: column choices to select category of Y
  ################################################################
  output$uiYselected <- renderUI({
    selectInput("Yselected", label = "Select Y",
                choices = inputData() %>% colnames()
    )
  })
  
  ########################################################
  # RENDER USER INTERFACE: -> select labels to show in PCA
  ########################################################
  output$uiLabelsPCA <- renderUI({
    selectInput("labelsPCA", label = "Select labels to display", selected = "None",
                choices = c("None", inputData() %>% colnames())
    )
  })
  
  ##################################################
  # RENDER USER INTERFACE: -> select Class to remove
  ##################################################
  output$uiRemoveClass <- renderUI({
    selectInput("removeClass", "Remove samples from any class?", selected = "None", multiple = F,
                choices = c("None", factors() %>% colnames()))
  })
  
  ##################################################################
  # RENDER USER INTERFACE: -> select levels from the class to remove
  ##################################################################
  output$uiRemoveLevel <- renderUI({
    req(input$removeClass)
    req(input$removeClass != "None")
    selectInput("removeLevel", "Which samples will be removed?", selected = "None", multiple = T,
                choices = inputData() %>% dplyr::select(input$removeClass) %>% unlist() %>% as.factor() %>% levels() %>% as.character()
    )
  })
  
  #########################################################################
  # RENDER USER INTERFACE: Labels size of scores plot if removeLevel exists
  #########################################################################
  output$uilabelSizePCAscores <- renderUI({
    req(input$labelsPCA != "None")
    column(6, numericInput("labelSizePCAscores", "Labels size", min = 0.1, max = 10, value = 2.5, step = 0.1))
  })
  
  ########################################################################
  # RENDER USER INTERFACE: Lines size of scores plot if removeLevel exists
  ########################################################################
  output$uilineSizePCAscores <- renderUI({
    req(input$labelsPCA != "None")
    column(6, numericInput("lineSizePCAscores", "Lines size", min = 0, max = 10, value = 0.1, step = 0.1))
  })
  
  #########################################
  # RENDER USER INTERFACE: Declutter labels
  #########################################
  output$uideclutterLabels <- renderUI({
    req(input$showLabelsLoadingsPCA == T)
    numericInput("declutterLabels", "Declutter labels", min = 0, value = 10, step = 0.1)
  })
  
  ###################################################
  # RENDER USER INTERFACE: Radios inner loadings plot
  ###################################################
  output$uiradInn <- renderUI({
    req(input$correlationPlotPCA == T)
    numericInput("radInn", "Inner circle", min = 0, max = 1, value = 0.5, step = 0.1)
  })
  
  ###################################################################
  # RENDER USER INTERFACE: Filter variables whithin the inner circle
  ###################################################################
  output$uifilterVar <- renderUI({
    req(input$correlationPlotPCA == T)
    checkboxInput("filterVar", "Filter vars", value = F)
  })
  
  ########################################
  # RENDER USER INTERFACE: Factors boxplot
  ########################################
  output$uiFactorsBP <- renderUI({
    selectInput("FactorsBP", "Select a factor", multiple = F,
                choices = c(factors() %>% colnames()))
  })
  
  ########################################
  # RENDER USER INTERFACE: Variable boxplot
  ########################################
  output$uiVariablesBP <- renderUI({
    selectInput("VariablesBP", "Select a variable", multiple = F,
                choices = c(X() %>% colnames()))
  })
  
  ########################################
  # RENDER USER INTERFACE: Factors ANOVA
  ########################################
  output$uiFactorsANOVA <- renderUI({
    selectInput("factorsANOVA", "Factors (between-subject variation)", multiple = T,
                choices = factors() %>% colnames())
  })
  
  
  ############################################################################################################################
  ############################################################################################################################
  ###### OUTPUTS
  ############################################################################################################################
  ############################################################################################################################
  
  ##########################
  # OUTPUT: Comment
  ##########################
  output$selectColumns <- renderText("Select columns to skip from the X-matrix in the summary table. A preselection is automatically suggested")
  
  ##########################
  # OUTPUT: Summary Table
  ##########################
  output$summaryData <- DT::renderDataTable({
    preselected <- lapply(inputData(), is.numeric) %>% unlist()
    preselectedColumns <- which(preselected == F) %>% as.numeric()
    datatable(head(inputData(), n = c(input$rowsDisplay,input$columnsDisplay)), extensions = 'Select', selection = list(target = "column", selected = preselectedColumns), 
              options = list(ordering = FALSE, searching = FALSE, pageLength = 15)) 
    
  })
  
  ##############################
  # OUTPUT: Summary data report
  ##############################
  output$summaryDataReport <- renderPrint({
    MV <- which(is.na(X()[-levelsRM(),])) %>% length()
    NegV <- which(X()[-levelsRM(),] < 0) %>% length()
    
    print(paste("Number of missing values: ", MV))
    print(paste("Number of negative values: ", NegV))
    print(paste("The loaded has", X()[-levelsRM(),] %>% nrow(), "rows and", X() %>% ncol(), "columns"))
  })
  
  ##########################
  # OUTPUT: PCA scores plot 
  ##########################
  output$scoresPCA <- renderPlot({
    Y <- Y()[-levelsRM()]
    scatterData <- my.pca()$variates$X %>% as.data.frame() %>% mutate(Y)
    expl.variance <- my.pca()$prop_expl_var$X * 100
    colnames(scatterData) = paste0(names(expl.variance), ": ", as.character(round(expl.variance, 2)), c("% of explained variance"))
    
    # Conditional to avoid warnings when labels = 0
    if(length(input$labelsPCA) == 0){labels <- "None"}else{
      if (input$labelsPCA == "None") {
        labels <- "None"
      }else{
        labels <- factors()[-levelsRM(),] %>% dplyr::select(input$labelsPCA, -any_of(constantVars())) %>% unlist() %>% as.character()
      }
    }
    
    scoresPCA <- scatterPlot(scatterData = scatterData, labelSize = input$labelSizePCAscores, pointSize = input$pointSizePCAscores,
                             legendSize = input$legendSizePCAscores, pch = col.pch()$pch, col = col.pch()$col, titleScatter = "",
                             Xcomp = input$Xcomponent, Ycomp = input$Ycomponent, Y = Y()[-levelsRM()], labels = labels, segment.size = input$lineSizePCAscores)
    scoresPCA
  }, 
  width = reactive(input$widthPCAscores),
  height = reactive(input$heightPCAscores),
  res = 150
  )
  
  ############################
  # OUTPUT: PCA loadings plot
  ############################
  output$loadingsPCA <- renderPlot({
    expl.variance <- my.pca()$prop_expl_var$X * 100
    
    # Loadings plot (F) or correlation plot (T, by default in the ui)
    if (input$correlationPlotPCA == T) {
      loadingsData <- cor(my.pca()$X, my.pca()$variates$X, use = "pairwise") %>% as.data.frame()
    }else{
      loadingsData <-my.pca()$loadings$X %>% as.data.frame()
    }
    
    colnames(loadingsData) = paste0(names(expl.variance), ": ", as.character(round(expl.variance, 2)), 
                                    c("% of explained variance"))
    
    declutterLabels <- ifelse(isTruthy(input$declutterLabels), input$declutterLabels, 10)
    filterVar <- ifelse(isTruthy(input$filterVar), input$filterVar, F)
    radInn <- ifelse(isTruthy(input$radInn), input$radInn, 0.5)
    
    loadingsPlotPCA <- loadingsPlot(loadingsData = loadingsData, labelSize = input$labelSizePCAloadings, pointSize = input$pointSizePCAloadings, 
                                    legendSize = input$legendSizePCAloadings, title = "", fixAxis = input$correlationPlotPCA, declutterLabels = declutterLabels,
                                    Xcomp = input$XcomponentLoadingsPCA, Ycomp = input$YcomponentLoadingsPCA, labels = input$showLabelsLoadingsPCA, radInn = radInn,
                                    filterVar = filterVar)
    return(loadingsPlotPCA)
  },
  width = reactive(input$widthPCAloadings),
  height = reactive(input$heightPCAloadings),
  res = 150
  )
  
  ##########################
  # OUTPUT: Box plot
  ##########################
  output$boxPlot <- renderPlot({
    dataBoxplot <- inputData() %>% dplyr::select(input$FactorsBP, input$VariablesBP)
    colnames(dataBoxplot) = c("x", "y")
    
    boxPlotRender <- boxplotFunction(dataBoxplot = dataBoxplot, setcolors = col.pch()$col, rotate = input$rotateBoxplot, size_axis = input$sizeAxisBoxplot, 
                               size_label_axis = input$sizeLabelAxisBoxplot, height = input$heightBoxplot, width = input$widthBoxplot, 
                               x_label_angle = input$XlabelAngleBoxplot, order = input$orderBoxplot, hline = F, violinPlot = input$violinPlot,
                               dotPlot = input$dotPlot)
    return(boxPlotRender)
    
  },
  width = reactive(input$widthBoxplot),
  height = reactive(input$heightBoxplot),
  res = 150
  )
  
  ##########################
  # OUTPUT: ANOVA automatic
  ##########################
  
  output$ANOVAautomatic <- renderTable({
    req(input$runANOVA)
    isolate(
      if (input$originalDataANOVA == T) {
        data <- inputData()
      }else{
        data <- inputData()[-levelsRM(),]
      }
    )
    isolate(
      myANOVA <- ANOVA(data = data, factors = input$factorsANOVA, doInteractions = input$doInteractions, showSD = input$showSDsANOVA)
    )
    return(myANOVA)
  }, rownames = TRUE)

  ##########################
  # Reset buttons
  ##########################
  observeEvent(input$resetValuesPCAloadings, {
    shinyjs::reset("loadingsPCAbar")
  })
  
  observeEvent(input$resetValuesPCAscores, {
    shinyjs::reset("scoressPCAbar")
  })

  output$tableresults <- renderTable({
    input$summaryData_columns_selected
  })
  
  
  #########################
  # Function: scatter plot
  #########################
  scatterPlot <- function(scatterData, labelSize = 3, pointSize = 3, legendSize = 14, pch, col, titleScatter = "",
                          Xcomp = 1, Ycomp = 2, Y, labels = "None", segment.size = 0.1, declutterLabels = 10){
    scatter <- ggplot(scatterData, aes(x = scatterData[,Xcomp], y = scatterData[,Ycomp], label = labels))  +
      geom_point(aes(color = Y, shape = Y), size = pointSize) + 
      theme_bw() +
      theme(legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)) +
      theme(legend.title = element_text(size = legendSize),
            legend.text = element_text(size = legendSize / 1.3)) +
      labs(color = "Legend", shape = "Legend") +
      scale_shape_manual(values = pch) +
      scale_color_manual(values = col) +
      ggtitle(titleScatter) + theme(plot.title = element_text(hjust = 0.5)) +
      xlab(colnames(scatterData)[Xcomp]) + 
      ylab(colnames(scatterData)[Ycomp]) + 
      theme(axis.text.y = element_text(angle = 90)) +
      geom_hline(yintercept = 0, linetype="dashed") +
      geom_vline(xintercept = 0, linetype="dashed") 
    # Add labels of the samples
    if (length(labels) == 1) {
      scatter
    }else{
      scatter <- scatter + 
        geom_text_repel((aes(color = Y)), size = labelSize, show.legend = F, max.overlaps = declutterLabels, segment.size = segment.size, 
                        max.time = 0.5, max.iter = 2000, min.segment.length = 0.1, force = 1, force_pull = 5) 
    }
    
    return(scatter)
  }
  
  
  #########################
  # Function: loadings plot
  #########################
  loadingsPlot <- function(loadingsData, labelSize = 3, pointSize = 3, legendSize = 14, title = "", segment.size = 0.02,
                           Xcomp = 1, Ycomp = 2, Y, labels = F, fixAxis = T, radInn = 0.5, declutterLabels = 10, filterVar = F){
    if (filterVar == T) {
      radMatrix <- sqrt((loadingsData[,Xcomp])^2 + loadingsData[,Ycomp]^2) %>% as.data.frame()
      loadingsData <-  filter(loadingsData, radMatrix > radInn)
    }
    loadings <- ggplot(loadingsData, aes(x = loadingsData[,Xcomp], y = loadingsData[,Ycomp], label = rownames(loadingsData)))  +
      geom_point(color = "darkblue", size = pointSize) + 
      theme_bw() +
      ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
      xlab(colnames(loadingsData)[Xcomp]) + 
      ylab(colnames(loadingsData)[Ycomp]) + 
      theme(axis.text.y = element_text(angle = 90)) +
      geom_hline(yintercept = 0, linetype="dashed") +
      geom_vline(xintercept = 0, linetype="dashed")
    # Add labels of the variables
    if (labels == T) {
      loadings <- loadings +
        geom_text_repel(size = labelSize, show.legend = F, max.overlaps = declutterLabels, segment.size = segment.size, max.time = 1,
                        max.iter = 10000, min.segment.length = 0.02, force = 10, force_pull = 10) 
    }
    # Correlation  plot
    if (fixAxis == T) {
      loadings <- loadings + scale_y_continuous(limits=c(-1,1)) + scale_x_continuous(limits=c(-1,1)) +
        geom_circle(n = 80, aes(x0 = 0, y0 = 0, r = 1, linewidth = 0.3)) + 
        geom_circle(n = 80, aes(x0 = 0, y0 = 0, r = radInn, linewidth = 0.3))
    }
    
    return(loadings)
  }
  
  #########################
  # Function: Box plot
  #########################
  
  boxplotFunction = function(dataBoxplot, setcolors, rotate = F, size_axis, size_label_axis, height, width, x_label_angle, order, hline, yname = "Variable", violinPlot = F,
                             dotPlot = "None"){
    
    
    if (order == "increasing") {
      boxplot <- ggplot(dataBoxplot, aes(x=reorder(x,y), y=y, fill = x))
      }else{
      boxplot <- ggplot(dataBoxplot, aes(x=x, y=y, fill = x)) 
      }
    
    # Violin  or box plot
    if (violinPlot == T) {
      boxplot <- boxplot + geom_violin() 
    }else{
      boxplot <- boxplot + geom_boxplot()
    }
    
    # Include dots
    if (dotPlot == "Dot") {
      boxplot <- boxplot + geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(1))
    }
    if (dotPlot == "Jitter") {
      boxplot <- boxplot + geom_jitter(shape=16, position=position_jitter(0.2))
    }
    
    boxplot <- boxplot + theme(plot.title = element_text(hjust = 0.35, face = "bold"))  
    # scale_y_continuous(limits=c(-2,12.5))
    # 
    # if (exists("hline")) {
    #   boxplot = boxplot + geom_hline(yintercept = hline, colour = "darkred", linetype = "dashed")
    # }
    # 
    boxplot <- boxplot + labs(x="", y = yname) +
      theme(axis.text=element_text(size=size_axis, face="bold", colour = "black"),
            axis.title=element_text(size=size_label_axis,face="bold"))
    boxplot <- boxplot  + theme(legend.position="none")  #+ scale_fill_manual(values = setcolors)
  

    if (rotate == T) {
      boxplot <- boxplot + coord_flip() + theme(axis.text.y = element_text(angle = x_label_angle, hjust = 1),
                                                axis.text.x = element_text(angle = 0))
    } else{
      boxplot <- boxplot + theme(axis.text.x = element_text(angle = x_label_angle, hjust = 1),
                                 axis.text.y = element_text(angle = 90, hjust = 0.3))
    }
    return(boxplot)
  }
  
  
  #########################
  # Function: ANOVA
  #########################
  ANOVA <- function(data, factors, doInteractions = T, showSD = F){
    
    library(readxl); library(rstatix); library(doParallel); library(magrittr); library(tidyverse); library(agricolae); library(ascii)
    # factors <- colnames(data[,c(1)]) # SELECCIONAR CON LA TABLA
    # doInteractions <- F # Boton para seleccionar
    # data <- read_xlsx("./data.xlsx") %>% dplyr::select(., 1:30)  # Poner bien
    
    posResponses <- lapply(data, is.numeric) %>% unlist() %>% which(.)
    responses <- colnames(data)[posResponses]
    responsesRenamed <- paste0("Feat_", 1:length(responses)); colnames(data)[posResponses] = responsesRenamed # Changed to avoid naming problems
    restFactors <- data %>% dplyr::select(-posResponses) %>% colnames() 
    posFactors <- restFactors %in% factors %>% which(.)
    factors <- restFactors[posFactors] # Fix order of factors 
    ini <- length(restFactors) + 1
    fin <- ncol(data)
    
    ####################################
    # ANOVA and pos-hoc
    ####################################
    anovaResults <- list()
    for(i in ini:fin){
      variable <- responsesRenamed[i - ini + 1]
      
      # Get formula
      mainEffects <- paste0(factors, "+")[-length(factors)] %>% append(factors[length(factors)])
      if (doInteractions == F | length(factors) == 1) {
        .f <- c(variable, "~", mainEffects) %>% paste0(., collapse = " ") %>% as.formula()
      }else{
        interactions <- paste0(factors, "*")[-length(factors)] %>% append(factors[length(factors)])
        .f <- c(variable, "~", mainEffects, "+", interactions) %>% paste0(. ,collapse = " ") %>% as.formula()
      }
      
      # AOV
      modelAOV <- aov(.f, data = data)
      
      # ANOVA
      anovaResults[[i - ini + 1]] <- rstatix::anova_summary(modelAOV) 
      rmPosHoc <- which(anovaResults[[i - ini + 1]] %>% dplyr::select(p) > 0.05) # Pos-hoc letter to be removed
      
      # Main Effects
      posHocFeat <- list()
      for (j in 1:length(factors)) {
        posHocFeat[[j]] <- HSD.test(modelAOV, factors[j], group = T, console = F)$groups %>% mutate(rownames(.)) %>%
          setNames(., c("Feat", "groups", "names")) %>% arrange(., names) %>% t() %>% as.tibble() %>% slice(2)
        if (j %in% rmPosHoc) { # Remove letters from pos-hoc when p-value > 0.05
          posHocFeat[[j]] <- posHocFeat[[j]] %>% mutate_all(funs(str_replace(., ., " ")))
        }
      }
      
      # Interactions
      if (doInteractions == T & length(factors) > 1) {
        posHocFeat[[j+1]] <- HSD.test(modelAOV, factors, group = T, console = F)$groups %>% mutate(rownames(.)) %>%
          setNames(., c("Feat", "groups", "names")) %>% arrange(., names) %>% t() %>% as.tibble() %>% slice(2)
        if (c(j+1) %in% rmPosHoc) { # Remove letters from pos-hoc when p-value > 0.05
          posHocFeat[[j+1]] <- posHocFeat[[j+1]] %>% mutate_all(funs(str_replace(., ., " ")))
        }
      }
      
      # rbind in a list separated by effect type
      if (i == ini) {
        posHoc <- posHocFeat
      }else{
        posHoc <- map2(posHoc, posHocFeat, rbind)
      }
    }
    names(anovaResults) <- responsesRenamed
    
    # p-values to astherics
    pvaluesAst <- pvalues <- lapply(anovaResults, function(x) x%>% as_tibble() %>% dplyr::select(p)) %>% sapply(., unlist) %>% t() %>% round(.,3) 
    pvaluesAst[pvalues <= 0.05 & pvalues > 0.01] <- "*"
    pvaluesAst[pvalues <= 0.01 & pvalues > 0.001] <- "**"
    pvaluesAst[pvalues <= 0.001] <- "***"
    pvaluesAst[pvalues > 0.05] <- "ns"
    if (length(factors) == 1){pvaluesAst <- t(pvaluesAst)}
    
    # Main effects: means and SD
    tableMeans = tableSD = list()
    for (i in 1:length(posFactors)) {
      tableMeans[[i]] <- data %>% group_by(data %>% dplyr::select(posFactors[i])) %>% summarise_all(mean) %>% t() %>% as.data.frame() %>% 
        purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
      tableSD[[i]] <- data %>% group_by(data %>% dplyr::select(posFactors[i])) %>% summarise_all(sd) %>% t() %>% as.data.frame() %>% 
        purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
    }
    
    # Interaction effect (number of factors is > 1): means and SD
    if (length(posFactors) > 1) {
      tableMeans[[i+1]] <- data %>% group_by(data %>% dplyr::select(all_of(posFactors))) %>% summarise_all(mean) %>% unite("Interactions", 1:2, remove = T, sep = " x ") %>% 
        t() %>% as.data.frame() %>% purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
      tableSD[[i+1]] <- data %>% group_by(data %>% dplyr::select(all_of(posFactors))) %>% summarise_all(sd) %>% unite("Interactions", 1:2, remove = T, sep = " x ") %>%
        t() %>% as.data.frame() %>% purrr::set_names(slice(., 1)) %>% slice(-1) %>% drop_na()
    }
    
    for (i in 1:length(posHoc)) {
      # Add decimals
      if (showSD == T) {tableMaking <- tableMeans[[i]] %>% paste.matrix(., tableSD[[i]], sep = " ± ")}else
      {tableMaking <- tableMeans[[i]]}
      
      tableMaking <- tableMaking %>% paste.matrix(., posHoc[[i]], sep = "") %>% 
        cbind(., pvaluesAst[,i]) 
      colnames(tableMaking) <- c(tableMeans[[i]] %>% colnames(.), "p-value")
      if (i == 1) {
        tableANOVA <- tableMaking
      }else{
        tableANOVA <- cbind(tableANOVA, tableMaking)
      }
    }
    rownames(tableANOVA) <- responses
    return(tableANOVA)
  }
}

shinyApp(ui, server)














