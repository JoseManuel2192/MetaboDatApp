server = function(input, output){
  
# Loading data --------------------------  
  inputData  <- reactive({
    # req(input$loadData)
    # # load_file(name = input$loadData$name, path = input$loadData$datapath) %>% as.data.frame()
    read_excel("./data.xlsx") %>% as.data.frame()
  })

# Preparing data ------------------------------------------------------  
  
  ## Removing factors from X matrix ------------------------------------------
  factors <- reactive({
    Yfactors <- inputData() %>% dplyr::select(input$summaryData_columns_selected) %>% as.data.frame()
  })
  
  ## Removing levels from the selected factor ------------------------------------------
  levelsRM <- reactive({
    levels <- 10000000
    if (isTruthy(input$removeLevel)) {
      if (input$removeClass == "None") {levels <- 10000000} else{
        factor <- inputData() %>% dplyr::select(input$removeClass) %>% unlist() %>% as.character()
        levels <- which(factor %in% input$removeLevel)
      }
      if (length(levels) == 0){
        levels <- 10000000
      }
    }
    return(levels)
  })
  
  ## X matrix ------------------------------------------
  X <- reactive({
    X <- inputData() %>% dplyr::select(-c(input$summaryData_columns_selected))
  })
  
  ## Y matrix ------------------------------------------
  Y <- reactive({
    Yposition <- which(colnames(inputData()) == input$Yselected)
    inputData()[,Yposition] 
  })
  
  ## col.pch ------------------------------------------
  col.pch = reactive({
    levels = length(levels(as.factor(Y())))
    col = c("darkblue","red4","darkgreen", "darkorange","3", "4", "5", "1", paste0(8:200))[1:levels] 
    pch = c(16,17,18,15,8, 9,10, 11, 12,13,14,19:200)[1:levels] 
    results = list("col" = col, "pch" = pch)
    return(results)
  })
  
  ## Constant variables ------------------------------------------
  constantVars <- reactive({
    Xrml <- X()[-levelsRM(),]
    all.sd <- apply(Xrml, 2, function(X) sd(X))
    all.sd[all.sd == 0] %>% names()
  })

  # Render user interface ---------------------------------------------------
  
  # Column choices to select category of Y
  output$uiYselected <- renderUI({
    selectInput("Yselected", label = "Select Y",
                choices = inputData() %>% colnames()
    )
  })
  
  # Select labels to show in PCA
  output$uiLabelsPCA <- renderUI({
    selectInput("labelsPCA", label = "Select labels to display", selected = "None",
                choices = c("None", inputData() %>% colnames())
    )
  })
  
  # Select Class to remove
  output$uiRemoveClass <- renderUI({
    selectInput("removeClass", "Remove samples from any class?", selected = "None", multiple = F,
                choices = c("None", factors() %>% colnames()))
  })
  
  # Select levels from the class to remove
  output$uiRemoveLevel <- renderUI({
    req(input$removeClass)
    req(input$removeClass != "None")
    selectInput("removeLevel", "Which samples will be removed?", selected = "None", multiple = T,
                choices = inputData() %>% dplyr::select(input$removeClass) %>% unlist() %>% as.factor() %>% levels() %>% as.character()
    )
  })
  

  
  # Factors boxplot
  output$uiFactorsBP <- renderUI({
    selectInput("FactorsBP", "Select a factor", multiple = F,
                choices = c(factors() %>% colnames()))
  })
  
  # Variable boxplot 
  output$uiVariablesBP <- renderUI({
    selectInput("VariablesBP", "Select a variable", multiple = F,
                choices = c(X() %>% colnames()))
  })
  
  # Summary data report -----------------------------------------------------

  # Summary Table
  output$summaryData <- DT::renderDataTable({
    preselected <- lapply(inputData(), is.numeric) %>% unlist()
    preselectedColumns <- which(preselected == F) %>% as.numeric()
    datatable(head(inputData(), n = c(input$rowsDisplay,input$columnsDisplay)), extensions = 'Select', selection = list(target = "column", selected = preselectedColumns), 
              options = list(ordering = FALSE, searching = FALSE, pageLength = 15)) 
  })
  
  # Output summary table
  output$summaryDataReport <- renderPrint({
    MV <- which(is.na(X()[-levelsRM(),])) %>% length()
    NegV <- which(X()[-levelsRM(),] < 0) %>% length()
    
    print(paste("Number of missing values: ", MV))
    print(paste("Number of negative values: ", NegV))
    print(paste("The loaded has", X()[-levelsRM(),] %>% nrow(), "rows and", X() %>% ncol(), "columns"))
  })
  
  # Comment
  output$selectColumns <- renderText("Select columns to skip from the X-matrix in 
                                     the summary table. A preselection is automatically suggested")
  
  # Box plot ----------------------------------------------------------------
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
  
  # ANOVA ---------------------------------------------------------  
  
  # Factors ANOVA
  output$uiFactorsANOVA <- renderUI({
    selectInput("factorsANOVA", "Factors (between-subject variation)", multiple = T,
                choices = factors() %>% colnames())
  })
  
  # Data
  dataANOVA <- reactive({
    if (input$originalDataANOVA == T) {
      dataANOVA <- inputData()
    }else{
      dataANOVA <- inputData()[-levelsRM(),]
    }
    return(dataANOVA)
  })
  
  # Mean_SD
  mean_sd_ANOVA <- reactive({
    req(input$runANOVA)
    isolate(
      mean_sdValues <- doMeanSD(data = dataANOVA(), factors = input$factorsANOVA, foldChange = input$doFoldChange)
    )
    return(mean_sdValues)
  })
  
  # Decimal to reactiveValues object
  decimalsANOVA <- reactiveValues(data = NULL)
  observe({decimalsANOVA$data <- mean_sd_ANOVA()$decimals})
  
  # New value to decimalsANOVA if changed in the editable table
  observeEvent(input$decimalsANOVA_cell_edit, {
    info <- input$decimalsANOVA_cell_edit
    row <- info$row %>% as.numeric()
    value <- info$value %>% as.numeric()
    value <- ifelse(value < 0, 0, value)
    if (exists("value")) {
      decimalsANOVA$data[row,1] <- value
    }
  })
  
  # Fitting decimal format
  resultsMeanSD <- reactive({
    req(mean_sd_ANOVA())
    formatMeanSD(output_mean = mean_sd_ANOVA()$tableMeans, output_SD = mean_sd_ANOVA()$tableSD, decimalsFitted = decimalsANOVA$data)
  })
  
  # ANOVA and poshoc
  myANOVA <- reactive({
    req(input$runANOVA)
    isolate(
      ANOVA(data = dataANOVA(), factors = input$factorsANOVA, doInteractions = input$doInteractions)
    )
  })
  
  # ANOVA table compilation
  finalANOVA <- reactive({
    compileANOVA(tableMeans = resultsMeanSD()$tableMeans, tableSD = resultsMeanSD()$tableSD, posHoc = myANOVA()$posHoc, pvaluesAst = myANOVA()$pvaluesAst,
                 showSD = input$showSDsANOVA)
  })
  
  # TABLE ANOVA
  output$decimalsANOVA <- renderDT({
    req(mean_sd_ANOVA())
    decimalsTable <- decimalsANOVA$data
    colnames(decimalsTable) <- "Decimals"
    finalTable <- cbind(decimalsTable, finalANOVA() %>% rownames() %>% as.data.frame() %>% setNames("Variables"), finalANOVA())
    datatable(finalTable, editable = TRUE, rownames = F, width = "100%", height = "100%", options = list(
      searching = F, ordering = T, lengthChange  = F, lengthMenu = FALSE, pageLength = FALSE, paging = F, info = FALSE))
  })
  
  # Barplot -----------------------------------------------------------------
  barPlotServer("barPlot", choicesX = factors() %>% colnames(), choicesY = X() %>% colnames(), data = inputData())
  
  # PLSDA scores ------------------------------------------------------------
  plsdaServer("plsdaScores", X = X() %>% scale(), Y = Y() %>% as.factor(), col.pch = col.pch())
  
  # PCA ---------------------------------------------------------------------
  
  ## General settings --------------------------------------------------------- 
  
  # Labels size of scores plot if removeLevel exists
  output$uilabelSizePCAscores <- renderUI({
    req(input$labelsPCA != "None")
    column(6, numericInput("labelSizePCAscores", "Labels size", min = 0.1, max = 10, value = 2.5, step = 0.1))
  })
  
  # Lines size of scores plot if removeLevel exists
  output$uilineSizePCAscores <- renderUI({
    req(input$labelsPCA != "None")
    column(6, numericInput("lineSizePCAscores", "Lines size", min = 0, max = 10, value = 0.1, step = 0.1))
  })
  
  # Declutter labels
  output$uideclutterLabels <- renderUI({
    req(input$showLabelsLoadingsPCA == T)
    numericInput("declutterLabels", "Declutter labels", min = 0, value = 10, step = 0.1)
  })
  
  # Radios inner loadings plot
  output$uiradInn <- renderUI({
    req(input$correlationPlotPCA == T)
    numericInput("radInn", "Inner circle", min = 0, max = 1, value = 0.5, step = 0.1)
  })
  
  # Filter variables whithin the inner circle
  output$uifilterVar <- renderUI({
    req(input$correlationPlotPCA == T)
    checkboxInput("filterVar", "Filter vars", value = F)
  })
  
  ## Performing PCA --------------------------------------------------------- 
  my.pca <- reactive({
    Xpca <- X()[-levelsRM(),] %>% dplyr::select(-any_of(constantVars())) 
    ncomp <- ifelse(Xpca %>% dim() %>% min() < 20, Xpca %>% dim() %>% min() %>% as.numeric(), 20)
    my.pca <- mixOmics::pca(Xpca, ncomp = ncomp, center = TRUE, scale = TRUE)
    return(my.pca)
  })
  
  ## Scores plot --------------------------------------------------------- 
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
  
  observeEvent(input$resetValuesPCAscores, {
    shinyjs::reset("scoressPCAbar")
  })
  
  ## Loadings plot -----------------------------------------------------------
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
  
  observeEvent(input$resetValuesPCAloadings, {
    shinyjs::reset("loadingsPCAbar")
  })
  
  # Template ----------------------------------------------------------------
  # a <- reactiveValues(a = {data.frame(rep(0, 68))})
  
  # output$tableresults <- renderTable({
  #   b$data
  # })
  
  
}