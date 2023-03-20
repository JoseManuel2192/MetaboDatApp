
plsdaUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3, align = "center",
                   # Panel for optimization
                   conditionalPanel(condition = "input.PLSDA == 1", 
                                    h4("PLS-DA settings"), br(),
                                    fluidRow(
                                      column(6, uiOutput(ns("uincompMax"))),
                                      column(6, selectInput(ns("type"), "Cross Validation re-sampling", choices = c("loo", "Mfold"), selected = "loo"))
                                    ),
                                    uiOutput(ns("uitypePartition")),
                                    fluidRow(
                                      column(6, uiOutput(ns("uifolds"))),
                                      column(6, uiOutput(ns("uirep")))
                                    ),
                                    uiOutput(ns("uiclassErrorSelection")),
                                    actionButton(ns("runPLSDAopt"), "Run cross-validation"),
                                    br(), br(),
                                    uiOutput(ns("uincompOpt")),
                                    h4("....................................................."),
                                    tableOutput(ns("optimalN")),
                                    # h4("....................................................."),
                                    ns = ns, align = "center"
                   ),
                   
                   # Panel for scores plot
                   conditionalPanel(condition = "input.PLSDA == 2",
                                    # h4("....................................................."),

                                    h4("Figure settings"), br(),
                                    settingScatterPlots(
                                      comp1 = ns("comp1"), comp2 = ns("comp2"), title = ns("title"), sizeTitle = ns("sizeTitle"), height = ns("height"),
                                      width = ns("width"), sizeLabels = ns("sizeLabels"), sizePoints = ns("sizePoints"), sizeXYaxis = ns("sizeXYaxis"), 
                                      sizeLegendTitle = ns("sizeLegendTitle"), sizeLegendLevels = ns("sizeLegendLevels"), declutterLabels = ns("declutterLabels"), 
                                      sizeSegment = ns("sizeSegment"), showCentroid = ns("showCentroid"), showEllipses = ns("showEllipses"),  sizeXYlabel = ns("sizeXYlabel")
                                    ),
                                    ns = ns
                   ),
                   conditionalPanel(condition = "input.PLSDA == 3",
                                    fluidRow(
                                      column(6, numericInput(ns("heightPredPlot"), "Height Plot", value = 900, step = 100, min = 0)), 
                                      column(6, numericInput(ns("widthPredPlot"), "Width Plot", value = 600, step = 100, min = 0)) 
                                    ),
                                    ns = ns
                   )
      ),
      mainPanel(width = 9,
                tabsetPanel(id = ns("PLSDA"),
                            tabPanel("Optimization & Validation", value = 1, align = "center",
                                     fluidRow(
                                       column(5, withSpinner(plotOutput(ns("errorRate")), type = 4, size = 1)),
                                       column(7, withSpinner(plotOutput(ns("accuracy")), type = 4, size = 1))
                                     ),
                                     withSpinner(plotOutput(ns("classError")), type = 4, size = 1)
                                     
                            ),
                            # column(6, withSpinner(tableOutput(ns("errorclassTable")), type = 4, size = 1))
                            tabPanel("Scores Plot", value = 2,
                                     plotOutput(ns("scatterPlot"))
                            ),
                            tabPanel("Debugging panel", value = 3,
                                     # tableOutput(ns("Debugging")), value = 3
                                     plotOutput(ns("predictionsPlot"))
                            )
                )
      )
    )
  )
}

plsdaServer <- function(id, X, Y, col.pch) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Debugging ---------------------------------------------------------------
      # output$Debugging <- renderTable({
      #   req(input$runPLSDAopt)
      #   ncompOptimal$data
      # })
      
      output$uincompMax <- renderUI({
        valueProp <- c(ncol(X), nrow(Y)) %>% 
          max()
        valueProp <- ifelse(valueProp > 8, 8, valueProp)
        numericInput(
          session$ns("ncompMax"), "N components to optimize", 
          value = valueProp, min = 1, step = 1)
      })
      output$uifolds <- renderUI({
        req(input$type)
        if (input$type  == "Mfold") {
          numericInput(session$ns("folds"), "Folds", value = 6, 
                       min = 1, step = 1)}
      })
      output$uirep <- renderUI({
        req(input$type)
        if (input$type  == "Mfold") {
          numericInput(session$ns("rep"), "Repetitions", 
                       min = 0, value = 30, step = 10)}
      })
      
      output$uitypePartition <- renderUI({
        req(input$type)
        if (input$type  == "Mfold") {
          selectInput(session$ns("typePartition"), "Select partition", 
                      choices = c("stratified", "basic", "grouped", "blocked"))}
      })
      
      # Perf optimization (single cross validation) of PLS-DA 
      diagnosticValues <- reactiveValues(data = NULL)
      observeEvent(input$runPLSDAopt, {
        Y <- Y %>% as.factor()
        # Partitions 
        my.partition <- doPartition(Y, input$type,input$typePartition, 
                                    input$folds, input$rep)
        folds <- my.partition$folds
        rep <- my.partition$rep
        # PLS-DA perf: obtaining assignation matrix 
        plsdaPerf <- plsdaCV(X,Y,rep, my.partition$partition, input$ncompMax)
        # Converting assignation matrix into diagnostics
        diagnostics <- list()
        for (i in 1:rep) {
          diagnosticValues$data[[i]] <- getErrors(map(plsdaPerf, i), Y)
        }
      })
      
      # Render error rate plot
      BERmean <- reactiveValues(data = NULL)
      output$errorRate <- renderPlot({
        req(input$runPLSDAopt)
        BERmean$data <- do.call(rbind, map(diagnosticValues$data, 1)) %>% 
          group_by(Component) %>% 
          summarise_each(.,mean)
        plotDiagnostic(BERmean$data, "BER", show.legend = F, show.sd)
      }, width = 500, height = 400, res = 120)
      
      # Render error rate plot
      accuracyMean <- reactiveValues(data = NULL)
      output$accuracy <- renderPlot({
        req(input$runPLSDAopt)
        accuracyMean$data <- do.call(rbind, map(diagnosticValues$data, 3)) %>% 
          group_by(Component) %>% 
          summarise_each(.,mean)
        plotDiagnostic(accuracyMean$data, "Accuracy")
      }, width = 630, height = 400, res = 120)
      
      # Render error rate plot
      classErrorMean <- reactiveValues(data = NULL)
      output$classError <- renderPlot({
        req(input$runPLSDAopt)
        classErrorMean$data <- do.call(rbind, map(diagnosticValues$data, 2)) %>% 
          group_by(distance, class) %>% 
          summarise_each(.,mean) %>% 
          arrange(class) %>%
          filter(class == input$classErrorSelection) %>% 
          dplyr::select(-class) %>% 
          remove_rownames %>% 
          column_to_rownames(var="distance") %>% 
          rotate_df(rn = "Component") %>% 
          as_tibble() %>%
          dplyr::mutate(Component = as.numeric(Component))
        plotDiagnostic(classErrorMean$data, str_glue("{input$classErrorSelection} class error"))
      }, width = 630, height = 400, res = 120)
      
      # UI: class error to show
      output$uiclassErrorSelection <- renderUI({
        selectInput(session$ns("classErrorSelection"), "Error by class", choices = levels(Y))
      })
      
      # UI: optimal number of components
      
      # Optimal number of components
      ncompOptimal <- reactive({
        noptimalBER <- findOptimalN(BERmean$data, "min")
        noptimalErrorClass <- findOptimalN(classErrorMean$data, "min")
        noptimalAccuracy <- findOptimalN(accuracyMean$data, "max")
        rbind(noptimalBER, noptimalErrorClass, noptimalAccuracy) %>%
          set_rownames(c("BER", str_glue("Error {input$classErrorSelection} class"), 
                         "Accuracy"))
      })
      
      output$optimalN <- renderTable({
        req(input$runPLSDAopt)
        ncompOptimal() 
      })
      
      # Select optimum number of components (automatic suggestion)
      output$uincompOpt <- renderUI({
        req(input$runPLSDAopt)
        selectInput(session$ns("ncompOpt"), "Optimum N comp", choices = 1:input$ncompMax, 
                    selected = ncompOptimal()[1,1])
      })
      
      output$errorclassTable <- renderTable({
        req(input$runPLSDAopt)
        classErrorMean$data
      })
      
      output$scatterPlot <- renderPlot({
        req(input$runPLSDAopt)
        my_plsda <- mixOmics::plsda(X,Y, ncomp = as.numeric(input$ncompOpt))
        scatterData <- my_plsda$variates$X
        expl.variance <- my_plsda$prop_expl_var$X * 100
        names(expl.variance) <- gsub("comp", x = expl.variance %>% names(), replacement = "PC")
        colnames(scatterData) = paste0(names(expl.variance), ": ", as.character(round(expl.variance, 2)),
                                       c("% of explained variance"))
        plotScores <- scatterPlotPLS(scatterData = scatterData, Xcomp = input$comp1, Ycomp = input$comp2, 
                                     title = input$title, sizeTitle = input$sizeTitle,
                                     sizeLabels = input$sizeLabels, sizePoints = input$sizePoints, 
                                     sizeXYaxis = input$sizeXYaxis, sizeXYlabel = input$sizeXYlabel,
                                     sizeLegendTitle = input$sizeLegendTitle, sizeLegendLevels = input$sizeLegendLevels, 
                                     declutterLabels = input$declutterLabels,
                                     sizeSegment = input$sizeSegment, showCentroid = input$showCentroid, 
                                     showEllipses = input$showEllipses, Y = Y, labels = "None",
                                     col = col.pch$col, pch = col.pch$pch
        )
        plotScores
      },
      res = 150,
      height = reactive(input$height),
      width = reactive(input$width)
      )
      
      output$predictionsPlot <- renderPlot({
        dist <- "max.dist.predictions"
        idc = Y
        ncompOpt = 4
        Y <- Y %>% as.factor()
        req(input$runPLSDAopt)
        my.partition <- doPartition(Y, input$type, input$typePartition, 
                                    input$folds, input$rep)
        folds <- my.partition$folds
        rep <- my.partition$rep
        plsdaPerf <- plsdaCV(X, Y, rep, my.partition$partition, input$ncompMax)
        
        predictions <- getPredictionMatrices(plsdaPerf, Y, ncompOpt, dist, idc)
        colourPredictions <- pivot_longer(predictions$colours, cols = -c(ID, order))
        dataPredictions <- pivot_longer(predictions$predictionSummary, cols = -c(ID, order)) %>%
          dplyr::mutate(dataPredictions, ID = factor(ID, levels = unique(ID)))
        
        Ymeans <- calculateMeans(rep, as.factor(dataPredictions$name))
        dataPredictions <- dataPredictions %>% 
          dplyr::mutate(.,
                 "mean" = Ymeans,
                 "min" = mean - value/2, 
                 "max" = mean + value/2,
                 "colour" = pull(colourPredictions, value)) %>% 
          arrange(name)
        
        sizeYaxis = 3
        sizeXaxis = 10
        sizeLegendTitle = 16
        sizeLegendLevels = 14
        sizeSegment = 3
        
        predictionPlot(dataPredictions, Y, sizeXaxis, sizeYaxis, sizeLegendTitle, 
                       sizeLegendLevels, sizeSegment, rep)
      }, res = 150,
      height = reactive(input$heightPredPlot),
      width = reactive(input$widthPredPlot)
      )
      
      # output$Debugging <- renderTable({
      #   req(input$runPLSDAopt)
      #   dist <- "max.dist.predictions"
      #   IDshow = Y
      #   Y <- Y %>% as.factor()
      #   req(input$runPLSDAopt)
      #   my.partition <- doPartition(Y = Y, type = input$type, typePartition = input$typePartition, folds = input$folds, rep = input$rep)
      #   folds <- my.partition$folds
      #   rep <- my.partition$rep
      #   plsdaPerf <- plsdaCV(X = X, Y = Y, rep = rep, partition = my.partition$partition, ncompMax = input$ncompMax)
      #   
      #   predictions <- getPredictionMatrices(plsdaPerf = plsdaPerf, Y = Y, ncompOpt = input$ncompOpt, dist = dist, IDshow = IDshow)
      #   colourPredictions <- pivot_longer(predictions$colours, cols = -c(ID, order))
      #   dataPredictions <- pivot_longer(predictions$predictionSummary, cols = -c(ID, order)) %>% 
      #     dplyr::mutate(dataPredictions, ID = factor(ID, levels = unique(ID)))
      #   
      #   Ymeans <- calculateMeans(rep = rep, Y = dataPredictions$name %>% as.factor())
      #   dataPredictions <- dataPredictions %>% dplyr::mutate(.,"mean" = Ymeans) %>% dplyr::mutate(., "min" = mean - value/2, "max" = mean + value/2) %>%
      #     dplyr::mutate(., "colour" = colourPredictions %>% pull(value)) %>% arrange(name)
      #   predictions$predictionSummary
      # })
      
    }
  )
}


# scatterPlot function ----------------------------------------------------
scatterPlotPLS <- function(scatterData, sizeLabels = 3, sizePoints = 3, 
                           col, pch, title = "", sizeTitle = 30,
                           Xcomp = 1, Ycomp = 2, Y, labels = "None", 
                           sizeSegment = 0.1, declutterLabels = 10, showCentroid = F, 
                           showEllipses = F, sizeXYaxis = 12, sizeXYlabel = 12, 
                           sizeLegendTitle = 12, sizeLegendLevels = 12){
  
  # This allows to plot 1 component when setting Xcomp and Ycomp to 1
  if (Xcomp == 1 && Ycomp == 1) {
    scatterDataplot <- scatterData %>% as.data.frame() %>% 
      dplyr::mutate(., Y) %>% 
      arrange(Y) %>% 
      dplyr::mutate(., 1:nrow(.))
    
    colnames(scatterDataplot)[ncol(scatterDataplot)] <- "Sample"
    Xcomp <- scatterDataplot %>% 
      ncol()
    rownames(scatterDataplot) <- scatterDataplot %>% 
      pull(Sample) 
    # scatterDataplot <- scatterData %>% dplyr::mutate(., Y) %>% arrange(Y) %>% as.data.frame()
    plotOneComp = T
  }else{
    plotOneComp = F
    scatterData <- scatterData %>% 
      as.data.frame()
    scatterDataplot <- scatterData  %>% 
      as.data.frame() %>% 
      dplyr::mutate(., Y)
  }
  
  scatter <- ggplot(scatterDataplot, aes(x = scatterDataplot[,Xcomp], 
                                         y = scatterDataplot[,Ycomp], label = labels))  +
    geom_point(aes(color = Y, shape = Y), size = sizePoints) + 
    
    labs(color = "Legend", shape = "Legend") +
    scale_shape_manual(values = pch) +
    scale_color_manual(values = col) +
    
    xlab(colnames(scatterDataplot)[Xcomp]) + 
    ylab(colnames(scatterDataplot)[Ycomp]) + 
    
    geom_hline(yintercept = 0, linetype="dashed") +
    geom_vline(xintercept = 0, linetype="dashed") +
    
    theme_bw() +
    theme(legend.background = element_rect(linetype = 1, linewidth = 0.5, colour = 1)) +
    
    ggtitle(title) + 
    theme(title = element_text(size = sizeTitle)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    
    theme(axis.text = element_text(size = sizeXYaxis, colour = "black")) +
    theme(axis.title = element_text(size = sizeXYlabel)) +
    theme(axis.text.x = element_text(angle = 0, size = sizeXYaxis)) +
    theme(axis.text.y = element_text(angle = 90, size = sizeXYaxis)) +
    
    theme(legend.title = element_text(size = sizeLegendTitle)) + 
    theme(legend.text = element_text(size = sizeLegendLevels))       
  
  # Add labels of the samples
  if (length(labels) == 1) {
    scatter
  }else{
    scatter <- scatter + 
      geom_text_repel((aes(color = Y)), size = sizeLabels, show.legend = F, 
                      max.overlaps = declutterLabels, segment.size = sizeSegment, 
                      max.time = 0.5, max.iter = 2000, min.segment.length = 0.1, 
                      force = 1, force_pull = 5) 
  }
  if (showCentroid == T && plotOneComp == F) {
    centroids <- scatterData[,c(Xcomp, Ycomp)] %>% 
      dplyr::mutate(., Y) %>% 
      group_by(Y) %>% 
      summarise_if(is.numeric, mean) %>% 
      as.data.frame()
    scatterCentroids <- scatterData %>% 
      dplyr::mutate(., Y) %>% 
      merge(., centroids, by = "Y")
    colnames(scatterCentroids) <- c("Y", colnames(scatterData), "centroid.X", "centroid.Y")
    
    scatter <- scatter + geom_point(data = scatterCentroids, 
                                    aes(x = centroid.X, y = centroid.Y, color = Y), 
                                    size = sizePoints/3) +
      geom_segment(data = scatterCentroids, 
                   aes(x = centroid.X, y = centroid.Y, xend = scatterCentroids[,Xcomp + 1], 
                                                yend = scatterCentroids[,Ycomp + 1], color = Y), 
                   show.legend = F)
  }
  if (showEllipses == T & plotOneComp == F) {
    scatter <- scatter + stat_ellipse(aes(color = Y, fill = Y), 
                                      geom = "polygon", alpha = 0.2, show.legend = F, 
                                      level = 0.95, type = "norm")
  }
  return(scatter)
}



# Partition calibration / validation  -------------------------------------
doPartition <- function(Y, type = "loo", typePartition = "stratified", 
                        folds = 6, rep = 10){
  if (type == "loo") {
    folds <- length(Y)
    rep <- 1
    my.partition <- create_folds(y = Y, k = folds, m_rep = rep, 
                                 type = typePartition)
  } else{
    if (type == "Mfold") {
      my.partition <- create_folds(y = Y, k = folds, m_rep = rep, 
                                   type = typePartition)
    }
  }
  return(list("partition" = my.partition, "folds" = folds, "rep" = rep))
}

# Extracting assignation from PLS-DA --------------------------------------
plsAssignations <- function(X, Y, partition, ncompMax){
  # ----------------- Partitions ----------------- #
  Xtrain <- X %>% 
    as_tibble() %>% 
    slice(partition)
  Xtest <- X %>% 
    as_tibble() %>% 
    slice(-partition)
  Ytrain <- Y[partition]
  Ytest <- Y[-partition]
  # ----------------- Perform PLS-DA model ----------------- #
  my.plsda <- mixOmics::plsda(Xtrain,Ytrain, ncomp = ncompMax, scale = F)
  predictions <- predict(my.plsda, Xtest)
  # ----------------- Obtain predictions from different distances ----------------- #
  max.dist <- predictions$MajorityVote$max.dist %>% 
    as_tibble() %>% 
    dplyr::mutate("Index" = (1:length(Y))[-partition])
  centroids.dist <- predictions$MajorityVote$centroids.dist %>% 
    as_tibble() %>% 
    dplyr::mutate("Index" = (1:length(Y))[-partition])
  mahalanobis.dist <- predictions$MajorityVote$mahalanobis.dist %>% 
    as_tibble() %>% 
    dplyr::mutate("Index" = (1:length(Y))[-partition])
  all.dist <- list("max.dist" = max.dist, "centroids.dist" = centroids.dist, 
                   "mahalanobis.dist" = mahalanobis.dist)
  return("assignations" = all.dist)
}

# Iterations of CV ---------------------------------------------------------
plsdaCV <- function(X, Y, rep, partition, ncompMax){
  withProgress(message = "Cross validation", value = 0, {
    plsAsses <- lapply(partition, function(x){
      incProgress(1/length(x))
      plsAssignations(X, Y, partition = x, ncompMax = ncompMax)
    })
  })
  
  fSplit <- rep(1:rep, length(Y)) %>% 
    sort(.)
  max.dist.Assign <- do.call(rbind, map(plsAsses, 1)) %>% 
    split(., f = fSplit) %>% 
    lapply(., function(x){x %>% arrange(Index) %>% dplyr::select(-Index)})
  centroids.dist.Assign <- do.call(rbind, map(plsAsses, 2)) %>% 
    split(., f = fSplit) %>% 
    lapply(., function(x){x %>% arrange(Index) %>% dplyr::select(-Index)})
  mahalanobis.dist.Assign <- do.call(rbind, map(plsAsses, 3)) %>% 
    split(., f = fSplit) %>% lapply(., function(x){x %>% 
        arrange(Index) %>% dplyr::select(-Index)})
  names(max.dist.Assign) = names(centroids.dist.Assign) = 
    names(mahalanobis.dist.Assign) = paste0("rep", 1:length(max.dist.Assign))
  
  all.dist.Assign <- list("max" = max.dist.Assign, 
                          "centroids" = centroids.dist.Assign, 
                          "mahalanobis" = mahalanobis.dist.Assign)
  
  return(all.dist.Assign)
}

# Getting errors  ---------------------------------------------------------
getErrors <- function(assignations, Y){
  
  # Confusion matrix for each distance
  conf.matrix.max <- alply(assignations$max %>% as.matrix, 2, 
                           function(X){table(X,Y)})
  conf.matrix.centroids <- alply(assignations$centroids %>% as.matrix, 2, 
                                 function(X){table(X,Y)})
  conf.matrix.mahalanobis <- alply(assignations$mahalanobis %>% as.matrix, 2, 
                                   function(X){table(X,Y)})
  
  # Obtaining diagnostics for each distance
  BER <- list("max.dist" = lapply(conf.matrix.max, BERF) %>% 
                do.call(cbind,.),
              "centroids.dist" = lapply(conf.matrix.centroids, BERF) %>% 
                do.call(cbind,.),
              "mahalanobis.dist" = lapply(conf.matrix.mahalanobis, BERF) %>% 
                do.call(cbind,.))
  classError <- list("max.dist" = lapply(conf.matrix.max, ClassErrorF) %>% 
                       do.call(cbind, .) %>% 
                       set_colnames(1:ncol(.)),
                     "centroids.dist" = lapply(conf.matrix.centroids, ClassErrorF) %>% 
                       do.call(cbind, .) %>% set_colnames(1:ncol(.)),
                     "mahalanobis.dist" = lapply(conf.matrix.mahalanobis, ClassErrorF) %>% 
                       do.call(cbind, .) %>% 
                       set_colnames(1:ncol(.)))
  accuracy <- list("max.dist" = lapply(conf.matrix.max, accuracyF) %>% 
                     do.call(cbind, .),
                   "centroids.dist" = lapply(conf.matrix.centroids, accuracyF) %>% 
                     do.call(cbind, .),
                   "mahalanobis.dist" = lapply(conf.matrix.mahalanobis, accuracyF) %>% 
                     do.call(cbind, .))
  
  # Redefine variables -> results in a matrix
  BER <- do.call(rbind, BER) %>% 
    set_rownames(., c("max", "centroids", "mahalanobis")) %>%
    t() %>% 
    as.data.frame() %>% 
    dplyr::mutate("Component" = 1:nrow(.), .before = max)
  classError <- classError %>% 
    map(., function(x) apply(x, 2, unlist)) %>% 
    do.call(rbind, .) %>% as.data.frame() %>%
    dplyr::mutate(., "class" = rep(rownames(.)[1:nlevels(Y)], 3)) %>% 
    as_tibble() %>%
    dplyr::mutate(., "distance" = c(rep("max", nlevels(Y)), 
                                    rep("centroid", nlevels(Y)), 
                                    rep("mahalanobis", nlevels(Y)))) 
  accuracy <- do.call(rbind, accuracy) %>% 
    set_rownames(., c("max", "centroids", "mahalanobis")) %>%
    t() %>% 
    as.data.frame() %>% 
    dplyr::mutate("Component" = 1:nrow(.), .before = max)
  
  return(list("BER" = BER, "classError" = classError, "accuracy" = accuracy))
}

# Accuracy  ---------------------------------------------------------------
accuracyF <- function(conf.matrix){
  cp <- 0
  for (i in 1:nrow(conf.matrix)) {cp <- conf.matrix[i,i] %>% sum(., cp)}
  cp / sum(conf.matrix)
}

# BER  ---------------------------------------------------------------
BERF <- function(conf.matrix){
  BERclass <- list()
  for (i in 1:ncol(conf.matrix)) {
    wrong <- conf.matrix[-i,i] %>% sum
    total <- sum(conf.matrix[,i])
    BERclass[[i]] <- wrong / total
  }
  BER <- do.call(cbind, BERclass) %>% mean()
  return(BER)
}

# Class error  ---------------------------------------------------------------
ClassErrorF <- function(conf.matrix){
  BERclass <- list()
  for (i in 1:ncol(conf.matrix)) {
    wrong <- conf.matrix[-i,i] %>% sum
    total <- sum(conf.matrix[,i])
    BERclass[[i]] <- wrong / total %>% as.numeric()
  }
  BERclass <- BERclass %>% set_names(conf.matrix %>% colnames()) 
  return(BERclass)
}

# Finding optimal N COMP  ---------------------------------------------------------------
findOptimalN <- function(objectOpt, objective){
  if (objective == "min") {
    opt <- objectOpt[,-1]
  }else{
    opt <- apply(objectOpt[,-1], 2, function(x) 1-x)
  }
  apply(opt, 2, function(opt){
    if (min(opt) > 0.05) {
      ncomp = which(opt == min(opt))[1]
    }else{
      ncomp = which(opt <= 0.05)[1]
    }
    if(length(ncomp) > 1){ncomp = ncomp[1]}
    return(ncomp)
  })
}

# Plotting diagnostics  ---------------------------------------------------------------
plotDiagnostic <- function(diagnostic, title = "", show.legend = T, show.sd = F){
  diagnosticPlot <- pivot_longer(diagnostic, cols = -Component, names_to = "Distance") %>% 
    as.data.frame()
  gg <- ggplot(data = diagnosticPlot, aes(x = factor(Component), y = value, group = Distance)) +
    ggtitle(title) +
    theme(title = element_text(size = 10)) +
    theme(plot.title = element_text(hjust = 0.5)) 
  if (show.legend == F) {
    gg <- gg + geom_point(aes(color=Distance), show.legend = F) +
      geom_line(aes(color=Distance), show.legend = F) +
      xlab("Component")
  }else{
    gg <- gg + geom_point(aes(color=Distance)) +
      geom_line(aes(color=Distance)) +
      xlab("Component")
  }
  gg
}

# Matrices for prediction plots  ---------------------------------------------------------------
getPredictionMatrices <- function(plsdaPerf, Y, ncompOpt, dist, IDshow){
  predictions <- map(plsdaPerf, function(x){map(x, function(x) {
    x <- x %>% 
      as_tibble() %>% 
      dplyr::select(any_of(ncompOpt)) %>% 
      cbind(as_tibble(Y)) %>% 
      dplyr::arrange(., value) %>% 
      pull(1) %>% 
      as.character()
  })})
  Y <- sort(Y)
  
  max.dist.predictions = centroids.dist.predictions = mahalanobis.dist.predictions = list()
  for (i in 1:length(Y)) {
    max.dist.predictions[[i]] <- map(predictions$max, i) %>% 
      do.call(cbind, .)
    centroids.dist.predictions[[i]] <- map(predictions$centroids, i) %>% 
      do.call(cbind, .)
    mahalanobis.dist.predictions[[i]] <- map(predictions$mahalanobis, i) %>% 
      do.call(cbind, .)
  }
  all.predictions <- list(
    "max.dist.predictions" = do.call(rbind, max.dist.predictions) %>% 
      as_tibble(),
    "centroids.dist.predictions" = do.call(rbind, centroids.dist.predictions) %>% 
      as_tibble(),
    "mahalanobis.dist.predictions" = do.call(rbind, mahalanobis.dist.predictions) %>% 
      as_tibble()
  )
  
  dist.prediction.selected <- all.predictions %>% 
    extract2(dist)
  predictionSummary <- apply(dist.prediction.selected, 1, function(x){
    sumLevels = c()
    for (i in 1:nlevels(Y)) {
      sumLevels[i] <- sum(x == levels(Y)[i])
    }
    return(sumLevels)
  }) %>% t() %>% set_colnames(levels(Y)) %>% as_tibble() %>% 
    dplyr::mutate(., "ID" = IDshow, "order" = 1:length(Y))
  
  colours <- predictionSummary %>% cbind(., Y)  
  for (i in 1:nlevels(Y)) {
    green <- which(pull(colours, Y) == levels(Y)[i])
    red <- which(pull(colours, Y) != levels(Y)[i])
    colours[,i][green] = "Correctly classified"
    colours[,i][red] = "Wrongly classified"
  }
  colours <- colours %>% 
    as_tibble() %>% 
    dplyr::select(-Y)
  
  return(list("predictionSummary" = predictionSummary, "colours" = colours))
}

# Position for prediction plots  ---------------------------------------------------------------
calculateMeans <- function(rep, Y){
  positionFirst <- rep/2 + 1
  positionRest = c()
  for (i in 1:(length(levels(Y))-1)) {
    positionRest[i] <- rep * i + i/2 + positionFirst
  }
  means <- c(positionFirst, positionRest)
  Ymeans <- Y %>% as.character()
  for (i in 1:length(means)) {
    Ymeans[Ymeans == levels(Y)[i]] <- means[i]
  }
  Ymeans <- Ymeans %>% as.numeric
  return(Ymeans)
}

# Prediction plots  ---------------------------------------------------------------
predictionPlot <- function(dataPredictions, Y, sizeXaxis, sizeYaxis, sizeLegendTitle, sizeLegendLevels, sizeSegment, rep){
  breaksPlot <- dataPredictions$mean %>% 
    as.factor() %>% 
    levels() %>% 
    as.numeric()
  pp <- ggplot(dataPredictions) +
    geom_segment(aes(x = min, y = order, xend = max, yend = order, color = colour), size = sizeSegment) + 
    scale_y_continuous(breaks = dataPredictions$order, labels = dataPredictions$ID) +
    scale_x_continuous(breaks = breaksPlot, labels = levels(Y)) +
    scale_color_manual(values = c("darkgreen", "darkred")) +
    theme_bw() +
    theme(axis.text.x = element_text(size = sizeXaxis)) +
    theme(axis.text.y = element_text(size = sizeYaxis)) +
    theme(legend.title = element_text(size = sizeLegendTitle)) + 
    theme(legend.text = element_text(size = sizeLegendLevels)) +
    guides(col = guide_legend(title = "Legend")) + 
    xlab("")
  
  cutoffX = 1; cutoffY = 0
  for (i in 1:(nlevels(as.factor(Y))-1)) {
    cutoffY <- cutoffY + summary(as.factor(Y))[i]
    if (i > 1) {incr = rep + 0.5} else{incr = rep}
    cutoffX <- cutoffX + incr  
    pp <- pp + geom_hline(yintercept = cutoffY + 0.5, linetype = "dashed")
    pp <- pp + geom_vline(xintercept = cutoffX + 0.25, linetype = "dashed")
  }
  return(pp)
}


