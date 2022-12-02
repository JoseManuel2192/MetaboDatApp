########################################################################
########################################################################
plsdaUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3, align = "center",
                   # Panel for optimization
                   conditionalPanel(condition = "input.PLSDA == 1", 
                                    h4("PLS-DA settings"), br(),
                                    # numericInput(ns("maxNcomp"), label = "N components to optimize", value = 2, min = 1, step = 1),
                                    uiOutput(ns("uimaxNcomp")),
                                    actionButton(ns("runPLSDAopt"), "Run cross-validation: component optimization"),
                                    br(), br(),
                                    uiOutput(ns("uincompOpt")),
                                    ns = ns
                   ),
                   
                   # Panel for scores plot
                   conditionalPanel(condition = "input.PLSDA  == 2",
                                    # uiOutput(ns("uirunPLSDA")),
                                    # h4("....................................................."),
                                    ################################
                                    h4("Figure settings"), br(),
                                    settingScatterPlots(
                                      comp1 = ns("comp1"), comp2 = ns("comp2"), title = ns("title"), sizeTitle = ns("sizeTitle"), height = ns("height"),
                                      width = ns("width"), sizeLabels = ns("sizeLabels"), sizePoints = ns("sizePoints"), sizeXYaxis = ns("sizeXYaxis"), 
                                      sizeLegendTitle = ns("sizeLegendTitle"), sizeLegendLevels = ns("sizeLegendLevels"), declutterLabels = ns("declutterLabels"), 
                                      sizeSegment = ns("sizeSegment"), showCentroid = ns("showCentroid"), showEllipses = ns("showEllipses"),  sizeXYlabel = ns("sizeXYlabel")
                                    ),
                                    ns = ns
                   )                 
      ),
      mainPanel(width = 9,
                tabsetPanel(id = ns("PLSDA"),
                  tabPanel("Optimization",
                           plotOutput(ns("errorRate")), value = 1
                  ),
                  
                  tabPanel("Scores Plot",
                           plotOutput(ns("scatterPlot")), value = 2
                  ),
                  tabPanel("Debugging panel",
                           tableOutput(ns("Debugging")), value = 3
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

      output$Debugging <- renderTable({input$PLSDA %>% as.character()})
      
      output$uimaxNcomp <- renderUI({
        valueProp <- c(ncol(X), nrow(Y)) %>% max
        valueProp <- ifelse(valueProp > 8, 8, valueProp)
        numericInput(session$ns("maxNcomp"), label = "N components to optimize", value = valueProp, min = 1, step = 1)
      })
      
      # Perf optimization (single cross validation) of PLS-DA
      perf_plsda <- reactiveValues(data = NULL)
      observeEvent(input$runPLSDAopt, {
        my_plsda <- plsda(X,Y, ncomp = input$maxNcomp)
        perf_plsda$perf = perf(my_plsda, auc = TRUE, progressBar = TRUE, validation = "loo")
      })
      
      # Render error rate plot
      output$errorRate <- renderPlot({
        req(perf_plsda$perf)
        plot(perf_plsda$perf)
      }, width = 800, height = 500, res = 120)
      
      # UI: run optimization plsda bottom
      # output$uirunPLSDA <- renderUI({
      #   req(perf_plsda$perf)
      #   actionButton("runPLSDAopt", "Run PLS-DA optimized")
      # })
      
      # Optimal number of component
      optimalN <- reactive({
        req(perf_plsda$perf)
        error <- perf_plsda$perf$error.rate$BER %>% as.data.frame() %>% dplyr::select(mahalanobis.dist) 
        findOptimalN(error) %>% as.numeric()
      })
      
      # Select optimum number of components (automatic suggestion)
      output$uincompOpt <- renderUI({
        req(perf_plsda$perf)
        # Building choices from perf_plsda. Thus it changes whether perf_plsda changes
        choicesComp <- paste0(1:(perf_plsda$perf$error.rate.class$mahalanobis.dist %>% ncol()), " components")
        selectInput(session$ns("ncompOpt"), "Optimum N comp (suggested)", choices = choicesComp, selected = choicesComp[optimalN()])
      })
      
      # output$prueba <- renderTable({input$ncompOpt})
      
      
      
      output$scatterPlot <- renderPlot({
        # observeEvent(input$runPLSDAopt, {
        # my_plsda <- mixOmics::plsda(X,Y, ncomp = input$ncompOpt)
        
        req(perf_plsda$perf)
        optComp <- gsub(x = input$ncompOpt, pattern = " components", replacement = "") %>% as.numeric()
        my_plsda <- mixOmics::plsda(X,Y, ncomp = optComp)
        scatterData <- my_plsda$variates$X
        
        expl.variance <- my_plsda$prop_expl_var$X * 100
        names(expl.variance) <- gsub("comp", x = expl.variance %>% names(), replacement = "PC")
        colnames(scatterData) = paste0(names(expl.variance), ": ", as.character(round(expl.variance, 2)), 
                                        c("% of explained variance"))
        
        plotScores <- scatterPlotPLS(scatterData = scatterData, Xcomp = input$comp1, Ycomp = input$comp2, title = input$title, sizeTitle = input$sizeTitle,
                    sizeLabels = input$sizeLabels, sizePoints = input$sizePoints, sizeXYaxis = input$sizeXYaxis, sizeXYlabel = input$sizeXYlabel,
                    sizeLegendTitle = input$sizeLegendTitle, sizeLegendLevels = input$sizeLegendLevels, declutterLabels = input$declutterLabels,
                    sizeSegment = input$sizeSegment, showCentroid = input$showCentroid, showEllipses = input$showEllipses, Y = Y, labels = "None",
                    col = col.pch$col, pch = col.pch$pch
        )
        return(plotScores)
      }, res = 150, 
      height = reactive(input$height),
      width = reactive(input$width))
    }
  )
}

########################################################################
# scatterPlot
########################################################################
scatterPlotPLS <- function(scatterData, sizeLabels = 3, sizePoints = 3, col, pch, title = "", sizeTitle = 30,
                           Xcomp = 1, Ycomp = 2, Y, labels = "None", sizeSegment = 0.1, declutterLabels = 10, showCentroid = F, showEllipses = F,
                           sizeXYaxis = 12, sizeXYlabel = 12, sizeLegendTitle = 12, sizeLegendLevels = 12){
  
  # This allows to plot 1 component when setting Xcomp and Ycomp to 1
  if (Xcomp == 1 & Ycomp == 1) {
    scatterDataplot <- scatterData %>% as.data.frame() %>% mutate(., Y) %>% arrange(Y) %>% mutate(., 1:nrow(.))
    colnames(scatterDataplot)[ncol(scatterDataplot)] <- "Sample"
    Xcomp <- scatterDataplot %>% ncol()
    rownames(scatterDataplot) <- scatterDataplot %>% pull(Sample) 
    # scatterDataplot <- scatterData %>% mutate(., Y) %>% arrange(Y) %>% as.data.frame()
    plotOneComp = T
  }else{
    plotOneComp = F
    scatterData <- scatterData %>% as.data.frame()
    scatterDataplot <- scatterData  %>% as.data.frame() %>% mutate(., Y)
  }
  
  
  
  scatter <- ggplot(scatterDataplot, aes(x = scatterDataplot[,Xcomp], y = scatterDataplot[,Ycomp], label = labels))  +
    geom_point(aes(color = Y, shape = Y), size = sizePoints) + 
    
    labs(color = "Legend", shape = "Legend") +
    scale_shape_manual(values = pch) +
    scale_color_manual(values = col) +
    
    xlab(colnames(scatterDataplot)[Xcomp]) + 
    ylab(colnames(scatterDataplot)[Ycomp]) + 
    
    geom_hline(yintercept = 0, linetype="dashed") +
    geom_vline(xintercept = 0, linetype="dashed") +
    
    theme_bw() +
    theme(legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)) +
    
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
      geom_text_repel((aes(color = Y)), size = sizeLabels, show.legend = F, max.overlaps = declutterLabels, segment.size = sizeSegment, 
                      max.time = 0.5, max.iter = 2000, min.segment.length = 0.1, force = 1, force_pull = 5) 
  }
  if (showCentroid == T & plotOneComp == F) {
    centroids <- scatterData[,c(Xcomp, Ycomp)] %>% mutate(., Y) %>% group_by(Y) %>% summarise_if(is.numeric, funs(mean)) %>% as.data.frame()
    scatterCentroids <- scatterData %>% mutate(., Y) %>% merge(., centroids, by = "Y")
    colnames(scatterCentroids) <- c("Y", colnames(scatterData), "centroid.X", "centroid.Y")
    
    scatter <- scatter + geom_point(data = scatterCentroids, aes(x = centroid.X, y = centroid.Y, color = Y), size = sizePoints/3) +
      geom_segment(data = scatterCentroids, aes(x = centroid.X, y = centroid.Y, xend = scatterCentroids[,Xcomp + 1], 
                                                yend = scatterCentroids[,Ycomp + 1], color = Y), show.legend = F)
  }
  if (showEllipses == T & plotOneComp == F) {
    scatter <- scatter + stat_ellipse(aes(color = Y, fill = Y), geom = "polygon", alpha = 0.2, show.legend = F, level = 0.95, type = "norm")
  }
  return(scatter)
}

########################################################################
# Optimal number of components
########################################################################
findOptimalN <- function(error){
  if (min(error) >0.05) {
    ncomp = which(error == min(error))[1]
  }else{
    ncomp = which(error <= 0.05)[1] #Elijo componentes por el error balanceado de la mahalanobis.dist
  }
  if(length(ncomp)>1){ncomp=ncomp[1]}
  # perf_plsda$auc
  if (ncomp == 1) {
    ncomp = 2
  }
  return(ncomp)
}






















#
# #
data <- read_excel("./data.xlsx")
X <- data[,-1]
Y <- data$Style
source("/Users/josem/Google Drive/Functions/col_pch.R")
col.pch = col.pch(Y)
sizeLabels = 3; sizePoints = 3; col = col.pch$co; pch = col.pch$pch; title = ""; sizeTitle = 30; Xcomp = 1; Ycomp = 2; Y;
labels = "None"; sizeSegment = 0.1; declutterLabels = 10; showCentroid = F; showEllipses = F;
sizeXYaxis = 12; sizeXYlabel = 12; sizeLegendTitle = 12; sizeLegendLevels = 12
my_plsda <- mixOmics::plsda(X,Y, ncomp = 6)
scatterData <- my_plsda$variates$X

perf_plsda <- perf(my_plsda, validation = "loo", progressBar = T)
plot(perf_plsda)

error <- perf_plsda$error.rate$BER %>% as.data.frame() %>% dplyr::select(mahalanobis.dist)
findOptimalN(error) %>% as.numeric()

# scatterPlotPLS(scatterData = scatterData, Y = Y, col = col.pch$col, pch = col.pch$pch, showCentroid = T, showEllipses = T, Xcomp = 1, Ycomp = 1)

if (crossVal == "loo") {perf_plsda <- perf(my_plsda, validation = "loo", progressBar = T)}
if (crossVal == "Mfold") {perf_plsda <- perf(my_plsda, validation = "Mfold", progressBar = T)}















#############################################################################################################
# PLS-DA construction
#############################################################################################################

# ----------------- Load inputs ----------------- #
library(caret); library(tidyverse); library(plyr); library(splitTools); library(foreach); library(magrittr); library(sjmisc)
data <- read_excel("./data.xlsx")
Y <- data$Style %>% as.factor()
X <- data[,-1] %>% scale()
ncompMax <- 6 # Max number of components to optimize
typePartition <- "stratified" # stratified, basic, grouped, blocked
type <- "Mfold" # Mfold / loo
folds = 6
rep = 5
classErrorPlot <- "Andalusian" # (For class error rate)


# ----------------- Partitions ----------------- #
if (type == "loo") {
  folds <- length(Y)
  rep <- 1
  my.partition <- create_folds(y = Y, k = folds, m_rep = rep, type = typePartition)
} else{
  if (type == "Mfold") {
    my.partition <- create_folds(y = Y, k = folds, m_rep = rep, type = typePartition)
  }
}

# ----------------- PLS-DA perf: obtain assignation matrix ----------------- #
plsdaPerf <- plsdaCV(X, Y, my.partition = my.partition)

# ----------------- Convert assignation matrix into diagnostics ----------------- #
diagnostics <- list()
for (i in 1:rep) {
  diagnostics[[i]] <- getErrors(assignations = purrr:::map(plsdaPerf, i), Y = Y)
}

# ----------------- Get diagnostic means ----------------- #
BERmean <- do.call(rbind, purrr::map(diagnostics, 1)) %>% group_by(Component) %>% summarise_each(.,funs(mean))
classErrormean <- do.call(rbind, purrr::map(diagnostics, 2)) %>% dplyr::group_by(distance, class) %>% summarise_each(.,funs(mean)) %>% arrange(class)
classErrormeanPlot <- classErrormean %>% filter(class == classErrorPlot) %>% dplyr::select(-class) %>% rotate_df(rn = "Component", cn = T) %>% slice(-1) %>% as_tibble() 
Accuracymean <- do.call(rbind, purrr::map(diagnostics, 3)) %>% group_by(Component) %>% summarise_each(.,funs(mean))
# ----------------- Plot each diagnostic mean ----------------- #
plotDiagnostic(BERmean, "BER")
plotDiagnostic(classErrormeanPlot, paste0("Class Error of ", classErrorPlot, " class"))
plotDiagnostic(Accuracymean, "Accuracy")


# ----------------- Calculate optimal ncomp according to each diagnostic ----------------- #
noptimalBER <- findOptimalN(BERmean, "min")
noptimalAccuracy <- findOptimalN(Accuracymean, "max")
ncompOptimal <- rbind(noptimalBER, noptimalAccuracy) %>% set_rownames(c("BER", "Accuracy"))

#############################################################################################################
# PLSDA
#############################################################################################################
# FUNCTION: extract assignation from PLS-DA
plsAssignations <- function(X, Y, partition, ncompMax){
  
  # ----------------- Partitions ----------------- #
  Xtrain <- X %>% as_tibble() %>% slice(partition)
  Xtest <- X %>% as_tibble() %>% slice(-partition)
  Ytrain <- Y[partition]
  Ytest <- Y[-partition]

  # ----------------- Perform PLS-DA model ----------------- #
  my.plsda <- mixOmics::plsda(Xtrain,Ytrain, ncomp = ncompMax, scale = F)
  predictions <- predict(my.plsda, Xtest)
  
  # ----------------- Obtain predictions from different distances ----------------- #
  max.dist <- predictions$MajorityVote$max.dist %>% as_tibble() %>% dplyr::mutate("Index" = (1:length(Y))[-partition])
  centroids.dist <- predictions$MajorityVote$centroids.dist %>% as_tibble() %>% dplyr::mutate("Index" = (1:length(Y))[-partition])
  mahalanobis.dist <- predictions$MajorityVote$mahalanobis.dist %>% as_tibble() %>% dplyr::mutate("Index" = (1:length(Y))[-partition])
  all.dist <- list("max.dist" = max.dist, "centroids.dist" = centroids.dist, "mahalanobis.dist" = mahalanobis.dist)
  
  return("assignations" = all.dist)
}
# FUNCTION: perform iterations of the cross validation
plsdaCV <- function(X, Y, my.partition){
  plsAsses <- lapply(my.partition, function(partition){
    plsAssignations(X, Y, partition = partition, ncompMax = 6)
  })
  fSplit <- rep(1:rep, length(Y)) %>% sort(.)
  max.dist.Assign <- do.call(rbind, purrr::map(plsAsses, 1)) %>% split(max.dist.Assign, f = fSplit) %>% lapply(., function(x){x %>% arrange(Index) %>% dplyr::select(-Index)})
  centroids.dist.Assign <- do.call(rbind, purrr::map(plsAsses, 2)) %>% split(max.dist.Assign, f = fSplit) %>% lapply(., function(x){x %>% arrange(Index) %>% dplyr::select(-Index)})
  mahalanobis.dist.Assign <- do.call(rbind, purrr::map(plsAsses, 3)) %>% split(max.dist.Assign, f = fSplit) %>% lapply(., function(x){x %>% arrange(Index) %>% dplyr::select(-Index)})
  names(max.dist.Assign) <- names(centroids.dist.Assign) <- names(mahalanobis.dist.Assign) <- paste0("rep", 1:length(max.dist.Assign))
  all.dist.Assign <- list("max" = max.dist.Assign, "centroids" = centroids.dist.Assign, "mahalanobis" = mahalanobis.dist.Assign)
  return(all.dist.Assign)
}
# FUNCTION: get errors from the outputs of plsdaCV
getErrors <- function(assignations, Y){
  
  # ----------------- Confusion matrix for each distance ----------------- #
  conf.matrix.max <- alply(assignations$max %>% as.matrix, 2, function(X){table(X,Y)})
  conf.matrix.centroids <- alply(assignations$centroids %>% as.matrix, 2, function(X){table(X,Y)})
  conf.matrix.mahalanobis <- alply(assignations$mahalanobis %>% as.matrix, 2, function(X){table(X,Y)})
  
  # ----------------- Obtain diagnostics for each distance ----------------- #
  BER <- list("max.dist" = lapply(conf.matrix.max, BERF) %>% do.call(cbind,.),
                               "centroids.dist" = lapply(conf.matrix.centroids, BERF) %>% do.call(cbind,.),
                               "mahalanobis.dist" = lapply(conf.matrix.mahalanobis, BERF) %>% do.call(cbind,.))
  classError <- list("max.dist" = lapply(conf.matrix.max, ClassErrorF) %>% do.call(cbind, .) %>% set_colnames(paste0("Comp", 1:ncol(.))),
                   "centroids.dist" = lapply(conf.matrix.centroids, ClassErrorF) %>% do.call(cbind, .) %>% set_colnames(paste0("Comp", 1:ncol(.))),
                   "mahalanobis.dist" = lapply(conf.matrix.mahalanobis, ClassErrorF) %>% do.call(cbind, .) %>% set_colnames(paste0("Comp", 1:ncol(.))))
  accuracy <- list("max.dist" = lapply(conf.matrix.max, accuracyF) %>% do.call(cbind, .),
                                         "centroids.dist" = lapply(conf.matrix.centroids, accuracyF) %>% do.call(cbind, .),
                                         "mahalanobis.dist" = lapply(conf.matrix.mahalanobis, accuracyF) %>% do.call(cbind, .))
  
  # ----------------- Redefine variables -> results in a matrix ----------------- #
  BER <- do.call(rbind, BER) %>% set_rownames(., c("max", "centroids", "mahalanobis")) %>%
    t() %>% as.data.frame() %>% dplyr::mutate("Component" = paste0("Comp", 1:nrow(.)), .before = max)
  classError <- classError %>% purrr::map(., function(x) apply(x, 2, unlist)) %>% do.call(rbind, .) %>% as.data.frame() %>%
    dplyr::mutate(., "class" = rep(rownames(.)[1:nlevels(Y)], 3)) %>% as_tibble() %>%
    dplyr::mutate(., "distance" = c(rep("max", nlevels(Y)), rep("centroid", nlevels(Y)), rep("mahalanobis", nlevels(Y)))) %>% arrange(class)
  accuracy <- do.call(rbind, accuracy) %>% set_rownames(., c("max", "centroids", "mahalanobis")) %>%
    t() %>% as.data.frame() %>% dplyr::mutate("Component" = paste0("Comp", 1:nrow(.)), .before = max)
    
  return(list("BER" = BER, "classError" = classError, "accuracy" = accuracy))
}
# FUNCTION: extract accuracy from confusion matrix
accuracyF <- function(conf.matrix){
  cp <- 0
  for (i in 1:nrow(conf.matrix)) {cp <- conf.matrix[i,i] %>% sum(., cp)}
  cp / sum(conf.matrix)
}
# FUNCTION: extract BER from confussion matrix
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
# Class error
# ClassError(conf.matrix.max$`1`) %>% set_names(conf.matrix.max$`1` %>% colnames())
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
# FUNCTION: Find optimal number of components
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
# FUNCTION: plot the diagnostics along the components
plotDiagnostic <- function(diagnostic, title = ""){
  diagnosticPlot <- pivot_longer(diagnostic, cols = -Component, names_to = "Distance") %>% as.data.frame()
  gg <- ggplot(data = diagnosticPlot, aes(x = Component, y = value, group = Distance)) +
    geom_point(aes(color=Distance)) +
    geom_line(aes(color=Distance)) + 
    ggtitle(title) +
    theme(title = element_text(size = 10)) +
    theme(plot.title = element_text(hjust = 0.5))
  gg
}