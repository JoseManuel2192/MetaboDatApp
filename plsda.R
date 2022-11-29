########################################################################
########################################################################
plsdaUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3, align = "center",
                   h4("PLS-DA settings"), br(),
                   numericInput(ns("maxNcomp"), label = "N components to optimize", value = 2, min = 1, step = 1),
                   actionButton(ns("runPLSDAopt"), "Run cross-validation: component optimization"),
                   h4("....................................................."),
                   
                   ################################
                   uiOutput(ns("uincompOpt")),
                   uiOutput(ns("uirunPLSDA")),
                   h4("....................................................."),
                   ################################
                   h4("Figure settings"), br(),
                   settingScatterPlots(
                     comp1 = ns("comp1"), comp2 = ns("comp2"), title = ns("title"), sizeTitle = ns("sizeTitle"), height = ns("height"),
                     width = ns("width"), sizeLabels = ns("sizeLabels"), sizePoints = ns("sizePoints"), sizeXYaxis = ns("sizeXYaxis"), 
                     sizeLegendTitle = ns("sizeLegendTitle"), sizeLegendLevels = ns("sizeLegendLevels"), declutterLabels = ns("declutterLabels"), 
                     sizeSegment = ns("sizeSegment"), showCentroid = ns("showCentroid"), showEllipses = ns("showEllipses"),  sizeYlabel = ns("sizeYlabel")
                   )                   
      ),
      mainPanel(width = 9,
                plotOutput(ns("errorRate")),
                plotOutput(ns("scatterPlot"))
      )
    )
  )
}

plsdaServer <- function(id, X, Y, col.pch) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Perf optimization (single cross validation) of PLS-DA
      perf_plsda <- reactiveValues(data = NULL)
      observeEvent(input$runPLSDAopt, {
        my_plsda=plsda(X,Y, ncomp = input$maxNcomp)
        perf_plsda$perf = perf(my_plsda, dist = "mahalanobis", auc = TRUE, progressBar = TRUE, validation = "loo")
      })
      
      # Render error rate plot
      output$errorRate <- renderPlot({
        req(perf_plsda$perf)
        plot(perf_plsda$perf)
      }, width = 800, height = 500, res = 120)
      
      # Select optimum number of components (automatic suggestion)
      output$uincompOpt <- renderUI({
        req(perf_plsda$perf)
        error <- perf_plsda$perf$error.rate$BER %>% as.data.frame()
        optimalN <- findOptimalN(error)
        numericInput("ncompOpt", "Optimum N comp (suggested)", value = optimalN, min = 1, step = 1)
      })
      
      output$uirunPLSDA <- renderUI({
        req(perf_plsda$perf)
        actionButton("runPLSDAopt", "Run PLS-DA optimized")
      })
      
      output$scatterPlot <- renderPlot({
        # observeEvent(input$runPLSDAopt, {
        # my_plsda <- mixOmics::plsda(X,Y, ncomp = input$ncompOpt)
        
        req(perf_plsda$perf)
        my_plsda <- mixOmics::plsda(X,Y, ncomp = 2)
        scatterData <- my_plsda$variates$X
        
        plotScores <- scatterPlot(scatterData = scatterData, Xcomp = input$comp1, Ycomp = input$comp2, title = input$title, sizeTitle = input$sizeTitle,
                    sizeLabels = input$sizeLabels, sizePoints = input$sizePoints, sizeXYaxis = input$sizeXYaxis, sizeYlabel = input$sizeYlabel,
                    sizeLegendTitle = input$sizeLegendTitle, sizeLegendLevels = input$sizeLegendLevels, declutterLabels = input$declutterLabels,
                    sizeSegment = input$sizeSegment, showCentroid = input$showCentroid, showEllipses = input$showEllipses, Y = Y, labels = "None",
                    col = col.pch$col, pch = col.pch$pch
        )
        return(plotScores)
      }, height = reactive(input$height),
      width = reactive(input$width))
    }
  )
}

########################################################################
########################################################################
library(shiny)

ui <- fluidPage(
  plsdaUI("plsdaScores")
  )

server <- function(input, output, session) {
  data <- readxl::read_excel("./data.xlsx")
  X <- data[,-c(1)]
  Y <- data %>% pull(Style)
  source("/Users/josem/Google Drive/Functions/col_pch.R")
  col.pch = col.pch(Y)
  
  plsdaServer("plsdaScores", X = X, Y = Y, col.pch = col.pch)
  
}

shinyApp(ui, server)










data <- read_excel("./data.xlsx")
X <- data[,-1]
Y <- data$Style
source("/Users/josem/Google Drive/Functions/col_pch.R")
col.pch = col.pch(Y)
sizeLabels = 3; sizePoints = 3; col = col.pch$co; pch = col.pch$pch; title = ""; sizeTitle = 30; Xcomp = 1; Ycomp = 2; Y;
labels = "None"; sizeSegment = 0.1; declutterLabels = 10; showCentroid = F; showEllipses = F;
sizeXYaxis = 12; sizeYlabel = 12; sizeLegendTitle = 12; sizeLegendLevels = 12
my_plsda <- mixOmics::plsda(X,Y, ncomp = 2)
scatterData <- my_plsda$variates$X

scatterPlot(scatterData = scatterData, Y = Y, col = col.pch$col, pch = col.pch$pch, showCentroid = T, showEllipses = T)


########################################################################
# scatterPlot
########################################################################
scatterPlot <- function(scatterData, sizeLabels = 3, sizePoints = 3, col, pch, title = "", sizeTitle = 30,
                        Xcomp = 1, Ycomp = 2, Y, labels = "None", sizeSegment = 0.1, declutterLabels = 10, showCentroid = F, showEllipses = F,
                        sizeXYaxis = 12, sizeYlabel = 12, sizeLegendTitle = 12, sizeLegendLevels = 12){
  
  scatterData <- scatterData %>% as.data.frame()
  scatterDataplot <- scatterData  %>% as.data.frame() %>% mutate(., Y)
  
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
      
    theme(axis.text = element_text(size = sizeXYaxis, face="bold", colour = "black")) +
    theme(axis.title = element_text(size = sizeYlabel, face="bold")) +
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
  if (showCentroid == T) {
    centroids <- scatterData[,c(Xcomp, Ycomp)] %>% mutate(., Y) %>% group_by(Y) %>% summarise_if(is.numeric, funs(mean)) %>% as.data.frame()
    scatterCentroids <- scatterData %>% mutate(., Y) %>% merge(., centroids, by = "Y")
    colnames(scatterCentroids) <- c("Y", colnames(scatterData), "centroid.X", "centroid.Y")
    
    scatter <- scatter + geom_point(data = scatterCentroids, aes(x = centroid.X, y = centroid.Y, color = Y), size = sizePoints/3) +
      geom_segment(data = scatterCentroids, aes(x = centroid.X, y = centroid.Y, xend = scatterCentroids[,Xcomp + 1], 
                                         yend = scatterCentroids[,Ycomp + 1], color = Y), show.legend = F)
  }
  if (showEllipses == T) {
    scatter <- scatter + stat_ellipse(aes(color = Y), geom = "polygon", alpha = 0.2, show.legend = F, level = 0.95, type = "norm")
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

########################################################################
########################################################################

source("/Users/josem/Google Drive/Functions/col_pch.R")
col.pch = col.pch(Y)
#####################################
# PLS-DA
#####################################
require(mixOmics)
my_plsda=plsda(X,Y, ncomp = 2)

perf_plsda=perf(my_plsda, dist = "mahalanobis", auc = TRUE, progressBar = TRUE, validation = "loo")
# perf_plsda=perf(my_plsda, dist = "mahalanobis", auc = TRUE, progressBar = TRUE, validation = "Mfold", nrepeat = 200, fold = 5)
plot(perf_plsda)
perf_plsda$error.rate
perf_plsda$error.rate.class

### Eleccion de componentes
choisecomp = as.data.frame(perf_plsda$error.rate$BER)
if (min(choisecomp) >0.05) {
  ncomp = which(choisecomp == min(choisecomp))[1]
}else{
  ncomp = which(choisecomp <= 0.05)[1] #Elijo componentes por el error balanceado de la mahalanobis.dist
}
if(length(ncomp)>1){ncomp=ncomp[1]}
perf_plsda$error.rate.class$mahalanobis.dist[,ncomp]
perf_plsda$auc[ncomp]
perf_plsda$error.rate$BER
# perf_plsda$auc
if (ncomp == 1) {
  ncomp = 2
}
ncomp
my_plsda=plsda(X,Y, ncomp = ncomp)



data <- readxl::read_excel("./data.xlsx")

Yname <- "Style"
X <- dplyr::select(data, -c(1:7))
Y <- pull(data, Yname)

source("/Users/josem/Google Drive/Functions/col_pch.R")
col.pch = col.pch(Y)

my_plsda=plsda(X,Y, ncomp = ncomp)
scatterData <- my_plsda$variates$X

scatterPlot(scatterData = scatterData, pch = col.pch$pch, col = col.pch$col, centroids = F, Y = Y)

