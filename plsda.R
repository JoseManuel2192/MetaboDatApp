


########################################################################
########################################################################
plsdaUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3, align = "center",
                   h4("Figure settings"),
                   settingScatterPlots(title = ns("title"), sizeTitle = ns("sizeTitle"), height = ns("height"),
                                       width = ns("width"), sizeXYaxis = ns("sizeXYaxis"), sizeYlabel = ns("sizeYlabel"),
                                       rotateXlabel = ns("rotateXlabel"), sizeLegendTitle = ns("sizeLegendTitle"), 
                                       sizeLegendLevels = ns("sizeLegendLevels"))
      ),
      mainPanel(width = 9)
    )
  )
}

plsdaServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}



########################################################################
########################################################################
library(shiny)

ui <- fluidPage(
  plsdaUI("plsdaScores"))

server <- function(input, output, session) {
  
}

shinyApp(ui, server)



########################################################################
########################################################################
scatterPlot <- function(scatterData, sizeLabels = 3, sizePoints = 3, sizeLegend = 14, pch, col, titleScatter = "",
                        Xcomp = 1, Ycomp = 3, Y, labels = "None", sizeSegment = 0.1, declutterLabels = 10, centroids){
  
  scatterData <- scatterData %>% as.data.frame()
  
  scatter <- ggplot(scatterData, aes(x = scatterData[,Xcomp], y = scatterData[,Ycomp], label = labels))  +
    geom_point(aes(color = Y, shape = Y), size = sizePoints) + 
    theme_bw() +
    theme(legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)) +
    theme(legend.title = element_text(size = sizeLegend),
          legend.text = element_text(size = sizeLegend / 1.3)) +
    labs(color = "Legend", shape = "Legend") +
    scale_shape_manual(values = pch) +
    scale_color_manual(values = col) +
    ggtitle(titleScatter) + theme(plot.title = element_text(hjust = 0.5)) +
    xlab(colnames(scatterData)[Xcomp]) + 
    ylab(colnames(scatterData)[Ycomp]) + 
    theme(axis.text.y = element_text(angle = 90)) +
    geom_hline(yintercept = 0, linetype="dashed") +
    geom_vline(xintercept = 0, linetype="dashed") +
    stat_ellipse(aes(color = Y), size = 0.8, geom = "polygon", alpha = 0.2, show.legend = F, level = 0.99)
  # Add labels of the samples
  if (length(labels) == 1) {
    scatter
  }else{
    scatter <- scatter + 
      geom_text_repel((aes(color = Y)), size = sizeLabels, show.legend = F, max.overlaps = declutterLabels, segment.size = sizeSegment, 
                      max.time = 0.5, max.iter = 2000, min.segment.length = 0.1, force = 1, force_pull = 5) 
  }
  if (centroids == T) {
    centroids <- dplyr::select(scatterData, any_of(c(Xcomp, Ycomp))) %>% mutate(., Y) %>% group_by(Y) %>% summarise_if(is.numeric, funs(mean)) %>% as.data.frame()
    scatterCentroids <- scatterData %>% mutate(., Y) %>% merge(., centroids, by = "Y")
    colnames(scatterCentroids) <- c("Y", colnames(scatterData), "centroid.X", "centroid.Y")
    
    scatter <- scatter + geom_point(data = scatterCentroids, aes(x = centroid.X, y = centroid.Y, color = Y), size = sizePoints/3) +
      geom_segment(data = scatterCentroids, aes(x = centroid.X, y = centroid.Y, xend = scatterCentroids[,Xcomp + 1], 
                                         yend = scatterCentroids[,Ycomp + 1], color = Y), show.legend = F)
  }
  return(scatter)
}

########################################################################
########################################################################
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




data <- readxl::read_excel("./data.xlsx")

Yname <- "Style"
X <- dplyr::select(data, -c(1:7))
Y <- pull(data, Yname)

source("/Users/josem/Google Drive/Functions/col_pch.R")
col.pch = col.pch(Y)

my_plsda=plsda(X,Y, ncomp = ncomp)
scatterData <- my_plsda$variates$X

scatterPlot(scatterData = scatterData, pch = col.pch$pch, col = col.pch$col, centroids = F, Y = Y)

