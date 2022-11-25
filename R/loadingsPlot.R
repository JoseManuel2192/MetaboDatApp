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