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
