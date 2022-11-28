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
    theme(axis.text=element_text(face="bold", colour = "black"),
          axis.title=element_text(size=size_label_axis,face="bold"))
  boxplot <- boxplot  + theme(legend.position="none")  #+ scale_fill_manual(values = setcolors)
  
  if (rotate == T) {
    boxplot <- boxplot + coord_flip() + theme(axis.text.y = element_text(angle = x_label_angle, hjust = 1, size = size_axis),
                                              axis.text.x = element_text(angle = 0, size = size_axis/1.2))
  } else{
    boxplot <- boxplot + theme(axis.text.x = element_text(angle = x_label_angle, hjust = 1, size = size_axis),
                               axis.text.y = element_text(angle = 90, hjust = 0.3, size = size_axis/1.2))
  }
  return(boxplot)
}