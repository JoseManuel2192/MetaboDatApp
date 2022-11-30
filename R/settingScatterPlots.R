settingScatterPlots <- function(comp1, comp2, title, sizeTitle, height, width, sizeLabels, sizePoints, 
                                sizeXYaxis, sizeXYlabel, sizeLegendTitle, sizeLegendLevels, declutterLabels,
                                sizeSegment, showCentroid, showEllipses) {
  tagList(
    
    fluidRow(
      column(6, numericInput(comp1, "Component 1", value = 1, step = 1, min = 1)), 
      column(6, numericInput(comp2, "Component 2", value = 2, step = 1, min = 1)) 
    ),
    
    fluidRow(
      column(8, textInput(title, "Title", value = "")),
      column(4, numericInput(sizeTitle, "Size title", value = 9))
    ), 
    
    fluidRow(
      column(6, numericInput(height, "Height Plot", value = 700, step = 100, min = 0)), 
      column(6, numericInput(width, "Width Plot", value = 900, step = 100, min = 0)) 
    ),
    
    fluidRow(
      column(6, numericInput(sizeLabels, "Size labels", value = 2, step = 0.5, min = 0)), 
      column(6, numericInput(sizePoints, "Size points", value = 3, step = 0.5, min = 0)) 
    ),
    
    fluidRow(
      column(6, numericInput(sizeXYaxis, "Size XY axis", value = 6, step = 1, min = 0)), 
      column(6, numericInput(sizeXYlabel, "Size XY names", value = 10, step = 1, min = 0)) 
    ),
    
    fluidRow(
      column(6, numericInput(sizeLegendTitle, "Size legend title", value = 11, step = 1, min = 0)), 
      column(6, numericInput(sizeLegendLevels, "Size legend levels", value = 9, step = 1, min = 0))
    ),
    
    fluidRow(
      column(6, numericInput(declutterLabels, "Declutter labels", value = 16, step = 1, min = 0)),
      column(6, numericInput(sizeSegment, "Size segment", value = 16, step = 1, min = 0))
    ),
    
    fluidRow(
      column(6, checkboxInput(showCentroid, "Show centroids", value = T)), 
      column(6, checkboxInput(showEllipses, "Show ellpises", value = F))
    )
  )  
}

