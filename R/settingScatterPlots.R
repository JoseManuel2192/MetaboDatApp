settingScatterPlots <- function(comp1, comp2, title, sizeTitle, height, width, sizeLabels, sizePoints, 
                                sizeXYaxis, sizeYlabel, sizeLegendTitle, sizeLegendLevels, declutterLabels,
                                sizeSegment, showCentroid, showEllipses) {
  tagList(
    
    fluidRow(
      column(6, numericInput(comp1, "Component 1", value = 1, step = 1, min = 1)), 
      column(6, numericInput(comp2, "Component 2", value = 2, step = 1, min = 1)) 
    ),
    
    fluidRow(
      column(8, textInput(title, "Title", value = "")),
      column(4, numericInput(sizeTitle, "Size title", value = 12))
    ), 
    
    fluidRow(
      column(6, numericInput(height, "Height Plot", value = 600, step = 100, min = 0)), 
      column(6, numericInput(width, "Width Plot", value = 700, step = 100, min = 0)) 
    ),
    
    fluidRow(
      column(6, numericInput(sizeLabels, "Size labels", value = 3, step = 1, min = 0)), 
      column(6, numericInput(sizePoints, "Size points", value = 5, step = 1, min = 0)) 
    ),
    
    fluidRow(
      column(6, numericInput(sizeXYaxis, "Size XY axis", value = 16, step = 1, min = 0)), 
      column(6, numericInput(sizeYlabel, "Size XY names", value = 16, step = 1, min = 0)) 
    ),
    
    fluidRow(
      column(6, numericInput(sizeLegendTitle, "Size legend title", value = 15, step = 1, min = 0)), 
      column(6, numericInput(sizeLegendLevels, "Size legend levels", value = 13, step = 1, min = 0))
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

