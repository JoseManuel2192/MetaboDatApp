settingPlot <- function(title, sizeTitle, height, width, sizeXYaxis, sizeYlabel, rotateXlabel, sizeLegendTitle, sizeLegendLevels) {
  tagList(
    
    fluidRow(
      column(8, textInput(title, "Title", value = "")),
      column(4, numericInput(sizeTitle, "Size title", value = 12))
    ), 
    
    fluidRow(
      column(6, numericInput(height, "Height Plot", value = 600, step = 100, min = 0)), 
      column(6, numericInput(width, "Width Plot", value = 600, step = 100, min = 0)) 
    ),
    
    fluidRow(
      column(4, numericInput(sizeXYaxis, "Size XY axis", value = 16, step = 1, min = 0)), 
      column(4, numericInput(sizeYlabel, "Size Y name", value = 16, step = 1, min = 0)), 
      column(4, numericInput(rotateXlabel, "Rotate X labels", value = 0, min = 0, step = 5)) 
    ),
    fluidRow(
      column(6, numericInput(sizeLegendTitle, "Size legend title", value = 20, step = 1, min = 0)), 
      column(6, numericInput(sizeLegendLevels, "Size legend levels", value = 16, step = 1, min = 0))
    )
  )  
}

