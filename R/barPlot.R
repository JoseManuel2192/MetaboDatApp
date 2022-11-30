barPlot <- function(data, xcomp, ycomp, fill, stat = "identity", alpha = 1, sizeLegendTitle = 15, sizeLegendLevels =  11,
                    jitter = F, sizeYlabel, sizeXYaxis, rotateXlabel = 0, showSD = F, title, sizeTitle){
  
  factors <- c(xcomp, fill)
  mean <- groupSummary(data = data, summaryFunction = mean, factors = factors) %>% data.frame()
  sd <- groupSummary(data = data, summaryFunction = sd, factors = factors) %>% data.frame()

  barPlot <- ggplot(data = mean, aes(x = pull(mean, xcomp), y =  pull(mean, ycomp), fill = pull(mean, fill))) +
    geom_bar(stat = stat, position = position_dodge(), alpha = alpha, color = "black") +
    theme_bw() +
    xlab("") + ylab(ycomp) + 
    labs(color = "Legend", shape = "Legend") + labs(fill = "Legend") +
    
    ggtitle(title) + 
    theme(title = element_text(size = sizeTitle)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    
    theme(axis.text = element_text(size = sizeXYaxis, face = "bold", colour = "black")) +
    theme(axis.title = element_text(size = sizeYlabel, face ="bold")) +
    theme(axis.text.x = element_text(angle = rotateXlabel, hjust = 1, size = sizeXYaxis)) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.3, size = sizeXYaxis/1.2)) +
    
    theme(legend.title = element_text(size = sizeLegendTitle)) + 
    theme(legend.text = element_text(size = sizeLegendLevels))
      
  if (jitter == T) {
    barPlot <- barPlot + geom_jitter(shape=16, position=position_jitter(0.2))
  }
  if (showSD == T) {
    lower <- mean %>% pull(ycomp) - sd %>% pull(ycomp)
    upper <- mean %>% pull(ycomp) + sd %>% pull(ycomp)
    barPlot <- barPlot + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position=position_dodge(.9))
  }
  barPlot 
}


barPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3, align = "center",
                   h4("Figure settings", align = "center"), br(),
                   uiOutput(ns("uixcomp")),
                   uiOutput(ns("uiycomp")),
                   uiOutput(ns("uifill")),
                   checkboxInput(ns("showSD"), "Show standard deviation", value = F),
                   settingPlot(title = ns("title"), sizeTitle = ns("sizeTitle"), height = ns("height"),
                               width = ns("width"), sizeXYaxis = ns("sizeXYaxis"), sizeYlabel = ns("sizeYlabel"),
                               rotateXlabel = ns("rotateXlabel"), sizeLegendTitle = ns("sizeLegendTitle"),
                               sizeLegendLevels = ns("sizeLegendLevels"))
      ),
      mainPanel(width = 9,
                plotOutput(ns("plottingBar"))
      )
    )
  )
}

barPlotServer <- function(id, choicesX, choicesY, data) {
  moduleServer(
    id, function(input, output, session) {
      
      #########################
      # Render user interfaces
      #########################
      
      output$uixcomp <- renderUI({
        selectInput(session$ns("xcomp"), "Select x component", multiple = F,
                    choices = choicesX)
      })
      
      output$uiycomp <- renderUI({
        selectInput(session$ns("ycomp"), "Select y component", multiple = F,
                    choices = choicesY)
      })
      
      output$uifill <- renderUI({
        selectInput(session$ns("fill"), "Fill colour", multiple = F,
                    choices = choicesX)
      })
      
      #########################
      # BarPlot function
      #########################
      output$plottingBar <- renderPlot({
        barPlot(data = data, xcomp = input$xcomp, ycomp = input$ycomp, fill = input$fill, sizeLegendTitle = input$sizeLegendTitle,
                sizeLegendLevels = input$sizeLegendLevels, sizeXYaxis = input$sizeXYaxis, sizeYlabel = input$sizeYlabel,
                title = input$title, sizeTitle = input$sizeTitle, rotateXlabel = input$rotateXlabel, showSD = input$showSD)
      },
      height = reactive(input$height),
      width = reactive(input$width)
      )
    }
  )
}

