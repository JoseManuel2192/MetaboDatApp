

library(shiny)
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



ui <- fluidPage(
    barPlotUI("barPlot")
)

server <- function(input, output, session) {
  
  data <- reactive({
    read_excel("./data.xlsx")
  })
  
  choicesX <- reactive({
    read_excel("./data.xlsx") %>% select(1:7) %>% colnames()
  })
  
  choicesY <- reactive({
    read_excel("./data.xlsx") %>% select(-c(1:7)) %>% colnames()
  })
  
  
  barPlotServer("barPlot", choicesX = choicesX(), choicesY = choicesY(), data = data())
  
}

shinyApp(ui, server)







xcomp <- 1
ycomp <- 8
fill <- c(1)

title <- "Barplot"
sizeTitle <- 12

sizeYlabel <- 20
sizeXYaxis <- 12
rotateXlabel <- 0

sizeLegendTitle <- 15
sizeLegendLevels <- 11



position <- "dodge"
alpha <- 1
stat <- "identity"
jitter <- F
showSD <- T

data <- read_excel("./data.xlsx")

barPlot(data, xcomp, ycomp, fill, position, stat, alpha, sizeLegendTitle, sizeLegendLevels, jitter, sizeXYaxis, sizeYlabel, rotateXlabel = 0, showSD = T,
        title = title, sizeTitle = sizeTitle)






