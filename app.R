library(shiny)
library(reactable)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  textInput("text1", NULL, placeholder = "Your name"),
  
  sliderInput("slider1", "When should we deliver?", 
              min = as.Date("2020-09-16", "%Y-%m-%d"),
              max = as.Date("2020-09-23", "%Y-%m-%d"),
              value = as.Date("2020-09-17", "%Y-%m-%d"),
              ticks = T),
  
  sliderInput("slider2", "slider2",
              min = 0,
              max = 100,
              value = 0,
              step = 5,
              animate = animationOptions(interval = 1000,
                                         loop = F,
                                         playButton = "Play!")),
  
  selectizeInput("selectize1", "Server Select", choices = NULL),
  
  selectInput("select1", "SubItem Select", 
              choices = list(`East Coast` = list("NY", "NJ", "CT"),
                             `West Coast` = list("WA", "OR", "CA"),
                             `Midwest` = list("MN"))),
  
  verbatimTextOutput("VTout1"),
  textOutput("Tout1"),
  
  verbatimTextOutput("VTout2"),
  textOutput("Tout2"),
  
  plotOutput("plot1", width = "700px", height = "300"),
  
  DT::DTOutput("dtable1"), # instead of dataTableOutput
  
  reactableOutput("dtable2"),
  
  textInput("name", "What's your name?"),
  textOutput("greeting")
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  updateSelectizeInput(session, "selectize1", 
                       choices = state.name, 
                       server = T);

  output$VTout1 <-  renderPrint(summary(mtcars))
  output$Tout1 <- renderText("Good morning!")

  output$VTout2 <- renderPrint(t.test(1:5, 2:6))
  output$Tout2 <- renderText(lm(mpg ~ wt, data = mtcars)$coefficients)
  
  output$plot1 <- renderPlot(plot(1:5, 
                                  main = "Scatter Plot by 5 random digits"), 
                             res = 96)
  
  output$dtable1 <- DT::renderDT(mtcars, 
                                    options = list(pageLength = 5, 
                                                   ordering = F,
                                                   dom = 'p')) # instead of renderDataTable
  output$dtable2 <- renderReactable({
    reactable(mtcars)
  })
  
  string <<- reactive(paste0("Hello ", input$name, "!"))
  output$greeting <- renderText({
    string()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
