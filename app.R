library(bs4Dash)
ui <- dashboardPage(
  dashboardHeader(title = "Practica 2"),
  dashboardSidebar(
    bs4SidebarUserPanel("Anderson Barbosa", image = "https://freesvg.org/img/comic-boy.png"),
    
    bs4SidebarMenu(
      bs4SidebarMenuItem("Grafica", tabName = "graficaTab", icon = icon("signal")),
      bs4SidebarMenuItem("Tabla", tabName = "tablaTab", icon = icon("th-list"))  
    )
    
    
  ),
  dashboardBody(
    
    
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      ),
      
      
    ),
    fluidRow(
      box(
        title = "Controls",
        sliderInput("slider3", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  
  biostats <- read.csv(file = 'data/biostats.csv')
  sunspots <- read.csv(file = 'data/sunspots.csv')
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)