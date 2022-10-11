library(bs4Dash)
library(dplyr)
library(DT)

sidebar <- bs4DashSidebar(
  status="orange",
  bs4SidebarUserPanel("Anderson Barbosa", image = "https://freesvg.org/img/comic-boy.png"),
  
  bs4SidebarMenu(
    bs4SidebarMenuItem("Grafica", tabName = "graficaTab", icon = icon("signal")),
    bs4SidebarMenuItem("Tabla", tabName = "tablaTab", icon = icon("th-list"), selected = TRUE)  
  )
)

body <- bs4DashBody(
  
  bs4TabItems(
    
    # Grafica Tab
    bs4TabItem(
      tabName = "graficaTab",
      fluidRow(
        bs4Card(
          width = 4,
          title = "Filter",
          status = "orange",
          dateRangeInput("daterange1", "Date range:",
                         start = "2001-01-01",
                         end   = "2010-12-31"),
          actionButton("do", "Click Me")
        ),
        bs4Card(
          width = 8,
          title = "Plot",
          status = "orange",
          sliderInput("slider2", "Number of observations:", 1, 100, 50)
        )
      )
    ),
    
    # Tabla Tab
    bs4TabItem(
      tabName = "tablaTab",
      fluidRow(
        column(
          width = 4, 
          uiOutput("tabla_tab_filters")
        ),
        column(
          width = 8,
          bs4Card(
            width = NULL,
            title = "Biostats",
            status = "orange",
            dataTableOutput("tableBiostats")
          )
        )
      )
    )
  )
  
)


ui <- bs4DashPage(
  dashboardHeader("Practica 2"),
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session) {
  # https://alvaroferro.shinyapps.io/practica2/
  
  values <- reactiveValues();
  
  # Import's CSV
  sunspots <- read.csv(file = 'data/sunspots.csv')
  biostats <- read.csv(file = 'data/biostats.csv')[,c("Name", "Sex", "Age", "Height..in.", "Weight..lbs.")]
  
  output$tabla_tab_filters <- renderUI({
    bs4Card(
      width = NULL,
      title = "Reactive Filter",
      status = "orange",
      selectInput("sex","Sex:", choices = c("ALL",unique(biostats$Sex))),
      numericInput("age", "Age:", min = min(biostats$Age), max = max(biostats$Age), value = median(biostats$Age)),
      numericInput("height", "Height:", min = min(biostats$Height..in.), max = max(biostats$Height..in.), value = median(biostats$Height..in.))
    );
  })
  
  
  getDataBiostats <- reactive({
    if(is.null(input$age)){
      return(NULL)
    }
    
    data <- biostats %>% dplyr::filter(Age >= input$age, Height..in. >= input$height);
    if(input$sex != "ALL"){
      data <- data %>% dplyr::filter(Sex == input$sex);
    }
    return(data);
  })
  
  output$tableBiostats <- renderDataTable({
    data <- getDataBiostats();
    if(is.null(data)){
      return(NULL);
    }
    
    DT::datatable(
      data, 
      options = list(orderClasses = TRUE),
      rownames = NULL,
      colnames = c(
        "Name" = "Name",
        "Sex" = "Sex",
        "Age" = "Age",
        "Height" = "Height..in.",
        "Weight" = "Weight..lbs."
      )
    );
  })
}

shinyApp(ui, server)