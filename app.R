library(bs4Dash)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(lubridate)

sidebar <- bs4DashSidebar(
  status="orange",
  bs4SidebarUserPanel("Anderson Barbosa", image = "https://freesvg.org/img/comic-boy.png"),
  
  bs4SidebarMenu(
    bs4SidebarMenuItem("Plot", tabName = "plotTab", icon = icon("chart-line"), selected = TRUE),
    bs4SidebarMenuItem("Table", tabName = "tableTab", icon = icon("table-list"))  
  )
)

body <- bs4DashBody(
  
  bs4TabItems(
    
    # Grafica Tab
    bs4TabItem(
      tabName = "plotTab",
      fluidRow(
        column(
          width = 4, 
          
          fluidRow(
            column(
              width = 12,
              uiOutput("plot_tab_filters")
            )
          ),
          
          fluidRow(
            
            # BOX Range
            bs4InfoBoxOutput("totalResults", width = 12),
            bs4InfoBoxOutput("totalRange", width = 12)
            
          )
          
        ),
        column(
          width = 8,
          bs4Card(
            width = NULL,
            title = "Plot (plotly)",
            status = "orange",
            
            fluidRow(
              column(
                width = 12,
                plotlyOutput("plot")  
              )
            )
          )
        ),
      )
    ),
    
    # Table Tab
    bs4TabItem(
      tabName = "tableTab",
      fluidRow(
        column(
          width = 4, 
          uiOutput("table_tab_filters")
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
  dashboardHeader("Practice 2"),
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session) {
  # https://alvaroferro.shinyapps.io/practica2/
  
  # Import's CSV
  sunspots <- read.csv(file = 'data/sunspots.csv', colClasses = c('character', 'Date','numeric'))
  biostats <- read.csv(file = 'data/biostats.csv')[,c("Name", "Sex", "Age", "Height..in.", "Weight..lbs.")]
  
  values <- reactiveValues();
  values$sunspots <- sunspots;
  
  #---------------------------------------------------------------
  #------ Event's Plot Tabs
  #---------------------------------------------------------------
  
  # Render Interface UI with params  
  output$plot_tab_filters <- renderUI({
    dateMin <- min(sunspots$Month);
    dateMax <- max(sunspots$Month);
    
    bs4Card(
      width = NULL,
      title = "Filter",
      status = "orange",
      dateRangeInput(
        "dateRangeMonth", 
        "Date range:",
        start = dateMin,
        end   = dateMax,
        min   = dateMin,
        max   = dateMax
      ),
      actionButton("updatePlot", "Update", status = "success", icon = icon("refresh"))
    )
  })
  
  # Event click button update plot
  observeEvent(input$updatePlot, {
    dateMin <- input$dateRangeMonth[1];
    dateMax <- input$dateRangeMonth[2];
    data <- sunspots %>% dplyr::filter(Month >= dateMin, Month <= dateMax);
    values$sunspots <- data;
  })
  
  output$totalResults <- renderbs4InfoBox({
    if(is.null(values$sunspots)) return(NULL)
    
    totalResults = nrow(values$sunspots);
    
    bs4InfoBox(
      totalResults,
      value = "Total Results",
      icon = icon("th-list", lib = "glyphicon")
    )
  })
  
  output$totalRange <- renderbs4InfoBox({
    if(is.null(values$sunspots)) return(NULL)
    
    dateMin <- min(values$sunspots$Month);
    dateMax <- max(values$sunspots$Month);
    totalRange <- paste(dateMin, " - ", dateMax);
    
    # Exact age
    int <- interval(dateMin, dateMax)
    years <- trunc(time_length(int, "year"), 0);
    
    # Exact month
    year(dateMin) <- year(dateMax);
    int <- interval(dateMin, dateMax)
    months <- trunc(time_length(int, "month"), 0);
    
    bs4InfoBox(
      totalRange,
      value = "Min/Max Range Plot",
      subtitle = paste("Years: ", years, "- Months: ", months),
      icon = icon("calendar", lib = "glyphicon")
    )
  })
  
  # Render plot
  output$plot <- renderPlotly({
    if(is.null(values$sunspots)) return(NULL)
    
    plot <- ggplot(values$sunspots, aes(Month, Sunspots));
    plot <- plot + geom_line()
    ggplotly(plot)
  })
  
  #---------------------------------------------------------------
  #------ Event's Table Tabs
  #---------------------------------------------------------------
  
  # Render Interface UI with params  
  output$table_tab_filters <- renderUI({
    bs4Card(
      width = NULL,
      title = "Reactive Filter",
      status = "orange",
      selectInput("sex","Sex:", choices = c("ALL",unique(biostats$Sex))),
      numericInput("age", "Age:", min = min(biostats$Age), max = max(biostats$Age), value = median(biostats$Age)),
      numericInput("height", "Height:", min = min(biostats$Height..in.), max = max(biostats$Height..in.), value = median(biostats$Height..in.))
    );
  })
  
  # Reactive Data Biostats
  getDataBiostats <- reactive({
    if(is.null(input$age)) return(NULL)
    
    data <- biostats %>% dplyr::filter(Age >= input$age, Height..in. >= input$height);
    if(input$sex != "ALL"){
      data <- data %>% dplyr::filter(Sex == input$sex);
    }
    return(data);
  })
  
  # Event to render table Biostats
  output$tableBiostats <- renderDataTable({
    data <- getDataBiostats();
    if(is.null(data)) return(NULL);
    
    
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