library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(ggplot2)
library(REDCapR)



# ui
ui <- page_navbar(
  
  # Tema
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  title = "Visualizador de datos REDCap",
  
  # Panel lateral
  sidebar = sidebar(
    actionButton(inputId = "refresh", label = "Actualizar datos")
  ),
  
  # Panel principal
  nav_panel(
    title = "Tabla",
    card(
      full_screen = T,
      DT::dataTableOutput(outputId = "data_table")
    )
  ),
  nav_panel(
    title = "Grafico",
    card(
      full_screen = T,
      plotOutput(outputId = "plot_1")
    )
  )
)

# server
server <- function(input, output, session) {
  # Reactive value
  rv <- reactiveValues(datos = NULL)
  
  observe(
    {
      isolate(
        {
          if(is.null(rv$datos)) {
            rv$datos <- REDCapR::redcap_report(
              redcap_uri = "https://redcap.upch.edu.pe/api/", 
              token = Sys.getenv("token_prueba"), 
              report_id = 1002, 
              guess_type = F
            )$data
          }
        }
      )
    }
  )
  
  observeEvent(
    input$refresh,
    {
      rv$datos <- REDCapR::redcap_report(
        redcap_uri = "https://redcap.upch.edu.pe/api/", 
        token = Sys.getenv("token_prueba"), 
        report_id = 1002, 
        guess_type = F
      )$data
    }
  )
  
  # Tabla reactiva
  output$data_table <- DT::renderDataTable(
    
    {
      
      req(rv$datos)
      DT::datatable(
        rv$datos,
        options = list(
          paging = F,
          pageLength = 10,
          scrollX = F,
          scrollY = F,
          autoWidth = T,
          server = F,
          dom = "t",
          colummDefs = list(
            list(
              targets = "_all",
              className = "dt_center"
            )
          )
        ),
        rownames = F,
        filter = "none"
      )
      
    }
    
  )
  
  # Grafico reactivo
  output$plot_1 <- renderPlot(
    {
      req(rv$datos)
      
      ggplot(rv$datos, aes(x = as.integer(pull(rv$datos, edad)))) +
        geom_histogram() +
        theme_minimal() +
        labs(x = "Edad", y = "Frecuencia", title = "Distribucion de edad")
    }
  )
  
}

# app
shinyApp(ui = ui, server = server)