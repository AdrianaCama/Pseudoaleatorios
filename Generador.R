library(shiny)
library(shinydashboard)
library(shinyjs)

ui <- dashboardPage(
  # App title ----
  dashboardHeader(title = "Generador de números aleatorios"),
  #skin = "green",
  dashboardSidebar(
    disable = TRUE
    #sidebarMenu(
    #   menuItem("Sistema de Inventarios", tabName = "inventarios", icon = icon("truck-loading")),
    #   menuItem("Sistema de Colas", tabName = "colas", icon = icon("user-clock"))
    #)
  ),
  
  # Sidebar panel for inputs ----
  dashboardBody(
    useShinyjs(),
    fluidRow(
      column(width = 6,
             box(
               title = "Valores",
               width = NULL,
               #solidHeader = TRUE,
               #status = "warning",
               # Input: Numerics para ingresar valores ----
               numericInput(inputId = "a",
                            label = "a:",
                            min = 0,
                            value = 2^7 + 1
                            ),
               numericInput(inputId = "m",
                            label = "m:",
                            min = 0,
                            value = 2^35
                            ),
               numericInput(inputId = "c",
                            label = "c:",
                            min = 0,
                            value = 1
                            ),
               numericInput(inputId = "n",
                            label = "n:",
                            min = 0,
                            value = 20
                            ),
               numericInput(inputId = "seed",
                            label = "Semilla:",
                            min = 0,
                            value = 621
                            ),
               radioButtons(inputId = "generador",
                            label = "Elige un generador: ",
                            choices = c( "Generador de valores de una v.a. con distribución 
                                        uniforme", "Monte Carlo", 
                                         "Generador Congruencial Multiplicativo", 
                                        "Generador Congruencial Mixto o Congruencial Lineal"),
                            selected = "Monte Carlo"
               ),
               actionButton(inputId = "Button_go", label = "Generar")
              ),
             box(
               title = "Pruebas estadísticas",
               width = NULL,
               radioButtons(inputId = "pruebas",
                            label = "Elige una prueba: ",
                            choices = c("Prueba de la Ji Cuadrada", "Prueba Serial", 
                                        "Prueba de Kolmogorov-Smirnov", 
                                        "Prueba de Cramer-von Mises",
                                        "Prueba de las corridas", "Correlación de atrasos"),
                            selected = "Prueba de la Ji Cuadrada"
                            ),
               selectInput(inputId = "alpha",
                           label = "Selecciona el nivel de significancia ",
                           choices = c(0.01, 0.025, 0.05, 0.1),
                           selected = 0.05),
               actionButton(inputId = "Button_evaluate", label = "Probar uniformidad y/o 
                            independencia")
               )
             ),
      column(width = 6,
             tabBox(
               title = "Generado",
               #solidHeader = TRUE,
               id = "tabbox",
               width = NULL,
               #background = "yellow",
               tabPanel("Números generados",
                        DT::dataTableOutput(outputId = "random_table"),
                        downloadButton("download", label = "Descargar CSV")
                        ),
               tabPanel("Histograma",
                        plotOutput("hist")
                        )
               ),
             box(
               title = "Resultados de las pruebas",
               width = NULL
               # Cambiar por algo
               )
             )
      )
    )
)


server <- function(input, output) {
  shinyjs::hide(id = "download")
  shinyjs::hide(id = "hist")
  
  reactive_go <- eventReactive(input$Button_go, {
    # Cambiar (Esto solo es para probar)
    numeros <- c(0.045555, 0.065749, 0.092871, 0.149668, 0.190782, 0.224291, 0.260000, 0.321474, 
                 0.332037, 0.392275, 0.404315, 0.431058, 0.468068, 0.495164, 0.569813, 0.631893, 
                 0.652066, 0.785885, 0.830250, 0.846584)
    
    shinyjs::show(id = "download")
    shinyjs::show(id = "hist")
    numeros_dt <- data.frame("Valores" = numeros)
    return(numeros_dt)
  })

  output$random_table <- DT::renderDataTable({reactive_go()})
  output$hist <- renderPlot({
    ggplot(reactive_go()) + geom_histogram(aes(x=Valores,y=..density..), 
                                        alpha=0.7, 
                                        breaks=seq(0, 1, 0.05), 
                                        closed="left", 
                                        color="white", 
                                        fill="orange") + 
      stat_function(fun = dunif, args = list(0,1), colour = "dodgerblue3",size=1) 
  })
  output$download <- downloadHandler(filename = "aleatorios.csv",
  content = function(file) {
    write.csv(reactive_go(), file)
    # Cambiar línea anterior por línea siguiente si se prefiere sin índices
    #write.csv(numeros_dt, file, row.names = FALSE)
  })
  
}

shinyApp(ui, server)
