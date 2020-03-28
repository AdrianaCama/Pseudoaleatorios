library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(ggfortify)


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
                            value = 10000
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
               id = "tabbox_generado",
               width = NULL,
               #background = "yellow",
               tabPanel("Números generados",
                        DT::dataTableOutput(outputId = "random_table"),
                        downloadButton("download", label = "Descargar CSV")
                        ),
               tabPanel("Histograma",
                        plotOutput("hist_uniformidad")
                        )
               ),
             tabBox(
               title = "Resutados de las pruebas",
               #solidHeader = TRUE,
               id = "tabbox_resultados",
               width = NULL,
               tabPanel("Región de rechazo",
                        plotOutput("hist_distribucion"),
                        textOutput("text_region")
                        ),
               tabPanel("Valor p",
                        valueBoxOutput("valuebox_pvalue", width = 100),
                        textOutput("text_pvalue")
                        )
               )
             )
      )
    )
)


server <- function(input, output) {
  shinyjs::hide(id = "download")
  shinyjs::hide(id = "hist_uniformidad")
  shinyjs::hide(id = "hist_distribucion")
  
  
  ### Generación de valores aleatorios #######################################################
  ############################################################################################
  reactive_go <- eventReactive(input$Button_go, {
    # Cambiar (Esto solo es para probar)
    numeros <- c(0.045555, 0.065749, 0.092871, 0.149668, 0.190782, 0.224291, 0.260000, 0.321474, 
                 0.332037, 0.392275, 0.404315, 0.431058, 0.468068, 0.495164, 0.569813, 0.631893, 
                 0.652066, 0.785885, 0.830250, 0.846584)
    
    shinyjs::show(id = "download")
    shinyjs::show(id = "hist_uniformidad")
    return(numeros)
  })

  output$random_table <- DT::renderDataTable({data.frame("Valores" = reactive_go())})
  output$hist_uniformidad <- renderPlot({
    ggplot(data.frame("Valores" = reactive_go())) + geom_histogram(aes(x=Valores,y=..density..), 
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
  
  ### Pruebas de aleatoriedad y/o uniformidad #################################################
  #############################################################################################
  
  # Prueba 4: Cramer-von Mises ######################
  ###################################################
  ###################################################
  CvM <- function(numeros, alpha){
    numeros_ordenados <- sort(numeros, decreasing=FALSE)
    N <- length(numeros_ordenados)
    empirica <- (2*seq(1, N, 1) - 1)/ (2*N)
    
    Y <- (1 / (12*N)) + sum((numeros_ordenados - empirica)^2)
    quantile_CvM <- qnorm(1-alpha)
    
    p_value <- pnorm(Y)

    
    if(Y > quantile_CvM){
      rechazo_por_region <- 1
    } else{
      rechazo_por_region <- 0
    }
    
    
    if(p_value <= alpha){
      rechazo_por_pvalue <- 1
    } else{
      rechazo_por_pvalue <- 0
    }
    
    return(list(Y, quantile_CvM, p_value, rechazo_por_region, rechazo_por_pvalue))
  }
  
  
  observeEvent(input$Button_evaluate, {
    #Codigo de las pruebas 
    if(input$pruebas=="Prueba de Cramer-von Mises"){
      prueba <- CvM(reactive_go(), as.numeric(input$alpha))
      estadistico <- round(as.numeric(prueba[1]), 2)
      cuantil <- round(as.numeric(prueba[2]), 2)
      pvalue <- round(as.numeric(prueba[3]), 2)
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
    }else{
      pvalue <- 0.5
      rechazo_por_region <- 1
      rechazo_por_pvalue<- 1
      }
    
    
    output$hist_distribucion <- renderPlot({
      ggdistribution(dnorm, seq(-3, 3, 0.1), mean = 0, sd = 1)
    })
    if(rechazo_por_region == 1){
      output$text_region <- renderText({paste("Existe suficiente evidencia para rechazar la uniformidad y/o independencia de los números generados.")})
    }else{
      output$text_region <- renderText({paste("No existe suficiente evidencia para rechazar la uniformidad y/o independencia de los números generados.")})
    }
    

    output$valuebox_pvalue <- renderValueBox({
      valueBox(
        pvalue, "Valor p", icon = icon("percent"),
        color = "teal"
      )
    })
    if(rechazo_por_pvalue == 1){
      output$text_pvalue <- renderText({paste("Existe suficiente evidencia para rechazar la uniformidad y/o independencia de los números generados.")})
    }else{
      output$text_pvalue <- renderText({paste("No existe suficiente evidencia para rechazar la uniformidad y/o independencia de los números generados.")})
    }
    
    
    shinyjs::show(id = "hist_distribucion")
  })
  
}

shinyApp(ui, server)
