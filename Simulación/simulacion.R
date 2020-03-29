library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(knitr)


a <- 1 # Cost mensual por almacenar un art?culo
e <- 5 # Cost mensual por no tener un art?culo :(
error <- 0

unif_generator <- function(n,min=0,max=1,seed=621,m=(2^31)-1,a=724938285,c=0){
  size <- n
  if(n == 1){
    n <- 621*n
  }
  
  res <- rep(0, n)
  res[1] <- (a*seed+c)%%m
  for(i in 2:n){
    res[i] <- (a*res[i-1]+c)%%m
  }
  res <- res/m
  
  res <- min + res*(max-min)
  res <- res[1:size]
  return(res)
}

exp_generator <- function(n,rate=1){
  res <- unif_generator(n)
  mean <- 1/rate
  res <- (-1)*(mean)*log(res)
  return(res)
}

ui <- dashboardPage(
  # App title ----
  dashboardHeader(title = "Simulaciones"),
  skin = "green",
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sistema de Inventarios", tabName = "inventarios", icon = icon("truck-loading")),
      menuItem("Sistema de Colas", tabName = "colas", icon = icon("user-clock"))
    )
  ),
  
  # Sidebar panel for inputs ----
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "inventarios",
              fluidRow(
                column(width = 6,
                       box(
                         title = "Controles",
                         width = NULL,
                         #solidHeader = TRUE,
                         status = "warning",
                         # Input: Slider para n, s y S ----
                         sliderInput(inputId = "n",
                                     label = "Número de meses:",
                                     min = 6,
                                     max = 360,
                                     value = 120),
                         sliderInput(inputId = "s",
                                     label = "s:",
                                     min = 0,
                                     max = 100,
                                     value = 40),
                         sliderInput(inputId = "S",
                                     label = "S:",
                                     min = 40,
                                     max = 150,
                                     value = 80),
                         
                         actionButton(inputId = "Button_inventory", label = "Ejecutar")
                       ),
                       box(
                         title = "Histograma",
                         #solidHeader = TRUE,
                         id = "box_plot",
                         width = NULL,
                         background = "yellow",
                         plotOutput(outputId = "distPlot")
                       )
                ),
                column(width = 6,
                       box(
                         title = "Indicadores",
                         width = NULL,
                         #solidHeader = TRUE,
                         status = "warning",
                         # textOutput(outputId = "label_I_mas"),
                         # verbatimTextOutput(outputId = "I_mas"),
                         # textOutput(outputId = "label_I_menos"),
                         # verbatimTextOutput(outputId = "I_menos"),
                         # textOutput(outputId = "label_costo_almacenamiento"),
                         # verbatimTextOutput(outputId = "costo_almacenamiento"),
                         # textOutput(outputId = "label_costo_escases"),
                         # verbatimTextOutput(outputId = "costo_escases"),
                         # textOutput(outputId = "label_costo_total"),
                         # verbatimTextOutput(outputId = "costo_total"),
                         valueBoxOutput("infobox_I_mas", width = 100),
                         valueBoxOutput("infobox_I_menos", width = 100),
                         valueBoxOutput("infobox_costo_almacenamiento", width = 100),
                         valueBoxOutput("infobox_costo_escases", width = 100),
                         valueBoxOutput("infobox_costo_total", width = 100)
                       )
                )
              )
      ),
      tabItem(tabName = "colas",
              fluidRow(
                column(width = 6,
                       box(id = "options_box",
                           width = NULL,
                           title = "Controles",
                           status = "primary",
                           radioButtons(inputId = "Checkbox",
                                        label = "Seleccionar tipo de simulación",
                                        choices = c("Manual (archivo CSV)", "Manual (directo)", "Generar datos"),
                                        selected = "Manual (archivo CSV)"),
                           # Input: Slider para A, S y n ----
                           textInput(inputId = "A", 
                                     label = "Tiempos entre llegadas:", 
                                     value = "",
                                     placeholder = "Números separados por comas"),
                           textInput(inputId = "S2",
                                     label = "Tiempos de servicio:",
                                     value = "",
                                     placeholder = "Números separados por comas"),
                           fileInput("file1", "Elegir archivo CSV",
                                     accept = c(
                                       "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")
                           ),
                           tags$hr(),
                           checkboxInput("header", "Header", TRUE),
                           numericInput(inputId = "lambda_llegadas", 
                                        label = "Media del tiempo entre llegadas:",
                                        value = 10, 
                                        min = 1, 
                                        max = 20),
                           numericInput(inputId = "lambda_servicios", 
                                        label = "Media del tiempo de servicio:",
                                        value = 10, 
                                        min = 1, 
                                        max = 20),
                           sliderInput(inputId = "n2",
                                       label = "n:",
                                       min = 0,
                                       max = 20,
                                       value = 6),
                           sliderInput(inputId = "capacidad",
                                       label = "Capacidad de la cola:",
                                       min = 0,
                                       max = 20,
                                       value = 4),
                           
                           actionButton(inputId = "Button_service", label = "Ejecutar"),
                           
                           downloadButton("download", label = "Descargar PDF")
                           #actionButton(inputId = "download", label = "Descargar PDF")
                           #downloadButton('report')
                       )
            
                       ),
                column(width = 6,
                       box(title = "Indicadores",
                           width = NULL,
                           id = "prueba",
                           status = "primary",
                           #textOutput(outputId = "label_p"),
                           #verbatimTextOutput(outputId = "p"),
                           #textOutput(outputId = "label_d"),
                           #verbatimTextOutput(outputId = "d"),
                           #textOutput(outputId = "label_q"),
                           #verbatimTextOutput(outputId = "q"),
                           #textOutput(outputId = "label_u"),
                           #verbatimTextOutput(outputId = "u")
                           valueBoxOutput("infobox_d", width = 100),
                           valueBoxOutput("infobox_q", width = 100),
                           valueBoxOutput("infobox_u", width = 100)
                       ),
                       box(title = "Proporción de tiempo observado en la simulación donde se observó i clientes en la cola.",
                           width = NULL,
                           id = "box_proportion",
                           status = "primary",
                           tableOutput("table_p")
                       )
                       
                       )
              )              
      )
    )
  )
)


server <- function(input, output) {
  #output$label_I_mas <- renderText({"NÃºmero promedio de artÃ�culos por mes que existen en el inventario:"})
  #output$I_mas <- renderText({0})
  #output$label_I_menos <- renderText({"NÃºmero promedio de artÃ�culos retrasados por mes:"})
  #output$I_menos <- renderText({0})
  #output$label_costo_almacenamiento <- renderText({"Costo promedio de almacenamiento por mes:"})
  #output$costo_almacenamiento <- renderText({0})
  #output$label_costo_escases <- renderText({"Costo promedio por mes por escases:"})
  #output$costo_escases <- renderText({0})
  #output$label_costo_total <- renderText({"Costo total por almacenamiento y escases:"})
  #output$costo_total <- renderText({0})
  #output$infobox_I_mas <- renderValueBox({
  
  shinyjs::hide(id = "download")
  # Checar esto. Tenemos que encontrar un comando que agarre el archivo .Rnw y le haga weave usando Sweave y no knitr.
  
  output$download <- downloadHandler(
    # if(error == 1){
    #   showModal(modalDialog(
    #   title = "Error",
    #   paste0("Aún no existen datos para exportar."),
    #   easyClose = TRUE,
    #   footer = NULL)
    #   )
    # } else {
    #     directory <- getwd()
    #     Sweave(paste0(directory,'/report.Rnw'))
    #     tools::texi2pdf(paste0(directory,'/report.tex'))
    # }
    
    filename = function() {
      paste("planillas_colas","pdf",sep = ".")
    },

    content = function(file) {
      directory <- getwd()
      Sweave(paste0(directory,'/report.Rnw'))
      out = tools::texi2pdf(paste0(directory,'/report.tex'))
      file.copy(paste0(directory,'/report.pdf'), file)
      #file.rename(out, file)
    },

    contentType = 'application/pdf'
  )
  
  # observeEvent(input$download,{
  #               if(error == 1){
  #                 showModal(modalDialog(
  #                   title = "Error",
  #                   paste0("Aún no existen datos para exportar."),
  #                   easyClose = TRUE,
  #                   footer = NULL)
  #                 )
  #               } else {
  #                 directory <- getwd()
  #                 Sweave(paste0(directory,'/report.Rnw'))
  #                 tools::texi2pdf(paste0(directory,'/report.tex'))
  #                 
  #                 #system(paste0('start "', paste0(directory,'/report.pdf'), '"'))
  #                 #system('open "/path/to/file.pdf"')
  #               }
  #               }
  #              )

  
  output$infobox_I_mas <- renderValueBox({
    valueBox(
      0, "Número promedio de artículos por mes en inventario", icon = icon("boxes"),
      color = "teal"
    )
  })
  output$infobox_I_menos <- renderValueBox({
    valueBox(
      0, "Número promedio de artículos retrasados por mes", icon = icon("tired"),
      color = "teal"
    )
  })
  output$infobox_costo_almacenamiento <- renderValueBox({
    valueBox(
      paste0("$",0), "Costo promedio de almacenamiento por mes", icon = icon("money"),
      color = "orange"
    )
  })
  output$infobox_costo_escases <- renderValueBox({
    valueBox(
      paste0("$",0), "Costo promedio por mes por escases", icon = icon("money"),
      color = "orange"
    )
  })
  output$infobox_costo_total <- renderValueBox({
    valueBox(
      paste0("$",0), "Costo promedio total por almacenamiento y escases", icon = icon("list"),
      color = "green"
    )
  })
  shinyjs::hide(id = "distPlot")
  shinyjs::hide(id = "box_plot")
  
  ###
  observeEvent(input$Button_inventory, {
    if (input$s>=input$S){
      showModal(modalDialog(
        title = "Error",
        paste0("El valor de S debe ser mayor al valor de s"),
        easyClose = TRUE,
        footer = NULL)
      )
      output$infobox_I_mas <- renderValueBox({
        valueBox(
          0, "Número promedio de artículos por mes en inventario", icon = icon("boxes"),
          color = "teal"
        )
      })
      output$infobox_I_menos <- renderValueBox({
        valueBox(
          0, "Número promedio de artículos retrasados por mes", icon = icon("tired"),
          color = "teal"
        )
      })
      output$infobox_costo_almacenamiento <- renderValueBox({
        valueBox(
          paste0("$",0), "Costo promedio de almacenamiento por mes", icon = icon("money"),
          color = "orange"
        )
      })
      output$infobox_costo_escases <- renderValueBox({
        valueBox(
          paste0("$",0), "Costo promedio por mes por escases", icon = icon("money"),
          color = "orange"
        )
      })
      output$infobox_costo_total <- renderValueBox({
        valueBox(
          paste0("$",0), "Costo promedio total por almacenamiento y escases", icon = icon("list"),
          color = "green"
        )
      })
      shinyjs::hide(id = "distPlot")
      shinyjs::hide(id = "box_plot")
    }
    else {
      ########################################################################
      ########################################################################
      ########################################################################
      set.seed(21)
      
      # Inicializaci?n
      S <- input$S # M?ximo nivel de inventario
      s <- input$s # Nivel de inventario por debajo del cu?l se pedir? m?s inventario
      n <- input$n # N?mero de periodos
      
      K <- 32 # Costo de servicio por pedir inventario
      ai <- 3 # Costo por art?culo al pedir inventario
      inventory <- S # Inicializaci?n de inventario
      
      a <- 1 # Cost mensual por almacenar un art?culo
      e <- 5 # Cost mensual por no tener un art?culo :(
      
      pedido_activo <- 0
      
      
      ########################################################################
      demand_times <- c()
      
      temp <- exp_generator(n = 12*n, rate = 10)
      for (i in 1:length(temp)) {
        demand_times[i] <- sum(temp[1:i]) 
      }
      ########################################################################
      
      ########################################################################
      # Tiempo de llegada de un pedido
      

      purchase_times <- unif_generator(n = n, min = 0.5, max = 1)
      ########################################################################
      
      
      
      
      
      ########################################################################
      # Tiempos de evaluaci?n de inventario
      evaluation_times <- seq(from = 1, to = n)
      ########################################################################
      
      
      demand_times_matrix <- cbind(demand_times,rep(2, each = length(demand_times)))
      evaluation_times_matrix <- cbind(evaluation_times,rep(1, each = length(evaluation_times)))
      
      
      events <- rbind(demand_times_matrix,evaluation_times_matrix)
      events <- events[order(events[,1],decreasing=FALSE),]
      
      
      events <- events[events[,1]<=n,]
      
      
      
      temp <- rep(0, each = length(events[,1]))
      events <- cbind(events, temp, temp, temp, temp)
      colnames(events) <- c("Tiempo","Evento", "Cantidad","I(t)", "I(t)+", "I(t)-")
      
      
      ########################################################################
      # Put all times in matrix?
      ########################################################################
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      ########################################################################################
      # Subrutina de demanda
      ########################################################################################
      demand <- function() {
        # Generar el tama?o de la demanda
        temp <- unif_generator(n = 1, min = 0, max = 1)
        if (temp < (1/6)){
          demand_size <- 1
        } else if (temp < (1/2)){
          demand_size <- 2
        } else if (temp < (5/6)){
          demand_size <- 3
        } else {
          demand_size <- 4
        }
        # Disminuir el nivel de inventario por el tama?o de esta demanda
        inventory = inventory - demand_size
        
        
        
        return(list("Quantity" = demand_size, "Inventory" = inventory))
      }
      
      ########################################################################################
      # Subrutina de llegada
      ########################################################################################
      receive <- function() {
        inventory <- inventory + restock_size
        pedido_activo <- 0
        restock_size <- restock_size
        return(list("Quantity" = restock_size, "Inventory" = inventory))
      }
      
      
      ###########################################################################################
      # Subrutina de evaluaci?n
      ###########################################################################################
      receive_time <- 0
      evaluation <- function() {
        if (inventory < s) {
          # Determinar la cantidad que ser? ordenada
          
          if (inventory > 0){
            Z <- S - inventory
          }
          else{
            Z <- S
          }
          # Calcular el tiempo de llegada
          receive_time <- unif_generator(n = 1, min = 0.5, max = 1)
          
          # A?adir fila con el evento de llegada
          new_row <- c(events[i,1] + receive_time, 3, Z, 0, 0, 0)
          events <- rbind(events, new_row)
          
          # Ordenar nuevamente las filas
          events <- events[order(events[,1],decreasing=FALSE),]
          
          # Considerar el costo de la orden y actualizar la estad?stica
          cost_of_order <- K + ai*Z
          
          if (pedido_activo == 1) {
            Z <- 0
          } else {
            Z <- Z
          }
          pedido_activo <- 1
        }
        else {
          Z <- 0
          cost_of_order <- 0
          receive_time <- 0
          # Hacer nada
        }
        
        inventory <- inventory
        
        # Estipular el pr?ximo tiempo de evaluaci?n del inventario
        ### Ya est? calculado
        
        #costo_por_pedir_inventario <- costo_por_pedir_inventario + cost_of_order
        
        return(list("Quantity" = Z, "Inventory" = inventory, "Receive_Time" = receive_time, "Events" = events))
      }
      
      
      
      
      
      
      
      ############################################################################################################
      ############################################################################################################
      ############################################################################################################
      
      ################### Programa ########################
      
      ############################################################################################################
      ############################################################################################################
      ############################################################################################################
      receive_times <- c()
      change_times <- c()
      inventory_change <- c()
      
      length <- dim(events)[1]
      
      i <- 1
      j <- 2
      
      
      while (i <= length) {
        if (events[i,2] == 1){
          event <- evaluation()
          restock_size <- event$Quantity
          events <- event$Events
          
        } else if (events[i,2] == 2) {
          event <- demand()
          change_times[j] <- events[i,1]
        }
        else if (events[i,2] == 3) {
          event <- receive()
          receive_times[i] <- event$receive_time
          change_times[j] <- events[i,1]
        }
        else {
        }
        
        inventory <- event$Inventory
        
        # Cantidad
        events[i,3] <- event$Quantity
        # Inventory
        events[i,4] <- event$Inventory
        # Inventory +
        events[i,5] <- max(inventory,0)
        # Inventory - 
        events[i,6] <- max(-inventory,0)
        
        if(events[i,2] == 2 || events[i,2] == 3){
          inventory_change[j] <- events[i,4]
          j <- j + 1
        }
        
        length <- dim(events)[1]
        
        i <- i + 1
        
        
      }
      
      
      costo_almacenamiento <- 0
      costo_escases <- 0
      
      change_times[1] <- 0
      inventory_change[1] <- S
      change_times[length(inventory_change)+1] <- events[length(events[,1]),1]
      inventory_change[length(change_times)] <- events[length(events[,1]),4]
      
      inventario_promedio <- 0
      faltante_promedio <- 0
      for(j in 2:length(inventory_change)){
        if(inventory_change[j-1] >= 0){
          inventario_promedio <- inventario_promedio + (change_times[j]-change_times[j-1])*inventory_change[j-1]
        }
        if(inventory_change[j-1] < 0){
          faltante_promedio <- faltante_promedio + (change_times[j]-change_times[j-1])*(-inventory_change[j-1])
        }
      }
      
      inventario_promedio <- round(inventario_promedio/n,2)
      faltante_promedio <- round(faltante_promedio/n,2)
      costo_almacenamiento_promedio <- round(inventario_promedio * a, 2)
      costo_escases_promedio <- round(faltante_promedio * e, 2)
      costo_total_promedio <- costo_almacenamiento_promedio + costo_escases_promedio 
      
      print(length(inventory_change))
      print(change_times)
      print(inventory_change)
      print(events)
      
      variable1 <- inventario_promedio
      variable2 <- faltante_promedio
      variable3 <- costo_almacenamiento_promedio
      variable4 <- costo_escases_promedio
      variable5 <- costo_total_promedio
      #output$I_mas <- renderText({variable1})
      #output$I_menos <- renderText({variable2})
      #output$costo_almacenamiento <- renderText({variable3})
      #output$costo_escases <- renderText({variable4})
      #output$costo_total <- renderText({variable5})
      
      
      output$infobox_I_mas <- renderValueBox({
        valueBox(
          variable1, "Número promedio de artículos por mes en inventario", icon = icon("boxes"),
          color = "teal"
        )
      })
      output$infobox_I_menos <- renderValueBox({
        valueBox(
          variable2, "Número promedio de artículos retrasados por mes", icon = icon("tired"),
          color = "teal"
        )
      })
      output$infobox_costo_almacenamiento <- renderValueBox({
        valueBox(
          paste0("$",variable3), "Costo promedio de almacenamiento por mes", icon = icon("money"),
          color = "orange"
        )
      })
      output$infobox_costo_escases <- renderValueBox({
        valueBox(
          paste0("$",variable4), "Costo promedio por mes por escases", icon = icon("money"),
          color = "orange"
        )
      })
      output$infobox_costo_total <- renderValueBox({
        valueBox(
          paste0("$",variable5), "Costo promedio total por almacenamiento y escases", icon = icon("list"),
          color = "green"
        )
      })
      
      ########################################################################
      ########################################################################
      ########################################################################
      
      ### GrÃ¡fica
      
      output$distPlot <- renderPlot({
        events1 <- data.frame(x = events[,1], y = events[,4])
        events2 <- data.frame(x = events[,1], y = events[,5])
        events3 <- data.frame(x = events[,1], y = events[,6])

        ggplot() + 
          geom_step(data=events1, mapping=aes(x=x, y=y, color = "black"), alpha = 1) +
          geom_step(data=events2, mapping=aes(x=x, y=y, color = "blue"), linetype=3, alpha = 0.7) +
          geom_step(data=events3, mapping=aes(x=x, y=y, color = "red"), linetype=4, alpha = 0.5) +
          labs(
            x = "Mes",
            y = "Nivel de inventario"
            #color = "Cylinders"
          ) +
          scale_color_discrete(name = "Inventario", labels = c("I", "I+", "I-"))+
          theme(legend.position=c(0.95, 0.95),legend.justification = c("right", "top"))
      })
      
      ###
      shinyjs::show(id = "distPlot")
      shinyjs::show(id = "box_plot")
    }
  })
  ###
  
  
  
  
  ###################
  # Sistema de Colas
  ###################
  #output$label_p <- renderText({"Proporción de tiempo observado en la simulación donde se observoÌ i clientes en la cola:"})
  #output$p <- renderText({0})
  #output$label_d <- renderText({"Retraso promedio en la cola de los n clientes:"})
  #output$d <- renderText({0})
  #output$label_q <- renderText({paste("NÃºmero promedio de clientes en la cola, al considerar ", input$n ," clientes:")})
  #output$q <- renderText({0})
  #output$label_u <- renderText({"ProporciÃ³n de tiempo durante la simulación que el servidor estÃ¡ ocupado:"})
  #output$u <- renderText({0})
  output$infobox_d <- renderValueBox({
    valueBox(
      0, "Retraso promedio en la cola de los n clientes:", icon = icon("angry"),
      color = "teal"
    )
  })
  output$infobox_q <- renderValueBox({
    valueBox(
      0, "Número promedio de clientes en la cola, al considerar n clientes:", icon = icon("users"),
      color = "orange"
    )
  })
  output$infobox_u <- renderValueBox({
    valueBox(
      0, "Proporción de tiempo durante la simulación que el servidor está ocupado:", icon = icon("tired"),
      color = "orange"
    )
  })
  shinyjs::hide(id = "box_table")
  shinyjs::hide(id = "table_p")
  shinyjs::hide(id = "download")
  
  ###################################################
  ############ Deshabilitar opciones ################
  ###################################################
  observeEvent(input$Checkbox, {
    
    if(input$Checkbox == "Manual (directo)"){
      shinyjs::hide(id = "lambda_llegadas")
      shinyjs::hide(id = "lambda_servicios")
      shinyjs::show(id = "A")
      shinyjs::show(id = "S2")
      shinyjs::hide(id = "file1")
      shinyjs::hide(id = "header")
    }
    else if(input$Checkbox == "Manual (archivo CSV)"){
      shinyjs::hide(id = "lambda_llegadas")
      shinyjs::hide(id = "lambda_servicios")
      shinyjs::hide(id = "A")
      shinyjs::hide(id = "S2")
      shinyjs::show(id = "file1")
      shinyjs::show(id = "header")
    }
    else{
      shinyjs::hide(id = "A")
      shinyjs::hide(id = "S2")
      shinyjs::show(id = "lambda_llegadas")
      shinyjs::show(id = "lambda_servicios")
      shinyjs::hide(id = "file1")
      shinyjs::hide(id = "header")
    }
  }
  )
  
  observeEvent(input$Button_service, {
    error <- 0
    if(input$Checkbox == "Manual (archivo CSV)"){
      
      ###################################################
      ####### Extraer números y guardarlos en vector ####
      ###################################################
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      df = read.csv(inFile$datapath, header = input$header)
      
      A_i <- as.numeric(na.exclude(df[,1]))
      S_i <- as.numeric(na.exclude(df[,2]))
      
      if ((FALSE %in% !is.na(as.numeric(A_i))) == TRUE || (FALSE %in% !is.na(as.numeric(S_i))) == TRUE){
        showModal(modalDialog(
          title = "Error",
          paste0("Los valores ingresados contienen un valor no numérico"),
          easyClose = TRUE,
          footer = NULL
        ))
        #Reset variables
        A_i <- c()
        S_i <- c()
        error <- 1
        shinyjs::hide(id = "download")
      } else if (length(S_i) > length(A_i)){
        # Checar si hay más salidas que entradas
        showModal(modalDialog(
          title = "Error",
          paste0("Los datos ingresados contienen más salidas que entradas. Revise los datos e intente nuevamente."),
          easyClose = TRUE,
          footer = NULL
        ))
        #Reset variables
        A_i <- c()
        S_i <- c()
        error <- 1
        shinyjs::hide(id = "download")
      } else {
        ## Hacer el código
        error <- 0
      }
      if (input$n2>length(A_i) || (input$n2)-1>length(S_i)){
        showModal(modalDialog(
          title = "Error",
          paste0("El valor de n es mayor que el número de entradas o salidas."),
          easyClose = TRUE,
          footer = NULL)
        )
        error <- 1
        shinyjs::hide(id = "download")
      }
      if (input$n2>length(A_i) || input$n2>length(S_i)){
        showModal(modalDialog(
          title = "Error",
          paste0("El valor de n es mayor que el número de entradas o salidas."),
          easyClose = TRUE,
          footer = NULL)
        )
        error <- 1
        shinyjs::hide(id = "download")
      }
      
    }
    else if(input$Checkbox == "Manual (directo)"){
      ###################################################
      ####### Extraer números y guardarlos en vector ####
      ###################################################
      A_i <- as.numeric(unlist(strsplit(input$A,",")))
      S_i <- as.numeric(unlist(strsplit(input$S2,",")))
      
      if ((FALSE %in% !is.na(as.numeric(A_i))) == TRUE || (FALSE %in% !is.na(as.numeric(S_i))) == TRUE){
        showModal(modalDialog(
          title = "Error",
          paste0("Los valores ingresados contienen un valor no numérico"),
          easyClose = TRUE,
          footer = NULL
        ))
        #Reset variables
        A_i <- c()
        S_i <- c()
        error <- 1
        shinyjs::hide(id = "download")
      } else if (length(S_i) > length(A_i)){
        # Checar si hay más salidas que entradas
        showModal(modalDialog(
          title = "Error",
          paste0("Los datos ingresados contienen más salidas que entradas. Revise los datos e intente nuevamente."),
          easyClose = TRUE,
          footer = NULL
        ))
        #Reset variables
        A_i <- c()
        S_i <- c()
        error <- 1
        shinyjs::hide(id = "download")
      } else {
        ## Hacer el código
        error <- 0
      }
      if (input$n2>length(A_i) || (input$n2)-1>length(S_i)){
        showModal(modalDialog(
          title = "Error",
          paste0("El valor de n es mayor que el número de entradas o salidas."),
          easyClose = TRUE,
          footer = NULL)
        )
        error <- 1
        shinyjs::hide(id = "download")
      }
      if (input$n2>length(A_i) || input$n2>length(S_i)){
        showModal(modalDialog(
          title = "Error",
          paste0("El valor de n es mayor que el número de entradas o salidas."),
          easyClose = TRUE,
          footer = NULL)
        )
        error <- 1
        shinyjs::hide(id = "download")
      }
    }
    
    
    
    else{
      ###################################################
      ######### Extraer parámetros del formulario #######
      ###################################################
      
      n2 <- input$n2
      lambda_llegadas <- input$lambda_llegadas
      lambda_servicios <- input$lambda_servicios
      
      ###################################################
      ###### Generar tiempos de llegada y salida ########
      ###################################################
      A_i <- round(exp_generator(n = n2 + 1, rate = 1/(lambda_llegadas)),2)
      S_i <- round(exp_generator(n = n2 + 1, rate = 1/(lambda_servicios)),2)
      
      #Checar si lambda es no númerico
      if((!is.na(as.numeric(lambda_llegadas)) == FALSE || !is.na(as.numeric(lambda_servicios))) == FALSE){
        showModal(modalDialog(
          title = "Error",
          paste0("La media de la distribución contiene un valor no numérico"),
          easyClose = TRUE,
          footer = NULL
        ))
        error <- 1
        shinyjs::hide(id = "download")
      }
      else {
        error <- 0
      }
      
      
      
      if (input$n2>length(A_i) || (input$n2)-1>length(S_i)){
        showModal(modalDialog(
          title = "Error",
          paste0("El valor de n es mayor que el número de entradas o salidas."),
          easyClose = TRUE,
          footer = NULL)
        )
        error <- 1
        shinyjs::hide(id = "download")
      }
      
    }
    
    # if (input$n2>length(A_i) || input$n2>length(S_i)){
    #   showModal(modalDialog(
    #     title = "Error",
    #     paste0("El valor de n es mayor que el número de entradas o salidas."),
    #     easyClose = TRUE,
    #     footer = NULL)
    #   )
    #   error <- 1
    #   shinyjs::hide(id = "download")
    # }
    
    # if(n>length(A_i) || n > length(S_i)){
    #   showModal(modalDialog(
    #     title = "Error",
    #     paste0("No hay suficientes eventos para el valor especificado de n. Verifique sus datos."),
    #     easyClose = TRUE,
    #     footer = NULL))
    #   shinyjs::hide(id = "download")
    #   error <- 1
    # }
    # 
    
    if (error == 0){
      numero_clientes <- input$n2
      limite_cola <- input$capacidad
      
      #Si todos los datos están bien, correr el programa con los datos generados
      ##
      #################################################################################
      #################################################################################
      # Programa de Colas
      #################################################################################
      #################################################################################
      
      #1)
      
      #### Problema original
      llegadas = A_i
      servicio = S_i
      print(llegadas)
      print(servicio)
      
      ########################################
      # Comentar esto después
      # llegadas= c(1.3,10,2.1,2.2,4.3)
      # servicio=c(9.1,5,3,1,2)
      # llegadas <- c(0.38  ,5.20 ,12.69  ,1.23 , 0.66,  3.55,  1.62 , 0.71,  0.34,  0.35)
      # servicio <- c(8.29,12.29  ,0.14,  1.71,  1.04,  3.77, 18.98,  8.22 ,37.85 , 9.74)
      # 
      # 
      # llegadas <- c(1.1,2.5,0.5,0.7,2,0.3)
      # servicio <- c(0.7,2.1,0.3,1.1)
      # llegadas <- c(2.18 ,6.00, 1.33, 1.81, 0.25, 5.05, 0.75)
      # servicio <- c(7.91, 28.06, 17.88 , 1.35,  0.25,  4.16, 13.98)
      # limite_cola <- 9
      # n <- 4
      ########################################
      ########################################
      
      
      # Si dos eventos (llegada y salida) coinciden, mandar un mensaje de error
      
      limite_cola <- limite_cola
      n = numero_clientes
      
      # if(n>length(llegadas) || n > length(servicio)){
      #   showModal(modalDialog(
      #     title = "Error",
      #     paste0("No hay suficientes eventos para el valor especificado de n. Verifique sus datos."),
      #     easyClose = TRUE,
      #     footer = NULL))
      #   shinyjs::hide(id = "download")
      #   break
      # }
      
      
      #proceso
      tiemposl=c()
      tiemposl[1]= llegadas[1]
      for (i in 2:length(llegadas)){
        tiemposl[i]=llegadas[i]+tiemposl[i-1]
      }
      
      
      
      tiemposs=c()
      tiemposs[1]= llegadas[1] + servicio[1]
      for (i in 2: length(servicio)){
        if(tiemposl[i]>tiemposs[i-1]){
          tiemposs[i]= servicio[i]+tiemposl[i]
        }
        else{
          tiemposs[i]= servicio[i]+tiemposs[i-1]
        }
      }
      
      print(llegadas)
      print(servicio)
      print(tiemposl)
      print(tiemposs)
      
      
      tiempo=c(0,tiemposl,tiemposs)
      tiempo=sort(tiempo)
      #print(tiempo)
      
      #Subrutina inicio
      A=tiemposl[1]
      D= Inf
      estatus=0
      cola=0
      tiemposfila=c()
      ultimoevento=0
      eventoanterior=0
      hora=0
      clientes=0
      retraso=c()
      retrasototal=0
      areaqt=0
      qt=0
      areabt=0
      bt=0
      entradas=0
      salidas=0
      evento<-"Inicio"
      ocupado=c()
      libre=c()
      total=length(tiempo)
      matrizresultados= matrix(0:0,1,12)
      matrizfila <- c()
      
      matrizresultados
      clientes
      
      
      length(tiemposl)
      clientes
      #Subrutinas llegada/salida
      for (i in 1:length(tiempo)){
        if (clientes < n){
          
          
          for (j in 1:length(tiemposl)){
            if (tiemposl[j]==tiempo[i]){
              evento<-"Llegada"
            }
          }
          for (j in 1:length(tiemposs)){
            if (tiemposs[j]==tiempo[i]){
              evento<-"Salida"
            }
          }
          if (evento =="Inicio" ){
            estatus
            cola
            tiemposfila
            A
            D
            clientes
            retrasototal
          }
          if (evento == "Llegada"){
            entradas=entradas+1
            A=tiemposl[entradas+1]
            if (estatus == 0 ){
              qt=cola
              bt=estatus
              estatus=1
              D= tiemposs[salidas+1]
              clientes <- clientes + 1
              
              ## Modificar aquí
              # if(ya_contado == 0){
              #   clientes= clientes + 1
              # }
              
            }
            else if (estatus==1) {
              qt=cola
              bt=estatus
              cola=cola+1
              tiemposfila[cola]=tiempo[i]
            }
          }
          if (evento =="Salida"){
            salidas=salidas+1
            if( cola==0) {
              tiemposfila=c()
              qt=cola
              bt=estatus
              estatus=0
              D=Inf
            }
            else {
              qt=cola
              bt=estatus
              cola=cola-1
              retraso=c(retraso,tiempo[i]-tiemposfila[1])
              retrasototal= retrasototal+tiempo[i]-tiemposfila[1]
              if (length(cola)==0){
                tiemposfila=c()
              } else {
                for (k in 1:length(cola)) {
                  tiemposfila[k]=tiemposfila[k+1]
                  tiemposfila[k+1]=tiemposfila[k+2]
                  tiemposfila[k+2]=tiemposfila[k+3]
                  tiemposfila[k+3]=tiemposfila[k+4]
                }}
              clientes=clientes+1
              D=tiemposs[salidas+1]
            }
          }
          if (is.na(A)=="TRUE"){
            A="NH+LL"
          }
          if (is.na(D)=="TRUE"){
            D="NH+S"
          }
          
          
          ultimoevento<-tiempo[i]
          if (i ==1){
            eventoanterior <- 0
          } else {
            eventoanterior<-tiempo[i-1]
          }
          
          eventoanterior
          
          hora<-tiempo[i]
          areaqt <- areaqt+qt*(ultimoevento-eventoanterior)
          areabt <- areabt+bt*(ultimoevento-eventoanterior)
          
          
          hora 
          areaqt
          
          ###################################################### 
          #Matriz resultados
          
          
          
          if (length(tiemposfila)==0){
            matrizfilavacio=matrix(c(0,0,0,0),1,4)
            matrizfila=rbind(matrizfila,matrizfilavacio)
          } else if (length(tiemposfila)==1){
            matrizfilavacio=matrix(c(tiemposfila,0,0,0),1,4)
            matrizfila=rbind(matrizfila,matrizfilavacio)
          } else if (length(tiemposfila)==2){
            matrizfilavacio=matrix(c(tiemposfila,0,0),1,4)
            matrizfila=rbind(matrizfila,matrizfilavacio)
          } else if (length(tiemposfila)==3){
            matrizfilavacio=matrix(c(tiemposfila,0),1,4)
            matrizfila=rbind(matrizfila,matrizfilavacio)
          } else if (length(tiemposfila)>=4){
            matrizfilavacio=matrix(tiemposfila,1,4)
            matrizfila=rbind(matrizfila,matrizfilavacio)
          }
          
          matrizfila 
          matrizfilavacio
          
          cola
          matrizfila
          matriz=matrix(c(evento,tiempo[i],estatus,cola,ultimoevento,hora,A,D,clientes,retrasototal,areaqt,areabt),1,12)
          matrizresultados=rbind(matrizresultados,matriz)
          #matrizresultados=cbind(matrizresultados,matrizfila)
          
          
          
          if (estatus==1){
            ocupado=c(ocupado,tiempo[i])
          } else {
            libre=c(libre,tiempo[i])
          }
          
          if (cola>limite_cola){
            showModal(modalDialog(
              title = "Error",
              paste0("La cola ha sobrepasado su límite."),
              easyClose = TRUE,
              footer = NULL))
            break
          }
          
          
          if (A==D){
            showModal(modalDialog(
              title = "Error",
              paste0("Un evento de llegada y uno de salida coinciden. Verifique sus datos."),
              easyClose = TRUE,
              footer = NULL))
            break
          }
          
          
        }
        print(paste("Se atendio",i))
      }
      
      ##########################################################################
      ##########################################################################
      # Añadir fila hasta el principio de #matrizfila con 0,0,0,0
      ##########################################################################
      ##########################################################################
      
      temp <- c(0,0,0,0)
      matrizfila <- rbind(temp, matrizfila)
      matrizresultados=cbind(matrizresultados,matrizfila)
      
      matrizresultados[is.na(matrizresultados)] <- 0
      matrizresultados
    
 
      tiempos_atencion <- c()
  
      for(j in 3:dim(matrizresultados)[1]){
        if(matrizresultados[j,1] == "Llegada"){
          if(matrizresultados[j,4] == 0){
            tiempos_atencion[j-2] <- matrizresultados[j,2]
          }
            
          } else if (matrizresultados[j,1] == "Salida"){
            if(matrizresultados[j,3] == 1){
              tiempos_atencion[j-2] <- matrizresultados[j,2]
          }
        }
      }
      
      # vector_temp <- c
      # counter1 <- 1
      # vector_temp <- tiempos_atencion[which(tiempos_atencion<=rev(as.numeric(matrizresultados[,2]))[1])]
      # vector_temp <- as.numeric(vector_temp)
      # vector_temp
      # 
      # for (i in 1:(length(vector_temp)-1)){
      #   index <- which(as.numeric(matrizresultados[,2])>=vector_temp[i])[1]
      #   matrizresultados[index:dim(matrizresultados)[1],9] <- counter1
      #   counter1 <- counter1 + 1
      #   print(matrizresultados)
      # }
      
      
      ########################################
      ########################################
      ########################################
      # Si algo falla, comentar esto
      
      
      # eliminar_filas <- which(as.numeric(matrizresultados[,9])==n)[-1]
      # matrizresultados <- matrizresultados[-eliminar_filas,]
      
      
      ########################################
      ########################################
      ########################################
      
      
      
      #Indices finales
      #Promedio de retraso
      promedioretraso=sum(retraso)/as.numeric(n)
      #Promedio de clientes en la cola al observar n clientes
      qgorro= areaqt/hora
      #Proporci?n de tiempo del servidor ocupado
      un=areabt/hora
      f=1
      resta=0
      
      
      # Checar esto 
      # tiempo_total <- tiempo[length(tiempo)]
      # 
      # matriz_ocupado <- rbind(ocupado,rep("Ocupado",length(ocupado)))
      # matriz_libre <- rbind(libre,rep("Libre",length(libre)))
      # matriz_servidor <- cbind(matriz_libre,matriz_ocupado)
      #       
      # matriz_servidor <- matriz_servidor[,order(matriz_servidor[1,], decreasing = F)]
      # 
      # tiempo_libre <- 0
      # tiempo_ocupado <- 0
      # 
      # for(i in 1:((dim(matriz_servidor)[2])-1)){
      #   # Si en el tiempo i el servidor está libre
      #   if(matriz_servidor[2,i] == "Libre"){
      #     # Si en el siguiente evento está libre, sumar
      #     # if(matriz_servidor[2,i+1] == "Libre"){
      #     #   tiempo_libre <- tiempo_libre + (as.numeric(matriz_servidor[1,i+1]) - as.numeric(matriz_servidor[1,i]))
      #     # } else if (matriz_servidor[2,i+1] == "Ocupado"){
      #     #   tiempo_ocupado <- tiempo_ocupado + (as.numeric(matriz_servidor[1,i+1]) - as.numeric(matriz_servidor[1,i]))
      #     # }
      #     tiempo_libre <- tiempo_libre + (as.numeric(matriz_servidor[1,i+1]) - as.numeric(matriz_servidor[1,i]))
      #   } else if (matriz_servidor[2,i] == "Ocupado"){
      #     tiempo_ocupado <- tiempo_ocupado + (as.numeric(matriz_servidor[1,i+1]) - as.numeric(matriz_servidor[1,i]))
      #   }
      # }
      # 
      # un <- tiempo_ocupado/tiempo_total
      # print(paste("U(n):",un))
      
      # for (i in 1:length(tiempo)){
      #   libre=c(libre,0)
      #   ocupado=c(ocupado,0)
      #   for (j in 1:length(tiempo)){
      #     if (libre[i] != resta){ 
      #       if (libre[i]<ocupado[j]){
      #         if (libre[i]!=0){
      #           resta= libre[i]
      #           un= un + libre[i]-ocupado[f]
      #           f=j
      #         }
      #       } 
      #       if (libre[i]== hora){
      #         un=un+libre[i]-ocupado[f]
      #       } 
      #       if (ocupado[j]==hora){
      #         un=un+ocupado[j]-ocupado[f]
      #       }
      #     }
      #   }
      # }
      
      matrizresultados <- matrizresultados[-1,]
      colnames(matrizresultados) <- c('Evento','Tiempo','Estatus','Cola','Último Evento','Hora','A','D','Clientes','Retraso','Área Q(t)','Área B(t)','1ro','2do','3ro','4to')
      
      
      
      
      
      
      print(matrizresultados)
      
      # Export CSV file and create PDF 
      directory <- getwd()
      write.csv(matrizresultados, paste0(directory,"/colas.csv"))
      
      shinyjs::show(id = "download")
      
      
      proporcion_cola <- rbind(as.numeric(matrizresultados[,2]),as.numeric(matrizresultados[,4]))

      suma_cola <- seq(0,0,length.out=length(unique(as.numeric(matrizresultados[,4])))+1)
      for(k in unique(as.numeric(matrizresultados[,4]))){
        temporal <- which(proporcion_cola[2,]==k)
        for(j in temporal){
          if(j==dim(proporcion_cola)[2]){
            
          } else {
            suma_cola[k+1] <- suma_cola[k+1] + proporcion_cola[1,j+1] - proporcion_cola[1,j] 
          }
        }
      }
      
      if(suma_cola[length(suma_cola)] == 0){
        suma_cola <- rev(rev(suma_cola)[-1])
      } 
      
      
      proporcion_cola_i <- suma_cola/hora
      
      proportion_table <- matrix(c(round(unique(as.numeric(matrizresultados[,4])),0),paste(round(proporcion_cola_i,4)*100,"%")), nrow = length(proporcion_cola_i)) 
      colnames(proportion_table) <- c("Tamaño de cola","Proporción")
      
      
      
      print(proporcion_cola_i)
      print(hora)
      
      #var1 <- paste(unique(as.numeric(matrizresultados[,4])),proporcion_cola_i,sep = ":")
      
      var1 <- 0
      var2 <- round(promedioretraso,2)
      var3 <- round(qgorro,2)
      var4 <- round(un,2)
      #output$p <- renderText({var1})
      #output$d <- renderText({var2})
      #output$q <- renderText({var3})
      #output$u <- renderText({var4})
      output$infobox_d <- renderValueBox({
        valueBox(
          var2, paste("Retraso promedio en la cola de los ", input$n2, " clientes:"), icon = icon("tired"),
          color = "teal"
        )
      })
      output$infobox_q <- renderValueBox({
        valueBox(
          var3, paste("Número promedio de clientes en la cola, al considerar ", input$n2 ," clientes:"), icon = icon("money"),
          color = "orange"
        )
      })
      output$infobox_u <- renderValueBox({
        valueBox(
          var4, "Proporción de tiempo durante la simulación que el servidor está ocupado:", icon = icon("money"),
          color = "orange"
        )
      })
      output$table_p <- renderTable({
        proportion_table
      })
      shinyjs::show(id = "table_p")
      
    }
    else{
      output$infobox_d <- renderValueBox({
        valueBox(
          0, "Retraso promedio en la cola de los n clientes:", icon = icon("angry"),
          color = "teal"
        )
      })
      output$infobox_q <- renderValueBox({
        valueBox(
          0, "Número promedio de clientes en la cola, al considerar n clientes:", icon = icon("users"),
          color = "orange"
        )
      })
      output$infobox_u <- renderValueBox({
        valueBox(
          0, "Proporción de tiempo durante la simulación que el servidor está ocupado:", icon = icon("tired"),
          color = "orange"
        )
      })
      shinyjs::hide(id = "table_p")
    }
    # shinyjs::show(id = "download")
  })
}


shinyApp(ui, server)