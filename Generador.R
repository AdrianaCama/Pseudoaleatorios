#install.packages("devtools")
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(ggfortify)
#library(gmp)
#library(devtools)
#install_github("nik01010/dashboardthemes")

# css_dir <- getwd()
# paste0(css_dir,"/")


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

ui <- dashboardPage(
  # App title ----
  dashboardHeader(title = HTML("GENERADOR DE <br/> NÚMEROS ALEATORIOS")),
  #skin = "green",
  
  dashboardSidebar(
    disable = TRUE,
  sidebarMenu(
  #     menuItem("Valores", tabName = "inventarios", icon = icon("truck-loading")),
  #     menuItem("Pruebas estadísticas", tabName = "colas", icon = icon("user-clock"))
  )
  ),
  
  # Sidebar panel for inputs ----
  dashboardBody(
    useShinyjs(),
    # shinyDashboardThemes(
    #   theme = "blue_gradient"
    # ),
    fluidRow(
      column(width = 6,
             box(
               title = "Valores",
               width = NULL,
               solidHeader = TRUE,
               #background = "navy",
               collapsible = TRUE,
               #status = "warning",
               radioButtons(inputId = "Checkbox",
                            label = "Números pseudoaleatorios",
                            choices = c("Generar con función de R", 
                                        "Generar con función programada"),
                            selected = "Generar con función programada"),
               # Input: Numerics para ingresar valores ----
               numericInput(inputId = "a",
                            label = "Multiplicador(a):",
                            min = 0,
                            value = 2^7 + 1
                            ),
               numericInput(inputId = "m",
                            label = "Módulo(m):",
                            min = 0,
                            value = 2^35
                            ),
               numericInput(inputId = "c",
                            label = "Incremento(c):",
                            min = 0,
                            value = 1
                            ),
               numericInput(inputId = "n",
                            label = "n:",
                            min = 4000,
                            value = 10000
                            ),
               numericInput(inputId = "seed",
                            label = "Semilla:",
                            min = 0,
                            value = 621
                            ),
               actionButton(inputId = "Button_go", label = "Generar")
              ),
             box(
               title = "Pruebas estadísticas",
               solidHeader = TRUE,
               br(),
               #background = "olive",
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
               selectInput(inputId = "atrasos",
                           label = "Número de atrasos",
                           choices = c(1, 2, 3, 4, 5),
                           selected = 1),
               numericInput(inputId = "intervalos",
                            label = "Número de intervalos:",
                            min = 1,
                            max = 1000,
                            value = 100),
               # numericInput(inputId = "intervals",
               #              label = "Número de intervalos:",
               #              min = 2,
               #              max = 100,
               #              value = 5
               # ),
               actionButton(inputId = "Button_evaluate", label = "Probar uniformidad y/o 
                            independencia")
               )
             ),
      column(width = 6,
             tabBox(
               title = tagList(shiny::icon("table"), "Datos"),
               #solidHeader = TRUE,
               id = "tabbox_generado",
               width = NULL,
               side = "right",
               tabPanel("Números generados",
                        DT::dataTableOutput(outputId = "random_table"),
                        downloadButton("download", label = "Descargar CSV")
                        ),
               tabPanel("Histograma",
                        plotOutput("hist_uniformidad")
                        )
               ),
             tabBox(
               title = tagList(shiny::icon("area-chart"), "Resultados"),
               id = "tabbox_resultados",
               side = "right",
               width = NULL,
               tabPanel("Región de rechazo",
                        valueBoxOutput("valuebox_rechazo", width = 100),
                        valueBoxOutput("valuebox_estadistico", width = 100),
                        plotOutput("hist_distribucion"),
                        textOutput("text_region")
                        ),
               tabPanel("Valor p",
                        valueBoxOutput("valuebox_pvalue", width = 100),
                        textOutput("text_pvalue")
                        ),
               tabPanel("Gráfica",
                        value = 3,
                        plotOutput("hist_intervalos"),
                        )
               )
             )
      )
    ),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom_white.css"))
)


server <- function(input, output) {
  shinyjs::hide(id = "download")
  shinyjs::hide(id = "hist_uniformidad")
  shinyjs::hide(id = "hist_distribucion")
  shinyjs::hide(id = "hist_intervalos")
  shinyjs::hide(id = "valuebox_rechazo")
  shinyjs::hide(id = "valuebox_estadistico")
  
  
  ###################################################
  ############ Deshabilitar opciones ################
  ###################################################
  observeEvent(input$Checkbox, {
    
    if(input$Checkbox == "Generar con función programada"){
      shinyjs::show(id = "a")
      shinyjs::show(id = "m")
      shinyjs::show(id = "c")
      shinyjs::show(id = "seed")
    }
    else if(input$Checkbox == "Generar con función de R"){
      shinyjs::hide(id = "a")
      shinyjs::hide(id = "m")
      shinyjs::hide(id = "c")
      shinyjs::hide(id = "seed")
    }
  })
  
  
  ### Generación de valores aleatorios #######################################################
  ############################################################################################
  observeEvent(input$Button_go, {
    # Cambiar (Esto solo es para probar)
    # numeros <<- c(0.045555, 0.065749, 0.092871, 0.149668, 0.190782, 0.224291, 0.260000, 0.321474, 
    #              0.332037, 0.392275, 0.404315, 0.431058, 0.468068, 0.495164, 0.569813, 0.631893, 
    #              0.652066, 0.785885, 0.830250, 0.846584)

    # Extraer valores
    a <- input$a 
    m <- input$m
    c <- input$c
    n <- input$n
    semilla <- input$seed
    
    # Checar que sean valores numéricos
    if(is.numeric(a) == FALSE || is.numeric(m) == FALSE || is.numeric(c) == FALSE || is.numeric(n) == FALSE || is.numeric(semilla) == FALSE || a < 0 || m < 0 || c < 0 || n <= 0 || semilla < 0){
      showModal(modalDialog(
        title = "Error",
        "Alguno de los valores introducidos es incorrecto, por favor verifique sus entradas."
      ))
      error <- 1
    } else if(a*seed+c > ){
      
    } else if(a >= m){
      showModal(modalDialog(
        title = "Error",
        "El valor de a debe ser menor al de m."
      ))
      error <- 1
    }else if(c >= m){
      showModal(modalDialog(
        title = "Error",
        "El valor de c debe ser menor al de m."
      ))
      error <- 1
    }else if(semilla >= m){
      showModal(modalDialog(
        title = "Error",
        "El valor de la semilla debe ser menor al de m."
      ))
      error <- 1
    } else {
      error <- 0
      if (input$Checkbox == "Generar con función programada"){
        numeros <- unif_generator(n = n, min = 0, max = 1, seed = semilla, m = m, a = a, c = c)
      } else if(input$Checkbox == "Generar con función de R"){
        numeros <- runif(n = n, 0, 1)
      } 
    }
    
    
    if(error == 0){
      output$random_table <- DT::renderDataTable({data.frame("Valores" = numeros)})
      output$hist_uniformidad <- renderPlot({
        ggplot(data.frame("Valores" = numeros)) + geom_histogram(aes(x=Valores,y=..density..), 
                                                                 alpha=0.7, 
                                                                 breaks=seq(0, 1, 0.025), 
                                                                 closed="left", 
                                                                 color="white", 
                                                                 fill="lightpink2") + 
          stat_function(fun = dunif, args = list(0,1), colour = "aquamarine3",size=1) +
          ylab("Densidad")
      })
      output$download <- downloadHandler(filename = "aleatorios.csv",
                                         content = function(file) {
                                           write.csv(numeros, file)
                                           # Cambiar línea anterior por línea siguiente si se prefiere sin índices
                                           #write.csv(numeros_dt, file, row.names = FALSE)
                                         })
      shinyjs::show(id = "download")
      shinyjs::show(id = "hist_uniformidad")
    }
  }
  )
  
  
  ### Pruebas de aleatoriedad y/o uniformidad #################################################
  #############################################################################################
  
  # Prueba 1:  Prueba de la Ji Cuadrada##############
  ###################################################
  ###################################################
  ChiSquaredTest <- function(numeros, alfa){
    # Obtenemos el número de intervalos y el número de observaciones, respectivamente
    ############################################################################################
    # PENDIENTE: Revisar si la k es establecida o el usuario la especifica.
    ############################################################################################
    k <- as.numeric(input$intervalos)
    n <- length(numeros)
    
    # f es un vector que contiene las f_j. f_j es el número de observaciones en la muestra que se 
    # encuentran en el j-ésimo intervalo.
    
    # Aquí, hacemos los intervalos, contamos cuántos elementos de la muestra se encuentran en cada
    # uno de los intervalos y los juntamos en el vector "f".
    f <- c()
    limits <- seq.int(from = 0, to = 1, length.out = k+1)
    for(i in 1:k){
      f[i] <- sum(numeros > limits[i] & numeros < limits[i+1])
    }
    
    # Ahora, usamos la fórmula para calcular el estadístico de prueba. "temp" es una variable
    # temporal (sólo se usa para guardar un valor y usarlo después).
    temp <- 0
    
    for(i in 1:k){
      temp <- temp + (f[i] - (n/k))^(2)
    }
    
    # Así, el estadístico queda:
    estadistico <- (k/n)*temp
    Y <- estadistico
    # Ahora, necesitamos aplicar la prueba de hipótesis. Las hipótesis son:
    # H_0: La muestra proviene de una distribución uniforme.
    # H_a: La muestra no proviene de una distribución uniforme.
    
    
    ##############################################################################################
    # Prueba de hipótesis con aproximación
    
    estadistico
    z <- qnorm(p = 1-alfa)
    
    cuantil <- (k-1)*(1-(2/(9*(k-1)))+z*sqrt(2/(9*(k-1))))^(3)
    quantile_ChiSquared <- cuantil
    
    print("Cuantil")
    print(cuantil)
    
    print("Estadístico")
    print(estadistico)
    
    print("Cuantil")
    print(cuantil)
    
    
    p_value <- pchisq(q=estadistico,df=k-1,lower.tail = FALSE)
    # Rechazo por región
    if(estadistico >= cuantil){
      rechazo_por_region <- 1
      # print("Se rechaza la hipótesis nula. Es decir, existe suficiente evidencia para afirmar que
      #     la muestra no proviene de una distribución uniforme.")
    } else{
      rechazo_por_region <- 0
      # print("No se rechaza la hipótesis nula. Es decir, no existe suficiente evidencia para afirmar que
      #     la muestra no proviene de una distribución uniforme.")
    }
    
    # Rechazo por p-value
    if(p_value <= alfa){
      rechazo_por_pvalue <- 1
      # print("Se rechaza la hipótesis nula. Es decir, existe suficiente evidencia para afirmar que
      #     la muestra no proviene de una distribución uniforme.")
    } else{
      rechazo_por_pvalue <- 0
      # print("No se rechaza la hipótesis nula. Es decir, no existe suficiente evidencia para afirmar que
      #     la muestra no proviene de una distribución uniforme.")
    }
    
    return(list(Y, quantile_ChiSquared, p_value, rechazo_por_region, rechazo_por_pvalue, k))
  }
  
  
  # Prueba 2: "Prueba Serial"########################
  ###################################################
  ###################################################
  
  
  
  SerialTest <- function(numeros, alfa){
    library(ggplot2)
    
    # Establecer el tamaño de k. Mandar aviso al usuario si establece una k muy grande.
    k <- as.numeric(input$intervalos)
    
    # Checar si el número de datos a verificar independencia es par
    if(length(numeros)%%2 == 1){
      # Si no es par, generar una observación más a num y agregarla
      # PENDIENTE: Checar si está bien quitar una observación.
      num_2 <- numeros[-1]
    } else {
      num_2 <- numeros
    }
    
    # "Revolver" los números
    num_2 <- sample(num_2, size = length(num_2), replace = FALSE)
    U1 <- num_2[1:(length(num_2)/2)]
    U2 <- num_2[((length(num_2)/2)+1):length(num_2)]
    
    limits <- seq(from = 0, to = 1, length.out = k + 1)
    # Contar el número de observaciones que caen en la cuadrícula
    count <- matrix(0, k, k)
    for(i in 1:k){
      for(j in 1:k){
        temp <- matrix(0, length(U1), 2)
        temp <- cbind(U1,U2)
        temp <- rbind(temp[rev(limits)[i] >= temp[,2],])
        temp <- rbind(temp[temp[,2] > rev(limits)[i+1],])
        temp <- rbind(temp[limits[j] < temp[,1],])
        
        temp
        
        if(is.vector(temp)==TRUE){
          if(temp[1] <= limits[j+1]){
            count[i,j] <- 1
          } else {
            count[i,j] <- 0
          }
        } else {
          temp <- temp[temp[,1] <= limits[j+1],]
          count[i,j] <- length(temp)/2
        }
      }
    }
    # El número de observaciones en cada cuadrante se encuentra en la matriz "count".
    count
    # Calcular el número esperado de observaciones en cada cuadrante de la cuadrícula
    temp <- c(U1,U2)
    expected <- matrix((length(temp)/2)/(k^2), k, k)

    #Cálculo del estadístico
    estadistico <- ((k^(2))/(length(temp)/2))*sum((count-expected)^2)
    Y <- estadistico

    # Grados de libertad
    df <- ((k^(2))-1)
    
    # Cálculo del cuantil
    cuantil <- qchisq(p=1-alfa,df=df,lower.tail = TRUE)
    z <- qnorm(p = 1-alfa)
    aproximacion <- ((k^2)-1)*(1-(2/(9*((k^2)-1)))+z*sqrt(2/(9*((k^2)-1))))^(3)
    
    quantile_SerialTest <- cuantil
    
    # Cálculo del valor p
    p_value <- pchisq(q=estadistico,df=df,lower.tail = FALSE)
    # p_value <- pchisq(q=estadistico,df=k-1,lower.tail = FALSE)
    
    # Rechazo por región
    if(k > 100){
      cuantil <- aproximacion
    }
    
    if(estadistico >= cuantil){
      rechazo_por_region <- 1
      # print("Se rechaza la hipótesis nula. Es decir, existe suficiente evidencia para afirmar que
      #     la muestra no proviene de una distribución uniforme.")
    } else{
      rechazo_por_region <- 0
      # print("No se rechaza la hipótesis nula. Es decir, no existe suficiente evidencia para afirmar que
      #     la muestra no proviene de una distribución uniforme.")
    }
    
    # Rechazo por p-value
    if(p_value <= alfa){
      rechazo_por_pvalue <- 1
      # print("Se rechaza la hipótesis nula. Es decir, existe suficiente evidencia para afirmar que
      #     la muestra no proviene de una distribución uniforme.")
    } else{
      rechazo_por_pvalue <- 0
      # print("No se rechaza la hipótesis nula. Es decir, no existe suficiente evidencia para afirmar que
      #     la muestra no proviene de una distribución uniforme.")
    }
    
    # Graficar los datos
    data <- data.frame(
      U1 <- U1,
      U2 <- U2
    )
    
    # Generar los límites de los intervalos para las gráficas
    limits <- seq(from = 0, to = 1, length.out = k + 1)
    
    graph <- ggplot(data, aes(U1, U2)) + geom_point() + scale_x_continuous(limits = c(0,1), expand = c(0, 0)) + scale_y_continuous(limits = c(0,1), expand = c(0, 0)) + geom_hline(yintercept=limits) + geom_vline(xintercept=limits)
    plot(graph)
    
    return(list(Y, quantile_SerialTest, p_value, rechazo_por_region, rechazo_por_pvalue, k, U1, U2, limits))
  }
  
  
  
  
  # Prueba 3: Prueba de Kolmogorov-Smirnov  #########
  ###################################################
  ###################################################
  KS <- function(numeros, alpha) {
    tabla_KS <- data.frame("n" = c(1:40,">40"), "0.2" = c(0.900, 0.684, 0.565, 0.493, 0.447, 0.410, 
                                                          0.381, 0.358, 0.339, 0.323, 0.308, 0.296, 
                                                          0.285, 0.275, 0.266, 0.258, 0.250, 0.244, 
                                                          0.237, 0.232, 0.226, 0.221, 0.216, 0.212,
                                                          0.208, 0.204, 0.200, 0.197, 0.193, 0.190, 
                                                          0.187, 0.184, 0.182, 0.179, 0.177, 0.174, 
                                                          0.172, 0.170, 0.168, 0.165, "1.07/raiz(n)"), 
                           "0.1" = c(0.950, 0.776, 0.636, 0.565, 0.509, 0.468, 0.436, 0.410, 0.387, 
                                     0.369, 0.352, 0.338, 0.325, 0.314, 0.304, 0.295, 0.286, 0.279, 
                                     0.271, 0.265, 0.259, 0.253, 0.247, 0.242, 0.238, 0.233, 0.229, 
                                     0.225, 0.221, 0.218, 0.214, 0.211, 0.208, 0.205, 0.202, 0.199, 
                                     0.196, 0.194, 0.191, 0.189, "1.22/raiz(n)"),
                           "0.05" = c(0.975, 0.842, 0.780, 0.624, 0.563, 0.519, 0.483, 0.454, 0.430, 
                                      0.409, 0.391, 0.375, 0.361, 0.349, 0.338, 0.327, 0.318, 0.309, 
                                      0.301, 0.294, 0.287, 0.281, 0.275, 0.269, 0.264, 0.259, 0.254, 
                                      0.250, 0.246, 0.242, 0.238, 0.234, 0.231, 0.227, 0.224, 0.221, 
                                      0.218, 0.215, 0.213, 0.210, "1.36/raiz(n)"),
                           "0.02" = c(0.990, 0.900, 0.785, 0.698, 0.627, 0.577, 0.538, 0.507, 0.480, 
                                      0.457, 0.437, 0.419, 0.404, 0.390, 0.377, 0.366, 0.355, 0.346, 
                                      0.337, 0.329, 0.321, 0.314, 0.307, 0.301, 0.295, 0.290, 0.284, 
                                      0.279, 0.275, 0.270, 0.266, 0.262, 0.258, 0.254, 0.251, 0.247, 
                                      0.244, 0.241, 0.238, 0.235, "1.52/raiz(n)"),
                           "0.01" = c(0.995, 0.929, 0.829, 0.734, 0.669, 0.617, 0.576, 0.542, 0.513, 
                                      0.489, 0.468, 0.449, 0.432, 0.418, 0.404, 0.392, 0.381, 0.371, 
                                      0.361, 0.352, 0.344, 0.337, 0.330, 0.323, 0.317, 0.311, 0.305, 
                                      0.300, 0.295, 0.290, 0.285, 0.281, 0.277, 0.273, 0.269, 0.265, 
                                      0.262, 0.258, 0.255, 0.252, "1.63/raiz(n)"),
                           stringsAsFactors = FALSE
                           )
    
    numeros_ordenados <- sort(numeros, decreasing=FALSE)
    n <- length(numeros_ordenados)
    empirica <- seq(1, n, 1) / n
    
    D_n <- max(abs(numeros_ordenados - empirica))
    
    if(alpha==0.2){
      columna <- 2
    } else if(alpha==0.1){
      columna <- 3
    } else if(alpha==0.05){
      columna <- 4
    } else if(alpha==0.025){
      columna <- 5
    } else if(alpha==0.01){
      columna <- 6
    }
    
    if(n>40){
      if(alpha==0.2){
        tabla_KS[41, 2] <- 1.07/sqrt(n)
      }
      if(alpha==0.1){
        tabla_KS[41, 3] <- 1.22/sqrt(n)
      }
      if(alpha==0.05){
        tabla_KS[41, 4] <- 1.36/sqrt(n)
      }
      if(alpha==0.025){
        tabla_KS[41, 5] <- 1.52/sqrt(n)
      }
      if(alpha==0.01){
        tabla_KS[41, 6] <- 1.63/sqrt(n)
      }
    }
    
    if(n<=40){
      d_alpha <- tabla_KS[n, columna]
    } else{
      d_alpha <- tabla_KS[41, columna]
    }
    
    i <- 2
    fin <- 0
    opcion <- 0
    while(fin == 0){
      if(n<= 40){
        if (tabla_KS[n, i] > D_n){
          fin <- 1
          if(i == 2){
            p_value <- "VP>0.20"
          }
          if(i == 3){
            p_value <- "0.10<VP<0.20"
          }
          if(i == 4){
            p_value <- "0.05<VP<0.10"
          }
          if(i == 5){
            p_value <- "0.02<VP<0.05"
          }
          if(i == 6){
            p_value <- "0.01<VP<0.02"
          }
          opcion <- 1
        } else if (tabla_KS[n, i] == D_n){
          fin <- 1
          if(i == 2){
            p_value <- 0.2
          }
          if(i == 3){
            p_value <- 0.1
          }
          if(i == 4){
            p_value <- 0.05
          }
          if(i == 5){
            p_value <- 0.02
          }
          if(i == 6){
            p_value <- 0.01
          }
          opcion <- 2
        }
        i <- i + 1
        if(i == 7){
          fin <- 1
          p_value <- "VP<0.01"
          opcion <- 1
          }
        }else{
          if (tabla_KS[41, i] > D_n){
            fin <- 1
            if(i == 2){
              p_value <- "VP>0.20"
            }
            if(i == 3){
              p_value <- "0.10<VP<0.20"
            }
            if(i == 4){
              p_value <- "0.05<VP<0.10"
            }
            if(i == 5){
              p_value <- "0.02<VP<0.05"
            }
            if(i == 6){
              p_value <- "0.01<VP<0.02"
            }
            opcion <- 1
          } else if (tabla_KS[41, i] == D_n){
            fin <- 1
            if(i == 2){
              p_value <- 0.2
            }
            if(i == 3){
              p_value <- 0.1
            }
            if(i == 4){
              p_value <- 0.05
            }
            if(i == 5){
              p_value <- 0.02
            }
            if(i == 6){
              p_value <- 0.01
            }
            opcion <- 2
          }
          i <- i + 1
          if(i == 7){
            fin <- 1
            p_value <- "VP<0.01"
            opcion <- 1
          }
        }
    } 
    D_n <- as.numeric(D_n)
    d_alpha <- as.numeric(d_alpha)
    
    if(D_n >= d_alpha){
      rechazo_por_region <- 1
    } else{
      rechazo_por_region <- 0
    }
    
    if(opcion == 1){
      if((substr(p_value, nchar(p_value)-3, nchar(p_value)) <= alpha && i != 2) || i == 7){
        rechazo_por_pvalue <- 1
      } else{
        rechazo_por_pvalue <- 0
      }
    }
    if(opcion == 2){
      if(p_value <= alpha){
        rechazo_por_pvalue <- 1
      } else{
        rechazo_por_pvalue <- 0
      }
    }
    
    
    return(list(D_n, d_alpha, p_value, rechazo_por_region, rechazo_por_pvalue))
  }

  # Prueba 4: Cramer-von Mises ######################
  ###################################################
  ###################################################
  CvM <- function(numeros, alpha){
    tabla_CVM <- data.frame("n" = c(2:10,20,50,200,1000,"Valores de n grandes"),
                            "0.99" = c(0.55052,0.63976,0.67017,0.68352,0.69443,0.70154,0.70912,0.71283,0.71582,
                                       0.72948,0.73784,0.74205,0.74318,0.74346),
                            "0.975" = c(0.48897,0.53316,0.54200,0.55056,0.55572,0.55935,0.56327,0.56513,0.56663,
                                        0.57352,0.57775,0.57990,0.58047,0.58061),
                            "0.95"  = c(0.42482,0.43938,0.44199,0.44697,0.44911,0.45100,0.45285,0.45377,0.45450,
                                        0.45788,0.45996,0.46101,0.46129,0.46136),
                            "0.90"  = c(0.34346,0.33786,0.34183,0.34238,0.34352,0.34397,0.34462,0.34491,0.34514,
                                        0.34621,0.34686,0.34719,0.34728,0.34730),
                            "0.85"  = c(0.28853,0.27963,0.28337,0.28305,0.28331,0.28345,0.28358,0.28364,0.28368,
                                        0.28387,0.28398,0.28404,0.28406,0.28406),
                            "0.80"  = c(0.24743,0.24169,0.24260,0.24236,0.24198,0.24197,0.24187,0.24180,0.24175,
                                        0.24150,0.24134,0.24126,0.24124,0.24124),
                            "0.75"  = c(0.21521,0.21339,0.21173,0.21165,0.21110,0.21087,0.21066,0.21052,0.21041,
                                        0.20990,0.20960,0.20944,0.20940,0.20939),
                            "0.50"  = c(0.12659,0.12542,0.12405,0.12252,0.12200,0.12158,0.12113,0.12088,0.12069,
                                        0.11979,0.11924,0.11897,0.11890,0.11888),
                            "0.25"  = c(0.08145,0.07683,0.07494,0.07427,0.07352,0.07297,0.07254,0.07228,0.07208,
                                        0.07117,0.07062,0.07035,0.07027,0.07026),
                            "0.20"  = c(0.07351,0.06886,0.06681,0.06611,0.06548,0.06492,0.06448,0.06423,0.06403,
                                        0.06312,0.06258,0.06231,0.06224,0.06222),
                            "0.15"  = c(0.06554,0.06092,0.05895,0.05799,0.05747,0.05697,0.05650,0.05625,0.05605,
                                        0.05515,0.05462,0.05435,0.05428,0.05426),
                            "0.10"  = c(0.05758,0.05287,0.05093,0.04970,0.04910,0.04869,0.04823,0.04798,0.04778,
                                        0.04689,0.04636,0.04610,0.04603,0.04601),
                            stringsAsFactors = FALSE)
    numeros_ordenados <- sort(numeros, decreasing=FALSE)
    N <- length(numeros_ordenados)
    empirica <- (2*seq(1, N, 1) - 1)/ (2*N)
    
    Y <- (1 / (12*N)) + sum((numeros_ordenados - empirica)^2)
    
    if(alpha==0.2){
      quantile_CvM <- tabla_CVM[14,7]
    } else if(alpha==0.1){
      quantile_CvM <- tabla_CVM[14,5]
    } else if(alpha==0.05){
      quantile_CvM <- tabla_CVM[14,4]
    } else if(alpha==0.025){
      quantile_CvM <- tabla_CVM[14,3]
    } else if(alpha==0.01){
      quantile_CvM <- tabla_CVM[14,2]
    }
    
    
    i <- 13
    fin <- 0
    opcion <- 0
    while(fin == 0){
        if (tabla_CVM[14, i]>Y){
          fin <- 1
          if(i == 13){
            p_value <- "VP>0.90"
          }
          if(i == 12){
            p_value <- "0.85<VP<0.90"
          }
          if(i == 11){
            p_value <- "0.80<VP<0.85"
          }
          if(i == 10){
            p_value <- "0.75<VP<0.80"
          }
          if(i == 9){
            p_value <- "0.50<VP<0.75"
          }
          if(i == 8){
            p_value <- "0.25<VP<0.50"
          }
          if(i == 7){
            p_value <- "0.20<VP<0.25"
          }
          if(i == 6){
            p_value <- "0.15<VP<0.20"
          }
          if(i == 5){
            p_value <- "0.10<VP<0.15"
          }
          if(i == 4){
            p_value <- "0.05<VP<0.10"
          }
          if(i == 3){
            p_value <- ".025<VP<0.05"
          }
          if(i == 2){
            p_value <- "0.01<VP<.025"
          }
          opcion <- 1
        }
         else if (tabla_CVM[14, i]==Y){
          fin <- 1
          if(i == 13){
            p_value <- 0.90
          }
          if(i == 12){
            p_value <- 0.85
          }
          if(i == 11){
            p_value <- 0.80
          }
          if(i == 10){
            p_value <- 0.75
          }
          if(i == 9){
            p_value <- 0.50
          }
          if(i == 8){
            p_value <- 0.25
          }
          if(i == 7){
            p_value <- 0.20
          }
          if(i == 6){
            p_value <- 0.15
          }
          if(i == 5){
            p_value <- 0.10
          }
          if(i == 4){
            p_value <- 0.05
          }
          if(i == 3){
            p_value <- 0.025
          }
          if(i == 2){
            p_value <- 0.01
          }
          opcion <- 2
        }
        i <- i - 1
        if(i == 1){
          fin <- 1
          p_value <- "VP<0.01"
          opcion <- 1
        }
    } 
  
    

    if(Y < quantile_CvM){
      rechazo_por_region <- 0
      } else{
        rechazo_por_region <- 1
      }
    
    
    if(opcion == 1){
      if((substr(p_value, nchar(p_value)-3, nchar(p_value)) <= alpha && i != 13) || i == 1){
        rechazo_por_pvalue <- 1
      } else{
        rechazo_por_pvalue <- 0
      }
    }
    if(opcion == 2){
      if(p_value <= alpha){
        rechazo_por_pvalue <- 1
      } else{
        rechazo_por_pvalue <- 0
      }
    }
    
    return(list(Y, quantile_CvM, p_value, rechazo_por_region, rechazo_por_pvalue))
  }
  
  
  # Prueba 5: Prueba de las corridas ################
  ###################################################
  ###################################################
  
  # secuencia <- c(0.22317383,0.82220999,0.11944974,0.23844417,0.00416896,0.04593599,0.88826024,0.73882373,0.2347258,0.81838537,0.28577309,0.06599242,0.48352499,0.41014991,0.6706971,0.88065793,0.854395,0.39438387,0.96948658,0.16150143,0.82201839,0.43925927,0.88317153,0.32954575,0.55911048,0.70385292,0.91883758,0.89169391,
  #               0.9144542,0.27766943)
  PC<- function(secuencia, alpha){
    a <- rbind(c(4529.4, 9044.9, 13568, 18091, 22615, 27892),
               c(9044.9, 18097, 27139, 36187, 45234, 55789),
               c(13568, 27139, 40721, 54281, 67852, 83685),
               c(18091, 36187, 54281, 72414, 90470, 111580),
               c(22615, 45234, 67852, 90470, 113262, 139476),
               c(27892, 55789, 83685, 111580, 139476, 172860))
    b <- c(1/6, 5/24, 11/120, 19/720, 29/5040, 1/840)
    
    n <- length(secuencia)
    
    
    largo <- 1
    r <- c(0, 0, 0, 0, 0, 0)
    for(i in 2:n){
      if(secuencia[i]>secuencia[i-1]){
        largo <- largo + 1
      } else{
        if(largo<=6){
          r[largo] <- r[largo] + 1
        }
        else{
          r[6] <- r[6] + 1
        }
        largo <- 1
      }
    }
    if (largo!=1){
      if(largo<=6){
        r[largo] <- r[largo] + 1
      }
      else{
        r[6] <- r[6] + 1
      }
    } else{
      r[1] <- r[1] + 1
    }
    
    
    suma <- 0
    for(i in 1:6){
      for(j in 1:6){
        suma <- suma + a[i,j] * (r[i] - n*b[i]) * (r[j] - n*b[j])
      }
    }
    R <- (1/n) * suma
    
    quantile_corridas <- qchisq(1-alpha, 6)
    p_value <- pchisq(R, 6, lower.tail = FALSE)
    
    if(R >= quantile_corridas){
      rechazo_por_region <- 1
    } else{
      rechazo_por_region <- 0
    }
    
    
    if(p_value <= alpha){
      rechazo_por_pvalue <- 1
    } else{
      rechazo_por_pvalue <- 0
    }
    
    return(list(R, quantile_corridas, p_value, rechazo_por_region, rechazo_por_pvalue))
  }
  
  # Prueba  6: Correlación de atrasos  ##############
  ###################################################
  ###################################################
  CorrelationTest <- function(numeros,alfa){
    # Obtener parámetros
    n <- length(numeros)
    
    # n debe ser al menos 2*atrasos + 1
    
    # Número de atrasos
    atrasos <- as.numeric(input$atrasos)
    h <- floor((n-1)/atrasos)-1
    
    # Formar vectores para el estimador considerando el número de atrasos
    temp <- numeros[seq(from = 1, to = n, by = atrasos)]
    
    lag <- temp[-1]
    normal <- temp[-length(temp)]

    lag <- lag[1:h]
    normal <- normal[1:h]
    
    p_hat <-  ((12/(h+1)) * (sum(lag*normal))) - 3
    variance <- (13*h+7)/((h+1)^(2))
    
    A <- p_hat/sqrt(variance)
    Y <- A
    z <- qnorm(1-(alfa/2))
    quantile_CorrelationTest <- z
    
    if(Y >= 0){
      p_value <- 2*pnorm(Y, lower.tail = FALSE)
    }else{
      p_value <- 2*(1 - pnorm(Y, lower.tail = FALSE))
    }

    
    
    if(abs(A) >= quantile_CorrelationTest){
      rechazo_por_region <- 1
    } else{
      rechazo_por_region <- 0
    }
    
    
    if(p_value <= alfa){
      rechazo_por_pvalue <- 1
    } else{
      rechazo_por_pvalue <- 0
    }
    
    return(list(Y, quantile_CorrelationTest, p_value, rechazo_por_region, rechazo_por_pvalue, atrasos))
    
  }
  
  # Deshabilitar opciones de acuerdo a lo que el usuario escoja
  observeEvent(input$pruebas, {
    if(input$pruebas == "Prueba de la Ji Cuadrada" || input$pruebas == "Prueba Serial"){
      shinyjs::hide(id = "atrasos")
      shinyjs::show(id = "intervalos")
    } else if(input$pruebas == "Correlación de atrasos"){
      shinyjs::hide(id = "intervalos")
      shinyjs::show(id = "atrasos")
    } else {
      shinyjs::hide(id = "intervalos")
      shinyjs::hide(id = "atrasos")
    }
  }
  )
  
  # Acciones al presionar el botón "Evaluar"
  observeEvent(input$Button_evaluate, {
    #Codigo de las pruebas 
    
    if(input$pruebas=="Prueba de Cramer-von Mises"){
      prueba <- CvM(numeros, as.numeric(input$alpha))
      estadistico <- round(as.numeric(prueba[1]), 5)
      cuantil <- round(as.numeric(prueba[2]), 5)
      pvalue <- toString(prueba[3])
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
      if(length(numeros) < 1000){
        showModal(modalDialog(
          title = "Advertencia",
          paste0("El número de valores generados es menor a 1000. Es recomendable que para que la Prueba de 
                 Cramer-von Mises sea eficiente este valor sea mayor a 1000."),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    } else if(input$pruebas=="Prueba de la Ji Cuadrada"){
      prueba <- ChiSquaredTest(numeros, as.numeric(input$alpha))
      estadistico <- as.numeric(prueba[1])
      cuantil <- as.numeric(prueba[2])
      pvalue <- round(as.numeric(prueba[3]), 2)
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
      intervalos <- as.numeric(prueba[6])
    } else if(input$pruebas=="Prueba Serial"){
      prueba <- SerialTest(numeros, as.numeric(input$alpha))
      estadistico <- as.numeric(prueba[1])
      cuantil <- as.numeric(prueba[2])
      pvalue <- round(as.numeric(prueba[3]), 2)
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
      intervalos <- as.numeric(prueba[6])
      U1_graph <- as.numeric(unlist(prueba[7]))
      U2_graph <- as.numeric(unlist(prueba[8]))
      limits <- as.numeric(unlist(prueba[9]))
    } else if(input$pruebas=="Prueba de Kolmogorov-Smirnov"){
      prueba <- KS(numeros, as.numeric(input$alpha))
      estadistico <- round(as.numeric(prueba[1]), 5)
      cuantil <- round(as.numeric(prueba[2]), 5)
      pvalue <- toString(prueba[3])
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
    } else if(input$pruebas=="Prueba de las corridas"){
      prueba <- PC(numeros, as.numeric(input$alpha))
      estadistico <- as.numeric(prueba[1])
      cuantil <- as.numeric(prueba[2])
      pvalue <- round(as.numeric(prueba[3]), 2)
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
      if(length(numeros) < 4000){
        showModal(modalDialog(
          title = "Advertencia",
          paste0("El número de valores generados es menor a 4000. Es recomendable que para que la Prueba de 
                 las corridas sea eficiente este valor sea mayor a 4000."),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    } else if(input$pruebas=="Correlación de atrasos"){
      prueba <- CorrelationTest(numeros, as.numeric(input$alpha))
      estadistico <- as.numeric(prueba[1])
      cuantil <- as.numeric(prueba[2])
      pvalue <- round(as.numeric(prueba[3]), 2)
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
      atrasos <- as.numeric(prueba[6])
    }
    
    
    ### Resultados de la región de rechazo ###
    ##########################################
    if(input$pruebas == "Correlación de atrasos"){
      output$hist_distribucion <- renderPlot({
        ggplot(data.frame(x = c(-3, 3)), aes(x)) +
          stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) + 
          stat_function(fun = dnorm,
                        args = list(mean = 0, sd = 1),
                        xlim = c(cuantil,3), 
                        geom = "area",
                        aes(fill = "indianred2")) +
          stat_function(fun = dnorm,
                        args = list(mean = 0, sd = 1),
                        xlim = c(-3,-cuantil), 
                        geom = "area",
                        aes(fill = "indianred2")) +
          geom_vline(aes(xintercept = estadistico, color = "black")) +
          scale_colour_manual(values = "black", labels = "Estadístico de prueba", name = "") +
          scale_fill_manual(values = "indianred2", labels = "Región de rechazo", name = "") +
          ylab("Densidad") + 
          theme(legend.position = c(.84, .80), panel.background = element_blank(), 
                legend.text=element_text(size=12))
        })
      shinyjs::hide(id = "valuebox_rechazo")
      shinyjs::hide(id = "valuebox_estadistico")
      shinyjs::show(id = "hist_distribucion")
    }
    
    k <- as.numeric(input$intervalos)
    d <- 2
    if(input$pruebas == "Prueba de las corridas" || input$pruebas == "Prueba de la Ji Cuadrada" || 
         input$pruebas == "Prueba Serial"){
      if(input$pruebas=="Prueba de las corridas"){
        df <- 6
        shinyjs::hide(id = "hist_intervalos")
        }else if(input$pruebas=="Prueba de la Ji Cuadrada") {
        df <- k - 1
        shinyjs::hide(id = "hist_intervalos")
        }else{
        df <- k^d - 1
        data <- data.frame(
          U1 <- U1_graph,
          U2 <- U2_graph
        )
        output$hist_intervalos <- renderPlot({
          ggplot(data, aes(U1, U2)) +
            geom_point() + 
            scale_x_continuous(limits = c(0,1), expand = c(0, 0)) + 
            scale_y_continuous(limits = c(0,1), expand = c(0, 0)) + 
            geom_hline(yintercept=limits) + 
            geom_vline(xintercept=limits)
        })
        shinyjs::show(id = "hist_intervalos")
        }
      output$hist_distribucion <- renderPlot({
        xtemp <- rchisq(10000, df)
        ggplot(data.frame(x = xtemp), aes(xtemp)) +
          stat_function(fun = dchisq, args = list(df = df)) + 
          stat_function(fun = dchisq, 
                        args = list(df = df),
                        xlim = c(cuantil,max(xtemp)),
                        geom = "area",
                        aes(fill = "Región de rechazo")) +
          geom_vline(aes(xintercept = estadistico, color = "Estadístico de prueba")) +
          scale_colour_manual(values = "black", labels = "Estadístico de prueba", name = "") +
          scale_fill_manual(values = "indianred2", labels = "Región de rechazo", name = "") +
          ylab("Densidad") + 
          xlab("x") + 
          theme(legend.position = c(.75, .80), panel.background = element_blank(), 
                legend.text=element_text(size=12))
        })
      
      shinyjs::hide(id = "valuebox_rechazo")
      shinyjs::hide(id = "valuebox_estadistico")
      shinyjs::show(id = "hist_distribucion")
    }
    if(input$pruebas == "Prueba de Kolmogorov-Smirnov" || input$pruebas == "Prueba de Cramer-von Mises"){
      shinyjs::hide(id = "hist_distribucion")
      shinyjs::show(id = "valuebox_rechazo")
      shinyjs::show(id = "valuebox_estadistico")
      shinyjs::hide(id = "hist_intervalos")
      if(input$pruebas == "Prueba de Kolmogorov-Smirnov"){
        output$valuebox_rechazo <- renderValueBox({
          valueBox(
            paste("{D: D ", intToUtf8(8805), cuantil,"}"), "Región de rechazo", icon = icon("chart-pie"),
            color = "red"
          )
        })
        output$valuebox_estadistico <- renderValueBox({
          valueBox(
            estadistico, "D_n", icon = icon("chart-pie"),
            color = "aqua"
          )
        })
      }
      if(input$pruebas == "Prueba de Cramer-von Mises"){
        output$valuebox_rechazo <- renderValueBox({
          valueBox(
            paste("{Z: Z ", intToUtf8(8805), cuantil,"}"), "Región de rechazo", icon = icon("chart-pie"),
            color = "red"
          )
        })
        output$valuebox_estadistico <- renderValueBox({
          valueBox(
            estadistico, "Z", icon = icon("chart-pie"),
            color = "aqua"
          )
        })
      }
    }
    
    
    if(rechazo_por_region == 1){
      if(input$pruebas == "Prueba de la Ji Cuadrada" || input$pruebas == "Prueba Serial" ||
         input$pruebas == "Prueba de Kolmogorov-Smirnov" || input$pruebas == "Prueba de Cramer-von Mises"){
        output$text_pvalue <- renderText({"Existe suficiente evidencia para rechazar la uniformidad de los números generados."})
      }else{
        output$text_pvalue <- renderText({"Existe suficiente evidencia para rechazar la independencia de los números generados."})
      }
    }else{
      if(input$pruebas == "Prueba de la Ji Cuadrada" || input$pruebas == "Prueba Serial" ||
         input$pruebas == "Prueba de Kolmogorov-Smirnov" || input$pruebas == "Prueba de Cramer-von Mises"){
        output$text_pvalue <- renderText({"No existe suficiente evidencia para rechazar la uniformidad de los números generados."})
      }else{
        output$text_pvalue <- renderText({"No existe suficiente evidencia para rechazar la independencia de los números generados."})
      }
    }

    
    
    ### Resultados del valor p ###
    ##############################
    output$valuebox_pvalue <- renderValueBox({
      valueBox(
        pvalue, "Valor p", icon = icon("percent"),
        color = "teal"
      )
    })
    if(rechazo_por_pvalue == 1){
      if(input$pruebas == "Prueba de la Ji Cuadrada" || input$pruebas == "Prueba Serial" ||
         input$pruebas == "Prueba de Kolmogorov-Smirnov" || input$pruebas == "Prueba de Cramer-von Mises"){
        output$text_region <- renderText({"Existe suficiente evidencia para rechazar la uniformidad de los números generados."})
      }else{
        output$text_region <- renderText({"Existe suficiente evidencia para rechazar la independencia de los números generados."})
      }
    }else{
      if(input$pruebas == "Prueba de la Ji Cuadrada" || input$pruebas == "Prueba Serial" ||
         input$pruebas == "Prueba de Kolmogorov-Smirnov" || input$pruebas == "Prueba de Cramer-von Mises"){
        output$text_region <- renderText({"No existe suficiente evidencia para rechazar la uniformidad de los números generados."})
      }else{
        output$text_region <- renderText({"No existe suficiente evidencia para rechazar la independencia de los números generados."})
      }
    }
  })
  
}

shinyApp(ui, server)
