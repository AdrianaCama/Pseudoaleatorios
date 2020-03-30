library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(ggfortify)

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
               # radioButtons(inputId = "generador",
               #              label = "Elige un generador: ",
               #              choices = c( "Generador de valores de una v.a. con distribución 
               #                          uniforme", "Monte Carlo", 
               #                           "Generador Congruencial Multiplicativo", 
               #                          "Generador Congruencial Mixto o Congruencial Lineal"),
               #              selected = "Monte Carlo"
               # ),
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
    if(is.numeric(a)==FALSE){
      showModal(modalDialog(
        title = "Error",
        "Alguno de los valores introducidos es incorrecto, por favor verifique sus entradas."
      ))
      error <- 1
    } else {
      error <- 0
      numeros <<- unif_generator(n = n, seed = semilla, m = m, a = a, c = c)
    }
    
    
    if(error == 0){
      output$random_table <- DT::renderDataTable({data.frame("Valores" = numeros)})
      output$hist_uniformidad <- renderPlot({
        ggplot(data.frame("Valores" = numeros)) + geom_histogram(aes(x=Valores,y=..density..), 
                                                                 alpha=0.7, 
                                                                 breaks=seq(0, 1, 0.05), 
                                                                 closed="left", 
                                                                 color="white", 
                                                                 fill="orange") + 
          stat_function(fun = dunif, args = list(0,1), colour = "dodgerblue3",size=1) 
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
    k <- 5
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
    if(estadistico > cuantil){
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
    
    return(list(Y, quantile_ChiSquared, p_value, rechazo_por_region, rechazo_por_pvalue))
  }
  
  
  # Prueba 2: "Prueba Serial"########################
  ###################################################
  ###################################################
  SerialTest <- function(numeros, alfa){
    
    library(ggplot2)
    # Establecer el tamaño de k. Mandar aviso al usuario si establece una k muy grande.
    k <- 5
    
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
    estadistico <- ((k^(2))/(length(temp)))*sum((count-expected)^2)
    Y <- estadistico

    # Grados de libertad
    df <- ((k^(2))-1)
    
    # Cálculo del cuantil
    cuantil <- qchisq(p=1-alfa,df=df,lower.tail = TRUE)
    quantile_SerialTest <- cuantil
    
    # Cálculo del valor p
    p_value <- pchisq(q=estadistico,df=df,lower.tail = FALSE)
    # p_value <- pchisq(q=estadistico,df=k-1,lower.tail = FALSE)
    
    # Rechazo por región
    if(estadistico > cuantil){
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
    
    print("Rechazo por region")
    print(rechazo_por_region)
    
    print("Rechazo por p value")
    print(rechazo_por_pvalue)
    
    
    # Graficar los datos
    data <- data.frame(
      U1 <- U1,
      U2 <- U2
    )
    
    # Generar los límites de los intervalos para las gráficas
    limits <- seq(from = 0, to = 1, length.out = k + 1)
    
    graph <- ggplot(data, aes(U1, U2)) + geom_point() + scale_x_continuous(limits = c(0,1), expand = c(0, 0)) + scale_y_continuous(limits = c(0,1), expand = c(0, 0)) + geom_hline(yintercept=limits) + geom_vline(xintercept=limits)
    plot(graph) 
    
    return(list(Y, quantile_SerialTest, p_value, rechazo_por_region, rechazo_por_pvalue, graph))
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
    
    i <- 1
    fin <- 0
    opcion <- 0
    while(fin == 0){
      if(n<= 40){
        if (tabla_KS[n, i]>D_n){
          fin <- 1
          if(i == 1){
            p_value <- "VP>0.20"
          }
          if(i == 2){
            p_value <- "0.10<VP<0.20"
          }
          if(i == 3){
            p_value <- "0.05<VP<0.10"
          }
          if(i == 4){
            p_value <- "0.02<VP<0.05"
          }
          if(i == 5){
            p_value <- "0.01<VP<0.02"
          }
          opcion <- 1
        } else if (tabla_KS[n, i]==D_n){
          fin <- 1
          if(i == 1){
            p_value <- 0.2
          }
          if(i == 2){
            p_value <- 0.1
          }
          if(i == 3){
            p_value <- 0.05
          }
          if(i == 4){
            p_value <- 0.02
          }
          if(i == 5){
            p_value <- 0.01
          }
          opcion <- 2
        }
        i <- i + 1
        if(i == 6){
          fin <- 1
          p_value <- "VP<0.01"
          }
        }else{
          if (tabla_KS[41, i]>D_n){
            fin <- 1
            if(i == 1){
              p_value <- "VP>0.20"
            }
            if(i == 2){
              p_value <- "0.10<VP<0.20"
            }
            if(i == 3){
              p_value <- "0.05<VP<0.10"
            }
            if(i == 4){
              p_value <- "0.02<VP<0.05"
            }
            if(i == 5){
              p_value <- "0.01<VP<0.02"
            }
            opcion <- 1
          } else if (tabla_KS[41, i]==D_n){
            fin <- 1
            if(i == 1){
              p_value <- 0.2
            }
            if(i == 2){
              p_value <- 0.1
            }
            if(i == 3){
              p_value <- 0.05
            }
            if(i == 4){
              p_value <- 0.02
            }
            if(i == 5){
              p_value <- 0.01
            }
            opcion <- 2
          }
          i <- i + 1
          if(i == 6){
            fin <- 1
            p_value <- "VP<0.01"
          }
        }
      } 
    
    D_n <- as.numeric(D_n)
    d_alpha <- as.numeric(d_alpha)
    
    if(D_n> d_alpha){
      rechazo_por_region <- 1
    } else{
      rechazo_por_region <- 0
    }
    
    if(opcion == 1){
      if((substr(p_value, nchar(p_value)-3, nchar(p_value)) <= alpha && i != 1) || i ==6){
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
  
  
  # Prueba 5: Prueba de las corridas ################
  ###################################################
  ###################################################
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
        r[largo] <- r[largo] + 1
        largo <- 1
      }
    }
    if (largo!=1){
      r[largo] <- r[largo] + 1
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
    
    
    if(n>=4000){
      quantile_corridas <- qchisq(1-alpha, 6)
    } 
    # Falta cuando n<4000 con un else
    
    
    if(n>=4000){
      p_value <- pchisq(R, 6, lower.tail = FALSE)
    } 
    # Falta cuando n<4000 con un else
    
    
    if(R > quantile_corridas){
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
    
    # Número de atrasos
    atrasos <- 3
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
    
    p_value <- 2*pnorm(0.8641, lower.tail = FALSE)
    
    if(abs(A) > quantile_CorrelationTest){
      rechazo_por_region <- 1
    } else{
      rechazo_por_region <- 0
    }
    
    
    if(p_value <= alfa){
      rechazo_por_pvalue <- 1
    } else{
      rechazo_por_pvalue <- 0
    }
    
    return(list(Y, quantile_CorrelationTest, p_value, rechazo_por_region, rechazo_por_pvalue))
    
  }
  
  
  
  
  
  
  observeEvent(input$Button_evaluate, {
    #Codigo de las pruebas 
    if(input$pruebas=="Prueba de Cramer-von Mises"){
      prueba <- CvM(numeros, as.numeric(input$alpha))
      estadistico <- round(as.numeric(prueba[1]), 2)
      cuantil <- round(as.numeric(prueba[2]), 2)
      pvalue <- round(as.numeric(prueba[3]), 2)
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
    } else if(input$pruebas=="Prueba de la Ji Cuadrada"){
      prueba <- ChiSquaredTest(numeros, as.numeric(input$alpha))
      estadistico <- round(as.numeric(prueba[1]), 2)
      cuantil <- round(as.numeric(prueba[2]), 2)
      pvalue <- round(as.numeric(prueba[3]), 2)
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
    } else if(input$pruebas=="Prueba Serial"){
      prueba <- SerialTest(numeros, as.numeric(input$alpha))
      estadistico <- round(as.numeric(prueba[1]), 2)
      cuantil <- round(as.numeric(prueba[2]), 2)
      pvalue <- round(as.numeric(prueba[3]), 2)
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
    } else if(input$pruebas=="Prueba de Kolmogorov-Smirnov"){
      prueba <- KS(numeros, as.numeric(input$alpha))
      estadistico <- round(as.numeric(prueba[1]), 2)
      cuantil <- round(as.numeric(prueba[2]), 2)
      pvalue <- toString(prueba[3])
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
      
    } else if(input$pruebas=="Prueba de las corridas"){
      prueba <- PC(numeros, as.numeric(input$alpha))
      estadistico <- round(as.numeric(prueba[1]), 2)
      cuantil <- round(as.numeric(prueba[2]), 2)
      pvalue <- round(as.numeric(prueba[3]), 2)
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
    } else if(input$pruebas=="Correlación de atrasos"){
      prueba <- CorrelationTest(numeros, as.numeric(input$alpha))
      estadistico <- round(as.numeric(prueba[1]), 2)
      cuantil <- round(as.numeric(prueba[2]), 2)
      pvalue <- round(as.numeric(prueba[3]), 2)
      rechazo_por_region <- as.numeric(prueba[4])
      rechazo_por_pvalue <- as.numeric(prueba[5])
    }
    
    
    ### Resultados de la región de rechazo ###
    ##########################################
    if(input$pruebas == "Prueba de Cramer-von Mises"  || input$pruebas == "Correlación de atrasos"){
      output$hist_distribucion <- renderPlot({
        ggplot(data.frame(x = c(-3, 3)), aes(x)) +
          stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) + 
          stat_function(fun = dnorm,
                        args = list(mean = 0, sd = 1),
                        xlim = c(cuantil,3),
                        geom = "area",
                        fill = "orange") +
          geom_vline(xintercept = estadistico) 
          # scale_fill_manual(values = "orange", name = "Región de rechazo", 
          #                   labels = "Región de rechazo")
          #Ponerlo si queremos que se muestren los valores del estadístico y el cuantil 
          #scale_x_continuous(breaks=c(-2,0,estadistico,cuantil,2))
        })
      shinyjs::show(id = "hist_distribucion")
    }
    
    k <- 5
    d <- 2
    if(input$pruebas == "Prueba de las corridas" || input$pruebas == "Prueba de la Ji Cuadrada" || 
         input$pruebas == "Prueba Serial"){
      if(input$pruebas=="Prueba de las corridas"){
        df <- 6
        }else if(input$pruebas=="Prueba de la Ji Cuadrada") {
        df <- k - 1
        }else{
        df <- k^d - 1
        }
      output$hist_distribucion <- renderPlot({
        xtemp <- rchisq(10000, df)
        ggplot(data.frame(x = xtemp), aes(xtemp)) +
          stat_function(fun = dchisq, args = list(df = df)) + 
          stat_function(fun = dchisq, 
                        args = list(df = df),
                        xlim = c(cuantil,max(xtemp)),
                        geom = "area",
                        fill = "orange") +
          geom_vline(xintercept = estadistico) 
        })
      shinyjs::show(id = "hist_distribucion")
    }
    if(input$pruebas == "Prueba de Kolmogorov-Smirnov"){
      shinyjs::hide(id = "hist_distribucion")
    }
    
    
    if(rechazo_por_region == 1){
      output$text_region <- renderText({paste("Existe suficiente evidencia para rechazar la uniformidad y/o independencia de los números generados.")})
    }else{
      output$text_region <- renderText({paste("No existe suficiente evidencia para rechazar la uniformidad y/o independencia de los números generados.")})
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
      output$text_pvalue <- renderText({paste("Existe suficiente evidencia para rechazar la uniformidad y/o independencia de los números generados.")})
    }else{
      output$text_pvalue <- renderText({paste("No existe suficiente evidencia para rechazar la uniformidad y/o independencia de los números generados.")})
    }
  })
  
}

shinyApp(ui, server)
