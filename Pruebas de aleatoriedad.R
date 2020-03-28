#Cambiar por alfa seleccionada
alfa <- 0.05
# Cambiar por lo generado en el programa
num <- c(0.045555, 0.065749, 0.092871, 0.149668, 0.190782, 0.224291, 0.260000, 0.321474, 
             0.332037, 0.392275, 0.404315, 0.431058, 0.468068, 0.495164, 0.569813, 0.631893, 
             0.652066, 0.785885, 0.830250, 0.846584)
sec <- c(0.22317383, 0.82220999, 0.11944974, 0.23844417, 0.00416896, 0.04593599, 
               0.88826024, 0.73882373, 0.2347258, 0.81838537, 0.29577309, 0.06599242,
               0.48352499, 0.41014991, 0.6706971, 0.88065793, 0.854395, 0.39438387,
               0.96948658, 0.16150143, 0.82201839, 0.43925927, 0.88317153, 0.32954575,
               0.55911048, 0.70385292, 0.91883758, 0.89169391, 0.9144542, 0.27766943)



# Prueba 1: Prueba de la Ji Cuadrada #########################################################
##############################################################################################
##############################################################################################

# Esta función requiere la muestra, nivel de significancia, número de intervalos
ChiSquared <- function(numeros, alfa){
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
  
  p_value <- qchisq(p=1-alfa,df=k-1,lower.tail = FALSE)
  
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





# Prueba 2: Prueba serial ####################################################################
##############################################################################################
##############################################################################################

SerialTest <- function(numeros, alfa){
  
  library(ggplot2)
  # Establecer el tamaño de k. Mandar aviso al usuario si establece una k muy grande.
  k <- 5
  
  # Checar si el número de datos a verificar independencia es par
  if(length(num)%%2 == 1){
    # Si no es par, generar una observación más a num y agregarla
    # PENDIENTE: Checar si está bien quitar una observación.
    num_2 <- num[-1]
  } else {
    num_2 <- num
  }
  
  # "Revolver" los números
  num_2 <- sample(num_2, size = length(num_2), replace = FALSE)
  U1 <- num_2[1:(length(num_2)/2)]
  U2 <- num_2[((length(num_2)/2)+1):length(num_2)]
  
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
  expected <- matrix(length(temp)/(k^2), k, k)
  
  #Cálculo del estadístico
  estadistico <- ((k^(2))/(length(temp)))*sum((count-expected)^2)
  Y <- estadistico
  
  # Cálculo del valor p
  df <- ((k^(2))-1)
  
  p_value <- 1 - pchisq(estadistico, df = df, lower.tail = FALSE)
  
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
  
  return(list(Y, quantile_SerialTest, p_value, rechazo_por_region, rechazo_por_pvalue))
}


# if(pvalue < alfa){
#   print("Se rechaza la hipótesis nula. Es decir, no existe suficiente evidencia para afirmar que
#         la muestra es independiente")
# } else{
#   print("No se rechaza la hipótesis nula. Es decir, no existe suficiente evidencia para afirmar que
#         la muestra no es independiente")
# }

# Graficar los datos
data <- data.frame(
  U1 <- U1,
  U2 <- U2
)

# Generar los límites de los intervalos para las gráficas
limits <- seq(from = 0, to = 1, length.out = k + 1)

graph <- ggplot(data, aes(U1, U2)) + geom_point() + scale_x_continuous(limits = c(0,1), expand = c(0, 0)) + scale_y_continuous(limits = c(0,1), expand = c(0, 0)) + geom_hline(yintercept=limits) + geom_vline(xintercept=limits)
graph 



# Kolmogorov-Smirnov #########################################################################
##############################################################################################
##############################################################################################
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


KS <- function(numeros, alpha) {
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
  while(fin == 0){
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
    }
    i <- i + 1
    if(i == 6){
      fin <- 1
      p_value <- "VP<0.01"
    }
  }
  
  D_n <- as.numeric(D_n)
  d_alpha <- as.numeric(d_alpha)
  
  if(D_n> d_alpha){
    rechazo_por_region <- 1
  } else{
    rechazo_por_region <- 0
  }
  

  if(p_value <= alpha || ((substr(p_value, nchar(p_value)-3, nchar(p_value)) <= alpha && i != 1) || i ==6)){
    rechazo_por_pvalue <- 1
  } else{
    rechazo_por_pvalue <- 0
  }
  
  
  return(list(D_n, d_alpha, p_value, rechazo_por_region, rechazo_por_pvalue))
}




# Cramer-von Mises ###########################################################################
##############################################################################################
##############################################################################################
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




# Prueba de las corridas #####################################################################
##############################################################################################
##############################################################################################
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
    quantile_corridas <- qchisq(1-alpha)
  } 
  # Falta cuando n<4000 con un else
  
  
  if(n>=4000){
    p_value <- pchisq(R, 6)
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





# Prueba 6: Correlación de atrasos ###########################################################
##############################################################################################
##############################################################################################

# Obtener parámetros
n <- length(num)

# Número de atrasos
atrasos <- 3
h <- floor((n-1)/atrasos)-1

num

# Formar vectores para el estimador considerando el número de atrasos
temp <- num[seq(from = 1, to = length(num), by = atrasos)]

lag <- temp[-1]
normal <- temp[-length(temp)]

lag <- lag[1:h]
normal <- normal[1:h]




p_hat <-  ((12/(h+1)) * (sum(lag*normal))) - 3
variance <- (13*h+7)/((h+1)^(2))

A <- p_hat/sqrt(variance)
z <- qnorm(1-(alfa/2))

if(abs(A)>z){
  print("Se rechaza la hipótesis nula. Es decir, existe suficiente evidencia para afirmar que los datos están correlacionados")
} else {
  print("No se rechaza la hipótesis nula. Es decir, no existe suficiente evidencia para afirmar que los datos no son independientes")
}



# Resultados #################################################################################
##############################################################################################
##############################################################################################

prueba1 <- KS(num, alfa)

prueba2 <- CvM(num, alfa)

prueba3 <- PC(sec, alfa)




