
# Gráficas

################################################################################

library (ggplot2)

# Rango-abundancia

rank_abundance <- function (abundancias) {
  
  for (i in 1:nrow(abundancias)) { # un ciclo por cada sitio
    
    trans <- t(abundancias) # Inicialmente lo hice así porque por alguna razón se pierden los nombres sin la transpuesta, pero después me dí cuenta que no era necesario que tuviera los nombres, pero como el código funciona lo dejé así.
    
    vector <- trans[,i] # Sitio i
    vector <- vector[!is.na(vector)] # Elimina las especies no presentes
    ordenado <- sort (vector, decreasing = T) # Ordena por abundancia de mayor a menor
    ordenado <- as.data.frame(ordenado) # convierte a dataframe porque ggplot no acepta vectores
    
    nombre_archivo <- paste0 ("figures/rank_abundance_", i, ".png") # Con esto se genera un nombre único para cada sitio
    titulo <- paste0 ("Rango-Abundancia en Sitio", i)
    
    png (nombre_archivo) # El archivo será png, contiene todo lo que esté antes de dev.off()
    print (ggplot (ordenado, aes (x = which(ordenado > 0), y = log10(ordenado))) + # which indica la posición de los elementos que cumplen la condición (en este caso todos porque eliminé las NA), por lo que el eje de las X es el orden de cada especie. Y es el log10 las abundancias
             geom_line () + # Genera una linea
             geom_point() + # genera los puntos
             labs (title = titulo, # le pone el título que hice anteriormente
                   x = "Orden (Rango)",
                   y = "log10 (Abundancia)"))
    dev.off()
  }
}

rank_abundance (Abundanciasexperimento)


# Rarefacción
rarecurve(msimp, step = 1, # aqui le decimos que la curva la tome de 1 por 1 
          sample = min(rowSums(msimp)), # aquim decimos que dibuje una línea vertical en el tamaño de muestra mas pequeño
          col = c("red","blue", "cyan4", "pink", "orange2", "red4", "purple4"), # definimos el color de las lineas
          cex = 0.7, # definimos el tamaño de las etiquetas de los sitios
          main = "Curva de Rarefacción por Sitio", # le asignamos el titulo a la grafica
          xlab = "sitio", # le asignas un nombre al eje x 
          ylab = "riqueza") # le asignas un nombre al eje y 






