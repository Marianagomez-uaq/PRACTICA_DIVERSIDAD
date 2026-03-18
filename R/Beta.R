
# Diversidad alfa

################################################################################

install.packages("pheatmap")
library(pheatmap)

# Jaccard

Jaccard <- function (abundancias) {
  
  transpuesta <- t(abundancias)  # Intercambia las columnas por los renglones, lo hice porque por alguna razón, con el dataframe original no se conservan los nombres.
  n <- nrow(abundancias) # Objeto con el número de renglones (sitios) del dataframe
  Jacc <- (matrix (ncol = n, nrow = n)) # matriz cuadrada n x n
  
  for (i in 1:ncol(transpuesta)) { # Procesa el primer sitio a comparar
    
    A <- transpuesta[,i]
    A <- A[!is.na(A)] # Selecciona solo los que NO SON NA
    A <- names(A) # Reemplaza las abundancias por los nombres
    
    for (e in 1:ncol(transpuesta)) { # Procesa el segundo sitio a comparar
      
      B <- transpuesta[,e]
      B <- B[!is.na(B)]
      B <- names(B)
      
      inter <- intersect (A, B)
      uni <- union (A, B)
      
      Jac <-length(inter)/length(uni) # Fórmula de Jaccard
      
      Jacc[i, e] <- Jac # Agrega el resultado a las coordenadas de los dos sitios
    }
  }
  colnames (Jacc) <- rownames (abundancias)
  rownames (Jacc) <- rownames (abundancias)
  
  return (Jacc)
}

htmp_Jaccard <- function (abundancias, nombre_archivo) {
  
  Jaccar <- (Jaccard (abundancias)) # Usa la función anterior para generar la matriz
  nombre_del_archivo <- paste0 ("figures/", nombre_archivo, ".png") # Le pone el nombre que indique el usuario
  
  png (nombre_del_archivo) # El archivo será png, contiene todo lo que esté antes de dev.off()
  pheatmap(Jaccar,
           main = "Mapa de calor de disimilitud de Jaccard",
           display_numbers = T) # Para que se vean los números
  dev.off()
  
}

htmp_Jaccard (Abundancias_con_NA, "Mapa de calor Jaccard") # Ejemplo


# Bray-Curtis

install.packages("reshape2")
library(reshape2) #cargas la libreria 

indiceBC <- vegdist(msimp, method = "bray") #aqui utilizas la funcion vegdist se utiliza para calcular diferentes indices utilizados en ecologia. 
#utilizas la misma matriz utilizada en el indice de simpson y se utiliza el metodo de bray para hacer el Bray-Curtis
indiceBC #imprime el indice de Bray-Curtis

hmBC <- function(dist_obj, titulo) {
  melt(as.matrix(dist_obj)) %>% #el %>% sirve como tipo pipe para encadenar funciones 
    ggplot(aes(Var1, Var2, fill = value)) + #asignamos las variables 
    geom_tile(color = "black") + #asignas el color de las lineas que lo dividen 
    geom_text(aes(label = round(value, 2)), size = 3.5) + #asignas la cantidad de los decimales y el tamaño de los numeros 
    scale_fill_gradient(low = "cyan", high = "green", # determinamos la escala de color, cyan para el bajo y green para el alto
                        name = "Disimilitud") + # asignamos el nombre del heat map
    labs(title = titulo, x = NULL, y = NULL) + #asignamos el nombre a los ejes x y y, en este caso lo dejamos sin modificar
    
    theme_minimal(base_size = 10) + #asignas el tamaño que ocupa en la pantalla la tabla
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) #asignas el angulo de lo que esta escrito en el eje x y y
}
hmBC(indiceBC, "Disimilitud de Bray-Curtis") #imprimimos el heat map (mapa de calor)




