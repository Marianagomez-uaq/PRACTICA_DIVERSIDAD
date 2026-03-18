
# Diversidad alfa

################################################################################


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

Ejemplo <- (Jaccard (Abundanciasexperimento))
Ejemplo


heatmap (Ejemplo)

# Bray-Curtis
library(reshape2) #cargas la libreria 

indiceBC <- vegdist(msimp, method = "bray") #aqui utilizas la funcion vegdist se utiliza para calcular diferentes indices utilizados en ecologia. 
#utilizas la misma matriz utilizada en el indice de simpson y se utiliza el metodo de bray para hacer el Bray-Curtis
indiceBC #imprime el indice de Bray-Curtis







