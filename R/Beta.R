
# Diversidad alfa

################################################################################


# Jaccard

Jaccard <- function (abundancias) {
  
  transpuesta <- t(abundancias)
  n <- nrow(abundancias)
  Jacc <- as.data.frame (matrix (ncol = n, nrow = n))
  
  for (i in 1:ncol(transpuesta)) {
    
    A <- transpuesta[,i] # Intercambia las columnas por los renglones, lo hice porque por alguna razón, con el dataframe original no se conservan los nombres.
    A <- A[!is.na(A)] # Selecciona solo los que NO SON NA
    A <- names(A) # Reemplaza las abundancias por los nombres
    
    for (e in 1:ncol(transpuesta)) {
      
      B <- transpuesta[,e]
      B <- B[!is.na(B)]
      B <- names(B)
      
      inter <- intersect (A, B)
      uni <- union (A, B)
      
      Jac <-length(inter)/length(uni)
      
      Jacc[i, e] <- Jac
    }
  }
  colnames (Jacc) <- rownames (abundancias)
  rownames (Jacc) <- rownames (abundancias)
  
  return (Jacc)
}

Ejemplo <- (Jaccard (Abundanciasexperimento))
Ejemplo


heatmap (Ejemplo, Rowv = NA, Colv = NA)

# Bray-Curtis
