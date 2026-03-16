
# Diversidad alfa

################################################################################

Abundancias_con_ceros <- read.csv ("data/Abundancias.csv")

row.names (Abundancias) <- Abundancias$Muestra
Abundancias <- Abundancias [,2:13]

Rarefaccion <- read.csv ("data/Rarefaccion.csv")

row.names (Rarefaccion) <- Rarefaccion$Muestra
Rarefaccion <- Rarefaccion [,2:6]

Abundanciasexperimento <- read.csv ("data/Abundanciasexperimento.csv")

row.names (Abundanciasexperimento) <- Abundanciasexperimento$Muestra
Abundanciasexperimento <- Abundanciasexperimento [,2:13]

Abundanciasexperimento



# Riqueza

Riqueza <- function (abundancias) {
  
  riquezas <- data.frame (Riqueza = numeric()) # Crea un data
  
  for (i in 1:nrow(abundancias)) {
    vector <- abundancias[i,]
    vector <- as.numeric (vector)
    vector_sin_NA <- na.omit(vector)
    # colnames(vector) <- NULL
    riquezas[i, 1] <- length(vector_sin_NA)
  }
  
  row.names(riquezas) <- row.names (Abundancias)
  
  return(riquezas)

}

Riqueza (Abundanciasexperimento)



# Shannon
abund2 <- c (50, 50, 50)

Shannon <- function (abund) {

  riqueza <- length (abund)
  total <- sum (abund)
  frec <- abund/total
  mult <- (log(frec))*frec
  s <- -(sum(mult))
  print (s)
  p <- s/log(riqueza)
  
  message ("Índice de Shannon")
  print (s)
  
  message ("Pielou")
  print (p)
}

Shannon (abund2)


# Simpson

# Simpson inverso

# Pielou

# Chao1

# Tabla resumen