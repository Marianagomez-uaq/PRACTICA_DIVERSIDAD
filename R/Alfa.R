
# Diversidad alfa

################################################################################

Abundancias <- read.csv ("data/Abundancias.csv")

row.names (Abundancias) <- Abundancias$Muestra
Abundancias <- Abundancias [,2:13]

Rarefaccion <- read.csv ("data/Rarefaccion.csv")

row.names (Rarefaccion) <- Rarefaccion$Muestra
Rarefaccion <- Rarefaccion [,2:6]



# Riqueza

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