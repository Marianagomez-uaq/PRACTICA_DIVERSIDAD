
# Diversidad alfa

################################################################################

Abundancias <- read.csv ("data/Abundancias.csv")
row.names (Abundancias) <- Abundancias$Muestra
Abundancias <- Abundancias [,2:13]

Rarefaccion <- read.csv ("data/Rarefaccion.csv")
row.names (Rarefaccion) <- Rarefaccion$Muestra
Rarefaccion <- Rarefaccion [,2:6]

Abundanciasexperimento <- read.csv ("data/Abundanciasexperimento.csv")
row.names (Abundanciasexperimento) <- Abundanciasexperimento$Muestra
Abundanciasexperimento <- Abundanciasexperimento [,2:13]



# Riqueza

Riqueza <- function (abundancias) {
  
  riquezas <- data.frame (Riqueza = numeric()) # Crea un dataframe con una columna vacía
  
  for (i in 1:nrow(abundancias)) { # ciclo for que tiene el número de ciclos como número de renglones, para calcular la riqueza de cada sitio
    vector <- abundancias[i,] # Toma las abundancias del sitio i
    vector <- as.numeric (vector) # Convierte el objeto a vector, porque era un dataframe y con dataframe no funciona na.omit
    vector_sin_NA <- na.omit(vector) # Elimina los NA del vector (Que son especies no encontradas en esa muestra)
    riquezas[i, 1] <- length(vector_sin_NA) # Cuenta cuántos elementos tiene el vector, y lo agrega al renglón i de la columna 1 del dataframe riquezas
  
    # Las últimas 2 líneas de código fueron modificadas de (Educative, s. f.).
    
  }
  
  row.names(riquezas) <- row.names (abundancias) # Le pone el nombre de los sitios a su respectiva riqueza
  
  return(riquezas)

}

Riqueza (Abundanciasexperimento)



# Shannon y Pielou

Shannon <- function (abundancias) { # Modificada de mi propio script (Gómez Becerra, 2026).

  shan <- data.frame (Shannon = numeric(), Pielou = numeric ()) # dataframe con 2 columnas vacías
  riqueza <- Riqueza (abundancias) # Riqueza usando la función creada anteriormente
  
  for (i in 1:nrow(abundancias)) { # Un ciclo por cada sitio
    
    total <- sum (abundancias[i,], na.rm = T) # Suma sin tomar en cuenta las NA
    frec <- abundancias[i,]/total
    mult <- (log(frec))*frec
    s <- -(sum(mult, na.rm = T))
    shan [i, 1] <- s
    
    if (riqueza[i, 1] == 1) { # Si la riqueza = 1, el logaritmo será cero, y no se puede dividir entre 0, por lo que genera un NAN. Por eso pongo un if, para que ponga texto en lugar de NaN.
      shan [i, 2] <- "No se puede calcular"
    } else {
      p <- s/log(riqueza[i, 1])
    shan [i, 2] <- p
    }
  }
  row.names(shan) <- row.names (abundancias)
  return(shan)
}

Shannon (Abundanciasexperimento)


# Simpson

# Simpson inverso

# Chao1

Chao1 <- function (abundancias) {
  
  riqueza <- Riqueza (abundancias)
  chao <- data.frame (Chao1 = numeric ())
  
  for (i in 1:nrow(abundancias)) {
    
    vector <- abundancias[i,] # Toma las abundancias del sitio i
    vector <- as.numeric (vector) # Convierte el objeto a vector, porque era un dataframe y con dataframe no funciona na.omit
    
    singletons <- sum(vector == 1, na.rm = T)
    doubletons <- sum(vector == 2, na.rm = T)
  
    if (singletons == 0 & doubletons == 0) { # Si no hay singletons ni doubletons, se dividiría 0/0, dando como resultado NaN. Por eso pongo un if, para que ponga texto en lugar de NaN.
      chao [i, 1] <- "No se puede calcular"
    } else if (doubletons == 0) { # Si sí hay singletons pero no doubletons, se dividiría x/0, lo que da Inf. Por eso pongo un if, para que ponga texto en lugar de Inf.
      chao [i, 1] <- "No se puede calcular"
    } else { 
      chao [i, 1] <- (riqueza[i, 1])+((singletons^2)/(2*doubletons))
    }
  }
  row.names(chao) <- row.names (abundancias)
  return(chao)
}

Chao1 (Abundanciasexperimento)

# Tabla resumen