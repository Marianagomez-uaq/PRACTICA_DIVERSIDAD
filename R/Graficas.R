
# Gráficas

################################################################################


# Rango-abundancia

abu <- c (4, 7, 10, 1)
rank_abundance <- function (abu) {
  
  ordenado <- sort (abu, decreasing = T)
  plot (ordenado)
}

rank_abundance(abu)


# Rarefacción