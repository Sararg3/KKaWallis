#' Prueba Kruskal Wallis
#'
#' Realiza una prueba no parametrica
#'
#'@param x (lista) contiene los datos de los grupos
#'@param alpha (valor) valor signicacion
#'@return Una lista con valor de H estaditico, grados de libertad, valor critico y .
#'@export
#'
#'@examples
#'\dontrun{
#' #Primer ejemplo
#'datos_grupo1 <- c(14, 10, 11, 13)
#'datos_grupo2 <- c(16, 18, 14, 15)
#'datos_grupo3 <- c(16, 15, 14, 12)
#'datos_grupo4 <- c(17, 20, 19, 21)
#'datos<-list(datos_grupo1, datos_grupo2, datos_grupo3, datos_grupo4)
#'#Cargo la libreria
#'library(Kruskal.Wallis)
#'KKalWallis(x=datos, alpha=0.05)
#'#Realizar la prueba de Kruskal-Wallis
#'cat("Estadístico H:", resultado$chi_squared, "\n")
#'cat("Grados de libertad:", resultado$df, "\n")
#'cat("Valor crítico:", resultado$Vcritico, "\n")
#'}
KKalWallis <- function(x, alpha) {
  # número de tratamientos
  long<- length(x)

  # ni (número de observaciones en el i-ésimo tratamiento)
  vectores<- unlist(lapply(x, length))

  # Rangos de los datos
  rango<- rank(unlist(x))
  Media<- NULL
  ran<- 1
  for (p in 1:long) {
    # Medias de los rangos por grupo
    Media<- c(Media, mean(rango[ran:(ran + vectores[p] - 1)]))
    ran<- ran + vectores[p]

  }
  # Calcular la media general de los rangos y la suma de los elementos
  Media.long <- mean(Media)
  suma<- sum(vectores)
  # Estadístico de prueba
  Estadistico<- (12 / (suma * (suma + 1))) * sum(vectores * (Media - Media.long)^2)

  critico<- qchisq(1 - alpha, long - 1)
  if (Estadistico > critico) {
    cat("Se rechaza la hipótesis nula.\n")

  } else {
    cat("No se rechaza la hipótesis nula.\n")
  }

  return(list(grupos = long, n_i = vectores, suma = suma, Media_p = round(Media, 3), Media = Media.long, chi_squared = round(Estadistico, 3), df = long - 1, Vcritico = round(qchisq(1 - alpha, long - 1), 3), pvalue = 1 - pchisq(Estadistico, long - 1)))
}


