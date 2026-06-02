#' Calcul d'une accélération
#'
#' Calcule l'accélération entre trois niveaux successifs en comparant
#' les taux de variation consécutifs.
#'
#' L'accélération correspond à la variation du taux entre
#' \code{(x1, x2)} et \code{(x2, x3)}.
#'
#' @param x1 Niveau le plus récent.
#' @param x2 Niveau précédent.
#' @param x3 Niveau le plus ancien.
#'
#' @return
#' Un nombre numérique correspondant à l'accélération en pourcentage.
#'
#' @seealso \code{\link{g}}, \code{\link{format_g}}
#'
#' @examples
#' a(4, 2, 1)  # 0
#' a(6, 2, 1)  # 100
#' a(2, 1, 1)  # valeur très élevée si taux précédent proche de zéro
#'
#' @export
a <- function(x1, x2, x3) {

  g12 <- serad::g(x1, x2)
  g23 <- serad::g(x2, x3)

  serad::g(g12, g23)
}

