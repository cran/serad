#' Calcul d'une variation relative
#'
#' Calcule la variation relative entre \code{x1} et \code{x2},
#' exprimée en pourcentage.
#'
#' @param x1 Niveau le plus récent.
#' @param x2 Niveau le plus ancien.
#' @param eps Valeur utilisée à la place de \code{x2} lorsque
#'   \code{x2 = 0}, afin d'éviter une division par zéro.
#'   Par défaut : \code{1e-8}.
#'
#' @return
#' Une valeur numérique correspondant à la variation en pourcentage
#' (par exemple, si x1 = 2 * x2, la fonction retourne 100).
#'
#' @seealso \code{\link{format_g}}
#'
#' @details
#' Si \code{x2 = 0}, la valeur epsilon définie par
#' \code{getOption("serad")$eps} est utilisée afin d'éviter une division
#' par zéro. Un message d'avertissement est émis dans ce cas.
#'
#' Il est possible de modifier cette valeur :
#'
#' \code{
#' serad0 <- getOption("serad")
#' serad0$eps <- 0
#' options(serad = serad0)
#' }
#'
#' @examples
#' g(2, 1)  # 100
#' g(2, 0)  # valeur très élevée et avertissement
#'
#' @export
g <- function(x1, x2, eps = 1e-8) {

  zero_denom <- (x2 == 0)

  if (any(zero_denom, na.rm = TRUE)) {
    warning("division par 0 dans serad::g()")
  }

  x2_adj <- ifelse(zero_denom, eps, x2)

  100 * (x1 / x2_adj - 1)
}
