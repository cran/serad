#' Met au pluriel selon la valeur numérique
#'
#' Retourne la forme singulière ou plurielle
#' selon la valeur absolue du nombre.
#'
#' @param a Valeur numérique.
#' @param sing Forme au singulier. Par défaut : chaîne vide.
#' @param plur Forme au pluriel. Par défaut : "s".
#' @param seuil Seuil à partir duquel la forme plurielle
#'   est utilisée (par défaut : 2).
#'
#' @return
#' Une chaîne de caractères correspondant à la forme
#' correctement accordée. NA si l'entrée est NA.
#'
#' @examples
#' serad::s(-7.5)
#' serad::s(-2)
#' serad::s(1.4, "chat parle", "chats parlent")
#' serad::s(-2, "chat parle", "chats parlent")
#' serad::s(1.97)
#' serad::s(arrondi_tot(1.97))
#' serad::s(1.97, seuil = 1.95)
#'
#' @seealso
#' \url{https://www.dictionnaire-academie.fr/article/QDL057}
#'
#' @export
s <- function(a, sing = "", plur = "s", seuil = 2) {

  if (is.na(a))
    return(NA_character_)

  if (abs(a) >= seuil) plur else sing
}

