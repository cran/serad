#' Formatage des variations en niveau
#'
#' Formate une différence numérique selon les règles d'arrondi
#' et d'affichage utilisées dans le package.
#'
#' @param y La différence à formater.
#' @param signe Indicateur logique : TRUE pour afficher le signe,
#'   FALSE sinon (par défaut : TRUE).
#' @param detail Précision d'arrondi. Par défaut, on utilise
#'   getOption("serad")$arrondi_niv.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant à la variation formatée.
#'
#' @seealso \code{\link{arrondi_tot}}, \code{\link{format_niv}}
#'
#' @examples
#' format_delta(365484)              # "+365 500"
#' format_delta(365484, FALSE)       # "365 500"
#' format_delta(-365484)             # "-365 500"
#' format_delta(-365484, FALSE)      # "365 500"
#' format_delta(365484, lang = "en") # "+365,500"
#'
#' @export
format_delta <- function(y,
                         signe = TRUE,
                         detail = getOption("serad")$arrondi_niv,
                         lang = get_serad_language()) {

  z <- format_niv(
    y,
    detail = detail,
    lang = lang
  )

  if (signe) {

    if (y >= 0) {
      return(paste0("+", z))
    } else {
      return(z)
    }

  } else {

    return(
      gsub("[-\u2212]", "", z)
    )

  }
}
