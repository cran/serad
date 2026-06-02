#' Formatage des niveaux en nombres
#'
#' Formate un niveau numérique selon les règles d'arrondi
#' définies dans le package.
#'
#' @param y Le niveau à formater.
#' @param detail Précision d'arrondi. Par défaut, on utilise
#'   getOption("serad")$arrondi_niv.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant au niveau formaté.
#'
#' @seealso \code{\link{arrondi_tot}}
#'
#' @examples
#' format_niv(365484)                # "365 500"
#' format_niv(365484, lang = "en")   # "365,500"
#'
#' @export
format_niv <- function(y,
                       detail = getOption("serad")$arrondi_niv,
                       lang   = get_serad_language()) {

  moins <- getOption("serad")$moins

  y0 <- arrondi_tot(y, detail)

  # séparateur milliers
  mark <- if (lang == "fr") {
    "\u00a0"   # espace insécable
  } else {
    ","
  }

  w <- format(
    y0,
    big.mark = mark,
    scientific = FALSE,
    trim = TRUE
  )

  gsub("-", moins, w)
}
