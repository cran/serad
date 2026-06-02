#' Formatage des variations en pourcentage
#'
#' Formate une variation exprimée en pourcentage selon les règles
#' d'arrondi et d'affichage du package.
#'
#' @param y La variation à formater.
#' @param signe Indicateur logique : TRUE pour afficher le signe,
#'   FALSE pour le retirer (par défaut : TRUE).
#' @param detail Nombre de chiffres après la virgule.
#'   Par défaut, on utilise getOption("serad")$arrondi_pourcent.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant à la variation formatée
#' (ex. "+5,4%", "-5,4%", "+5.4%").
#'
#' @seealso \code{\link{g}}, \code{\link{arrondi_tot}}
#'
#' @details
#' Le symbole "moins" peut être personnalisé via
#' getOption("serad")$moins.
#'
#' @examples
#' format_g(5.3654, signe = FALSE)           # "5,4 %"
#' format_g(5.3654)                          # "+5,4 %"
#' format_g(-5.3654, FALSE)                  # "5,4 %"
#' format_g(-5.3654)                         # "-5,4 %"
#' format_g(-5.3654, detail = 2)             # "-5,37 %"
#' format_g(0.35)                            # "+0,4 %"
#' format_g(5.3654, lang = "en")             # "+5.4%"
#'
#' @export
format_g <- function(y,
                     signe = TRUE,
                     detail = getOption("serad")$arrondi_pourcent,
                     lang = get_serad_language()) {

  moins <- if (lang == "fr") getOption("serad")$moins else "-"
  espace_pct <- if (lang == "fr") "\u00A0" else ""

  y0 <- serad::arrondi_tot(y, detail)

  fmt <- paste0("%+.", detail, "f")
  w <- sprintf(fmt, y0)

  if (lang == "fr") {
    w <- gsub("\\.", ",", w)
  }

  if (!signe) {
    w <- gsub("^\\+", "", w)
    w <- gsub("^[-\u2212]", "", w)
  } else {
    w <- gsub("^-", moins, w)
  }

  paste0(w, espace_pct, "%")
}
