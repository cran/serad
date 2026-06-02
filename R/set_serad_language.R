#' Définit la langue de serad
#'
#' @description
#' Modifie la langue utilisée par \{serad\} et réinitialise
#' les règles de rédaction associées.
#'
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' NULL invisiblement.
#'
#' @export
set_serad_language <- function(lang = c("fr", "en")) {
  lang <- match.arg(lang)

  options(serad.lang = lang)

  if (identical(lang, "en")) {
    init_serad_en()
  } else {
    init_serad_fr()
  }

  invisible(NULL)
}

#' Récupère la langue de serad
#'
#' @description
#' Retourne la langue actuellement utilisée par \{serad\}.
#'
#' @return
#' Une chaîne de caractères indiquant la langue courante ("fr" ou "en").
#'
#' @export
get_serad_language <- function() {
  getOption("serad.lang", "fr")
}
