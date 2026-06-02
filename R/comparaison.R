#' Comparaison qualitative entre deux niveaux
#'
#' Compare deux niveaux successifs et retourne une formulation
#' selon l'évolution observée.
#'
#' @param x1 Niveau le plus récent.
#' @param x2 Niveau le plus ancien.
#' @param hausse_defaut Formulation en cas de hausse.
#' @param egalite_defaut Formulation en cas de stabilité.
#' @param baisse_defaut Formulation en cas de baisse.
#' @param seuil Seuil d'égalité en valeur absolue. Par défaut : 0.1.
#' @param alt Indicateur logique permettant d'utiliser
#'   une formulation alternative.
#' @param hausse_alt Formulation alternative en cas de hausse.
#' @param egalite_alt Formulation alternative en cas de stabilité.
#' @param baisse_alt Formulation alternative en cas de baisse.
#'
#' @details
#' La comparaison repose sur le taux de variation calculé via \code{\link{g}}.
#' Des cas particuliers sont traités lorsque \code{x2} est nul ou négatif.
#'
#' @return
#' Une chaîne de caractères correspondant à la formulation retenue.
#'
#' @seealso \code{\link{comparaison_taux}}, \code{\link{g}}
#'
#' @examples
#' comparaison(1.04, 1, "augmente", "reste stable", "diminue")
#' comparaison(0.9991, 1, "augmente", "reste stable", "diminue")
#' comparaison(1, 1, "augmente", "reste égal", "diminue", seuil = 0)
#'
#' @export
comparaison <- function(x1, x2,
                        hausse_defaut,
                        egalite_defaut,
                        baisse_defaut,
                        seuil = 0.1,
                        alt = 0,
                        hausse_alt  = hausse_defaut,
                        egalite_alt = egalite_defaut,
                        baisse_alt  = baisse_defaut) {

  hausse  <- if (alt == 0) hausse_defaut  else hausse_alt
  egalite <- if (alt == 0) egalite_defaut else egalite_alt
  baisse  <- if (alt == 0) baisse_defaut  else baisse_alt

  if (x2 == 0) {
    if (x1 > abs(seuil)) {
      return(hausse)
    } else if (x1 < -abs(seuil)) {
      return(baisse)
    } else {
      return(egalite)
    }
  }

  if (x2 > 0) {
    return(
      comparaison_taux(
        g(x1, x2),
        hausse_defaut, egalite_defaut, baisse_defaut,
        seuil, alt,
        hausse_alt, egalite_alt, baisse_alt
      )
    )
  }

  if (x2 < 0 && x1 > 0) {
    return(
      comparaison_taux(
        g(-x2, -x1),
        hausse_defaut, egalite_defaut, baisse_defaut,
        seuil, alt,
        hausse_alt, egalite_alt, baisse_alt
      )
    )
  }

  if ((x1 - x2) <= abs(seuil)) {
    return(egalite)
  } else {
    return(hausse)
  }
}


#' Position relative entre deux niveaux
#'
#' Indique si \code{x1} est au-dessus ou en dessous de \code{x2}.
#'
#' @param x1 Niveau le plus récent.
#' @param x2 Niveau le plus ancien.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères.
#'
#' @seealso \code{\link{comparaison}}
#'
#' @export
audessus <- function(x1, x2, lang = get_serad_language()) {

  if (lang == "en") {
    return(comparaison(
      x1, x2,
      hausse_defaut  = "above",
      egalite_defaut = "at or above",
      baisse_defaut  = "below",
      seuil = 0
    ))
  }

  comparaison(
    x1, x2,
    hausse_defaut  = "au-dessus",
    egalite_defaut = "au-dessus",
    baisse_defaut  = "en dessous",
    seuil = 0
  )
}


#' Évolution à la hausse, à la baisse ou inchangée
#'
#' Indique si \code{x1} évolue à la hausse, à la baisse ou reste
#' inchangé par rapport à \code{x2}.
#'
#' @param x1 Niveau le plus récent.
#' @param x2 Niveau le plus ancien.
#' @param seuil Seuil d'égalité en valeur absolue.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères.
#'
#' @seealso \code{\link{comparaison}}
#'
#' @export
alahausse <- function(x1, x2, seuil = 0.1, lang = get_serad_language()) {

  if (lang == "en") {
    return(comparaison(
      x1, x2,
      hausse_defaut  = "up",
      egalite_defaut = "unchanged",
      baisse_defaut  = "down",
      seuil = seuil
    ))
  }

  comparaison(
    x1, x2,
    hausse_defaut  = "\u00e0 la hausse",
    egalite_defaut = "inchang\u00e9",
    baisse_defaut  = "\u00e0 la baisse",
    seuil = seuil
  )
}


#' Davantage ou moins
#'
#' Indique si \code{x1} est davantage ou moins élevé que \code{x2}.
#'
#' @param x1 Niveau le plus récent.
#' @param x2 Niveau le plus ancien.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères.
#'
#' @seealso \code{\link{comparaison}}
#'
#' @export
davantage <- function(x1, x2, lang = get_serad_language()) {

  if (lang == "en") {
    return(comparaison(
      x1, x2,
      hausse_defaut  = "more",
      egalite_defaut = "as many",
      baisse_defaut  = "fewer",
      seuil = 0
    ))
  }

  comparaison(
    x1, x2,
    hausse_defaut  = "davantage",
    egalite_defaut = "davantage",
    baisse_defaut  = "moins",
    seuil = 0
  )
}


#' Verbe pour exprimer le sens de l'évolution
#'
#' Indique si \code{x1} excède, est au niveau de ou est en dessous de
#' \code{x2}, en tenant compte du nombre grammatical.
#'
#' @param x1 Le niveau le plus récent.
#' @param x2 Le niveau le plus ancien.
#' @param sing Indicateur logique : FALSE si le sujet est pluriel,
#'   TRUE s'il est singulier (par défaut : pluriel).
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères.
#'
#' @seealso \code{\link{comparaison}}
#'
#' @export
depasse <- function(x1, x2, sing = FALSE, lang = get_serad_language()) {

  if (lang == "en") {
    return(comparaison(
      x1, x2,
      hausse_defaut  = "exceed",
      egalite_defaut = "are in line with",
      baisse_defaut  = "are below",
      alt = as.integer(sing),
      hausse_alt  = "exceeds",
      egalite_alt = "is in line with",
      baisse_alt  = "is below"
    ))
  }

  comparaison(
    x1, x2,
    hausse_defaut  = "exc\u00e8dent",
    egalite_defaut = "sont au niveau de",
    baisse_defaut  = "sont en dessous de",
    alt = as.integer(sing),
    hausse_alt  = "exc\u00e8de",
    egalite_alt = "est au niveau de",
    baisse_alt  = "est en dessous de"
  )
}


#' Évolution nominale simple
#'
#' Décrit l'évolution entre \code{x1} et \code{x2}
#' sous forme nominale suivie du pourcentage correspondant.
#'
#' @param x1 Le niveau le plus récent.
#' @param x2 Le niveau le plus ancien.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères.
#'
#' @export
g_nom_simple <- function(x1, x2, lang = get_serad_language()) {

  variation <- g(x1, x2)

  if (lang == "en") {
    z <- comparaison(
      x1, x2,
      hausse_defaut  = "up",
      egalite_defaut = "stable",
      baisse_defaut  = "down"
    )

    return(paste(z, format_g(variation, signe = 0)))
  }

  z <- comparaison(
    x1, x2,
    hausse_defaut  = "en hausse de",
    egalite_defaut = "en hausse",
    baisse_defaut  = "en baisse de"
  )

  paste(z, format_g(variation, signe = 0))
}
