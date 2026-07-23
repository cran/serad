#' Évolution verbale d'un taux
#'
#' @description
#' Décrit une évolution sous forme verbale, sans tenir compte
#' d'une éventuelle accélération, et suivie de la valeur formatée.
#'
#' @param g L'évolution.
#' @param sing Indicateur logique : TRUE si le sujet est singulier
#'   (par défaut), FALSE sinon.
#' @param evolution Type d'évolution :
#'   `"pourcents"` (variation relative) ou `"points"`.
#' @param stable_sans_valeur Indicateur logique : TRUE (par défaut)
#'   pour ne rien ajouter après une stabilité.
#'   Si FALSE, ajoute la valeur après la formulation de stabilité.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères décrivant l'évolution.
#'
#' @details
#' La fonction sélectionne, dans la table
#' `getOption("serad")$evo_simple`, la ligne dont la condition est vérifiée
#' par la valeur de `g`.
#'
#' La table `evo_simple` doit contenir une colonne `condition`, composée de
#' chaînes de caractères évaluables par R, par exemple :
#' `"g >= -0.10 & g <= 0.10"`.
#'
#' Les conditions doivent être disjointes : pour une valeur donnée de `g`,
#' une seule condition doit être vraie.
#'
#' Si `sing = TRUE`, la fonction renvoie la colonne `verbe_sing`.
#' Sinon, elle renvoie `verbe_plur`.
#'
#' @section Personnalisation :
#' Les formulations utilisées par cette fonction proviennent de la table
#' `getOption("serad")$evo_simple`.
#'
#' Pour modifier les conditions ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @seealso
#' \code{\link{g_verbe}},
#' \code{\link{format_g}},
#' \code{\link{format_pts}},
#' \code{\link{init_serad}}
#'
#' @examples
#' g_verbe_taux(10)
#' g_verbe_taux(0.1)
#' g_verbe_taux(-0.1)
#' g_verbe_taux(-0.1, stable_sans_valeur = FALSE)
#'
#' @export
g_verbe_taux <- function(g,
                         sing = TRUE,
                         evolution = c("pourcents", "points"),
                         stable_sans_valeur = TRUE,
                         lang = get_serad_language()) {

  evolution <- match.arg(evolution)

  serad0 <- getOption("serad")

  if (is.null(serad0) || is.null(serad0$evo_simple)) {
    stop("Les options serad ne sont pas initialis\u00E9es. Utiliser init_serad_fr() ou init_serad_en().")
  }

  tab <- serad0$evo_simple

  # ---- checks coh\u00E9rents ----
  if (!is.data.frame(tab)) {
    stop("serad$evo_simple doit \u00EAtre une data.frame.")
  }

  cols_attendues <- c("condition", "verbe_sing", "verbe_plur")
  if (!all(cols_attendues %in% names(tab))) {
    stop("serad$evo_simple doit contenir : condition, verbe_sing, verbe_plur.")
  }

  # ---- s\u00E9lection ----
  test_conditions <- vapply(
    tab$condition,
    function(condition) {
      eval(parse(text = condition), envir = list(g = g))
    },
    logical(1)
  )

  i <- which(test_conditions)

  if (length(i) == 0) {
    stop("Aucune cat\u00E9gorie trouv\u00E9e pour g = ", g, call. = FALSE)
  }

  if (length(i) > 1) {
    stop(
      "Plusieurs cat\u00E9gories trouv\u00E9es pour g = ",
      g,
      ". Les conditions de serad$evo_simple ne sont pas disjointes.",
      call. = FALSE
    )
  }

  verbe <- if (sing) {
    as.character(tab$verbe_sing[i])
  } else {
    as.character(tab$verbe_plur[i])
  }

  # ---- stabilit\u00E9 ----
  est_stable <- isTRUE(
    eval(parse(text = tab$condition[i]), envir = list(g = 0))
  )

  # ---- valeur ----
  format_fun <- if (evolution == "pourcents") format_g else format_pts

  val <- if (g < 0 && est_stable) {
    format_fun(g, signe = TRUE, lang = lang)
  } else {
    format_fun(g, signe = FALSE, lang = lang)
  }

  # ---- rendu ----
  if (est_stable && stable_sans_valeur) {
    verbe <- sub("\\s+\u00E0$", "", verbe)
    verbe <- sub("\\s+at$", "", verbe)

    return(verbe)
  }

  paste(verbe, val)
}
