#' Évolution nominale d'un taux
#'
#' @description
#' Décrit une évolution exprimée en pourcentage sous forme nominale
#' (par exemple : "une forte hausse").
#'
#' @param g L'évolution en pourcentage.
#' @param titre Indicateur logique : TRUE pour supprimer l'article
#'   initial et mettre une majuscule, notamment en début de titre.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères décrivant l'évolution
#' (par exemple : "une forte hausse", "une stabilité").
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
#' Une catégorie est considérée comme une stabilité si sa condition est
#' également vérifiée pour `g = 0`.
#'
#' La fonction renvoie ensuite la colonne `nom` correspondante.
#' Si `titre = TRUE`, l'article initial est supprimé et la première lettre
#' restante est mise en majuscule.
#'
#' @section Personnalisation:
#' Les formulations utilisées par cette fonction proviennent de la table
#' `getOption("serad")$evo_simple`.
#'
#' Pour modifier les conditions ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @examples
#' g_nom_taux(4)
#' g_nom_taux(1)
#' g_nom_taux(0.4)
#' g_nom_taux(0.1)
#' g_nom_taux(0)
#' g_nom_taux(-0.1)
#' g_nom_taux(-0.3)
#' g_nom_taux(-1)
#' g_nom_taux(-4)
#' g_nom_taux(-5)
#'
#' @seealso
#' \code{\link{g_verbe_taux}},
#' \code{\link{init_serad}}
#'
#' @export
g_nom_taux <- function(g, titre = FALSE, lang = get_serad_language()) {

  serad0 <- getOption("serad")

  if (is.null(serad0) || is.null(serad0$evo_simple)) {
    stop("Les options serad ne sont pas initialis\u00E9es. Utiliser init_serad_fr() ou init_serad_en().")
  }

  tab <- serad0$evo_simple

  if (!is.data.frame(tab)) {
    stop("serad$evo_simple doit \u00EAtre une data.frame.")
  }

  cols_attendues <- c("condition", "nom")
  if (!all(cols_attendues %in% names(tab))) {
    stop("serad$evo_simple doit contenir : condition, nom.")
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

  # ---- stabilit\u00E9 ----
  est_stable <- isTRUE(
    eval(parse(text = tab$condition[i]), envir = list(g = 0))
  )

  res <- as.character(tab$nom[i])

  if (titre) {
    if (lang == "en") {
      res <- sub("^(a|an|the)\\s+", "", res, ignore.case = TRUE)
    } else {
      res <- sub("^(une|un|des|la|le|les|du|de la|de l'|d'|l')\\s*", "", res, ignore.case = TRUE)
    }

    res <- paste0(
      toupper(substr(res, 1, 1)),
      substr(res, 2, nchar(res))
    )
  }

  res
}
