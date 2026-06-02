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
#' `getOption("serad")$evo_simple`, la première ligne dont
#' le seuil est strictement inférieur à `g`.
#'
#' Elle renvoie ensuite la colonne `nom` correspondante.
#' Si `titre = TRUE`, l'article initial est supprimé et la
#' première lettre restante est mise en majuscule.
#'
#' @section Personnalisation:
#' Les formulations utilisées par cette fonction proviennent de la table
#' `getOption("serad")$evo_simple`.
#'
#' Pour modifier les seuils ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @examples
#' g_nom_taux(4)
#' g_nom_taux(1)
#' g_nom_taux(0.4)
#' g_nom_taux(0.1)
#' g_nom_taux(0)
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

  cols_attendues <- c("seuil", "nom")
  if (!all(cols_attendues %in% names(tab))) {
    stop("serad$evo_simple doit contenir : seuil, nom.")
  }

  i <- which(g > tab$seuil)[1]
  if (is.na(i)) i <- nrow(tab)

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
