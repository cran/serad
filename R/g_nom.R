#' Évolution nominale non suivie d'une valeur
#'
#' @description
#' Décrit une évolution sous forme nominale à partir de deux niveaux,
#' sans ajouter la valeur de variation.
#'
#' @param x1 Le niveau le plus récent.
#' @param x2 Le niveau le plus ancien.
#' @param evolution Type d'évolution :
#'   `"pourcents"` (variation relative, par défaut) ou `"points"`.
#' @param titre Indicateur logique : TRUE pour supprimer l'article
#'   initial et mettre une majuscule, notamment en début de titre.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @return
#' Une chaîne de caractères correspondant à la formulation nominale
#' retenue (par exemple : "une forte hausse").
#'
#' @details
#' La fonction calcule d'abord une évolution à partir de `x1`
#' et `x2` :
#' \itemize{
#'   \item si `evolution = "pourcents"`, elle utilise
#'   \code{\link{g}} ;
#'   \item si `evolution = "points"`, elle calcule `x1 - x2`.
#' }
#'
#' La valeur obtenue est ensuite transmise à \code{\link{g_nom_taux}},
#' qui détermine la formulation à partir de la table
#' `getOption("serad")$evo_simple`.
#'
#' @section Personnalisation:
#' Les formulations utilisées par cette fonction proviennent de la table
#' `getOption("serad")$evo_simple`.
#'
#' Pour modifier les seuils ou les libellés, voir
#' \code{\link{init_serad}}.
#'
#' @seealso
#' \code{\link{g_nom_taux}},
#' \code{\link{g}},
#' \code{\link{init_serad}}
#'
#' @examples
#' g_nom(1.04, 1)
#' g_nom(1.01, 1)
#' g_nom(1.004, 1)
#' g_nom(1.001, 1)
#' g_nom(1, 1)
#' g_nom(0.997, 1)
#' g_nom(0.95, 1)
#' g_nom(0.95, 1, evolution = "points")
#'
#' @export
g_nom <- function(x1, x2,
                  evolution = c("pourcents", "points"),
                  titre = FALSE,
                  lang = get_serad_language()) {

  evolution <- match.arg(evolution)

  valeur <- switch(
    evolution,
    pourcents = serad::g(x1, x2),
    points    = x1 - x2
  )

  g_nom_taux(
    g = valeur,
    titre = titre,
    lang = lang
  )
}
