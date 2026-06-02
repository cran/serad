#' Comparaison d'une variation à un seuil
#'
#' @param g Variation exprimée en pourcentage
#'   (5 signifie 5 %, 0.1 signifie 0.1 %).
#' @param hausse_defaut Mot si hausse (forme par défaut).
#' @param egalite_defaut Mot si égalité (forme par défaut).
#' @param baisse_defaut Mot si baisse (forme par défaut).
#' @param seuil Limite pour l'égalité (0.1 par défaut).
#' @param alt Paramètre supplémentaire égal à 0 ou 1
#'   (par exemple pour distinguer singulier/pluriel).
#' @param hausse_alt Formulation alternative si alt = 1.
#' @param egalite_alt Formulation alternative si alt = 1.
#' @param baisse_alt Formulation alternative si alt = 1.
#'
#' @details
#' Fonction interne utilisée par \code{\link{comparaison}}.
#'
#' @return Une chaîne de caractères correspondant à la modalité choisie.
#'
#' @seealso \code{\link{comparaison}}
#'
#' @examples
#' comparaison_taux(5, "augmente", "reste stable", "diminue")
#' comparaison_taux(0.05, "augmente", "reste stable", "diminue")
#' comparaison_taux(0, "as", "bs", "cs", seuil = 0, alt = 1,
#'                  hausse_alt = "a", egalite_alt = "b", baisse_alt = "c")
#'
#' @export
comparaison_taux <- function(g,
                             hausse_defaut, egalite_defaut, baisse_defaut,
                             seuil = 0.1,
                             alt = 0,
                             hausse_alt = hausse_defaut,
                             egalite_alt = egalite_defaut,
                             baisse_alt = baisse_defaut) {

  if (g >= seuil) {
    mot_defaut <- hausse_defaut
    mot_alt    <- hausse_alt
  } else if (g <= -seuil) {
    mot_defaut <- baisse_defaut
    mot_alt    <- baisse_alt
  } else {
    mot_defaut <- egalite_defaut
    mot_alt    <- egalite_alt
  }

  if (alt == 0) mot_defaut else mot_alt
}
