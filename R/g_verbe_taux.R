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
#' `getOption("serad")$evo_simple`, la première ligne dont
#' le seuil est strictement inférieur à `g`.
#'
#' Si `sing = TRUE`, la fonction renvoie la colonne `verbe_sing`.
#' Sinon, elle renvoie `verbe_plur`.
#'
#' @section Personnalisation :
#' Les formulations utilisées par cette fonction proviennent de la table
#' `getOption("serad")$evo_simple`.
#'
#' Pour modifier les seuils ou les libellés, voir
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
#' g_verbe_taux(-0.1)
#'
#' @export
g_verbe_taux <- function(g,
                         sing = TRUE,
                         evolution = c("pourcents", "points"),
                         stable_sans_valeur = TRUE,
                         lang = get_serad_language()) {

  evolution <- match.arg(evolution)

  serad0 <- getOption("serad")
  tab <- serad0$evo_simple

  # ---- checks cohérents ----
  if (!is.data.frame(tab)) {
    stop("serad$evo_simple doit \u00EAtre une data.frame.")
  }

  cols_attendues <- c("seuil", "verbe_sing", "verbe_plur")
  if (!all(cols_attendues %in% names(tab))) {
    stop("serad$evo_simple doit contenir : seuil, verbe_sing, verbe_plur.")
  }

  # ---- séléction ----
  i <- which(g > tab$seuil)[1]
  if (is.na(i)) i <- nrow(tab)

  verbe <- if (sing) {
    as.character(tab$verbe_sing[i])
  } else {
    as.character(tab$verbe_plur[i])
  }

  # ---- stabilité ----
  verbes_stables <- if (lang == "en") {
    c("is stable", "are stable")
  } else {
    c("est stable", "sont stables")
  }

  est_stable <- verbe %in% verbes_stables

  # ---- valeur ----
  format_fun <- if (evolution == "pourcents") format_g else format_pts

  val <- if (g < 0 && est_stable) {
    format_fun(g, signe = TRUE, lang = lang)
  } else {
    format_fun(g, signe = FALSE, lang = lang)
  }

  # ---- rendu ----
  if (est_stable && stable_sans_valeur) {
    return(verbe)
  }

  if (est_stable) {
    lien <- if (lang == "en") "at" else "\u00e0"
    return(paste(verbe, lien, val))
  }

  paste(verbe, val)
}
