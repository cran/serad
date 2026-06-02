#' Documente les contributions à une évolution
#'
#' @description
#' Un dataframe en entrée.\cr
#' Si non indiqué, la date est considérée comme étant la première colonne.
#' Pas besoin de la formater.\cr
#' Si non indiqué, on suppose les valeurs les plus anciennes en haut
#' (temps croissant).\cr
#' Toutes les autres colonnes sont considérées comme les composantes dont les
#' contributions doivent être analysées. Elles doivent être numériques.\cr
#'
#' @details
#' Fonction écrite sur demande d'une utilisatrice.
#'
#' @param df Le dataframe en entrée
#' @param temps "croissant" par défaut si du passé vers le présent. "décroissant" sinon.
#' @param vart La variable de temps du dataframe. La première valeur par défaut.
#' @param Tglissement Sur combien de périodes faut-il remonter pour calculer l'évolution à 1 par défaut.
#' @param seuilpc Un seuil en dessous duquel la contribution (en valeur absolue)
#' n'est pas prise en compte. 5 % par défaut.
#' @param lang Langue de sortie : "fr" ou "en".
#'
#' @details
#' Si aucune contribution n'est supérieure à seuilpc en valeur absolue,
#' contributions() retourne a minima la plus grande contribution de même sens
#' que la contribution totale.
#'
#' @return Un descriptif ordonné des différentes glissements.
#'
#' @examples
#' col0 = c("Y1T1", "Y1T2", "Y1trim3", "Y1T4","Y2T1","Y2-T2")
#' col1 = c( 12,      6,      2,         86,     19,     10)
#' col2 = c(  4,      8,      7,         34,     87,     14)
#' col3 = c( 10,     20,      3,         66,     90,     54)
#' col4 = c( 29,     12,      4,         16,     40,     94)
#' col5 = c( 58,     76,      1,          3,     34,     19)
#' df1 = data.frame(col0,col1,col2,col3,col4,col5)
#'
#' contributions(df1)
#'
#' @export
contributions <- function(df,
                          temps = "croissant",
                          vart,
                          Tglissement = 1,
                          seuilpc = 20,
                          lang = get_serad_language()) {

  df0 <- df

  if (!is.data.frame(df0)) {
    stop("contributions() requiert un dataframe en entr\u00E9e")
  }

  if (ncol(df0) <= 1) {
    stop("le dataframe dans contributions() requiert au moins 2 colonnes")
  }

  if (!temps %in% c("croissant", "decroissant")) {
    stop("temps peut prendre 2 valeurs: croissant ou decroissant")
  }

  # ---- Definition xt / dfy ----
  if (missing(vart)) {
    xt  <- df0[, 1]
    dfy <- df0[, -1, drop = FALSE]
  } else {
    if (!(vart %in% colnames(df0))) {
      stop("vart doit etre un nom de variable de df")
    }
    xt  <- df0[, vart]
    dfy <- df0[, -which(names(df0) == vart), drop = FALSE]
  }

  if (temps == "decroissant") {
    xt  <- rev(xt)
    dfy <- dfy[nrow(dfy):1, , drop = FALSE]
  }

  # ---- Forcer numerique proprement ----
  dfy <- as.data.frame(lapply(dfy, function(x) {
    if (is.factor(x)) x <- as.character(x)
    as.numeric(x)
  }))

  if (!all(sapply(dfy, is.numeric))) {
    stop("Au moins une des s\u00E9ries composantes n'est pas num\u00E9rique!")
  }

  if (!is.numeric(Tglissement) || length(Tglissement) != 1 ||
      Tglissement < 1 ||
      abs(Tglissement - round(Tglissement)) > .Machine$double.eps) {
    stop("Tglissement doit etre un entier >= 1")
  }

  if (nrow(dfy) <= Tglissement) {
    stop("Pas assez de periodes pour calculer le glissement")
  }

  # ---- Selection des periodes ----
  idx1 <- nrow(dfy) - Tglissement
  idx2 <- nrow(dfy)

  xt  <- xt[c(idx1, idx2)]
  dfy <- dfy[c(idx1, idx2), , drop = FALSE]

  # ---- Calcul DELTA ----
  total1 <- sum(dfy[1, ], na.rm = TRUE)
  total2 <- sum(dfy[2, ], na.rm = TRUE)

  DELTA <- total2 - total1
  if (DELTA == 0) DELTA <- .Machine$double.eps

  # ---- Contributions ----
  delta_comp <- unlist(dfy[2, ]) - unlist(dfy[1, ])
  Adfy <- 100 * delta_comp / DELTA
  names(Adfy) <- colnames(dfy)

  Adfy <- sort(Adfy, decreasing = TRUE)

  # ---- Seuil ----
  Adfy_all <- Adfy
  Adfy <- Adfy[abs(Adfy) > seuilpc]

  if (length(Adfy) == 0) {
    signe_total <- sign(DELTA)

    if (signe_total > 0) {
      candidats <- Adfy_all[Adfy_all > 0]
    } else if (signe_total < 0) {
      candidats <- Adfy_all[Adfy_all < 0]
    } else {
      candidats <- Adfy_all
    }

    if (length(candidats) == 0) {
      Adfy <- Adfy_all[which.max(abs(Adfy_all))]
    } else {
      Adfy <- candidats[which.max(abs(candidats))]
    }
  }

  # ---- Formatage ----
  Bdfy <- sapply(Adfy, function(x) format_g(x, lang = lang))

  # ---- Texte ----
  if (lang == "en") {
    A <- paste0(
      names(Adfy),
      " accounts for ",
      Bdfy,
      " of the change between ",
      xt[1],
      " and ",
      xt[2],
      collapse = "; "
    )
    return(paste0(A, "."))
  }

  A <- paste0(
    names(Adfy),
    " contribue pour ",
    Bdfy,
    " \u00E0 l'\u00E9volution entre ",
    xt[1],
    " et ",
    xt[2],
    collapse = " ; "
  )

  paste0(A, " ; ")
}
