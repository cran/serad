## ----setup, include = FALSE---------------------------------------------------

knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  error = TRUE,
  fig.align = "center"
  # out.width = "75%"
)

library(serad)

## ----presidents_data----------------------------------------------------------
data(presidents)

tps <- time(presidents)

annee <- floor(tps)
trimestre <- round((tps - annee) * 4) + 1
mois <- (trimestre - 1) * 3 + 1

df_pres <- data.frame(
  date = as.Date(paste(annee, mois, 1, sep = "-")),
  taux = as.numeric(presidents)
)

taux_prec <- c(NA, head(df_pres$taux, -1))

df_pres$delta <- df_pres$taux - taux_prec
df_pres$taux_evolution <- round(g(df_pres$taux, taux_prec))

df_pres <- tail(df_pres, 24)

last_row <- nrow(df_pres)
taux_actuel <- df_pres$taux[last_row]

taux_1_an <- df_pres$taux[last_row - 4]
evo_1_an_pts <- taux_actuel - taux_1_an
evo_1_an_pct <- g(taux_actuel, taux_1_an)

taux_debut <- df_pres$taux[1]
evo_mandat_pts <- taux_actuel - taux_debut

moy_mandat <- mean(df_pres$taux, na.rm = TRUE)
min_mandat <- min(df_pres$taux, na.rm = TRUE)
max_mandat <- max(df_pres$taux, na.rm = TRUE)


## ----ukdriver_data------------------------------------------------------------
data(UKDriverDeaths)

start_year  <- start(UKDriverDeaths)[1]
start_month <- start(UKDriverDeaths)[2]

n <- length(UKDriverDeaths)

dates <- seq(
  from = as.Date(paste0(start_year, "-", start_month, "-01")),
  by = "month",
  length.out = n
)

df_driverdeath <- data.frame(
  date = dates,
  deaths = as.numeric(UKDriverDeaths)
)

deaths_prec <- c(NA, head(df_driverdeath$deaths, -1))

df_driverdeath$delta <- df_driverdeath$deaths - deaths_prec
df_driverdeath$taux_evolution <- round(g(df_driverdeath$deaths, deaths_prec), 1)

df_driverdeath$annee <- as.integer(format(df_driverdeath$date, "%Y"))

df_annuel_driverdeath <- aggregate(
  deaths ~ annee,
  data = df_driverdeath,
  FUN = function(x) c(
    deaths_moy_annuelle = round(mean(x, na.rm = TRUE), 1),
    deaths_total = sum(x, na.rm = TRUE)
  )
)

df_annuel_driverdeath <- data.frame(
  annee = df_annuel_driverdeath$annee,
  deaths_moy_annuelle = df_annuel_driverdeath$deaths[, "deaths_moy_annuelle"],
  deaths_total = df_annuel_driverdeath$deaths[, "deaths_total"]
)

deaths_moy_prec <- c(NA, head(df_annuel_driverdeath$deaths_moy_annuelle, -1))

df_annuel_driverdeath$delta_annuel <-
  df_annuel_driverdeath$deaths_moy_annuelle - deaths_moy_prec

df_annuel_driverdeath$taux_evolution_annuel <- round(
  g(df_annuel_driverdeath$deaths_moy_annuelle, deaths_moy_prec),
  1
)

last_row <- nrow(df_annuel_driverdeath)

