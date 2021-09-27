# Tämä tiedosto toistaa aihemallinnuksen artikkelissa "Aihemallinnuksen ja klusterianalyysin yhdistäminen aineiston esikäsittelyn ja mallinnuksen valintojen tutkimiseksi" 
# esitetystä analyysista. 

# Tiedosto on kirjoitettu CSC:n palvelimella batch job-suoritettavaksi.
# Yhdellä työasemalla sama analyysi onnistuu vaihtamalla clusterMap map-perheen komentoon ja poistamalla cluster-komennota.
# CSC:n palvelumella ajot suoritetaan batch-tiedostolla stemma_lemma_models_with_snow.sh


.libPaths(c("[path]", .libPaths()))
library(tidyverse)
library(snow)
library(stm)

uusi_energia_term_dfm_08032021 <- readRDS("uusi_energia_term_dfm_08032021.RDS")
uusi_energia_stem_dfm_08032021 <- readRDS("uusi_energia_stem_dfm_08032021.RDS")
uusi_energia_lemma_dfm_08032021 <- readRDS("uusi_energia_lemma_dfm_08032021.RDS")

dfms <- list(uusi_energia_term_dfm_08032021, uusi_energia_stem_dfm_08032021, uusi_energia_lemma_dfm_08032021)
ks <- rep(c(7, 7, 7, 10, 10, 10, 15, 15, 15, 20, 20, 20, 30, 30, 30), 10)

do_topic_model <- function(df, k) {
  stm(df, K=k, verbose=F, init.type="LDA")
}

cl <- getMPIcluster()
# one heldout for all
clusterEvalQ(cl, .libPaths(c("[path]", .libPaths())))
clusterEvalQ(cl, library(stm))
clusterExport(cl, "dfms")
clusterExport(cl, "ks")
clusterExport(cl, "do_topic_model")


stemlem_analysis <- clusterMap(cl, function(dfms, ks) do_topic_model(dfms, ks), dfms, ks)

saveRDS(stemlem_analysis, "stemlem_analysis2_09032021.RDS")

stopCluster(cl)
