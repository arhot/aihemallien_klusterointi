library(digest)
library(tidyverse)
library(rlist)
library(quanteda)
library(pipeR)
library(tidytext)
library(SnowballC)



library(stringr)
library(tidyr)
library(tm)
library(topicmodels)
library(extdplyr)
library(dplyr)
library(stm)
library(ldatuning)
library(stm)
library(furrr)
library(lsa)

library(janitor)
library(dbscan)

kaikkipostitUusienergiapolitiikka2017 <- readRDS("C:/Users/atoikka/OneDrive - University of Helsinki/analyyseja/new_energy_policy_network/kaikkipostit_dumppi.RDS")

post_id <- kaikkipostitUusienergiapolitiikka2017 %>% map(c("post", "id"))
aloitusviesti <- kaikkipostitUusienergiapolitiikka2017 %>% map(c("post", "message"))
kommentit <- kaikkipostitUusienergiapolitiikka2017 %>% map(c("comments", "message"))
replies <- kaikkipostitUusienergiapolitiikka2017 %>% map(c("replies", "message"))


a <- list()
for (i in 1:length(kaikkipostitUusienergiapolitiikka2017)) {a <- list.append(a, unlist(c(aloitusviesti[i], kommentit[i], replies[i])))}
topic_model_kaikki_postit <- data.frame(Var1 = 1:length(kaikkipostitUusienergiapolitiikka2017), postid=flatten_chr(post_id), viestit = matrix(a))

vdigest <- Vectorize(digest)
topic_model_kaikki_postit <- topic_model_kaikki_postit %>%
  mutate(teksti = sapply(viestit, toString)) %>% select(-viestit) %>% 
  mutate(id = vdigest(postid)) %>% select(-postid)


saveRDS(topic_model_kaikki_postit, "topicmodelpostit_dumppi.RDS")
