library(tidyverse)
library(quanteda)
library(stm)
library(tidytext)
library(janitor)
library(lsa)
library(dbscan)
library(tidyr)

# ladataan apufunktiot
source("topic_clustering_functions.R")

# ladataan tiedosto, jossa on lista aihemalleista
stemlem_analysis1_08032021 <- readRDS("stemlem_analysis2_09032021.RDS")

# ladataan analyysissä käytetyt sanastot
uusi_energia_all_vocabs <- readRDS("uusi_energia_all_vocabs.RDS")

# muutetaan lista malleista data.frame-muotoon, siistitään ja lisätään sanaston mukainen nimi
stem_lem_models <- tibble(run=c(1:150), topic_model=stemlem_analysis1_08032021) 
stem_lem_models_all <- make_per_model_df_with_tidy_and_top_terms(stem_lem_models)
stem_lem_models_all <- stem_lem_models_all %>% mutate(K = map_dbl(topic_model, function(a) a$settings$dim$K),
                                    vocab = map_dbl(topic_model, function(a) a$settings$dim$V),
                                    type_of_model = case_when(vocab == 3643 ~ "term", vocab == 2115 ~ "stem", vocab == 1542 ~ "lemma"))

# tarkistus
stem_lem_models_all %>% tabyl(K, type_of_model)
# K lemma stem term
# 7    10   10   10
# 10    10   10   10
# 15    10   10   10
# 20    10   10   10
# 30    10   10   10

# tehdään aihekohtainen data frame
stem_lem_models_per_topic <- make_per_topic_df(stem_lem_models_all) %>% mutate(K_run_topic = paste(type_of_model, K_run_topic, sep="_"))


# sanastot pitää yhdistää, jotta ne voidaan klusteroida. 
# Tehdään tätä varten sanasto, jossa kaikki kutakin lemmaa aineistossa vastanneet käsitteet ja stemmat.
lemma_dictionary <- uusi_energia_all_vocabs %>% select(lemma, stem, term) %>% 
  pivot_longer(c(stem, term), names_to="type", values_to="value") %>% distinct()
just_lemmas <- lemma_dictionary %>% select(lemma) %>% mutate(type="lemma", value=lemma) %>% distinct()
lemma_dictionary <- lemma_dictionary %>% bind_rows(just_lemmas) %>% distinct(lemma, value)
unique_lemmas <- unique(lemma_dictionary$lemma)
lemma_dictionary <- lemma_dictionary[!duplicated(lemma_dictionary$value),]
# tarkistus
lemma_dictionary %>% filter(lemma=="sähkö")

# funktio laskee jokaiselle lemmalle todenäköisyyksien summan kaikista käsitteen muodoista
calculate_per_lemma_sum_of_prob <- function(lemma_to_lookup){
  values_for_this_lemma <- lemma_dictionary %>% filter(lemma==lemma_to_lookup)
  probabilities <- stem_lem_models_per_topic %>% select(one_of(values_for_this_lemma$value)) %>% 
     rowSums(na.rm = T) %>% as.data.frame()
  colnames(probabilities) <- lemma_to_lookup 
  probabilities
}

# lasketaan kaikkien lemmojen mukaiset todennäköisyydet kullekin aiheelle
combined_vocabulary_stem_lem_per_topic <- stem_lem_models_per_topic %>% select(starts_with("K_run"))
for (lemma in unique_lemmas) {
  result <- calculate_per_lemma_sum_of_prob(lemma)
  combined_vocabulary_stem_lem_per_topic <- bind_cols(combined_vocabulary_stem_lem_per_topic, result)
}

# tuloksena:  2460 topic rows, K_run_topic + 1542 lemma columns
# split K_run_topic to type_K
combined_vocabulary_stem_lem_per_topic <- combined_vocabulary_stem_lem_per_topic %>% mutate(K = str_extract(K_run_topic, "(?<=_)(.*?)(?=_)"))

# klusterointi tehdään erikseen kullekin aihemäärälle, jotene yhdistetään nämä aihekohtaiset oikealla tavalla
df_as_per_list <- combined_vocabulary_stem_lem_per_topic %>%
  select(-K) %>%
  pivot_longer(cols=!(one_of("K_run_topic"))) %>%
  pivot_wider(names_from=K_run_topic, values_from=value) %>% 
  split.default(., str_extract(names(.), "(?<=_)(.*?)(?=_)"))
tidy_per_K <- tibble(K = names(df_as_per_list), frames_for_clustering = df_as_per_list)

# Lisätään aiheiden väliset kosinin samankaltaisuudet
stem_lem_models_K <- tidy_per_K  %>% add_cosine_distance()

# tehdään klusterointi erilaisilla klusterin minimikoko määrittävillä parametreilla
stem_lem_models_K_repeats <- do_clustering_by_split_at_varying_minPts(stem_lem_models_K, vardef="K", minPts = c(3:22))

# sopivaa minimikokoparametriä määrittää klusterien määrän ja klusteroimattomien pisteiden yhteys - molemmat ovat toivottavasti verrattain pieniä
stem_lem_models_K_repeats %>% filter(K=="7") %>% 
  ggplot(aes(number_of_clusters, noise_points, label=as.factor(minPts))) + geom_label(aes(fill=K))


# valitaan kullekin klusteroidulle ryhmälle sopivan minimikokoparametrin malli
chosen_models <- stem_lem_models_K_repeats %>% filter(K=="7" & minPts==5 |
                                                      K=="10" & minPts==6 |
                                                      K=="15" & minPts==6 |
                                                      K=="20" & minPts==5 |
                                                      K=="30" & minPts==5)


# kaikki aiheet ja klusterit samaan data frameen
models_and_clusters <-  chosen_models %>% add_clusters_as_df() %>% 
  mutate(lemma_K_run_topic = K_run_topic,
         K_run_topic = str_replace(K_run_topic, "^.*?_", ""))

# lasketaan jokaisen klusterin yleisimmät sanat tulkintaa varten
combined_vocabulary_stem_lem_per_topic_long <- combined_vocabulary_stem_lem_per_topic %>% select(-K) %>% pivot_longer(!K_run_topic, names_to="term", values_to="beta")
stem_lem_term_distribution_by_group <- combined_vocabulary_stem_lem_per_topic_long %>%
  left_join(models_and_clusters, by=c("K_run_topic" = "lemma_K_run_topic")) %>%
  mutate(K = str_extract(K_run_topic.y, "[^_]+")) %>%
  group_by(K, value, term) %>% summarize(sum_beta = sum(beta)) %>% 
  arrange(desc(sum_beta)) %>% top_n(10, sum_beta) %>% 
  summarise(terms = list(term)) %>% mutate(top_terms = sapply(terms, toString)) %>% select(-terms) 


# tehdään aihekohtainen data.frame, jossa kunkin aiheen perustiedot, yleisimmät käsitteet sekä klusteri
stem_lem_models_top_terms <- get_top_terms(stem_lem_models_all)
stem_lem_models_final <- combine_clusters_and_top_terms(models_and_clusters, stem_lem_models_top_terms) %>% 
  mutate(type_K = str_extract(lemma_K_run_topic, "[^_]*_[^_]*"), type_K_clust = paste(type_K, value, sep="_"))

# Tehdään taulukko, jossa klusterit, siihen kuuluvien aiheiden määrät
stem_lem_term_distribution_by_group <- stem_lem_term_distribution_by_group %>% mutate(K = as.numeric(K))
all_models_grouped <- add_per_cluster_variables_and_labels(stem_lem_models_final, grouping_variable="K", resolved_threshold = 3) %>%
  left_join(stem_lem_term_distribution_by_group, by=c("grouping"="K", "value"="value")) %>% 
  mutate(tyyppi = fct_recode(cluster_label, "Roska-aiheet"= "Junk", "Ydinaihe"="Resolved", "Yhdistelmä"="Fused", "Näkökulma"="Semi-resolved") %>%
           fct_relevel("Roska-aiheet", "Ydinaihe","Näkökulma", "Yhdistelmä")) %>%
  arrange(grouping, tyyppi)

# Lisätään kustakin perusmuotoistamisen tyypistä jokaiseen klusteriin päätyneiden aiheiden määrät

stem_lem_term_per_cluster_counts <- stem_lem_models_final %>% mutate(type = str_extract(type_K, "[^_]+")) %>% group_by(K,value, type) %>% summarize(n=n()) %>%
  pivot_wider(names_from = type, values_from=n)
stem_lem_clusters_final <- all_models_grouped %>% left_join(stem_lem_term_per_cluster_counts, by=c("grouping"="K", "value"="value"))


# Tallennetaan varsinainen tulostaulukko, 75 klusteria x 12 muuttujaa
stem_lem_clusters_final %>% write.csv2("combined_analysis_clusters_25032021.csv")

# Tallentaan kaikki aiheita koskevat taulukko, 2460 aihetta x 10 muuttujaa
stem_lem_models_final %>% select(-terms) %>% write.csv2("combined_analysis_allmodels_25032021.csv")






