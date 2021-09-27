# Tämä tiedosto toistaa esikäsittelyn artikkelissa "Aihemallinnuksen ja klusterianalyysin yhdistäminen aineiston esikäsittelyn ja mallinnuksen valintojen tutkimiseksi" 
# esitetystä analyysista. 

# Raaka-aineisto ei ole anonymisoitavissa, koska se on edelleen jaossa julkisessa internetissä, ja tämän takia sitä ei jaeta tiedoston kanssa. 
# Tiedosto toimii siis vain esimerkkinä.

# Koodi olettaa, että käsiteltävä teksti on dataframessa, jossa on muuttuja nimeltä teksti, jossa on kunkin tarkasteltavan dokumentin koko teksti
# tässä tapauksessa teksti on siis keskusteluketju.

library(tidyverse)
library(rlist)
library(quanteda)
library(pipeR)
library(tidytext)
library(SnowballC)

topic_model_kaikki_postit <- readRDS("topicmodelpostit_dumppi.RDS")

# tokenisoidaan keskustelut. 
n_grams <- 1
by_words_kaikki_postit <- map_df(n_grams, ~ topic_model_kaikki_postit %>%
                         unnest_tokens(output = word,
                                       input = teksti,
                                       token = "ngrams",
                                       n = .x) %>%
                         mutate(ngram = .x,
                                token_id = row_number()) %>%
                         drop_na(word))

# Aineistossa kokonaisuudessaan sanoja / uniikkeja
nrow(by_words_kaikki_postit)
length(unique(by_words_kaikki_postit$word))
# 3764589 total / 350711 unique

# Poistetaan numerot, välimerkit ja alle kahden merkin mittaiset sanat
by_words_kaikki_postit <- by_words_kaikki_postit %>%
  filter(!str_detect(word, "^[0-9]*$")) %>%  
  filter(!str_detect(word, "[:punct:]")) %>%
  filter(nchar(word)>2)
nrow(by_words_kaikki_postit)
length(unique(by_words_kaikki_postit$word))
# total 3262145 / unique 330216

# Luodaan poistosanalista, jossa on a) yleisesti käytetty lista suomen kielen poistosanoista, b) eräitä lyhenteitä ja yleisiä sanoja, c) joukko englanninkielisiä termejä, joita toistuvasti keskustelussa lainattiin 
# Aineistokohtainen lista on tehty manuaalisessa käsitelistan tarkastelussa (ja on nyt osin päällekäinen muiden poistojen kanssa)
# Suurin osa sanoista on englannin kielen sanoja muutamasta raportista, joita lainattiin keskusteluissa ahkerasti. 
# Automaattinen kielentunnistus poisti liian aggressiivisesti keskustelun suomenkielistä slangia, joten sanasto karsittiin manuaalisesti.

stopwordsFI <- stopwords::stopwords("fi", source="stopwords-iso") %>% append(c("mw", "gw", "oy", "the", "sen", "sitä", "mwh", "gwh", "tw", "twh", "esim", "in", "to",
                                                                               "jo", "voi", "nb", "at", "http", "https", "eli", "vain", "myös",
                                                                               "a", "of", "siis", "kyllä", "as", "by", "b",
                                                                               "vielä", "sitten", "ihan", "miten", "edes", 
                                                                               "saada", "pitää","tulla", "voida",
                                                                               "energy", "that", "from", "solar", "nuclear",
                                                                               "news", "will", "Will", "with", "climate",
                                                                               "china", "reactor","coal","project","capacity", "demand","scale",
                                                                               "change", "only", "plants", "also", "system", "this", "have", "point",
                                                                               "were", "countries", "could","groups", "view", "been", "said", "files",
                                                                               "cars", "says", "should", "articles", "source", "uploads", "start",
                                                                               "long", "large", "price", "content", "these", "there", "much", "when",
                                                                               "edit", "sites", "their", "share", "company", "after", "most", "next",
                                                                               "More", "wind", "renewable", "about", "carbon", "than", "world", "global",
                                                                               "policy", "which", "they", "than", "industry", "would", "costs",
                                                                               "article", "year", "cost", "first", "HTML", "years", "storage", "plant",
                                                                               "over", "technology", "even", "fossil", "generation", "national", "growth",
                                                                               "future", "report", "market", "other", "reactors")) %>% as.data.frame()

colnames(stopwordsFI) <- c("word")

# Poistetaan poistosanat
words_counted <- by_words_kaikki_postit %>%
    anti_join(stopwordsFI) %>% 
  count(id, word, sort=T) 

# Poistetaan sanat, jotka esiintyvät alle 1% keskusteluista
uusi_energia_dfm <- words_counted %>%
  cast_dfm(id, word, n) %>%
  dfm_trim(sparsity = 0.99)
uusi_energia_dfm <- uusi_energia_dfm %>% 
  dfm_subset(rowSums(uusi_energia_dfm) > 0)

# Erilaisilla rajauksilla syntyy erilainen määrä käsitteitä ja erilainen määrä keskusteluja:
# Vähintään 1% keskusteluista: 6786 documents, 4078 features
# Vähintään 0.5% keskusteluista: 6975, 8970 features
# Vähintään 0.1% keskusteluista: 6873, 32317 features

# --> valitaan 1%, jossa laskennalliset vaatimukset tasapainossa yksityiskohtaisuuden kanssa

# Tallentaan esikäsitelty sanasto siistissä muodossa.
uusi_energia_tidy <- tidy(uusi_energia_dfm)
write.csv(uusi_energia_tidy, "uusi_energia_data_frame_for_lemmatization.csv", fileEncoding="utf-8")

# Lemmatisoidaan sanasto pythonissa, tiedosto lemmatization_from_dataframe.ipynb

# Luetaan lemmatisoitu tiedosto
uusi_energia_tidy <- read_csv("C:/Users/atoikka/OneDrive - University of Helsinki/analyyseja/python_projects/worddata_lemmatized_v2.csv")

# Stemmataan ja siistitään aineisto
uusi_energia_tidy <- uusi_energia_tidy %>%
  mutate(stem = wordStem(term, "finnish")) %>%
  mutate(lemma = as_factor(lemma) %>% droplevels() %>% tolower())

# Tallentaan aineisto manuaaliseen tarkastukseen
# sanasto_tarkastukseen <- distinct(uusi_energia_tidy, term, .keep_all = TRUE)
# write.csv2(sanasto_tarkastukseen, "uusi_energia_for_final_review.csv")

# Manuaalisessa tarkastuksessa tehtiin kahdenlaisia korjauksia: lisättiin poistosanaksi tai korjattiin lemma
sanasto_tarkastettu <- read.csv2("uusi_energia_for_final_review_done.csv", na.strings="") %>% select(-X, -X.1, -Unnamed..0, -count, -document)

# manuaalisesti lisätyt poistosanat poistetaan ja korjataan lemmatisoinnin virheet
uusi_energia_tidy_check <- uusi_energia_tidy %>% select(-X1, -'Unnamed: 0') %>% 
  left_join(sanasto_tarkastettu, by=c("term", "lemma", "stem")) %>% 
  filter(is.na(add_stopword)) %>%
  mutate(lemma = ifelse(is.na(fix_lemma), lemma, as.character(fix_lemma))) %>%
  anti_join(stopwordsFI, by=c("lemma"="word"))

# lopullisessa sanastossa on 3643 uniikkia termijä, 2115 uniikkia stemmaa, ja 1542 uniikkia lemmaa
length(unique(uusi_energia_tidy_check$term)) #3643 
length(unique(uusi_energia_tidy_check$stem)) #2115
length(unique(uusi_energia_tidy_check$lemma))  # 1542


# Tallennetaan koko sanasto ja lisäksi erilliset document-feature -matriisit kustakin esikäsittelystä.

saveRDS(uusi_energia_tidy_check, "uusi_energia_all_vocabs.RDS")

uusi_energia_term_dfm <- uusi_energia_all_vocabs %>%
  cast_dfm(document, term, count)
# saveRDS(uusi_energia_term_dfm, "uusi_energia_term_dfm_08032021.RDS")

uusi_energia_stem_dfm <- uusi_energia_all_vocabs %>%
  cast_dfm(document, stem, count)
# saveRDS(uusi_energia_stem_dfm, "uusi_energia_stem_dfm_08032021.RDS")

uusi_energia_lemma_dfm <- uusi_energia_all_vocabs %>%
  cast_dfm(document, lemma, count)
# saveRDS(uusi_energia_lemma_dfm, "uusi_energia_lemma_dfm_08032021.RDS")