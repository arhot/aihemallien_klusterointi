
# Helper functions to do clustering of topics

# inputs are data frame with column topic_model consisting of STM models and document-term-matrix in either stm or dfm format
calculate_semantic_coherence_and_exclusivity <- function(df, dtm) {
  df %>% mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dtm),
         semantic_coherence_mean = map_dbl(semantic_coherence, mean),
         exclusivity_mean = map_dbl(exclusivity, mean))
}

# tidy each model and make a list of top terms per topic
make_per_model_df_with_tidy_and_top_terms <- function(df) {
  df %>% 
    mutate(tidy_model = map(topic_model, tidy),
        top_terms = map(tidy_model, function(df) df %>% group_by(topic) %>% 
                             arrange(desc(beta)) %>% top_n(10, beta) %>% 
                             summarise(terms = list(term))))
}

# make a data frame where rows are topics, columns are terms
# uses indicator variables K, run, topic, and K_run_topic to identify topics
# if your dictionary includes terms that overlap with the indicators, use argument prefix to a distinguishing character
make_per_topic_df <- function(df, prefix="") {
  df %>% mutate(tidy_model_wide = pmap(list(df=tidy_model, prefix=prefix), function(df, prefix) pivot_wider(names_from=term, values_from=beta, data=df, names_prefix=prefix))) %>% 
    unnest(tidy_model_wide) %>% 
    mutate(K_run_topic = paste(K, run, topic, sep="_"))
}


# combine the models/topics you want to cluster. 
# This might be "total" (cluster everything together), 
# "per_K" (cluster models with same number of topics together, most sensible if number of topics varies considerably),
# "vocab_K" (if you want to separately cluster models with different vocabularies)
# This function works, but should be rewritten..
make_separate_df <- function(df, mode="per_K") {
  if(mode=="per_K") {
    df_as_per_list <- df %>%
    select(-K, -run, -topic, -topic_model, -tidy_model, -top_terms) %>%
    pivot_longer(cols=!(one_of("K_run_topic"))) %>%
    pivot_wider(names_from=K_run_topic, values_from=value) %>% 
    split.default(., str_extract(names(.), "[^_]+"))
  tidy_per_K <- tibble(K = names(df_as_per_list[-length(df_as_per_list)]), frames_for_clustering = df_as_per_list[-length(df_as_per_list)])
  return(tidy_per_K)
  }
  else if (mode=="per_run") {
    df_as_per_list <- df %>%
      select(-K, -run, -topic, -topic_model, -tidy_model, -top_terms) %>%
      pivot_longer(cols=!(one_of("K_run_topic"))) %>%
      pivot_wider(names_from=K_run_topic, values_from=value) %>% 
      split.default(., str_extract(names(.), "(?<=_)(.*?)(?=_)") %>% replace_na("name"))
    tidy_per_run <- tibble(run = names(df_as_per_list[-length(df_as_per_list)]), frames_for_clustering = df_as_per_list[-length(df_as_per_list)])
    return(tidy_per_run)
  }
  else if (mode=="total") {
    df_as_per_list <- df %>%
      select(-K, -run, -topic, -topic_model, -tidy_model, -top_terms) %>%
      pivot_longer(cols=!(one_of("K_run_topic"))) %>%
      pivot_wider(names_from=K_run_topic, values_from=value) %>% 
      split.default(., str_detect(names(.), "^[0-9]"))
    tidy_total <- tibble(run = names(df_as_per_list[-1]), frames_for_clustering = df_as_per_list[-1])
    return(tidy_total)
  } 
  else if (mode=="vocab_K") {
    df_as_per_list <- df %>%
      select(-K, -run, -topic, -topic_model, -tidy_model, -top_terms, -vocab, -type_of_model) %>%
      pivot_longer(cols=!(one_of("K_run_topic"))) %>%
      pivot_wider(names_from=K_run_topic, values_from=value) %>% 
      split.default(., str_extract(names(.), "^([^_]*_[^_]*)"))
    tidy_total <- tibble(type_K = names(df_as_per_list), frames_for_clustering = df_as_per_list)  %>% 
      mutate(frames_for_clustering = map(frames_for_clustering, na.omit))
    return(tidy_total)
  } 
  else
    {
    return("unknown mode") 
    }
}

# add cosine distance (1-cosine similarity) between all pairs of topics
add_cosine_distance <- function(df) {
  df %>% mutate(cosine_distances = map(frames_for_clustering, function(df) 1-lsa::cosine(as.matrix(df))))
} 

# do hdbscan clustering for dataframe, for minPts as defined in the argument 
do_clustering_by_split_at_varying_minPts <- function(df, vardef="K", minPts=c(3:10)) {
  df$splitting_variable <- df[[vardef]]
  expand_grid(splitting_variable = df[[vardef]], minPts=minPts) %>% left_join(df) %>%
    mutate(hdbscan = pmap(list(distances=cosine_distances, minPts=minPts), function(distances, minPts) hdbscan(x=distances, minPts=minPts))) %>%
    mutate(noise_points = map(hdbscan, function(df) sum(df$cluster==0)) %>% unlist(),
           largest_cluster = map(hdbscan, function(df) max(table(df$cluster))) %>% unlist(),
           number_of_clusters = map(hdbscan, function(df) length(table(df$cluster))) %>% unlist())
}


# get cluster number for each topic
add_clusters_as_df <- function(df){
  result <- df %>% mutate(topics_on_rows = map(frames_for_clustering, function(df) tibble(K_run_topic = colnames(df))),
         cluster = map(hdbscan, function(df) df$cluster %>% as_tibble()),
         clusters_frame = map2(topics_on_rows, cluster, function(topics_on_rows, cluster) bind_cols(topics_on_rows, cluster))) %>% 
    select(clusters_frame) %>% map(bind_rows) %>% as_tibble() %>% ungroup()
  return(result$clusters_frame)
}

# get top terms as a string in dataframe
get_top_terms <- function(df) {
  df %>% select(K, run, top_terms) %>% unnest(cols=c(top_terms)) %>% mutate(K_run_topic = paste(K, run, topic, sep="_"),
                                                                             top_terms = sapply(terms, toString))
} 

combine_clusters_and_top_terms <- function(df, top_terms) {
    df %>% left_join(top_terms) %>%  arrange(K, value, run)
}

calculate_per_cluster_top_terms <- function(df, model, grouping_variable="K") {
  df$grouping_variable <- df[[grouping_variable]]
  df %>% select(-topic_model, -top_terms) %>% unnest(cols=c(tidy_model)) %>% 
    mutate(K_run_topic = paste(K, run, topic, sep="_")) %>%
    left_join(model) %>% group_by(grouping_variable, value, term) %>% summarize(sum_beta = sum(beta)) %>% 
    arrange(desc(sum_beta)) %>% top_n(10, sum_beta) %>% 
    summarise(terms = list(term)) %>% mutate(top_terms = sapply(terms, toString)) %>% select(-terms)
}


add_per_cluster_variables_and_labels <- function(df, grouping_variable="K", duplicated_threshold = 2, resolved_threshold = 2) {
  df$grouping <- df[[grouping_variable]]
  df %>% mutate(K_run = str_extract(K_run_topic, "[^_]*_[^_]*")) %>%
    group_by(grouping) %>% mutate(total_runs_by_type = n_distinct(K_run)) %>% group_by(grouping, value) %>% 
    summarize(unique_in_cluster = n_distinct(K_run), total_in_cluster=n(), duplicated_in_cluster = total_in_cluster-unique_in_cluster, 
              runs_that_did_not_find = max(total_runs_by_type)-unique_in_cluster) %>%
  mutate(cluster_label = ifelse(value == 0, "Junk", 
                                ifelse(duplicated_in_cluster > duplicated_threshold, "Fused",
                                       ifelse(runs_that_did_not_find <= resolved_threshold, "Resolved", "Semi-resolved"))))
}


