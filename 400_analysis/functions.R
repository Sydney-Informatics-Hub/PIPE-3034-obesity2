assess_year_source <- function(df){
  df %>%
    dplyr::select(source, year) %>%
    group_by(year, source) %>%
    count(year) %>%
    rename(count = n) %>%
    pivot_wider(id_cols = c(source), names_from = year, values_from = c(count), values_fill = 0) %>%
    janitor::adorn_totals(c("row", "col")) %>%
    kable()
}

read_cqpweb <- function(filename){
  read.csv(
    here("100_data_raw", filename),
    skip = 3, sep = "\t") %>%
    janitor::clean_names()
}

cramers_v_condpers <- function(df, chisq_result){
  df_interest <- df[,c("condition-first", "person-first")]
  # deg_f <- min(dim(df_interest)) - 1
  # always 1 for our datasets
  deg_f <- 1
  sqrt(as.numeric(chisq_result$statistic) / (sum(df_interest) * deg_f))
}

articles_per_journal <- function(df, label) {
  df %>%
    select(article_id, year, source) %>%
    mutate(type = label)
}

prettyrbind_chisq_result <- function(df, chisq_result, prefix){

  observed <- data.frame(chisq_result$observed) |>
    rename_with(.fn = ~ paste(prefix, .x, "observed", sep = "_")) |>
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "value") |>
    mutate(variable = stringr::str_replace(variable, "\\.", "_"))

  expected <- data.frame(chisq_result$expected) |>
    rename_with(.fn = ~ paste(prefix, .x, "expected", sep = "_")) |>
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "value") |>
    mutate(variable = stringr::str_replace(variable, "\\.", "_"))

  broomed <- broom::tidy(chisq_result) |>
    # reorder columns
    select(method, parameter, statistic, p.value) |>
    t() |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    rename(variable = rowname, value = V1) |>
    mutate(variable = paste(prefix, variable, sep = "_"))

  effect_size <-
    data.frame(
      variable = paste(prefix, "effect_size", sep = "_"),
      value = cramers_v_condpers(df, chisq_result))
  rbind(broomed, effect_size, observed, expected)
}

prettyrbind_goodnessoffit <- function(chisq_result){

  obsexp <-
    broom::augment(chisq_result) |>
    select(Var1, .observed, .expected) |>
    rename(variable = Var1, observed = .observed, expected = .expected) |>
    pivot_longer(cols = c(observed, expected)) |>
    mutate(variable = paste(variable, name, sep = "_")) |>
    select(-name)


  broomed <- broom::tidy(chisq_result) |>
    # reorder columns
    select(method, parameter, statistic, p.value) |>
    t() |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    rename(variable = rowname, value = V1)

  rbind(broomed, obsexp)
}


chisq_instances_wc_normalised <- function(df_ofinterest, metadata, var) {
  var <- enquo(var)
  total_wc <-
    metadata |>
    select(!!var, wordcount_total) |>
    group_by(!!var) |>
    summarise(total_wc = sum(wordcount_total))

  no_hits_in_text <-
    df_ofinterest |>
    group_by(!!var) |>
    select(!!var, no_hits_in_text) |>
    summarise(total_hits = sum(no_hits_in_text))

  joined <- inner_join(total_wc, no_hits_in_text)

  total_instances <- joined$total_hits
  names(total_instances) <- joined[,rlang::as_name(var), drop = TRUE]
  total_wc_test <- joined$total_wc


  chisq_res <- stats::chisq.test(total_instances,
                                 p = total_wc_test,
                                 rescale.p = T)
  prettyrbind_goodnessoffit(chisq_res)
}


chisq_articles_totalart_normalised <- function(df_ofinterest, metadata, var) {
  var <- enquo(var)
  total_articles <-
    metadata |>
    select(!!var, article_id) |>
    group_by(!!var) |>
    count() |>
    rename(total_articles = n)

  no_articles_with_feature <-
    df_ofinterest |>
    group_by(!!var) |>
    count() |>
    rename(no_articles_with_feature = n)

  joined <- inner_join(total_articles, no_articles_with_feature)

  total <- joined$no_articles_with_feature
  names(total) <- joined[,rlang::as_name(var), drop = TRUE]
  total_artcount_test <- joined$total_articles


  chisq_res <- stats::chisq.test(total,
                                 p = total_artcount_test,
                                 rescale.p = T)
  prettyrbind_goodnessoffit(chisq_res)
}

get_article_counts_nooverlap <- function(cond_df, pers_df, var){
  var = enquo(var)
  pers_article_ids <- pers_df |> pull(article_id)
  cond_article_ids <- cond_df |> pull(article_id)
  cond_df %>%
    filter(!(article_id %in% pers_article_ids)) %>%
    mutate(type = "condition-first") %>%
    rbind({
      pers_df %>%
        filter(!(article_id %in% cond_article_ids)) %>%
        mutate(type = "person-first")
    }) %>%
    group_by(!!var, type) %>%
    count() %>%
    pivot_wider(names_from = type, values_from = n)
}

get_article_counts_woverlap <- function(cond_df, pers_df, var){
  var = enquo(var)
  cond_df %>%
    mutate(type = "condition-first") %>%
    rbind({
      pers_df %>%
        mutate(type = "person-first")
    }) %>%
    group_by(!!var, type) %>%
    count() %>%
    pivot_wider(names_from = type, values_from = n)
}

get_instance_counts_nooverlap <- function(cond_df, pers_df, var){
  var = enquo(var)
  pers_article_ids <- pers_df |> pull(article_id)
  cond_article_ids <- cond_df |> pull(article_id)
  cond_df %>%
    filter(!(article_id %in% pers_article_ids)) %>%
    mutate(type = "condition-first") %>%
    rbind({
      pers_df %>%
        filter(!(article_id %in% cond_article_ids)) %>%
        mutate(type = "person-first")
    }) %>%
    group_by(!!var, type) %>%
    summarise(value = sum(no_hits_in_text)) |>
    pivot_wider(names_from = type, values_from = value)
}

get_instance_counts_woverlap <- function(cond_df, pers_df, var){
  var = enquo(var)
  cond_df %>%
    mutate(type = "condition-first") %>%
    rbind({
      pers_df %>%
        mutate(type = "person-first")
    }) %>%
    group_by(!!var, type) %>%
    summarise(value = sum(no_hits_in_text)) |>
    pivot_wider(names_from = type, values_from = value)
}


### ----

get_cramers_v <- function(mymatrix, chi) {
  # Find degrees of freedom - min row or col - 1
  n = sum(mymatrix)
  # always 1 for all of the comparisons in this study
  df <- 1
  #df <- min(dim(mymatrix)) - 1
  sqrt((chi)/(n * df))
}

generate_cnt <- function(df, var, type) {
  var = enquo(var)
  if (type == "wc") {
    df |>
      group_by(!!var) |>
      summarise(total_words = sum(wordcount_total))
  } else if (type == "art") {
    df |>
      group_by(!!var) |>
      summarise(total_articles = n())
  } else {
    print("type must be wc or art!")
  }
}


mydistribution <- coin::approximate(nresample = 10000,
                              parallel = "multicore",
                              ncpus = 8)
fp_test <- function(
    # can also be frequency here
    wc1 = au_broadsheet_wordcounts,
    wc2 = au_tabloid_wordcounts,
    label1 = "broadsheet",
    label2 = "tabloid",
    dist = mydistribution) {
  df <-
    rbind(data.frame(wc = wc1,
                     label = label1),
          data.frame(wc = wc2,
                     label = label2))|>
    mutate(label = as.factor(label))

  coin::oneway_test(formula(wc ~ label),
                    df,
                    distribution = dist,
                    conf.int = TRUE )
}


histogram_pairwise <- function(
    # for word counts
  wc1 = au_broadsheet_wordcounts,
  wc2 = au_tabloid_wordcounts,
  label1 = "broadsheet",
  label2 = "tabloid") {
  rbind(
    data.frame(wc = wc1,
               corpus = label1),
    data.frame(wc = wc2,
               corpus = label2)
  ) |>
    ggplot(aes(x = wc, fill = corpus)) +
    geom_histogram(bins = 1000) +
    labs(x = "Word count, CQP web",
         y = "Number of articles")

}
