
#' Add TF-IDF summary based on groupings.
#'
#' @description The TF-IDF function in tidytext requires an explicit 'doc'
#' parameter; this applies it on the existing dataset groups.
#'
#' @param frame A grouped data from
#' @param word The unquoted variable name storing terms for frequency
#' @param count The unquoted variable storing the count
#'
#' @return A data_frame with a column tfidf added.
#'
#' @example get_recent_SOTUs() %>% group_by(president, word) %>%
#'  summarize(count=n()) %>%
#'  summarize_tf_idf(word, count) %>%
#'  arrange(-.tf_idf)
#' @export
#'
summarize_tf_idf <- function(frame, word, count) {
  groupings <- groups(frame)
  n_docs <- frame %>% distinct(!!!groupings) %>% nrow()
  frame %>%
    distinct(!!!groupings, {{word}}) %>%
    group_by({{word}}) %>%
    summarize(idf = log(n_docs / n())) %>%
    inner_join(frame) %>%
    group_by(!!!groupings)%>%
    mutate(doc_total = sum({{count}})) %>%
    group_by({{word}}, add = TRUE) %>%
    summarize(.count = sum({{count}}), .tf_idf = .count / doc_total[1] * idf[1])
}



#' Add Pointwise mutual information summary based on groupings.
#'
#' @description Create a new column called '.pmi' for the field identified by
#' 'word'
#'
#' @param frame A grouped tibble.
#' @param word The unquoted variable name storing terms for frequency.
#' @param count The unquoted variable storing the count.
#'
#' @return A data_frame with a column .pmi added.
#' @export

summarize_pmi <- function(frame, word, count, fill = FALSE) {
  groupings <- groups(frame)
  #n_docs <- frame %>% distinct(!!!groupings) %>% nrow()
  message(groupings)
  frame %>%
    group_by({{word}}, add=TRUE) %>%
    summarize(count=sum({{count}})) %>%
    ungroup %>%
    mutate(.total_words = sum(count)) %>%
    group_by({{word}}) %>%
    mutate(.word_share = sum(count)/.total_words) %>%
    group_by(!!!groupings) %>%
    mutate(.local_word_share = count/sum(count)) %>%
    mutate(.pmi = log(.local_word_share/.word_share)) %>%
    select(-.local_word_share, -.word_share, -.total_words)
}

#' Summarize the log-likelihood ratio across a grouped data frame
#'
#' @param data A data frame
#' @param token The column indicating a token.
#' @param count The column indicating wordcount data.
#'
#' Cite: Ted Dunning, Accurate Statistics.
#'
#' @return A dataframe with the supplied grouping and a log-likelihood for each token in that grouping.
#' Strongly positive numbers are over-represented; strongly negative numbers are under-represented.
#' @export
#'
summarize_llr <- function(data, token, count = rep(1, n())) {
  token <- enquo(token)
  count <- enquo(count)
  groupings <- groups(data)

  data %>%
    group_by(!!token, add = TRUE) %>%
    # Numeric to fix some integer overflow problems.
    summarize(count = sum(!!count) %>% as.numeric()) %>%
    group_by(!!token) %>%
    mutate(grandtot = sum(count)) %>%
    group_by(!!!groupings) %>%
    # Dunning scores
    mutate(count.x = count, count.y = grandtot - count) %>%
    mutate(c = sum(count.x), d = sum(count.y), totalWords = c + d) %>%
    mutate(count.y = ifelse(count.y == 0, 1, count.y)) %>%
    mutate(exp.x = c * grandtot / totalWords, exp.y = d * grandtot / totalWords) %>%
    mutate(score = 2 * (
      (count * log(
        count / exp.x
      )) +
        count.y * log(count.y / exp.y))) %>%
    mutate(score = ifelse((count.y - exp.y) > 0, -score, score)) %>%
    select(!!!groupings, !!token, dunning_llr = score) %>%
    ungroup()
}
