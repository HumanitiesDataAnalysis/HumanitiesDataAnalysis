
#' Read a text file into an unnested tidy frame.
#'
#' @param filename
#' @param ... Further arguments are passed to unnest_tokens, such as 'to_lower',
#' 'strip_punct', and so forth.
#'
#' @return A data frame with the columns 'word', 'paragraph', and 'filename.'
#' @export
#'
#' @examples

read_tokens <- function(filename, ...) {
  read_lines(filename) %>%
    tibble(text = .) %>%
    filter(text != "") %>%
    # Add a counter for the paragraph number
    mutate(paragraph = 1:n()) %>%
    tidytext::unnest_tokens(word, text, ...) %>%
    mutate(filename = filename %>%
             basename() %>%
             str_replace(".txt", ""))
}

#' Summarize the log-likelihood ratio across a grouped data frame
#'
#' @param data A data frame
#' @param token The column indicating a token. Unquoted.
#' @param count The column indicating count data. Unquoted.
#' @param sig_thresh The signifance level (p-value) to test against. Default is 0.05.
#'
#' @return A dataframe with the supplied grouping and additional log-likelihood for each token in that grouping.
#'
#' The direction of the Dunning scores are encoded with a sign.
#' Strongly positive numbers are over-represented; strongly negative numbers are under-represented.
#' Also included are unadjusted p-values assuming a single degree of freedom, and a significance test of the difference the Holm-Sidak
#' correction ranked by decreasing word frequency. (Holm-Sidak)
#' @export
#'

summarize_llr <- function(data, token, count = rep(1, n()), sig_thresh = 0.05) {
  token <- enquo(token)
  count <- enquo(count)
  groupings <- groups(data)

  data %>%
    group_by(!!token, add = TRUE) %>%
    # Numeric to fix some integer overflow problems in very large sets.
    summarize(count = sum(!!count) %>% as.numeric()) %>%
    group_by(!!token) %>%
    # Total number of words in the set.
    mutate(grandtot = sum(count)) %>%
    group_by(!!!groupings) %>%
    # Three levels of counts: for this group, for all the other groups together *except* this one (count.y),
    # and for all groups together (grandtot)
    mutate(count.x = count, count.y = grandtot - count) %>%
    # We need not just the token-level counts, but the counts summed across all words.
    mutate(c = sum(count.x), d = sum(count.y), totalWords = c + d) %>%
    # A phony correction to allow logarithms; if a word does not appear in the
    #
    mutate(count.y = ifelse(count.y == 0, 1, count.y)) %>%
    # Expected counts based on uniform rates.
    mutate(exp.x = c * grandtot / totalWords, exp.y = d * grandtot / totalWords) %>%
    # The Dunning log-likelihood based on mutual information.
    mutate(score = 2 * (
      (count * log(
        count / exp.x
      ) + count.y * log(count.y / exp.y)))) %>%
    # Using chi-square tables for the lookup; not exactly right.
    mutate(p = pchisq(abs(score), df = 1)) %>%
    # Holm-Sidak sig by frequency, descending
    arrange(-grandtot) %>%
    mutate(sig = p > (1 - sig_thresh)^(1/(1:n()))) %>%
    # Use negative signage to indicate *under-represented* words.
    mutate(score = ifelse((count.y - exp.y) > 0, -score, score)) %>%
    # Throw away some of the on-the-way calculations.
    select(!!!groupings, !!token, .dunning_llr = score, .dunning_p = p, .dunning_significance = sig) %>%
    ungroup()
}

summarize_pmi <- function(data, token, count = rep(1, n()), all_fields = FALSE) {
  token <- enquo(token)
  count <- enquo(count)
  groupings <- groups(data)
  if (all_fields) {groups = quos(.pmi, .p_share, .count, .word_share, .total_words, .doc_total)} else {
    groups = quos(.pmi, .p_share, .count)
  }
  data %>%
    group_by(!!!groupings, !!token) %>%
    summarize(.count = sum(!!count)) %>%
    ungroup() %>%
    mutate(.total_words = sum(.count)) %>%
    group_by(!!token) %>%
    mutate(.word_share = sum(.count) / .total_words) %>%
    group_by(!!!groupings) %>%
    mutate(.doc_total = sum(.count), .p_share = .count/.doc_total) %>%
    mutate(.pmi = log(.p_share / .word_share)) %>%
    select(!!!groupings, !!token, !!!groups) %>%
    ungroup
}


#' Add ordered chunks to a grouped text frame.
#'
#' @param data a dataframe of token counts, where the grouping
#'  represent documents.
#' @param count The column of word counts. If NULL (the default),
#'  assumes that each row is the solitary occurence of that word.
#' @param chunk_length The number of words in each chunk.
#'
#' @return
#' @export
#'
#' @examples
add_chunks <- function(data, count = NULL, chunk_length = 2000) {
  count <- enquo(count)
  group_names <- group_vars(data)
  groupings <- groups(data)
  # If there are no counts, then the previous ordering is preserved exactly
  if (rlang::quo_is_null(count)) {
    data %>%
      mutate(.count = 1) %>%
      mutate(.tally = cumsum(.count), chunk = 1 + .tally %/% chunk_length) %>%
      select(-.tally, -.count)
  } else {
    data %>%
      summarize(.count = sum(!!count)) %>%
      mutate(.tally = cumsum(.count), chunk = 1 + .tally %/% chunk_length) %>%
      select(-.tally, -.count) %>%
      inner_join(data, ., by = group_names)
  }
}


#' Add TF-IDF summary based on groupings.
#'
#' @description The TF-IDF function in tidytext requires an explicit 'doc'
#' parameter; this applies it on the existing dataset groups.
#'
#' @param frame A grouped data set where groups represent 'documents.'
#' @param word The unquoted variable name storing terms for frequency.
#' @param count The unquoted variable storing the count.
#'
#' @return A data_frame with a column tfidf added.
#' @example get_recent_SOTUs() %>% group_by(president, word) %>%
#'  summarize(count=n()) %>%
#'  summarize_tf_idf(word, count) %>%
#'  arrange(-.tf_idf)
#' @export
#'
summarize_tf_idf <- function(data, word, count = rep(1, n())) {
  token <- enquo(word)
  count <- enquo(count)
  groupings <- groups(data)
  n_docs <- data %>% n_groups()
  data <- data %>%
    group_by(!!token, add = TRUE) %>%
    summarize(.count := sum(!!count))
  data %>%
    distinct(!!!groupings, !!token) %>%
    group_by(!!token) %>%
    summarize(idf = log(n_docs / n())) %>%
    inner_join(data) %>%
    group_by(!!!groupings) %>%
    mutate(doc_total = sum(.count)) %>%
    group_by(!!token, add = TRUE) %>%
    summarize(tf = sum(.count) / doc_total[1], idf = idf[1], .tf_idf = tf * idf) %>%
    select(-tf, -idf) %>%
    ungroup()
}
