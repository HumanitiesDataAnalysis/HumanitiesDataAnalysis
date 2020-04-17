#' Search words for a regular expression
#'
#' @param regex_string
#'
#' @return The words in the dictionary matching your regular expression.
#' @export
#'
#' @examples
dictionary_search = function(regex_string) {
  data(wordlist)
  return(paste(wordlist[stringr::str_detect(wordlist, regex_string)][1:10], collapse = ", "))
}

