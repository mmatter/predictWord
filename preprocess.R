

preprocess <- function(corpus) {
  
  corpus <- tm_map(corpus, removeNumbers) # removing numbers
  # removing some special characters:
  regulexp <- "\\$|\"|\\.|,|\\?|\\!|;|:|\\(|\\)|<|>|_|-|+|~|@|&|%|=|^"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern=regulexp, replacement="")
  corpus <- tm_map(corpus, stripWhitespace) # removing additional whitespaces
  corpus <- tm_map(corpus, content_transformer(tolower)) # everything to lower case:
  
  return(corpus)
  
}