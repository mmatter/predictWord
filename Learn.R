
library(tm)

source("ngrams.R")

docs <- paste(readLines(con="data/final/en_US/en_US.news.txt", n=10000), collapse=" ")
# creating a corpus (consisting in a single large document):
vs <- VectorSource(docs)
wnews <- VCorpus(vs)

### Some preprocessing:
wnews <- tm_map(wnews, removeNumbers) # removing numbers
# removing some special characters:
regulexp <- "\\$|\"|\\.|,|\\?|\\!|;|:|\\(|\\)"
wnews <- tm_map(wnews, content_transformer(gsub), pattern=regulexp, replacement="")
wnews <- tm_map(wnews, stripWhitespace) # removing additional whitespaces
wnews <- tm_map(wnews, content_transformer(tolower)) # everything to lower case:

### Creating a term-document matrix:
docmat <-TermDocumentMatrix(wnews)
minocc <- 1 # minimal number of occurrence to consider the term
counts <- docmat$v[docmat$v >= minocc]
terms <- dimnames(docmat)$Terms[docmat$v >= minocc]
names(counts) <- terms
counts <- sort(counts, decreasing = TRUE)
prior <- counts/sum(docmat$v) # relative frequencies of the words


### Computing 2-grams and building a first-order transition matrix:
ordered_words <- strsplit(wnews[[1]]$content, split=" ")[[1]]
two_grams <- ngrams(ordered_words, shift=1)

counts_2grams <- sort(table(two_grams), decreasing = TRUE)
# keeping only 2grams appearing more than a very few times:
minocc_2grams <- round(0.00001*length(two_grams))
counts_freq2grams <- counts_2grams[counts_2grams > minocc_2grams]

splitted <- strsplit(names(counts_freq2grams), split=" ")
wstart <- unique(sapply(splitted, FUN=function(el) { el[1] }))
wend  <- unique(sapply(splitted, FUN=function(el) { el[2] }))

# Initializing transition matrix:
P1 <- matrix(0, nrow=length(wstart), ncol=length(wend), dimnames=list(wstart, wend))
# filling it with counts:
for (i in names(counts_freq2grams)) {
    indices <- strsplit(i, split=" ")[[1]]
    P1[indices[1], indices[2]] <- counts_freq2grams[i]
}
# normalizing rows:
P1 <- P1/rowSums(P1)

### Saving data:
save(P1, prior, file="learned.Rdata")

