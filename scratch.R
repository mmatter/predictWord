
library(tm)

## Importing all documents from a target directory:

#sam <- VCorpus(DirSource("data/final/en_US", encoding = "UTF-8"), readerControl = list(language = "en"))

## Reading a given document:
# gathering all lines in a single document
#docs <- paste(readLines(con="data/final/en_US/en_US.news.txt", n=100000), collapse=" ")
set.seed(1234)
news_all <- readLines(con="data/final/en_US/en_US.news.txt")
nsamples <- 10000
news_sample <- news_all[sample(1:length(news_all), nsamples)]


# creating a corpus:
vs <- VectorSource(news_all)
sam <- VCorpus(vs)

### Performing some preprocessing:
# Info about possible transformations:
#getTransformations()
sam <- tm_map(sam, removeNumbers)
#sam <- tm_map(sam, stemDocument)
sam <- tm_map(sam, content_transformer(gsub), pattern="\\$|\"|\\.|,|\\?|\\!|;|:|\\(|\\)", replacement="")
# removing whitespaces:
sam <- tm_map(sam, stripWhitespace)
# everything to lower case:
#sam <- tm_map(sam, content_transformer(tolower))
#lapply(sam, FUN=function(doc) { doc$content })

### Creating a term-document matrix:
docmat <-TermDocumentMatrix(sam)
# removing sparse items (only if more than 1 document):
#docmat2 <- removeSparseTerms(docmat, 0.2)
minocc <- 1 # minimal number of occurrence to consider the term
counts <- docmat$v[docmat$v > minocc]
terms <- dimnames(docmat)$Terms[docmat$v > minocc]
names(counts) <- terms
counts <- sort(counts, decreasing = TRUE)
### Initial probability distribution of all terms
probs <- counts/sum(docmat$v)
length(probs)
plot(head(probs, n=100))

sum(head(probs, n=100))

### Computing n-grams:
ordered_words <- strsplit(sam[[1]]$content, split=" ")[[1]]

ngrams <- function(seq, shift=1) {
    seqshifted <- c(tail(seq, n=-1), rep(NA, 1))
    if (shift == 1) {
        output <- mapply(FUN=function(w1, w2) { paste(w1, w2, sep=" ") },
                         seq, seqshifted)
    } else if (shift == 2) {
        seqshifted2 <- c(tail(seq, n=-2), rep(NA, 2))
        output <- mapply(FUN=function(w1, w2, w3) { paste(w1, w2, w3, sep=" ") },
                         seq, seqshifted, seqshifted2)
    } else {
        stop("Not allowed value for argument 'shift' !")
    } 
    return(head(output, n=-shift))
}

### Building a 1-order transition matrix:
two_grams <- ngrams(ordered_words, shift=1)
#head(two_grams, n=30)
#head(sort(table(two_grams), decreasing = TRUE), n=30)

counts_2grams <- sort(table(two_grams), decreasing = TRUE)
# keeping only 2grams appearing more than a very few times:
minocc_2grams <- round(0.00001*length(two_grams))
counts_freq2grams <- counts_2grams[counts_2grams > minocc_2grams]

splitted <- strsplit(names(counts_freq2grams), split=" ")
wstart <- unique(sapply(splitted, FUN=function(el) { el[1] }))
wend  <- unique(sapply(splitted, FUN=function(el) { el[2] }))

# Initializing transition matrix:
TransMat <- matrix(0, nrow=length(wstart), ncol=length(wend), dimnames=list(wstart, wend))
# filling it with counts:
for (i in names(counts_freq2grams)) {
    indices <- strsplit(i, split=" ")[[1]]
    TransMat[indices[1], indices[2]] <- counts_freq2grams[i]
}
# normalizing rows:
TransMat <- TransMat/rowSums(TransMat)

### Building a 2-order transition matrix:
three_grams <- ngrams(ordered_words, shift=2)
#head(three_grams, n=30)
#head(sort(table(three_grams), decreasing = TRUE), n=30)

counts_3grams <- sort(table(three_grams), decreasing = TRUE)
# keeping only 3grams appearing more than a very few times:
minocc_3grams <- round(0.00001*length(three_grams))
counts_freq3grams <- counts_3grams[counts_3grams > minocc_3grams]

splitted <- strsplit(sub(" ", "_", names(counts_freq3grams)), split=" ")
wstart <- unique(sub("_", " ", sapply(splitted, FUN=function(el) { el[1] })))
wend  <- unique(sapply(splitted, FUN=function(el) { el[2] }))

# Initializing transition matrix:
TransMat2 <- matrix(0, nrow=length(wstart), ncol=length(wend), dimnames=list(wstart, wend))
# filling it with counts:
for (i in names(counts_freq3grams)) {
    indices <- strsplit(sub(" ", "_", i), split=" ")[[1]]
    TransMat2[sub("_", " ", indices[1]), indices[2]] <- counts_freq3grams[i]
}
# normalizing rows:
TransMat2 <- TransMat2/rowSums(TransMat2)


### Updating the probability distribution given an input consisting of first letters of a term
start <- "ber"
indices <- grep(pattern=paste0("^", start), terms)
terms[indices]
counts[indices]
newprobs <- counts[indices]/sum(counts[indices])
plot(newprobs)


findAssocs(docmat, "feed", 0.6)







#-----------------------------------------------------------------------------
### Quiz 1:
## Getting the number of lines (wo importing the full doc):
f <- file("data/final/en_US/en_US.news.txt", open="rb")
nlines <- 0L
while (length(chunk <- readBin(f, "raw", 65536)) > 0) {
    nlines <- nlines + sum(chunk == as.raw(10L))
}
print(nlines)
close(f)



## Getting the longest line (importing the full doc):
news <- readLines(con="data/final/en_US/en_US.news.txt")
max(sapply(news, FUN=nchar)) # 11384
blogs <- readLines(con="data/final/en_US/en_US.blogs.txt")
max(sapply(blogs, FUN=nchar)) # 40833
twitter <- readLines(con="data/final/en_US/en_US.twitter.txt")
max(sapply(twitter, FUN=nchar)) # 140

## Searching for words:
nlove <- length(grep(pattern="love", x=twitter))
nhate <- length(grep(pattern="hate", x=twitter))
nlove/nhate

twitter[grep(pattern="biostats", x=twitter)]

grep(pattern="A computer once beat me at chess, but it was no match for me at kickboxing", x=twitter)

