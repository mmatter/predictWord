
library(tm)
library(Matrix)
library(parallel)

source("ngram.model.R")
source("ngrams.R")


# ---------------------------------------------------------------------------------------------------------------------------------


dataFiles <- list("news"="data/final/en_US/en_US.news.txt",
                  "blogs"="data/final/en_US/en_US.blogs.txt",
                  "twitter"="data/final/en_US/en_US.twitter.txt") 

nsamples <- 1000
print(paste("Number of sampled lines:", nsamples))
system.time({
TOKENS <- lapply(dataFiles, FUN=function(file, nsam) {
    
    ### reading the head of each file (faster)
    docs <- paste(readLines(con=file, n=nsam), collapse=" ")
    ### or a random sample (slower but better sampling):
    #data_all <- readLines(con=file)
    #docs <- paste(data_all[sample(1:length(data_all), nsam)], collapse=" ")
    ###
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
    ordered_words <- strsplit(wnews[[1]]$content, split=" ")[[1]]
    
    ### Creating a term-document matrix:
    docmat <-TermDocumentMatrix(wnews)
    minocc <- 1 # minimal number of occurrence to consider the term
    counts <- docmat$v[docmat$v >= minocc]
    terms <- dimnames(docmat)$Terms[docmat$v >= minocc]
    names(counts) <- terms
    counts <- sort(counts, decreasing = TRUE)
    prior <- counts/sum(docmat$v) # relative frequencies of the words
    
    return(list("ordered_words"=ordered_words, "prior"=prior))
}, nsam=nsamples)
})

# Computing n-gram models for n=1..3 (i.e. first, second and third order transition matrices)
system.time({NGR <- mclapply(TOKENS, FUN=function(text) { 
    ngr <- mclapply(1:3, FUN=ngram.model, text=text$ordered_words, mc.cores = 1)
    names(ngr) <- c("P1", "P2", "P3")
    ngr$prior <- text$prior
    
    return(ngr)
}, mc.cores=2)
})

#TESTsparse <- ngram.model(n=3, text=TOKENS$news$ordered_words)

save(NGR, file="learned_data/learned_en_US_100000.Rdata")
print("--------------------------------")

### old:
# ### Computing 2-grams and building a first-order transition matrix:
# two_grams <- ngrams(ordered_words, shift=1)
# 
# counts_2grams <- sort(table(two_grams), decreasing = TRUE)
# # keeping only 2grams appearing more than a very few times:
# minocc_2grams <- round(0.00001*length(two_grams))
# counts_freq2grams <- counts_2grams[counts_2grams > minocc_2grams]
# 
# splitted <- strsplit(names(counts_freq2grams), split=" ")
# wstart <- unique(sapply(splitted, FUN=function(el) { el[1] }))
# wend  <- unique(sapply(splitted, FUN=function(el) { el[2] }))
# 
# # Initializing transition matrix:
# P1 <- matrix(0, nrow=length(wstart), ncol=length(wend), dimnames=list(wstart, wend))
# # filling it with counts:
# for (i in names(counts_freq2grams)) {
#     indices <- strsplit(i, split=" ")[[1]]
#     P1[indices[1], indices[2]] <- counts_freq2grams[i]
# }
# # normalizing rows:
# P1 <- P1/rowSums(P1)
# 
# ### Computing 3-grams and building a second-order transition matrix:
# three_grams <- ngrams(ordered_words, shift=2)
# 
# counts_3grams <- sort(table(three_grams), decreasing = TRUE)
# # keeping only 3grams appearing more than a very few times:
# minocc_3grams <- round(0.00001*length(three_grams))
# counts_freq3grams <- counts_3grams[counts_3grams > minocc_3grams]
# 
# splitted <- strsplit(sub(" ", "_", names(counts_freq3grams)), split=" ")
# wstart <- unique(sub("_", " ", sapply(splitted, FUN=function(el) { el[1] })))
# wend  <- unique(sapply(splitted, FUN=function(el) { el[2] }))
# 
# # Initializing transition matrix:
# P2 <- matrix(0, nrow=length(wstart), ncol=length(wend), dimnames=list(wstart, wend))
# # filling it with counts:
# for (i in names(counts_freq3grams)) {
#     indices <- strsplit(sub(" ", "_", i), split=" ")[[1]]
#     P2[sub("_", " ", indices[1]), indices[2]] <- counts_freq3grams[i]
# }
# # normalizing rows:
# P2 <- P2/rowSums(P2)
# 
# ### Computing 4-grams and building a third-order transition matrix:
# four_grams <- ngrams(ordered_words, shift=3)
# 
# counts_4grams <- sort(table(four_grams), decreasing = TRUE)
# 
# # keeping only 3grams appearing more than a very few times:
# minocc_4grams <- round(0.00001*length(four_grams))
# counts_freq4grams <- counts_4grams[counts_4grams > minocc_4grams]
# #pruned_4grams <- counts_4grams[counts_4grams <= minocc_4grams]
# #pruned_mass <- sum(pruned_4grams)/length(four_grams)
# 
# splitted <- do.call(rbind, strsplit(names(counts_freq4grams), ' (?=[^ ]+$)', perl=TRUE))
# wstart <- unique(splitted[, 1])
# wend <- unique(splitted[, 2])
# 
# # Initializing transition matrix:
# P3 <- matrix(0, nrow=length(wstart), ncol=length(wend), dimnames=list(wstart, wend))
# # filling it with counts:
# for (i in names(counts_freq4grams)) {
#     #indices <- strsplit(sub(" ", "_", i), split=" ")[[1]]
#     indices <- strsplit(i, split=" (?=[^ ]+$)", perl=TRUE)[[1]]
#     P3[indices[1], indices[2]] <- counts_freq4grams[i]
# }
# # normalizing rows:
# P3 <- P3/rowSums(P3)

### Saving data:
#save(P1, P2, P3, prior, file="learned.Rdata")

