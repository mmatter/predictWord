
library(tm)
library(Matrix)
library(parallel)

source("ngram.model.R")
source("ngrams.R")
source("preprocess.R")

set.seed(123)
# ---------------------------------------------------------------------------------------------------------------------------------
tic_tot <- as.numeric(Sys.time())

dataFiles <- list("news"="data/final/en_US/en_US.news.txt",
                  "blogs"="data/final/en_US/en_US.blogs.txt",
                  "twitter"="data/final/en_US/en_US.twitter.txt") 

# choose source
src <- "blogs"
print(paste0("source: ", src))
nsam_voc <- 10000
print(paste0("sample (number of lines) for vocabulary: ", nsam_voc))
nsam_train <- 1000
print(paste0("sample (number of lines) for training: ", nsam_train))
nsam_test <- 100
print(paste0("sample (number of lines) for testing: ", nsam_test))

tic <- as.numeric(Sys.time())
print(paste0("Reading in data for source: ", src))
data_all <- readLines(con=dataFiles[[src]])
toc <- as.numeric(Sys.time())
print(paste("elapsed time in seconds:", round(toc-tic, digits=2)))
#--------------------------------------------------------------------------
# Preparing vocabulary:
tic <- as.numeric(Sys.time())
print(paste0("Learning vocabulary for source: ", src))

indices_voc <- sample(1:length(data_all), nsam_voc)
doc <- paste(data_all[indices_voc], collapse=" ")

vs <- VectorSource(doc) # creating a corpus (consisting in a single large document)
cps <- VCorpus(vs)
cps <- preprocess(cps) # some preprocessing
ordered_words <- strsplit(cps[[1]]$content, split=" ")[[1]]
counts <- sort(table(ordered_words), decreasing = TRUE)
# define vocabulary by size
# voc_size <- 50000
# vocabulary <- head(counts, n=voc_size)
# define vocabulary by frequency:
voc_freq <- 5
print(paste0("vocabulary: retaining only words occurring more than ", voc_freq, " times"))
vocabulary <- names(counts[counts >= voc_freq])
rm(doc, cps)

toc <- as.numeric(Sys.time())
print(paste("elapsed time in seconds:", round(toc-tic, digits=2)))
#--------------------------------------------------------------------------
# Preparing training and test sets:
tic <- as.numeric(Sys.time())
print(paste0("Preparing training and test sets for source: ", src))

indices_train <- sample(1:length(data_all), nsam_train)
indices_test <- sample((1:length(data_all))[-indices_train], nsam_test)
docs <- list("train" = paste(data_all[indices_train], collapse=" "),
             "test" = paste(data_all[indices_test], collapse=" "))
rm(data_all)

ordered_words <- lapply(docs, FUN=function(doc, vocabulary) {
    vs <- VectorSource(doc) # creating a corpus (consisting in a single large document)
    cps <- VCorpus(vs)
    cps <- preprocess(cps) # some preprocessing
    tmp <- strsplit(cps[[1]]$content, split=" ")[[1]]
    output <- ifelse(!(tmp %in% vocabulary), "<UNK>", tmp) # replacing words not in vocabulary by generic word 
    return(output)
}, vocabulary)
rm(docs)

toc <- as.numeric(Sys.time())
print(paste("elapsed time in seconds:", round(toc-tic, digits=2)))
#-------------------------------------------------------------------------
### Learning phase:
n <- 4
tic <- as.numeric(Sys.time())
print(paste0("Learning n-grams model from n=1 to ", n))
NGR_model <- ngram.model(n, text=ordered_words$train)

toc <- as.numeric(Sys.time())
print(paste("elapsed time in seconds:", round(toc-tic, digits=2)))

#------------------------------------------------------------------------
## Saving data:
save(NGR_model, vocabulary, file=paste0("learned_data/learned_", src, "_10000.Rdata"))

toc_tot <- as.numeric(Sys.time())
print(paste("Total elapsed time in seconds:", round(toc_tot-tic_tot, digits=2)))
print("#####################################################################")






