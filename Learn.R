
library(tm)
#library(RWeka)
library(Matrix)
library(parallel)

source("ngram.model.R")
source("ngrams.R")

# ---------------------------------------------------------------------------------------------------------------------------------

dataFiles <- list("news"="data/final/en_US/en_US.news.txt",
                  "blogs"="data/final/en_US/en_US.blogs.txt",
                  "twitter"="data/final/en_US/en_US.twitter.txt") 

nsamples <- list("news"=1000,
                 "blogs"=1000,
                 "twitter"=1000)

minoccs <- list("news"=c("prior"=2, "P1"=2, "P2"=2, "P3"=2),
                "blogs"=c("prior"=2, "P1"=2, "P2"=2, "P3"=2),
                "twitter"=c("prior"=3, "P1"=2, "P2"=2, "P3"=2))

tic <- as.numeric(Sys.time())
NGR <- list() 
for (s in names(dataFiles)) {
    
    file <- dataFiles[[s]]
    minocc <- minoccs[[s]]
    nsam <- nsamples[[s]]
    print(paste("Processed source:", s))
    print(paste("Number of sampled lines:", nsam))
    tic_int <- as.numeric(Sys.time())
    ### reading the head of each file (faster)
    docs <- paste(readLines(con=file, n=nsam), collapse=" ")
    ### or a random sample (slower but better sampling):
    #data_all <- readLines(con=file)
    #docs <- paste(data_all[sample(1:length(data_all), nsam)], collapse=" ")
    ###
    # creating a corpus (consisting in a single large document):
    vs <- VectorSource(docs)
    corpus <- VCorpus(vs)
    ### Some preprocessing:
    corpus <- tm_map(corpus, removeNumbers) # removing numbers
    # removing some special characters:
    regulexp <- "\\$|\"|\\.|,|\\?|\\!|;|:|\\(|\\)|<|>|_|-|+|~|@|&|%"
    corpus <- tm_map(corpus, content_transformer(gsub), pattern=regulexp, replacement="")
    corpus <- tm_map(corpus, stripWhitespace) # removing additional whitespaces
    corpus <- tm_map(corpus, content_transformer(tolower)) # everything to lower case:
    ordered_words <- strsplit(corpus[[1]]$content, split=" ")[[1]]
    rm(docs, corpus)
    ngrmodels <- 1:4
    names(ngrmodels) <- c("prior", "P1", "P2", "P3")
    NGR[[s]] <- mcmapply(FUN=ngram.model, ngrmodels, minocc, MoreArgs = list(ordered_words), mc.cores = 1)
    toc_int <- as.numeric(Sys.time())
    print(paste("elapsed time in seconds:", round(toc_int-tic_int, digits=2)))
    print("--------------------------------")
}

#save(NGR, file="learned_data/learned_en_US_100000.Rdata")

toc <- as.numeric(Sys.time())
print(paste("Total elapsed time in seconds:", round(toc-tic, digits=2)))
print("###########################")




