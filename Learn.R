
library(tm)
#library(RWeka)
library(Matrix)
library(parallel)

source("ngram.model.R")
source("ngrams.R")
source("preprocess.R")

set.seed(123)
# ---------------------------------------------------------------------------------------------------------------------------------

dataFiles <- list("news"="data/final/en_US/en_US.news.txt",
                  "blogs"="data/final/en_US/en_US.blogs.txt",
                  "twitter"="data/final/en_US/en_US.twitter.txt") 

nsamples_train <- list("news"=1000,
                       "blogs"=1000,
                       "twitter"=1000)

nsamples_test <- list("news"=100,
                      "blogs"=100,
                      "twitter"=100)

minoccs <- list("news"=c("prior"=100, "P1"=50, "P2"=10, "P3"=5),
                "blogs"=c("prior"=100, "P1"=50, "P2"=10, "P3"=5),
                "twitter"=c("prior"=100, "P1"=50, "P2"=10, "P3"=3))

tic <- as.numeric(Sys.time())
NGR <- list()
TEST <- list()
for (s in names(dataFiles)) {
    
    file <- dataFiles[[s]]
    minocc <- minoccs[[s]]
    nsam_train <- nsamples_train[[s]]
    nsam_test <- nsamples_test[[s]]
    print(paste("Processing source:", s))
    print(paste("Number of sampled lines for training:", nsam_train))
    tic_int <- as.numeric(Sys.time())
    ### reading the head of each file (faster)
    #docs <- paste(readLines(con=file, n=nsam), collapse=" ")
    ### or a random sample (slower but better sampling):
    data_all <- readLines(con=file)
    indices_train <- sample(1:length(data_all), nsam_train)
    indices_test <- sample((1:length(data_all))[-indices_train], nsam_test)
    docs <- list("train" = paste(data_all[indices_train], collapse=" "),
                 "test" = paste(data_all[indices_test], collapse=" "))
    rm(data_all)
    
    ordered_words <- lapply(docs, FUN=function(doc) {
      vs <- VectorSource(doc) # creating a corpus (consisting in a single large document)
      cps <- VCorpus(vs)
      cps <- preprocess(cps) # some preprocessing
      output <- strsplit(cps[[1]]$content, split=" ")[[1]]
      return(output)
    })
    rm(docs)
    
    ngrmodels <- 1:4
    names(ngrmodels) <- c("prior", "P1", "P2", "P3")
    NGR[[s]] <- mcmapply(FUN=ngram.model, ngrmodels, minocc, MoreArgs = list(ordered_words$train), mc.cores = 4)
    TEST[[s]] <- ordered_words$test
    rm(ordered_words)
    toc_int <- as.numeric(Sys.time())
    print(paste("elapsed time in seconds:", round(toc_int-tic_int, digits=2)))
    print("--------------------------------")
}

save(NGR, TEST, minoccs, file="learned_data/learned_en_US_1000000.Rdata")

toc <- as.numeric(Sys.time())
print(paste("Total elapsed time in seconds:", round(toc-tic, digits=2)))
print("#####################################################################")




