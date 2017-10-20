
# beg - beginning of word
# v - full word
# nouts - number of outputs

# output: nouts top words 

prediction <- function(beg, v, nouts=3) {
    
    indices <- grep(pattern=paste0("^", beg), dimnames(P1)[[2]]) # if beg is empty, all words are matched
    
    if (v %in% dimnames(P1)[[1]]) {
        # transition probabilities from v to a word starting with beg:
        p1 <- P1[v, dimnames(P1)[[2]][indices]] 
        # probability of having a word starting with beg after v:
        p2 <- sum(P1[v, dimnames(P1)[[2]][indices]])
        
    } else {
        indices2 <- grep(pattern=paste0("^", beg), names(prior))
        p1 <- prior[indices2]
        p2 <- sum(prior[indices2])
    }
    
    # normalizing:
    output <- head(sort(p1/p2, decreasing = TRUE), n=nouts)
    
    return(output)
}