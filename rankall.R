rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomeAll <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that outcome are valid
    if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("Error: invalid outcome")
    }

    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

    #Validate the specified rank number
    if(num == "best") {
        num <- 1
    }
    else if(num == "worst") {
        num <- nrow(outcomeAll)
    }
    else if(is.numeric(x=num)) {
        if(num<1 || num > nrow(outcomeAll)) {
            stop('invalid num')
        }
    }
    else {
       stop('invalid num')
    }

    if (outcome == "heart attack") {
       dataset <- subset(outcomeAll, select = c(2, 7, 11))
    }
    else if (outcome == "heart failure") {
       dataset <- subset(outcomeAll, select = c(2, 7, 17))
    }
    else if (outcome == "pneumonia") {
       dataset <- subset(outcomeAll, select = c(2, 7, 23))
    }

    dataset[, 3] <- suppressWarnings(as.numeric(dataset[, 3]))
    colnames(dataset) <- c("hospital", "state", "mortality_rate")

    state_groups <- split(dataset, dataset$state)

    all_state_ranks = lapply(splited, function(x, num) {
           # Order by mortality_rate and hospital
           x <- x[order(x$mortality_rate, x$hospital),]
    
           return (x$hospital[num])
         }, num)

    final_dfrm <- data.frame(hospital=unlist(all_state_ranks), state=names(all_state_ranks))

    final_dfrm
}
