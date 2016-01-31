rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomeAll <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Get the state names from the data set 
    state_names <- unique(outcomeAll[, 7]) ## column# 7 in outcome is state
    
    ## Check that state and outcome are valid
    if (!is.element(state, state_names)) {
        stop("Error: invalid state") 
    }
    
    if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("Error: invalid outcome")
    }

    #Subset data for the specified state
    data_state <- outcomeAll[outcomeAll$State == state,]

    #Validate the specified rank number
    if(num == "best") {
        num <- 1
    }
    else if(num == "worst") {
        num <- nrow(data_state)
    }
    else if(is.numeric(x=num)) {
        if(num<1 || num > nrow(data_state)) {
            stop('invalid num')
        }
    }
    else {
      stop('invalid num')
    }

    if (outcome == "heart attack") {
       dataset <- subset(data_state, select = c(2, 7, 11))
    }
    else if (outcome == "heart failure") {
       dataset <- subset(data_state, select = c(2, 7, 17))
    }
    else if (outcome == "pneumonia") {
       dataset <- subset(data_state, select = c(2, 7, 23))
    }

    dataset[, 3] <- suppressWarnings(as.numeric(dataset[, 3]))
    colnames(dataset) <- c("hospital", "state", "mortality_rate")
    hospital_list <- dataset[order(dataset$mortality_rate, dataset$hospital),]
    hospital_name <- hospital_list[num,]$hospital

    hospital_name
}
