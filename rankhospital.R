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

    ## Return hospital name in that state with the given rank 30-day death rate
    mortality_vec <- c("heart attack", "heart failure", "pneumonia")
    col_pos <- c(11, 17, 23)

    #Choose the column index from the data set for the specified outcome 
    col_ix <- which(mortality_vec == outcome)


    #Ordered list of hospitals with mortality rates for the specified outcome
    hospital_list <- data_state[order(data_state[,col_pos[col_ix]], data_state$Hospital.Name),]

    #Get name of the hospital with specified ranking
    hospital_name <- hospital_list[num,]$Hospital.Name

    hospital_name
}
