best <- function(state, outcome) {
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

    ## Return hospital name in that state with the given rank 30-day death rate
    mortality_vec <- c("heart attack", "heart failure", "pneumonia")
    outcomeAll_col_pos <- c(11, 17, 23)

    #Choose the column index from the data set for the specified outcome 
    col_ix <- which(mortality_vec == outcome)

    #Get a vector of 30 day rates of all hospital for the specified outcome
    mortality_rate <- suppressWarnings(as.numeric(outcomeAll[,outcomeAll_col_pos[col_ix]]))
    
    #Order all hospitals from all states for the specified outcome
    orderHospitalList <- outcomeAll[order(mortality_rate, outcomeAll[,2]),]

    #Pick the top hospital for the specified outcome for the specified state
    best_hospital <- orderHospitalList[orderHospitalList$State==state,][,2][1]
    best_hospital
}
