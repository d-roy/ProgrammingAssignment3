plot_30d_outcome <- function () {
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome[, 11] <- supppressWarnings(as.numeric(outcome[, 11]))
    hist(outcome[, 11])
}
