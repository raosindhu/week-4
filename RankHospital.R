rankhospital <- function(state, disease, rank){
  i <- 0
  if(toupper(disease) == "HEART ATTACK")
    i <- 11
  else if(toupper(disease) == "HEART FAILURE")
    i <- 18
  else if(toupper(disease) == "PNEUMONIA")
    i <- 23
  data <- read.csv(file = "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("Not Available"))
  subdata <- subset(data[, c(2, 7, i)], State == state)
  fdata <- subdata[complete.cases(subdata),]
  rankdata <- fdata[order(fdata[,3], fdata[,2], na.last = TRUE),]
  if (is.numeric(rank))
    rankdata[rank,1]
  else if (rank == "worst")
    rankdata[nrow(rankdata),1]
}