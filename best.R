best <- function(state, disease){
  i <- 0
  if(toupper(disease) == "HEART ATTACK")
    i <- 11
  else if(toupper(disease) == "HEART FAILURE")
    i <- 18
  else if(toupper(disease) == "PNEUMONIA")
    i <- 23
  data <- read.csv(file = "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("Not Available"))
  fdata <- subset(data[, c(2, 7, i)], State == state)
  bestvalue <- min(fdata[,3], na.rm = TRUE )
  best <- subset(fdata[,1], fdata[,3] == bestvalue)
}