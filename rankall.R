rankall <- function(disease, rank){
  
  i <- 0
  if(toupper(disease) == "HEART ATTACK")
    i <- 11
  else if(toupper(disease) == "HEART FAILURE")
    i <- 18
  else if(toupper(disease) == "PNEUMONIA")
    i <- 23
  num <- rank
  data <- read.csv(file = "outcome-of-care-measures.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("Not Available"))[,c(2,7,i)]
  #data <- subset(data[, c(2, 7, i)])
  names(data) <- c("hospital", "state", "disease")
  data <- data[complete.cases(data),]
  #rankdata <- tapply(fdata, fdata$state, order )
  s <- split(data, data$state)
  rankdata <- lapply(s, function(x, rank) {
    rdata <- x[order(x$disease),] 
    if (is.numeric(rank))
      return(rdata$hospital[rank])
    else if (rank == "worst")
      return(rdata$hospital[nrow(rdata)])
  }, rank)
    
  return ( data.frame(hospital=unlist(rankdata), state=names(rankdata)) )
  }

