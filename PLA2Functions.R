#-----------------------------------
# Functions for Perceptron 
# learning algorithm 

# functin for generating a matrix with variables 
# From the uniform distribution

pla2 <- function(nCol,nPoints,aMin,aMax){
  X <- rep(0,nPoints)
  for(i in 1:nCol){
    X <- cbind(X,runif(nPoints,aMin,aMax))
  }
  colnames(X) <- c("X.0",paste("X",1:nCol,sep="."))
  return(array(X[,-1],dim=c(nPoints,nCol)))
}

# function for generating the eqn of a line between two points 
pla2.params <- function(X){
	params <- glm(X[,2] ~ X[,1])$coef
	out <- list()
	out$m <- params[2]
	out$b <- params[1]
	return(out)
}

# function for evaluating the true outcome 
# of a specific point -1 or 1

pla2.outcomes <- function(X,params){
	return(
		sign(X[,1]*params$m + params$b - X[,2])
	)
}

# function for calculating the current prediction
calcSign <- function(X,wts){
	pred <- as.matrix(X[,]) %*% wts
	return(sign(pred))
}

# function to perform 1 iteration of the PLA
pla2.eval <- function(X,w,y){
	preds 	<- calcSign(X,w)
	temp 		<- preds != y
	z <- (1:nrow(X))[temp]
	if(len(z) > 1){
		temp 		<- sample(z,1)
	}
	else
		temp <- z
	w 		<- w + X[temp,]*y[temp]
	return(w)
}
	







