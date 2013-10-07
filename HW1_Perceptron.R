#--------------------------------
# Perceptron Learning Algorithm

# Function for generating a matrix 
# with variables that hvae the uniform distribution

pla2 <- function(nCol,nPoints,aMin,aMax){
  X <- rep(0,nPoints)
  for(i in 1:nCol){
    X <- cbind(X,runif(nPoints,aMin,aMax))
  }
  colnames(X) <- c("X.0",paste("X",1:nCol,sep="."))
  return(data.frame(X[,-1]))
}

# Generate the true F function 
trueF <- pla2(2,2,-1,1)
class(trueF)

# function for creating a line: 
plaFunc <- glm("X.2~X.1",data=trueF)
        
X <- pla2(2,10,-1,1)

prodOutcomes <- function(ds,func){
  out <- predict(func,newdata=ds)
  out <- ifelse(out > ds[,2],-1,1)
  return(out)
}

outcomes <- prodOutcomes(X,plaFunc)

plot(trueF,type="l",xlim=c(-1,1),ylim=c(-1,1))
points(X[outcomes == 1,])
points(X[outcomes == -1,],pch="X")

#---------------------------------------------
# Now that we've looked at that, we have to 
# create and evaluate the perceptron algorithm

# Create a vector of ones 
X$X.0 <- rep(1,nrow(X))

calcProp <- function(X,wts,actual){
  newvec <- sign(rowSums(X*wts)) != actual
  return(newvec)
}

calcProp(X,w.vec,outcomes)

w.vec <- rep(0,3)
for(i in 1:10000){
  inds <- calcProp(X,w.vec,outcomes)
  cat("Iteration #",i,":","\n",
      "Correct: ",sum(!inds)/nrow(X),"\n")
  x.temp <- X[inds,]
  temp   <- sample(1:nrow(x.temp),1)
  x.temp <- x.temp[temp,]
  w.vec <- w.vec + as.numeric(x.temp*outcomes[temp])
}

as.matrix(X)%*%w.vec
z + w.vec
z <- matrix(as.numeric(x.temp*outcomes[temp]))

class(w.vec)

w.vec + x.temp*outcomes[temp]
dim(X)
class(X)
class(w.vec)