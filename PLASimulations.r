#-------------------------
# Use the functions 
# in the other script to 
# set up and run a 
# perceptron Learning algorithm
# with d = 2

# pla2 - function PLA with d = 2

# the true function
f <- pla2(2,2,-1,1)

# parameters of that line
params <- pla2.params(f)

# generate our N random samples
N <- 10
X <- pla2(2,N,-1,1)

# those points outcome vs. f found aboev
Y <- pla2.outcomes(X,params)

# initialize wts 
wts <- c(0,0,0)

# add a vector of ones 
X <- cbind(X,1)

xs <- TRUE
iteration <- 0
while(xs){
	iteration <- iteration + 1
	wts <- pla2.eval(X,wts,Y)
	xs  <- !all(calcSign(X,wts) == Y)
}
iteration


# Now run this experiment 1000 times and take the varage 
i <- 1
output.N10 <- data.frame(Trial= 1:100,Iteration = 0,Prob=0)
for(i in 1:100){
	# the true function
	f <- pla2(2,2,-1,1)
	# parameters of that line
	params <- pla2.params(f)
	# generate our N random samples
	N <- 10
	X <- pla2(2,N,-1,1)
	# those points outcome vs. f found aboev
	Y <- pla2.outcomes(X,params)
	# initialize wts 
	wts <- c(0,0,0)
	# add a vector of ones 
	X <- cbind(X,1)
	
	# run PLA to create g(X)
	xs <- TRUE
	iteration <- 0
	while(xs){
		iteration <- iteration + 1
		wts <- pla2.eval(X,wts,Y)
		xs  <- (!all(calcSign(X,wts) == Y) & (iteration < 1000))
	}
	output.N10[i,"Iteration"] <- iteration
	
	# Generate 1000 points and determine the prob that they are 
	# predicted correctly

	#XTest <- pla2(2,1000,-1,1)
	#XTest <- cbind(XTest,1)
	#YTest <- pla2.outcomes(XTest,params)
	#output.N10[i,"Prob"] <- mean(YTest == calcSign(XTest,wts))
}





