# Load library
library(deSolve)

# Define logistic growth function
logistic <- function(t, y, param){
    with(as.list(c(y, param)),{
        dN <- r * N * (1 - N / k)
         list(c(dN))
    })
}

# Set initial values
init <- c(N = 100)
param <- c(r = .3, k = 300)
times <- seq(from = 0, to = 100, by = 0.01)

# Solve logistic growth function
out <- lsoda(y = init,
             times = times,
             func = logistic,
             parms = param)
# View (part of) results
head(out)
# Plot results
plot(N ~ time, data = out, type = "l")

# Generate "samples" from the model and fit to samples (as a check on fitting procedure)
## First, generate relevant time slices
datatimes <- seq(from = 0, to = 10, by = 1)
## Now subset output of model by these time slices
data.base <- out[which(out[, 1] %in% datatimes), ]
## Add some noise to underlying data
data <- data.base
data[, 2] <- rnorm(n = nrow(data.base), mean = data.base[, 2], sd = 5)
## Let's do this a few times to generate multiple samples (each column in data is a sample)
for(i in 1:20){
    data <- cbind(data, data.frame(N = rnorm(n = nrow(data.base), mean = data.base[, 2], sd = 5)))
}
## View data
data

# Define a flexible least squares function
leastsquares <- function(param, data){
    ndata <- ncol(data) - 1 # number of samples
    stor <- dim(ndata) # for storing sums of squares 
    for(i in 1:ndata){
        # Solve the model
        out.temp <- lsoda(y = c(N = data[1, i + 1]), # initial pop size
                      times = seq(0, data[nrow(data), 1], by = 0.01), # times for solver
                      func = logistic, # model to solve
                      parms = param) # parameters to use
        out2 <- out.temp[which(out.temp[, "time"] %in% data[, "time"]), ] # subset by times of data
        stor[i] <- sum((out2[, "N"] - data[, i]) ^ 2) # sum of the squared difference
    }
    return(sum(stor)) # sum of sum of squares
}

# Now optimize (find values of r and k at that minimize output of leastsquares
# Here we use default method; consider using L-BFGS-B if you want to set lower
# upper bounds are the parameters
#
# Starting values VERY important
fit <- optim(par = c(r = .2, k = 500),
             fn = leastsquares,
             data = data)
# Summarize fit
fit
# Compare best-fit with true values
fit$par
param
# Pretty good!
