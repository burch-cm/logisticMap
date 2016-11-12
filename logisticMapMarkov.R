library(magrittr)
library(ggplot2)
#### ----- Functions -----
buildMarkov <- function(nstates = 20, p.remain = .25, from = 1, to = 4, names = FALSE){
    mrkv <- matrix(data = NA, nrow = nstates, ncol = nstates)

    getDist <- function(nstates){
        d <- matrix(data = 0, nrow = nstates, ncol=nstates)
        for(i in 1:nstates){
            for(j in 1:nstates){
                d[i,j] <- abs(j-i)
            }
        }
        return(d)
    }
    
    getWeights <- function(distMatrix){
        w <- matrix(data = NA, nrow = nrow(distMatrix), ncol = ncol(distMatrix))
        for(i in 1:nrow(distMatrix)){
            rs <- sum(distMatrix[i,])
            for(j in 1:ncol(distMatrix)){
                w[i,j] <- ifelse(i == j, 0, abs(distMatrix[i,j] - rs))
            }
        }
        return(w)
    }
    
    d <- getDist(nstates)
    w <- getWeights(d)
    rsw <- rowSums(w)
    for(i in 1:nstates){
        mrkv[i, ] <- (1-p.remain) * w[i, ]/rsw[i]
    }
    
    if(names){
        ns <- seq(from = from, to = to, length.out = nstates)
        rownames(mrkv) <- ns
        colnames(mrkv) <- ns
    }
    
    return(mrkv + diag(p.remain, nstates, nstates))
}

getMoves <- function(states, transition.matrix, nsteps, init = 0){
    current.state <- ifelse(init == 0, sample(states, 1), init) # initial random state
    init.state <<- current.state
    moves <- numeric(length = nsteps)
    for(i in 1:nsteps){
        # check the current row of the transition matrix
        row.c <- which(states == current.state)
        # draw a random number from 0 to 1
        rn <- runif(1)
        # compare number with cumulative sum of pr on selected row
        # get new state from transition matrix row
        current.state <- states[which(transition.matrix[row.c, ] >= rn)[1]]
        # save to output vector
        moves[i] <- current.state    
    }
    return(moves)
}

logisticMapMarkov <- function(moves, x0){
    x <- c(x0)
    for(i in 1:length(moves)){
        r <- moves[i] # the Markov-chosen r-value
        x[i+1] <- r * x[i] * (1 - x[i]) # the logistic map
    }
    return(x)
}

#### ----- Set variables -----
nstates = 40
start = 1
end = 4
pr.remain = .995
init = 1 # setting to 0 uses a random number
nsteps = 1000
#### ----- Run model -----
set.seed(1778)
prtrans <- buildMarkov(nstates, pr.remain, start, end) %>% apply(1, cumsum) %>% t
states <- seq(start, end, length.out = nstates)
init <- ifelse(init == 0, init, states[which(states >= init)[1]])
m <- getMoves(states, prtrans, nsteps, init)

vals <- logisticMapMarkov(m, x0 = 0.5)
#### ----- Plot outcomes -----
vals.dt <- data.frame(time = 1:length(vals), population = vals, r_val = c(init.state, m))
g1 <- ggplot(vals.dt, aes(x = time, y = population))
g1 + geom_point(col = "grey") + geom_line(col = "black")
