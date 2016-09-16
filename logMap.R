library(magrittr) # always
library(ggplot2)
library(ggthemes)

# linear function iterative mapping
linearMap <- function(c = 4, lambda = 0.4, x){
    return(c * lambda * x * (1-x))
}
curve(linearMap(4, 0.4, x), 0, 1, xaxs="i", yaxs="i", ylim = c(0, 0.5))

# the logistic map
lmap <- function(r = 3.2, x){
    return(r*x*(1-x))
}
curve(lmap(3.2, x), 0, 1)

# iterative logistic map - a Markov chain
logisticMap <- function(r, x0 = 0.5, nper = 100){
    x <- c(x0)
    for(i in 1:nper){
        x[i+1] <- r * x[i] * (1 - x[i])
    }
    return(x)
}

# apply logistic map over a group of r-values
rvals <- seq(0.4, 3.8, 0.2)
multix <- sapply(rvals, FUN = logisticMap) %>% data.frame
names(multix) <- paste0("rval_", as.character(rvals))
library(reshape2)
multix$iter <- c(1:nrow(multix))
m.melt <- melt(multix, id = c("iter"))
g <- ggplot(m.melt, aes(x = iter, y = value, group = variable, color = variable))
g + geom_line() + theme_tufte() + xlab("time") + ylab("population / carrying capacity") +
    ggtitle("Logisitc map population fraction over time for various r values")

# zoom in on r-values > 3
rvals_3_to_4 <- seq(3.0, 4.0, 0.1)
pv_r_3_4 <- sapply(rvals_3_to_4, FUN = logisticMap) %>% as.data.frame
names(pv_r_3_4) <- paste0("rval_", as.character(rvals_3_to_4))
pv_r_3_4$iter <- c(1:nrow(pv_r_3_4))
pv_melt <- melt(pv_r_3_4, id = c("iter"))
g2 <- ggplot(pv_melt, aes(x = iter, y = value, group = variable, color = variable))
g2 + geom_line() + theme_tufte() + xlab("time") + ylab("population / carrying capacity") +
    ggtitle("Logistic map population fraction for 3 > r > 4")

# how does N/k vary against r?
rv <- seq(2.5, 4, length = 1000)
