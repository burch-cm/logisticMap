# linear function iterative mapping
linearMap <- function(c = 4, lambda = 0.4, x){
    return(c * lambda * x * (1-x))
}
curve(linearMap(4, 0.4, x), 0, 1, xaxs="i", yaxs="i", ylim = c(0, 0.5))

getLMap <- function(c = 4, lambda = 0.4, x = 0.1, n = 10){
    out <- c(linearMap(c, lambda, x))
    for(i in 2:n){
        out[i] <- linearMap(c, lambda, x = out[i-1])
    }
    out_df <- data.frame(iter = c(1:n), value = out)
    return(out_df)
}
plot(getLMap(n = 10)$value, type="l", xaxs="i")

lambda_val <- seq(from = 0.2, to = 0.8, by = 0.05)
x <- list()
for(j in 1:length(lambda_val)){
    r_val = paste0("r_", as.character(lambda_val[j]))
    x[[r_val]] <- getLMap(4, lambda_val[j], 0.1, 10)
}
