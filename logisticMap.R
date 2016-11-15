---
title: "Logistic Map in R"
author: "Kitt Burch"
date: "September 30, 2016"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## The Logistic Map
The logistic map is a function which models population over time. It was developed by Robert May, and is a discreete-time, unit-free version of Verhult's "Logistic Equation". The logistic map is often-studied example of how deterministic, non-linear functions can generate chaotic and complex behavior.  

The logistic map function takes the form $x_{t+1} = Rx_t(1-x_t)$  
where:  
$R$ = growth factor (reproductive constant)  
$x_t$ = ratio of population to maximum population at time $t$

```{r libs}
library(magrittr)
library(ggplot2)
```

## Define the logistic map as a function

```{r logisticMap}
logisticMap <- function(r, x0, n){
    x <- c(x0)
    for(i in 1:n){
        x[i+1] <- r * x[i] * (1 - x[i]) # the logistic map
    }
    return(x)
}
```

## Behavior

The behavior of the logistic map is primarily dependent on the reporductive constant $R$. For example, despite starting at different population levels, multiple systems with the same $R$ value will converge over time.

```{r, echo = FALSE}
x_0.25 <- logisticMap(r = 2.1, x0 = 0.25, n = 10) %>% data.frame(t = 1:length(.), x_t = ., x_val = rep(0.25, length(.)))
x_0.35 <- logisticMap(r = 2.1, x0 = 0.35, n = 10) %>% data.frame(t = 1:length(.), x_t = ., x_val = rep(0.35, length(.)))
x_0.45 <- logisticMap(r = 2.1, x0 = 0.45, n = 10) %>% data.frame(t = 1:length(.), x_t = ., x_val = rep(0.45, length(.)))
x_0.55 <- logisticMap(r = 2.1, x0 = 0.55, n = 10) %>% data.frame(t = 1:length(.), x_t = ., x_val = rep(0.55, length(.)))
x_0.65 <- logisticMap(r = 2.1, x0 = 0.65, n = 10) %>% data.frame(t = 1:length(.), x_t = ., x_val = rep(0.65, length(.)))
x_0.75 <- logisticMap(r = 2.1, x0 = 0.75, n = 10) %>% data.frame(t = 1:length(.), x_t = ., x_val = rep(0.75, length(.)))
dat <- rbind(x_0.25, x_0.35, x_0.45, x_0.55, x_0.65, x_0.75)
ggplot(dat, aes(x = t, y = x_t, group = x_val)) + geom_line(aes(color = factor(x_val))) + ggtitle("Convergence of systems with different initial values")
```

While diffent initial values of $x_0$ quickly converge on the same stable point for a given $R$ value (at least for this $R$ value), different values of $R$ will yield very diffent outcomes for the same starting value of $x_0$.  

```{r, echo = FALSE}
r_0.7 <- logisticMap(r = 0.7, x0 = 0.5, n = 50) %>% data.frame(t = 1:length(.), x_t = ., r_val = rep(0.7, length(.)))
r_1.1 <- logisticMap(r = 1.1, x0 = 0.5, n = 50) %>% data.frame(t = 1:length(.), x_t = ., r_val = rep(1.1, length(.)))
r_2.4 <- logisticMap(r = 2.4, x0 = 0.5, n = 50) %>% data.frame(t = 1:length(.), x_t = ., r_val = rep(2.4, length(.)))
r_3.1 <- logisticMap(r = 3.1, x0 = 0.5, n = 50) %>% data.frame(t = 1:length(.), x_t = ., r_val = rep(3.1, length(.)))
dat <- rbind(r_0.7, r_1.1, r_2.4, r_3.1)
ggplot(dat, aes(x = t, y = x_t, group = r_val)) + geom_line(aes(color = factor(r_val)))
```

When $R = 0.7$, the population slowly declines to zero as the birth rate is too low to compensate for the death rate. When $R = 1.1$ the system finds an equilibrium at a low $x$ value. At $R = 2.4$ the system quickly settles on a stable, higher, value of $x$. When $R = 3.1$, however, the system does not settle on a single stable value, but oscillates between two fixed points. This represents a boom and bust cycle, with population spiking in response to plentiful resources, and then crashing due to overcrowding and starvation.  

## Chaotic behavior

The logistic map is said to have increasingly complex behavior for $R > 3$. A simple bifurcation plot will show that as $R$ increases, the system moves from a single fixed solution to multiple solutions. When $R$ is high enough, these solutions move from a pattern of stable oscillation between a number of fixed points to a seemingly-random and unstable number of solutions.  

Using a bifurcation plot, we can examine the behavior of the system over a range of $R$ values.
```{r bifurcation}
bifurcateLM <- function(r.from = 2.5, r.to = 4, r.num = 100, x0 = rand(1, 0, 1), x.num = 500){
    r_seq <- seq(from = r.from, to = r.to, length.out = r.num)
    r <- c()
    x_t <- c()
    x_from <- max(1, x.num - 100)
    for(i in 1:length(r_seq)){
        x_s <- unique(logisticMap(r = r_seq[i], x0 = x0, n = x.num)[x_from:(x.num+1)])
        r_s <- rep(r_seq[i], length(x_s))
        r <- c(r, r_s)
        x_t <- c(x_t, x_s)
    }
    return(data.frame(r_val = r, x_val = x_t))
}
x <- bifurcateLM(r.from = 2.8, r.to = 4, r.num = 1000, x0 = 0.5, x.num = 1000)
ggplot(x, aes(x = r_val, y = x_val)) + geom_point(size = .25)
```
  
This is the classic bifurcation plot associated with the logistic map. For $R$ values less than 3, there is only one stable solution. When $R > 3$ multiple stable solutions appear, with the number of stable points increasing as a function of $R$. When $R$ gets above about 3.6, chaotic behavior appears as the system no longer finds a stable solution, but oscillates between an increasing number of points. Especially interesting is the fact that "islands of normalcy" occur in the bifurcation where the chaos dies down and the system oscillates between a handful of points.

The logistic map is fascinating because it is a relatively simple deterministic function which generates complex, chaotic behavior. Chaos is not dependent on stochastic systems, but can occur as an emergent property of deterministic functions.