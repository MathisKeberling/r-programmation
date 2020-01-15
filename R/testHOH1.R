H0H1 <- function(µ0,s0,µ1,s1,alpha) {
  res <- list(µ0=µ0,s0=s0,µ1=µ1,s1=s1,alpha=alpha)
  return(res)
}

####

data <- rnorm(100,mean = 10, sd = 2)

tester <- function(data,h0h1) {
  n <- length(data)
  xbar <- mean(data)
  s <- sd(data)
  z <- (xbar-h0h1$µ0)/(s/sqrt(n)) #realisation

  if(h0h1$µ0 < h0h1$µ1){r <- 1 - qnorm(1 - h0h1$alpha)}
  if(h0h1$µ0 > h0h1$µ1){r <- qnorm(h0h1$alpha)}

  res <- "on accepte H0"
  if(h0h1$µ0 < h0h1$µ1){if (z > r){res <- "on rejette H0"}}
  if(h0h1$µ0 > h0h1$µ1){if (z < r){res <- "on rejette H0"}}

  #calcul de la p-value et de la puissance



  if(h0h1$µ0 < h0h1$µ1){pvalue = 1 - pnorm(q = z)}
  if(h0h1$µ0 > h0h1$µ1){pvalue = pnorm(q = z)}
  resNew <- list(res = res, pvalue = pvalue)
  return(resNew)

}
h <- H0H1(0,5,1,5,0.05)
data <- rnorm(100,1,5)
tester(data,h)

hello()
#####♣ Faire affichage
