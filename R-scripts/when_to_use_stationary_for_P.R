library(Matrix)

f <- function(a, nu){
  lambda <- matrix(c(-a, 1-a, a, a-1), nrow=2)
  
  return(
    expm(lambda  * nu)
  )
}


stationary <- function(a){
  return(
    matrix(
      c(1-a, 1-a, a, a), nrow = 2
    )
  )
}

a <- seq(0, 1, length.out = 11)

l_nu <- seq(-4, 10, length.out=100)

res <- list()
err <- matrix(0, nrow = length(l_nu), ncol = length(a))

for(j in 1:length(a)){
  res[[j]] <- matrix(0, nrow=length(l_nu), ncol = 4)

  st <- stat(a[j])

  for(i in 1:length(l_nu)){
    me <- f(a[j], exp(l_nu[i]))
    res[[j]][i,] <- as.numeric(me)
    err[i,j] <- max(abs(me - st))
  }
}


# plot matrix
plot(0, type='l', xlim=range(l_nu), ylim=c(0,1), xlab="log nu")
for(i in 1:4){
  lines(l_nu, res[[1]][,i], col = i)
}

# plot error
plot(0, xlim=range(l_nu), ylim=c(-20, 0), type = 'n', xlab = "log nu", ylab = "log10 error")
for(j in 1:length(a)){
  lines(l_nu, log10(err[,j]), col = j)
}
abline(h = -10, lty =2)

threshold <- max( apply(log10(err), 2, function(x){ min(which(x < -10)) }) )

print(paste0("Use stationary once nu > ", exp(l_nu[threshold])))
