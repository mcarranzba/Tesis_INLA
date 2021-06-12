m <- 10
theta <- rbeta(1,3,10)
n <- 5
#rbinomV <- Vectorize(FUN = rbinom, vectorize.args = c("prob"))
DD <- rbinom(m,n,theta)

mean(DD/n)

bar_x <- mean(DD)
S_x <- ((n-1)/n)*var(DD)
hat_Var_x <- (1/n)*(bar_x)*(n-bar_x)
delta <-(hat_Var_x-(1/n)*S_x)/(S_x-hat_Var_x)
delta
hat_alpha <- bar_x*delta 
hat_beta <- (n-bar_x)*delta
Dom <- seq(0,1,0.01)
Ran <- dnorm(Dom,bar_x/n, sqrt((1/(n*m))*(bar_x/n)*(1-bar_x/n)))
plot(Dom,Ran)
Ran <- dbeta(Dom, hat_alpha + m*bar_x, hat_beta +n*m-(m*bar_x))
lines(Dom,Ran)
Ran <- dbeta(Dom, hat_alpha + 2*m*bar_x, hat_beta +n*2*m-(2*m*bar_x))
lines(Dom,Ran)

MM <- (hat_alpha+ m*bar_x )/(hat_alpha + m*bar_x+hat_beta+n*m-(m*bar_x))
MM

dbeta(Dom, hat_alpha + 2*m*bar_x, hat_beta +2*n*m-(m*bar_x))
