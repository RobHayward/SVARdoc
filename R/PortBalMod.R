# PB model from blanchard
# intervention can offset from none to all of the capital inflow
int = seq(0, 1, 0.1)
# starting point for effect of capital flow on exchange rate
rho = 1
# gross outflows respond one-to-one with inflows when confronted by shocks. 
a = b = 0.5
# Effect of growss inflows and gross outflows on the exchange rate. 
w = 0.2
# AR to exogenous capital flows (z)
g = 0.2
# sensitivity of current account to exchange rate
d = 0
# central bank response to overseas-domestic interest rate differential.
e <- (1 + rho)*(1 - int)/((a + b)*(1 - d - w) + g)
plot(int, e, type = 'l', main = "Effect of intervention when domestic risk
     aversion changes", lty = 1)
rho = 0
e <- (1 + rho)*(1 - int)/((a + b)*(1 - d - w) + g)
lines(int, e, type = 'l', col = "blue", lty = 2)
rho = -1
e <- (1 + rho)*(1 - int)/((a + b)*(1 - d - w) + g)
lines(int, e, type = 'l', col = "red", lty = 3)
legend = legend("topright", c("rho = 1", "rho = 0", "rho = -1"), 
                col = c("black", "blue",  "Dark Green"), 
                lty = c(1, 2, 3)) 
