data("Produc", package = "Ecdat")
data("usaww") # matrix

library(spdep)

usalw <- mat2listw(usaww) # listw

fm <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp
sararremod <- spml(formula = fm, data = Produc, index = NULL, 
                   listw = usalw, model = "random", lag = TRUE, spatial.error = "b")

summary(sararremod)

library(splm)
library(plm)

df <- data.frame(y = c(1,2,3,4,1,1.7,2.5,3.7), x = c(10,5,20,10,12,7,18,11))
df$time <- rep(c(1,2), each = 4)
df$ind <- rep(1:4, 2)
df <- df[,c("ind","time", "y", "x")]
df

# matrix for spatial estimation - Spatial Panel Autoregressive Model
W <- matrix( c( 0,0,1,1,
                0,0,1,1,
                1,1,0,5,
                0,1,10,0), nrow = 4)
W <- sweep(W, 1, rowSums(W), "/")
listw <- mat2listw(W, style = "W")

# spatial estimation
res_spatial <- spml(y ~ x, data = df, listw = listw, effect = "individual",
                    model="within", spatial.error="none", lag = T)


# FIXED EFFECTS WITH INTERCEPT -> WHY??
effects.splm(res_spatial)