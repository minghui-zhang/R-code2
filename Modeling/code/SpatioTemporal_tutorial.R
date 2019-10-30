# from vignette("ST_intro", package = "SpatioTemporal")

library(SpatioTemporal)
library(Matrix)
library(plotrix)
library(maps)
data(mesa.data.raw, package = "SpatioTemporal")

# Step 1. create STdata S3 object --------------------------------------------------

# obs is a dataframe with time on the rows and stations on the columns
# X is a dataframe with geographic covariates. columns are the variable names, rows are the station ID
# lax.conc.1500 is structured like obs, but with the outcome variable

# note, obs can also be given as a data.frame with the fields date, ID and obs giving observations,
# time points and location names
# obs can also be a matrix
# spatiotemporal covariates can be given either as a list of matrices or as a 3D array. 
# the row and column names should provide the dates and locations of the spatiotemporal covariates


mesa.data <- createSTdata(obs = mesa.data.raw$obs, 
                          covars = mesa.data.raw$X,
                          SpatioTemporal = list(lax.conc.1500 =mesa.data.raw$lax.conc.1500), n.basis = 2)

# plot location and timing of observations
plot(mesa.data, "loc", main = "occurrence of observations", xlab = "", ylab = "Location", 
     col = c("black", "red", legend.loc = NULL))

# plot qq plot
qqnorm(mesa.data, line = 1)

# plot scatterplot
scatterPlot(mesa.data, covar = "km.to.coast", xlab = "Distance to coast", ylab = "NOx", pch = 19, cex = 0.25, 
            smooth.args = list(span = 4/5, degree = 2))


# Step 2. temporal basis functions ----------------------------------------------------

# to estimate smooth teporal functions we construct a data matrix
D <- createDataMatrix(mesa.data)

# determining the number of basis functions. 
#the statistics flatten out after 2 basis functions, indicating that 2 basis functions is likely 
#to provide the most efficient description of the temporal variability
# look at autocorrelation when fitting the temporal basis to data at each location. if no autocorrelation, 
#indicates that you ahve sufficient number of basis functions 
# to capture the temporal structure (BUT do we need to capture temporal structure?)
SVD.cv <- SVDsmoothCV(D, 0:4)
print(SVD.cv)
plot(SVD.cv)

# add smooth temporal basis functions to STdata object
mesa.data <- updateTrend(mesa.data, n.basis = 2)

# alternatively cross validate temporal basis functions (but at one site)
smooth.trend <- calcSmoothTrends(mesa.data, n.basis=2, cv=TRUE)
mesa.data.cv <- vector("list", length(smooth.trend$trend.fnc.cv))
for(i in 1:length(mesa.data.cv)){
  suppressMessages(mesa.data.cv[[i]] <- updateTrend(mesa.data,
                                                    fnc=smooth.trend$trend.fnc.cv[[i]]))
  }
plot(mesa.data, main="Possible temporal trends",
        xlab="", ylab="NOx (log ppb)", pch=c(19,NA), cex=.25)
for(i in 1:length(mesa.data.cv)){
  plot(mesa.data.cv[[i]], add=TRUE, col=i, pch=NA, lty=c(NA,2))
}

# evaluate the basis functions with plot.STdata
# see how well the temporal basis functions capture the temporal structure
# no pattern in residuals and low ACF indicates the temporal basis functions capture the temporal variability in the data
par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(mesa.data, "obs", ID="60370113",
       xlab="", ylab="NOx (log ppb)",
      main="Temporal trend 60370113")
plot(mesa.data, "res", ID="60370113",
        xlab="", ylab="NOx (log ppb)")
plot(mesa.data, "acf", ID="60370113")
plot(mesa.data, "pacf", ID="60370113")

# could also specify deterministic basis functions, like f1(t) = 1, f2(t) = sin(t). 
# see the tutorial for this

# Step 3. specify the model ------------------------------------------------------
# add covariance and covariate specifications for the beta- and nu-fields to a STdata object

# they specify mean models for the beta fields with regression estimates of the betas, look at tutorial for details
LUR <- list(~log10.m.to.a1+s2000.pop.div.10000+km.to.coast, 
            ~km.to.coast, ~km.to.coast)
cov.beta <- list(covf="exp", nugget=FALSE)

# for nu-field, use exponential covariance
cov.nu <- list(covf="exp", nugget=~type, random.effect=FALSE)

# finally specify coordinates for the observations and create the STmodel object
# coords specifies which ones to use to calculate distances, while long.lat retains fields useful for plotting
locations <- list(coords=c("x","y"), long.lat=c("long","lat"),
                  others="type")
mesa.model <- createSTmodel(mesa.data, LUR=LUR, ST="lax.conc.1500",
                              cov.beta=cov.beta, cov.nu=cov.nu,
                              locations=locations)

# Step 4. parameter estimation -------------------------------------------------

# construct a matrix where each column represents a different optimisation starting point
dim <- loglikeSTdim(mesa.model)
x.init <- cbind(c( rep(2, dim$nparam.cov-1), 0),
                  c( rep(c(1,-3), dim$m+1), -3, 0))
rownames(x.init) <- loglikeSTnames(mesa.model, all=FALSE)

# estimate model parameters. this takes a long time to run, use precomputed results
#est.mesa.model <- estimate(mesa.model, x.init, type="p", hessian.all=TRUE)
data(est.mesa.model, package="SpatioTemporal")
print(est.mesa.model)

# Step 5. prediction ----------------------------------------------------------
pred <- predict(mesa.model, est.mesa.model, LTA=TRUE, type="p")
pred.log <- predict(mesa.model, est.mesa.model, LTA=TRUE,
                      transform="unbiased", type="p")

# Step 6. cross validation and model evaluation -------------------------------

