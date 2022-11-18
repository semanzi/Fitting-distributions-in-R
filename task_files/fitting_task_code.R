library(fitdistrplus)
library(ggplot2)

# Read in the data
df <- read.csv("fitting_task_1.csv", sep=",", header=TRUE)
#df <- read.csv("fitting_task_2.csv", sep=",", header=TRUE)

# Convert dataframe column to a vector
my_data <- df$data

# Create a scatter plot of the data
plot(my_data, pch=20)

# Create a histogram of the data
df = data.frame("data" = my_data)
ggplot(data=df) +
  geom_histogram(mapping=aes(x=data),bins=10,
                 col="black",
                 fill="grey")

# Plot empirical density and cumulative distribution
plotdist(my_data, histo = TRUE, demp = TRUE)

# Create a Cullen and Frey graph
descdist(my_data, discrete=FALSE, boot=500)

# Fit specific distributions
# Note: you will need to change the distribution names
# Name options: "norm", "lnorm", "pois", "exp", "gamma", "nbinom", "geom", "beta", "unif"
dists <- c("exp", "gamma", "lnorm")
fit <- list()
for (i in 1:length(dists)){
  fit[[i]]  <- fitdist(my_data, dists[i])
}

for (i in 1:length(dists)){
  print(summary(fit[[i]]))
}

#Plot the fitting results
#par(mfrow=c(2,2))
plot.legend <- dists
denscomp(fit, legendtext = plot.legend)
cdfcomp (fit, legendtext = plot.legend)
qqcomp  (fit, legendtext = plot.legend)
ppcomp  (fit, legendtext = plot.legend)

# Goodness of fit statistics
# Don't forget to match your fitnames argument to your named distributions above
f <- gofstat(fit, fitnames=c("exp", "gamma", "lnorm"))
f

#Estimating uncertainty in the parameters
for (i in 1:length(fit)){
  ests <- bootdist(fit[[i]], niter = 1e3)
  print(paste0("****",dists[i],"****"))
  print(summary(ests))
}
