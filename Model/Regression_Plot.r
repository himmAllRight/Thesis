# Plots a 2nd and 3rd degree linear regression
# for the input data.

library(stringr)	# String Library

# Use command arg to set input file name
args      <- commandArgs(trailingOnly = TRUE)
inputFile <- args[1]
degreeMax <- args[2] 	# What degree the regressions should go to (up to 6)
inputType <- ".txt"

plotName  <- paste(unlist(str_split(inputFile,inputType))[1], "_RegressionPlot.png", sep="")

# Read in Data
data <- read.csv(inputFile, header = TRUE, sep="\t")

# Fit the polynomial regressions
fit1 <- lm(data$Sws2 ~ data$step)
fit2 <- lm(data$Sws2 ~ data$step + I(data$step^2))
fit3 <- lm(data$Sws2 ~ data$step + I(data$step^2) + I(data$step^3))
fit4 <- lm(data$Sws2 ~ data$step + I(data$step^2) + I(data$step^3) + I(data$step^4))

# Alternative way to writting the regressions.
#fit2b <- lm(data$Sws2 ~ poly(data$step, 2, raw = TRUE))
#fit3b <- lm(data$Sws2 ~ poly(data$step, 3, raw = TRUE))
fit5 <- lm(data$Sws2 ~ poly(data$step, 5, raw = TRUE))
fit6 <- lm(data$Sws2 ~ poly(data$step, 6, raw = TRUE))


# Print the summaries of the regressions
print("Second Degree Regression")
print("------------------------")
summary(fit2)

print("Third Degree Regression")
print("-----------------------")
summary(fit3)

print("Fourth Degree Regression")
print("-----------------------")
summary(fit4)

print("Fifth Degree Regression")
print("-----------------------")
summary(fit5)

print("Sixth Degree Regression")
print("-----------------------")
summary(fit6)

# Run an ANOVA on the fits
print("ANOVA of the two Fits")
print("---------------------")
anova(fit2, fit3)

# Plot Sws data and Regressions
png(plotName)
plot(data$step, data$Sws2, type="l", lwd=3)

# Add second degree fit to plot
points(data$step, predict(fit2), type="l", col="red", lwd=2, label="2nd Degree")

if(degreeMax > 2){
	# Add third degree fit to plot
	points(data$step, predict(fit3), type="l", col="blue", lwd=2)
}

if(degreeMax > 3){
	# Add fourth degree fit to plot
	points(data$step, predict(fit4), type="l", col="green", lwd=2)
}

if(degreeMax > 4){
	# Add fifts degree fit to plot
	points(data$step, predict(fit5), type="l", col="orange", lwd=2)
}

if(degreeMax > 5){
	# Add sixth degree fit to plot
	points(data$step, predict(fit6), type="l", col="cyan", lwd=2)
}

dev.off()