# Plots a 2nd and 3rd degree linear regression
# for the input data.

library(stringr)	# String Library

# Use command arg to set input file name
args        <- commandArgs(trailingOnly = TRUE)
inputFile   <- args[1]
degreeMax   <- args[2] 	# degree the regressions plots go to (up to 6)
inputType   <- ".txt"

plotName    <- paste(unlist(str_split(inputFile,inputType))[1],
			   "_RegressionPlot",degreeMax,".png", sep="")
outFileName <- paste(unlist(str_split(inputFile,inputType))[1], 
			   "_Regressions",degreeMax,".txt", sep="")

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
fit5  <- lm(data$Sws2 ~ poly(data$step, 5,  raw = TRUE))
fit6  <- lm(data$Sws2 ~ poly(data$step, 6,  raw = TRUE))
fit7  <- lm(data$Sws2 ~ poly(data$step, 7,  raw = TRUE))
fit8  <- lm(data$Sws2 ~ poly(data$step, 8,  raw = TRUE))
fit9  <- lm(data$Sws2 ~ poly(data$step, 9,  raw = TRUE))
fit10 <- lm(data$Sws2 ~ poly(data$step, 10, raw = TRUE))


# Print the summaries of the regressions
write("Second Degree Regression", file = outFileName, append = TRUE)
write("------------------------", file = outFileName, append = TRUE)
out <- capture.output(summary(fit2), file = NULL, append = FALSE)
write(out, file = outFileName, append = TRUE)


if(degreeMax > 2){
	write("Third Degree Regression", file = outFileName, append = TRUE)
	write("------------------------", file = outFileName, append = TRUE)
	out <- capture.output(summary(fit3), file = NULL, append = FALSE)
	write(out, file = outFileName, append = TRUE)
}

if(degreeMax > 3){
	write("Fourth Degree Regression", file = outFileName, append = TRUE)
	write("------------------------", file = outFileName, append = TRUE)
	out <- capture.output(summary(fit4), file = NULL, append = FALSE)
	write(out, file = outFileName, append = TRUE)
}

if(degreeMax > 4){
	write("Fifth Degree Regression", file = outFileName, append = TRUE)
	write("------------------------", file = outFileName, append = TRUE)
	out <- capture.output(summary(fit5), file = NULL, append = FALSE)
	write(out, file = outFileName, append = TRUE)
}

if(degreeMax > 5){
	write("Sixth Degree Regression", file = outFileName, append = TRUE)
	write("------------------------", file = outFileName, append = TRUE)
	out <- capture.output(summary(fit6), file = NULL, append = FALSE)
	write(out, file = outFileName, append = TRUE)
}

if(degreeMax > 6){
	write("Seventh Degree Regression", file = outFileName, append = TRUE)
	write("------------------------", file = outFileName, append = TRUE)
	out <- capture.output(summary(fit7), file = NULL, append = FALSE)
	write(out, file = outFileName, append = TRUE)
}

if(degreeMax > 7){
	write("Eigth Degree Regression", file = outFileName, append = TRUE)
	write("------------------------", file = outFileName, append = TRUE)
	out <- capture.output(summary(fit8), file = NULL, append = FALSE)
	write(out, file = outFileName, append = TRUE)
}

if(degreeMax > 8){
	write("Ninth Degree Regression", file = outFileName, append = TRUE)
	write("------------------------", file = outFileName, append = TRUE)
	out <- capture.output(summary(fit9), file = NULL, append = FALSE)
	write(out, file = outFileName, append = TRUE)
}

if(degreeMax > 9){
	write("Sixth Degree Regression", file = outFileName, append = TRUE)
	write("------------------------", file = outFileName, append = TRUE)
	out <- capture.output(summary(fit10), file = NULL, append = FALSE)
	write(out, file = outFileName, append = TRUE)
}


# Plot Sws data and Regressions
png(plotName)
plot(data$step, data$Sws2, type="l", lwd=3)

# Add second degree fit to plot
points(data$step, predict(fit2), type="l", col="yellow", lwd=2)

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

if(degreeMax > 6){
	# Add 7th degree fit to plot
	points(data$step, predict(fit7), type="l", col="purple", lwd=2)
}

if(degreeMax > 7){
	# Add 8th degree fit to plot
	points(data$step, predict(fit8), type="l", col="brown", lwd=2)
}

if(degreeMax > 8){
	# Add 9th degree fit to plot
	points(data$step, predict(fit9), type="l", col="red", lwd=2)
}

if(degreeMax > 9){
	# Add 10th degree fit to plot
	points(data$step, predict(fit10), type="l", col="black", lwd=2)
}
s
dev.off()
