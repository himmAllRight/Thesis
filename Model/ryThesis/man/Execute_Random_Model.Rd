\name{Execute_Random_Model}
\alias{Execute_Random_Model}
\title{Runs the execution code for the Random Model}
\description{Runs the execution code for the Random Model. Used to be
  the set of code at the end.}
\usage{
Execute_Random_Model(topFolder, name, dimension, size, nei, p, hubThreshold, trialCount, timesteps, n = 10, d = 2)
}

\arguments{
  \item{topFolder}{
The top directory of the model run.
}
  \item{name}{
The name of the model run? It's been so long since I used this code....
}
  \item{dimension}{
The dimension parameter for generating the random graph.
}
  \item{size}{
The size parameter for generating the random graph.
}
  \item{nei}{
The nei parameter for generating the random graph.
}
  \item{p}{
The probability parameter for generating the random graph. Determines
  the rewiring probaility when converting the generated lattice into an
  ER random graph.
}
  \item{hubThreshold}{
The threshold value for determining hubs. (I believe in standard deviations...)
}
  \item{trialCount}{
The number of trials to run the model.
}
  \item{timesteps}{
The number of steps to run the model in each trial.
}
  \item{n}{
The n parameter for generating clustered multi plots. Determines the
  number of sub graphs to cluster.
}
  \item{d}{
The branching factor of the clustered multi plots. Determines how many
  connections the graphs have to each other.
}
}
\details{
It has been a LONG time since I actively worked with these functions. I
  will clean up the package after I actually port the functions to the package.
}
\value{
No value is actually returned. This is just a function to set the
  parameters of a model and control the run loop.
}
\references{

}
\author{
  Ryan Himmelwright
}


\seealso{

}
\examples{
  print("To add later. Once I ensure it works.")
}