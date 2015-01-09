\name{MakeSWPNetwork}
\alias{MakeSWPNetwork}
\title{Makes a Small World Network}
\description{Generates a Small World network using the watts.strogatz.game function from the igraph library.
}
\usage{
MakeSWPNetwork(dimension, size, nei, p)
}
\arguments{
  \item{dimension}{
Integer constant, the dimension of the starting lattice.
}
  \item{size}{
Integer constant, the size of the lattice along each dimension.
}
  \item{nei}{
Integer constant, the neighborhood within which the vertices of the lattice will be connected.
}
  \item{p}{
Real constant between zero and one, the rewiring probability.
}
}
\value{
Returns a watts.strogatz.game igraph graph object.
}
\references{
igraph
}
\author{
Ryan Himmelwright
}


\examples{
MakeSWPNetwork(3,2,1,.25)
}
