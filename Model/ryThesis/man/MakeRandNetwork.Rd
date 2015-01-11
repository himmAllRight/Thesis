\name{MakeRandNetwork}
\alias{MakeRandNetwork}
\title{Makes a random netowrk.}
\description{Makes a random network using the erdos.renyi.game igraph function.}
\usage{
MakeRandNetwork(nodeCount, edgeCount)
}
\arguments{
  \item{nodeCount}{
The number of nodes to be in the random graph.
}
  \item{edgeCount}{
The number of edges to be in the random graph.
}
}
\value{
A erdos.renyi random network igraph object.
}

\references{
ER graphs, erdos.renyi.game
}
\author{
Ryan Himmelwright
}
\examples{
MakeRandNetwork(100,200)
}
