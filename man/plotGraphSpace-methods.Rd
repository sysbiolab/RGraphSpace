% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gspaceMethods.R
\docType{methods}
\name{plotGraphSpace,GraphSpace-method}
\alias{plotGraphSpace,GraphSpace-method}
\alias{plotGraphSpace}
\alias{plotGraphSpace,igraph-method}
\title{Plotting igraph objects with RGraphSpace}
\usage{
\S4method{plotGraphSpace}{GraphSpace}(
  gs,
  xlab = "Graph coordinates 1",
  ylab = "Graph coordinates 2",
  font.size = 1,
  theme = c("th1", "th2", "th3"),
  bg.color = "grey95",
  marks = FALSE,
  mark.size = 3,
  mark.color = "grey20"
)

\S4method{plotGraphSpace}{igraph}(gs, ..., mar = 0.075)
}
\arguments{
\item{gs}{Either an \code{igraph} or \linkS4class{GraphSpace} class object.
If \code{gs} is an \code{igraph}, then it must include \code{x}, \code{y}, 
and \code{name}  vertex attributes (see \code{\link{GraphSpace}}).}

\item{xlab}{The title for the 'x' axis of a 2D-image space.}

\item{ylab}{The title for the 'y' axis of a 2D-image space.}

\item{font.size}{A single numeric value passed to ggplot themes.}

\item{theme}{Name of a custom RGraphSpace theme. These themes 
(from 'th1' to 'th3') consist mainly of preconfigured ggplot settings, 
which the user can subsequently fine-tune within the resulting 
ggplot object.}

\item{bg.color}{A single color for background.}

\item{marks}{A logical value indicating whether to add 'marks' to vertex 
positions. Alternatively, this could be a vector listing vertex names.}

\item{mark.size}{A font size argument passed to \code{\link[ggplot2]{geom_text}}.}

\item{mark.color}{A color passed to \code{\link[ggplot2]{geom_text}}.}

\item{...}{Additional arguments passed to the 
\code{\link{plotGraphSpace}} function.}

\item{mar}{A single numeric value (in \code{[0,1]}) indicating the size of
the outer margins as a fraction of the graph space.}
}
\value{
A ggplot-class object.
}
\description{
\code{plotGraphSpace} is a wrapper function to 
create dedicated ggplot graphics for igraph- and GraphSpace-class objects.
}
\examples{
# Load a demo igraph
data('gtoy1', package = 'RGraphSpace')

# Generate a ggplot for gtoy1
plotGraphSpace(gtoy1)

# Create a GraphSpace object
gs <- GraphSpace(gtoy1)

# Generate a ggplot for gs
plotGraphSpace(gs)

}
\seealso{
\code{\link{GraphSpace}}
}
\author{
Sysbiolab.
}
