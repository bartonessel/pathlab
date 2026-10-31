\name{check_setup}
\alias{check_setup}
\title{Check the PathLab SEM setup}
\description{
Lists required and optional packages that are not installed, and whether Java (for the WEKA learners)
and Quarto (for Word reports) are available.
}
\usage{
check_setup()
}
\value{A list (invisibly) with \code{missing}, \code{extras}, \code{java} and \code{quarto}.}
\examples{
\dontrun{
pathlab::check_setup()
}
}
