\name{install_extras}
\alias{install_extras}
\title{Install the optional packages of PathLab SEM}
\description{
Installs the optional packages: \pkg{bibliometrix} and \pkg{bibliometrixData} (Bibliometric analysis; on Linux they
need Poppler, through \pkg{pdftools}), \pkg{cSEM} (GSCA analyses; on Linux it needs GMP, through \pkg{symmoments}),
\pkg{RWeka} and \pkg{rJava} (WEKA learners; they also need a 64-bit Java), \pkg{av} (audio and video recordings),
\pkg{pdftools} (PDF documents) and \pkg{blavaan} (Bayesian CB-SEM; it installs \pkg{rstan}, a large download).

Every feature that needs one of these says so in the app, naming the package, so an install without them works
everywhere else.
}
\usage{
install_extras(which = c("bibliometrix", "bibliometrixData", "cSEM", "RWeka", "rJava", "av", "pdftools", "blavaan"),
               repos = getOption("repos"))
}
\arguments{
\item{which}{The optional packages to install (those already installed are skipped).}
\item{repos}{Repositories to install from (CRAN by default).}
}
\value{The packages that were installed, invisibly.}
