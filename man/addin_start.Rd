\name{addin_start}
\alias{addin_start}
\alias{addin_viewer}
\title{Start PathLab SEM from the RStudio Addins menu}
\description{
The functions behind the RStudio Addins menu items \emph{Start PathLab SEM} (web browser) and
\emph{Start PathLab SEM in the Viewer} (Viewer pane). Both keep R free while the app runs; stop it with
\emph{Stop PathLab SEM} or \code{\link{stop_app}()}.
}
\usage{
addin_start()
addin_viewer()
}
\value{The address of the app, invisibly.}
\seealso{\code{\link{run}}}
