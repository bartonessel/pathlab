\name{run}
\alias{run}
\title{Start PathLab SEM}
\description{
Starts PathLab SEM on this computer and opens it in your web browser, in the way
\code{bibliometrix::biblioshiny()} does. Your data stay on your computer: the app runs in a separate
R process that only this computer can reach (\code{127.0.0.1}).
}
\usage{
run(port = NULL, host = "127.0.0.1", launch.browser = TRUE, viewer = FALSE, background = FALSE, timeout = 180)
}
\arguments{
\item{port}{Port number. \code{NULL} (default) picks a free port.}
\item{host}{Address to listen on. Keep \code{"127.0.0.1"} so that only this computer can open the app.}
\item{launch.browser}{Open the app (in the web browser, or the Viewer pane with \code{viewer = TRUE}).}
\item{viewer}{Open the app in the Viewer pane of RStudio or Positron instead of the web browser.}
\item{background}{\code{FALSE} (default): R waits while the app runs; press Esc (RStudio) or Ctrl+C to stop it.
\code{TRUE}: the app keeps running while you use R; stop it with \code{\link{stop_app}()}.}
\item{timeout}{Seconds to wait for the app to start.}
}
\value{The address of the app, invisibly.}
\details{
Save your work with the app's Save button: a project lives in memory while the app runs, and saved
\code{.plj} project files open again later. Run \code{\link{check_setup}()} if the app does not start.

In RStudio the Addins menu has \emph{Start PathLab SEM}, \emph{Start PathLab SEM in the Viewer} and
\emph{Stop PathLab SEM}. On RStudio Server and Posit Cloud the app opens at the address RStudio forwards
to it.
}
\examples{
\dontrun{
pathlab::run()
}
}
