% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appendLibLog.R
\name{appendLibLog}
\alias{appendLibLog}
\title{Append a command to the Log tab dealing with namespaces}
\usage{
appendLibLog(comment, ..., include.libs = TRUE)
}
\arguments{
\item{comment}{A message to include as a comment.}

\item{...}{The command(s) to report in the log.}

\item{include.libs}{Include any required library() calls.

Report a command to the rattleM Log tab textview. We check the
commands for any namespace usage and then include an appropriate
library() call for each and remove them from the commands
themselves.

Each command will be printed on a new line.}
}
\description{
Time-stamp: <2016-09-19 11:30:05 Graham Williams>
}
