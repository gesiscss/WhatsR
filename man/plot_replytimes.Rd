% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_replytimes.R
\name{plot_replytimes}
\alias{plot_replytimes}
\title{Plotting replytimes in WhatsApp chatlogs}
\usage{
plot_replytimes(
  data,
  names = "all",
  starttime = anytime("1960-01-01 00:00"),
  endtime = Sys.time(),
  return.data = FALSE,
  aggregate.sessions = TRUE,
  plot = "box",
  type = "replytime"
)
}
\arguments{
\item{data}{A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}}

\item{names}{A vector of author names that the plots will be restricted to}

\item{starttime}{Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".}

\item{endtime}{Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".}

\item{return.data}{If TRUE, returns a dataframe of LatLon coordinates extracted from the chat for more elaborate plotting. Default is FALSE.}

\item{aggregate.sessions}{If TRUE, concurrend messages of the same author are aggregated into one session. Default is TRUE.}

\item{plot}{Type of plot to be returned, options include "box", "dist" and "heatmap"}

\item{type}{If "replytime", plots display how much time it takes authors to reply to previous message, if "reactiontime", plots display how much time it takes for authors to get responded to}
}
\value{
Plots for Replytimes of Reactiontimes of authors
}
\description{
Visualizes the replytimes and reactiontimes to messages per author
}
\examples{
data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
plot_replytimes(data)
}
