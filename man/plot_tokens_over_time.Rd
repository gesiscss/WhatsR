% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tokens_over_time.R
\name{plot_tokens_over_time}
\alias{plot_tokens_over_time}
\title{Distribution of Tokens over time}
\usage{
plot_tokens_over_time(
  data,
  names = "all",
  names_col = "Sender",
  starttime = "1960-01-01 00:00",
  endtime = "2200-01-01 00:00",
  plot = "alltime",
  return_data = FALSE,
  exclude_sm = FALSE
)
}
\arguments{
\item{data}{A 'WhatsApp' chat log that was parsed with \code{\link[WhatsR]{parse_chat}} with parameters anonymize = FALSE or anonymize = "add".}

\item{names}{A vector of author names that the plots will be restricted to.}

\item{names_col}{A column indicated by a string that should be accessed to determine the names. Only needs to be changed when \code{\link[WhatsR]{parse_chat}} used the parameter anon = "add" and the column "Anonymous" should be used. Default is "Sender".}

\item{starttime}{Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.}

\item{endtime}{Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.}

\item{plot}{Type of plot to be returned. Options are "year", "day", "hour", "heatmap" and "alltime". Default is "alltime".}

\item{return_data}{If TRUE, returns the subset data frame. Default is FALSE.}

\item{exclude_sm}{If TRUE, excludes the 'WhatsApp' system messages from the descriptive statistics. Default is FALSE.}
}
\value{
A summary of tokens over time. Input will be ordered by TimeOrder column.
}
\description{
Summarizes the distribution of user-generated tokens over time
}
\examples{
data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
plot_tokens_over_time(data)
}
