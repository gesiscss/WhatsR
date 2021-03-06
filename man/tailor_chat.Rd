% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tailor_chat.R
\name{tailor_chat}
\alias{tailor_chat}
\title{Basic WhatsApp Chatlog Statistics}
\usage{
tailor_chat(
  data,
  names = "all",
  starttime = anytime("1960-01-01 00:00"),
  endtime = Sys.time(),
  excludeSM = TRUE
)
}
\arguments{
\item{data}{A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}}

\item{names}{If TRUE, excludes the WhatsAppSystemmessages from the descriptive statistics. Default is TRUE.}

\item{starttime}{Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".}

\item{endtime}{Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".}

\item{excludeSM}{If TRUE, excludes the WhatsAppSystemmessages from the descriptive statistics. Default is TRUE.}
}
\value{
A dataframe that is restricted to the specified timeframe and authors
}
\description{
Excluding parts of the chat by senders or timestamps
}
\examples{
data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
tailor_chat(data, names = c("Mallory","Alice"))
}
