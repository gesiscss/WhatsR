% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_wordcloud.R
\name{plot_wordcloud}
\alias{plot_wordcloud}
\title{Wordclouds for WhatsApp chatlogs}
\usage{
plot_wordcloud(
  data,
  names = "all",
  starttime = anytime("1960-01-01 00:00"),
  endtime = Sys.time(),
  remove.stops = TRUE,
  stop = "english",
  comparison = FALSE,
  return.data = FALSE,
  font.size = 10,
  min.freq = 5
)
}
\arguments{
\item{data}{A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}}

\item{names}{A vector of author names that the Plots will be restricted to}

\item{starttime}{Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".}

\item{endtime}{Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".}

\item{stop}{The language for stopword removal. Stopwords are taken from code{\link[tm]{stopwords}}. Default is "english".}

\item{comparison}{Must be TRUE or FALSE. If TRUE, will split up wordcloud by sender. Default is FALSE.}

\item{return.data}{Will return the dataframe used to create the plot if TRUE. Default is FALSE}

\item{font.size}{Size of the words in the wordcloud, passed to code{\link[ggwordcloud]{scale_size_area}}. Default is 10, a good starting value is 0.0125 * number of messages in dataframe}
}
\value{
A wordcloud plot per author for WhatsApp chatlogs
}
\description{
Creates a wordcloud by author for WhatsApp chatlogs
}
\examples{
data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
plot_wordcloud(data, comparison = TRUE)
}
