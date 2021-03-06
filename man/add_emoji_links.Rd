% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_emoji_links.R
\name{add_emoji_links}
\alias{add_emoji_links}
\title{Getting picture links for printing emoji in plots}
\usage{
add_emoji_links(emojidict)
}
\arguments{
\item{emojidict}{A dictionary of emojis created by \code{\link{download_emoji}}.}
}
\value{
A dictionary of emojis inlcuding two new columns with links to the pictures and html tags of those links respectively.
}
\description{
Fetches links to pictures of emojis from www.emojipedia.org, which can be used to plot the actual emojis
in plots instead of their textual description only. The procedure was adapted from: https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/.
This only needs to be done when new emoji are added to www.emojipedia.org and the emoji dictionary needed to be updated.
}
\examples{
EmojiDic <- read.csv(system.file("EmojiDictionary.csv", package = "WhatsR"),stringsAsFactors = FALSE)[,-c(1:2)]
EmojiDic2 <- add_emoji_links(EmojiDic[1:20,])
}
