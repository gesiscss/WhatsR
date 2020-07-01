#' @title Wordclouds for WhatsApp chatlogs
#' @description Creates awordcloud by author for WhatsApp chatlogs
#' @param data A WhatsApp chatlog that was parsed with parse_chat()
#' @param names A vector of author names that the Plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with anytime(). Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with anytime(). Standard format is "yyyy-mm-dd hh:mm".
#' @param stop The language of the chat for stopword removal, passed down to quanteda::stopwords(). default is "german"
#' @param maxwords Maximum number of words to display in the wordcloud. Default is 100.
#' @param mincount Minimum number of occurances a word has to have to be included in the wordplot. Default is 1
#' @param comparison If TRUE, compares the unique wordclouds for different authors for a maximum of 8 authors. Default is FALSE
#' @import quanteda
#' @importFrom anytime anytime
#' @importFrom quanteda textplot_wordcloud
#' @importFrom quanteda stopwords
#' @importFrom quanteda dfm_group
#' @export
#' @return A wordcloud plot per author for WhatsApp chatlogs
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_wordcloud(data, comparison = TRUE)

################## Function to make a wordcloud
plot_wordcloud <- function(data,
                           names = "all",
                           starttime = anytime("1960-01-01 00:00"),
                           endtime = Sys.time(),
                           stop = "german",
                           maxwords = 100,
                           mincount = 1,
                           comparison = FALSE){

  # setting starttime
  if (starttime == anytime("1960-01-01 00:00")) {

    starttime <- min(data$DateTime)

  } else {starttime <- anytime(starttime, asUTC = TRUE)}

  # setting endtime
  if (difftime(Sys.time(),endtime, units = "min") < 1) {

    endtime <- max(data$DateTime)

  } else {endtime <- anytime(endtime, asUTC = TRUE)}

  # setting names argument
  if (length(names) == 1 && names == "all") {

    # All names in the dataframe except System Messages
    names = unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

  }

  # limiting data to time and namescope
  data <- data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,]

  # creating wordcloud
  Messages <- corpus(data$Flat)

  # setting Sender and Timestamp as docvars for easier accessibility
  docvars(Messages, "Sender") <- data$Sender
  docvars(Messages, "Timestamp") <- data$DateTime

  if (comparison == FALSE) {

    # wordcloud for all tokens
    textplot_wordcloud(dfm(Messages, remove = stopwords(stop)),
                       random_order = FALSE,
                       max_words = maxwords,
                       min_count = mincount)

  }

  if (comparison == TRUE) {

    if (length(unique(data$Sender)) >= 8) {

      warning("Comparing Senders in wordclouds only works for up to 8 senders")
      stop()

    }

    # comparison Wordcloud
    SenderDFM <- dfm_group(dfm(Messages, remove = stopwords(stop)), groups = "Sender")
    textplot_wordcloud(SenderDFM, comparison = TRUE, min_count = mincount,max_words = maxwords)

  }

}
