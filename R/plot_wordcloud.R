#' @title Wordclouds for WhatsApp chatlogs
#' @description Creates a wordcloud by author for WhatsApp chatlogs
#' @param data A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}
#' @param names A vector of author names that the Plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param stop The language for stopword removal. Stopwords are taken from code{\link[tm]{stopwords}}. Default is "english".
#' @param comparison Must be TRUE or FALSE. If TRUE, will split up wordcloud by sender. Default is FALSE.
#' @param return.data Will the dataframe used to create the plot if TRUE. Default is FALSE
#' @param font.size Size of the words in the wordcloud, passed to code{\link[ggwordcloud]{scale_size_area}}. Default is 5, a good starting value is 0.0125 * number of messages in dataframe
#' @import ggplot2
#' @import ggwordcloud
#' @importFrom anytime anytime
#' @importFrom dplyr bind_rows
#' @importFrom tm stopwords
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
                           remove.stops = TRUE,
                           stop = "english",
                           comparison = FALSE,
                           return.data = FALSE,
                           font.size = 10,
                           min.freq = 5){

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


  ###### New Solution
  if (comparison == FALSE) {

    words <- data.frame(tolower(unlist(data$TokVec)[unlist(data$TokVec) != "NA"]))
    colnames(words) <- "tokens"

    # Setting Size as time of occurance
    words <- as.data.frame(table(words$tokens))
    words <- words[order(-words$Freq),]
    colnames(words) <- c("tokens","freq")

    # removing stopwords if desired
    if (remove.stops == TRUE) {

      # computing intersection and excluding rows
      words <- words[-which(words$tokens %in% tm::stopwords(kind = stop)),]

    }

    # removing everything below min.freq
    index <- which(words$freq < min.freq)
    if (length(index) != 0) {words <- words[-c(index),]}

    if (dim(words)[1] == 0) {

      warnings("No words are used at least min.freq number of times. Try lowering min.freq.")
      stop("No words are used at least min.freq number of times. Try lowering min.freq.")

    }

    # plotting basic wordcloud
    plot <- ggplot(words, aes(label = tokens, size = freq)) +
      geom_text_wordcloud_area(rm_outside = TRUE) +
      scale_size_area(max_size = font.size) +
      theme_minimal() +
      labs(title = "Wordcloud", subtitle = paste("Including words occuring at least ",min.freq," times", sep = "")) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

  }

  if (comparison == TRUE) {

    # Split dataframe by sender
    words <-split(data, data$Sender)

    # function to get word frequencies per sneder
    GetWordFreq <- function(x) {

      WordFreq <- as.data.frame(table(tolower(unlist(x$TokVec)[unlist(x$TokVec) != "NA"])))

      if(dim(WordFreq)[1] != 0) {

        WordFreq <- WordFreq[order(-WordFreq$Freq),]
        colnames(WordFreq) <- c("word","freq")
        WordFreq$sender <- rep(unique(x$Sender),dim(WordFreq)[1])

      }

      return(WordFreq)

    }

    # Excluding System Messages
    FreqList <- lapply(words,GetWordFreq)
    FreqList["WhatsApp System Message"] <- NULL
    FreqFrame <- dplyr::bind_rows(FreqList)

    # removing stopwords if desired
    if (remove.stops == TRUE) {

      # computing intersection and excluding rows
      FreqFrame <- FreqFrame[-which(FreqFrame$word %in% tm::stopwords(kind = stop)),]

    }

    # removing everything below min.freq
    index <- which(FreqFrame$freq < min.freq)
    if (length(index) != 0) {FreqFrame <- FreqFrame[-c(index),]}

    if (dim(FreqFrame)[1] == 0) {

      warnings("No words are used at least min.freq number of times. Try lowering min.freq.")
      stop()

    }

    # Plotting facetted plot
    plot <- ggplot(FreqFrame, aes(label = word, size = freq)) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = font.size) +
      theme_minimal() +
      labs(title = "Wordclouds by Sender", subtitle = paste("Including words occuring at least ",min.freq," times", sep = "")) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
      facet_wrap(~sender)

  }

  # returning data if desired
  if (return.data == TRUE) {

    return(words)

  } else {

    return(plot)

  }

}
