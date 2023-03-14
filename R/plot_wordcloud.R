#' @title Wordclouds for WhatsApp chatlogs
#' @description Creates a wordcloud by author for WhatsApp chatlogs
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param names.col A column indicated by a string that should be accessed to determine the names. Only needs to be changed when \code{\link[WhatsR]{parse_chat}} used the parameter anon = "add" and the column "Anonymous" should be used. Default is "Sender".
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param stop The language for stopword removal. Stopwords are taken from \code{\link[tm]{stopwords}}. Options include "english" and "german".
#' @param comparison Must be TRUE or FALSE. If TRUE, will split up wordcloud by sender. Default is FALSE.
#' @param return.data Will return the data frame used to create the plot if TRUE. Default is FALSE.
#' @param font.size Size of the words in the wordcloud, passed to \code{\link[ggplot2]{scale_size_area}}. Default is 10, a good starting value is 0.0125 * number of messages in data frame.
#' @param remove.stops Either TRUE or FALSE, default is TRUE. Configures whether stopwords from \code{\link[tm]{stopwords}} are removed from the text strings.
#' @param min.freq Sets the minimum frequency a token must occur in the chat for it to be included in the plot. Default is 5.
#' @param excludeSM If TRUE, excludes the WhatsApp system messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr bind_rows
#' @importFrom ggwordcloud geom_text_wordcloud_area
#' @export
#' @return A wordcloud plot per author for WhatsApp chatlogs
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_wordcloud(data, comparison = TRUE, min.freq = 1)
################## Function to make a wordcloud
plot_wordcloud <- function(data,
                           names = "all",
                           names.col = "Sender",
                           starttime = anytime("1960-01-01 00:00"),
                           endtime = Sys.time(),
                           remove.stops = TRUE,
                           stop = "english",
                           comparison = FALSE,
                           return.data = FALSE,
                           font.size = 10,
                           min.freq = 5,
                           excludeSM = FALSE) {


  # catching bad params
  # start- and endtime are POSIXct
  if (is(starttime, "POSIXct") == F) stop("starttime has to be of class POSIXct.")
  if (is(endtime, "POSIXct") == F) stop("endtime has to be of class POSIXct.")
  # names.col must be in preset options
  if (any(!names.col %in% c("Sender", "Anonymous"))) stop("names.col has to be either Sender or Anonymous.")
  # names in data or all names (Sender or Anonymous)
  if(names.col == "Sender"){
    if (!("all" %in% names) & any(!names %in% data$Sender)) stop("names has to either be \"all\" or a vector of names to include.")}
  else{
    if(!("all" %in% names) & any(!names %in% data$Anonymous)) stop("names has to either be \"all\" or a vector of names to include.")}
  # font.size must be >= 1
  if (font.size < 1) stop("Please provide a font.size of >= 1.")
  # min.freq must be >= 1
  if (min.freq < 1) stop("Please provide a min.freq of >= 1.")
  # return.data must be bool
  if (!is.logical(return.data)) stop("return.data has to be either TRUE or FALSE.")
  # comparison must be bool
  if (!is.logical(comparison)) stop("comparison has to be either TRUE or FALSE.")
  # remove.stops must be bool
  if (!is.logical(remove.stops)) stop("remove.stops has to be either TRUE or FALSE.")
  # stop must be in data
  if (any(!stop %in% c("english", "german"))) stop("The language for stopword removal has to be \"english\" or \"german\".")
  # excludeSM must be bool
  if (!is.logical(excludeSM)) stop("excludeSM has to be either TRUE or FALSE.")

  #if names.col == "Anonymous", rename to Sender and rename Sender to placeholder
  if(names.col == "Anonymous"){
    colnames(data)[colnames(data) == "Sender"] <- "Placeholder"
    colnames(data)[colnames(data) == "Anonymous"] <- "Sender"
  }

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  `tokens` <- `freq` <- `word` <- NULL

  # importing tm stopwords
  tm_stopwords <- readRDS(system.file("tm_stopwords.rds", package = "WhatsR"))

  # setting starttime
  if (starttime == anytime("1960-01-01 00:00")) {
    starttime <- min(data$DateTime)
  } else {
    starttime <- anytime(starttime, asUTC = TRUE)
  }

  # setting endtime
  if (difftime(Sys.time(), endtime, units = "min") < 1) {
    endtime <- max(data$DateTime)
  } else {
    endtime <- anytime(endtime, asUTC = TRUE)
  }

  # setting names argument
  if (length(names) == 1 && names == "all") {
    if (excludeSM == TRUE) {
      # All names in the dataframe except System Messages
      names <- unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

      # dropping empty levels
      if (is.factor(names)) {
        names <- droplevels(names)
      }
    } else {
      # including system messages
      names <- unique(data$Sender)
    }
  }

  # limiting data to time and namescope
  data <- data[is.element(data$Sender, names) & data$DateTime >= starttime & data$DateTime <= endtime, ]


  ###### New Solution
  if (comparison == FALSE) {
    words <- data.frame(tolower(unlist(data$TokVec)[unlist(data$TokVec) != "NA"]))
    colnames(words) <- "tokens"

    # Setting Size as time of occurance
    words <- as.data.frame(table(words$tokens))
    words <- words[order(-words$Freq), ]
    colnames(words) <- c("tokens", "freq")

    # removing stopwords if desired
    if (remove.stops == TRUE) {
      # computing intersection and excluding rows
      words <- words[-which(words$tokens %in% tm_stopwords[[stop]]), ]
    }

    # removing everything below min.freq
    index <- which(words$freq < min.freq)
    if (length(index) != 0) {
      words <- words[-c(index), ]
    }

    if (dim(words)[1] == 0) {
      warnings("No words are used at least min.freq number of times. Try lowering min.freq.")
      stop("No words are used at least min.freq number of times. Try lowering min.freq.")
    }

    # plotting basic wordcloud
    out <- ggplot(words, aes(label = tokens, size = freq)) +
      geom_text_wordcloud_area(rm_outside = TRUE) +
      scale_size_area(max_size = font.size) +
      theme_minimal() +
      labs(title = "Wordcloud", subtitle = paste("Including words occuring at least ", min.freq, " times", sep = "")) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  }

  if (comparison == TRUE) {
    # Split dataframe by sender
    words <- split(data, data$Sender)

    # function to get word frequencies per sneder
    GetWordFreq <- function(x) {
      WordFreq <- as.data.frame(table(tolower(unlist(x$TokVec)[unlist(x$TokVec) != "NA"])))

      if (dim(WordFreq)[1] != 0) {
        WordFreq <- WordFreq[order(-WordFreq$Freq), ]
        colnames(WordFreq) <- c("word", "freq")
        WordFreq$sender <- rep(unique(x$Sender), dim(WordFreq)[1])
      }

      return(WordFreq)
    }

    # Excluding System Messages
    FreqList <- lapply(words, GetWordFreq)
    FreqList["WhatsApp System Message"] <- NULL
    FreqFrame <- dplyr::bind_rows(FreqList)

    # removing stopwords if desired
    if (remove.stops == TRUE) {
      # computing intersection and excluding rows
      FreqFrame <- FreqFrame[-which(FreqFrame$word %in% tm_stopwords[[stop]]), ]
    }

    # removing everything below min.freq
    index <- which(FreqFrame$freq < min.freq)
    if (length(index) != 0) {
      FreqFrame <- FreqFrame[-c(index), ]
    }

    if (dim(FreqFrame)[1] == 0) {
      warnings("No words are used at least min.freq number of times. Try lowering min.freq.")
      stop()
    }

    # Plotting facetted plot
    out <- ggplot(FreqFrame, aes(label = word, size = freq)) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = font.size) +
      theme_minimal() +
      labs(title = "Wordclouds by Sender", subtitle = paste("Including words occuring at least ", min.freq, " times", sep = "")) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
      facet_wrap(~sender)
  }

  # returning data if desired
  if (return.data == TRUE) {
    print(out)

    if (comparison == TRUE) {
      # Rename Sender and Anonymous columns again to what they were initially
      if(names.col == "Anonymous"){
        colnames(FreqFrame)[colnames(FreqFrame) == "Sender"] <- "Anonymous"
        colnames(FreqFrame)[colnames(FreqFrame) == "Placeholder"] <- "Sender"
      }
      return(FreqFrame)
    } else {
      # Rename Sender and Anonymous columns again to what they were initially
      if(names.col == "Anonymous"){
        colnames(words)[colnames(words) == "Sender"] <- "Anonymous"
        colnames(words)[colnames(words) == "Placeholder"] <- "Sender"
      }
      return(words)
    }
  } else {
    print(out)
    return(out)
  }
}
