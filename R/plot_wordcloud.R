#' @title Wordclouds for 'WhatsApp' chat logs
#' @description Creates a wordcloud by author for 'WhatsApp' chat logs. Requires raw message text to be present in data.
#' @param data A 'WhatsApp' chat log that was parsed with \code{\link[WhatsR]{parse_chat}} and anonymize = FALSE or anonymize = "add"
#' @param names A vector of author names that the plots will be restricted to.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param stop The language for stopword removal. Stopwords are taken from \code{\link[tm]{stopwords}}. Options are "english" and "german".
#' @param comparison Must be TRUE or FALSE. If TRUE, will split up wordcloud by sender. Default is FALSE.
#' @param return_data Will return the data frame used to create the plot if TRUE. Default is FALSE.
#' @param font_size Size of the words in the wordcloud, passed to \code{\link[ggplot2]{scale_size_area}}. Default is 10, a good starting value is 0.0125 * number of messages in data frame.
#' @param remove_stops Either TRUE or FALSE, default is TRUE. Configures whether stopwords from \code{\link[tm]{stopwords}} are removed from the text strings.
#' @param min_occur Sets the minimum frequency a token must occur in the chat for it to be included in the plot. Default is 5.
#' @param exclude_sm If TRUE, excludes the 'WhatsApp' system messages from word clouds. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr bind_rows
#' @importFrom ggwordcloud geom_text_wordcloud_area
#' @importFrom methods is
#' @export
#' @return A wordcloud plot per author for 'WhatsApp' chat logs
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_wordcloud(data, comparison = TRUE, min_occur = 6)

################## Function to make a wordcloud
plot_wordcloud <- function(data,
                           names = "all",
                           starttime = "1960-01-01 00:00",
                           endtime = "2200-01-01 00:00",
                           remove_stops = TRUE,
                           stop = "english",
                           comparison = FALSE,
                           return_data = FALSE,
                           font_size = 10,
                           min_occur = 5,
                           exclude_sm = FALSE) {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  `tokens` <- `freq` <- `word` <- NULL

  # catching bad params

  # checking data
  if (!is.data.frame(data)) {stop("'data' must be a dataframe parsed with parse_chat()")}

  # start- and endtime are convertable to POSIXct
  if (is.character(starttime) == FALSE | is.na(as.POSIXct(starttime,tz = "UTC"))) stop("starttime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by as.POSIXct().")
  if (is.character(endtime) == FALSE | is.na(as.POSIXct(endtime,tz = "UTC"))) stop("endtime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by as.POSIXct().")
  if (as.POSIXct(starttime,tz = "UTC") >= as.POSIXct(endtime,tz = "UTC")) stop("starttime has to be before endtime.")

  # font_size must be >= 1
  if (font_size < 1) stop("Please provide a 'font_size' of >= 1.")

  # min_occur must be >= 1
  if (min_occur < 1) stop("Please provide a 'min_occur' of >= 1.")

  # return_data must be bool
  if (!is.logical(return_data)) stop("'return_data' has to be either TRUE or FALSE.")

  # comparison must be bool
  if (!is.logical(comparison)) stop("'comparison' has to be either TRUE or FALSE.")

  # remove_stops must be bool
  if (!is.logical(remove_stops)) stop("'remove_stops' has to be either TRUE or FALSE.")

  # stop must be in data
  if (any(!stop %in% c("english", "german"))) stop("The language for stopword removal has to be 'english' or 'german'.")

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("'exclude_sm' has to be either TRUE or FALSE.")

  # importing tm stopwords
  tm_stopwords <- readRDS(system.file("tm_stopwords.rds", package = "WhatsR"))

  # setting starttime
  if (as.POSIXct(starttime,tz = "UTC") <= min(data$DateTime)) {
    starttime <- min(data$DateTime)
  } else {
    starttime <- as.POSIXct(starttime,tz = "UTC")
  }

  # setting endtime
  if (as.POSIXct(endtime,tz = "UTC") >= max(data$DateTime)) {
    endtime <- max(data$DateTime)
  } else {
    endtime <- as.POSIXct(endtime,tz = "UTC")
  }

  # setting names argument
  if (length(names) == 1 && names == "all") {
    if (exclude_sm == TRUE) {
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


  # comparisons
  if (comparison == FALSE) {
    words <- data.frame(tolower(unlist(data$TokVec)[unlist(data$TokVec) != "NA"]))
    colnames(words) <- "tokens"

    # Setting Size as time of occurance
    words <- as.data.frame(table(words$tokens))
    words <- words[order(-words$Freq), ]
    colnames(words) <- c("tokens", "freq")

    # removing stopwords if desired
    if (remove_stops == TRUE) {
      # computing intersection and excluding rows
      words <- words[-which(words$tokens %in% tm_stopwords[[stop]]), ]
    }

    # removing everything below min_occur
    index <- which(words$freq < min_occur)
    if (length(index) != 0) {
      words <- words[-c(index), ]
    }

    if (dim(words)[1] == 0) {
      warnings("No words are used at least min_occur number of times. Try lowering min_occur.")
      stop("No words are used at least min_occur number of times. Try lowering min_occur.")
    }

    # plotting basic wordcloud
    out <- ggplot(words, aes(label = tokens, size = freq)) +
      geom_text_wordcloud_area(rm_outside = TRUE) +
      scale_size_area(max_size = font_size) +
      theme_minimal() +
      labs(title = "Wordcloud", subtitle = paste("Including words occuring at least ", min_occur, " times", sep = "")) +
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
    if (remove_stops == TRUE) {
      # computing intersection and excluding rows
      FreqFrame <- FreqFrame[-which(FreqFrame$word %in% tm_stopwords[[stop]]), ]
    }

    # removing everything below min_occur
    index <- which(FreqFrame$freq < min_occur)
    if (length(index) != 0) {
      FreqFrame <- FreqFrame[-c(index), ]
    }

    if (dim(FreqFrame)[1] == 0) {
      warnings("No words are used at least min_occur number of times. Try lowering min_occur.")
      stop()
    }

    # Plotting facetted plot
    out <- ggplot(FreqFrame, aes(label = word, size = freq)) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = font_size) +
      theme_minimal() +
      labs(title = "Wordclouds by Sender", subtitle = paste("Including words occuring at least ", min_occur, " times", sep = "")) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
      facet_wrap(~sender)
  }

  # returning data if desired
  if (return_data == TRUE) {
    print(out)

    if (comparison == TRUE) {

      return(FreqFrame)
    } else {

      return(words)
    }
  } else {
    print(out)
    return(out)
  }
}
