#' @title Visualizing token distribution per person
#' @description Visualizing token distribution per person
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param names.col A column indicated by a string that should be accessed to determine the names. Only needs to be changed when \code{\link[WhatsR]{parse_chat}} used the parameter anon = "add" and the column "Anonymous" should be used. Default is "Sender".
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param plot The type of plot to be used. Options include "bar","box","violin" and "cumsum". Default is "bar".
#' @param return.data If TRUE, returns the subsetted data frame. Default is FALSE.
#' @param excludeSM If TRUE, excludes the WhatsApp System Messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#' @return Plots showcasing the distribution of tokens per person
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_tokens(data)
######################### Visualizing amount of tokens per person
plot_tokens <- function(data,
                        names = "all",
                        names.col = "Sender",
                        starttime = anytime("1960-01-01 00:00"),
                        endtime = Sys.time(),
                        plot = "bar",
                        return.data = FALSE,
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
  # return.data must be bool
  if (!is.logical(return.data)) stop("return.data has to be either TRUE or FALSE.")
  # plot must be one of the the preset options
  if (any(!plot %in% c("bar", "box", "violin", "cumsum"))) stop("The plot type has to be bar, box, violin or cumsum.")
  # excludeSM must be bool
  if (!is.logical(excludeSM)) stop("excludeSM has to be either TRUE or FALSE.")#

  #if names.col == "Anonymous", rename to Sender and rename Sender to placeholder
  if(names.col == "Anonymous"){
    colnames(data)[colnames(data) == "Sender"] <- "Placeholder"
    colnames(data)[colnames(data) == "Anonymous"] <- "Sender"
  }

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  tokens <- freq <- word <- Sender <- TokCount <- DateTime <- total <- NULL

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

  # tailoring data
  data <- data[is.element(data$Sender, names) & data$DateTime >= starttime & data$DateTime <= endtime, ]


  if (plot == "bar") {
    output <- ggplot(data, aes(x = Sender, fill = Sender, y = TokCount, color = Sender)) +
      theme_minimal() +
      geom_bar(stat = "identity") +
      labs(
        title = "Number of Tokens sent by Persons",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      xlab("Person") +
      ylab("No. of Tokens") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }

  if (plot == "box") {
    output <- ggplot(
      data,
      aes(
        x = Sender, y = TokCount,
        color = Sender
      )
    ) +
      theme_minimal() +
      geom_boxplot() +
      labs(
        title = "Distribution of Message length by Person",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Persons") +
      ylab("Tokens per Message")
  }

  if (plot == "violin") {
    output <- ggplot(
      data,
      aes(x = Sender, y = TokCount, fill = Sender, color = Sender)
    ) +
      theme_minimal() +
      geom_violin() +
      labs(
        title = "Distribution of Message length by Person",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Persons") +
      ylab("Tokens per Message")
  }

  if (plot == "cumsum") {
    data <- data[with(data, order(data$Sender, data$DateTime)), ]
    data$total <- do.call("c", tapply(data$TokCount, data$Sender, FUN = cumsum))

    output <- ggplot(data, aes(x = DateTime, y = total, color = Sender)) +
      theme_minimal() +
      geom_line() +
      labs(
        title = "Cumulative number of Tokens sent",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Time") +
      ylab("Total Tokens Sent")
  }

  if (return.data == TRUE) {
    # print
    print(output)
    # Rename Sender and Anonymous columns again to what they were initially
    if(names.col == "Anonymous"){
      colnames(data)[colnames(data) == "Sender"] <- "Anonymous"
      colnames(data)[colnames(data) == "Placeholder"] <- "Sender"
    }
    return(as.data.frame(data))
  } else {
    return(output)
  }
}
