#' @title Visualizing token distribution per person
#' @description Visualizing token distribution per person
#' @param data A 'WhatsApp' chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param plot The type of plot to be used. Options include "bar","box","violin" and "cumsum". Default is "bar". NA values will be removed before plotting. For "violin", Senders with less than 2 messages are removed.
#' @param return_data If TRUE, returns the subsetted data frame. Default is FALSE.
#' @param exclude_sm If TRUE, excludes the 'WhatsApp' System Messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom methods is
#' @export
#' @return Plots showcasing the distribution of tokens per person
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_tokens(data)

######################### Visualizing amount of tokens per person
plot_tokens <- function(data,
                        names = "all",
                        starttime = "1960-01-01 00:00",
                        endtime = "2200-01-01 00:00",
                        plot = "bar",
                        return_data = FALSE,
                        exclude_sm = FALSE) {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  tokens <- freq <- word <- Sender <- TokCount <- DateTime <- total <- NULL

  # catching bad params

  # checking data
  if (!is.data.frame(data)) {stop("'data' must be a dataframe parsed with parse_chat()")}

  # start- and endtime are convertable to POSIXct
  if (is.character(starttime) == FALSE | is.na(as.POSIXct(starttime,tz = "UTC"))) stop("starttime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by as.POSIXct().")
  if (is.character(endtime) == FALSE | is.na(as.POSIXct(endtime,tz = "UTC"))) stop("endtime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by as.POSIXct().")
  if (as.POSIXct(starttime,tz = "UTC") >= as.POSIXct(endtime,tz = "UTC")) stop("starttime has to be before endtime.")

  # return_data must be bool
  if (!is.logical(return_data)) stop("'return_data' has to be either TRUE or FALSE.")

  # plot must be one of the the preset options
  if (any(!plot %in% c("bar", "box", "violin", "cumsum"))) stop("The plot type has to be bar, box, violin or cumsum.")

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")#

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

  # tailoring data
  data <- data[is.element(data$Sender, names) & data$DateTime >= starttime & data$DateTime <= endtime, ]


  if (plot == "bar") {

    # removing NAs for plotting
    Plottingframe <- data[!is.na(data$TokCount),]

    output <- ggplot(Plottingframe, aes(x = Sender, fill = Sender, y = TokCount, color = Sender)) +
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

    # removing NAs for plotting
    Plottingframe <- data[!is.na(data$TokCount),]

    output <- ggplot(
      Plottingframe,
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

    # removing NAs for plotting
    Plottingframe <- data[!is.na(data$TokCount),]

    # removing groups with less than 2 observations for plotting violin plots
    remove_names <- names(table(Plottingframe$Sender))[table(Plottingframe$Sender) < 2]
    Plottingframe <- Plottingframe[!(Plottingframe$Sender %in% remove_names),]

    output <- ggplot(
      Plottingframe,
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

    # computing for output
    data <- data[with(data, order(data$Sender, data$DateTime)), ]
    data$total <- do.call("c", tapply(data$TokCount, data$Sender, FUN = cumsum))

    # removing NAs for plotting
    Plottingframe <- data[!is.na(data$TokCount),]
    PlottingframePlottingframe <- data[with(Plottingframe, order(Plottingframe$Sender, Plottingframe$DateTime)), ]
    Plottingframe$total <- do.call("c", tapply(Plottingframe$TokCount, Plottingframe$Sender, FUN = cumsum))

    output <- ggplot(Plottingframe, aes(x = DateTime, y = total, color = Sender)) +
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

  if (return_data == TRUE) {
    # print
    print(output)
    return(as.data.frame(data))
  } else {
    return(output)
  }
}
