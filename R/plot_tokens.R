#' @title Visualizing token distribution per person
#' @description Visualizing token distribution per person
#' @param data A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}
#' @param names A vector of author names that the Plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param plot The type of plot to be used. Options include "bar","box","violin" and "cumsum". Default is "bar".
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#' @return Plots showcasing the disitribution of tokens per person
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_tokens(data)

######################### Visualizing amount of tokens per person
plot_tokens <- function(data,
                        names = "all",
                        starttime = anytime("1960-01-01 00:00"),
                        endtime = Sys.time(),
                        plot = "bar"){

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  tokens <- freq <- word <- Sender <- TokCount <- DateTime <- total <- NULL

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

  if (plot == "bar") {

    # We have some weird stripes on the bargraphs here (maybe because of the WhatsApp System messages?)
    output <- ggplot(data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,], aes(x = Sender, fill = Sender,y = TokCount)) +
      geom_bar(stat = "identity") +
      labs(title = "Amount of Tokens sent by Persons",
           subtitle = paste(starttime, " - ", endtime)) +
      xlab("Person") +
      ylab("No. of Tokens") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

  }

  if (plot == "box") {

    output <- ggplot(data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,],
                     aes(x = Sender, y = TokCount,
                         color = Sender)) +
      geom_boxplot() +
      labs(title = "Distribution of Message length by Person",
           subtitle = paste(starttime, " - ", endtime))  +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
      xlab("Persons") +
      ylab("Tokens per Message")

  }

  if (plot == "violin") {

    output <- ggplot(data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,],
                     aes(x = Sender, y = TokCount, fill = Sender, color = Sender)) +
      geom_violin() +
      labs(title = "Distribution of Message length by Person",
           subtitle = paste(starttime, " - ", endtime))  +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
      xlab("Persons") +
      ylab("Tokens per Message")

  }

  if ( plot == "cumsum") {

    data <- data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,]
    data <- data[with(data, order(data$Sender,data$DateTime)),]
    data$total <- do.call("c", tapply(data$TokCount, data$Sender, FUN = cumsum))

    output <- ggplot(data, aes(x = DateTime, y = total, color = Sender)) +
      geom_line() +
      labs(title = "Cumulative number of Tokens sent",
           subtitle = paste(starttime, " - ", endtime))  +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
      xlab("Time") +
      ylab("Total Tokens Sent")

  }

  return(output)

}
