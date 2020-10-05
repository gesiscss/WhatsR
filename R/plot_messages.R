#' @title Amount of messages per person
#' @description Plots summarizing the amount of messages per person
#' @param data A WhatsApp chatlog that was parsed with code{\link[WhatsR]{parse_chat}}
#' @param names A vector of author names that the Plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param plot Type of plot to be returned, options include "bar" and "pie". Default is "bar".
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#' @return Plots summarizing the amount of messages per person
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_messages(data)

######## overall amount of messages per person
# TODO: Replace this with a similar function to the token plotting function with a heatmap and over time function
plot_messages <- function(data,
                          names = "all",
                          starttime = anytime("1960-01-01 00:00"),
                          endtime = Sys.time(),
                          plot = "bar") {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  Var1 <- Freq <-  NULL

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

  if (plot == "bar") {

    # creating barplot
    out <- ggplot(as.data.frame(table(data$Sender)), aes(x = Var1, y = Freq, fill = Var1)) +
      geom_bar(stat = "identity") +
      labs(title = "Amount of Messages sent by Persons",
           subtitle = paste(starttime, " - ", endtime)) +
      xlab("Person") +
      ylab("No. of Messages") +
      labs(fill = "Person") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

  }

  if (plot == "pie") {

    # creating piechart
    out <- ggplot(as.data.frame(table(data$Sender)), aes(x = "", y = Freq, fill = Var1)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      xlab("") +
      ylab("") +
      labs(fill = "Person", title = "Share of Messages by Persons",
           subtitle = paste(starttime, " - ", endtime)) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = 'white', colour = 'white'))
  }

  return(out)

}
