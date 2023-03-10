#' @title Number of messages per person
#' @description Plots summarizing the amount of messages per person
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param plot Type of plot to be returned, options include "bar", "cumsum", "heatmap" and "pie". Default is "bar".
#' @param return.data If TRUE, returns the subsetted data frame. Default is FALSE.
#' @param excludeSM If TRUE, excludes the WhatsApp system messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom mgsub mgsub
#' @export
#' @return Plots summarizing the number of messages per person
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_messages(data)
######## overall amount of messages per person
plot_messages <- function(data,
                          names = "all",
                          starttime = anytime("1960-01-01 00:00"),
                          endtime = Sys.time(),
                          plot = "bar",
                          return.data = FALSE,
                          excludeSM = FALSE) {
  # checking for column names of senders
  if (!("Sender" %in% colnames(data))) {
    colnames(data)[colnames(data) == "Anonymous"] <- "Sender"
  }


  # catching bad params
  # start- and endtime are POSIXct
  if (is(starttime, "POSIXct") == F) stop("starttime has to be of class POSIXct.")
  if (is(endtime, "POSIXct") == F) stop("endtime has to be of class POSIXct.")
  # names in data or all names
  if (!("all" %in% names) & any(!names %in% data$Sender)) stop("names has to either be \"all\" or a vector of names to include.")
  # return.data must be bool
  if (!is.logical(return.data)) stop("return.data has to be either TRUE or FALSE.")
  # plot must be one of the the preset options
  if (any(!plot %in% c("heatmap", "cumsum", "bar", "pie"))) stop("The plot type has to be heatmap, cumsum, bar or pie.")
  # excludeSM must be bool
  if (!is.logical(excludeSM)) stop("excludeSM has to be either TRUE or FALSE.")

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  Var1 <- Freq <- DateTime <- total <- Sender <- day <- hour <- `Number of Messages` <- NULL

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

  # TODO: This doesn't seem to work properly
  # limiting data to time and namescope
  data <- data[is.element(data$Sender, names) & data$DateTime >= starttime & data$DateTime <= endtime, ]

  if (plot == "bar") {
    # creating barplot
    output <- ggplot(as.data.frame(table(as.character(data$Sender))), aes(x = Var1, y = Freq, fill = Var1)) +
      theme_minimal() +
      geom_bar(stat = "identity") +
      labs(
        title = "Number of Messages sent by Persons",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      xlab("Person") +
      ylab("No. of Messages") +
      labs(fill = "Person") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))


    # printing
    print(output)
  }

  if (plot == "cumsum") {
    data <- data[with(data, order(data$Sender, data$DateTime)), ]
    data <- cbind(data, MessageCount = rep(1, dim(data)[1]))

    data$total <- do.call("c", tapply(data$MessageCount, data$Sender, FUN = cumsum))

    output <- ggplot(data, aes(x = DateTime, y = total, color = Sender)) +
      theme_minimal() +
      geom_line() +
      labs(
        title = "Cumulative number of Messages sent",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Time") +
      ylab("Total Messages Sent")

    # printing
    print(output)
  }

  if (plot == "pie") {
    # creating piechart
    output <- ggplot(as.data.frame(table(as.character(data$Sender))), aes(x = "", y = Freq, fill = Var1)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      xlab("") +
      ylab("") +
      labs(
        fill = "Person", title = "Share of Messages by Persons",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white")
      )

    # printing
    print(output)
  }

  if (plot == "heatmap") {
    # creating time data
    data <- cbind(data, MessageCount = rep(1, dim(data)[1]))
    NewFrame <- data[, c("DateTime", "Sender", "MessageCount")]
    NewFrame$hour <- as.POSIXlt(NewFrame$DateTime)$hour
    NewFrame$year <- as.POSIXlt(NewFrame$DateTime)$year + 1900
    NewFrame$day <- weekdays(as.POSIXlt(NewFrame$DateTime), abbreviate = FALSE)

    # shaping dataframe
    helperframe2 <- NewFrame %>%
      group_by(day, hour) %>%
      summarise("Number of Messages" = n())

    # factor ordering
    weekdays <- rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    # transalte to english for better compatibility
    helperframe2$day <- mgsub(helperframe2$day,
      pattern = c("Sonntag", "Samstag", "Freitag", "Donnerstag", "Mittwoch", "Dienstag", "Montag"),
      replacement = weekdays
    )

    helperframe2$day <- as.factor(helperframe2$day)

    if (sum(weekdays %in% levels(helperframe2$day)) == 7) {
      helperframe2$day <- factor(helperframe2$day, levels = weekdays)
    } else {
      helperframe2$day <- factor(helperframe2$day, c(levels(helperframe2$day), weekdays[!weekdays %in% levels(helperframe2$day)]))
      helperframe2$day <- factor(helperframe2$day, levels = weekdays)
    }

    # plotting Heatmap
    output <- ggplot(helperframe2, aes(hour, day)) +
      theme_minimal() +
      geom_tile(aes(fill = `Number of Messages`), colour = "black") +
      labs(
        title = "Messages by Weekday and Hour",
        subtitle = paste(starttime, " - ", endtime),
        x = "",
        y = ""
      ) +
      scale_fill_distiller(palette = "YlGnBu", direction = 1) +
      scale_y_discrete(drop = FALSE) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        panel.grid = element_blank()
      ) +
      coord_equal() +
      scale_x_continuous(
        breaks = seq(-0.5, 23.5, 1),
        limits = c(-0.5, 23.5),
        labels = c(
          "00:00",
          "01:00",
          "02:00",
          "03:00",
          "04:00",
          "05:00",
          "06:00",
          "07:00",
          "08:00",
          "09:00",
          "10:00",
          "11:00",
          "12:00",
          "13:00",
          "14:00",
          "15:00",
          "16:00",
          "17:00",
          "18:00",
          "19:00",
          "20:00",
          "21:00",
          "22:00",
          "23:00",
          "24:00"
        )
      )

    print(output)
  }

  # returning data if desired
  if (return.data == TRUE) {
    if (plot == "heatmap") {
      # returning
      return(as.data.frame(helperframe2))
    } else {
      return(as.data.frame(table(data$Sender)))
    }
  } else {
    return(output)
  }
}
