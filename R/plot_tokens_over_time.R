#' @title Distribution of Tokens over time
#' @description Summarizes the distribution of tokens over time
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param names.col A column indicated by a string that should be accessed to determine the names. Only needs to be changed when \code{\link[WhatsR]{parse_chat}} used the parameter anon = "add" and the column "Anonymous" should be used. Default is "Sender".
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param plot Time resolution for plots. Options include "year", "day", "hour", "heatmap" and "alltime". Default is "alltime".
#' @param return.data If TRUE, returns the subsetted data frame. Default is FALSE.
#' @param excludeSM If TRUE, excludes the WhatsApp system messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#' @return A summary of tokens over time
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_tokens_over_time(data)
############# Tokens over time
plot_tokens_over_time <- function(data,
                                  names = "all",
                                  names.col = "Sender",
                                  starttime = anytime("1960-01-01 00:00"),
                                  endtime = Sys.time(),
                                  plot = "alltime",
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
  if (any(!plot %in% c("year", "day", "hour", "heatmap", "alltime"))) stop("The plot type has to be year, day, hour, heatmap or alltime.")
  # excludeSM must be bool
  if (!is.logical(excludeSM)) stop("excludeSM has to be either TRUE or FALSE.")

  #if names.col == "Anonymous", rename to Sender and rename Sender to placeholder
  if(names.col == "Anonymous"){
    colnames(data)[colnames(data) == "Sender"] <- "Placeholder"
    colnames(data)[colnames(data) == "Anonymous"] <- "Sender"
  }

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  DateTime <- TokCount <- Sender <- year <- day <- hour <- `Number of Tokens` <- NULL

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

  # building helper dataframe
  helperframe <- data[, c("DateTime", "Sender", "TokCount")]

  # creating time data
  helperframe$hour <- as.POSIXlt(helperframe$DateTime)$hour
  helperframe$year <- as.POSIXlt(helperframe$DateTime)$year + 1900
  helperframe$day <- weekdays(as.POSIXlt(helperframe$DateTime), abbreviate = FALSE)

  # basic barchart
  if (plot == "alltime") {
    # plotting chart
    out <- ggplot(data, aes(x = DateTime, y = TokCount, color = Sender, fill = Sender)) +
      theme_minimal() +
      geom_bar(stat = "identity") +
      labs(
        title = "Sent Tokens by Persons over Time",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      xlab("Persons") +
      ylab("Sent Tokens")
  }

  # barchart by year
  if (plot == "year") {
    # plotting by year
    out <- ggplot(helperframe, aes(x = as.factor(year), y = TokCount, color = Sender, fill = Sender)) +
      theme_minimal() +
      geom_bar(stat = "identity") +
      labs(
        title = "Tokens by Years",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      xlab("Year") #+
    # scale_x_discrete(limits = as.factor(unique(helperframe$year)))
  }

  # barchart per day
  if (plot == "day") {
    # debugger()
    browser()

    # plotting by weekday
    out <- ggplot(helperframe, aes(x = day, y = TokCount, color = Sender, fill = Sender)) +
      theme_minimal() +
      geom_bar(stat = "identity") +
      labs(
        title = "Tokens by day of the week",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      xlab("Weekday") +
      scale_x_discrete(limits = as.factor(c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )))
  }


  if (plot == "hour") {
    # debugger
    browser()

    # preprocessing hour variable
    helperframe$hour <- factor(helperframe$hour, levels = c(1:23, 0))


    # plotting by hour
    out <- ggplot(helperframe, aes(x = hour, y = TokCount, color = Sender, fill = Sender)) +
      theme_minimal() +
      geom_bar(stat = "identity") +
      labs(
        title = "Tokens by hour of the day",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      xlab("Hour of Day") +
      scale_x_discrete(drop = FALSE)



    # scale_x_continuous(breaks = seq(-0.5,23.5,1),
    #                    limits = c(-0.5,23.5),
    #                    labels = c("00:00",
    #                               "01:00",
    #                               "02:00",
    #                               "03:00",
    #                               "04:00",
    #                               "05:00",
    #                               "06:00",
    #                               "07:00",
    #                               "08:00",
    #                               "09:00",
    #                               "10:00",
    #                               "11:00",
    #                               "12:00",
    #                               "13:00",
    #                               "14:00",
    #                               "15:00",
    #                               "16:00",
    #                               "17:00",
    #                               "18:00",
    #                               "19:00",
    #                               "20:00",
    #                               "21:00",
    #                               "22:00",
    #                               "23:00",
    #                               "24:00")) +
    # theme(axis.title.y = element_blank(),
    #       axis.text.y = element_blank(),
    #       axis.ticks.y = element_blank(),
    #       axis.text.x = element_text(angle = 90, hjust = 1))
  }


  if (plot == "heatmap") {
    # TODO: Add option in the dataframe shaper to average and median instead of summarize?

    # shaping dataframe
    helperframe <- helperframe %>%
      group_by(day, hour) %>%
      summarise("Number of Tokens" = sum(TokCount))

    # factor ordering
    weekdays <- rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

    # transalte to english for better compatibility
    helperframe$day <- mgsub(helperframe$day,
      pattern = c("Sonntag", "Samstag", "Freitag", "Donnerstag", "Mittwoch", "Dienstag", "Montag"),
      replacement = weekdays
    )

    helperframe$day <- as.factor(helperframe$day)

    if (sum(weekdays %in% levels(helperframe$day)) == 7) {
      helperframe$day <- factor(helperframe$day, levels = weekdays)
    } else {
      helperframe$day <- factor(helperframe$day, c(levels(helperframe$day), weekdays[!weekdays %in% levels(helperframe$day)]))
      helperframe$day <- factor(helperframe$day, levels = weekdays)
    }

    # plotting Heatmap
    out <- ggplot(helperframe, aes(hour, day)) +
      theme_minimal() +
      geom_tile(aes(fill = `Number of Tokens`), colour = "black") +
      labs(
        title = "Tokens by Weekday and Hour",
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
      scale_x_binned(
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
  }

  # printing output
  print(out)

  # returning data and/or output
  if (return.data == TRUE) {
    # Rename Sender and Anonymous columns again to what they were initially
    if(names.col == "Anonymous"){
      colnames(helperframe)[colnames(helperframe) == "Sender"] <- "Anonymous"
      colnames(helperframe)[colnames(helperframe) == "Placeholder"] <- "Sender"
    }
    return(as.data.frame(helperframe))
  } else {
    return(out)
  }
}
