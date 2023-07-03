#' @title Distribution of Tokens over time
#' @description Summarizes the distribution of user-generated tokens over time
#' @param data A 'WhatsApp' chat log that was parsed with \code{\link[WhatsR]{parse_chat}} with parameters anonimize = FALSE or anonimize = "add".
#' @param names A vector of author names that the plots will be restricted to.
#' @param names_col A column indicated by a string that should be accessed to determine the names. Only needs to be changed when \code{\link[WhatsR]{parse_chat}} used the parameter anon = "add" and the column "Anonymous" should be used. Default is "Sender".
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[base]{as.POSIXct}}. Standard format is "yyyy-mm-dd hh:mm". Is interpreted as UTC to be compatible with 'WhatsApp' timestamps.
#' @param plot Type of plot to be returned. Options are "year", "day", "hour", "heatmap" and "alltime". Default is "alltime".
#' @param return_data If TRUE, returns the subset data frame. Default is FALSE.
#' @param exclude_sm If TRUE, excludes the 'WhatsApp' system messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom methods is
#' @export
#' @return A summary of tokens over time
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_tokens_over_time(data)

############# Tokens over time
plot_tokens_over_time <- function(data,
                                  names = "all",
                                  names_col = "Sender",
                                  starttime = "1960-01-01 00:00",
                                  endtime = "2200-01-01 00:00",
                                  plot = "alltime",
                                  return_data = FALSE,
                                  exclude_sm = FALSE) {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  DateTime <- TokCount <- Sender <- year <- day <- hour <- `Number of Tokens` <- NULL

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
  if (any(!plot %in% c("year", "day", "hour", "heatmap", "alltime"))) stop("The plot type has to be 'year', 'day', 'hour', 'heatmap' or 'alltime'.")

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")

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

  # building helper dataframe
  helperframe <- data[, c("DateTime", "Sender", "TokCount")]

  # creating time data
  helperframe$hour <- as.POSIXlt(helperframe$DateTime,tz="UTC")$hour
  helperframe$year <- as.POSIXlt(helperframe$DateTime,tz="UTC")$year + 1900
  helperframe$day <- weekdays(as.POSIXlt(helperframe$DateTime,tz="UTC"), abbreviate = FALSE)

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

  }


  if (plot == "heatmap") {
    # TODO: Add option in the dataframe shaper to average and median instead of summarize?

    # shaping dataframe
    helperframe <- helperframe %>%
      group_by(day, hour) %>%
      summarise("Number of Tokens" = sum(TokCount, na.rm = TRUE))

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
  if (return_data == TRUE) {
    return(as.data.frame(helperframe))
  } else {
    return(out)
  }
}
