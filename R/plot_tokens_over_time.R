#' @title Distribution of Tokens over time
#' @description Summarizes the distribution of tokens over time
#' @param data A WhatsApp chatlog that was parsed with WhatsAppParse()
#' @param names A vector of author names that the Plots will be restricted to
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with anytime(). Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with anytime(). Standard format is "yyyy-mm-dd hh:mm".
#' @param plot Time resolution for plots. Options include "year", "month", "day", "hour", "heatmap" and "alltime". Default is "alltime".
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
                                  starttime = anytime("1960-01-01 00:00"),
                                  endtime = Sys.time(),
                                  plot = "alltime") {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  DateTime <- TokCount <- Sender <- year <- day <- hour <- `Number of Tokens` <- NULL

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

  # building helper dataframe
  helperframe <- data[,c("DateTime","Sender","TokCount")]

  # creating time data
  helperframe$hour <- as.POSIXlt(helperframe$DateTime)$hour
  helperframe$year <- as.POSIXlt(helperframe$DateTime)$year + 1900
  helperframe$day <- weekdays(as.POSIXlt(helperframe$DateTime), abbreviate = FALSE)

  # basic barchart
  if (plot == "alltime") {

    # plotting chart
    print(ggplot(data,aes(x = DateTime, y = TokCount, color = Sender, fill = Sender)) +
            geom_bar(stat = "identity") +
            labs(title = "Sent Tokens by Persons over Time",
                 subtitle = paste(starttime, " - ", endtime))  +
            xlab("Persons") +
            ylab("Sent Tokens"))

  }

  # barchart by year
  if (plot == "year") {

    # plotting by year
    print(ggplot(helperframe, aes(x = as.integer(year), y = TokCount, color = Sender, fill = Sender)) +
            geom_bar(stat = "identity") +
            labs(title = "Tokens by Years",
                 subtitle = paste(starttime, " - ", endtime)) +
            xlab("Year") +
            scale_x_discrete(limits = unique(helperframe$year)))
  }

  if (plot == "weekday") {

    # plotting by weekday
    print(ggplot(helperframe, aes(x = day, y = TokCount, color = Sender, fill = Sender)) +
            geom_bar(stat = "identity") +
            labs(title = "Tokens by day of the week",
                 subtitle = paste(starttime, " - ", endtime)) +
            xlab("Weekday")  +
            scale_x_discrete(limits = c("Monday",
                                        "Tuesday",
                                        "Wednesday",
                                        "Thursday",
                                        "Friday",
                                        "Saturday",
                                        "Sunday")))
  }

  if (plot == "hours") {

    # plotting by hour
    print(ggplot(helperframe, aes(x = hour, y = TokCount, color = Sender, fill = Sender)) +
            geom_bar(stat = "identity") +
            labs(title = "Tokens by hour of the day",
                 subtitle = paste(starttime, " - ", endtime)) +
            xlab("Hour of Day") +
            scale_x_continuous(breaks = seq(-0.5,23.5,1),
                               limits = c(-0.5,23.5),
                               labels = c("00:00",
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
                                          "24:00")) +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_text(angle = 90, hjust = 1)))
  }


  if (plot == "heatmap") {

    # TODO: Add option in the dataframe shaper to average and median instead of summarize

    # shaping dataframe
    helperframe2 <- helperframe %>%
      group_by(day, hour) %>%
      summarise("Number of Tokens" = sum(TokCount))

    # factor ordering
    weekdays <- rev(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
    helperframe2$day <- as.factor(helperframe2$day)

    if (sum(weekdays %in% levels(helperframe2$day)) == 7) {

      helperframe2$day <- factor(helperframe2$day, levels = weekdays)

    } else {

      helperframe2$day <- factor(helperframe2$day,c(levels(helperframe2$day),weekdays[!weekdays %in% levels(helperframe2$day)]))
      helperframe2$day <- factor(helperframe2$day, levels = weekdays)

    }

    # plotting Heatmap
    print(ggplot(helperframe2, aes(hour, day)) +
            geom_tile(aes(fill = `Number of Tokens`), colour = "black") +
            labs(title = "Tokens by Weekday and Hour",
                 subtitle = paste(starttime, " - ", endtime),
                 x = "",
                 y = "") +
            scale_fill_distiller(palette = "YlGnBu", direction = 1) +
            scale_y_discrete(drop = FALSE) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.ticks.x = element_blank(),
                  legend.position = "bottom",
                  legend.key.width = unit(2, "cm"),
                  panel.grid = element_blank()) +
            coord_equal() +
            scale_x_continuous(breaks = seq(-0.5,23.5,1),
                               limits = c(-0.5,23.5),
                               labels = c("00:00",
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
                                          "24:00")))

  }

}
