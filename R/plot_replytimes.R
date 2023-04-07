#' @title Plotting replytimes in WhatsApp chatlogs
#' @description Visualizes the replytimes and reactiontimes to messages per author
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param names_col A column indicated by a string that should be accessed to determine the names. Only needs to be changed when \code{\link[WhatsR]{parse_chat}} used the parameter anon = "add" and the column "Anonymous" should be used. Default is "Sender".
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param return_data If TRUE, returns a data frame of LatLon coordinates extracted from the chat for more elaborate plotting. Default is FALSE.
#' @param aggregate_sessions If TRUE, concurrent messages of the same author are aggregated into one session. Default is TRUE.
#' @param plot Type of plot to be returned, options include "box", "dist" and "heatmap".
#' @param type If "replytime", plots display how much time it takes authors to reply to previous message, if "reactiontime", plots display how much time it takes for authors to get responded to.
#' @param exclude_sm If TRUE, excludes the WhatsApp system messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table rleid
#' @importFrom data.table :=
#' @importFrom data.table data.table
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom methods is
#' @export
#' @return Plots for Replytimes or Reactiontimes of authors
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_replytimes(data)

############## Looking at response times between messages
#TODO: Check dist parameter and update function description
plot_replytimes <- function(data,
                            names = "all",
                            names_col = "Sender",
                            starttime = anytime("1960-01-01 00:00"),
                            endtime = Sys.time(),
                            return_data = FALSE,
                            aggregate_sessions = TRUE,
                            plot = "box",
                            type = "replytime",
                            exclude_sm = FALSE) {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  `.` <- Sender <- ReactionTime <- day <- hour <- median <- `Median Reply time` <- NULL

  # catching bad params
  # start- and endtime are POSIXct
  if (is(starttime, "POSIXct") == F) stop("starttime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by anytime().")
  if (is(endtime, "POSIXct") == F) stop("endtime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by anytime().")
  if (starttime >= endtime) stop("starttime has to be before endtime.")

  # names_col must be in preset options
  if (any(!names_col %in% c("Sender", "Anonymous"))) stop("'names_col' has to be either Sender or Anonymous.")

  # names in data or all names (Sender or Anonymous)
  if (names_col == "Sender") {
    if (!("all" %in% names) & any(!names %in% data$Sender)) stop("names has to either be 'all' or a vector of names to include.")}
  else{
    if (!("all" %in% names) & any(!names %in% data$Anonymous)) stop("names has to either be 'all' or a vector of names to include.")}

  # return_data must be bool
  if (!is.logical(return_data)) stop("'return_data' has to be either TRUE or FALSE.")

  # aggregate_sessions must be bool
  if (!is.logical(aggregate_sessions)) stop("aggregate_sessions has to be either TRUE or FALSE.")

  # plot must be one of the the preset options
  if (any(!plot %in% c("box", "dist", "heatmap"))) stop("The plot type has to be 'box', 'dist' or 'heatmap'.") #TODO: Check if dist should be kept in the function

  # type must be one of the the preset options
  if (any(!type %in% c("replytime", "reactiontime"))) stop("Type has to be replytime or reactiontime.")

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")

  #if names_col == "Anonymous", rename to Sender and rename Sender to placeholder
  if (names_col == "Anonymous") {
    colnames(data)[colnames(data) == "Sender"] <- "Placeholder"
    colnames(data)[colnames(data) == "Anonymous"] <- "Sender"
  }

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

  # aggregating sessions into messages
  if (aggregate_sessions == TRUE) {
    # finding start and end postions of streaks
    streaks <- data.table(data$Sender)[, .(start = .I[1], end = .I[.N]), by = rleid(data$Sender)][, rleid := NULL][]

    # we can find start and endtimes of streaks
    Starttime <- data$DateTime[streaks$start]
    Endtime <- data$DateTime[streaks$end]

    # computing time differences for sessions
    Sessionlength <- difftime(Endtime, Starttime, units = "mins")

    # What we need now is a vector of sender names in the right order but without repetitions
    Sessionframe <- cbind.data.frame(
      Sender = rle(as.character(data$Sender))$values,
      Starttime = Starttime,
      Endtime = Endtime,
      MessageAmount = rle(as.character(data$Sender))$lengths,
      Duration = Sessionlength
    )

    # creating new vector for computing the timediff since the last message of another person
    Replytime <- vector()

    # computing duration of sessions of messages from the same sender
    for (i in 1:length(Sessionframe$Starttime)) {
      if (i == 1) {
        Replytime[i] <- NA
      }

      if (i != 1) {
        Replytime[i] <- difftime(Sessionframe$Starttime[i], Sessionframe$Endtime[i - 1], units = "mins")
      }
    }

    # putting Replytimes into the new dataframe:
    Sessionframe <- cbind.data.frame(Sessionframe,
      ReactionTime = Replytime
    )
  } else {
    # building frame without sessions
    Sessionframe <- cbind.data.frame(
      Sender = data$Sender,
      Starttime = data$DateTime,
      Endtime = data$DateTime,
      Messageamount = 1,
      Duration = 0
    )

    # computing replytime
    # creating new vector for computing the timediff since the last message of another person
    Replytime <- vector()

    # computing duration of sessions of messages from the same sender
    for (i in 1:length(Sessionframe$Starttime)) {
      if (i == 1) {
        Replytime[i] <- NA
      }

      if (i != 1) {
        Replytime[i] <- difftime(Sessionframe$Starttime[i], Sessionframe$Endtime[i - 1], units = "mins")
      }
    }

    # putting Replytimes into the new dataframe:
    Sessionframe <- cbind.data.frame(Sessionframe,
      ReactionTime = Replytime
    )
  }

  # Add a column idicating how long it took for the message to be replied to
  RepliedToAfter <- c(Sessionframe$ReactionTime[2:length(Sessionframe$ReactionTime)], NA)

  # Adding it to the sessionframe
  Sessionframe <- cbind.data.frame(
    Sessionframe,
    RepliedToAfter
  )

  if (plot == "box") {
    if (type == "replytime") {
      # Distribution of response times per person (logscale)
      out <- ggplot(Sessionframe, aes(x = Sender, y = log(ReactionTime + 1), color = Sender)) +
        theme_minimal() +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) +
        labs(
          title = "Distribution of Time it takes to respond per Person",
          subtitle = paste(starttime, " - ", endtime)
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ylab("Distribution of Reply times in log(Minutes+1)")
    }

    if (type == "reactiontime") {
      # Distribution of responsd to times per person (logscale)
      out <- ggplot(Sessionframe, aes(x = Sender, y = log(RepliedToAfter + 1), color = Sender)) +
        theme_minimal() +
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) +
        labs(
          title = "Distribution of how quickly messages are responded to per Person",
          subtitle = paste(starttime, " - ", endtime)
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ylab("Distribution of Reply times in log(Minutes+1)")
    }
  }

  #TODO: Add this back in and test it?
  # if (plot == "dist") {
  #
  #   if (type == "replytime") {
  #
  #     out <- ggplot(Sessionframe, aes(log(ReactionTime + 1))) +
  #       geom_histogram() +
  #       theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) +
  #       labs(title = "Overall Distribution of Reaction Times to previous messages",
  #            subtitle = paste(starttime, " - ", endtime),
  #            x = "Reaction times in log(Minutes+1)",
  #            y = "Frequency")
  #
  #   }
  #
  #   if (type == "reactiontime") {
  #
  #     out <- ggplot(Sessionframe, aes(log(RepliedToAfter + 1))) +
  #       geom_histogram() +
  #       theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) +
  #       labs(title = "Overall Distribution of Time it takes to respond to previous messages",
  #            subtitle = paste(starttime, " - ", endtime),
  #            x = "Reaction times in log(Minutes+1)",
  #            y = "Frequency")
  #   }
  #
  # }

  if (plot == "heatmap") {
    NewFrame <- Sessionframe[c(1, 3, 6, 7)]

    # creating time data
    NewFrame$hour <- as.POSIXlt(NewFrame$Endtime)$hour
    NewFrame$year <- as.POSIXlt(NewFrame$Endtime)$year + 1900
    NewFrame$day <- weekdays(as.POSIXlt(NewFrame$Endtime), abbreviate = FALSE)


    # shaping dataframe
    if (type == "replytime") {
      helperframe2 <- NewFrame %>%
        group_by(day, hour) %>%
        summarise("Median Reply time" = median(ReactionTime))
    }

    if (type == "reactiontime") {
      helperframe2 <- NewFrame %>%
        group_by(day, hour) %>%
        summarise("Median Reply time" = median(RepliedToAfter))
    }

    # factor ordering
    weekdays <- rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    helperframe2$day <- as.factor(helperframe2$day)

    if (sum(weekdays %in% levels(helperframe2$day)) == 7) {
      helperframe2$day <- factor(helperframe2$day, levels = weekdays)
    } else {
      helperframe2$day <- factor(helperframe2$day, c(levels(helperframe2$day), weekdays[!weekdays %in% levels(helperframe2$day)]))
      helperframe2$day <- factor(helperframe2$day, levels = weekdays)
    }

    if (type == "reactiontime") {
      title_string <- "Median Answer Time"
    } else {
      title_string <- "Median Reply time"
    }

    # plotting Heatmap
    out <- ggplot(helperframe2, aes(hour, day)) +
      theme_minimal() +
      geom_tile(aes(fill = `Median Reply time`), colour = "black", width = 1) +
      labs(
        title = "Links by Weekday and Hour",
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
  }

  # printing output
  print(out)

  # returning data
  if (return_data == TRUE) {
    if (plot == "dist" | plot == "box") {
      # Rename Sender and Anonymous columns again to what they were initially
      if (names_col == "Anonymous") {
        colnames(Sessionframe)[colnames(Sessionframe) == "Sender"] <- "Anonymous"
        colnames(Sessionframe)[colnames(Sessionframe) == "Placeholder"] <- "Sender"
      }
      return(as.data.frame(Sessionframe))
    } else {
      # Rename Sender and Anonymous columns again to what they were initially
      if (names_col == "Anonymous") {
        colnames(helperframe2)[colnames(helperframe2) == "Sender"] <- "Anonymous"
        colnames(helperframe2)[colnames(helperframe2) == "Placeholder"] <- "Sender"
      }
      return(as.data.frame(helperframe2))
    }
  } else {
    return(out)
  }
}
