#' @title Visualizing media files in WhatsApp chatlogs
#' @description Creates a list of basic information about a single WhatsApp chatlog
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}.
#' @param names A vector of author names that the plots will be restricted to.
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Is parsed with \code{\link[anytime]{anytime}}. Standard format is "yyyy-mm-dd hh:mm".
#' @param use_filetype If TRUE, shortens sent files to file types.
#' @param min_occur The minimum number of occurrences a media type has to have to be included in the visualization. Default is 1.
#' @param return_data If TRUE, returns the subsetted data frame. Default is FALSE.
#' @param media_vec A vector of media types that the visualizations will be restricted to.
#' @param plot The type of plot that should be outputted. Options include "heatmap", "cumsum", "bar" and "splitbar".
#' @param exclude_sm If TRUE, excludes the WhatsApp system messages from the descriptive statistics. Default is FALSE.
#' @import ggplot2
#' @importFrom anytime anytime
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom stringi stri_extract_all
#' @importFrom methods is
#' @export
#' @return Plots and/or the subsetted data frame based on author names, datetime and emoji occurrence
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_media(data, plot = "heatmap")
#'
# Visualizing sent Media files
plot_media <- function(data,
                       names = "all",
                       starttime = anytime("1960-01-01 00:00"),
                       endtime = Sys.time(),
                       use_filetype = TRUE,
                       min_occur = 1,
                       return_data = FALSE,
                       media_vec = "all",
                       plot = "bar",
                       exclude_sm = FALSE) {

  # First of all, we assign local variables with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  defaultW <- Dates <- Sender <- Media <- Frequency <- day <- hour <- n <- `Number of Media files` <- ave <- total <- Var1 <- Freq.Freq <- n <- NULL

  # function to shorten media files to filetypes
  if (use_filetype == TRUE) {

    # function for shortening filenames to filetypes
    shortener <- function(x) {
      stri_extract_all(unlist(x), regex = "[^.]+$", simplify = FALSE)}

    }

  # catching bad params
  # start- and endtime are POSIXct
  if (is(starttime, "POSIXct") == F) stop("starttime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by anytime().")
  if (is(endtime, "POSIXct") == F) stop("endtime has to be a character string in the form of 'yyyy-mm-dd hh:mm' that can be converted by anytime().")
  if (starttime >= endtime) stop("starttime has to be before endtime.")

  # min_occur must be >= 1
  if (min_occur < 1) stop("Please provide a min_occur of >= 1.")

  # return_data must be bool
  if (!is.logical(return_data)) stop("return_data has to be either TRUE or FALSE.")

  # media_vec must be in data
  if (!("all" %in% media_vec) & any(!media_vec %in% data$Media)) stop("media_vec has to either be \"all\" or a vector of emojis to include.")

  # plot must be one of the the preset options
  if (any(!plot %in% c("heatmap", "cumsum", "bar", "splitbar"))) stop("The plot type has to be heatmap, cumsum, bar or splitbar.")

  # exclude_sm must be bool
  if (!is.logical(exclude_sm)) stop("exclude_sm has to be either TRUE or FALSE.")

  # use_filetype must be bool
  if (!is.logical(use_filetype)) stop("use_filetype has to be either TRUE or FALSE.")

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

  # This tells us if at least one media file is present (if it's TRUE then theres at least one media file)
  MediaPresent <- !sapply(sapply(data$Media, is.na), sum)

  # This tells us how many elements are in each list element (includes NA aswell)
  NoElements <- lengths(data$Media)

  # We take the New counter and set it to zero where-ever no media files are present
  NoElements[is.na(data$Media)] <- 0

  # Media
  UnlistedMedia <- unlist(data$Media)
  NewMedia <- UnlistedMedia[!is.na(UnlistedMedia)]

  # Senders
  NewSender <- list()

  for (i in seq_along(data$Sender)) {
    NewSender[[i]] <- rep(data$Sender[i], NoElements[i])
  }

  NewSender <- unlist(NewSender)

  # New Dates
  NewDates <- list()

  for (i in seq_along(data$DateTime)) {
    NewDates[[i]] <- rep(data$DateTime[i], NoElements[i])
  }

  NewDates <- as.POSIXct(unlist(NewDates), origin = "1970-01-01")

  # pasting together
  options(stringsAsFactors = FALSE)
  NewFrame <- cbind.data.frame(NewDates, NewSender, NewMedia)

  # creating time data
  NewFrame$hour <- as.POSIXlt(NewFrame$NewDates)$hour
  NewFrame$year <- as.POSIXlt(NewFrame$NewDates)$year + 1900
  NewFrame$day <- weekdays(as.POSIXlt(NewFrame$NewDates), abbreviate = FALSE)

  # setting correct media_vec
  if (length(media_vec) == 1 && media_vec == "all") {
    media_vec <- unique(NewMedia)
  }

  if (use_filetype == TRUE) {
    NewFrame$NewMedia <- trimws(unlist(shortener(NewFrame$NewMedia)))
    media_vec <- trimws(unlist(shortener(media_vec)))
 } else {}

  # restricting to media_vec range
  NewFrame <- NewFrame[is.element(NewFrame$NewMedia, media_vec), ]

  if (dim(NewFrame)[1] == 0) {
    # exit
    warning("No media elements defined by media_vec are contained in the chat")
    stop()
  }

  if (plot == "heatmap") {
    # shaping dataframe
    helperframe2 <- NewFrame %>%
      group_by(day, hour) %>%
      summarise("Number of Media files" = n())

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
    out <- ggplot(helperframe2, aes(hour, day)) +
      theme_minimal() +
      geom_tile(aes(fill = `Number of Media files`), colour = "black", width = 1) +
      labs(
        title = "Media by Weekday and Hour",
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

    # printing
    print(out)

    if (return_data == TRUE) {
      # returning
      return(as.data.frame(helperframe2))
    } else {
      return(out)
    }
  }


  if (plot == "cumsum") {
    # cumulative number of media files per sender
    NewFrame$counter <- rep(1, length(NewFrame$NewMedia))
    NewFrame$total <- ave(NewFrame$counter, NewFrame$NewSender, FUN = cumsum)

    names(NewFrame) <- c("Dates", "Sender", "Media", "hour", "year", "day", "counter", "total")

    # constructing graph (addede group = 1 to remove error message)
    out <- ggplot(NewFrame, aes(x = Dates, y = total, color = Sender, group =1)) +
      theme_minimal() +
      geom_line() +
      labs(
        title = "Cumulative number of Media files sent",
        subtitle = paste(starttime, " - ", endtime)
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Time") +
      ylab("Total Media files sent") +
      # theme(legend.title = element_text("Media")) +
      geom_point()

    # pinting plot
    suppressMessages(print(out))

    if (return_data == TRUE) {
      # returning
      return(as.data.frame(NewFrame))
    } else {
      return(out)
    }
  }

  if (plot == "bar") {
    # Converting to dataframe to make it usable by ggplot
    df <- cbind.data.frame(sort(table(NewFrame$NewMedia)))

    # restructuring when we have only 1 type
    if (nrow(df) == 1) {
      names <- rownames(df)
      rownames(df) <- NULL
      df <- cbind(names, df)
    }

    # setting names
    names(df) <- c("Media", "Frequency")

    if (dim(df)[1] == 0) {
      # exit
      warning("No media elements defined by media_vec are contained in the chat")
      stop()
    }

    # Visualizig the distribution of Mdia file types (file types sent more than x)
    out <- ggplot(df[df$Frequency >= min_occur, ], aes(x = Media, y = Frequency, fill = Media)) +
      theme_minimal() +
      geom_bar(stat = "identity") +
      labs(
        title = "Distribution of sent Media files",
        subtitle = paste(starttime, " - ", endtime),
        x = "Media file types",
        y = "Frequency"
      ) +
      theme( # legend.title = element_text("Media"),
        axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)
      )

    # printing
    print(out)

    # return data
    if (return_data == TRUE) {
      # returning
      return(as.data.frame(df))
    } else {
      return(out)
    }
  }

  if (plot == "splitbar") {

    ## Summarize per Sender who often each domain was sent
    SumFrame <- group_by(NewFrame, NewSender, NewMedia) %>% summarise(n = n())
    SumFrame <- SumFrame[SumFrame$n >= min_occur, ]

    names(SumFrame) <- c("Sender", "Media", "Frequency")

    # building graph object
    out <- ggplot(SumFrame, aes(x = Sender, y = Frequency, fill = as.factor(Media))) +
      theme_minimal() +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(
        title = "Media files sent per Person",
        subtitle = paste(starttime, " - ", endtime),
        x = "Sender",
        y = "Frequency"
      ) #+
    # theme(legend.title = element_text("Media"))


    # printing
    print(out)

    # return data
    if (return_data == TRUE) {
      # returning
      return(as.data.frame(SumFrame))
    } else {
      return(out)
    }
  }

  # unmuting useless dplyr message
  options(warn = defaultW)
}
