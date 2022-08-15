#' @title Visualize the Network of replies in Whatsapp chatlogs
#' @description Plots a network for replies between authors in chatlogs
#' @param data A WhatsApp chatlog that was parsed with \code{\link[WhatsR]{parse_chat}}
#' @param names A vector of author names that the visulaization will be restricted to. Non-listed authors will be removed
#' @param starttime Datetime that is used as the minimum boundary for exclusion. Input is parsed with anytime(). Standard format is "yyyy-mm-dd hh:mm".
#' @param endtime Datetime that is used as the maximum boundary for exclusion. Input is parsed with anytime(). Standard format is "yyyy-mm-dd hh:mm".
#' @param return.data If TRUE, returns a dataframe of subsequent interactions with senders and recipients. Default is FALSE.
#' @param collapse_sessions Whether multiple subsequent messages by the same sender should be collapsed into one row. Default is FALSE.
#' @param edgetype What type of content is displayed as an edge. Must be one of "TokCount","EmojiCount","SmilieCount","LocationCount","URLCount","MediaCount" or "n".
#' @importFrom anytime anytime
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table rleid
#' @importFrom data.table :=
#' @importFrom dplyr group_by summarise %>% mutate ungroup row_number
#' @importFrom visNetwork visNetwork visEdges
#' @export
#' @return A network visualization of authors in WhatsApp chatlogs wher each subsequent message is considered a reply to the previous one.
#' @examples
#' data <- readRDS(system.file("ParsedWhatsAppChat.rds", package = "WhatsR"))
#' plot_network(data)


### visualizing Distribution of reply times (only possible between multiple senders and recipients: n > 2)
plot_network <- function(data,
                         names = "all",
                         starttime = anytime("1960-01-01 00:00"),
                         endtime = Sys.time(),
                         return.data = FALSE,
                         collapse_sessions=FALSE,
                         edgetype = "n"
                        ) {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  mutate <- trials <- start <- streak_id <- ungroup <- `draw_network` <- `.` <- `get_streaks` <- `%v%<-` <- `lagged` <- `lag` <- NULL

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
    names <- unique(data$Sender)[unique(data$Sender) != "WhatsApp System Message"]

  }

  # limiting data to time and namescope
  data <- data[is.element(data$Sender,names) & data$DateTime >= starttime & data$DateTime <= endtime,]

  # We need to exclude the WhatsApp system messages
  Tempframe <- data[data$Sender != "WhatsApp System Message",]

  # function for unlisting and counting elements ( THIS IS WHERE SHIT BREAKS)
  Unlist_counter <- function(x){if(all(is.na(unlist(x)))){x <- NA} else{x <- length(unlist(x))}}


  # collapsing into messages into sessions
  if (collapse_sessions == TRUE) {

      # finding start and end postions of streaks
      streaks <- data.table(Tempframe$Sender)[ , .(start = .I[1], end = .I[.N]), by = rleid(Tempframe$Sender)][, rleid := NULL][]

      # we find start and endtimes of streaks
      Starttime <- data$DateTime[streaks$start]
      Endtime <- data$DateTime[streaks$end]

      # creating new vector for time difference between sessions
      SessionDiff <- vector()

      # computing time differences for sessions
      Sessionlength <- difftime(Endtime,Starttime, units = "mins")

      # What we need now is a vector of sender names in the right order but without repetitions
      # and to put int into a new dataframe
      Sessionframe <- cbind.data.frame(Sender = rle(as.character(Tempframe$Sender))$values,
                                       Starttime = Starttime,
                                       Endtime = Endtime,
                                       MessageAmount = rle(as.character(Tempframe$Sender))$lengths,
                                       Duration = Sessionlength)

      # creating new vector for computing the timediff since the last message of another person
      Replytime <- vector()

      for (i in 1:length(Sessionframe$Starttime)) {

        if (i == 1) {

          Replytime[i] <- NA

        }

        if (i != 1) {

          Replytime[i] <- difftime(Sessionframe$Starttime[i],Sessionframe$Endtime[i - 1], units = "mins")

        }

      }

      # putting it into the new dataframe:
      Sessionframe <- cbind.data.frame(Sessionframe,
                                       Replytime)



      # We need to reverse the order first so we can interpret edges from A -> B as a response of A to B
      # We take the Sessionframe as a basis because it excludes the WhatsApp System Messages already and
      # accounts for sessions
      Graphframe <- Sessionframe[seq(dim(Sessionframe)[1],1),]

      # computing aggregate counts of tokens, smilies, emoji etc

      # taken form: https://www.r-bloggers.com/2020/06/detecting-streaks-in-r/
      get_streaks <- function(vec){
        x <- data.frame(trials=vec)
        x <- x %>% mutate(lagged=lag(trials)) %>% #note: that's dplyr::lag, not stats::lag
          mutate(start=(trials != lagged))
        x[1, "start"] <- TRUE
        x <- x %>% mutate(streak_id=cumsum(start))
        x <- x %>% group_by(streak_id) %>% mutate(streak=row_number()) %>%
          ungroup()
        return(x)
      }

      # computing a new frame for streask/sessions
      streak_frame <- get_streaks(Tempframe$Sender)
      Extended_streak_lengths <- sapply(rle(as.character(Tempframe$Sender))$lengths, function(x){unlist(rep(x,times=x))})
      Extended_streak_lengths <- unlist(Extended_streak_lengths)

      # calculating easy metrics
      Sender <- streak_frame[streak_frame$start == TRUE,]$trials
      AnsweredTo <- streak_frame[streak_frame$start == TRUE,]$lagged
      Timestamp <- Tempframe[streak_frame$start == TRUE,]$DateTime
      Replytime <- rev(Graphframe$Replytime)
      MessageAmount <- rev(Graphframe$MessageAmount)

      # initializing empty strings
      TokCount <- rep(NA,dim(Graphframe)[1])
      SmilieCount <- rep(NA,dim(Graphframe)[1])
      EmojiCount <- rep(NA,dim(Graphframe)[1])
      LocationCount <- rep(NA,dim(Graphframe)[1])
      MediaCount <- rep(NA,dim(Graphframe)[1])
      URLCount <- rep(NA,dim(Graphframe)[1])

      # counting in for loop
      counter <- 1


      # CHECK inside the if statemetns for condition length

      for (i in which(streak_frame$start == TRUE)) {

        # aggregating over streaks/sessions

        # Smilies
        if (all(is.na(unlist(Tempframe$Smilies[i:(i+Extended_streak_lengths[i]-1)])))) {
          SmilieCount[counter] <- NA
        } else {
          SmilieCount[counter] <- length(unlist(Tempframe$Smilies[i:(i+Extended_streak_lengths[i]-1)])[!is.na(unlist(Tempframe$Smilies[i:(i+Extended_streak_lengths[i]-1)]))])
        }

        # Emoji
        if (all(is.na(unlist(Tempframe$Emoji[i:(i+Extended_streak_lengths[i]-1)])))) {
          EmojiCount[counter] <- NA
        } else {
          EmojiCount[counter] <- length(unlist(Tempframe$Emoji[i:(i+Extended_streak_lengths[i]-1)])[!is.na(unlist(Tempframe$Emoji[i:(i+Extended_streak_lengths[i]-1)]))])
        }

        # Locations
        if (all(is.na(unlist(Tempframe$Location[i:(i+Extended_streak_lengths[i]-1)])))) {
          LocationCount[counter] <- NA
        } else {
          LocationCount[counter] <- length(unlist(Tempframe$Location[i:(i+Extended_streak_lengths[i]-1)])[!is.na(unlist(Tempframe$Location[i:(i+Extended_streak_lengths[i]-1)]))])
        }

        # Media
        if (all(is.na(unlist(Tempframe$Media[i:(i+Extended_streak_lengths[i]-1)])))) {
          MediaCount[counter] <- NA
        } else {
          MediaCount[counter] <- length(unlist(Tempframe$Media[i:(i+Extended_streak_lengths[i]-1)])[!is.na(unlist(Tempframe$Media[i:(i+Extended_streak_lengths[i]-1)]))])
        }

        # URLs
        if (all(is.na(unlist(Tempframe$URL[i:(i+Extended_streak_lengths[i]-1)])))) {
          URLCount[counter] <- NA
        } else {
          URLCount[counter] <- length(unlist(Tempframe$URL[i:(i+Extended_streak_lengths[i]-1)])[!is.na(unlist(Tempframe$URL[i:(i+Extended_streak_lengths[i]-1)]))])
        }

        # TokCount
        if (all(is.na(unlist(Tempframe$TokCount[i:(i+Extended_streak_lengths[i]-1)])))) {
          TokCount[counter] <- NA
        } else {
          TokCount[counter] <- sum(unlist(Tempframe$TokCount[i:(i+Extended_streak_lengths[i]-1)])[!is.na(unlist(Tempframe$TokCount[i:(i+Extended_streak_lengths[i]-1)]))])
        }

        # counting up
        counter <- (counter + 1)

      }

      # pasting everything together
      # combining into dataset
      NetFrame <- cbind.data.frame(Sender,
                                   AnsweredTo,
                                   Timestamp,
                                   TokCount,
                                   SmilieCount,
                                   EmojiCount,
                                   LocationCount,
                                   MediaCount,
                                   URLCount,
                                   Replytime,
                                   MessageAmount)

  } else {

    Sender <- Tempframe$Sender
    Timestamp <- Tempframe$DateTime
    AnsweredTo <- c(NA,Tempframe$Sender[1:length(Tempframe$Sender)-1])
    TokCount <-Tempframe$TokCount
    SmilieCount <- sapply(Tempframe$Smilies,Unlist_counter)
    EmojiCount <- sapply(Tempframe$Emoji,Unlist_counter)
    LocationCount <- sapply(Tempframe$Location,Unlist_counter)
    MediaCount <- sapply(Tempframe$Media,Unlist_counter)
    URLCount <- sapply(Tempframe$URL,Unlist_counter)

    # combining into dataset
    NetFrame <- cbind.data.frame(Sender,AnsweredTo,Timestamp,TokCount,SmilieCount,EmojiCount,LocationCount,MediaCount,URLCount)

  }

  # deleting counter
  counter <<- NULL

  # specifiying unique interactions
  Interaction <- paste(NetFrame$Sender,NetFrame$AnsweredTo)
  Added_Netframe <- cbind.data.frame(Interaction,NetFrame)

  # summing over unique interactions
  Added_Netframe <- Added_Netframe %>%
    group_by(Interaction) %>%
    summarise(TokCount = sum(TokCount, na.rm = TRUE),
              EmojiCount = sum(EmojiCount, na.rm = TRUE),
              SmilieCount = sum(SmilieCount, na.rm = TRUE),
              LocationCount = sum(LocationCount, na.rm = TRUE),
              URLCount = sum(URLCount, na.rm = TRUE),
              MediaCount = sum(MediaCount, na.rm = TRUE),
              n = n())


  # defining function for drawing network with parameters
  draw_network <- function(dataframe, edgewidth = edgetype) {

    # putting together senders and recipients from unique interactions
    Sender_answered <- strsplit(dataframe$Interaction," ")
    dataframe$Sender <- sapply(Sender_answered,`[[`,1)
    dataframe$AnsweredTo <- sapply(Sender_answered,`[[`,2)

    dataframe <- cbind.data.frame(dataframe[,8:10],dataframe[,2:7],N_messages = dataframe$n)

    # Building network
    nodes = data.frame(id=unique(dataframe$Sender),
                       title=unique(dataframe$Sender),
                       label=unique(dataframe$Sender))

    #scaling function from https://stackoverflow.com/questions/18303420/how-to-map-a-vector-to-a-different-range-in-r
    linMap <- function(x, from, to) {
      # Shifting the vector so that min(x) == 0
      x <- x - min(x)
      # Scaling to the range of [0, 1]
      x <- x / max(x)
      # Scaling to the needed amplitude
      x <- x * (to - from)
      # Shifting to the needed level
      x + from
    }

    edges = data.frame(from = dataframe$Sender[dataframe[edgewidth] > 0],
                       to = dataframe$AnsweredTo[dataframe[edgewidth] > 0],
                       width=linMap(dataframe[dataframe[edgewidth] > 0,edgewidth],1,10),
                       arrows="to",
                       title = as.character(sapply(dataframe[dataframe[edgewidth] > 0,edgewidth],function(x){paste(edgewidth,": ",x)})))

    # plotting network
    network <- visNetwork(nodes,
                          edges,
                          height = "1000px",
                          width = "100%",
                          main="Network of WhatsApp Chat Replies",
                          submain = paste("Edges are representing sent ",edgewidth)) |> visEdges(physics = FALSE,
                                                                                                 smooth=list(enabled = TRUE))

    # return object
    return(network)

  }

  if (return.data == TRUE) {

    # drawing network
    print(draw_network(Added_Netframe,edgewidth = edgetype))

    # returning result
    return(as.data.frame(Added_Netframe))


  } else {

    # drawing network
    draw_network(Added_Netframe,edgewidth = edgetype)

  }
}
