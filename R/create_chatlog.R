#' @title Creating test data in the structure of 'WhatsApp' chat logs
#'
#' @description Creates a .txt file in the working directory that has the same structure as chat logs exported from 'WhatsApp'. Messages have a timestamp, sender name and message body
#' containing lorem ipsum, emoji, links, smilies, location, omitted media files, linebreaks, self-deleting photos, and 'WhatsApp' system messages. Timestamps are formatted according to specified phone operating system and
#' time format settings. 'WhatsApp' system messages are formatted according to specified phone operating system and language.
#' @param n_messages Number of messages that are contained in the created .txt file.
#' @param n_chatters Number of different chatters present in the created .txt file.
#' @param n_emoji Number of messages that contain emoji. Must be smaller or equal to n_messages.
#' @param n_diff_emoji Number of different emoji that are used in the simulated chat.
#' @param n_links Number of messages that contain links. Must be smaller or equal to n_messages.
#' @param n_locations Number of messages that contain locations. Must be smaller or equal to n_messages.
#' @param n_smilies Number of messages that contain smilies. Must be smaller or equal to n_messages.
#' @param n_diff_smilies Number of different smilies that are used in the simulated chat.
#' @param n_media Number of messages that contain media files. Must be smaller or equal to n_messages.
#' @param media_excluded Whether media files were excluded in simulated export or not. Default is TRUE.
#' @param n_sdp Number of messages that contain self-deleting photos. Must be smaller or equal to n_messages.
#' @param n_deleted Number of messages that contain deleted messages. Must be smaller or equal to n_messages.
#' @param startdate Earliest possible date for messages. Format is 'dd.mm.yyyy'. Timestamps for messages are created automatically between startdate and enddate. Input is interpreted as UTC
#' @param enddate Latest possible date for messages. Format is 'dd.mm.yyyy'. Timestamps for messages are created automatically between startdate and enddate. Input is interpreted as UTC
#' @param language Parameter for the language setting of the exporting phone. Influences structure of system messages
#' @param time_format Parameter for the time format setting of the exporting phone (am/pm vs. 24h). Influences the structure of timestamps.
#' @param os Parameter for the operating system setting of the exporting phone. Influences the structure of timestamps and 'WhatsApp' system messages.
#' @param path Character string for indicating the file path of where to save the file. Can be NA to not save a file. Default is getwd()
#' @param chatname Name for the created .txt file.
#' @export
#' @importFrom checkmate assert_numeric assert_logical
#' @importFrom stats rlnorm rnorm
#' @return A .txt file with a simulated 'WhatsApp' chat containing lorem ipsum but all structural properties of actual chats.
#'
#' @examples
#' SimulatedChat <- create_chatlog(path = NA)
#
create_chatlog <- function(n_messages = 150,
                           n_chatters = 2,
                           n_emoji = 50,
                           n_diff_emoji = 20,
                           n_links = 20,
                           n_locations = 5,
                           n_smilies = 20,
                           n_diff_smilies = 15,
                           n_media = 10,
                           media_excluded = TRUE,
                           n_sdp = 3,
                           n_deleted = 5,
                           startdate = "01.01.2019",
                           enddate = "31.12.2022",
                           language = "german",
                           time_format = "24h",
                           os = "android",
                           path = getwd(),
                           chatname = "Simulated_WhatsR_chatlog") {

  runif <- NULL

  ##### Input Validation ######

  # validating numeric inputs
  assert_numeric(n_messages, lower = 50, upper = 40000, len = 1)
  assert_numeric(n_chatters, lower = 2, upper = 50, len = 1)
  assert_numeric(n_emoji, lower = 0, upper = 80000, len = 1)
  assert_numeric(n_diff_emoji, lower = 1, upper = 3315, len = 1)
  assert_numeric(n_diff_emoji, lower = 1, upper = 321, len = 1)
  assert_numeric(n_links, lower = 0, upper = 1000, len = 1)
  assert_numeric(n_locations, lower = 0, upper = 1000, len = 1)
  assert_numeric(n_smilies, lower = 0, upper = 80000, len = 1)
  assert_numeric(n_media, lower = 0, upper = 1000, len = 1)
  assert_numeric(n_deleted, lower = 0, upper = 1000, len = 1)
  assert_logical(media_excluded,len = 1)

  # ensuring that n_variables are not larger than the number of messages
  if (n_messages <= n_emoji & n_messages <= n_links & n_messages <= n_locations & n_messages <= n_smilies & n_messages <= n_media & n_messages <= n_sdp & n_messages <= n_deleted) {
    warning("The number of messages containing a specific feature must be smaller than the overall number of messages. Try increasing n_messages.")
    stop()
  }

  # validating language
  if (language == "english" | language == "german") {
  } else {
    warning("Variable 'language' must be either 'german' or 'english'")
    stop()
  }

  # validating time_format
  if (time_format == "24h" | time_format == "ampm") {
  } else {
    warning("Variable 'time_format' must be either '24h' or 'ampm'")
    stop()
  }

  # validating OS
  if (os == "android" | os == "ios") {
  } else {
    warning("Variable 'os' must be either 'android' or 'ios'")
    stop()
  }

  # validating startdate
  startdate_check <- try(as.Date(startdate, format = "%d.%m.%Y", tz = "UTC"))
  if ("try-error" %in% class(startdate_check) || is.na(startdate_check)) {
    warning("Variable 'startdate' musst be a character string of format dd.mm.YYYY")
  }

  # validating enddate
  enddate_check <- try(as.Date(enddate, format = "%d.%m.%Y", tz = "UTC"))
  if ("try-error" %in% class(enddate_check) || is.na(enddate_check)) {
    warning("Variable 'enddate' musst be a character string of format dd.mm.YYYY")
  }

  # validate that startdate is before enddate
  sDate <- as.POSIXct(as.Date(startdate, format = "%d.%m.%Y", tz = "UTC"), tz = "UTC")
  eDate <- as.POSIXct(as.Date(enddate, format = "%d.%m.%Y", tz = "UTC"), tz = "UTC")

  if (sDate >= eDate) {
    warning("starting date must be earlier than ending date.")
    stop()
  }

  # checking path variable
  if (!(is.na(path) | is.character(path))) {
    warning("'path' must be NA or a path to a valid location on your system.")
  }

  # checking that there are enough messages to satisfy desired amount of links, emoji etc.
  min_messages <- 20 + n_locations + n_sdp + n_media + n_deleted + max(n_links,n_emoji,n_smilies)

  # adding buffer
  min_messages <- min_messages + 50

  # checking
  if (n_messages < min_messages) {
    warning(paste("'n_messages' is too small to satisfy specified chat characteristics. Should be at least",min_messages,"for this configuration.",sep = " "))
    stop()
  }


  #### Importing data ####

  # Importing smilie dictionary
  smilies <- read.csv(system.file("SmileyDictionary.csv", package = "WhatsR"),
    stringsAsFactors = F
  )[, 2]

  # Limiting smiley dictionary to number of different smilies to sample from
  smilies <- smilies[sample(1:length(smilies), n_diff_smilies)]

  # Importing emoji dictionary
  EmojiDictionary <- read.csv(system.file("EmojiDictionary.csv", package = "WhatsR"),
    header = TRUE,
    stringsAsFactors = FALSE,
    strip.white = FALSE,
    colClasses = "character",
    blank.lines.skip = TRUE
  )

  # Limiting emoji dictionary to number of different emoji to sample from
  EmojiDictionary <- EmojiDictionary[sample(1:dim(EmojiDictionary)[1], n_diff_emoji), ]


  # Importing list of system messages
  WAStrings <- read.csv(system.file("Languages.csv", package = "WhatsR"),
    stringsAsFactors = F,
    fileEncoding = "UTF-8"
  )

  # Importing names for simulated chat
  ExampleNames <- read.csv(system.file("ExampleNames.csv", package = "WhatsR"),
    stringsAsFactors = F,
    fileEncoding = "UTF-8"
  )

  # subsetting WAstrings
  if (language == "english") {
    if (os == "android") {
      WAStrings <- WAStrings[3, 2:25]
    } else {
      WAStrings <- WAStrings[4, 2:25]
    }
  } else {
    if (os == "android") {
      WAStrings <- WAStrings[1, 2:25]
    } else {
      WAStrings <- WAStrings[2, 2:25]
    }
  }

  # importing list of example links
  Links <- read.csv(system.file("ExampleLinks.csv", package = "WhatsR"),
    stringsAsFactors = F
  )


  #### Creating Timestamps ####

  # Timestamp function (taken from: https://stackoverflow.com/questions/42021394/random-incremental-timestamp-in-r)
  RandomTimeStamp <- function(M, sDate = startdate, eDate = enddate) {
    sDate <- as.POSIXct(as.Date(sDate, format = "%d.%m.%Y", tz = "UTC"), tz = "UTC")
    eDate <- as.POSIXct(as.Date(eDate, format = "%d.%m.%Y", tz = "UTC"), tz = "UTC")
    dTime <- as.numeric(difftime(eDate, sDate, units = "sec"))
    sTimeStamp <- sort(runif(M, 0, dTime))
    TimeStamp <- sDate + sTimeStamp
  }

  # creating timestamp
  ts <- RandomTimeStamp(n_messages)


  #### Formatting Timestamps ####

  if (language == "german") {
    if (os == "android") {
      if (time_format == "24h") {
        ts <- strftime(ts, format = "%d.%m.%y, %H:%M - ", tz = "UTC")
      } else {
        ts <- strftime(ts, format = "%d.%m.%y, %I:%M %p - ", tz = "UTC")
      }
    } else {
      if (time_format == "24h") {
        ts <- strftime(ts, format = "[%d.%m.%y, %H:%M:%S] ", tz = "UTC")
      } else {
        ts <- strftime(ts, format = "[%m/%d/%y, %I:%M:%S %p] ", tz = "UTC")
      }
    }
  } else {
    if (os == "android") {
      if (time_format == "24h") {
        ts <- strftime(ts, format = "%m/%d/%y, %H:%M - ", tz = "UTC")
      } else {
        ts <- strftime(ts, format = "%m/%d/%y, %I:%M %p - ", tz = "UTC")
      }
    } else {
      if (time_format == "24h") {
        ts <- strftime(ts, format = "[%m/%d/%y, %H:%M:%S] ", tz = "UTC")
      } else {
        ts <- strftime(ts, format = "[%m/%d/%y, %I:%M:%S %p] ", tz = "UTC")
      }
    }
  }

  #### Creating names ####

  # create vector of chatter names
  Names <- sample(ExampleNames$x, n_chatters)
  Names <- paste(Names, ": ", sep = "")
  Names <- sample(Names, n_messages, replace = TRUE)

  #### Creating Messages #####

  # creating empty message vector
  Messages <- rep(NA, n_messages)

  # importing lorem words
  Lorem <- readRDS(system.file("LoremWords.rds", package = "WhatsR"))

  # Creating lorem ipsum WhatsApp messages
  for (i in seq_along(ts)) {

    # sample number of sentences
    sent_num <- sample(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5), 1)

    # initializing empty vector
    sentences <- rep(NA, sent_num)

    for (j in 1:sent_num) {

      # sample words
      words <- sample(Lorem, round(abs(rnorm(1, 11, 20)) + 0.5, digits = 0))

      # form sentence
      sent <- paste(words, collapse = " ")

      # capitalizing first letter
      letters <- strsplit(sent, "")[[1]]
      sent <- paste(c(toupper(letters[1]), letters[2:length(letters)]), collapse = "")

      # Adding End of sentence mark
      mark <- sample(c(".", "!", "?", "?!", "??", " "), 1)

      # pasting together
      sentences[j] <- paste(sent, mark, collapse = "", sep = "")
    }

    Messages[i] <- paste(sentences, sep = " ", collapse = "")

  }


  ### Making messages realistic:

  ### Add linebreaks & non-breaking space characters

  # adapted from: https://statisticsglobe.com/insert-character-pattern-in-string-r
  fun_insert <- function(x, pos, insert) {
    gsub(
      paste0("^(.{", pos, "})(.*)$"),
      paste0("\\1", insert, "\\2"),
      x
    )
  }

  # inserting linebreaks and &nbsp into messages
  for (i in sample(c(1:n_messages), round(0.05 * n_messages, 0))) {
    Messages[i] <- fun_insert(Messages[i], pos = sample(c(1:20), 1), " \n \n ")
  }

  #### Adding System messages

  # sample messages to replace with WhatsApp system messages
  sm_rows <- sample(2:n_messages, 20)

  # transpose WAStrings for easier handling
  WAStrings <- t(WAStrings)

  # Create system messages from RegExes built to detect them and insert them
  # according to specified language and operating system

  if (language == "german") {

    if (os == "android") {
      # replacing regex strings
      WAStrings <- gsub("$", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("^", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("(.)*?", "Bob", WAStrings, fixed = TRUE)
      WAStrings <- gsub("\\", "", WAStrings, fixed = TRUE)

      # deleting/replacing unnecessary parts
      WAStrings[6] <- "Standort: https://maps.google.com/?q=-37.46874211,-23.82071615"
      WAStrings <- WAStrings[-c(4)]
      WAStrings[20] <- "+ 49 000 000 hat zu 004900000000 gewechselt."
      WAStrings[3] <- "2 Kontakte.vcf (Datei angeh\u0061\u0308ngt)"

      # selecting multi-option system messages
      WAStrings[1] <- gsub("(","",WAStrings[1],fixed = TRUE)
      WAStrings[1] <- gsub(")","",WAStrings[1],fixed = TRUE)
      WAStrings[1] <- sample(unlist(strsplit(WAStrings[1],"|",fixed = TRUE)),1)
      Messages[1] <- WAStrings[1]

      WAStrings[2] <- gsub("(","",WAStrings[2],fixed = TRUE)
      WAStrings[2] <- gsub(")","",WAStrings[2],fixed = TRUE)
      WAStrings[2] <- sample(unlist(strsplit(WAStrings[2],"|",fixed = TRUE)),1)

      WAStrings[21] <- gsub("(","",WAStrings[21],fixed = TRUE)
      WAStrings[21] <- gsub(")","",WAStrings[21],fixed = TRUE)
      WAStrings[21] <- sample(unlist(strsplit(WAStrings[21],"|",fixed = TRUE)),1)

      WAStrings[c(4)] <- sample(c("<Medien ausgeschlossen>","<Videonachricht weggelassen>"),1)

      # replace messages with system messages
      Messages[sm_rows] <- WAStrings[c(3, 5:23)]

    } else {

      # replacing regex strings
      WAStrings <- gsub("$", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("^", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("(.)*?", "Bob", WAStrings, fixed = TRUE)
      WAStrings <- gsub("\\.", ".", WAStrings, fixed = TRUE)

      # deleting/replacing unnecessary parts
      WAStrings[6] <- "Standort: https://maps.google.com/?q=-37.46874211,-23.82071615"
      WAStrings[3] <- "<Anhang: 3 Filename.vcf>"
      WAStrings[21] <- "+ 49 000 000 hat zu 004900000000 gewechselt."
      WAStrings <- WAStrings[-c(4)]

      # selecting multi-option system messages
      WAStrings[1] <- gsub("(","",WAStrings[1],fixed = TRUE)
      WAStrings[1] <- gsub(")","",WAStrings[1],fixed = TRUE)
      WAStrings[1] <- sample(unlist(strsplit(WAStrings[1],"|",fixed = TRUE)),1)
      Messages[1] <- WAStrings[1]

      WAStrings[2] <- gsub("(","",WAStrings[2],fixed = TRUE)
      WAStrings[2] <- gsub(")","",WAStrings[2],fixed = TRUE)
      WAStrings[2] <- sample(unlist(strsplit(WAStrings[2],"|",fixed = TRUE)),1)

      WAStrings[22] <- gsub("(","",WAStrings[22],fixed = TRUE)
      WAStrings[22] <- gsub(")","",WAStrings[22],fixed = TRUE)
      WAStrings[22] <- sample(unlist(strsplit(WAStrings[22],"|",fixed = TRUE)),1)

      WAStrings[c(4)] <- sample(c(paste(c("Bild","Audio","Video","Videonachricht","GIF","Sticker"),"weggelassen"),"Kontaktkarte ausgelassen"),1)

      # replace messages with system messages
      Messages[sm_rows] <- WAStrings[c(3, 5:23)]
    }

  } else {

    if (os == "android") {

      # replacing regex strings
      WAStrings <- gsub("$", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("^", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("(.)*?", "Bob", WAStrings, fixed = TRUE)
      WAStrings <- gsub("\\.", ".", WAStrings, fixed = TRUE)

      # deleting/replacing unnecessary parts
      WAStrings[6] <- "location: https://maps.google.com/?q=-37.46874211,-23.82071615"
      WAStrings <- WAStrings[-c(4)]
      WAStrings[3] <- "3 Filename.vcf (file attached)"
      WAStrings[20] <- "+ 49 000 000 changed to 004900000000."

      # selecting multi-option system messages
      WAStrings[1] <- gsub("(","",WAStrings[1],fixed = TRUE)
      WAStrings[1] <- gsub(")","",WAStrings[1],fixed = TRUE)
      WAStrings[1] <- sample(unlist(strsplit(WAStrings[1],"|",fixed = TRUE)),1)
      Messages[1] <- WAStrings[1]

      WAStrings[2] <- gsub("(","",WAStrings[2],fixed = TRUE)
      WAStrings[2] <- gsub(")","",WAStrings[2],fixed = TRUE)
      WAStrings[2] <- sample(unlist(strsplit(WAStrings[2],"|",fixed = TRUE)),1)

      WAStrings[21] <- gsub("(","",WAStrings[21],fixed = TRUE)
      WAStrings[21] <- gsub(")","",WAStrings[21],fixed = TRUE)
      WAStrings[21] <- sample(unlist(strsplit(WAStrings[21],"|",fixed = TRUE)),1)


      WAStrings[c(4)] <- sample(c("<Media omitted>","<Video message omitted>"),1)

      # replace messages with system messages
      Messages[sm_rows] <- WAStrings[c(3, 5:23)]

    } else {

      # replacing regex strings
      WAStrings <- gsub("$", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("^", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("(.)*?", "Bob", WAStrings, fixed = TRUE)
      WAStrings <- gsub("\\.", ".", WAStrings, fixed = TRUE)

      # deleting/replacing unnecessary parts
      WAStrings[6] <- "Location: https://maps.google.com/?q=-37.46874211,-23.82071615"
      WAStrings[3] <- "<attached: 3 Filename.vcf>"
      WAStrings[21] <- "+ 49 000 000 changed to 004900000000."
      WAStrings <- WAStrings[-c(4)]

      WAStrings[1] <- gsub("(","",WAStrings[1],fixed = TRUE)
      WAStrings[1] <- gsub(")","",WAStrings[1],fixed = TRUE)
      WAStrings[1] <- sample(unlist(strsplit(WAStrings[1],"|",fixed = TRUE)),1)
      Messages[1] <- WAStrings[1]

      WAStrings[2] <- gsub("(","",WAStrings[2],fixed = TRUE)
      WAStrings[2] <- gsub(")","",WAStrings[2],fixed = TRUE)
      WAStrings[2] <- sample(unlist(strsplit(WAStrings[2],"|",fixed = TRUE)),1)

      WAStrings[22] <- gsub("(","",WAStrings[22],fixed = TRUE)
      WAStrings[22] <- gsub(")","",WAStrings[22],fixed = TRUE)
      WAStrings[22] <- sample(unlist(strsplit(WAStrings[22],"|",fixed = TRUE)),1)

      WAStrings[c(4)] <- sample(c(paste(c("image","audio","video","video message","GIF","sticker"),"omitted"),"contact card omitted"),1)

      # replace messages with system messages
      Messages[sm_rows] <- WAStrings[c(3, 5:23)]
    }
  }

  # creating a list of messages that are free for adding stuff to
  all_messages <- 2:n_messages
  free_messages <- all_messages[!all_messages %in% sm_rows]

  #### Adding Locations

  # creating latitude and longitude for location links
  locations <- paste(round(as.numeric(runif(n_locations, -0.005, 1.0049) + sample(-89:89, n_locations, replace = TRUE)), 8),
                     ",",
                     round(as.numeric(runif(n_locations, -0.005, 1.0049) + sample(-89:89, n_locations, replace = TRUE)), 8),
                     sep = ""
  )

  # creating location links
  if (os == "android") {

    if (language == "english") {
      locations <- paste("location: https://maps.google.com/?q=", locations, sep = "")
    } else {
      locations <- paste("Standort: https://maps.google.com/?q=", locations, sep = "")
    }

  } else {

    if (language == "english") {
      locations <- paste("Location: https://maps.google.com/?q=", locations, sep = "")
    } else {
      locations <- paste("Standort: https://maps.google.com/?q=", locations, sep = "")
    }
  }

  # sampling messages to add locations to
  location_rows <- sample(free_messages, n_locations - 1)

  # adding locations to messages
  for (i in location_rows) {
    Messages[i] <- sample(locations,1, replace = TRUE)
  }

  # updating free messages
  free_messages <- free_messages[!free_messages %in% location_rows]

  #### Add self-deleting photos

  # sample messages to replace with indicator for self-deleting photo
  sdp_rows <- sample(free_messages, n_sdp)

  # replace message with empty string as indicator for a self-deleting photo
  for (i in sdp_rows) {
    Messages[i] <- ""
  }

  # updating free messages
  free_messages <- free_messages[!free_messages %in% sdp_rows]


  ## Add media files or exclusion indicators

  # sample messages to replace with indicator for self-deleting photo
  media_rows <- sample(free_messages, n_media)

  if (media_excluded == TRUE) {

    if (os == "android") {

      if (language == "english") {

        # replacing free messages with omittance indicator
        for (i in media_rows) {
          Messages[i] <-  sample(c("<Media omitted>","<Video message omitted>"),1)
        }

      } else {

        # replacing free messages with omittance indicator
        for (i in media_rows) {
          Messages[i] <-  sample(c("<Medien ausgeschlossen>","<Videonachricht weggelassen>"),1)
        }

      }

    } else {

      if (language == "english") {

        # replacing free messages with omittance indicator
        for (i in media_rows) {
          Messages[i] <-  sample(c(paste(c("image","audio","video","video message","GIF","sticker"),"omitted"),"contact card omitted"),1)
        }

      } else {

        # replacing free messages with omittance indicator
        for (i in media_rows) {
          Messages[i] <-  sample(c(paste(c("Bild","Audio","Video","Videonachricht","GIF","Sticker"),"weggelassen"),"Kontaktkarte ausgelassen"),1)
        }

      }

    }

  } else {

    # defining media files
    media_files <- c("PTT-20231224-WA0000.opus",
                     "IMG-20231224-WA0001.jpg",
                     "VID-20231227-WA0002.mp4",
                     "DOC-20240108-WA0007.zip",
                     "DOC-20240108-WA0007.pdf",
                     "DOC-20240108-WA0007.docx")

    # building messages in OS and language specific way
    if (language == "german") {
      if (os == "android") {
        media_files <- paste(media_files, "(Datei angeh\u00E4ngt)")
      } else {
        media_files <- paste("<Anhang: ",media_files, ">", sep = "")
      }
    } else {
      if (os == "android") {
        media_files <- paste(media_files, "(file attached)")
      } else {
        media_files <- paste("<attached: ",media_files, ">", sep = "")
      }
    }
    # replacing messages with media files
    for (i in media_rows) {
      Messages[i] <-  sample(media_files,1)
    }
  }

  # updating free messages
  free_messages <- free_messages[!free_messages %in% media_rows]

  #### Adding Links

  # sampling messages to add links to
  link_rows <- sample(free_messages, n_links)

  # adding link(s) at end of message text
  for (i in link_rows) {
    Messages[i] <- paste(Messages[i], paste(sample(Links$x, sample(c(1:5), 1), replace = TRUE), collapse = " "), sep = " ")
  }

  # updating free messages
  free_messages <- free_messages[!free_messages %in% link_rows]


  #### Adding Emoji

  # sample messages to add emoji to
  emoji_rows <- sample(free_messages, n_emoji)

  # adding emoji to end of message
  for (i in emoji_rows) {
    Messages[i] <- paste(Messages[i], paste(sample(EmojiDictionary$R.native, sample(c(1:5), 1), replace = TRUE), collapse = " "), sep = " ")
  }

  # updating free messages
  free_messages <- free_messages[!free_messages %in% emoji_rows]


  #### Adding Smilies

  # sample messages to add smilies to
  smilie_rows <- sample(free_messages, n_smilies)

  # Adding smilies at end of message
  for (i in smilie_rows) {
    Messages[i] <- paste(Messages[i], paste(sample(smilies, sample(c(1:5), 1), replace = TRUE), collapse = " "), sep = " ")
  }

  # updating free messages
  free_messages <- free_messages[!free_messages %in% smilie_rows]


  #### Adding deleted messages
  deleted_rows <- sample(free_messages, n_deleted)

  # Adding smilies at end of message
  for (i in deleted_rows) {
    Messages[i] <- WAStrings[21]
  }

  # updating free messages
  free_messages <- free_messages[!free_messages %in% deleted_rows]


  #### Pasting timestamps, names and messages together, based on OS structure

  if (os == "ios") {

    # system messages with names
    Messages[sm_rows][c(1:4, 14:17, 19)] <- paste0(ts[sm_rows][c(1:4, 14:17, 19)], Names[sm_rows][c(1:4, 14:17, 19)], Messages[sm_rows][c(1:4, 14:17, 19)])

    # system messages without names
    Messages[sm_rows][c(5:13,18,20)] <- paste0(ts[sm_rows][c(5:13,18,20)], Messages[sm_rows][c(5:13,18,20)])

    # other messages (with names)
    Messages[-c(1, sm_rows)] <- paste0(ts[-c(1, sm_rows)], Names[c(-c(1, sm_rows))], Messages[c(-c(1, sm_rows))])

    # first message
    Messages[1] <- paste0(ts[1], Messages[1])

  } else {

    # system messages with names
    Messages[sm_rows][c(1:4, 15:17, 19)] <- paste0(ts[sm_rows][c(1:4, 15:17, 19)], Names[sm_rows][c(1:4, 15:17, 19)], Messages[sm_rows][c(1:4, 15:17, 19)])

    # system messages without names
    Messages[sm_rows][c(5:14,18,20)] <- paste0(ts[sm_rows][c(5:14,18,20)], Messages[sm_rows][c(5:14,18,20)])

    # other messages (with names)
    Messages[-c(1, sm_rows)] <- paste0(ts[-c(1, sm_rows)], Names[c(-c(1, sm_rows))], Messages[c(-c(1, sm_rows))])

    # first message
    Messages[1] <- paste0(ts[1], Messages[1])
  }

  # write simulated chat log to file
  if (!is.na(path)) {
    writeLines(Messages, paste0(path,"/",chatname, ".txt"))
  }

  # returning results
  return(Messages)
}
