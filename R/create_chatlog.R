#' @title Creating test datasets in the structure of WhatsApp chatlogs
#'
#' @description Creates a .txt file in the working directory that has the same structure as chatlogs exported from WhatsApp. Messages have a timestamp, sender name and message body
#' containing lorem ipsum, emoji, links, smilies, location, omitted media files, linebreaks, self-deleting photos, and WhatsApp system messages. Timestamps are formatted according to specified phone operating system and
#' time format settings. WhatsApp system messages are formatted according to specified phone operating system and language.
#' @param n_messages Number of messages that are contained in the created .txt file.
#' @param n_chatters Number of different chatters present in the created .txt file.
#' @param n_emoji Number of messages that contain emoji. Must be smaller or equal to n_messages.
#' @param n_diff_emoji Number of different emoji that are used in the simulated chat.
#' @param n_links Number of messages that contain links. Must be smaller or equal to n_messages.
#' @param n_locations Number of messages that contain locations. Must be smaller or equal to n_messages.
#' @param n_smilies Number of messages that contain smilies. Must be smaller or equal to n_messages.
#' @param n_diff_smilies Number of different smilies that are used in the simulated chat.
#' @param n_media Number of messages that contain omitted media files. Must be smaller or equal to n_messages.
#' @param n_sdp Number of messages that contain self-deleting photos. Must be smaller or equal to n_messages.
#' @param startdate Earliest possible date for messages. Timestamps for messages are created automatically between startdate and enddate.
#' @param enddate Latest possible date for messages. Timestamps for messages are created automatically between startdate and enddate.
#' @param language Parameter for the language setting of the exporting phone. Influences structure of system messages.
#' @param time_format Parameter for the time format setting of the exporting phone (am/pm vs. 24h). Influences the structure of timestamps.
#' @param os Parameter for the operating system setting of the exporting phone. Influences the structure of timestamps and WhatsApp system messages.
#' @param path Character string for indicating the file path of where to save the file. Can be NA to not save a file. Default is getwd()
#' @param chatname Name for the created .txt file.
#' @export
#' @importFrom checkmate assert_numeric
#' @importFrom stats rlnorm rnorm
#' @return A .txt file with a simulated WhatsApp chat containing lorem ipsum but all structural properties of actual chats.
#'
#' @examples
#' SimulatedChat <- create_chatlog(path=NA)
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
                           n_sdp = 3,
                           startdate = "01.01.2019",
                           enddate = "31.12.2022",
                           language = "german",
                           time_format = "24h",
                           os = "android",
                           path = getwd(),
                           chatname = "Simulated_WhatsR_chatlog") {

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

  # ensuring that n_variables are not larger than the number of messages
  if (n_messages < n_emoji | n_messages < n_links | n_messages < n_locations | n_messages < n_smilies | n_messages < n_media | n_messages < n_sdp) {
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
  startdate_check <- try(as.Date(startdate, format = "%d.%m.%Y"))
  if ("try-error" %in% class(startdate_check) || is.na(startdate_check)) {
    print("Variable 'startdate' musst be a character string of format dd.mm.YYYY")
  }

  # validating enddate
  enddate_check <- try(as.Date(enddate, format = "%d.%m.%Y"))
  if ("try-error" %in% class(enddate_check) || is.na(enddate_check)) {
    print("Variable 'enddate' musst be a character string of format dd.mm.YYYY")
  }

  # validate that startdate is before enddate
  sDate <- as.POSIXct(as.Date(startdate, format = "%d.%m.%Y"))
  eDate <- as.POSIXct(as.Date(enddate, format = "%d.%m.%Y"))

  if (sDate >= eDate) {
    warning("starting date must be earlier than ending date.")
    stop()
  }

  # checking path variable
  if (!(is.na(path) | is.character(path))) {
    warning("'path' must be NA or a path to a valid location on your system.")
  }

  #### Importing data ####

  # Importing smilie dictionary
  smilies <- read.csv(system.file("SmileyDictionary.csv", package = "WhatsR"),
    stringsAsFactors = F
  )[, 2]

  # Limiting Smiley dictionary of number of different smilies to sample from
  smilies <- smilies[sample(1:length(smilies), n_diff_smilies)]

  # Importing emoji dictionary
  EmojiDictionary <- read.csv(system.file("EmojiDictionary.csv", package = "WhatsR"),
    header = TRUE,
    stringsAsFactors = FALSE,
    strip.white = FALSE,
    colClasses = "character",
    blank.lines.skip = TRUE
  )

  # Limiting emoji dictionary of number of different emoji to sample from
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
    sDate <- as.POSIXct(as.Date(sDate, format = "%d.%m.%Y"))
    eDate <- as.POSIXct(as.Date(eDate, format = "%d.%m.%Y"))
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
        ts <- strftime(ts, format = "%d.%m.%y, %H:%M - ")
      } else {
        ts <- strftime(ts, format = "%d.%m.%y, %I:%M %p - ")
      }
    } else {
      if (time_format == "24h") {
        ts <- strftime(ts, format = "[%d.%m.%y, %H:%M:%S] ")
      } else {
        ts <- strftime(ts, format = "[%m/%d/%y, %I:%M:%S %p] ")
      }
    }
  } else {
    if (os == "android") {
      if (time_format == "24h") {
        ts <- strftime(ts, format = "%m/%d/%y, %H:%M - ")
      } else {
        ts <- strftime(ts, format = "%m/%d/%y, %I:%M %p - ")
      }
    } else {
      if (time_format == "24h") {
        ts <- strftime(ts, format = "[%m/%d/%y, %H:%M:%S] ")
      } else {
        ts <- strftime(ts, format = "[%m/%d/%y, %I:%M:%S %p] ")
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

  # Creating mock WhatsApp Messages
  for (i in seq_along(ts)) {

    # sample number of sentences
    # TODO for later: This could be done more elegantly
    sent_num <- sample(c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5), 1)

    # initilaizing empty vector
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

  #### Adding Links

  # sampling messages to add links to
  link_rows <- sample(1:n_messages, n_links)

  # adding links at end of message text
  for (i in link_rows) {
    Messages[i] <- paste(Messages[i], paste(sample(Links$x, sample(c(1:5), 1), replace = TRUE), collapse = " "), sep = " ")
  }


  #### Adding Emoji

  # sample messages to add emoji to
  emoji_rows <- sample(1:n_messages, n_emoji, replace = TRUE)

  # adding emoji to end of message
  for (i in emoji_rows) {
    Messages[i] <- paste(Messages[i], paste(sample(EmojiDictionary$R.native, sample(c(1:5), 1), replace = TRUE), collapse = " "), sep = " ")
  }


  #### Adding smilies

  # sample messages to add smilies to
  smilie_rows <- sample(1:n_messages, n_smilies, replace = TRUE)

  # Adding smilies at end of message
  for (i in smilie_rows) {
    Messages[i] <- paste(Messages[i], paste(sample(smilies, sample(c(1:5), 1), replace = TRUE), collapse = " "), sep = " ")
  }

  #### Adding Locations

  # creating latitude and longtide for location links
  locations <- paste(round(as.numeric(runif(n_locations, -0.005, 1.0049) + sample(-89:89, n_locations, replace = TRUE)), 8),
    ",",
    round(as.numeric(runif(n_locations, -0.005, 1.0049) + sample(-89:89, n_locations, replace = TRUE)), 8),
    sep = ""
  )

  # creating location links
  locations <- paste("https://maps.google.com/?q=", locations, sep = "")

  # sampling messages to add locations to
  location_rows <- sample(1:n_messages, n_locations, replace = TRUE)

  # adding locations to messages
  for (i in location_rows) {
    Messages[i] <- paste(Messages[i], paste(sample(locations, sample(c(1:5), 1), replace = TRUE), collapse = " "), sep = " ")
  }

  #### Add self-deleting photos

  # sample messages to replace with indicator for self-deleting photo
  sdp_rows <- sample(1:n_messages, n_sdp, replace = TRUE)

  # replace message with empty string as indicator for a self-deleting photo
  for (i in sdp_rows) {
    Messages[i] <- ""
  }

  #### Add System messages

  # sample messages to replace with WhatsApp system messages
  Messages[1] <- substr(WAStrings[1], 2, nchar(WAStrings[1]) - 1)
  sm_rows <- sample(2:n_messages, 20)

  # transpose WAStrings for easier handling
  WAStrings <- t(WAStrings)

  # Create system messages from Regexes built to detect them and insert them
  # according to specified language and operating system
  if (language == "german") {
    if (os == "android") {
      # replacing regex strings
      WAStrings <- gsub("$", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("^", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("(.)*?", "Bob", WAStrings, fixed = TRUE)
      WAStrings <- gsub("\\", "", WAStrings, fixed = TRUE)

      # deleting/replacing unnecessary parts
      WAStrings[6] <- paste0("Standort: ", locations[1])
      WAString <- WAStrings[-c(4), ]
      WAStrings[21] <- "+ 49 000 000 hat zu 004900000000 gewechselt."
      WAStrings[3] <- "2 Kontakte.vcf (Datei angeh\u0061\u0308ngt)"

      # replace messages with system messages
      Messages[sm_rows] <- WAStrings[c(3, 5:23)]
    } else {
      # replacing regex strings
      WAStrings <- gsub("$", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("^", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("(.)*?", "Bob", WAStrings, fixed = TRUE)
      WAStrings <- gsub("\\.", ".", WAStrings, fixed = TRUE)

      # deleting/replacing unnecessary parts
      WAStrings[6] <- paste0("Standort: ", locations[1])
      WAStrings[3] <- "<angeh\u0061\u0308ngt: 3 Filename.vcf>"
      WAStrings[21] <- "+ 49 000 000 hat zu 004900000000 gewechselt."
      WAStrings <- WAStrings[-c(4)]

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
      WAStrings[6] <- paste0("Location: ", locations[1])
      WAString <- WAStrings[-c(4), ]
      WAStrings[3] <- "3 Filename.vcf (file attached)"
      WAStrings[21] <- "+ 49 000 000 changed to 004900000000."

      # replace messages with system messages
      Messages[sm_rows] <- WAStrings[c(3, 5:23)]
    } else {
      # replacing regex strings
      WAStrings <- gsub("$", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("^", "", WAStrings, fixed = TRUE)
      WAStrings <- gsub("(.)*?", "Bob", WAStrings, fixed = TRUE)
      WAStrings <- gsub("\\.", ".", WAStrings, fixed = TRUE)

      # deleting/replacing unnecessary parts
      WAStrings[6] <- paste0("location: ", locations[1])
      WAStrings[3] <- "<attached: 3 Filename.vcf>"
      WAStrings[21] <- "+ 49 000 000 changed to 004900000000."
      WAStrings <- WAStrings[-c(4)]

      # replace messages with system messages
      Messages[sm_rows] <- WAStrings[c(3, 5:23)]
    }
  }


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
  for (i in sample(c(1:n_messages)[-c(1, sm_rows)], round(0.05 * n_messages, 0))) {
    Messages[i] <- fun_insert(Messages[i], pos = sample(c(1:20), 1), " \n \n ")
  }


  #### Pasting timestamps, names and messages together, based on OS structure

  if (os == "ios") {
    # system messages with names
    Messages[sm_rows][c(1:4, 14:17, 19)] <- paste0(ts[sm_rows][c(1:4, 14:17, 19)], Names[sm_rows][c(1:4, 14:17, 19)], Messages[sm_rows][c(1:4, 14:17, 19)])

    # system messages without names
    Messages[sm_rows][c(5:13, 20)] <- paste0(ts[sm_rows][c(5:13, 20)], Messages[sm_rows][c(5:13, 20)])

    # other messages (with names)
    Messages[-c(1, sm_rows)] <- paste0(ts[-c(1, sm_rows)], Names[c(-c(1, sm_rows))], Messages[c(-c(1, sm_rows))])

    # first message
    Messages[1] <- paste0(ts[1], Messages[1])
  } else {
    # system messages with names
    Messages[sm_rows][c(1:4, 15:17, 19)] <- paste0(ts[sm_rows][c(1:4, 15:17, 19)], Names[sm_rows][c(1:4, 15:17, 19)], Messages[sm_rows][c(1:4, 15:17, 19)])

    # system messages without names
    Messages[sm_rows][c(5:14, 20)] <- paste0(ts[sm_rows][c(5:14, 20)], Messages[sm_rows][c(5:14, 20)])

    # other messages (with names)
    Messages[-c(1, sm_rows)] <- paste0(ts[-c(1, sm_rows)], Names[c(-c(1, sm_rows))], Messages[c(-c(1, sm_rows))])

    # first message
    Messages[1] <- paste0(ts[1], Messages[1])
  }


  # write simulated chatlog to file
  if (!is.na(path)) {
    writeLines(Messages, paste0(path,"/",chatname, ".txt"))
  }

  ##### Returning Results #####
  return(Messages)
}
