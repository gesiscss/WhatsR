#' @title Parsing exported 'WhatsApp' chat logs as a dataframe
#'
#' @description Creates a data frame from an exported 'WhatsApp' chat log containing one row per message. Some columns
#' are saved as lists using the I() function so that multiple elements can be stored per message while still maintaining
#' the general structure of one row per message. These columns should be treated as lists or unlisted first.
#' @param path Character string containing the file path to the exported 'WhatsApp' chat log as a .txt file or .zip folder.
#' @param os Operating system of the phone the chat was exported from. Default "auto" tries to automatically detect the OS. Also supports "android" or "ios".
#' @param language Indicates the language setting of the phone with which the messages were exported. Default is "auto" trying to match either 'English' or 'German'. More languages might be supported in the future.
#' @param anonymize TRUE results in the vector of sender names being anonymized and columns containing personal identifiable information to be deleted or restricted, FALSE displays the actual names and all content, "add" adds
#' anonomized columns to the full info columns. Do not blindly trust this and always double check.
#' @param consent String containing a consent message. All messages from chatters who have not posted this *exact* message into the chat will be deleted. Default is NA, no deleting anything.
#' @param emoji_dictionary Dictionary for emoji matching. Can use a version included in this package when set to "internal" or
#' an updated data frame created by \code{\link[WhatsR]{download_emoji}} passed as a character string containing the path to the file.
#' @param smilie_dictionary Value "emoticons" uses \code{\link[qdapRegex]{ex_emoticon}} to extract smilies, "wikipedia" uses a more inclusive custom list
#' of smilies containing all mentions from https://de.wiktionary.org/w/index.php?title=Verzeichnis:International/Smileys
#' and manually added ones.
#' @param rpnl Replace newline. A character string for replacing line breaks within messages for the parsed message for better readability. Default is " start_newline ".
#' @param verbose Prints progress messages for parse_chat() to the console if TRUE, default is FALSE.
#' @importFrom readr parse_character
#' @importFrom qdapRegex rm_url rm_between ex_emoticon rm_non_words
#' @importFrom stats na.omit
#' @importFrom tokenizers tokenize_words
#' @importFrom stringi stri_extract_all_regex  stri_replace_all stri_extract_all stri_split_boundaries
#' @importFrom mgsub mgsub
#' @importFrom utils tail read.csv
#' @return A dataframe containing one row per message and 11,15, or 19 columns, depending on the setting of the anonymize parameter
#'
#' @examples
#' data <- parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"))
#' @export

parse_chat <- function(path,
                       os = "auto",
                       language = "auto",
                       anonymize = "add",
                       consent = NA,
                       emoji_dictionary = "internal",
                       smilie_dictionary = "wikipedia",
                       rpnl = " start_newline ",
                       verbose = FALSE
                       ) {

  # Input checking
  if (!file.exists(path)) {stop("'path' must be a valid file path to an exported 'WhatsApp' chatlog in .txt format")}
  if (!(os == "auto" | os == "android" | os == "ios")) {stop("'os' must either be 'android','ios', or 'auto'")}
  if (!(language == "auto" | language == "english" | language == "german")) {stop("'language' must be either 'english', 'german', or 'auto'")}
  if (!(is.logical(anonymize) | anonymize == "add")) {stop("'anonymize' must be either TRUE, FALSE, or 'add'")}
  if (!(is.character(consent) | is.na(consent))) {stop("'consent' must be either NA or a character vector")}
  if (!(emoji_dictionary == "internal" | file.exists(emoji_dictionary))) {stop("'emoji_dictionary' must be 'internal' or valid path to a dictionary scraped using download_emoji()")}# TODO
  if (!(smilie_dictionary == "emoticons" | smilie_dictionary == "wikipedia")) {stop("'smilie_dictionary' must be 'emoticons' or 'wikipedia'")}
  if (!is.character(rpnl)) {stop("'rpnl' must be a character string")}
  if (!is.logical(verbose)) {stop("'verbose' must be either TRUE or FALSE")}

    # accept .txt or .zip (containing one or more .txt)
    if (!file.exists(path)) stop("'path' must be a valid path to a .txt or .zip file with a WhatsApp chat export")
    if (grepl("\\.zip$", path, ignore.case = TRUE)) {
      z <- utils::unzip(path, list = TRUE)
      txt <- z$Name[grepl("\\.txt$", z$Name, ignore.case = TRUE)]
      if (!length(txt)) stop("No .txt found inside the .zip export.")
      tmpdir <- file.path(tempdir(), "whatsr_zip")
      utils::unzip(path, files = txt, exdir = tmpdir, overwrite = TRUE)
      # If multiple txts exist, choose the largest by size (usually the chat)
      files <- file.path(tmpdir, txt)
      sizes <- file.info(files)$size
      path <- files[which.max(sizes)]
      if (verbose) cat(sprintf("Detected chat log file: %s\n", basename(path)))
    }

  # Importing raw chat file
  RawChat <- readChar(path, file.info(path)$size)

  # printing info
  if (verbose) {cat("Imported raw chat file \U2713 \n")}

  # Regex that detects 24h/ampm, american date format, european date format and all combinations for ios and android
  # This is the new version, also accounting for German AM/PM translations [See github history for older verisons]
  TimeRegex_android <- "(?!^)(?=((\\d{2}\\.\\d{2}\\.\\d{2})|(\\d{1,2}/\\d{1,2}/\\d{2,4})),\\s\\d{1,2}:\\d{2}(?:\\s*\\p{Zs}*\\s*(?i:(AM|PM|morgens|vorm\\.|mittags|nachm\\.|abends|nachts)))?\\s-)"
  TimeRegex_ios <- "(?!^)(?=\\[((\\d{2}\\.\\d{2}\\.\\d{2})|(\\d{1,2}\\/\\d{1,2}\\/\\d{2,4})),\\s\\d{1,2}:\\d{2}((\\:\\d{2}\\s(?i:(pm|am|morgens|vorm\\.|mittags|nachm\\.|abends|nachts)))|(\\s(?i:(pm|am|morgens|vorm\\.|mittags|nachm\\.|abends|nachts)))|(\\:\\d{2}\\])|(\\:\\d{2})|(\\s))\\])"



  ### reducing RawChat to workable size for language and os detection (if necessary) ####
  if (nchar(RawChat) > 10000) {

    excerpt <- substr(RawChat, 1, 10000)
  } else {
    excerpt <- RawChat
  }


  # trying to automatically detect operating system [this takes quite long for larger chats]
  if (os == "auto") {

    # getting number of os-specific timestamps from chat
    android_stamps <- length(unlist(stri_extract_all(excerpt, regex = TimeRegex_android)))
    ios_stamps <- length(unlist(stri_extract_all(excerpt, regex = TimeRegex_ios)))

    # selecting operating system
    if (android_stamps > ios_stamps) {

      os <- "android"
      if (verbose) {cat("Operating System was automatically detected: android \U2713 \n")}
      TimeRegex <- TimeRegex_android
    } else if (android_stamps == ios_stamps) {

      cat("Operating System could not be detected automatically, please enter either 'ios' or 'android' without quotation marks and press enter")
      os <- readline(prompt = "Enter operating system: ")

      if (os == "android") {

        if (verbose) {cat("Operating System was set to: android \U2713 \n")}
        TimeRegex <- TimeRegex_android
      } else if (os == "ios") {

        cat("Operating System was set to: ios \U2713 \n")
        TimeRegex <- TimeRegex_ios
      } else if (os != "android" & os != "ios") {


        warning("Parameter os must be either 'android', 'ios' or 'auto'")
        return(NULL)
      }
    } else if (android_stamps < ios_stamps) {

      os <- "ios"
      if (verbose) {cat("Operating System was automatically detected: ios \U2713 \n")}
      TimeRegex <- TimeRegex_ios
    }
  } else if (os == "ios") {

    TimeRegex <- TimeRegex_ios
  } else if (os == "android") {

    TimeRegex <- TimeRegex_android
  }


  # loading language indicators
  WAStrings <- read.csv(system.file("Languages.csv", package = "WhatsR"),
    stringsAsFactors = F,
    fileEncoding = "UTF-8"
  )

  # trying to auto-detect language
  if (language == "auto") {

    # checking presence of indicator strings
    german_a <- sum(!is.na(unlist(stri_extract_all(excerpt, regex = gsub("$", "", gsub("^", "", WAStrings[1,c(3:18,21:28,33:51)], fixed = TRUE), fixed = TRUE)[c(3:18,21:28,33:51)]))))
    german_i <- sum(!is.na(unlist(stri_extract_all(excerpt, regex = gsub("$", "", gsub("^", "", WAStrings[2, ], fixed = TRUE), fixed = TRUE)[3:51]))))
    english_a <- sum(!is.na(unlist(stri_extract_all(excerpt, regex = gsub("$", "", gsub("^", "", WAStrings[3,c(3:18,21:28,33:51)], fixed = TRUE), fixed = TRUE)[c(3:18,21:28,33:51)]))))
    english_i <- sum(!is.na(unlist(stri_extract_all(excerpt, regex = gsub("$", "", gsub("^", "", WAStrings[4, ], fixed = TRUE), fixed = TRUE)[3:51]))))

    # Best guess about language based on presence of indicator strings
    guess <- WAStrings[which(c(german_a, german_i, english_a, english_i) == max(c(german_a, german_i, english_a, english_i))), 1]

    # setting auto-detected language
    language <- unlist(stri_extract_all(guess, fixed = c("german", "english")))
    language <- language[!is.na(language)]

    # printing info
    if (verbose) {cat(paste0("Auto-detected language setting of exporting phone: ", language, " \U2713 \n"))}
  } else if (language != "english" & language != "german") {

    cat("Language was set incorrectly or could not automatically be detected. Please set language to either 'german' or 'english' without the quotation marks below")
    language <- readline(prompt = "Enter the phone's language setting from which the chat was exported: ")
  }

  # selecting indicators based on language
  Indicators <- WAStrings[WAStrings$Settings == paste0(language, os), ]

  # assigning indicator strings for message bodies
  ExtractAttached <- Indicators$ExtractAttached
  DeleteAttached <- Indicators$DeleteAttached
  OmittanceIndicator <- Indicators$OmittanceIndicator
  SentLocation <- Indicators$SentLocation
  LiveLocation <- Indicators$LiveLocation
  MissedCallVoice <- Indicators$MissedCallVoice
  MissedCallVideo <- Indicators$MissedCallVideo

  # assigning indicator strings without sender info
  StartMessage <- Indicators$StartMessage
  StartMessageGroup <- Indicators$StartMessageGroup
  GroupCreateSelf <- Indicators$GroupCreateSelf # Can contain PII
  GroupCreateOther <- Indicators$GroupCreateOther # Can contain PII
  GroupRenameSelf <- Indicators$GroupRenameSelf # Can contain PII
  GroupPicChange <- Indicators$GroupPicChange
  GroupRenameOther <- Indicators$GroupRenameOther # Can contain PII
  UserRemoveSelf <- Indicators$UserRemoveSelf # Can contain PII
  UserAddSelf <- Indicators$UserAddSelf # Can contain PII
  UserRemoveOther <- Indicators$UserRemoveOther # Can contain PII
  UserAddOther <- Indicators$UserAddOther # Can contain PII
  GroupPicChangeOther <- Indicators$GroupPicChangeOther # Can contain PII
  UserNumberChangeKnown <- Indicators$UserNumberChangeKnown # Can contain PII
  UserNumberChangeUnknown <- Indicators$UserNumberChangeUnknown # Can contain PII
  DeletedMessage <- Indicators$DeletedMessage
  UserLeft <- Indicators$UserLeft # Can contain PII
  SafetyNumberChange <- Indicators$SafetyNumberChange # Can contain PII
  GroupCallStarted <- Indicators$GroupCallStarted # Can contain PII
  GroupVideoCallStarted <- Indicators$GroupVideoCallStarted # Can contain PII
  VoiceCallTaken <- Indicators$VoiceCallTaken
  VideoCallTaken <- Indicators$VideoCallTaken
  VoiceCallNoResponse <- Indicators$VoiceCallNoResponse
  VideoCallNoResponse <- Indicators$VideoCallNoResponse
  NewContactCreation <- Indicators$NewContactCreation
  FoursquareLoc <- Indicators$FoursquareLoc
  SelfDelMessageDuration <- Indicators$SelfDelMessageDuration
  VideoNote <- Indicators$VideoNote
  ContactBlocked <- Indicators$ContactBlocked
  ContactUnblocked <- Indicators$ContactUnblocked
  MetaAIChatDisclaimer <- Indicators$MetaAIChatDisclaimer
  MetaAIInfo <- Indicators$MetaAIInfo
  SelfDelMsgActivateSelf <- Indicators$SelfDelMsgActivateSelf
  SelfDelMsgDeactivateSelf <- Indicators$SelfDelMsgDeactivateSelf
  GroupPicDelete <- Indicators$GroupPicDelete
  AdminJoinApprovalActivate <- Indicators$AdminJoinApprovalActivate
  GroupDescDelete <- Indicators$GroupDescDelete
  GroupMultiadd <- Indicators$GroupMultiadd
  ExtendedChatProtection <- Indicators$ExtendedChatProtection
  ExtendedChatProtectionOff <- Indicators$ExtendedChatProtectionOff
  GroupNameChangeOnCreate <- Indicators$GroupNameChangeOnCreate
  AdminNow <- Indicators$AdminNow
  NoAdminNow <- Indicators$NoAdminNow

  # print info
  if (verbose) {cat(paste("Imported matching strings for: ", paste(language, os, sep = " "), " \U2713 \n", sep = ""))}

  # Replacing special characters
  ReplacedSpecialCharactersChat <- parse_character(RawChat)

  # Deleting left-to-right markers if present
  ReplacedSpecialCharactersChat <- gsub("\u200e", "", ReplacedSpecialCharactersChat)

  # Deleting zero-width no break spaces if present
  ReplacedSpecialCharactersChat <- gsub("\uFEFF", "", ReplacedSpecialCharactersChat)

  # printing info
  if (verbose) {cat("Replaced special characters \U2713 \n")}

  if (os == "android") {

    # Parsing the message according to android text structure
    ParsedChat <- parse_android(ReplacedSpecialCharactersChat,
      newline_indicator = "\n",
      newline_replace = rpnl,
      media_omitted = OmittanceIndicator,
      media_indicator = ExtractAttached,
      sent_location = SentLocation,
      live_location = LiveLocation,
      datetime_indicator = TimeRegex,
      media_replace = OmittanceIndicator,
      foursquare_loc = FoursquareLoc
    )

    # printing info
    if (verbose) {cat("Parsed chat according to Android document structure \U2713 \n")}
  } else if (os == "ios") {

    # Parsing the message according to android text structure
    ParsedChat <- parse_ios(ReplacedSpecialCharactersChat,
      newline_indicator = "\n",
      newline_replace = "",
      media_omitted = OmittanceIndicator,
      media_indicator = DeleteAttached,
      sent_location = SentLocation,
      live_location = LiveLocation,
      datetime_indicator = TimeRegex,
      media_replace = OmittanceIndicator,
      foursquare_loc = FoursquareLoc
    )

    # printing info
    if (verbose) {cat("Parsed chat according to iOS document structure \U2713 \n")}
  }

  # Setting WhatsApp system messages indicator RegExes
  # INFO: If any if these weirdly doesn't match a seemingly identical string,
  # check for non-breaking spaces and invisible characters using charToRaw() on the
  # original string!
  WAStrings <- c(
    StartMessage,
    StartMessageGroup,
    GroupCreateSelf,
    GroupCreateOther,
    GroupRenameSelf,
    GroupPicChange,
    GroupRenameOther,
    UserRemoveSelf,
    UserAddSelf,
    UserLeft,
    UserRemoveOther,
    UserAddOther,
    GroupPicChangeOther,
    UserNumberChangeKnown,
    UserNumberChangeUnknown,
    DeletedMessage,
    SafetyNumberChange,
    GroupCallStarted,
    GroupVideoCallStarted,
    VoiceCallTaken,
    VideoCallTaken,
    VoiceCallNoResponse,
    VideoCallNoResponse,
    NewContactCreation,
    SelfDelMessageDuration,
    ContactBlocked,
    ContactUnblocked,
    MetaAIChatDisclaimer,
    MetaAIInfo,
    SelfDelMsgActivateSelf,
    GroupPicDelete,
    AdminJoinApprovalActivate,
    GroupDescDelete,
    GroupMultiadd,
    SelfDelMsgDeactivateSelf,
    ExtendedChatProtection,
    ExtendedChatProtectionOff,
    GroupNameChangeOnCreate,
    AdminNow,
    NoAdminNow
  )

  # TODO: This is where the last testing chat fails!

  # checking whether a WhatsApp message was parsed into the sender column
  WAMessagePresent <- unlist(stri_extract_all_regex(str = ParsedChat$Sender, pattern = paste(WAStrings, collapse = "|")))
  ParsedChat$SystemMessage[!is.na(WAMessagePresent)] <- WAMessagePresent[!is.na(WAMessagePresent)]
  ParsedChat$Sender[!is.na(WAMessagePresent)] <- "WhatsApp System Message"

  # checking Whatsapp System Messages that are erroneously attributed to a chat participant:
  WAMessagePresent <- unlist(stri_extract_all_regex(str = ParsedChat$Message, pattern = paste(WAStrings, collapse = "|")))
  ParsedChat$SystemMessage[!is.na(WAMessagePresent)] <- WAMessagePresent[!is.na(WAMessagePresent)]
  ParsedChat$Sender[!is.na(WAMessagePresent)] <- "WhatsApp System Message"
  ParsedChat$Message[!is.na(WAMessagePresent)] <- NA

  # number change detection
  NumberChangePresent <- unlist(stri_extract_all_regex(str = ParsedChat$Message, pattern = UserNumberChangeUnknown))
  ParsedChat$SystemMessage[is.na(ParsedChat$SystemMessage)] <- NumberChangePresent[is.na(ParsedChat$SystemMessage)]
  ParsedChat$Sender[!is.na(NumberChangePresent)] <- "WhatsApp System Message"
  ParsedChat$Message[!is.na(NumberChangePresent)] <- NA


  # printing info
  if (verbose) {cat("Differentiated System Messages from User generated content \U2713 \n")}

  # fixing parsing of messages with self-deleting photos:
  # selecting rows with no content where the senders contain a ":"
  ParsedChat[grepl(":", ParsedChat$Sender) &
    is.na(ParsedChat$Message) &
    is.na(ParsedChat$SystemMessage) &
    is.na(ParsedChat$Media) &
    is.na(ParsedChat$Location), ]$Sender <- gsub(
    ":",
    "",
    ParsedChat[grepl(":", ParsedChat$Sender) &
      is.na(ParsedChat$Message) &
      is.na(ParsedChat$SystemMessage) &
      is.na(ParsedChat$Media) &
      is.na(ParsedChat$Location), ]$Sender
  )

  # Removing messages of participants who did not post the consent message into the chat
  if (!is.na(consent)) {

    # getting vector with names of consenting chat participants
    consentintg_ppts <- c(na.omit(ParsedChat$Sender[ParsedChat$Message == consent]), "WhatsApp System Message")

    if (!(is.character(consentintg_ppts) & length(consentintg_ppts) >= 1)) {stop("No participants contained in the chat that posted the consent message.")}

    # removing all messages from non-consenting participants
    ParsedChat <- ParsedChat[is.element(ParsedChat$Sender, consentintg_ppts), ]

  }

  ### We create handy vectors for used emoji, extracted links, extracted media data
  # and one containing the message without stopwords, emoji, linebreaks, URLs and punctuation

  # extracting links
  URL <- (rm_url(ParsedChat$Message, extract = TRUE))

  # printing info
  if (verbose) {cat("Extracted Links from text \U2713 \n")}

  #### Extracting emoji

  # importing emoji dictionary
  if (emoji_dictionary == "internal") {

    # using internal emoji dictionary that comes with the package
    EmojiDictionary <- read.csv(system.file("EmojiDictionary.csv", package = "WhatsR"),
                                header = TRUE,
                                stringsAsFactors = FALSE,
                                strip.white = FALSE,
                                colClasses = "character",
                                blank.lines.skip = TRUE
                                )

  } else {

    # using custom emoji dictionary
    EmojiDictionary <- read.csv(emoji_dictionary,
                                header = TRUE,
                                stringsAsFactors = FALSE,
                                strip.white = FALSE,
                                colClasses = "character",
                                blank.lines.skip = TRUE
    )

    # check if custom emoji dictionary is usable and error out if it is not
    if (!("R.native" %in% colnames(EmojiDictionary) &
          "Desc" %in% colnames(EmojiDictionary))) {

      warning("'emoji_dictionary' must either be 'internal' or a valid file path
      to a .csv file created by download_emoji(). Columns 'R.native' and 'Desc'
      must be present in the csv.")
      stop()

    }


  }

  # isolating emoji to get a better and faster matching than methods using stringr, stringi, rm_default or mgsub
  # (sped up using idea from: https://github.com/JBGruber/rwhatsapp/blob/master/R/emoji_lookup.R)
  MessageNumber <- 1:length(ParsedChat$Message)
  CharSplit <- stri_split_boundaries(ParsedChat$Message, type = "character")

  # creating split data frame
  SplitFrame <- data.frame(
    MessageNumber = rep(MessageNumber, sapply(CharSplit, length)),
    Emoji = unlist(CharSplit)
  )

  # doing the matching
  R.native <- EmojiDictionary$Desc[match(SplitFrame$Emoji, EmojiDictionary$R.native)]
  SplitFrame <- cbind.data.frame(SplitFrame, R.native)

  # deleting empties
  SplitFrame <- SplitFrame[!is.na(SplitFrame$R.native), ]

  # creating list of vectors for emoji descriptions and glyphs
  EmojiSplitNames <- split(SplitFrame$R.native, SplitFrame$MessageNumber)
  EmojiSplitGlyphs <- split(SplitFrame$Emoji, SplitFrame$MessageNumber)

  # Rows in DF that contain emoji
  EmojiRows <- as.numeric(names(EmojiSplitNames))

  # Adding to dataframe
  Emoji <- rep(NA, dim(ParsedChat)[1])
  EmojiDescriptions <- rep(NA, dim(ParsedChat)[1])
  Emoji[EmojiRows] <- I(EmojiSplitGlyphs)
  EmojiDescriptions[EmojiRows] <- I(EmojiSplitNames)

  # printing info
  if (verbose) {cat("Extracted emoji from text \U2713 \n")}

  ### Creating a 'flattened' message for text mining
 # removing emoji, newlines & media indicators

  # removing newline and omittance indicators
  Flat <- rm_between(ParsedChat$Message, " start_newlin", "e ", replacement = "")
  Flat <- stri_replace_all(Flat, regex = OmittanceIndicator, replacement = "")

  # printing info
  if (verbose) {cat("Removed emoji, newlines and media file indicators from flat text column \U2713 \n")}

  # deleting the file attachments from flattened message
  # FIXME: https://github.com/gesiscss/WhatsR/issues/21
  if (os == "android") {
    Flat <- gsub(paste0("(.)*?", substring(DeleteAttached, 4, nchar(DeleteAttached) - 1), "($|\\s)"), "", Flat, perl = TRUE)
  } else if (os == "ios") {
    Flat <- gsub(x = Flat, pattern = ExtractAttached, replacement = "", perl = T)
  }

  # printing info
  if (verbose) {cat("Deleted filenames from flat text column \U2713 \n")}

  ### Smilies

  # basic version with prebuild dictionary
  if (smilie_dictionary == "emoticons") {

    Smilies <- ex_emoticon(Flat)

    # printing info
    if (verbose) {cat("Extracted Smilies using prebuild dictionary \U2713 \n")}

  # using a more inclusive custom dictionary
  } else if (smilie_dictionary == "wikipedia") {

    # package version
    smilies <- read.csv(system.file("SmileyDictionary.csv", package = "WhatsR"),
      stringsAsFactors = F

    )

    # deleting whitespace from smilies column
    smilies <- smilies[, 2]
    smilies <- trimws(smilies)

    # Splitting smilies column
    Smilies <- sapply(strsplit(Flat, " "), function(x) x[x %in% smilies])
    Smilies[lapply(Smilies, length) == 0] <- NA

    # printing info
    if (verbose) {cat("Extracted smilies using custom build dictionary \U2713 \n")}
  }

  # replacing sent locations in flattened message
  Flat <- gsub(
    x = Flat,
    pattern = SentLocation,
    replacement = NA,
    perl = T
  )

  Flat <- gsub(
    x = Flat,
    pattern = FoursquareLoc,
    replacement = NA,
    perl = T
  )

  # printing info
  if (verbose) {cat("Deleted sent location indicators from flat text column \U2713 \n")}

  # replacing live location in flattened message
  Flat <- gsub(
    x = Flat,
    pattern = LiveLocation,
    replacement = NA,
    perl = T
  )

  # deleting live locations with captions (without deleting the caption)
  # FIXME: This is only for german -> fix
  #Flat <- gsub(
  #  x = Flat,
  #  pattern = "^Live-Standort wird geteilt\\.",
  #  replacement = "",
  # perl = T
  #)

  # printing info
  if (verbose) {cat("Deleted live location indicators from flat text column \U2713 \n")}

  ## Voice Calls
  # TODO: For iOS, the indicators include invisible lrm and rlm characters (\u200E and \u200F)
  # If something breaks with the removal of indicators, these are a likely culprit

  # replacing missed voice calls in flattened message
  Flat <- gsub(
    x = Flat,
    pattern = MissedCallVoice,
    replacement = NA,
    perl = T
  )

  # replacing taken voice calls in flattened message
  Flat <- gsub(
    x = Flat,
    pattern = VoiceCallTaken,
    replacement = NA,
    perl = T
  )

  # replacing unanswered voice calls in flattened message
  Flat <- gsub(
    x = Flat,
    pattern = VoiceCallNoResponse,
    replacement = NA,
    perl = T
  )

  # replacing missed video calls in flattened message
  Flat <- gsub(
    x = Flat,
    pattern = MissedCallVideo,
    replacement = NA,
    perl = T
  )

  # replacing taken video calls in flattened message
  Flat <- gsub(
    x = Flat,
    pattern = VideoCallTaken,
    replacement = NA,
    perl = T
  )

  # replacing unanswered video calls in flattened message
  Flat <- gsub(
    x = Flat,
    pattern = VideoCallNoResponse,
    replacement = NA,
    perl = T
  )
  # replacing video notes in flattened message
  Flat <- gsub(
    x = Flat,
    pattern = VideoNote,
    replacement = NA,
    perl = T
  )

  ## Removing deleted messages
  Flat <- gsub(
    x = Flat,
    pattern = DeletedMessage,
    replacement = NA,
    perl = T
  )



  # printing info
  if (verbose) {cat("Deleted voice call indicators from flat text column \U2713 \n")}

  # deleting URLs from flattened messages
  Flat <- rm_url(Flat)
  Flat[Flat == "" | Flat == "NULL"] <- NA

  # printing info
  if (verbose) {cat("Deleted URLs from flat text column \U2713 \n")}

  # Deleting all non words
  Flat <- rm_non_words(Flat)

  # printing info
  if (verbose) {cat("Deleted all non-words from flat text column \U2713 \n")}

  # making all empty strings NA
  Flat[nchar(Flat) == 0] <- NA

  # tokenizing the flattened message
  TokVec <- tokenize_words(Flat, lowercase = TRUE)

  # printing info
  if (verbose) {cat("Tokenized flat text column to individual words \U2713 \n")}

  # Reassigment
  DateTime <- ParsedChat$DateTime
  Sender <- ParsedChat$Sender
  Message <- ParsedChat$Message
  Media <- ParsedChat$Media
  Location <- ParsedChat$Location
  SystemMessage <- ParsedChat$SystemMessage

  # Including everything in one data frame
  DF <- data.frame(
    DateTime = DateTime,
    Sender = Sender,
    Message = Message,
    Flat = Flat,
    TokVec = I(TokVec),
    URL = I(URL),
    Media = Media,
    Location = Location,
    Emoji = I(Emoji),
    EmojiDescriptions = I(EmojiDescriptions),
    Smilies = I(Smilies),
    SystemMessage = SystemMessage, # Why is this NULL?
    stringsAsFactors = FALSE
  )


  # Creating new variable for number of Tokens
  # IMPORTANT: [this only counts user-generated tokens, not system messages]
  DF$TokCount <- sapply(DF$TokVec,function(x){

    a <- unlist(x)
    b <- a;
    b[is.na(a)] <- NA
    b[!is.na(a)] <- length(x)
    return(as.numeric(b[1]))

  })


  # fixing issue with character NAs
  DF$Flat[DF$Flat == "NA"] <- NA

  # printing info
  if (verbose) {cat("Created Dataframe containing all columns \U2713 \n")}

  # anonymizing chat participant names and mentions and removing system messages
  if (anonymize == TRUE) {

    # only anonymize when there is still consenting people in the chat, if it's only System messages, Sender don't need to be anonymized
    # We still need to add/remove the respective empty columns though
    if (length(unique(DF$Sender)) == 1 & unique(DF$Sender)[1] == "WhatsApp System Message") {

      # removing columns
      DF <- DF[,!(names(DF) %in% c("Message","Flat","TokVec","SystemMessage"))]


    } else {

      Anons <- paste(rep("Person", length(unique(DF$Sender[DF$Sender != "WhatsApp System Message"]))),
                     seq(1, length(unique(DF$Sender[DF$Sender != "WhatsApp System Message"])), 1),
                     sep = "_"
      )

      # create Anon Lookup table
      AnonLookupTable <- cbind.data.frame(Sender = unique(DF$Sender[DF$Sender != "WhatsApp System Message"]), Anon = Anons, stringsAsFactors = FALSE)

      # TODO: Currently, we simply delete system messages for anonymization. They could be anonymized using
      # RegEx but this would require very rigorous testing and constant adaptation for little benefit. The commented out
      # code below would remove all user names/numbers from the system messages but only:
      # - if the name appears in the senders column (not the case if people do not send a message in the chat!)
      # - the user names. Group names etc. might still contain PII!

      # Replacing names in SystemMessages column
      # DF$SystemMessage <- mgsub(DF$SystemMessage, AnonLookupTable$Sender, AnonLookupTable$Anon, recycle = FALSE)
      # DF$SystemMessage <- gsub("\\+Person", "Person", DF$SystemMessage, perl = TRUE)

      # factorizing Sender column and anonymizing it
      DF$Sender <- factor(DF$Sender, levels = unique(DF$Sender))
      levels(DF$Sender)[levels(DF$Sender) != "WhatsApp System Message"] <- AnonLookupTable$Anon

      # print info
      if (verbose) {cat("Anonymized names of chat participants and deleted personable identifiable information \U2713 \n")}

      # Removing message content for anonymization
      DF <- DF[,!(names(DF) %in% c("Message","Flat","TokVec","SystemMessage"))]

      # print info
      if (verbose) {cat("Deleted all columns containing message content for anonymization. \U2713 \n")}

      # Reduce media column to file types
      media_anon <- strsplit(DF$Media,".",fixed = TRUE)
      file_extensions <- sapply(media_anon,tail,1)
      DF$Media <- file_extensions

      # print info
      if (verbose) {cat("Reduced filenames (if contained) to file extensions for anonymization \U2713 \n")}

      # Reduce the links to domains
      helper <- lapply(URL, strsplit, "(?<=/)", perl = TRUE)
      helper2 <- rapply(helper, function(x) {
        x <- unlist(x)[1:3]
      }, how = "list")
      helper3 <- rapply(helper2, function(x) {
        x <- paste(x, collapse = "")
      }, how = "list")
      helper4 <- lapply(helper3, unlist)
      helper4[helper4 == "NANANA"] <- NA
      URL <- helper4

      # print info
      if (verbose) {cat("Shortened links to domains \U2713 \n")}

      # anonymizing live locations
      DF$Location[!is.na(DF$Location) & DF$Location != gsub("$","",gsub("^","",LiveLocation, fixed = TRUE),fixed = TRUE)] <- "Location shared"

      # print info
      if (verbose) {cat("Replaced shared locations with placeholders \U2713 \n")}

      # printing info
      if (verbose) {cat("Finished anonymization \U2713 \n")}

    }


    }

  if (anonymize == "add") {

    # only anonymize when there is still consenting people in the chat, if it's only System messages, Sender don't need to be anonymized
    if (length(unique(DF$Sender)) == 1 & unique(DF$Sender)[1] == "WhatsApp System Message") {

      # adding empty columns
      DF <- cbind.data.frame(DF[,1:2],
                             Sender_anon = rep(NA,dim(DF)[1]),
                             DF[,3:6],
                             URL_anon = I(rep(NA,dim(DF)[1])),
                             Media = I(DF[,7]),
                             Media_anon = I(rep(NA,dim(DF)[1])),
                             Location = DF[,8],
                             Location_anon = rep(NA,dim(DF)[1]),
                             DF[,9:13])

    } else {


    Anons <- paste(rep("Person", length(unique(DF$Sender[DF$Sender != "WhatsApp System Message"]))),
                   seq(1, length(unique(DF$Sender[DF$Sender != "WhatsApp System Message"])), 1),
                   sep = "_"
    )

    # create Anon Lookup table
    AnonLookupTable <- cbind.data.frame(Sender = unique(DF$Sender[DF$Sender != "WhatsApp System Message"]), Anon = Anons, stringsAsFactors = FALSE)

    # TODO: Currently, we simply delete system messages for anonymization. They could be anonymized using
    # RegEx but this would require very rigorous testing and constant adaptation for little benefit. The commented out
    # code below would remove all user names/numbers from the system messages but only:
    # - if the name appears in the senders column (not the case if people do not send a message in the chat!)
    # - the user names. Group names etc. might still contain PII!

    # Replacing names in SystemMessages column
    # DF$SystemMessage <- mgsub(DF$SystemMessage, AnonLookupTable$Sender, AnonLookupTable$Anon, recycle = FALSE)
    # DF$SystemMessage <- gsub("\\+Person", "Person", DF$SystemMessage, perl = TRUE)

    # factorizing Sender column and anonymizing it
    DF$Sender_anon <- factor(DF$Sender, levels = unique(DF$Sender))
    levels(DF$Sender_anon)[levels(DF$Sender_anon) != "WhatsApp System Message"] <- AnonLookupTable$Anon

    # print info
    if (verbose) {cat("Anonymized names of chat participants and deleted personable identifiable information \U2713 \n")}

    # Reduce media column to file types
    media_anon <- strsplit(DF$Media,".",fixed = TRUE)
    file_extensions <- sapply(media_anon,tail,1)
    DF$Media_anon <- file_extensions

    # print info
    if (verbose) {cat("Reduced filenames (if contained) to file extensions for anonymization \U2713 \n")}

    # Reduce the links to domains
    helper <- lapply(URL, strsplit, "(?<=/)", perl = TRUE)
    helper2 <- rapply(helper, function(x) {
      x <- unlist(x)[1:3]
    }, how = "list")
    helper3 <- rapply(helper2, function(x) {
      x <- paste(x, collapse = "")
    }, how = "list")
    helper4 <- lapply(helper3, unlist)
    helper4[helper4 == "NANANA"] <- NA
    URL_anon <- helper4

    # print info
    if (verbose) {cat("Shortened links to domains \U2713 \n")}

    # Anonymizing live locations
    DF$Location_anon <- DF$Location
    DF$Location_anon[!is.na(DF$Location) & DF$Location != gsub("$","",gsub("^","",LiveLocation, fixed = TRUE),fixed = TRUE)] <- "Location shared"

    # print info
    if (verbose) {cat("Replaced shared locations with placeholders \U2713 \n")}

    # printing info
    if (verbose) {cat("Finished anonymization \U2713 \n")}

    # pasting together combined dataframe
    DF <- cbind.data.frame(DF[,1:2],
                           Sender_anon = DF$Sender_anon,
                           DF[,3:6],
                           URL_anon = I(URL_anon),
                           Media = I(DF[,7]),
                           Media_anon = I(DF$Media_anon),
                           Location = DF[,8],
                           Location_anon = DF$Location_anon,
                           DF[,9:13])


    # printing info
    if (verbose) {cat("Added anonymized variables to non-anonymous data frame \U2713 \n")}

    }


  }

  # computing TimeOrder
  TimeOrder <- order(DF$DateTime)

  # computing Displayorder
  DisplayOrder <- 1:dim(DF)[1]

  # add them
  DF <- cbind.data.frame(DF, TimeOrder, DisplayOrder)

  # adding attributes
  attributes(DF) <- c(attributes(DF),
                      parsedAt = as.POSIXct(Sys.time()),
                      language = language,
                      detectedOS = os)

  # return datframe
  return(DF)

}
