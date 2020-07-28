#' @title Parsing exported WhatsApp Textfiles as Dataframes
#'
#' @description Creates a dataframe from an exported WhatsApp textfile containing one row per message. Some columns
#' were saved as lists using the I() function so that multiple elements could be stored per message, while still maintaining
#' the general structure of one row per message. In your analysis, you should treat these columns as lists or unlist them first.
#' @param name the name of the exported Whatsapp textfile to be parsed as a character string
#' @param os operating system of the phone the chat was exported from. Supports "android" or "iOS"
#' @param EmojiDic Dictionary for Emoji matching. Can use a version included in this package when set to "interal" or
#' an updated dataframe created by \code{\link[WhatsR]{download_emoji}}
#' @param smilies 1 uses \code{\link[qdapRegex]{ex_emoticon}} to extract smilies, 2 uses a more inclusive custom list
#' of smilies containing all mentions from https://de.wiktionary.org/w/index.php?title=Verzeichnis:International/Smileys
#' and manually added ones
#' @param anon TRUE results in the vector of sender names being anonymized, FALSE displays the actual names, "add" adds a
#' column of anomyized names next to the actual names
#' @param media TRUE/FALSE indicates whether the chatlog was downloaded with or without mediafiles. If TRUE, names of
#' attached mediafiles will be extracted into a seperate column.
#' @param web  "domain" will shorten sent links to domains, "url" will display the full URL
#' @param ... further arguments passed down to ReplaceEmoji()
#' @param order determines how the messages are ordered. "display" order them in the same order that they appear on the phone
#' that the messages were exported from. "time" orders the messages according to the WhatsApp Timestamp the message received while it was sent.
#' Due to internet problems, these orders are not necessarily interchangeable. "both" gives two columns with the respective orders
#' @param language indicates the language of WhatsApp on the phone with which the messages were exported. This is important because
#' it changes the structure of date/time columns and indicators for sent media. Currently, "english" and "german" are available.
#' @param rpnl replace newline. A character string for replacing linebreaks within messages for the parsed message for better readibility
#' @param rpom replace omitted media. A character string replacing the indicator for omitted media files for better readibility
#' @importFrom readr parse_character
#' @importFrom qdapRegex rm_url
#' @importFrom qdapRegex rm_between
#' @importFrom qdapRegex ex_emoticon
#' @importFrom stringr str_replace_all
#' @importFrom tokenizers tokenize_words
#' @importFrom stringi stri_extract_all_regex
#' @importFrom mgsub mgsub
#' @return A dataframe containing:
#'
#'      1) A column to indicate the date and time when the message was send \cr
#'      2) A column containing the anonimized name of the sender \cr
#'      3) A column to indicate the name of the sender \cr
#'      4) A column containing the raw message body (Emoji are replaced with textual representation) \cr
#'      5) A column containgin a "flat" message, stripped of Emoji, numbers, special characters, file attachments, sent Locations etc. \cr
#'      6) A column containing a tokenized version of the flat message \cr
#'      7) A column containing only URLs that were contained in the messages (optional: can be shortend to only display domains) \cr
#'      8) A column containing only the names of attached meda files \cr
#'      9) A column containing only sent locations and indicators for shared live locations \cr
#'      10) A column containing only Emoji that were used in the message \cr
#'      11) A column containing only Emoticons (e.g. ":-)") that were used in the message \cr
#'      12) A column containing the number of tokens per message, derived from the "flattened" message \cr
#'      13) A column containing WhatsApp System Messages in group chats (e.g."You added Frank to the group") \cr
#'      14) A column specifying the order of the rows according to the timestamp the messages have on the phone used for extracting the chatlog \cr
#'      15) A column for specifying the order of the rows as they are displayed on the phone used for extracting the chatlog \cr
#'
#' @examples
#' data <- parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"),
#'                       media = TRUE,
#'                       language = "english")
#' @export

# Function to import a WhatsApp Textmessage and parse it into a readable dataframe
parse_chat <- function(name,
                       EmojiDic = "internal",
                       smilies = 2,
                       anon = "add",
                       media = TRUE,
                       web = "domain",
                       order = "both",
                       language = "german",
                       os = "android",
                       rpnl = " start_newline ",
                       rpom = " media_omitted ",
                       ...){

        # loading language indicators
        WAStrings <- read.csv(system.file("Languages.csv", package = "WhatsR"),
                              stringsAsFactors = F,
                              fileEncoding = "UTF-8")

        # selecting indicators based on language
        Indicators <- WAStrings[WAStrings$Settings == paste0(language,os),]

        # assigning indicator strings for message bodies
        ExtractAttached <- Indicators$ExtractAttached
        DeleteAttached <- Indicators$DeleteAttached
        OmittanceIndicator <- Indicators$OmittanceIndicator
        SentLocation <- Indicators$SentLocation
        LiveLocation <- Indicators$LiveLocation
        MissedCall <- Indicators$MissedCall

        # assigning indicator strings without sender info
        StartMessage <- Indicators$StartMessage
        StartMessageGroup <- Indicators$StartMessageGroup
        GroupCreateSelf <- Indicators$GroupCreateSelf
        GroupCreateOther <- Indicators$GroupCreateOther
        GroupRenameSelf <- Indicators$GroupRenameSelf
        GroupPicChange <- Indicators$GroupPicChange
        GroupRenameOther <- Indicators$GroupRenameOther
        UserRemoveSelf <- Indicators$UserRemoveSelf
        UserAddSelf <- Indicators$UserAddSelf
        UserRemoveOther <- Indicators$UserRemoveOther
        UserAddOther <- Indicators$UserAddOther
        GroupPicChangeOther <- Indicators$GroupPicChangeOther
        UserNumberChangeKnown <- Indicators$UserNumberChangeKnown
        UserNumberChangeUnknown <- Indicators$UserNumberChangeUnknown
        DeletedMessage <- Indicators$DeletedMessage
        UserLeft <- Indicators$UserLeft
        SafetyNumberChange <- Indicators$SafetyNumberChange

        # New ultimate time Regex that detects 24h/ampm, american date format, european date format and all combinations
        if (os == "android") {

                TimeRegex <- c("(?!^)(?=((\\d{2}\\.\\d{2}\\.\\d{2})|(\\d{1,2}\\/\\d{1,2}\\/\\d{2})),\\s\\d{2}\\:\\d{2}((\\s\\-)|(\\s(?i:(am|pm))\\s\\-)))")

        } else if (os == "ios") {

                TimeRegex <- c("(?!^)(?=\\[((\\d{2}\\.\\d{2}\\.\\d{2})|(\\d{1,2}\\/\\d{1,2}\\/\\d{2})),\\s\\d{1,2}\\:\\d{2}((\\:\\d{2}\\s(?i:(pm|am)))|(\\s(?i:(pm|am)))|(\\:\\d{2}\\])|(\\:\\d{2})|(\\s))\\])")

        }

        # print info
        cat(paste("Imported matching strings for: ", paste(language, os, sep = " ") ," \U2713 \n", sep = ""))

        # We use readChar so that we can do the splitting manually after replacing the
        # Emojis, special characters and newlines
        RawChat <- readChar(name,file.info(name)$size)

        # printing info
        cat("Imported raw chat file \U2713 \n")

        # replacing EMOJI with text representations
        ReplacedEmojiChat <- ReplaceEmoji(RawChat, EmojiDictionary = EmojiDic)

        # printing info
        cat("Replaced emoji with text representations \U2713 \n")

        # Replacing special characters
        ReplacedSpecialCharactersChat <- parse_character(ReplacedEmojiChat)

        # Deleting Left-to-right marker if present
        ReplacedSpecialCharactersChat <- gsub("\u200e","",ReplacedSpecialCharactersChat)

        # Deleting zero-width no break space if present
        ReplacedSpecialCharactersChat <- gsub("\uFEFF","",ReplacedSpecialCharactersChat)

        # printing info
        cat("Replaced special characters \U2713 \n")

        if (os == "android") {

                # Parsing the message according to android text structure
                ParsedChat <- ParseAndroid(ReplacedSpecialCharactersChat,
                                           nl = "\n",
                                           nlreplace = rpnl,
                                           mediaomitted = OmittanceIndicator,
                                           mediaindicator = ExtractAttached,
                                           sentlocation = SentLocation,
                                           livelocation = LiveLocation,
                                           datetimeindicator = TimeRegex,
                                           mediareplace = OmittanceIndicator)

                # printing info
                cat("Parsed chat according to Android document structure \U2713 \n")

        } else if (os == "ios") {

                # Parsing the message according to android text structure
                ParsedChat <- ParseIos(ReplacedSpecialCharactersChat,
                                       nl = "\n",
                                       nlreplace = rpnl,
                                       mediaomitted = OmittanceIndicator,
                                       mediaindicator = DeleteAttached,
                                       sentlocation = SentLocation,
                                       livelocation = LiveLocation,
                                       datetimeindicator = TimeRegex,
                                       mediareplace = OmittanceIndicator)

                # printing info
                cat("Parsed chat according to iOS document structure \U2713 \n")

        }

        ### We create handy vectors for used Emojis, extracted links, extracted media data
        # and one containing the message without stopwords, Emojis, linebreaks, URLs and punctuation

        # extracting links
        URL <- (rm_url(ParsedChat$Message, extract = TRUE))

        # printing info
        cat("Extracted Links from text \U2713 \n")

        if (web == "domain") {

                # Reduce the links to domain-names
                helper <- lapply(URL,strsplit,"(?<=/)",perl = TRUE)
                helper2 <- rapply(helper,function(x){x <- unlist(x)[1:3]},how = "list")
                helper3 <- rapply(helper2,function(x){x <- paste(x,collapse = "")},how = "list")
                helper4 <- lapply(helper3,unlist)
                helper4[helper4 == "NANANA"] <- NA
                URL <- helper4

                # printing info
                cat("Shortend links to domains \U2713 \n")

        }

        # Removing Emoji and pipes from flattened message
        Emoji <- rm_between(ParsedChat$Message," |Emoji_","| ", extract = TRUE, include.markers = TRUE)
        Emoji <- lapply(Emoji,function(x){substr(x,2,nchar(x) - 1)})

        # printing info
        cat("Extracted emoji from text \U2713 \n")

        ### Creating a flattened Message for text mining

        # removing Emojis,newlines, media indicators
        Flat <- rm_between(ParsedChat$Message," |Emoji_","| ",replacement = "")
        Flat <- rm_between(Flat," start_newlin","e ",replacement = "")
        Flat <- str_replace_all(Flat,pattern = OmittanceIndicator, replacement = "")

        # printing info
        cat("Removed emoji, newlines and media file indicators from text column \U2713 \n")

        # printing info
        cat("Deleted filenames from text column \U2713 \n")

        # deleting the file attachments from flattend message
        if (os == "android") {

                Flat <- gsub(paste0("(.)*?",substring(DeleteAttached,4,nchar(DeleteAttached) - 1),"($|\\s)"), "", Flat , perl = TRUE)

        } else if (os == "ios") {

                Flat <- gsub(x = Flat, pattern = ExtractAttached, replacement = "" , perl = T)
                # We might need to fix an issue here where Filenames are not deleted properly if there is text behind them.
                # Needs further testing!

        }


        # printing info
        cat("Deleted URLs from text column \U2713 \n")

        ### Smilies

        # lazy version with prebuild dictionary
        if (smilies == 1) {

                Smilies <- ex_emoticon(Flat)

                # printing info
                cat("Extracted Smilies using prebuild dictionary \U2713 \n")

        } else if (smilies == 2) { #using custom dictionary

                # package version
                smilies <- read.csv(system.file("SmileyDictionary.csv", package = "WhatsR"),
                                    stringsAsFactors = F)

                # deleting whitespace from smilies
                smilies <- smilies[,2]
                smilies <- trimws(smilies)

                # Splitting smilies
                Smilies <- sapply(strsplit(Flat, " "), function(x) x[x %in% smilies])
                Smilies[lapply(Smilies,length) == 0] <- NA

                # printing info
                cat("Extracted smilies using custom build dictionary \U2713 \n")

        }

        # replacing sent location in flattened message
        Flat <- gsub(x = Flat,
                     pattern = SentLocation,
                     replacement = NA,
                     perl = T)

        # replacing live location in falttened message
        Flat <- gsub(x = Flat,
                     pattern = LiveLocation,
                     replacement = NA,
                      perl = T)

        # replacing missed voice calls in flattened message
        Flat <- gsub(x = Flat,
                     pattern = MissedCall,
                     replacement = NA,
                     perl = T)

        # deleting URLs from messages
        Flat <- rm_url(Flat)
        Flat[Flat == "" | Flat == "NULL"] <- NA

        # removing punctuation and special characters
        Flat <- gsub("[[:punct:]]"," ", Flat)

        # printing info
        cat("Removed punctuation, smilies and special characters from text column \U2713 \n")

        # removing numbers
        Flat <- gsub("\\d","",Flat, perl = TRUE)

        # printing info
        cat("Removed numbers from text column \U2713 \n")

        # adding exra space at the end to make regex work
        Flat <- paste(Flat," ",sep = "")

        # deleting single letters as leftovers from smilies with removed punctuation
        Flat <- gsub(pattern = "(?=\\s[[:alpha:]]\\s)..",
                     replacement = "",
                     Flat,
                     perl = TRUE)

        # deleting trailing whitespace
        Flat <- trimws(Flat)

        # making all empty strings NA
        Flat[nchar(Flat) == 0] <- NA

        # tokenizing the flattened message
        TokVec <- tokenize_words(Flat, lowercase = FALSE)

        # printing info
        cat("Tokenized text column to single words \U2713 \n")

        # Reassigment
        DateTime <- ParsedChat$DateTime
        Sender <- ParsedChat$Sender
        Message <- ParsedChat$Message
        Media <- ParsedChat$Media
        Location <- ParsedChat$Location

        # Including everything in dataframe
        DF <- data.frame(DateTime,
                         Sender,
                         Message,
                         Flat,
                         I(TokVec),
                         I(URL),
                         Media,
                         Location,
                         I(Emoji),
                         I(Smilies),
                         stringsAsFactors = FALSE)

        # Creating new variable for number of Tokens
        DF$TokCount <- sapply(DF$TokVec,function(x){length(unlist(x))})
        DF[which(DF$TokVec == "NA"),"TokCount"] <- 0
        DF$TokCount <- unlist(DF$TokCount)

        # fixing weird issue with character NAs
        DF$Flat[DF$Flat == "NA"] <- NA

        # printing info
        cat("Created Dataframe containing all columns \U2713 \n")

        # Extracting WhatsApp System Messages and removing them from Message and flattened Message body
        WAStrings <- c(StartMessage,
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
                       SafetyNumberChange)

        # checking whether a WhatsApp Message was parsed into the sender column
        WAMessagePresent <- unlist(stri_extract_all_regex(str = DF$Sender, pattern = paste(WAStrings, collapse = "|")))
        DF$SystemMessage <- WAMessagePresent
        DF$Sender[!is.na(WAMessagePresent)] <- "WhatsApp System Message"

        # printing info
        cat("Differentiated System Messages from User generated content \U2713 \n")

        # anonymizing chat participants
        if (anon == TRUE) {

                Anons <- paste(rep("Person", length(unique(DF$Sender))),
                               seq(1,length(unique(DF$Sender)),1),
                               sep = "_")

                # create Anon Lookup table
                AnonLookupTable <- cbind.data.frame(Sender = unique(DF$Sender),Anon = Anons,stringsAsFactors = FALSE)

                # Replacing names in SystemMessage Column
                DF$SystemMessage <- mgsub(DF$SystemMessage, AnonLookupTable$Sender, AnonLookupTable$Anon, recycle = FALSE)
                DF$SystemMessage <- gsub("\\+Person","Person",DF$SystemMessage, perl = TRUE)
                # There is still an issue with People who are added to the conversation but never send a message: We cannot anonymize them
                # because they do not show up in the Sender column, the anonimization breaks down for these cases!

                DF$Sender <- factor(DF$Sender, levels = unique(DF$Sender))
                levels(DF$Sender) <- Anons

                # printing info
                cat("Anonymized names of chat participants \U2713 \n")

        }

        if (anon == "add") {

                Anons <- paste(rep("Person", length(unique(DF$Sender))),
                               seq(1,length(unique(DF$Sender)),1),
                               sep = "_")

                Anonymous <- DF$Sender
                Anonymous <- factor(Anonymous, levels = unique(Anonymous))
                levels(Anonymous) <- Anons

                # printing info
                cat("Anonymized names of chat participants \U2713 \n")
        }


        if (anon == "add") {

                DF <- cbind.data.frame(DF[1],Anonymous,DF[2:ncol(DF)])

        }

        # including ordering
        if (order == "time") {

                #change order to time
                DF <- DF[order(DF$DateTime),]

        } else if (order == "both") {

                # TimeOrder
                TimeOrder <- order(DF$DateTime)

                # Displayorder
                DisplayOrder <- 1:dim(DF)[1]

                # add them
                DF <- cbind.data.frame(DF,TimeOrder,DisplayOrder)


        } else {}

        # Deleting empty rows
        DF <- DF[rowSums(is.na(DF)) <= 10,]

        # return datframe
        return(DF)
}
