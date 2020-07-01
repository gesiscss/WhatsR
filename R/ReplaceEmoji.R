#' @title Replacing Emoji with Text or Unicode
#'
#' @description Replaces all instances of Emoji in a textstring with a textual description or their unicode equivalent
#' @param string a character string containing the Emojis to be replaced
#' @param replacement "text" replaces the character string with a textual descirption, "uni" with correpsonding unicode
#' @param start a character string for pasted to the beginning of the replacement, default is " |Emoji_"
#' @param end charactaer string pasted to the end of the replacement, default is "| "
#' @param EmojiDictionary Dictionary for Emoji matching. Can use a version included in this package when set to "interal" or
#' an updated dataframe created by \code{\link[WhatsR]{download_emoji}}
#' @param ... arguments passed from other functions
#' @export
#' @importFrom qdapRegex rm_default
#' @importFrom utils read.csv
#' @return a character string containing textual descriptions or unicode instead of emoji
#' @examples
#' example <- readRDS(system.file("EmojiExample.rds", package = "WhatsR"))
#' print(example)
#' Replacement <- ReplaceEmoji(example)
#' print(Replacement)

# Function for replacing the Emojis
ReplaceEmoji <- function(string,
                         replacement = "text",
                         start = " |Emoji_",
                         end =  "| ",
                         EmojiDictionary = "internal",
                         ...) {

  if (identical(EmojiDictionary,"internal")){

    EmojiDictionary <- read.csv(system.file("EmojiDictionary.csv",package = "WhatsR"),
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         strip.white = FALSE,
                         colClasses = "character",
                         blank.lines.skip = TRUE)
  }

  # rm_default throws a useless warning on each iteration that we can ignore
  oldw <- getOption("warn")
  options(warn = -1)

  # cycle through the list and replace all Emojis
  # we have to add clean = FALSE and trim = FALSE to not delete whitespaces
  # that are part of the pattern.
  if(replacement == "text"){

    for (i in 1:dim(EmojiDictionary)[1]){

      string <- rm_default(string,
                           pattern = EmojiDictionary$R.native[i],
                           replacement = paste(start, EmojiDictionary$Desc[i],end, sep=""),
                           fixed = TRUE,
                           clean = FALSE,
                           trim = FALSE)
    }
  }

  else if (replacement == "uni"){

    for (i in 1:dim(EmojiDictionary)[1]){

      string <- rm_default(string,
                           pattern=EmojiDictionary$R.native[i],
                           replacement= paste(start, EmojiDictionary$Desc[i],end, sep =""),
                           fixed = TRUE,
                           clean = FALSE,
                           trim = FALSE)
    }
  }

  # turning warnings back on
  options(warn = oldw)

  # output result
  return(string)
}
