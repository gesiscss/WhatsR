#' @title Scraping a dictionary of emoji from https://www.unicode.org/
#'
#' @description Scrapes a dictionary of emoji from \href{https://www.unicode.org/}{https://www.unicode.org/}, assuming that the website is available and its structure does not change.
#' Can be used to update the emoji dictionary contained in this package by replacing the file and recompiling the package. The dictionary is ordered according to the length of
#' the emojis' byte representation (longer ones first) to prevent partial matching of shorter strings when iterating
#' through the data frame.
#' @param unicode_page URL to the unicode page containing the emoji dictionary.
#' @param delete_header Number of lines to delete from the top of the file.
#' @param nlines Number of lines to read from the file. Passed to \code{\link{readLines}} as n. Negative Integers will read all lines.
#' @export
#' @importFrom stringi stri_trans_totitle
#' @return A data frame containing:\cr
#'      1) The native representation (glyphs) of all emoji in R \cr
#'      2) A textual description of what the emoji is displaying \cr
#'      3) The hexadecimal codepoints of the emoji \cr
#'      4) The status of the emoji (e.g. "fully-qualified" or "component") \cr
#'      5) Original order of the .txt file that the emoji were fetched from \cr
#'
#' @examples
#'Emoji_dictionary <- download_emoji(nlines = 50)


# Function to scrape an emoji dictionary from https://www.unicode.org/
download_emoji <- function(unicode_page = "https://www.unicode.org/Public/emoji/15.1/emoji-test.txt",
                           delete_header = 32,
                           nlines = -1L) {

  # checking if input is correct
  if (nlines <= 0) {} else {

    if (delete_header >= nlines) {stop("delete_header must be smaller than nlines")}

  }

  # downloading emoji list
  emoji_txt <- readLines(unicode_page, n = nlines)

  # deleting the first 32 elements of the emoji_txt file
  emoji_txt <- emoji_txt[-c(1:delete_header)]

  # deleting all empty elements from the vector emoji_text
  emoji_txt <- emoji_txt[emoji_txt != ""]

  # deleting all elements where the first character is # from the emoji_text vector
  emoji_txt <- emoji_txt[substr(emoji_txt, 1, 1) != "#"]

  # splitting each string at the first occurance of ";"
  emoji_split <- strsplit(emoji_txt, ";", fixed = TRUE)

  # extracting the first elements of emoji_split and saving them in a variable named "hex_codepoints"
  hex_codepoints <- sapply(emoji_split, "[", 1)
  hex_codepoints <- trimws(hex_codepoints)

  # taking the second elements of emoji_split and saving them in a variable named "status"
  status <- sapply(emoji_split, "[", 2)
  status <- strsplit(status, "#", fixed = TRUE)
  status <- sapply(status, "[", 1)
  status <- trimws(status)

  # taking the third elements in emoji_split and saving them in a variable named "emoji"
  emoji <- sapply(emoji_split, "[", 2)
  emoji <- strsplit(emoji, "#", fixed = TRUE)
  emoji <- sapply(emoji, "[", 2)
  emoji <- trimws(emoji)
  emoji <- strsplit(emoji, " ", fixed = TRUE)
  emoji <- sapply(emoji, "[", 1)
  emoji <- trimws(emoji)

  # taking the fourth elements in emoji_split and saving them in a variable named "description"
  description <- sapply(emoji_split, "[", 2)
  description <- strsplit(description, "#", fixed = TRUE)
  description <- sapply(description, "[", 2)
  description <- trimws(description) # \.\d
  description <- strsplit(description, "\\.\\d", perl = TRUE)
  description <- sapply(description, "[", 2)
  description <- trimws(description)
  description <- stri_trans_totitle(description)
  description <- gsub(" ","_",description)

  # Combining into data frame
  EmojiDF <- data.frame(R.native = emoji,Desc = description)

  # saving original order
  EmojiDF$OriginalOrder <- as.numeric(rownames(EmojiDF))

  # importing WhatsApp unique emoji and incorrectly parsed emoji to add them manually
  ManAdd <- readRDS(system.file("ManualEmojiAddtions.rds", package = "WhatsR"))

  # Combining manually added emoji with the rest
  EmojiDF <- rbind.data.frame(EmojiDF,ManAdd[3:4,])

  # Matching the keycap exceptions
  # TODO: This doesn't work when not using the Full document with nlines!
  EmojiDF[c(4648,4649),] <- ManAdd[1:2,]
  EmojiDF$OriginalOrder[4648:4649] <- c(4648,4649)

  # ordering from longest to shortest (prevents partial matching of shorter strings further down the line)
  EmojiDF <- EmojiDF[rev(order(nchar(as.character(EmojiDF$R.native)))), ]

  # fixing capitalization issue to pass existing checks
  EmojiDF$Desc[EmojiDF$Desc == "Grinning_Face_With_Smiling_Eyes"] <- "Grinning_Face_with_Smiling_Eyes"

  # return dictionary
  return(EmojiDF)
}
