#' @title Getting picture links for printing emoji in plots
#'
#' @description Fetches links to picture of emojis from www.emojipedia.org, which can be used to plot the actual emojis
#' in plots instead of their textual description only. The procedure was adapted from: https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/.
#' This only needs to be done when new emoji are added to www.emojipedia.org and the emoji dictionary needed to be updated.
#' @param emojidict A dictionary of emojis created by \code{\link{download_emoji}}.
#' @return A dictionary of emojis inlcuding two new columns with links to the pictures and html tags of those links respectively.
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attrs
#' @importFrom rvest html_node
#' @export
#' @examples
#' EmojiDic <- read.csv(system.file("EmojiDictionary.csv", package = "WhatsR"),stringsAsFactors = FALSE)
#' #EmojiDic2 <- add_emoji_links(EmojiDic[1:20,])

# function for getting Emoji links and html tags
add_emoji_links <- function(emojidict) {

  # First of all, we assign local variable with NULL to prevent package build error: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
  html_attr <- `.` <- NULL

  # Second, we need to import the necessary function for getting the links
  # and trasnforming them into html tags, those are taken and modified from: https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/

  # 1) function to get an image link for every Emoji
  emoji_to_link <- function(x) {

    if (!exists("counter")){counter <- 0}

    out <- tryCatch(
      {
        # Just to highlight: if you want to use more than one
        # R expression in the "try" part then you'll have to
        # use curly brackets.
        # 'tryCatch()' will return the last evaluated expression
        # in case the "try" part was completed successfully

        counter <<- counter + 1

        message(paste("downloaded", counter, "Emoji Pictures"))

        Link <- paste0("https://emojipedia.org/emoji/",x) %>%
          read_html() %>%
          html_nodes("tr td a") %>%
          .[1] %>%
          html_attr("href") %>%
          paste0("https://emojipedia.org/", .) %>%
          read_html() %>%
          html_node('div[class="vendor-image"] img') %>%
          html_attr("src")

      },

      error=function(cond) {
        message(paste("URL does not seem to exist:", url))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NA)
      },

      warning=function(cond) {
        message(paste("URL caused a warning:", url))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      },
      finally={
        # NOTE:
        # Here goes everything that should be executed at the end,
        # regardless of success or error.
        # If you want more than one expression to be executed, then you
        # need to wrap them in curly brackets ({...}); otherwise you could
        # just have written 'finally=<expression>'
        message(paste("Processed URL:", Link))
      }
    )
    return(out)
  }

  # 2) function to embed the image links into a viable html tag
  link_to_img <- function(x, size = 25) {
    paste0("<img src='", x, "' width='", size, "'/>")
  }

  #### Getting the emoji pictures
  data <- sapply(emojidict$R.native,emoji_to_link)

  # checking for NAs
   if (length(which(is.na(data))) > 0) {

     warning("Some Emojis could not be grabbed automatically, Please update their links manually for correct plotting")
     warning(paste("The following emojis need manual updating:", data$Desc[which(is.na(data))]))


   }

  # adding column with html tags
  data2 <- sapply(data,link_to_img)

  # binding the columns together
  NewEmojiDic <- cbind.data.frame(emojidict,Link = data, HTML = data2,stringsAsFactors = FALSE)

  # returning
  return(NewEmojiDic)

}
