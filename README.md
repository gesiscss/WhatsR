# WhatsR
This is a first beta version of an R-package to import exported WhatsApp chatlogs, parse them into a usable dataframe format and thereby enable further analysis. This parser was built with the goal to work with chatlogs extracted on Android as well as iOS devices, run on Linux, MAc and Windows and to be able to handle multiple languages. Currently, only English and German are supported, but in principle, other languages could be added relatively easily (see below). The repo also contains a function to scrape and update the Emoji dictionary, should new Emoji be added to WhatsApp in the meantime.


## What does this parser extract?
Currently, the parser extracts the following information from an updated WhatsApp Chatlog:

 - A column to indicate the date and time when the message was send
 - A column to indicate the name of the sender (can be anonymized)
 - A column containing the raw message body (Emoji are replaced with textual representation)
 - A column containgin a "flat" message, stripped of Emoji, numbers, special characters, file attachments, sent Locations etc.
 - A column containing a tokenized version of the flat message
 - A column containing only URLs that were contained in the messages (optional: can be shortend to only display domains)
 - A column containing only the names of attached meda files
 - A column containing only sent locations and indicators for shared live locations
 - A column containing only Emoji that were used in the message
 - A column containing only Emoticons (e.g. ":-)") that were used in the message
 - A column containing WhatsApp System Messages (e.g."You added Frank to the group"), these do not appear in the Message and       Flat column and get recognized as "WhatsApp System" in the Sender column
 - A column specifying the order of the rows according to the timestamp the messages have on the phone used for extracting the    chatlog
 - A column for specifying the order of the rows as they are displayed on the phone used for extracting the chatlog (this is      not necessarily the same order as the one for the timestamps, for example, when messages are written and send but there is      no internet conenction)
 
 ## How do I use this parser?
 
 ### 1) Install it
 ```R
 devtools::install_github("gesiscss/WhatsR/WhatsR")
 ```
 
 ### 2) Extract a Chatlog on your phone
 
 For Android Phones: https://faq.whatsapp.com/en/android/23756533/?category=5245251
 
 For Iphones: https://faq.whatsapp.com/en/iphone/20888066/?category=5245251#email (see bottom of page)
 
 ### 3) Parse the Chat according to your settings
 
 ```R
 Dataframe <- WhatsAppParse(PathToYourTxTFile,
                            EmojiDic = "internal",
                            smilies = 2,
                            anon = TRUE,
                            media = TRUE,
                            web = "domain",
                            order = "both",
                            language = "english",
                            os = "android")                               
```

## Does this parser work with other languages too?

In principle yes, but you would need to add language specific strings and regexes to the language.csv file contained in this repo. These strings are also different for android and iOS. For example, file attachments are marked with ` FILENAME (file attached)` in Android but with `<attached: FILENAME>` in iOS. If you are willing to extract these strings from one of your chatloags in your language, I will gladly add them to the language file and update this repo.
