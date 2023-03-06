# `WhatsR` <img src="inst/WhatsR_sticker.png" align="right" width="250" height="250"/>
This is a first beta version of an R-package to import exported WhatsApp chatlogs, parse them into a usable dataframe format and thereby enable further analysis. This parser was built with the goal to work with chatlogs extracted on Android as well as iOS devices, run on Linux, Mac and Windows and to be able to handle multiple languages. Currently, only English and German are supported, but in principle, other languages could be added relatively easily (see below). The repo also contains a function to scrape and update the Emoji dictionary, should new Emoji be added to WhatsApp in the meantime.


## What does this parser extract?
Currently, the parser extracts the following information from an updated WhatsApp Chatlog:

 - A column to indicate the date and time when the message was send
 - A column to indicate the name of the sender (can be anonymized)
 - A column containing the raw message body 
 - A column containing a "flat" message, stripped of Emoji, numbers, special characters, file attachments, sent Locations etc.
 - A column containing a tokenized version of the flat message
 - A column containing only URLs that were contained in the messages (optional: can be shortend to only display domains)
 - A column containing only the names of attached meda files
 - A column containing only sent locations and indicators for shared live locations
 - A column containing only Emoji that were used in the message
 - A column containing only Smilies (e.g. ":-)") that were used in the message
 - A column containing WhatsApp System Messages (e.g."You added Frank to the group"), these do not appear in the Message and       Flat columns and get recognized as "WhatsApp System Message" in the Sender column
 - A column specifying the order of the rows according to the timestamp the messages have on the phone used for extracting the    chatlog
 - A column for specifying the order of the rows as they are displayed on the phone used for extracting the chatlog (this is      not necessarily the same order as the one for the timestamps, for example, when messages are written and send but there is      no internet connection)
 
 ## How do I use this parser?
 
 ### 1) Install it
 ```R
 devtools::install_github("gesiscss/WhatsR")
 ```
 
 ### 2) Extract a Chatlog on your phone
 
 For Android Phones: https://faq.whatsapp.com/en/android/23756533/?category=5245251
 
 For Iphones: https://faq.whatsapp.com/en/iphone/20888066/?category=5245251#email (see bottom of page)
 
 ### 3) Parse the Chat according to your settings
 
 ```R
 data <- parse_chat("Path/YourTextFile.txt")                               
```

## Does this parser work with other languages too?

In principle yes, but you would need to add language specific strings and regexes to the language.csv file contained in this repo. These strings are also different for android and iOS. For example, file attachments are marked with ` FILENAME (file attached)` in Android but with `<attached: FILENAME>` in iOS. If you are willing to extract these strings from one of your chatlogs in your language, I will gladly add them to the language file and update this repo.


## Troubleshooting

This section contains issues you may be experiencing when installing or using WhatsR.

#### Package requirements

To install WhatsR, please first download and install the latest R-release version of RTools for your operating system. Please note that it is also necessary to have rJava installed. For information on its installation please see <https://github.com/s-u/rJava/blob/master/README.md>.


# Examples

The package also includes some functions to compute additional metrics and visualize them. We will provide some basic examples for chats with two participants and for group chats with multiple participants. The used chat is a private chat that was parsed with the `anon = TRUE` parameter to exclude participant names. The dataframe is called `data`. All plotting functions include additional parameters to restrict the range of the data

### Basic statistics
Some basic statistics about the nature of the chat.

```
summarize_chat(data)
```

```
$NumberOfMessages
[1] 18827

$NumberOfTokens
[1] 186934

$NumberOfParticipants
[1] 3

$StartDate
[1] "2020-10-27 18:51:00 UTC"

$EndDate
[1] "2022-10-06 19:57:00 UTC"

$TimeSpan
Time difference of 709.0458 days

$NumberOfSystemMessages
[1] 1

$NumberOfEmoji
[1] 8637

$NumberOfSmilies
[1] 3877

$NumberOfLinks
[1] 591

$NumberOfMedia
[1] 3

$NumberOfLocation
[1] 1

```
### Token Summary per Person

```
summarize_tokens_per_person(data)
```

```
$`WhatsApp System Message`
$`WhatsApp System Message`$Timespan
$`WhatsApp System Message`$Timespan$Start
[1] "2020-10-27 18:51:00 UTC"

$`WhatsApp System Message`$Timespan$End
[1] "2022-10-06 19:57:00 UTC"


$`WhatsApp System Message`$TokenStats
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      1       1       1       1       1       1 


$Person_1
$Person_1$Timespan
$Person_1$Timespan$Start
[1] "2020-10-27 18:51:00 UTC"

$Person_1$Timespan$End
[1] "2022-10-06 19:57:00 UTC"


$Person_1$TokenStats
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   6.000   9.195  13.000 169.000 


$Person_2
$Person_2$Timespan
$Person_2$Timespan$Start
[1] "2020-10-27 18:51:00 UTC"

$Person_2$Timespan$End
[1] "2022-10-06 19:57:00 UTC"


$Person_2$TokenStats
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    1.00    6.00   10.75   14.00  407.00 

```


### Message Distribution
Destribution of sent Messages.
```
plot_messages(data, excludeSM = TRUE)
```

![](man/figures/plot_messages()_bar.png)

```
plot_messages(data, plot = "cumsum", excludeSM = TRUE)
```

![](man/figures/plot_messages()_cumsum.png)

### Token Distribution
Destribution of sent Tokens (words).
```
plot_tokens(data, excludeSM = TRUE)
```
![](man/figures/plot_tokens()_bar.png)

```
plot_tokens(data, plot = "cumsum", excludeSM = TRUE)
```

![](man/figures/plot_tokens()_cumsum.png)

```
plot_tokens(data, plot = "box", excludeSM = TRUE)
```
![](man/figures/plot_tokens()_box.png)

```
plot_tokens(data, plot = "violin", excludeSM = TRUE)
```

![](man/figures/plot_tokens()_violin.png)


### Tokens over Time
Destribution of sent Tokens per Person over time
```
plot_tokens_over_time(data, plot = "alltime", excludeSM = TRUE)
```
![](man/figures/plot_tokens_over_time()_alltime.png)

```
plot_tokens_over_time(data, plot = "heatmap", excludeSM = TRUE)
```

![](man/figures/plot_tokens_over_time()_heatmap.png)

```
plot_tokens_over_time(data, plot = "year", excludeSM = TRUE)
```
![](man/figures/plot_tokens_over_time()_year.png)

```
plot_tokens_over_time(data, plot = "weekday", excludeSM = TRUE)
```

![](man/figures/plot_tokens_over_time()_weekday.png)

```
plot_tokens_over_time(data, plot = "hours", excludeSM = TRUE)
```

![](man/figures/plot_tokens_over_time()_hours.png)

### Wordcloud
Wordcloud of sent tokens, for all chat participants overall - and seperately for each participant.
```
plot_wordcloud(data, font.size=50, min.freq= 1000)
```
![](man/figures/plot_wordlcoud().png)

```
plot_wordcloud(data, comparison = TRUE, excludeSM = TRUE, font.size=50, min.occur= 300)
```
![](man/figures/plot_wordcloud()_comparison.png)

### Lexical Dispersion Plot
Occurances of keywords in the chat with. Example keyword is "Weihnachten" (Christmas).
```
plot_lexical_dispersion(data,keywords = c("weihnachten"), excludeSM = TRUE)
```
![](man/figures/plot_lexical_dispersion().png)

### Sent Links
Amount of sent Links per person and over time

```
plot_links(data, plot = "cumsum", excludeSM = TRUE)
```
![](man/figures/plot_links()_cumsum.png)

```
plot_links(data, plot = "heatmap", excludeSM = TRUE)
```
![](man/figures/plot_links()_heatmap.png)

```
plot_links(data, plot = "bar", min.occur = 10, excludeSM = TRUE)
```
![](man/figures/plot_links()_bar.png)

```
plot_links(data, plot = "splitbar", min.occur = 5, excludeSM = TRUE)
```
![](man/figures/plot_links()_splitbar.png)

### Sent Smilies
Amount of sent Smilies per person and over time
```
plot_smilies(data, plot = "cumsum", excludeSM = TRUE)
```

![](man/figures/plot_smilies()_cumsum.png)

```
plot_smilies(data, plot = "bar", excludeSM = TRUE)
```
![](man/figures/plot_smilies()_bar.png)

```
plot_smilies(data, plot = "splitbar", excludeSM = TRUE,min.occur = 10)
```
![](man/figures/plot_smilies()_splitbar.png)

```
plot_smilies(data, plot = "splitbar", SmilieVec = c(":)",":>",":D",":p",":("), excludeSM = TRUE)
```

![](man/figures/plot_links()_splitbar_SmilieVec.png)

### Sent Emojis
Amount of sent Emojis per person and over time
```
plot_emoji(data, plot = "cumsum", excludeSM = TRUE)
```
![](man/figures/plot_emoji()_cumsum.png)

```
plot_emoji(data, plot = "bar", min.occur = 50, excludeSM = TRUE)
```

![](man/figures/plot_emoji()_bar.png)

```
plot_emoji(data, plot = "splitbar", min.occur = 50, excludeSM = TRUE)
```
![](man/figures/plot_emoji()_splitbar.png)

```
plot_emoji(data, plot = "heatmap", excludeSM = TRUE)
```
![](man/figures/plot_emoji()_heatmap.png)

### Location Visualization
Plotting mentioned locations by person
```
plot_location(data, add.jitter = TRUE, jitter.val = 0.05 ,mapzoom = 7, excludeSM = TRUE)
```
![](man/figures/plot_location().png)

### Replytimes
Plotting time it takes to respond and be responded to by person.
```
plot_replytimes(data, type = "replytime", excludeSM = TRUE)
```
![](man/figures/plot_replytimes().png)

```
plot_replytimes(data, type = "reactiontime", excludeSM = TRUE)
```
![](man/figures/plot_replytimes()_reaction.png)

## Group Chat
The previous example chat was exported without media files and only includes two people chatting. To demonstrate the visualization of different media files and the network of chat participants, we will use a different example file, exported from a group chat.

### Sent Media
Amount of sent Media files per person and over time

```
plot_media(data, plot = "cumsum", excludeSM = TRUE)
```
![](man/figures/plot_media()_cumsum.png)

```
plot_media(data, plot = "heatmap", excludeSM = TRUE)
```
![](man/figures/plot_media()_heatmap.png)

```
plot_media(data, plot = "bar", excludeSM = TRUE)
```
![](man/figures/plot_media()_bar.png)

```
plot_media(data, plot = "splitbar",excludeSM = TRUE)
```
![](man/figures/plot_media()_splitbar.png)


### Interactive Network
Interactive network of chat participants. A connection represents a response to a message. Each Message is interpreted as a response to the previous message. Consecutive messages by the same chat participant are summarized into one "session". The shown plot is simple image, the actual output is an interactive HTML object that GitHub does not permit in Readme files.
```
plot_network(data)
```
![](man/figures/plot_network()_image.png)

