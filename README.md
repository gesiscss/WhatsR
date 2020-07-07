# WhatsR
This is a first beta version of an R-package to import exported WhatsApp chatlogs, parse them into a usable dataframe format and thereby enable further analysis. This parser was built with the goal to work with chatlogs extracted on Android as well as iOS devices, run on Linux, Mac and Windows and to be able to handle multiple languages. Currently, only English and German are supported, but in principle, other languages could be added relatively easily (see below). The repo also contains a function to scrape and update the Emoji dictionary, should new Emoji be added to WhatsApp in the meantime.


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
 - A column containing only Smilies (e.g. ":-)") that were used in the message
 - A column containing WhatsApp System Messages (e.g."You added Frank to the group"), these do not appear in the Message and       Flat column and get recognized as "WhatsApp System" in the Sender column
 - A column specifying the order of the rows according to the timestamp the messages have on the phone used for extracting the    chatlog
 - A column for specifying the order of the rows as they are displayed on the phone used for extracting the chatlog (this is      not necessarily the same order as the one for the timestamps, for example, when messages are written and send but there is      no internet conenction)
 
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
 Dataframe <- parse_chat(PathToYourTxTFile,
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

# Examples

The package also includes some functions to compute additional metrics and visualize them. We will provide some basic examples for chats with two participants and for group chats with multiple participants. The used chat is a private chat that was parsed with the `anon = TRUE` parameter to exclude participant names. The dataframe is called `data`. All plotting functions include additional parameters to restrict the range of the data

### Basic statistics
Some basic statistics about the nature of the chat.

```
summarize_chat(data)
```

```
$Participants
[1] 2

$FirstSender
[1] Person_1
Levels: Person_1 Person_2

$FirstMessage
[1] "echt jetzt?"

$TimeSpan
$TimeSpan$FirstMessage
[1] "2018-03-19 23:42:00 UTC"

$TimeSpan$LastMessage
[1] "2019-07-09 13:44:00 UTC"

$TimeSpan$Duration
Time difference of 476.5847 days

```
### Token Summary per Person

```
summarize_tokens_per_person(data)
```

### Message Distribution
Destribution of sent Messages.
```
plot_messages(data)
```

<a href="https://ibb.co/SQ7pmT4"><img src="https://i.ibb.co/0BV39NR/plot-messages1.png" alt="plot-messages1" border="0"></a>

```
plot_messages(data, plot = "pie")
```

<a href="https://imgbb.com/"><img src="https://i.ibb.co/ngwjWgQ/plot-messages2.png" alt="plot-messages2" border="0"></a>

### Token Distribution
Destribution of sent Tokens (words).
```
plot_tokens(data)
```
<a href="https://ibb.co/VqYyTby"><img src="https://i.ibb.co/jrkKDqK/plot-tokens1.png" alt="plot-tokens1" border="0"></a>

```
plot_tokens(data, plot = "cumsum")
```

<a href="https://ibb.co/cQVvSX5"><img src="https://i.ibb.co/YhwWFT9/plot-tokens4.png" alt="plot-tokens4" border="0" /></a>

```
plot_tokens(data, plot = "box")
```
<a href="https://ibb.co/n0t1KdS"><img src="https://i.ibb.co/X4M5G0T/plot-tokens2.png" alt="plot-tokens2" border="0"></a>

```
plot_tokens(data, plot = "violin")
```

<a href="https://imgbb.com/"><img src="https://i.ibb.co/k00W4nq/plot-tokens3.png" alt="plot-tokens3" border="0"></a>


### Tokens over Time
Destribution of sent Tokens per Person over time
```
plot_tokens_over_time(data)
```
<a href="https://ibb.co/Gpkb12y"><img src="https://i.ibb.co/Kh73Qqd/plot-tokens-over-time1.png" alt="plot-tokens-over-time1" border="0"></a>

```
plot_tokens_over_time(data, plot = "heatmap")
```

<a href="https://ibb.co/6r0h3JH"><img src="https://i.ibb.co/MZkTY5c/plot-tokens-over-time5.png" alt="plot-tokens-over-time5" border="0"></a>

```
plot_tokens_over_time(data, plot = "year")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/gjHZVJH/plot-tokens-over-time2.png" alt="plot-tokens-over-time2" border="0"></a>

```
plot_tokens_over_time(data, plot = "weekday")
```

<a href="https://ibb.co/xFccHqM"><img src="https://i.ibb.co/FHffV4B/plot-tokens-over-time3.png" alt="plot-tokens-over-time3" border="0"></a>

```
plot_tokens_over_time(data, plot = "hours")
```

<a href="https://imgbb.com/"><img src="https://i.ibb.co/VwGSJfd/plot-tokens-over-time4.png" alt="plot-tokens-over-time4" border="0"></a>

### Wordcloud
Wordcloud of sent tokens, for all chat participants overall - and seperately for each participant.
```
plot_wordcloud(data)
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/nR5znxB/plot-wordcloud1.png" alt="plot-wordcloud1" border="0"></a>

```
plot_wordcloud(data, comparison = TRUE)
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/5TSfhNy/plot-wordcloud2.png" alt="plot-wordcloud2" border="0"></a>

### Lexical Dispersion Plot
Occurances of keywords in the chat with. Example keyword is "Weihnachten" (Christmas).
```
plot_lexical_dispersion(data,keywords = c("weihnachten"))
```
<a href="https://ibb.co/Zm9f34B"><img src="https://i.ibb.co/fvJXb6k/plot-lexical-dispersion1.png" alt="plot-lexical-dispersion1" border="0"></a>

### Sent Links
Amount of sent Links per person and over time

```
plot_links(data, plot = "cumsum")
```
<a href="https://ibb.co/58Cc1jB"><img src="https://i.ibb.co/SnHv6mR/plot-links2.png" alt="plot-links2" border="0"></a>

```
plot_links(data, plot = "heatmap")
```
<a href="https://ibb.co/mHbgVFp"><img src="https://i.ibb.co/197jtJc/plot-links1.png" alt="plot-links1" border="0"></a>

```
plot_links(data, plot = "bar", min.occur = 10)
```
<a href="https://ibb.co/CK2q6mD"><img src="https://i.ibb.co/Ld5MrYH/plot-links5.png" alt="plot-links5" border="0"></a>

### Sent Smilies
Amount of sent Smilies per person and over time
```
plot_smilies(data, plot = "cumsum")
```

IMAGE MISSING, UPLOAD NWE VERSION

```
plot_smilies(data, plot = "bar")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/7N0zJhQ/plot-smilies2.png" alt="plot-smilies2" border="0"></a>

```
plot_smilies(data, plot = "splitbar")
```
<a href="https://ibb.co/bmd1sSQ"><img src="https://i.ibb.co/zxfsXyQ/plot-smilies3.png" alt="plot-smilies3" border="0"></a>

```
plot_smilies(data, plot = "splitbar", SmilieVec = c(":)",":>",":D",":p",":("))
```

<a href="https://ibb.co/m5NnSWJ"><img src="https://i.ibb.co/9HZxG5n/plot-smilies5.png" alt="plot-smilies5" border="0"></a>

### Sent Emojis
Amount of sent Emojis per person and over time
```
plot_emoji(data, plot = "cumsum")
```
<a href="https://ibb.co/vwwP6nr"><img src="https://i.ibb.co/C1173GX/plot-emoji1.png" alt="plot-emoji1" border="0"></a>

```
plot_emoji(data, plot = "bar", min.occur = 50)
```

<a href="https://ibb.co/0FwBr15"><img src="https://i.ibb.co/QC58Mh0/plot-emoji3.png" alt="plot-emoji3" border="0"></a>

```
plot_emoji(data, plot = "splitbar", min.occur = 50)
```
<a href="https://ibb.co/GJHc16c"><img src="https://i.ibb.co/sPWyDfy/plot-emoji4.png" alt="plot-emoji4" border="0"></a>

```
plot_emoji(data, plot = "heatmap")
```
<a href="https://ibb.co/YQb1Fm1"><img src="https://i.ibb.co/vqDT6pT/plot-emoji2.png" alt="plot-emoji2" border="0"></a>

### Location Visualization
Plotting mentioned locations by person
```
plot_location(data, add.jitter = TRUE, jitter.val = 0.05 ,mapzoom = 7)
```
<a href="https://ibb.co/MNvDLnT"><img src="https://i.ibb.co/tCGM5Bf/plot-location1.png" alt="plot-location1" border="0"></a>

### Replytimes
Plotting time it takes to respond and be responded to by person.
```
plot_reply_times(data, type = "replytime")
```
<a href="https://ibb.co/RD6Y5Wm"><img src="https://i.ibb.co/P6WFS8n/plot-replytimes1.png" alt="plot-replytimes1" border="0"></a>

```
plot_reply_times(data, type = "reactiontime")
```
<a href="https://ibb.co/3WrCXWD"><img src="https://i.ibb.co/BjzCDjX/plot-replytimes2.png" alt="plot-replytimes2" border="0"></a>

## Group Chat
The previous example chat was exported without mediafiles and only includes two people chatting. To demonstrate the visualization of different media files and the network of chat participants, we will thus use a different example file, exported from a groupchat.

### Sent Media
Amount of sent Media files per person and over time
```
plot_media(data, plot = "cumsum")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/7jZVLJf/plot-media2.png" alt="plot-media2" border="0"></a>

```
plot_media(data, plot = "bar")
```
<a href="https://ibb.co/NSm9SPd"><img src="https://i.ibb.co/L5Jx5WX/plot-media1.png" alt="plot-media1" border="0"></a>

```
plot_media(data, plot = "splitbar")
```
<a href="https://ibb.co/bXm2Wx1"><img src="https://i.ibb.co/YhQyR62/plot-media4.png" alt="plot-media4" border="0"></a>

```
plot_media(data, plot = "heatmap")
```
<a href="https://ibb.co/Kb5WLT0"><img src="https://i.ibb.co/S56BdFs/plot-media3.png" alt="plot-media3" border="0"></a>


### Interactive Network
Interactive network of chat participants. A connection represents a response to a message. Each Message is interpreted as a response to the previous message. Consecutrive messages by the same chat participant are summarized into one "session". The shown plot is a low quality GIF, the actual output is an interactive HTML object that GitHub does not permit in Readme files.
```
animate_network(data)
```
![Network Visualization](https://s7.gifyu.com/images/WANetworkAnimation.gif)

