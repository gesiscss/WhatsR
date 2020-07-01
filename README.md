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
 - A column containing only Emoticons (e.g. ":-)") that were used in the message
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

# Examples

The package also includes some functions to compute additional metrics and visualize them. We will provide some basic examples for chats with two participants and for group chats with multiple participants. The used chat is a private chat that was parsed with the `anon = TRUE` parameter to exclude participant names. The dataframe is called `data`. All plotting functions include additional parameters to restrict the range of the data

### Basic statistics
Some basic statistics about the nature of the chat.

```
BasicStats(data)
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
### Message Distribution
Destribution of sent Messages.
```
MsgDsp(data)
```
<a href="https://ibb.co/sqvrFXt"><img src="https://i.ibb.co/7KJFG0b/Message-Amount.png" alt="Message-Amount" border="0" /></a>

### Token Distribution
Destribution of sent Tokens (words).
```
TokDisp(data)
```

### Tokens per Person
Destribution of sent Tokens per Person.
```
TokenSummary(data)
```
<a href="https://ibb.co/Dwjcc1S"><img src="https://i.ibb.co/0Yz44nd/Token-Distribution.png" alt="Token-Distribution" border="0" /></a>

### Tokens over Time
Destribution of sent Tokens per Person
over time
```
TokensOverTime(data)
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/LvSn39m/Tokens-Overtime.png" alt="Tokens-Overtime" border="0" /></a>

```
TokensOverTime(data, plot = "heatmap")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/wWzKcVz/Tokens-Over-Time-Heatmap.png" alt="Tokens-Over-Time-Heatmap" border="0" /></a>

### Wordcloud
Wordcloud of sent tokens, for all chat participants overall and seperately for each participant.
```
WAwordcloud(data)
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/Qv032wS/Word-Cloud1.png" alt="Word-Cloud1" border="0" /></a>

```
WAwordcloud(data, comparison = TRUE)
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/0yXYRMj/Wordcloud.png" alt="Wordcloud" border="0" /></a>

### Lexical Dispersion Plot
Occurances of keywords in the chat with. Example keyword is "Weihnachten" (Christmas).
```
LeDiPlo(data,keywords = c("weihnachten"))
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/0CHszty/Le-Di-Plo-Christmas.png" alt="Le-Di-Plo-Christmas" border="0" /></a>

### Sent Links
Amount of sent Links per person and over time
```
LinkViz(data)
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/qmfnyyJ/Links-Heatmap.png" alt="Links-Heatmap" border="0" /></a>

```
LinkViz(data, plot = "cumsum")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/JrgMdrJ/Links-Cumulative.png" alt="Links-Cumulative" border="0" /></a>

```
LinkViz(data, plot = "bar", min.occur=10)
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/r76gHjY/Links-Bar-Domains.png" alt="Links-Bar-Domains" border="0" /></a>

### Sent Smilies
Amount of sent Smilies per person and over time
```
SmilieViz(data, plot = "cumsum")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/Yjwws0K/Smilies-Cumulative.png" alt="Smilies-Cumulative" border="0" /></a>

```
SmilieViz(data, plot = "bar")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/rbnDjP1/Smilies-Splitbar.png" alt="Smilies-Splitbar" border="0" /></a>

```
SmilieViz(data, plot = "splitbar", SmilieVec = c(":)",":>",":D",":p",":("))
```

<a href="https://imgbb.com/"><img src="https://i.ibb.co/p2P3kqY/Smilies-Per-Person.png" alt="Smilies-Per-Person" border="0" /></a>

### Sent Emojis
Amount of sent Emojis per person and over time
```
EmojiViz(data, plot = "cumsum")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/4ZgKt8m/Emoji-Cumulative.png" alt="Emoji-Cumulative" border="0" /></a>

```
EmojiViz(data, plot = "bar", min.occur = 50)
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/rpJ5DVb/Emojidistribution50occur.png" alt="Emojidistribution50occur" border="0" /></a>

```
EmojiViz(data, plot = "splitbar", min.occur = 50
```
<a href="https://ibb.co/Rbm6L58"><img src="https://i.ibb.co/pw8KCmq/Emoji-By-Person50occur.png" alt="Emoji-By-Person50occur" border="0" /></a>

```
EmojiViz(data, plot = "heatmap", EmojiVec = "Emoji_Grinning_Face_with_Smiling_Eyes")
```
<a href="https://ibb.co/Sx1FdDN"><img src="https://i.ibb.co/hMn3Zxs/Emoji-Heatmap.png" alt="Emoji-Heatmap" border="0" /></a>

### Location Visualization
Plotting mentioned locations by person
```
LocViz(data, add.jitter = TRUE, jitter.val = 0.05 ,mapzoom = 7)
```
<a href="https://ibb.co/dBH3xxq"><img src="https://i.ibb.co/cJspVVH/Location-By-Senders.png" alt="Location-By-Senders" border="0" /></a>

### Replytimes
Plotting time it takes to respond and be responded to by person.
```
ReplyTimeViz(data, type = "replytime")
```
<a href="https://ibb.co/S5zNypj"><img src="https://i.ibb.co/FmF63tr/Response-Time-Dist.png" alt="Response-Time-Dist" border="0" /></a>

```
ReplyTimeViz(data, type = "reactiontime")
```
<a href="https://ibb.co/MSk3TD9"><img src="https://i.ibb.co/02ydNjQ/Reaction-Time-Dist.png" alt="Reaction-Time-Dist" border="0" /></a>

## Group Chat

### Sent Media
Amount of sent Media files per person and over time
```
MediaViz(data, plot = "bar")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/XFdZ9VC/Media-Files-Bar.png" alt="Media-Files-Bar" border="0" /></a>

```
MediaViz(data, plot = "cumsum")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/5kqCJpG/Mediacumsum.png" alt="Mediacumsum" border="0" /></a>

```
MediaViz(data, plot = "heatmap")
```
<a href="https://ibb.co/wMRtM5v"><img src="https://i.ibb.co/FgKpgRP/Media-Heatmap.png" alt="Media-Heatmap" border="0" /></a>

```
MediaViz(data, plot = "splitbar")
```
<a href="https://imgbb.com/"><img src="https://i.ibb.co/8bSBXR5/Media-Splitbar.png" alt="Media-Splitbar" border="0" /></a>

### Interactive Network
Interactive network of chat participants. A connection represents a response to a message. Each Message is interpreted as a response to the previous message. Consecutrive messages by the same chat participant are summarized into one "session". The shown plot is a low quality GIF, the actual output is an interactive HTML object that GitHub does not permit in Readme files.
```
WaNetworkAnimation(data)
```
![Network Visualization](https://s7.gifyu.com/images/WANetworkAnimation.gif)


