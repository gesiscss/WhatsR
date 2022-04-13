#################### TESTING ESSENTIAL FUNCTIONS #####################

#### Testing creation of artificial chatlogs ####

# german android 24h
test_that("chatlogs: German, android, 24h",{

  test1 <- create_chatlog(n_messages = 200,
                 n_chatters = 45,
                 n_emoji=99,
                 n_links = 99,
                 n_locations = 99,
                 n_smilies=99,
                 n_media= 99,
                 n_sdp = 50,
                 time_format="24h",
                 syslang = "german",
                 os= "android")

  expect_equal(class(test1), "character")
  expect_equal(length(test1),200)

})

# german ios 24 h | WARNS
test_that("creating chatlogs: German, ios, 24h",{

  # german android 24h
  test1 <- create_chatlog(n_messages = 200,
                          n_chatters = 45,
                          n_emoji=99,
                          n_links = 99,
                          n_locations = 99,
                          n_smilies=99,
                          n_media= 99,
                          n_sdp = 50,
                          time_format="24h",
                          syslang = "german",
                          os= "ios")

  expect_equal(class(test1), "character")
  expect_equal(length(test1),200)

})


# english android 24 h | WARNS
test_that("creating chatlogs: English, android, 24h",{

  # german android 24h
  test1 <- create_chatlog(n_messages = 200,
                          n_chatters = 45,
                          n_emoji=99,
                          n_links = 99,
                          n_locations = 99,
                          n_smilies=99,
                          n_media= 99,
                          n_sdp = 50,
                          time_format="24h",
                          syslang = "english",
                          os= "android")

  expect_equal(class(test1), "character")
  expect_equal(length(test1),200)

})


# english ios 24 h | WARNS
test_that("creating chatlogs: English, ios, 24h",{

  # german android 24h
  test1 <- create_chatlog(n_messages = 200,
                          n_chatters = 45,
                          n_emoji=99,
                          n_links = 99,
                          n_locations = 99,
                          n_smilies=99,
                          n_media= 99,
                          n_sdp = 50,
                          time_format="24h",
                          syslang = "english",
                          os= "ios")

  expect_equal(class(test1), "character")
  expect_equal(length(test1),200)

})


# german android ampm
test_that("creating chatlogs: German, android, ampm",{

  # german android 24h
  test1 <- create_chatlog(n_messages = 200,
                          n_chatters = 45,
                          n_emoji=99,
                          n_links = 99,
                          n_locations = 99,
                          n_smilies=99,
                          n_media= 99,
                          n_sdp = 50,
                          time_format="ampm",
                          syslang = "german",
                          os= "android")

  expect_equal(class(test1), "character")
  expect_equal(length(test1),200)

})


# german ios ampm | WARNS
test_that("creating chatlogs: German, ios, ampm",{

  # german android 24h
  test1 <- create_chatlog(n_messages = 200,
                          n_chatters = 45,
                          n_emoji=99,
                          n_links = 99,
                          n_locations = 99,
                          n_smilies=99,
                          n_media= 99,
                          n_sdp = 50,
                          time_format="ampm",
                          syslang = "german",
                          os= "ios")

  expect_equal(class(test1), "character")
  expect_equal(length(test1),200)

})


# english android ampm | WARNS
test_that("creating chatlogs: English, android, ampm",{

  # german android 24h
  test1 <- create_chatlog(n_messages = 200,
                          n_chatters = 45,
                          n_emoji=99,
                          n_links = 99,
                          n_locations = 99,
                          n_smilies=99,
                          n_media= 99,
                          n_sdp = 50,
                          time_format="ampm",
                          syslang = "english",
                          os= "android")

  expect_equal(class(test1), "character")
  expect_equal(length(test1),200)

})


# english ios ampm | WARNS
test_that("creating chatlogs: English, ios, ampm",{

  # german android 24h
  test1 <- create_chatlog(n_messages = 200,
                          n_chatters = 45,
                          n_emoji=99,
                          n_links = 99,
                          n_locations = 99,
                          n_smilies=99,
                          n_media= 99,
                          n_sdp = 50,
                          time_format="ampm",
                          syslang = "english",
                          os= "ios")

  expect_equal(class(test1), "character")
  expect_equal(length(test1),200)

})


#### Testing parsing of chats with default options  ####

## 24h
test_that("Parsing Chatlogs: German, Android, 24h; default", {

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("GermanAndroid24H_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"))),test)

})

test_that("Parsing Chatlogs: German, Ios, 24h; default", {

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("GermanIos24H_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"))),test)

})

test_that("Parsing Chatlogs: English, Android, 24h; default", {

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("EnglishAndroid24H_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"))),test)

})

test_that("Parsing Chatlogs: English, ios, 24h; default", {

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("EnglishIos24H_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"))),test)

})





# am/pm
test_that("Parsing Chatlogs: German, Android, ampm; default", {

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("GermanAndroidAMPM_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"))),test)

})

test_that("Parsing Chatlogs: German, Ios, ampm; default", {

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("GermanIosAMPM_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"))),test)

})

test_that("Parsing Chatlogs: English, Android, ampm; default", {

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("EnglishAndroidAMPM_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"))),test)

})

test_that("Parsing Chatlogs: English, Ios, ampm; default", {

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("EnglishIosAMPM_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))),test)

})


##### Testing Emoji Replacement #####

test_that("Replacing Emoji function", {

  Emoji <- replace_emoji(readRDS(system.file("EmojiExample.rds", package = "WhatsR")))
  expect_identical(Emoji,"I'm on the Highway to hell!  |Emoji_Smiling_Face_with_Horns| ")

})

##### Testing summarize function #####

test_that("Chat summary function", {

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))
  hush(summarize_chat(data))
  hush(summarize_chat(data, excludeSM = TRUE))

})


##### Testing tailoring function #####

test_that("tailoring function", {

  library(anytime)

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))

  tailored_data <- tailor_chat(data,
                               names = c("Mallory","Alice"),
                               starttime = anytime("1976-01-01 00:00"),
                               endtime=anytime("2022-01-01 00:00"))

  test <- readRDS(system.file("TailoredData.rds", package = "WhatsR"))
  expect_identical(test,tailored_data)

  # argument names and exlcudeSM are contradicting each other
  test <- tailor_chat(data,
              names = "Dave",
              starttime = anytime("	2018-01-29 12:24:03"),
              endtime=anytime("	2018-01-30 00:13:03"),
              excludeSM = FALSE)

  test <- tailor_chat(data,
                      names = "Dave",
                      starttime = anytime("	2018-01-29 12:24:03"),
                      endtime=anytime("	2018-01-30 00:13:03"),
                      excludeSM = TRUE)

  test <- tailor_chat(data,
                      names = "all",
                      starttime = anytime("	2018-01-29 12:24:03"),
                      endtime=Sys.time(),
                      excludeSM = FALSE)



})


########### TESTING PLOTTING FUNCTIONS ############


test_that("Plotting Emoji",{

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))

  hush(plot_emoji(data,
                  names = "all",
                  # starttime=,
                  # endtime=,
                  # min.occur = ,
                  # return.data =,
                  EmojiVec = "all",
                  plot= "bar",
                  EmojiSize = 10,
                  FontFamily = "Times",
                  return.data=TRUE))

  # hush(plot_emoji(data,
  #                 names = "all",
  #                 # starttime=,
  #                 # endtime=,
  #                 # min.occur = ,
  #                 return.data = TRUE,
  #                 EmojiVec = "all",
  #                 plot= "bar",
  #                 EmojiSize = 10,
  #                 FontFamily = "Times"))

  # hush(plot_emoji(data,
  #                 names = "all",
  #                 # starttime=,
  #                 # endtime=,
  #                 min.occur = 1,
  #                 return.data = TRUE,
  #                 EmojiVec = c("Emoji_Grinning_Face_with_Smiling_Eyes"),
  #                 plot= "bar",
  #                 EmojiSize = 10,
  #                 FontFamily = "Times"))



})

test_that("Plotting Links",{

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))

  plot_links(data)
  plot_links(data, plot= "cumsum")
  plot_links(data, plot= "heatmap")
  plot_links(data, plot= "bar")
  plot_links(data, plot= "splitbar")

  plot_links(data, plot= "cumsum", use.domains = FALSE)
  plot_links(data, plot= "cumsum", exclude.long = FALSE)
  plot_links(data, plot= "cumsum", length=20)
  plot_links(data, plot= "cumsum", return.data=TRUE)



})

test_that("Plotting Media",{

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))


})

test_that("Plotting Location",{

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))


})

test_that("Plotting Messages",{

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))


})

test_that("Plotting Replytimes",{

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))


})

test_that("Plotting tokens",{

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))


})

test_that("Plotting tokens over time",{

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))


})

test_that("Plotting Worcloud",{

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))


})

test_that("Plotting Network",{

  hush=function(code){
    sink("NULL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))


})






