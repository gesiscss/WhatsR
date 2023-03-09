#################### TESTING ESSENTIAL FUNCTIONS #####################

#### Testing creation of artificial chatlogs ####

# german android 24h
test_that("chatlogs: German, android, 24h", {
  test1 <- create_chatlog(
    n_messages = 200,
    n_chatters = 45,
    n_emoji = 99,
    n_links = 99,
    n_locations = 99,
    n_smilies = 99,
    n_media = 99,
    n_sdp = 50,
    time_format = "24h",
    syslang = "german",
    os = "android"
  )

  expect_equal(class(test1), "character")
  expect_equal(length(test1), 200)
})

# german ios 24 h
test_that("creating chatlogs: German, ios, 24h", {
  # german android 24h
  test1 <- create_chatlog(
    n_messages = 200,
    n_chatters = 45,
    n_emoji = 99,
    n_links = 99,
    n_locations = 99,
    n_smilies = 99,
    n_media = 99,
    n_sdp = 50,
    time_format = "24h",
    syslang = "german",
    os = "ios"
  )

  expect_equal(class(test1), "character")
  expect_equal(length(test1), 200)
})


# english android 24 h
test_that("creating chatlogs: English, android, 24h", {
  # german android 24h
  test1 <- create_chatlog(
    n_messages = 200,
    n_chatters = 45,
    n_emoji = 99,
    n_links = 99,
    n_locations = 99,
    n_smilies = 99,
    n_media = 99,
    n_sdp = 50,
    time_format = "24h",
    syslang = "english",
    os = "android"
  )

  expect_equal(class(test1), "character")
  expect_equal(length(test1), 200)
})


# english ios 24 h
test_that("creating chatlogs: English, ios, 24h", {
  # german android 24h
  test1 <- create_chatlog(
    n_messages = 200,
    n_chatters = 45,
    n_emoji = 99,
    n_links = 99,
    n_locations = 99,
    n_smilies = 99,
    n_media = 99,
    n_sdp = 50,
    time_format = "24h",
    syslang = "english",
    os = "ios"
  )

  expect_equal(class(test1), "character")
  expect_equal(length(test1), 200)
})


# german android ampm
test_that("creating chatlogs: German, android, ampm", {
  # german android 24h
  test1 <- create_chatlog(
    n_messages = 200,
    n_chatters = 45,
    n_emoji = 99,
    n_links = 99,
    n_locations = 99,
    n_smilies = 99,
    n_media = 99,
    n_sdp = 50,
    time_format = "ampm",
    syslang = "german",
    os = "android"
  )

  expect_equal(class(test1), "character")
  expect_equal(length(test1), 200)
})


# german ios ampm
test_that("creating chatlogs: German, ios, ampm", {
  # german android 24h
  test1 <- create_chatlog(
    n_messages = 200,
    n_chatters = 45,
    n_emoji = 99,
    n_links = 99,
    n_locations = 99,
    n_smilies = 99,
    n_media = 99,
    n_sdp = 50,
    time_format = "ampm",
    syslang = "german",
    os = "ios"
  )

  expect_equal(class(test1), "character")
  expect_equal(length(test1), 200)
})


# english android ampm
test_that("creating chatlogs: English, android, ampm", {
  # german android 24h
  test1 <- create_chatlog(
    n_messages = 200,
    n_chatters = 45,
    n_emoji = 99,
    n_links = 99,
    n_locations = 99,
    n_smilies = 99,
    n_media = 99,
    n_sdp = 50,
    time_format = "ampm",
    syslang = "english",
    os = "android"
  )

  expect_equal(class(test1), "character")
  expect_equal(length(test1), 200)
})


# english ios ampm
test_that("creating chatlogs: English, ios, ampm", {
  # german android 24h
  test1 <- create_chatlog(
    n_messages = 200,
    n_chatters = 45,
    n_emoji = 99,
    n_links = 99,
    n_locations = 99,
    n_smilies = 99,
    n_media = 99,
    n_sdp = 50,
    time_format = "ampm",
    syslang = "english",
    os = "ios"
  )

  expect_equal(class(test1), "character")
  expect_equal(length(test1), 200)
})


#### Testing parsing of chats with default options  ####

## 24h
test_that("Parsing Chatlogs: German, Android, 24h; default", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("GermanAndroid24H_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"))), test)
})

test_that("Parsing Chatlogs: German, Ios, 24h; default", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("GermanIos24H_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"))), test)
})

test_that("Parsing Chatlogs: English, Android, 24h; default", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("EnglishAndroid24H_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"))), test)
})

test_that("Parsing Chatlogs: English, ios, 24h; default", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("EnglishIos24H_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"))), test)
})










# am/pm
test_that("Parsing Chatlogs: German, Android, ampm; default", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("GermanAndroidAMPM_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"))), test)
})

test_that("Parsing Chatlogs: German, Ios, ampm; default", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("GermanIosAMPM_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"))), test)
})

test_that("Parsing Chatlogs: English, Android, ampm; default", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("EnglishAndroidAMPM_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"))), test)
})

test_that("Parsing Chatlogs: English, Ios, ampm; default", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  test <- readRDS(system.file("EnglishIosAMPM_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))), test)
})


##### Testing Emoji Replacement #####

test_that("Replacing Emoji function", {
  Emoji <- replace_emoji(readRDS(system.file("EmojiExample.rds", package = "WhatsR")))
  expect_identical(Emoji, "I'm on the Highway to hell!  |Emoji_Smiling_Face_with_Horns| ")
})

##### Testing summarize function #####

test_that("Chat summary function", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test1 <- hush(summarize_chat(data, excludeSM = FALSE))
  test2 <- hush(summarize_chat(data, excludeSM = TRUE))

  test_version1 <- readRDS(system.file("summarize_chat1.rds", package = "WhatsR"))
  test_version2 <- readRDS(system.file("summarize_chat2.rds", package = "WhatsR"))

  expect_identical(test1, test_version1)
  expect_identical(test2, test_version2)
})


##### Testing tailoring function #####

test_that("tailoring function", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }


  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  tailored_data <- tailor_chat(data,
    names = c("Mallory", "Alice"),
    starttime = anytime("1976-01-01 00:00"),
    endtime = anytime("2022-01-01 00:00"),
    excludeSM = TRUE
  )

  test <- readRDS(system.file("TailoredData.rds", package = "WhatsR"))
  expect_identical(test, tailored_data)

  tailored_data <- tailor_chat(data,
    names = "Dave",
    starttime = anytime("2018-01-29 12:24:03"),
    endtime = anytime("	2018-01-30 00:13:03"),
    excludeSM = TRUE
  )

  test <- readRDS(system.file("TailoredData2.rds", package = "WhatsR"))
  expect_identical(test, tailored_data)

  tailored_data <- tailor_chat(data,
    names = "Dave",
    starttime = anytime("	2018-01-29 12:24:03"),
    endtime = anytime("	2018-01-30 00:13:03"),
    excludeSM = TRUE
  )

  tailored_data <- readRDS(system.file("TailoredData3.rds", package = "WhatsR"))
  expect_identical(test, tailored_data)

  tailored_data <- tailor_chat(data,
    names = "all",
    starttime = anytime("2018-01-29 12:24:03"),
    endtime = Sys.time(),
    excludeSM = TRUE
  )

  test <- readRDS(system.file("TailoredData4.rds", package = "WhatsR"))
  expect_identical(test, tailored_data)
})


########### TESTING PLOTTING FUNCTIONS ############

# This returns a lot of warning due to the correct font not being available in the testing environment
# This should be unproblematic in actual use though
suppressWarnings(test_that("Plotting Emoji", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }


  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test_emoji1 <- hush(plot_emoji(data,
    names = "all",
    # starttime=,
    # endtime=,
    # min.occur = ,
    EmojiVec = "all",
    plot = "bar",
    EmojiSize = 10,
    FontFamily = "Times", # "Times" on Windows
    return.data = TRUE,
    excludeSM = TRUE
  ))

  test <- readRDS(system.file("test_emoji1.rds", package = "WhatsR"))
  suppressWarnings(expect_identical(test_emoji1, test))


  test_emoji2 <- hush(plot_emoji(data,
    names = "all",
    # starttime=,
    # endtime=,
    # min.occur = ,
    EmojiVec = "all",
    plot = "cumsum",
    EmojiSize = 10,
    FontFamily = "Times", # "Times" on Windows
    return.data = TRUE,
    excludeSM = TRUE
  ))

  test <- readRDS(system.file("test_emoji2.rds", package = "WhatsR"))
  expect_identical(test_emoji2, test)


  test_emoji3 <- hush(plot_emoji(data,
    names = "all",
    # starttime=,
    # endtime=,
    min.occur = 1,
    return.data = TRUE,
    EmojiVec = c("Grinning_Face_with_Smiling_Eyes"),
    plot = "heatmap",
    EmojiSize = 10,
    FontFamily = "Times",
    excludeSM = TRUE
  )) # "Times" on Windows

  test <- readRDS(system.file("test_emoji3.rds", package = "WhatsR"))
  expect_identical(test_emoji3, test)


  test_emoji4 <- hush(plot_emoji(data,
    names = "all",
    # starttime=,
    # endtime=,
    min.occur = 1,
    return.data = TRUE,
    EmojiVec = c("Grinning_Face_with_Smiling_Eyes"),
    plot = "bar",
    EmojiSize = 10,
    FontFamily = "Times",
    excludeSM = TRUE
  )) # "Times" on Windows

  test <- readRDS(system.file("test_emoji4.rds", package = "WhatsR"))
  expect_identical(test_emoji4, test)
}))


test_that("Plotting Links", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test_links1 <- plot_links(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    use.domains = TRUE,
    exclude.long = TRUE,
    length = 50,
    min.occur = 1,
    return.data = TRUE,
    LinkVec = "https://github.com/",
    plot = "bar",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_links1.rds", package = "WhatsR"))
  expect_identical(test_links1, test)



  test_links2 <- plot_links(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    use.domains = TRUE,
    exclude.long = TRUE,
    length = 50,
    min.occur = 1,
    return.data = TRUE,
    LinkVec = "all",
    plot = "cumsum",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_links2.rds", package = "WhatsR"))
  expect_identical(test_links2, test)




  test_links3 <- plot_links(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    use.domains = FALSE,
    exclude.long = TRUE,
    length = 50,
    min.occur = 1,
    return.data = TRUE,
    LinkVec = "all",
    plot = "heatmap",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_links3.rds", package = "WhatsR"))
  expect_identical(test_links3, test)



  test_links4 <- plot_links(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    use.domains = TRUE,
    exclude.long = FALSE,
    length = 50,
    min.occur = 1,
    return.data = TRUE,
    LinkVec = "all",
    plot = "splitbar",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_links4.rds", package = "WhatsR"))
  expect_identical(test_links4, test)
})



test_that("Plotting Media", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))


  test_media1 <- plot_media(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    use.type = TRUE,
    min.occur = 1,
    return.data = TRUE,
    MediaVec = "all",
    plot = "heatmap",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_media1.rds", package = "WhatsR"))
  expect_identical(test_media1, test)


  # invalid font type for hjust bullshit
  test_media2 <- plot_media(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    use.type = FALSE,
    min.occur = 1,
    return.data = TRUE,
    MediaVec = "all",
    plot = "splitbar",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_media2.rds", package = "WhatsR"))
  expect_identical(test_media2, test)


  # invalid font type for hjust bullshit
  test_media3 <- plot_media(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    use.type = TRUE,
    min.occur = 1,
    return.data = TRUE,
    MediaVec = "all",
    plot = "cumsum",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_media3.rds", package = "WhatsR"))
  expect_identical(test_media3, test)
  #

  test_media4 <- plot_media(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    use.type = TRUE,
    min.occur = 1,
    return.data = TRUE,
    MediaVec = "all",
    plot = "bar",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_media4.rds", package = "WhatsR"))
  expect_identical(test_media4, test)
})


test_that("Plotting Location", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))


  test_location1 <- hush(plot_location(data,
    return.data = TRUE,
    add.jitter = TRUE,
    jitter.val = 0.1,
    jitter.seed = 123,
    mapzoom = 10,
    mapleeway = 0.1,
    excludeSM = TRUE
  ))

  test <- readRDS(system.file("test_location1.rds", package = "WhatsR"))
  expect_identical(test_location1, test)

  test_location2 <- hush(plot_location(data,
    return.data = TRUE,
    add.jitter = FALSE,
    jitter.val = 0.1,
    jitter.seed = 567,
    mapzoom = 10,
    mapleeway = 0.1,
    excludeSM = TRUE
  ))

  test <- readRDS(system.file("test_location1.rds", package = "WhatsR"))
  expect_identical(test_location1, test)

  test_location3 <- hush(plot_location(data,
    return.data = TRUE,
    add.jitter = TRUE,
    jitter.val = 0.5,
    jitter.seed = 890,
    mapzoom = 10,
    mapleeway = 0.1,
    excludeSM = TRUE
  ))

  test <- readRDS(system.file("test_location1.rds", package = "WhatsR"))
  expect_identical(test_location1, test)

  test_location4 <- hush(plot_location(data,
    return.data = TRUE,
    add.jitter = TRUE,
    jitter.val = 0.5,
    jitter.seed = 345,
    mapzoom = 10,
    mapleeway = 0.3,
    excludeSM = TRUE
  ))

  test <- readRDS(system.file("test_location1.rds", package = "WhatsR"))
  expect_identical(test_location1, test)
})



test_that("Plotting Messages", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))

  test_messages1 <- plot_messages(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "bar",
    return.data = TRUE,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_messages1.rds", package = "WhatsR"))
  expect_identical(test_messages1, test)





  test_messages2 <- plot_messages(data,
    names = c("Carol", "Dave"),
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "bar",
    return.data = TRUE,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_messages2.rds", package = "WhatsR"))
  expect_identical(test_messages2, test)




  test_messages3 <- plot_messages(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "pie",
    return.data = TRUE,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_messages3.rds", package = "WhatsR"))
  expect_identical(test_messages3, test)




  test_messages4 <- plot_messages(data,
    names = c("Carol", "Dave"),
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "pie",
    return.data = TRUE,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_messages4.rds", package = "WhatsR"))
  expect_identical(test_messages4, test)
})


# TODO:


test_that("Plotting Replytimes", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test_replytimes1 <- plot_replytimes(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    aggregate.sessions = TRUE,
    plot = "box",
    type = "replytime",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_replytimes1.rds", package = "WhatsR"))
  expect_identical(test_replytimes1, test)


  test_replytimes2 <- plot_replytimes(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    aggregate.sessions = TRUE,
    plot = "box",
    type = "reactiontime",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_replytimes2.rds", package = "WhatsR"))
  expect_identical(test_replytimes2, test)


  test_replytimes3 <- plot_replytimes(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    aggregate.sessions = TRUE,
    plot = "heatmap",
    type = "replytime",
    excludeSM = TRUE
  )


  test <- readRDS(system.file("test_replytimes3.rds", package = "WhatsR"))
  expect_identical(test_replytimes3, test)

  test_replytimes4 <- plot_replytimes(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    aggregate.sessions = TRUE,
    plot = "heatmap",
    type = "reactiontime",
    excludeSM = TRUE
  )


  test <- readRDS(system.file("test_replytimes4.rds", package = "WhatsR"))
  expect_identical(test_replytimes4, test)
})



test_that("Plotting tokens", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test_tokens1 <- plot_tokens(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "bar",
    excludeSM = TRUE,
    return.data = TRUE
  )

  test <- readRDS(system.file("test_tokens1.rds", package = "WhatsR"))
  expect_identical(test_tokens1, test)

  test_tokens2 <- plot_tokens(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "cumsum",
    excludeSM = TRUE,
    return.data = TRUE
  )

  test <- readRDS(system.file("test_tokens2.rds", package = "WhatsR"))
  expect_identical(test_tokens2, test)


  test_tokens3 <- plot_tokens(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "violin",
    excludeSM = TRUE,
    return.data = TRUE
  )

  test <- readRDS(system.file("test_tokens3.rds", package = "WhatsR"))
  expect_identical(test_tokens3, test)


  test_tokens4 <- plot_tokens(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "box",
    excludeSM = TRUE,
    return.data = TRUE
  )

  test <- readRDS(system.file("test_tokens4.rds", package = "WhatsR"))
  expect_identical(test_tokens4, test)
})



test_that("Plotting tokens over time", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test_tot1 <- plot_tokens_over_time(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "alltime",
    return.data = TRUE,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_tot1.rds", package = "WhatsR"))
  expect_identical(test_tot1, test)


  test_tot2 <- plot_tokens_over_time(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "year",
    return.data = TRUE,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_tot2.rds", package = "WhatsR"))
  expect_identical(test_tot2, test)


  test_tot3 <- plot_tokens_over_time(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "day",
    return.data = TRUE,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_tot3.rds", package = "WhatsR"))
  expect_identical(test_tot3, test)


  test_tot4 <- plot_tokens_over_time(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    plot = "heatmap",
    return.data = TRUE,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_tot4.rds", package = "WhatsR"))
  expect_identical(test_tot4, test)
})


test_that("Plotting Smilies", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))


  test_smilies1 <- plot_smilies(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    min.occur = 1,
    return.data = TRUE,
    SmilieVec = "all",
    plot = "bar",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_smilies1.rds", package = "WhatsR"))
  expect_identical(test_smilies1, test)


  test_smilies2 <- plot_smilies(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    min.occur = 1,
    return.data = TRUE,
    SmilieVec = "all",
    plot = "splitbar",
    excludeSM = TRUE
  )


  test <- readRDS(system.file("test_smilies2.rds", package = "WhatsR"))
  expect_identical(test_smilies2, test)

  # TODO: fix invalid font type error
  # test_smilies3 <- plot_smilies(data,
  #                               names = "all",
  #                               starttime = anytime("1960-01-01 00:00"),
  #                               endtime = Sys.time(),
  #                               min.occur = 1,
  #                               return.data = TRUE,
  #                               SmilieVec = "all",
  #                               plot = "cumsum")
  #
  # test <- readRDS(system.file("test_smilies3.rds", package = "WhatsR"))
  # expect_identical(test_smilies3,test)

  test_smilies4 <- plot_smilies(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    min.occur = 1,
    return.data = TRUE,
    SmilieVec = "all",
    plot = "heatmap",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_smilies4.rds", package = "WhatsR"))
  expect_identical(test_smilies4, test)
})


test_that("Plotting Wordcloud", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))


  test_wc1 <- plot_wordcloud(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    remove.stops = TRUE,
    stop = "english",
    comparison = FALSE,
    return.data = TRUE,
    font.size = 10,
    min.freq = 3,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_wc1.rds", package = "WhatsR"))
  expect_identical(test_wc1, test)



  test_wc2 <- plot_wordcloud(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    remove.stops = FALSE,
    stop = "english",
    comparison = TRUE,
    return.data = TRUE,
    font.size = 15,
    min.freq = 3,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_wc2.rds", package = "WhatsR"))
  expect_identical(test_wc2, test)

  test_wc3 <- plot_wordcloud(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    remove.stops = TRUE,
    stop = "english",
    comparison = FALSE,
    return.data = TRUE,
    font.size = 20,
    min.freq = 5,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_wc3.rds", package = "WhatsR"))
  expect_identical(test_wc3, test)


  test_wc4 <- plot_wordcloud(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    remove.stops = FALSE,
    stop = "english",
    comparison = TRUE,
    return.data = TRUE,
    font.size = 10,
    min.freq = 1,
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_wc4.rds", package = "WhatsR"))
  expect_identical(test_wc4, test)
})


test_that("Plotting Network", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  # TODO: why do we need this here? Should be loaded via package requirements?
  library(dplyr)

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test_network1 <- plot_network(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    collapse_sessions = FALSE,
    edgetype = "n",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_network1.rds", package = "WhatsR"))
  expect_identical(test_network1, test)


  test_network2 <- plot_network(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    collapse_sessions = TRUE,
    edgetype = "TokCount",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_network2.rds", package = "WhatsR"))
  expect_identical(test_network2, test)

  test_network3 <- plot_network(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    collapse_sessions = FALSE,
    edgetype = "EmojiCount",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_network3.rds", package = "WhatsR"))
  expect_identical(test_network3, test)

  test_network4 <- plot_network(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    collapse_sessions = TRUE,
    edgetype = "SmilieCount",
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_network4.rds", package = "WhatsR"))
  expect_identical(test_network4, test)
})


test_that("Plotting Lexical Dispersion", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test_lediplo1 <- plot_lexical_dispersion(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    keywords = c("data", "consent"),
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_lediplo1.rds", package = "WhatsR"))
  expect_identical(test_lediplo1, test)


  test_lediplo2 <- plot_lexical_dispersion(data,
    names = c("Alice", "Bob"),
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    keywords = c("data", "consent"),
    excludeSM = TRUE
  )

  test <- readRDS(system.file("test_lediplo2.rds", package = "WhatsR"))
  expect_identical(test_lediplo2, test)


  test_lediplo3 <- plot_lexical_dispersion(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    keywords = c("data", "consent", "this"),
    excludeSM = TRUE
  )


  test <- readRDS(system.file("test_lediplo3.rds", package = "WhatsR"))
  expect_identical(test_lediplo3, test)

  test_ledipl4 <- plot_lexical_dispersion(data,
    names = "all",
    starttime = anytime("1960-01-01 00:00"),
    endtime = Sys.time(),
    return.data = TRUE,
    keywords = c("data", "consent"),
    excludeSM = TRUE
  )


  test <- readRDS(system.file("test_ledipl4.rds", package = "WhatsR"))
  expect_identical(test_ledipl4, test)
})
