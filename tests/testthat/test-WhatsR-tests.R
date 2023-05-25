#################### TESTING ESSENTIAL FUNCTIONS #####################


#### Testing updating of emoji dictionary

test_that("Updating emoji dictionary",{

  # downloading emoji dictionary
  emoji_dictionary <- download_emoji(pages = c("https://emojipedia.org/activity/"))

  # testing for valid dataframe
  expect_equal(class(emoji_dictionary), "data.frame")

  # testing if columns are contained
  expect_named(emoji_dictionary,c("R.native","Desc","OriginalOrder"))


})

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
    language = "german",
    os = "android",
    path = NA
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
    language = "german",
    os = "ios",
    path = NA
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
    language = "english",
    os = "android",
    path = NA
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
    language = "english",
    os = "ios",
    path = NA
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
    language = "german",
    os = "android",
    path = NA
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
    language = "german",
    os = "ios",
    path = NA
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
    language = "english",
    os = "android",
    path = NA
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
    language = "english",
    os = "ios",
    path = NA
  )

  expect_equal(class(test1), "character")
  expect_equal(length(test1), 200)
})
#
#
#### Testing parsing of chats with default options  ####

## 24h
test_that("Parsing Chatlogs: German, Android, 24h; default", {

  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  # test <- parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"), anonimize = "add")
  # saveRDS(test,"GermanAndroid24H_default.rds", version = 2)

  # load and check file
  test <- readRDS(system.file("GermanAndroid24H_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"))), test) # creates warning
})

test_that("Parsing Chatlogs: German, Ios, 24h; default", {

  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  # test <- parse_chat(system.file("germanios24h.txt", package = "WhatsR"), anonimize = "add")
  # saveRDS(test,"GermanIos24H_default.rds", version = 2)

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

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  # test <- parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"), anonimize = "add")
  # saveRDS(test,"EnglishAndroid24H_default.rds", version = 2)

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

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  # test <- parse_chat(system.file("englishios24h.txt", package = "WhatsR"), anonimize = "add")
  # saveRDS(test,"EnglishIos24H_default.rds", version = 2)

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

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  # test <- parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"), anonimize = "add")
  # saveRDS(test,"GermanAndroidAMPM_default.rds", version = 2)

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

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  # test <- parse_chat(system.file("germaniosampm.txt", package = "WhatsR"), anonimize = "add")
  # saveRDS(test,"GermanIosAMPM_default.rds", version = 2)

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

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  # test <- parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"), anonimize = "add")
  # saveRDS(test,"EnglishAndroidAMPM_default.rds", version = 2)

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

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  # test <- parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = "add")
  # saveRDS(test,"EnglishIosAMPM_default.rds", version = 2)

  # Problem with start_newline
  test <- readRDS(system.file("EnglishIosAMPM_default.rds", package = "WhatsR"))
  expect_identical(hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"))), test)
})


##### Testing summarize function #####

test_that("Chat summary function", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  # Timport data
  data <- hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR")))

  test1 <- hush(summarize_chat(data, exclude_sm = FALSE))
  test2 <- hush(summarize_chat(data, exclude_sm = TRUE))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test1,"summarize_chat1.rds",version = 2)
  #saveRDS(test2,"summarize_chat2.rds",version = 2)

  test_version1 <- readRDS(system.file("summarize_chat1.rds", package = "WhatsR"))
  test_version2 <- readRDS(system.file("summarize_chat2.rds", package = "WhatsR"))

  expect_identical(test1, test_version1)
  expect_identical(test2, test_version2)
})

##### Testing anonymization

test_that("Anoynmization & Consent", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  # all data types, anon = add, consent = NA
  data_e_ios_24_add_NA <- hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"), anonimize = "add"))
  data_e_android_24_add_NA <- hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"), anonimize = "add"))
  data_e_ios_ampm_add_NA <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = "add"))
  data_e_android_ampm_add_NA <- hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"), anonimize = "add"))

  data_g_ios_24_add_NA <- hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"), anonimize = "add"))
  data_g_android_24_add_NA <- hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"), anonimize = "add"))
  data_g_ios_ampm_add_NA <- hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"), anonimize = "add"))
  data_g_android_ampm_add_NA <- hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"), anonimize = "add"))



  #TESTS

  # number of columns = 19
  expect_identical(dim(data_e_ios_24_add_NA)[2], as.integer(19))
  expect_identical(dim(data_e_android_24_add_NA)[2], as.integer(19))
  expect_identical(dim(data_e_ios_ampm_add_NA)[2], as.integer(19))
  expect_identical(dim(data_e_android_ampm_add_NA)[2], as.integer(19))

  expect_identical(dim(data_g_ios_24_add_NA)[2], as.integer(19))
  expect_identical(dim(data_g_android_24_add_NA)[2], as.integer(19))
  expect_identical(dim(data_g_ios_ampm_add_NA)[2], as.integer(19))
  expect_identical(dim(data_g_android_ampm_add_NA)[2], as.integer(19))

  # number of rows = 50
  expect_identical(dim(data_e_ios_24_add_NA)[1], as.integer(50))
  expect_identical(dim(data_e_android_24_add_NA)[1], as.integer(50))
  expect_identical(dim(data_e_ios_ampm_add_NA)[1], as.integer(50))
  expect_identical(dim(data_e_android_ampm_add_NA)[1], as.integer(50))

  expect_identical(dim(data_g_ios_24_add_NA)[1], as.integer(50))
  expect_identical(dim(data_g_android_24_add_NA)[1], as.integer(50))
  expect_identical(dim(data_g_ios_ampm_add_NA)[1], as.integer(50))
  expect_identical(dim(data_g_android_ampm_add_NA)[1], as.integer(50))

  # column names
  add_cols <- c("DateTime","Sender","Sender_anon","Message","Flat","TokVec","URL","URL_anon","Media","Media_anon","Location","Location_anon","Emoji","EmojiDescriptions","Smilies","SystemMessage","TokCount","TimeOrder","DisplayOrder")
  expect_identical(sum(colnames(data_e_ios_24_add_NA) == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_e_android_24_add_NA)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_e_ios_ampm_add_NA)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_e_android_ampm_add_NA)  == add_cols), as.integer(19))

  expect_identical(sum(colnames(data_g_ios_24_add_NA)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_g_android_24_add_NA)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_g_ios_ampm_add_NA)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_g_android_ampm_add_NA)  == add_cols), as.integer(19))

#####

  # all data types, anon = TRUE, consent = NA
  data_e_ios_24_TRUE_NA <- hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"), anonimize = TRUE))
  data_e_android_24_TRUE_NA <- hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"), anonimize = TRUE))
  data_e_ios_ampm_TRUE_NA <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = TRUE))
  data_e_android_ampm_TRUE_NA <- hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"), anonimize = TRUE))

  data_g_ios_24_TRUE_NA <- hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"), anonimize = TRUE))
  data_g_android_24_TRUE_NA <- hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"), anonimize = TRUE))
  data_g_ios_ampm_TRUE_NA <- hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"), anonimize = TRUE))
  data_g_android_ampm_TRUE_NA <- hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"), anonimize = TRUE))



  #TESTS

  # number of columns = 11
  expect_identical(dim(data_e_ios_24_TRUE_NA)[2], as.integer(11))
  expect_identical(dim(data_e_android_24_TRUE_NA)[2], as.integer(11))
  expect_identical(dim(data_e_ios_ampm_TRUE_NA)[2], as.integer(11))
  expect_identical(dim(data_e_android_ampm_TRUE_NA)[2], as.integer(11))

  expect_identical(dim(data_g_ios_24_TRUE_NA)[2], as.integer(11))
  expect_identical(dim(data_g_android_24_TRUE_NA)[2], as.integer(11))
  expect_identical(dim(data_g_ios_ampm_TRUE_NA)[2], as.integer(11))
  expect_identical(dim(data_g_android_ampm_TRUE_NA)[2], as.integer(11))

  # number of rows = 50
  expect_identical(dim(data_e_ios_24_TRUE_NA)[1], as.integer(50))
  expect_identical(dim(data_e_android_24_TRUE_NA)[1], as.integer(50))
  expect_identical(dim(data_e_ios_ampm_TRUE_NA)[1], as.integer(50))
  expect_identical(dim(data_e_android_ampm_TRUE_NA)[1], as.integer(50))

  expect_identical(dim(data_g_ios_24_TRUE_NA)[1], as.integer(50))
  expect_identical(dim(data_g_android_24_TRUE_NA)[1], as.integer(50))
  expect_identical(dim(data_g_ios_ampm_TRUE_NA)[1], as.integer(50))
  expect_identical(dim(data_g_android_ampm_TRUE_NA)[1], as.integer(50))

  # column names
  add_cols <- c("DateTime","Sender","URL","Media","Location","Emoji","EmojiDescriptions","Smilies","TokCount","TimeOrder","DisplayOrder")
  expect_identical(sum(colnames(data_e_ios_24_TRUE_NA) == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_e_android_24_TRUE_NA)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_e_ios_ampm_TRUE_NA)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_e_android_ampm_TRUE_NA)  == add_cols), as.integer(11))

  expect_identical(sum(colnames(data_g_ios_24_TRUE_NA)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_g_android_24_TRUE_NA)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_g_ios_ampm_TRUE_NA)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_g_android_ampm_TRUE_NA)  == add_cols), as.integer(11))



#####



  # all data types, anon = FALSE, consent = NA
  data_e_ios_24_FALSE_NA <- hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"), anonimize = FALSE))
  data_e_android_24_FALSE_NA <- hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"), anonimize = FALSE))
  data_e_ios_ampm_FALSE_NA <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = FALSE))
  data_e_android_ampm_FALSE_NA <- hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"), anonimize = FALSE))

  data_g_ios_24_FALSE_NA <- hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"), anonimize = FALSE))
  data_g_android_24_FALSE_NA <- hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"), anonimize = FALSE))
  data_g_ios_ampm_FALSE_NA <- hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"), anonimize = FALSE))
  data_g_android_ampm_FALSE_NA <- hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"), anonimize = FALSE))


  #TESTS

  # number of columns = 15
  expect_identical(dim(data_e_ios_24_FALSE_NA)[2], as.integer(15))
  expect_identical(dim(data_e_android_24_FALSE_NA)[2], as.integer(15))
  expect_identical(dim(data_e_ios_ampm_FALSE_NA)[2], as.integer(15))
  expect_identical(dim(data_e_android_ampm_FALSE_NA)[2], as.integer(15))

  expect_identical(dim(data_g_ios_24_FALSE_NA)[2], as.integer(15))
  expect_identical(dim(data_g_android_24_FALSE_NA)[2], as.integer(15))
  expect_identical(dim(data_g_ios_ampm_FALSE_NA)[2], as.integer(15))
  expect_identical(dim(data_g_android_ampm_FALSE_NA)[2], as.integer(15))

  # number of rows = 50
  expect_identical(dim(data_e_ios_24_FALSE_NA)[1], as.integer(50))
  expect_identical(dim(data_e_android_24_FALSE_NA)[1], as.integer(50))
  expect_identical(dim(data_e_ios_ampm_FALSE_NA)[1], as.integer(50))
  expect_identical(dim(data_e_android_ampm_FALSE_NA)[1], as.integer(50))

  expect_identical(dim(data_g_ios_24_FALSE_NA)[1], as.integer(50))
  expect_identical(dim(data_g_android_24_FALSE_NA)[1], as.integer(50))
  expect_identical(dim(data_g_ios_ampm_FALSE_NA)[1], as.integer(50))
  expect_identical(dim(data_g_android_ampm_FALSE_NA)[1], as.integer(50))

  # column names
  add_cols <- c("DateTime","Sender","Message","Flat","TokVec","URL","Media","Location","Emoji","EmojiDescriptions","Smilies","SystemMessage","TokCount","TimeOrder","DisplayOrder")
  expect_identical(sum(colnames(data_e_ios_24_FALSE_NA) == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_e_android_24_FALSE_NA)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_e_ios_ampm_FALSE_NA)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_e_android_ampm_FALSE_NA)  == add_cols), as.integer(15))

  expect_identical(sum(colnames(data_g_ios_24_FALSE_NA)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_g_android_24_FALSE_NA)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_g_ios_ampm_FALSE_NA)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_g_android_ampm_FALSE_NA)  == add_cols), as.integer(15))




####



  # all data types, anon = add, consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."
  data_e_ios_24_add_match <- hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"), anonimize = "add",consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))
  data_e_android_24_add_match <- hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"), anonimize = "add",consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))
  data_e_ios_ampm_add_match <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = "add",consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))
  data_e_android_ampm_add_match <- hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"), anonimize = "add",consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))

  data_g_ios_24_add_match <- hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"), anonimize = "add",consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))
  data_g_android_24_add_match <- hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"), anonimize = "add",consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))
  data_g_ios_ampm_add_match <- hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"), anonimize = "add",consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))
  data_g_android_ampm_add_match <- hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"), anonimize = "add",consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))




  #TESTS

  # number of columns = 19
  expect_identical(dim(data_e_ios_24_add_match)[2], as.integer(19))
  expect_identical(dim(data_e_android_24_add_match)[2], as.integer(19))
  expect_identical(dim(data_e_ios_ampm_add_match)[2], as.integer(19))
  expect_identical(dim(data_e_android_ampm_add_match)[2], as.integer(19))

  expect_identical(dim(data_g_ios_24_add_match)[2], as.integer(19))
  expect_identical(dim(data_g_android_24_add_match)[2], as.integer(19))
  expect_identical(dim(data_g_ios_ampm_add_match)[2], as.integer(19))
  expect_identical(dim(data_g_android_ampm_add_match)[2], as.integer(19))

  # number of rows = 45
  expect_identical(dim(data_e_ios_24_add_match)[1], as.integer(45))
  expect_identical(dim(data_e_android_24_add_match)[1], as.integer(45))
  expect_identical(dim(data_e_ios_ampm_add_match)[1], as.integer(45))
  expect_identical(dim(data_e_android_ampm_add_match)[1], as.integer(45))

  expect_identical(dim(data_g_ios_24_add_match)[1], as.integer(45))
  expect_identical(dim(data_g_android_24_add_match)[1], as.integer(45))
  expect_identical(dim(data_g_ios_ampm_add_match)[1], as.integer(45))
  expect_identical(dim(data_g_android_ampm_add_match)[1], as.integer(45))

  # column names
  add_cols <- c("DateTime","Sender","Sender_anon","Message","Flat","TokVec","URL","URL_anon","Media","Media_anon","Location","Location_anon","Emoji","EmojiDescriptions","Smilies","SystemMessage","TokCount","TimeOrder","DisplayOrder")
  expect_identical(sum(colnames(data_e_ios_24_add_match) == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_e_android_24_add_match)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_e_ios_ampm_add_match)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_e_android_ampm_add_match)  == add_cols), as.integer(19))

  expect_identical(sum(colnames(data_g_ios_24_add_match)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_g_android_24_add_match)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_g_ios_ampm_add_match)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_g_android_ampm_add_match)  == add_cols), as.integer(19))


######



  # all data types, anon = TRUE, consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."
  data_e_ios_24_TRUE_match <- hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"), anonimize = TRUE,consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))
  data_e_android_24_TRUE_match <- hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"), anonimize = TRUE,consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))
  data_e_ios_ampm_TRUE_match <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = TRUE,consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))
  data_e_android_ampm_TRUE_match <- hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"), anonimize = TRUE,consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))

  data_g_ios_24_TRUE_match <- hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"), anonimize = TRUE,consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))
  data_g_android_24_TRUE_match <- hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"), anonimize = TRUE,consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))
  data_g_ios_ampm_TRUE_match <- hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"), anonimize = TRUE,consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))
  data_g_android_ampm_TRUE_match <- hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"), anonimize = TRUE,consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))




  #TESTS

  # number of columns = 11
  expect_identical(dim(data_e_ios_24_TRUE_match)[2], as.integer(11))
  expect_identical(dim(data_e_android_24_TRUE_match)[2], as.integer(11))
  expect_identical(dim(data_e_ios_ampm_TRUE_match)[2], as.integer(11))
  expect_identical(dim(data_e_android_ampm_TRUE_match)[2], as.integer(11))

  expect_identical(dim(data_g_ios_24_TRUE_match)[2], as.integer(11))
  expect_identical(dim(data_g_android_24_TRUE_match)[2], as.integer(11))
  expect_identical(dim(data_g_ios_ampm_TRUE_match)[2], as.integer(11))
  expect_identical(dim(data_g_android_ampm_TRUE_match)[2], as.integer(11))

  # number of rows = 45
  expect_identical(dim(data_e_ios_24_TRUE_match)[1], as.integer(45))
  expect_identical(dim(data_e_android_24_TRUE_match)[1], as.integer(45))
  expect_identical(dim(data_e_ios_ampm_TRUE_match)[1], as.integer(45))
  expect_identical(dim(data_e_android_ampm_TRUE_match)[1], as.integer(45))

  expect_identical(dim(data_g_ios_24_TRUE_match)[1], as.integer(45))
  expect_identical(dim(data_g_android_24_TRUE_match)[1], as.integer(45))
  expect_identical(dim(data_g_ios_ampm_TRUE_match)[1], as.integer(45))
  expect_identical(dim(data_g_android_ampm_TRUE_match)[1], as.integer(45))

  # column names
  add_cols <- c("DateTime","Sender","URL","Media","Location","Emoji","EmojiDescriptions","Smilies","TokCount","TimeOrder","DisplayOrder")
  expect_identical(sum(colnames(data_e_ios_24_TRUE_match) == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_e_android_24_TRUE_match)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_e_ios_ampm_TRUE_match)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_e_android_ampm_TRUE_match)  == add_cols), as.integer(11))

  expect_identical(sum(colnames(data_g_ios_24_TRUE_match)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_g_android_24_TRUE_match)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_g_ios_ampm_TRUE_match)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_g_android_ampm_TRUE_match)  == add_cols), as.integer(11))



##### RUN BELOW


  # all data types, anon = FALSE, consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."
  data_e_ios_24_FALSE_match <- hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"), anonimize = FALSE,consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))
  data_e_android_24_FALSE_match <- hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"), anonimize = FALSE,consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))
  data_e_ios_ampm_FALSE_match <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = FALSE,consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))
  data_e_android_ampm_FALSE_match <- hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"), anonimize = FALSE,consent = "I hereby consent to donate anonimized metainformation of this conversation for research purposes. No personal identifiable information about myself will be saved. I am aware that my data will be used for research about data donation behaviors and will be made available in anonomized form to other researchers and used in aggregated form in research publications."))

  data_g_ios_24_FALSE_match <- hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"), anonimize = FALSE,consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))
  data_g_android_24_FALSE_match <- hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"), anonimize = FALSE,consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))
  data_g_ios_ampm_FALSE_match <- hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"), anonimize = FALSE,consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))
  data_g_android_ampm_FALSE_match <- hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"), anonimize = FALSE,consent = "Ich gebe hiermit meine informierte Einwilligung zur Spende von anonymisierte Metainformationen aus dieser Konversation für Forschungszwecke. Es werden keine personenbezogenen Daten über mich gespeichert. Mir ist bewusst, dass meine Daten für Forschungszwecke zum Thema Datenspendeverhalten in aggregierter Form für Publikationen genutzt werden. Meine Daten können auch anonymisert anderen Forschern bereitgestellt werden."))



  #TESTS

  # number of columns = 15
  expect_identical(dim(data_e_ios_24_FALSE_match)[2], as.integer(15))
  expect_identical(dim(data_e_android_24_FALSE_match)[2], as.integer(15))
  expect_identical(dim(data_e_ios_ampm_FALSE_match)[2], as.integer(15))
  expect_identical(dim(data_e_android_ampm_FALSE_match)[2], as.integer(15))

  expect_identical(dim(data_g_ios_24_FALSE_match)[2], as.integer(15))
  expect_identical(dim(data_g_android_24_FALSE_match)[2], as.integer(15))
  expect_identical(dim(data_g_ios_ampm_FALSE_match)[2], as.integer(15))
  expect_identical(dim(data_g_android_ampm_FALSE_match)[2], as.integer(15))

  # number of rows = 45
  expect_identical(dim(data_e_ios_24_FALSE_match)[1], as.integer(45))
  expect_identical(dim(data_e_android_24_FALSE_match)[1], as.integer(45))
  expect_identical(dim(data_e_ios_ampm_FALSE_match)[1], as.integer(45))
  expect_identical(dim(data_e_android_ampm_FALSE_match)[1], as.integer(45))

  expect_identical(dim(data_g_ios_24_FALSE_match)[1], as.integer(45))
  expect_identical(dim(data_g_android_24_FALSE_match)[1], as.integer(45))
  expect_identical(dim(data_g_ios_ampm_FALSE_match)[1], as.integer(45))
  expect_identical(dim(data_g_android_ampm_FALSE_match)[1], as.integer(45))

  # column names
  add_cols <- c("DateTime","Sender","Message","Flat","TokVec","URL","Media","Location","Emoji","EmojiDescriptions","Smilies","SystemMessage","TokCount","TimeOrder","DisplayOrder")
  expect_identical(sum(colnames(data_e_ios_24_FALSE_match) == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_e_android_24_FALSE_match)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_e_ios_ampm_FALSE_match)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_e_android_ampm_FALSE_match)  == add_cols), as.integer(15))

  expect_identical(sum(colnames(data_g_ios_24_FALSE_match)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_g_android_24_FALSE_match)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_g_ios_ampm_FALSE_match)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_g_android_ampm_FALSE_match)  == add_cols), as.integer(15))




  ####

  # all data types, anon = add, consent = "Test String not contained in any messages"
  data_e_ios_24_add_mismatch <- hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"), anonimize = "add",consent = "Test String not contained in any messages"))
  data_e_android_24_add_mismatch <- hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"), anonimize = "add",consent = "Test String not contained in any messages"))
  data_e_ios_ampm_add_mismatch <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = "add",consent = "Test String not contained in any messages"))
  data_e_android_ampm_add_mismatch <- hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"), anonimize = "add",consent = "Test String not contained in any messages"))

  data_g_ios_24_add_mismatch  <- hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"), anonimize = "add",consent = "Test String not contained in any messages"))
  data_g_android_24_add_mismatch <- hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"), anonimize = "add",consent = "Test String not contained in any messages"))
  data_g_ios_ampm_add_mismatch <- hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"), anonimize = "add",consent = "Test String not contained in any messages"))
  data_g_android_ampm_add_mismatch <- hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"), anonimize = "add",consent = "Test String not contained in any messages"))



  #TESTS

  # number of columns = 15
  expect_identical(dim(data_e_ios_24_add_mismatch)[2], as.integer(19))
  expect_identical(dim(data_e_android_24_add_mismatch)[2], as.integer(19))
  expect_identical(dim(data_e_ios_ampm_add_mismatch)[2], as.integer(19))
  expect_identical(dim(data_e_android_ampm_add_mismatch)[2], as.integer(19))

  expect_identical(dim(data_g_ios_24_add_mismatch)[2], as.integer(19))
  expect_identical(dim(data_g_android_24_add_mismatch)[2], as.integer(19))
  expect_identical(dim(data_g_ios_ampm_add_mismatch)[2], as.integer(19))
  expect_identical(dim(data_g_android_ampm_add_mismatch)[2], as.integer(19))

  # number of rows = 8
  expect_identical(dim(data_e_ios_24_add_mismatch)[1], as.integer(8))
  expect_identical(dim(data_e_android_24_add_mismatch)[1], as.integer(8))
  expect_identical(dim(data_e_ios_ampm_add_mismatch)[1], as.integer(8))
  expect_identical(dim(data_e_android_ampm_add_mismatch)[1], as.integer(8))

  expect_identical(dim(data_g_ios_24_add_mismatch)[1], as.integer(8))
  expect_identical(dim(data_g_android_24_add_mismatch)[1], as.integer(8))
  expect_identical(dim(data_g_ios_ampm_add_mismatch)[1], as.integer(8))
  expect_identical(dim(data_g_android_ampm_add_mismatch)[1], as.integer(8))

  # column names
  add_cols <- c("DateTime","Sender","Sender_anon","Message","Flat","TokVec","URL","URL_anon","Media","Media_anon","Location","Location_anon","Emoji","EmojiDescriptions","Smilies","SystemMessage","TokCount","TimeOrder","DisplayOrder")
  expect_identical(sum(colnames(data_e_ios_24_add_mismatch) == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_e_android_24_add_mismatch)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_e_ios_ampm_add_mismatch)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_e_android_ampm_add_mismatch)  == add_cols), as.integer(19))

  expect_identical(sum(colnames(data_g_ios_24_add_mismatch)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_g_android_24_add_mismatch)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_g_ios_ampm_add_mismatch)  == add_cols), as.integer(19))
  expect_identical(sum(colnames(data_g_android_ampm_add_mismatch)  == add_cols), as.integer(19))




######



  # all data types, anon = TRUE, consent = "Test String not contained in any messages"
  data_e_ios_24_TRUE_mismatch <- hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"), anonimize = TRUE,consent = "Test String not contained in any messages"))
  data_e_android_24_TRUE_mismatch <- hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"), anonimize = TRUE,consent = "Test String not contained in any messages"))
  data_e_ios_ampm_TRUE_mismatch <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = TRUE,consent = "Test String not contained in any messages"))
  data_e_android_ampm_TRUE_mismatch <- hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"), anonimize = TRUE,consent = "Test String not contained in any messages"))

  data_g_ios_24_TRUE_mismatch <- hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"), anonimize = TRUE,consent = "Test String not contained in any messages"))
  data_g_android_24_TRUE_mismatch <- hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"), anonimize = TRUE,consent = "Test String not contained in any messages"))
  data_g_ios_ampm_TRUE_mismatch <- hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"), anonimize = TRUE,consent = "Test String not contained in any messages"))
  data_g_android_ampm_TRUE_mismatch <- hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"), anonimize = TRUE,consent = "Test String not contained in any messages"))


  #TESTS

  # number of columns = 15
  expect_identical(dim(data_e_ios_24_TRUE_mismatch)[2], as.integer(11))
  expect_identical(dim(data_e_android_24_TRUE_mismatch)[2], as.integer(11))
  expect_identical(dim(data_e_ios_ampm_TRUE_mismatch)[2], as.integer(11))
  expect_identical(dim(data_e_android_ampm_TRUE_mismatch)[2], as.integer(11))

  expect_identical(dim(data_g_ios_24_TRUE_mismatch)[2], as.integer(11))
  expect_identical(dim(data_g_android_24_TRUE_mismatch)[2], as.integer(11))
  expect_identical(dim(data_g_ios_ampm_TRUE_mismatch)[2], as.integer(11))
  expect_identical(dim(data_g_android_ampm_TRUE_mismatch)[2], as.integer(11))

  # number of rows = 8
  expect_identical(dim(data_e_ios_24_TRUE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_e_android_24_TRUE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_e_ios_ampm_TRUE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_e_android_ampm_TRUE_mismatch)[1], as.integer(8))

  expect_identical(dim(data_g_ios_24_TRUE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_g_android_24_TRUE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_g_ios_ampm_TRUE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_g_android_ampm_TRUE_mismatch)[1], as.integer(8))

  # column names
  add_cols <- c("DateTime","Sender","URL","Media","Location","Emoji","EmojiDescriptions","Smilies","TokCount","TimeOrder","DisplayOrder")
  expect_identical(sum(colnames(data_e_ios_24_TRUE_mismatch) == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_e_android_24_TRUE_mismatch)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_e_ios_ampm_TRUE_mismatch)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_e_android_ampm_TRUE_mismatch)  == add_cols), as.integer(11))

  expect_identical(sum(colnames(data_g_ios_24_TRUE_mismatch)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_g_android_24_TRUE_mismatch)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_g_ios_ampm_TRUE_mismatch)  == add_cols), as.integer(11))
  expect_identical(sum(colnames(data_g_android_ampm_TRUE_mismatch)  == add_cols), as.integer(11))



#####


  # all data types, anon = FALSE, consent = "Test String not contained in any messages"
  data_e_ios_24_FALSE_mismatch <- hush(parse_chat(system.file("englishios24h.txt", package = "WhatsR"), anonimize = FALSE,consent = "Test String not contained in any messages"))
  data_e_android_24_FALSE_mismatch <- hush(parse_chat(system.file("englishandroid24h.txt", package = "WhatsR"), anonimize = FALSE,consent = "Test String not contained in any messages"))
  data_e_ios_ampm_FALSE_mismatch <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = FALSE,consent = "Test String not contained in any messages"))
  data_e_android_ampm_FALSE_mismatch <- hush(parse_chat(system.file("englishandroidampm.txt", package = "WhatsR"), anonimize = FALSE,consent = "Test String not contained in any messages"))

  data_g_ios_24_FALSE_mismatch <- hush(parse_chat(system.file("germanios24h.txt", package = "WhatsR"), anonimize = FALSE,consent = "Test String not contained in any messages"))
  data_g_android_24_FALSE_mismatch <- hush(parse_chat(system.file("germanandroid24h.txt", package = "WhatsR"), anonimize = FALSE,consent = "Test String not contained in any messages"))
  data_g_ios_ampm_FALSE_mismatch <- hush(parse_chat(system.file("germaniosampm.txt", package = "WhatsR"), anonimize = FALSE,consent = "Test String not contained in any messages"))
  data_g_android_ampm_FALSE_mismatch <- hush(parse_chat(system.file("germanandroidampm.txt", package = "WhatsR"), anonimize = FALSE,consent = "Test String not contained in any messages"))


  #TESTS

  # number of columns = 15
  expect_identical(dim(data_e_ios_24_FALSE_mismatch)[2], as.integer(15))
  expect_identical(dim(data_e_android_24_FALSE_mismatch)[2], as.integer(15))
  expect_identical(dim(data_e_ios_ampm_FALSE_mismatch)[2], as.integer(15))
  expect_identical(dim(data_e_android_ampm_FALSE_mismatch)[2], as.integer(15))

  expect_identical(dim(data_g_ios_24_FALSE_mismatch)[2], as.integer(15))
  expect_identical(dim(data_g_android_24_FALSE_mismatch)[2], as.integer(15))
  expect_identical(dim(data_g_ios_ampm_FALSE_mismatch)[2], as.integer(15))
  expect_identical(dim(data_g_android_ampm_FALSE_mismatch)[2], as.integer(15))

  # number of rows = 8
  expect_identical(dim(data_e_ios_24_FALSE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_e_android_24_FALSE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_e_ios_ampm_FALSE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_e_android_ampm_FALSE_mismatch)[1], as.integer(8))

  expect_identical(dim(data_g_ios_24_FALSE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_g_android_24_FALSE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_g_ios_ampm_FALSE_mismatch)[1], as.integer(8))
  expect_identical(dim(data_g_android_ampm_FALSE_mismatch)[1], as.integer(8))

  # column names
  add_cols <- c("DateTime","Sender","Message","Flat","TokVec","URL","Media","Location","Emoji","EmojiDescriptions","Smilies","SystemMessage","TokCount","TimeOrder","DisplayOrder")
  expect_identical(sum(colnames(data_e_ios_24_FALSE_mismatch) == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_e_android_24_FALSE_mismatch)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_e_ios_ampm_FALSE_mismatch)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_e_android_ampm_FALSE_mismatch)  == add_cols), as.integer(15))

  expect_identical(sum(colnames(data_g_ios_24_FALSE_mismatch)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_g_android_24_FALSE_mismatch)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_g_ios_ampm_FALSE_mismatch)  == add_cols), as.integer(15))
  expect_identical(sum(colnames(data_g_android_ampm_FALSE_mismatch)  == add_cols), as.integer(15))


})


##### Testing tailoring function #####

test_that("tailoring function", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }


  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR"), anonimize = "add"))

  tailored_data1 <- tailor_chat(data,
    names = c("Mallory", "Alice"),
    starttime = "1976-01-01 00:00",
    endtime = "2022-01-01 00:00",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(tailored_data1,"TailoredData1.rds",version = 2)

  test <- readRDS(system.file("TailoredData1.rds", package = "WhatsR"))
  expect_identical(test, tailored_data1)

  tailored_data2 <- tailor_chat(data,
    names = "Dave",
    starttime = "2018-01-29 12:24:03",
    endtime = "2018-01-30 00:13:03",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(tailored_data2,"TailoredData2.rds",version = 2)

  test <- readRDS(system.file("TailoredData2.rds", package = "WhatsR"))
  expect_identical(test, tailored_data2)

  tailored_data3 <- tailor_chat(data,
    names = "Dave",
    starttime = "2018-01-29 12:24:03",
    endtime = "2018-01-29 23:33:03",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(tailored_data3,"TailoredData3.rds",version = 2)

  test <- readRDS(system.file("TailoredData3.rds", package = "WhatsR"))
  expect_identical(test, tailored_data3)

  tailored_data4 <- tailor_chat(data,
    names = "all",
    starttime = "2018-01-29 12:24:03",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(tailored_data4,"TailoredData4.rds",version = 2)

  test <- readRDS(system.file("TailoredData4.rds", package = "WhatsR"))
  expect_identical(test, tailored_data4)
})


########### TESTING PLOTTING FUNCTIONS ############

# TODO:
# # This returns a lot of warning due to the correct font not being available in the testing environment
# # This should be unproblematic in actual use though
# See: https://github.com/REditorSupport/vscode-R/issues/293
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
    # starttime =,
    # endtime =,
    # min_occur = ,
    emoji_vec = "all",
    plot = "bar",
    emoji_size = 10,
    font_family = "Times", # "Times" on Windows
    return_data = TRUE,
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_emoji1,"test_emoji1.rds",version = 2)

  test <- readRDS(system.file("test_emoji1.rds", package = "WhatsR"))
  suppressWarnings(expect_identical(test_emoji1, test))

  test_emoji2 <- hush(plot_emoji(data,
    names = "all",
    # starttime=,
    # endtime=,
    # min.occur = ,
    emoji_vec = "all",
    plot = "cumsum",
    emoji_size = 10,
    font_family = "Times", # "Times" on Windows
    return_data = TRUE,
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_emoji2,"test_emoji2.rds",version = 2)

  test <- readRDS(system.file("test_emoji2.rds", package = "WhatsR"))
  expect_identical(test_emoji2, test)

  test_emoji3 <- hush(plot_emoji(data,
    names = "all",
    # starttime=,
    # endtime=,
    min_occur = 1,
    return_data = TRUE,
    emoji_vec = c("Grinning_Face_with_Smiling_Eyes"),
    plot = "heatmap",
    emoji_size = 10,
    font_family = "Times", # "Times" on Windows
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_emoji3,"test_emoji3.rds",version = 2)

  test <- readRDS(system.file("test_emoji3.rds", package = "WhatsR"))
  expect_identical(test_emoji3, test)


  test_emoji4 <- hush(plot_emoji(data,
    names = "all",
    # starttime=,
    # endtime=,
    min_occur = 1,
    return_data = TRUE,
    emoji_vec = c("Grinning_Face_with_Smiling_Eyes"),
    plot = "bar",
    emoji_size = 10,
    font_family = "Times", # "Times" on Windows
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_emoji4,"test_emoji4.rds",version = 2)

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
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    use_domains = TRUE,
    exclude_long = 50,
    min_occur = 1,
    return_data = TRUE,
    link_vec = "https://github.com/",
    plot = "bar",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_links1,"test_links1.rds",version = 2)

  test <- readRDS(system.file("test_links1.rds", package = "WhatsR"))
  expect_identical(test_links1, test)


  test_links2 <- plot_links(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    use_domains = TRUE,
    exclude_long = 50,
    min_occur = 1,
    return_data = TRUE,
    link_vec = "all",
    plot = "cumsum",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_links2,"test_links2.rds",version = 2)

  test <- readRDS(system.file("test_links2.rds", package = "WhatsR"))
  expect_identical(test_links2, test)

  test_links3 <- plot_links(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    use_domains = FALSE,
    exclude_long = 50,
    min_occur = 1,
    return_data = TRUE,
    link_vec = "all",
    plot = "heatmap",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_links3,"test_links3.rds",version = 2)

  test <- readRDS(system.file("test_links3.rds", package = "WhatsR"))
  expect_identical(test_links3, test)



  test_links4 <- plot_links(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    use_domains = TRUE,
    exclude_long = 50,
    min_occur = 1,
    return_data = TRUE,
    link_vec = "all",
    plot = "splitbar",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_links4,"test_links4.rds",version = 2)

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
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    use_filetype = TRUE,
    min_occur = 1,
    return_data = TRUE,
    media_vec = "all",
    plot = "heatmap",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_media1,"test_media1.rds",version = 2)

  test <- readRDS(system.file("test_media1.rds", package = "WhatsR"))
  expect_identical(test_media1, test)

  test_media2 <- plot_media(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    use_filetype = FALSE,
    min_occur = 1,
    return_data = TRUE,
    media_vec = "all",
    plot = "splitbar",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_media2,"test_media2.rds",version = 2)

  test <- readRDS(system.file("test_media2.rds", package = "WhatsR"))
  expect_identical(test_media2, test)

  test_media3 <- plot_media(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    use_filetype = TRUE,
    min_occur = 1,
    return_data = TRUE,
    media_vec = "all",
    plot = "cumsum",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_media3,"test_media3.rds",version = 2)

  test <- readRDS(system.file("test_media3.rds", package = "WhatsR"))
  expect_identical(test_media3, test)

  test_media4 <- plot_media(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    use_filetype = TRUE,
    min_occur = 1,
    return_data = TRUE,
    media_vec = "all",
    plot = "bar",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_media4,"test_media4.rds",version = 2)

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


  test_location1 <- suppressMessages(plot_locations(data,
    return_data = TRUE,
    jitter_val = 1,
    jitter_seed = 123,
    mapzoom = 10,
    map_leeway = 0.1,
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_location1,"test_location1.rds",version = 2)

  test <- readRDS(system.file("test_location1.rds", package = "WhatsR"))
  expect_identical(test_location1, test)

  test_location2 <- suppressMessages(plot_locations(data,
    return_data = TRUE,
    jitter_val = NA,
    jitter_seed = 567,
    mapzoom = 10,
    map_leeway = 0.1,
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_location2,"test_location2.rds",version = 2)

  test <- readRDS(system.file("test_location1.rds", package = "WhatsR"))
  expect_identical(test_location1, test)

  test_location3 <- suppressMessages(plot_locations(data,
    return_data = TRUE,
    jitter_val = 0.5,
    jitter_seed = 890,
    mapzoom = 10,
    map_leeway = 0.1,
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_location3,"test_location3.rds",version = 2)

  test <- readRDS(system.file("test_location1.rds", package = "WhatsR"))
  expect_identical(test_location1, test)

  test_location4 <- suppressMessages(plot_locations(data,
    return_data = TRUE,
    jitter_val = 0.5,
    jitter_seed = 345,
    mapzoom = 10,
    map_leeway = 0.3,
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_location4,"test_location4.rds",version = 2)

  test <- readRDS(system.file("test_location1.rds", package = "WhatsR"))
  expect_identical(test_location1, test)

  # testing if jittering has worked
  expect_identical(identical(test_location1$Lat,test_location2$Lat,test_location3$Lat,test_location4$Lat),FALSE)

})



test_that("Plotting Messages", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test_messages1 <- plot_messages(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "bar",
    return_data = TRUE,
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_messages1,"test_messages1.rds",version = 2)

  test <- readRDS(system.file("test_messages1.rds", package = "WhatsR"))
  expect_identical(test_messages1, test)

  test_messages2 <- plot_messages(data,
    names = c("Carol", "Dave"),
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "bar",
    return_data = TRUE,
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_messages2,"test_messages2.rds",version = 2)

  test <- readRDS(system.file("test_messages2.rds", package = "WhatsR"))
  expect_identical(test_messages2, test)

  test_messages3 <- plot_messages(data,
    names = "all",
    starttime = "2018-01-30 00:11:20",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "pie",
    return_data = TRUE,
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_messages3,"test_messages3.rds",version = 2)

  test <- readRDS(system.file("test_messages3.rds", package = "WhatsR"))
  expect_identical(test_messages3, test)

  test_messages4 <- plot_messages(data,
    names = c("Alice", "Bob"),
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "pie",
    return_data = TRUE,
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_messages4,"test_messages4.rds",version = 2)

  test <- readRDS(system.file("test_messages4.rds", package = "WhatsR"))
  expect_identical(test_messages4, test)
})



test_that("Plotting Replytimes", {
  hush <- function(code) {
    sink("") # use /dev/null in UNIX
    tmp <- code
    sink()
    return(tmp)
  }

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test_replytimes1 <- suppressMessages(plot_replytimes(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    return_data = TRUE,
    aggregate_sessions = TRUE,
    plot = "box",
    type = "replytime",
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_replytimes1,"test_replytimes1.rds",version = 2)

  test <- readRDS(system.file("test_replytimes1.rds", package = "WhatsR"))
  expect_identical(test_replytimes1, test)


  test_replytimes2 <- suppressMessages(plot_replytimes(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    return_data = TRUE,
    aggregate_sessions = TRUE,
    plot = "box",
    type = "reactiontime",
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_replytimes2,"test_replytimes2.rds",version = 2)

  test <- readRDS(system.file("test_replytimes2.rds", package = "WhatsR"))
  expect_identical(test_replytimes2, test)


  test_replytimes3 <- plot_replytimes(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    return_data = TRUE,
    aggregate_sessions = TRUE,
    plot = "heatmap",
    type = "replytime",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_replytimes3,"test_replytimes3.rds",version = 2)


  test <- readRDS(system.file("test_replytimes3.rds", package = "WhatsR"))
  expect_identical(test_replytimes3, test)

  test_replytimes4 <- plot_replytimes(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    return_data = TRUE,
    aggregate_sessions = TRUE,
    plot = "heatmap",
    type = "reactiontime",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_replytimes4,"test_replytimes4.rds",version = 2)


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

  test_tokens1 <- suppressMessages(plot_tokens(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "bar",
    exclude_sm = TRUE,
    return_data = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_tokens1,"test_tokens1.rds",version = 2)

  test <- readRDS(system.file("test_tokens1.rds", package = "WhatsR"))
  #expect_identical(test_tokens1, test)

  test_tokens2 <- suppressMessages(plot_tokens(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "cumsum",
    exclude_sm = TRUE,
    return_data = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_tokens2,"test_tokens2.rds",version = 2)

  test <- readRDS(system.file("test_tokens2.rds", package = "WhatsR"))
  #expect_identical(test_tokens2, test)


  test_tokens3 <- suppressMessages(plot_tokens(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "violin",
    exclude_sm = TRUE,
    return_data = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_tokens3,"test_tokens3.rds",version = 2)

  test <- readRDS(system.file("test_tokens3.rds", package = "WhatsR"))
  #expect_identical(test_tokens3, test)


  test_tokens4 <- suppressMessages(plot_tokens(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "box",
    exclude_sm = TRUE,
    return_data = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_tokens4,"test_tokens4.rds",version = 2)

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

  test_tot1 <- suppressMessages(plot_tokens_over_time(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "alltime",
    return_data = TRUE,
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_tot1,"test_tot1.rds",version = 2)

  test <- readRDS(system.file("test_tot1.rds", package = "WhatsR"))
  expect_identical(test_tot1, test)


  test_tot2 <- suppressMessages(plot_tokens_over_time(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "year",
    return_data = TRUE,
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_tot2,"test_tot2.rds",version = 2)

  test <- readRDS(system.file("test_tot2.rds", package = "WhatsR"))
  expect_identical(test_tot2, test)


  test_tot3 <- suppressMessages(plot_tokens_over_time(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "day",
    return_data = TRUE,
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_tot3,"test_tot3.rds",version = 2)

  test <- readRDS(system.file("test_tot3.rds", package = "WhatsR"))
  expect_identical(test_tot3, test)


  test_tot4 <- suppressMessages(plot_tokens_over_time(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    plot = "heatmap",
    return_data = TRUE,
    exclude_sm = TRUE
  ))

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_tot4,"test_tot4.rds",version = 2)

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
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    min_occur = 1,
    return_data = TRUE,
    smilie_vec = "all",
    plot = "bar",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_smilies1,"test_smilies1.rds",version = 2)

  test <- readRDS(system.file("test_smilies1.rds", package = "WhatsR"))
  expect_identical(test_smilies1, test)


  test_smilies2 <- plot_smilies(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    min_occur = 1,
    return_data = TRUE,
    smilie_vec = "all",
    plot = "splitbar",
    exclude_sm = TRUE
  )
  # generate and write file [Use this to recreate test files when parse_chat() changed]
  # saveRDS(test_smilies2,"test_smilies2.rds",version = 2)


  test <- readRDS(system.file("test_smilies2.rds", package = "WhatsR"))
  expect_identical(test_smilies2, test)

  # TODO: This fails only when running check from RStudio, not with devtools::test()
  # Seems to be a bug: https://github.com/hadley/r-pkgs/issues/483
  # test_smilies3 <- plot_smilies(data,
  #   names = "all",
  #   starttime = "1960-01-01 00:00",
  #   endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
  #   min_occur = 1,
  #   return_data = TRUE,
  #   smilie_vec = "all",
  #   plot = "cumsum"
  # )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_smilies3,"test_smilies3.rds",version = 2)

  # test <- readRDS(system.file("test_smilies3.rds", package = "WhatsR"))
  # expect_identical(test_smilies3,test)

  test_smilies4 <- plot_smilies(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    min_occur = 1,
    return_data = TRUE,
    smilie_vec = "all",
    plot = "heatmap",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_smilies4,"test_smilies4.rds",version = 2)

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
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    remove_stops = TRUE,
    stop = "english",
    comparison = FALSE,
    return_data = TRUE,
    font_size = 10,
    min_occur = 3,
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_wc1,"test_wc1.rds",version = 2)

  test <- readRDS(system.file("test_wc1.rds", package = "WhatsR"))
  expect_identical(test_wc1, test)



  test_wc2 <- plot_wordcloud(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    remove_stops = FALSE,
    stop = "english",
    comparison = TRUE,
    return_data = TRUE,
    font_size = 15,
    min_occur = 3,
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_wc2,"test_wc2.rds",version = 2)

  test <- readRDS(system.file("test_wc2.rds", package = "WhatsR"))
  expect_identical(test_wc2, test)

  test_wc3 <- plot_wordcloud(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    remove_stops = TRUE,
    stop = "english",
    comparison = FALSE,
    return_data = TRUE,
    font_size = 20,
    min_occur = 5,
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_wc3,"test_wc3.rds",version = 2)

  test <- readRDS(system.file("test_wc3.rds", package = "WhatsR"))
  expect_identical(test_wc3, test)


  test_wc4 <- plot_wordcloud(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    remove_stops = FALSE,
    stop = "english",
    comparison = TRUE,
    return_data = TRUE,
    font_size = 10,
    min_occur = 2,
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_wc4,"test_wc4.rds",version = 2)

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

  data <- hush(parse_chat(system.file("englishiosampm.txt", package = "WhatsR")))

  test_network1 <- plot_network(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    return_data = TRUE,
    collapse_sessions = FALSE,
    edgetype = "n",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_network1,"test_network1.rds",version = 2)

  test <- readRDS(system.file("test_network1.rds", package = "WhatsR"))
  expect_identical(test_network1, test)

  # TODO: This fails only when running check from RStudio, not with devtools::test()
  # Seems to be a bug: https://github.com/hadley/r-pkgs/issues/483
  # test_network2 <- plot_network(data,
  #   names = "all",
  #   starttime = "1960-01-01 00:00",
  #   endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
  #   return_data = TRUE,
  #   collapse_sessions = TRUE,
  #   edgetype = "TokCount",
  #   exclude_sm = TRUE
  # )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_network2,"test_network2.rds",version = 2)

  # test <- readRDS(system.file("test_network2.rds", package = "WhatsR"))
  # expect_identical(test_network2, test)

  test_network3 <- plot_network(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(Sys.time()),
    return_data = TRUE,
    collapse_sessions = FALSE,
    edgetype = "EmojiCount",
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_network3,"test_network3.rds",version = 2)

  test <- readRDS(system.file("test_network3.rds", package = "WhatsR"))
  expect_identical(test_network3, test)

  # TODO: This fails only when running check from RStudio, not with devtools::test()
  # Seems to be a bug: https://github.com/hadley/r-pkgs/issues/483
  # test_network4 <- plot_network(data,
  #   names = "all",
  #   starttime = "1960-01-01 00:00",
  #   endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
  #   return_data = TRUE,
  #   collapse_sessions = TRUE,
  #   edgetype = "SmilieCount",
  #   exclude_sm = TRUE
  # )

  # # generate and write file [Use this to recreate test files when parse_chat() changed]
  # #saveRDS(test_network4,"test_network4.rds",version = 2)

  # test <- readRDS(system.file("test_network4.rds", package = "WhatsR"))
  # expect_identical(test_network4, test)
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
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    return_data = TRUE,
    keywords = c("data", "consent"),
    exclude_sm = TRUE
  )
  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_lediplo1,"test_lediplo1.rds",version = 2)

  test <- readRDS(system.file("test_lediplo1.rds", package = "WhatsR"))
  expect_identical(test_lediplo1, test)


  test_lediplo2 <- plot_lexical_dispersion(data,
    names = c("Alice", "Bob"),
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    return_data = TRUE,
    keywords = c("data", "consent"),
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_lediplo2,"test_lediplo2.rds",version = 2)

  test <- readRDS(system.file("test_lediplo2.rds", package = "WhatsR"))
  expect_identical(test_lediplo2, test)


  test_lediplo3 <- plot_lexical_dispersion(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    return_data = TRUE,
    keywords = c("data", "consent", "this"),
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_lediplo3,"test_lediplo3.rds",version = 2)


  test <- readRDS(system.file("test_lediplo3.rds", package = "WhatsR"))
  expect_identical(test_lediplo3, test)

  test_lediplo4 <- plot_lexical_dispersion(data,
    names = "all",
    starttime = "1960-01-01 00:00",
    endtime = as.character(as.POSIXct(Sys.time(), tz = "UTC")),
    return_data = TRUE,
    keywords = c("data", "consent"),
    exclude_sm = TRUE
  )

  # generate and write file [Use this to recreate test files when parse_chat() changed]
  #saveRDS(test_lediplo4,"test_lediplo4.rds",version = 2)


  test <- readRDS(system.file("test_lediplo4.rds", package = "WhatsR"))
  expect_identical(test_lediplo4, test)
})

