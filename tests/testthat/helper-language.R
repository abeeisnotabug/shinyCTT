## The app now starts in whichever language R itself is running in, so on a German machine
## tr() would give back German unless something says otherwise. Every expectation below that
## names a word names the English one, so pin it here rather than in each test.
##
## withLanguage() in test-translations.R sets and restores this same option, so it goes back
## to English after each of those.
options(shinyCTT.language = "en")
