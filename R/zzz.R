.onAttach <- function(lib, pkg) {
    packageStartupMessage(paste("\nThis is yagop version",
                                packageVersion("yagop"), "\n"))
    prepareGOTermsInEnv()
}
