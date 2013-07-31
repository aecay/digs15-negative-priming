# Copyright 2013 Aaron Ecay and Meredith Tamminga
# Available under the Creative Commons AT-SA or GPL v2+ licenses: see
# the file LICENSE for more information

cleanNegData <- function(path = "queries/coding.cod.ooo") {
    library(stringr)

    df <- df <- read.csv(path, sep = ":", header = FALSE)
    colnames(df) <- c("century", "decade", "unityear", "exclude",
                      "ne", "not", "clausetype", "never.posn", "not.posn",
                      "finite", "id")
    df$year <- (1000 + 100 * as.numeric(as.character(df$century)) +
                10 * as.numeric(as.character(df$decade)) +
                as.numeric(as.character(df$unityear)))
    df$century <- df$decade <- df$unityear <- NULL

    df$author <- str_replace(df$id, "-.*", "")

    df$exclude <- factor(ifelse(df$clausetype == "imperative" |
                                df$finite == "-",
                                "X",
                                as.character(df$exclude)))

    prev.ne <- rep(NA, times = nrow(df))
    prev.not <- rep(NA, times = nrow(df))

    for (i in 2:nrow(df)) {
        if (df[i-1,]$author == df[i,]$author &&
            df[i-1,]$exclude != "X" &&
            df[i,]$exclude != "X") {
              prev.ne[i] <- as.character(df[i-1,]$ne)
              prev.not[i] <- as.character(df[i-1,]$not)
        }
    }

    df$prev.ne <- factor(prev.ne)
    df$prev.not <- factor(prev.not)

    ## and we need the next NEG, for calculating the patch
    next.ne <- rep(NA, times = nrow(df))
    next.not <- rep(NA, times = nrow(df))
    for (i in 1:(nrow(df)-1)) {
        if (df[i+1,]$author == df[i,]$author &&
            df[i+1,]$exclude != "X" &&
            df[i,]$exclude != "X") {
            next.ne[i] <- as.character(df[i+1,]$ne)
            next.not[i] <- as.character(df[i+1,]$not)
        }
    }
    df$next.ne <- factor(next.ne)
    df$next.not <- factor(next.not)

    df$has.ne <- as.logical(df$ne == "ne")
    df$has.not <- as.logical(df$not %in% c("preverbal","postverbal"))
    df$has.both <- as.logical(df$has.ne & df$has.not)

    df$neg.type <- ifelse(df$has.both, "both",
                          ifelse(df$has.ne, "ne",
                                 ifelse(df$has.not, "not", NA)))

    ## calculate derived vars about previous NEG
    df$has.ne.prev <- as.logical(df$prev.ne == "ne")
    df$has.not.prev <- as.logical(
        ifelse(is.na(df$prev.not),
               NA,
               df$prev.not %in% c("preverbal", "postverbal")))
    df$has.both.prev <- as.logical(df$has.ne.prev & df$has.not.prev)

    df$prev.neg.type <- ifelse(df$has.both.prev, "both",
                               ifelse(df$has.ne.prev, "ne",
                                      ifelse(df$has.not.prev, "not", NA)))

    ## Same, for next NEG
    df$has.ne.next <- as.logical(df$next.ne == "ne")
    df$has.not.next <- as.logical(
        ifelse(is.na(df$next.not),
               NA,
               df$next.not %in% c("nexterbal", "postverbal")))
    df$has.both.next <- as.logical(df$has.ne.next & df$has.not.next)

    df$next.neg.type <- ifelse(df$has.both.next, "both",
                               ifelse(df$has.ne.next, "ne",
                                      ifelse(df$has.not.next, "not", NA)))

    ## A silly attempt to bin dates
    df$datepd <- ifelse(df$year < 1300, 1,
                        ifelse(df$year == 1300, 2, 3))

    return(df)
}
