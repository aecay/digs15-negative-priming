cleanNegData <- function(path = "queries/coding.cod.ooo") {
    library(stringr)

    df <- df <- read.csv(path, sep = ":", header = FALSE)
    colnames(df) <- c("century", "decade", "unityear", "exclude",
                      "ne", "not", "clausetype", "id")
    df$year <- (1000 + 100 * as.numeric(as.character(df$century)) +
                10 * as.numeric(as.character(df$decade)) +
                as.numeric(as.character(df$unityear)))
    df$century <- df$decade <- df$unityear <- NULL

    df$author <- str_replace(df$id, "-.*", "")

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

    df$has.ne <- as.logical(df$ne == "ne")
    df$has.not <- as.logical(df$not %in% c("preverbal","postverbal"))
    df$has.both <- as.logical(df$has.ne & df$has.not)

    df$neg.type <- ifelse(df$has.both, "both",
                          ifelse(df$has.ne, "ne",
                                 ifelse(df$has.not, "not", NA)))

    df$has.ne.prev <- as.logical(df$prev.ne == "ne")
    df$has.not.prev <- as.logical(
        ifelse(is.na(df$prev.not),
               NA,
               df$prev.not %in% c("preverbal", "postverbal")))
    df$has.both.prev <- as.logical(df$has.ne.prev & df$has.not.prev)

    df$prev.neg.type <- ifelse(df$has.both.prev, "both",
                               ifelse(df$has.ne.prev, "ne",
                                      ifelse(df$has.not.prev, "not", NA)))

    df$datepd <- ifelse(df$year < 1300, 1,
                        ifelse(df$year == 1300, 2, 3))

    return(df)
}
