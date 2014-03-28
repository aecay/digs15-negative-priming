
library(ggplot2)
library(plyr)
library(binom)

clauses <- read.csv("data/do.out",
                    sep=":",header=FALSE)

colnames(clauses) <- c("ID",
                       "century",
                       "decade",
                       "unityear",
                       "negation",
                       "aux", # 5
                       "x1",
                       "inversion",
                       "clausetype",
                       "x2",
                       "x3", #10
                       "x4",
                       "subjtype",
                       "x5",
                       "x6",
                       "x7", # 15
                       "x8",
                       "negbeforesubj",
                       "dosupppriming",
                       "dopriming",
                       "doinsent", #20
                       "last.neg.decl",
                       "toks.since.last.nd",
                       "last.aff.que",
                       "toks.since.last.aq",
                       "last.neg.que",
                       "toks.since.last.nq"
)


# convert date info into normal year format
clauses$year <- (1000 + 100 * as.numeric(as.character(clauses$century)) +
                   10 * as.numeric(as.character(clauses$decade)) +
                   as.numeric(as.character(clauses$unityear)))
clauses$century <- clauses$decade <- clauses$unityear <- NULL

# get rid of random columns
clauses$x1 <- clauses$x2 <- clauses$x3 <- clauses$x4 <- clauses$x5 <- NULL
clauses$x6 <- clauses$x7 <- clauses$x8 <- NULL

# get rid of columns with old priming coding scheme
clauses$dosupppriming <- clauses$dopriming <- clauses$doinsent <- NULL


## number all clauses in order for each speaker
clauses <- ddply(clauses, .(ID), transform,
                 clausenum = 1:length(ID))

## Code clauses for whether they contain a possible do-support environment
clauses$is.do.env <- with(clauses, ifelse(
    ((negation == "Negative" & clausetype %in% c("ip-mat","ip-sub")) |
     clausetype %in% c("cp-que","cp-que-2")) &
    aux %in% c("do-support","main-vb-only"),
    "yes","no"))

## Do support in modern environments already coded into clauses need to add
## info on aff dec environments then look at whether they affect each other
clauses$is.aff <- ifelse(clauses$negation == "-" &
                         clauses$clausetype %in% c("ip-mat","ip-sub") &
                         clauses$aux %in% c("do-support","main-vb-only"),
                         "yes","no")

## Is this a modern do-support or affirmative-declarative environment?
clauses$any.do <- ifelse(clauses$is.do.env == "yes" | clauses$is.aff=="yes",
                         "yes", "no")

## take out clauses that don't contain a possible do-support environment
anydo.envs <- subset(clauses, any.do == "yes")

## code type of do-support environment
anydo.envs$type[anydo.envs$is.do.env == "yes"] <- "Modern"
anydo.envs$type[anydo.envs$is.aff == "yes"] <- "Aff. decl."

## three periods: up through peak, during decline, and after it
anydo.envs$period[anydo.envs$year > 1500 & anydo.envs$year <= 1575] <- "1500-1575"
anydo.envs$period[anydo.envs$year > 1575 & anydo.envs$year <= 1625] <- "1576-1625"
anydo.envs$period[anydo.envs$year > 1625 & anydo.envs$year <= 1710] <- "1626-1710"
anydo.envs$period <- as.factor(anydo.envs$period)
anydo.envs <- subset(anydo.envs, !is.na(period))

## code whether do-support or not
anydo.envs$dosupp <- ifelse(anydo.envs$aux == "do-support", 1, 0)

# code previous tokens
anydo.envs <- ddply(anydo.envs, .(ID), transform,
                    prev.do = c(NA, dosupp[-length(dosupp)]),
                    prev.type = c(NA, type[-length(type)]),
                    lag = c(NA, diff(clausenum)))

#anydo.envs$type <- as.factor(anydo.envs$type)
#anydo.envs$prevtype <- as.factor(anydo.envs$prevtype)

anydo.envs$prev.do <- as.factor(anydo.envs$prev.do)

anydo.env.avgs <- ddply(anydo.envs, .(period, type, prev.type, prev.do), summarize,
                        baseline = mean(dosupp),
                        do.count = sum(dosupp),
                        N = length(period))

anydo.env.avgs <- within(anydo.env.avgs, {
    ci <- binom.confint(do.count, N, method="exact")
    base.upper <- ci$upper
    base.lower <- ci$lower
    rm(ci)
})

# target average rates
aff.avgs <- subset(anydo.env.avgs, type == "Affirmative declarative")

plot <- ggplot(subset(anydo.env.avgs, !is.na(type) & !is.na(prev.type)),
               aes(period, baseline)) +
    geom_errorbar(aes(ymin = base.lower, ymax = base.upper, color = prev.do), width=.05) +
    coord_cartesian(ylim = c(-0.05,0.8)) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75)) +
    facet_grid(type ~ prev.type) +
    ggtitle("Primes (rows) and targets (columns)") +
    xlab("Period") +
    ylab("Proportion do")

cairo_pdf("figures/do-results.pdf", width = 6, height = 4, family = "Linux Biolinum")
print(plot +
      geom_point(aes(color = prev.do)) +
      scale_color_brewer("Primed?", labels = c("0" = "no", "1" = "yes"), palette = "Set2"))
dev.off()

cairo_pdf("figures/do-results-handout.pdf", width = 6, height = 4, family = "Linux Biolinum")
print(plot +
      geom_point(aes(shape = prev.do)) +
      scale_color_manual(values = c("black","black")) +
      scale_shape_discrete("Primed?")
dev.off()
