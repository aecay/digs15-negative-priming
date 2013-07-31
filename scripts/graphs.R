# Copyright 2013 Aaron Ecay and Meredith Tamminga
# Available under the Creative Commons AT-SA or GPL v2+ licenses: see
# the file LICENSE for more information

library(ggplot2)
library(plyr)

library(tikzDevice)

options(tikzLualatexPackages = c("\\usepackage{tikz}\n",
            "\\usepackage[active,tightpage]{preview}\n",
            "\\usepackage{fontspec}\n",
            "\\setmainfont{Linux Biolinum}\n",
            "\\PreviewEnvironment{pgfpicture}\n",
            "\\setlength\\PreviewBorder{0pt}\n"))
options(tikzUnicodeMetricPackages = c("\\usetikzlibrary{calc}\n"))
options(tikzMetricPackages = c("\\usetikzlibrary{calc}\n"))
options(tikzDocumentDeclaration = c("\\documentclass[11pt]{article}\n"))
options(tikzDefaultEngine = "luatex")

estival.graph <- function (write = FALSE) {
    library(ggplot2)
    library(tikzDevice)
    estival <- read.csv("data/estival.csv")

    dodge <- position_dodge(width=.9)

    estival$Prime <- factor(estival$Prime, levels=c('none','lexical','trans.'))

    plt <- ggplot(estival, aes(fill = Target, x = Prime, y = Rate)) +
        geom_bar(position = dodge, stat = "identity") +
        scale_y_continuous(limits = c(0,1)) +
        theme(panel.grid.major.x = element_blank())

    if (write) {
        tikz("figures/estival.tikz", width = 4, height = 2.25)
        print(plt + scale_fill_brewer(palette = "Set2"))
        dev.off()
        tikz("figures/estival-handout.tikz", width = 4, height = 2.25)
        print(plt + scale_fill_grey() +
              (theme_minimal() + theme(panel.grid.major.x = element_blank())))
        dev.off()
    }

    return (plt)
}

three.lines.graph <- function(neg, write = FALSE) {
    neg.plot.data <- ddply(neg, .(year), summarize,
                           ne = sum(neg.type == "ne", na.rm = TRUE),
                           not = sum(neg.type == "not", na.rm = TRUE),
                           ne.not = sum(neg.type == "both", na.rm = TRUE))
    neg.plot.data$total <- with(neg.plot.data, ne + not + ne.not)
    neg.plot.data$ne <- with(neg.plot.data, ne / total)
    neg.plot.data$not <- with(neg.plot.data, not / total)
    neg.plot.data$ne.not <- with(neg.plot.data, ne.not / total)
    neg.plot.data <- melt(neg.plot.data, id = c("year","total"))

    plt <- ggplot(aes(x = year, y = value, color = variable),
                  data = neg.plot.data) +
                      geom_point(aes(size = total)) +
                      geom_smooth(aes(weight = total), se = FALSE, linewidth=5) +
                      xlab("Year") +
                      ylab("\\% negative declaratives") +
                      scale_size_continuous("N", breaks = c(750, 1500))

    if (write) {
        tikz("figures/three-lines.tikz", width = 4, height = 2)
        print(plt + scale_color_brewer("Type", palette = "Set2"))
        dev.off()
        tikz("figures/three-lines-handout.tikz", width = 4, height = 2)
        print(plt + scale_color_grey("Type") + theme_minimal())
        dev.off()
    }

    return(plt)
}

ne.not.fac <- function(df, pre = 1250, post = 1350,
                       write = FALSE, file = "figures/ne-not-fac") {
    library(ggplot2)
    library(reshape2)

    df <- subset(df, year >= pre & year < post)
    plot.data <- ddply(df, .(prev.neg.type),
                       summarize,
                       count.ne = sum(has.ne),
                       count.not = sum(has.not),
                       n = length(has.ne))
    plot.data$pct.not <- plot.data$count.not / plot.data$n
    plot.data$pct.ne <- plot.data$count.ne / plot.data$n

    plot.data.melt <- melt(plot.data, id.vars=c("prev.neg.type"),
                           measure.vars = c("pct.ne", "pct.not"))
    plot.data.melt <- subset(plot.data.melt, !is.na(prev.neg.type))

    plt <- ggplot(aes(x = prev.neg.type, y = value, fill = variable), data = plot.data.melt) +
        geom_bar(stat = "identity", position = "dodge") +
        ggtitle("Facilitation of \\emph{ne} and \\emph{not}") +
        xlab("Preceding token") +
        ylab("Percent") +
        scale_y_continuous(limits=c(0,1)) +
        # It's kind of bizarre that the below uses limits and not breaks, but
        # what can one do?
        scale_x_discrete(limits = c("ne", "not", "both"),
                         labels = c("ne alone", "not alone", "ne...not")) +
        guides(fill = guide_legend("Token")) +
        theme(panel.grid.major.x = element_blank())

    if (write) {
        tikz(paste0(file, ".tikz"), width = 4, height = 2.5)
        print(plt + scale_fill_brewer(palette = "Set2",
                                      breaks = c("pct.ne", "pct.not"),
                                      labels = c("ne + ne...not", "not + ne...not")))
        dev.off()
        tikz(paste0(file, "-handout.tikz"), width = 4, height = 2.5)
        print(plt + scale_fill_grey(breaks = c("pct.ne", "pct.not"),
                                    labels = c("has ne?", "has not?")) +
              (theme_minimal() + theme(panel.grid.major.x = element_blank())))
        dev.off()
    }

    return (plt)
}

nnb.fac <- function(df, pre = 1250, post = 1350, write = FALSE,
                    file = "figures/nnb-fac", height = 2.5) {
    library(ggplot2)
    library(reshape)

    df <- subset(df, year >= pre & year < post)
    plot.data <- ddply(df, .(prev.neg.type),
                       summarize,
                       count.ne = sum(has.ne) - sum(has.both),
                       count.not = sum(has.not) - sum(has.both),
                       count.both = sum(has.both),
                       n = length(has.ne))
    plot.data$pct.not <- plot.data$count.not / plot.data$n
    plot.data$pct.ne <- plot.data$count.ne / plot.data$n
    plot.data$pct.both <- plot.data$count.both / plot.data$n

    plot.data.melt <- melt(plot.data, id.vars=c("prev.neg.type"),
                           measure.vars = c("pct.ne", "pct.not", "pct.both"))
    plot.data.melt <- subset(plot.data.melt, !is.na(prev.neg.type))

    plt <- ggplot(aes(x = prev.neg.type, y = value, fill = variable), data = plot.data.melt) +
        geom_bar(stat = "identity", position = "dodge") +
        ggtitle("Facilitation of \\emph{ne} and \\emph{not}") +
        xlab("Preceding token") +
        ylab("Percent") +
        scale_y_continuous(limits=c(0,1)) +
        scale_x_discrete(limits = c("ne", "not", "both"),
                         labels = c("ne alone", "not alone", "ne...not")) +
        guides(fill = guide_legend("Token")) +
        theme(panel.grid.major.x = element_blank())

    if (write) {
        tikz(paste0(file, ".tikz"), width = 4, height = height)
        print(plt + scale_fill_brewer(palette = "Set2",
                                      breaks = c("pct.ne", "pct.not", "pct.both"),
                                      labels = c("ne alone", "not alone", "ne...not")))
        dev.off()
        tikz(paste0(file, "-handout.tikz"), width = 4, height = height)
        print(plt + scale_fill_grey(breaks = c("pct.ne", "pct.not", "pct.both"),
                                    labels = c("ne alone", "not alone", "both"))
              + (theme_minimal() + theme(panel.grid.major.x = element_blank())))
        dev.off()
    }

    return (plt)
}

patch.graph <- function(df, pre = 1250, post = 1350, write = FALSE,
                        file = "figures/patch", height = 2.5) {
    library(ggplot2)
    library(reshape)

    df.subs <- subset(df, year >= pre & year < post & !is.na(prev.neg.type))

    plot.data <- ddply(df.subs, .(prev.neg.type),
                       summarize,
                       count.ne = sum(has.ne) - sum(has.both),
                       count.not = sum(has.not) - sum(has.both),
                       count.both = sum(has.both),
                       n = length(has.ne),
                       discounts = sum(not.posn == "preverbal"))

    ## Here, we can make an exact calculation of the rate of adverbial not
    ## after a previous ne, by counting the number of (ne & preverbal not)
    ## tokens that follow a previous ne.
    prev.ne.to.both <-
        plot.data[plot.data$prev.neg.type == "ne",]$count.both /
        plot.data[plot.data$prev.neg.type == "ne",]$n
    bogus.ne.to.both <- plot.data[plot.data$prev.neg.type == "ne",]$discounts / 0.16
    plot.data[plot.data$prev.neg.type == "ne",]$count.both <-
        plot.data[plot.data$prev.neg.type == "ne",]$count.both - bogus.ne.to.both
    ## We also have to subtract the bad cases out of the N!!!  We could add
    ## them to the ne column instead, but that's a bit like double dipping
    plot.data[plot.data$prev.neg.type == "ne",]$n <-
        plot.data[plot.data$prev.neg.type == "ne",]$n - bogus.ne.to.both

    ## Here, we can't get an exact count, since we want to look at the prime
    ## (well we could, but it would be a pain).  So we'll just approximate by
    ## looking at all tokens of ne-not, whether or not they are primes
    ne.not.discount.rate <- with(subset(df,
                                        neg.type == "both" &
                                        !is.na(next.neg.type)),
                                 sum(not.posn == "preverbal") / 0.16 /
                                 (sum(not.posn == "preverbal") +
                                  sum(not.posn == "postverbal")))

    prev.both.to.ne <-
        plot.data[plot.data$prev.neg.type == "both",]$count.ne /
        plot.data[plot.data$prev.neg.type == "both",]$n
    old.count.both.to.ne <- plot.data[plot.data$prev.neg.type == "both",]$count.ne
    plot.data[plot.data$prev.neg.type == "both",]$count.ne <-
        plot.data[plot.data$prev.neg.type == "both",]$count.ne *
        (1 - ne.not.discount.rate)
    ## And subtract from N, again.  It might be a little fishy that we have
    ## non-integer counts now, but it probably makes little difference (the
    ## alternative is to round...)
    plot.data[plot.data$prev.neg.type == "both",]$n <-
        plot.data[plot.data$prev.neg.type == "both",]$n -
        old.count.both.to.ne +
        plot.data[plot.data$prev.neg.type == "both",]$count.ne

    plot.data$pct.not <- plot.data$count.not / plot.data$n
    plot.data$pct.ne <- plot.data$count.ne / plot.data$n
    plot.data$pct.both <- plot.data$count.both / plot.data$n

    plot.data.melt <- melt(plot.data, id.vars=c("prev.neg.type"),
                           measure.vars = c("pct.ne", "pct.not", "pct.both"))
    plot.data.melt <- subset(plot.data.melt, !is.na(prev.neg.type))

    plot.data.melt$adj <- NA

    plot.data.melt[with(plot.data.melt,
                        prev.neg.type == "ne" & variable == "pct.both"),]$adj <-
                            prev.ne.to.both
    plot.data.melt[with(plot.data.melt,
                        prev.neg.type == "both" & variable == "pct.ne"),]$adj <-
                            prev.both.to.ne

    plt <- ggplot(aes(x = prev.neg.type, y = value, fill = variable),
                  data = plot.data.melt) +
        geom_bar(stat = "identity", position = "dodge") +
        ggtitle("Facilitation of \\emph{ne} and \\emph{not} (patched)") +
        xlab("Preceding token") +
        ylab("Percent") +
        scale_y_continuous(limits=c(0,1)) +
        scale_x_discrete(limits = c("ne", "not", "both"),
                         labels = c("ne alone", "not alone", "ne...not")) +
        guides(fill = guide_legend("Token")) +
        theme(panel.grid.major.x = element_blank()) +
        geom_errorbar(aes(y = adj, ymin = adj, ymax=adj),
                      position = "dodge")

    if (write) {
        tikz(paste0(file, ".tikz"), width = 4, height = height)
        print(plt + scale_fill_brewer(palette = "Set2",
                                      breaks = c("pct.ne", "pct.not", "pct.both"),
                                      labels = c("ne alone", "not alone", "ne...not")))
        dev.off()
        tikz(paste0(file, "-handout.tikz"), width = 4, height = height)
        print(plt + scale_fill_grey(breaks = c("pct.ne", "pct.not", "pct.both"),
                                    labels = c("ne alone", "not alone", "both"))
              + (theme_minimal() + theme(panel.grid.major.x = element_blank())))
        dev.off()
    }

    return (plt)
}

all.graphs <- function () {
    estival.graph(write = TRUE)
    three.lines.graph(neg, write = TRUE)
    ne.not.fac(neg, write = TRUE)
    nnb.fac(neg, write = TRUE)
    nnb.fac(neg, write = TRUE, file = "figures/nnb-fac-late",
            pre = 1350, post = 1401, height = 2.25)
    patch.graph(neg, write = TRUE)

}
