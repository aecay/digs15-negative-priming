library(ggplot2)
library(reshape2)
library(binom)

two.way.patch.graph <- function (df, pre = 1250, post = 1350, write = FALSE,
                                 file = "figures/patch-two-way", height = 2.5,
                                 confint = FALSE, patch.lines = TRUE) {
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

    plot.data$pct.ne <- (plot.data$count.ne + plot.data$count.both) /
        (plot.data$count.ne + plot.data$count.both + plot.data$count.not)
    plot.data$pct.not <- (plot.data$count.not + plot.data$count.both) /
        (plot.data$count.ne + plot.data$count.both + plot.data$count.not)

    plot.data.melt <- melt(plot.data, id.vars=c("prev.neg.type"),
                           measure.vars = c("pct.ne", "pct.not"))
    plot.data.melt <- subset(plot.data.melt, !is.na(prev.neg.type))

    ns <- plot.data$n
    names(ns) <- plot.data$prev.neg.type
    plot.data.melt <- within(plot.data.melt, {
        ci <- binom.confint(floor(value * ns[prev.neg.type]),
                            floor(ns[prev.neg.type]),
                            methods = "exact")
        upper <- ci$upper
        lower <- ci$lower
        rm(ci)
    })

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
        theme(panel.grid.major.x = element_blank())

    ## if (patch.lines) {
    ##     plt <- plt + geom_errorbar(aes(y = adj, ymin = adj, ymax=adj),
    ##                                position = "dodge")
    ## }

    if (confint) {
        plt <- plt + geom_pointrange(aes(y = value, ymin = lower, ymax = upper),
                                     position = position_dodge(width = 0.9),
                                     color = "black", show_guide = FALSE)
    }

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


estival.graph3 <- function (write = FALSE) {
    estival <- read.csv("data/estival.csv")

    dodge <- position_dodge(width = .9)

    estival$Prime <- factor(estival$Prime, levels=c('none','lexical','trans.'))
    levels(estival$Prime)[3] <- "transitive"
    levels(estival$Target)[3] <- "transitive"

    estival <- droplevels(subset(estival, Target != "active"))

    plt <- ggplot(estival, aes(fill = Target, x = Prime, y = Rate)) +
        geom_bar(position = dodge, stat = "identity") +
        scale_y_continuous(limits = c(0,1)) +
        theme(panel.grid.major.x = element_blank())

    if (write) {
        cairo_pdf("figures/estival-handout.pdf", width = 4, height = 2.25, family = "Linux Libertine")
        print(plt + scale_fill_grey() +
              (theme_minimal() + theme(panel.grid.major.x = element_blank())))
        dev.off()
    }

    return (plt)
}

ne.not.graph3 <- function (neg, write = FALSE) {
    g <- ne.not.fac(neg, write = FALSE, confint = TRUE) +
        ggtitle(expression(paste("Facilitation of ", italic("ne"),
            " and ", italic("not")))) +
        xlab("Prime") +
        guides(fill = guide_legend("Target")) +
        ylab("Percent target outcome")
    if (write) {
        cairo_pdf("figures/ne-not3-handout.pdf", width = 4, height = 3,
                  family = "Linux Libertine")
        print(g + theme_minimal() + theme(panel.grid.major.x = element_blank()) +
                  scale_fill_grey(breaks = c("pct.ne", "pct.not", "pct.both"),
                                  labels = expression(paste("contains ", italic("ne")),
                                      paste("contains ", italic("not")),
                                      italic("ne...not"))))
        dev.off()
    }
    return (g)
}

ne.not.both.graph3 <- function (neg, write = FALSE) {
    g <- nnb.fac(neg, write = FALSE, confint = TRUE) +
        ggtitle(expression(paste("Facilitation of ", italic("ne"),
            " and ", italic("not")))) +
        xlab("Prime") +
        guides(fill = guide_legend("Target")) +
        ylab("Percent target outcome")
    if (write) {
        cairo_pdf("figures/ne-not-both3-handout.pdf", width = 4, height = 3,
                  family = "Linux Libertine")
        print(g + theme_minimal() + theme(panel.grid.major.x = element_blank()) +
                  scale_fill_grey(breaks = c("pct.ne", "pct.not", "pct.both"),
                                  labels = expression(paste(italic("ne")," alone"),
                                      paste(italic("not")," alone"),
                                      italic("ne...not"))))
        dev.off()
    }
}


three.lines.graph3 <- function(neg, write = FALSE) {
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
                      ylab("proportion neg. decl.") +
                      scale_size_area("N", breaks = c(750, 1500))

    if (write) {
        cairo_pdf("figures/three-lines3-handout.pdf", width = 4, height = 2.25,
                  family = "Linux Libertine")
        print(plt + scale_color_grey("Type",
                                     breaks = c("ne", "not", "ne.not"),
                                     labels = expression(
                                         italic("ne"),
                                         italic("not"),
                                         italic("ne...not"))) + theme_minimal())
        dev.off()
    }

    return(plt)
}

patch.graph3 <- function (neg, write = FALSE) {
    g <- patch.graph(neg, write = FALSE, confint = FALSE, patch.lines = TRUE) +
        ggtitle("Facilitation of ne and not") +
        xlab("Prime") +
        guides(fill = guide_legend("Target"))
    if (write) {
        cairo_pdf("figures/patch3.pdf", width = 4, height = 3,
                  family = "Linux Libertine")
        print(g + scale_fill_brewer(palette = "Set2",
                                    breaks = c("pct.ne", "pct.not", "pct.both"),
                                    labels = expression(paste(italic("ne")," alone"),
                                        paste(italic("not")," alone"),
                                        italic("ne...not"))))
        dev.off()
        cairo_pdf("figures/patch3-handout.pdf", width = 4, height = 3,
                  family = "Linux Libertine")
        print(g + theme_minimal() + theme(panel.grid.major.x = element_blank()) +
              scale_fill_grey(breaks = c("pct.ne", "pct.not", "pct.both"),
                              labels = expression(paste(italic("ne")," alone"),
                                  paste(italic("not")," alone"),
                                  italic("ne...not"))))
        dev.off()
    }
}

ne.not.both.late.graph3 <- function (neg, write = FALSE) {
    g <- nnb.fac(neg, write = FALSE, confint = TRUE, pre = 1350, post = 1401) +
        ggtitle(expression(paste("Facilitation of ", italic("ne"),
            " and ", italic("not")))) +
        xlab("Prime") +
        guides(fill = guide_legend("Target"))
    if (write) {
        cairo_pdf("figures/ne-not-both-late3.pdf", width = 4, height = 3,
                  family = "Linux Libertine")
        print(g + scale_fill_brewer(palette = "Set2",
                                    breaks = c("pct.ne", "pct.not", "pct.both"),
                                    labels = expression(paste(italic("ne")," alone"),
                                        paste(italic("not")," alone"),
                                        italic("ne...not"))))
        dev.off()
        cairo_pdf("figures/ne-not-both-late3-handout.pdf", width = 4, height = 3,
                  family = "Linux Libertine")
        print(g + theme_minimal() + theme(panel.grid.major.x = element_blank()) +
                  scale_fill_grey(breaks = c("pct.ne", "pct.not", "pct.both"),
                                  labels = expression(paste(italic("ne")," alone"),
                                      paste(italic("not")," alone"),
                                      italic("ne...not"))))
        dev.off()
    }
}


all.graphs3 <- function (neg) {
    ne.not.graph3(neg, TRUE)
    ne.not.both.graph3(neg, TRUE)
    patch.graph3(neg, TRUE)
    three.lines.graph3(neg, TRUE)
    estival.graph3(TRUE)
    ne.not.both.late.graph3(neg, TRUE)
}
