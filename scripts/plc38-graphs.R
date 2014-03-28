# Copyright 2014 Aaron Ecay and Meredith Tamminga
# Available under the Creative Commons AT-SA or GPL v2+ licenses: see
# the file LICENSE for more information

library(ggplot2)
library(plyr)

source("scripts/graphs.R")

estival.graph2 <- function (write = FALSE) {
    estival <- read.csv("data/estival.csv")

    dodge <- position_dodge(width = .9)

    estival$Prime <- factor(estival$Prime, levels=c('none','lexical','trans.'))

    plt <- ggplot(estival, aes(fill = Target, x = Prime, y = Rate)) +
        geom_bar(position = dodge, stat = "identity") +
        scale_y_continuous(limits = c(0,1)) +
        theme(panel.grid.major.x = element_blank())

    if (write) {
        cairo_pdf("figures/estival.pdf", width = 4, height = 2.25, family = "Linux Biolinum")
        print(plt + scale_fill_brewer(palette = "Set2"))
        dev.off()
        cairo_pdf("figures/estival-handout.pdf", width = 4, height = 2.25, family = "Linux Biolinum")
        print(plt + scale_fill_grey() +
              (theme_minimal() + theme(panel.grid.major.x = element_blank())))
        dev.off()
    }

    return (plt)
}

patch.graph2 <- function (neg, write = FALSE) {
    g <- patch.graph(neg, write = FALSE, confint = TRUE, patch.lines = FALSE) +
        ggtitle("Facilitation of ne and not") +
        theme(legend.position = "bottom")
    if (write) {
        cairo_pdf("figures/patch2.pdf", width = 4, height = 6, family = "Linux Biolinum")
        print(g + scale_fill_brewer(palette = "Set2",
                                    breaks = c("pct.ne", "pct.not", "pct.both"),
                                    labels = c("ne alone", "not alone", "ne...not")))
        dev.off()
        cairo_pdf("figures/patch2-handout.pdf", width = 4, height = 6,
                  family = "Linux Biolinum")
        print(g + theme_minimal() + theme(panel.grid.major.x = element_blank()) +
              scale_fill_grey(breaks = c("pct.ne", "pct.not", "pct.both"),
                              labels = c("ne alone", "not alone", "both")))
        dev.off()
    }
}

ne.not.graph2 <- function (neg, write = FALSE) {
    g <- ne.not.fac(neg, write = FALSE, confint = TRUE) +
        ggtitle("Facilitation of ne and not") +
        theme(legend.position = "bottom")
    if (write) {
        cairo_pdf("figures/ne-not2.pdf", width = 4, height = 6, family = "Linux Biolinum")
        print(g + scale_fill_brewer(palette = "Set2",
                                    breaks = c("pct.ne", "pct.not", "pct.both"),
                                    labels = c("ne alone", "not alone", "ne...not")))
        dev.off()
        cairo_pdf("figures/ne-not2-handout.pdf", width = 4, height = 6,
                  family = "Linux Biolinum")
        print(g + theme_minimal() + theme(panel.grid.major.x = element_blank()) +
              scale_fill_grey(breaks = c("pct.ne", "pct.not", "pct.both"),
                              labels = c("ne alone", "not alone", "both")))
        dev.off()
    }
}

## Differences from three.lines.graph in graphs.R:
## - write to pdf, not tikz
## - use scale_size_area
three.lines.graph2 <- function(neg, write = FALSE) {
    library(reshape2)
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
                      ylab("% negative declaratives") +
                      scale_size_area("N", breaks = c(750, 1500))

    if (write) {
        cairo_pdf("figures/three-lines2.pdf", width = 4, height = 2.25, family = "Linux Biolinum")
        print(plt + scale_color_brewer("Type", palette = "Set2"))
        dev.off()
        cairo_pdf("figures/three-lines2-handout.pdf", width = 4, height = 2.25, family = "Linux Biolinum")
        print(plt + scale_color_grey("Type") + theme_minimal())
        dev.off()
    }

    return(plt)
}
