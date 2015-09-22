
## load libraries
library(ggplot2)
library(grid)
library(scales)

## read data
dat <- read.csv("../data/voter-turnout-by-prov-1980-to-2014.csv")
dat

## re-order provinces by median voter turnout
dat$province <- with(dat, reorder(province, percent.turnout, median))

## plot voter turnout by province
png("../figures/all-provs-1980-2014.png", height = 5, width = 6.5, units = "in", res = 300)
ggplot(dat, aes(province, percent.turnout)) +
       geom_boxplot(fill = "salmon") +
       coord_flip() +
       scale_y_continuous(limits=c(37, 90), labels = function(label) paste(label, "%", sep = "")) +
       xlab(NULL) +
       ylab("Voter turnout in provincial general elections, 1980-2014") +
       theme(axis.title.x = element_text(size = 15, vjust = -0.8),
             axis.text.x = element_text(size = 12, colour = "grey40"),
             axis.text.y = element_text(size = 12, colour = "grey40"),
             plot.margin = unit(c(5, 5, 5, 5), "mm"))
dev.off()