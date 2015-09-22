
# Load relevant libraries
library(rgdal)
library(plyr)
library(ggplot2)
library(grid)

# Read voter turnout data and electoral boundaries shapefile
voterTurnout <- read.csv("../data/alberta-voter-turnout-2015.csv")
electBoundaries <- readOGR("../data/Alberta_Electoral_Boundaries_Act2010", "EDs_Act2010_FINAL")

# Check data
head(voterTurnout)
head(electBoundaries@data)

# Print list of electoral districts by voter turnout
voterTurnout[order(voterTurnout$percent.turnout),]

# Create ids for each electoral district in electBoundaries attribute table (based on row numbers)
electBoundaries@data$id <- rownames(electBoundaries@data)

# Re-order voterTurnout df to match order of electoral districts in electBoundaries
voterTurnout <- voterTurnout[rank(electBoundaries@data$EDName2010),]

# Check that order of districts is consistent between voterTurnout and electBoundaries
data.frame(electBoundaries@data$EDName2010, voterTurnout$district)

# Give ids to each district within voterTurnout df (same ids as electBoundaries)
voterTurnout$id <- electBoundaries@data$id

# Join voterTurnout df to electBoundaries
electBoundaries@data <- join(electBoundaries@data, voterTurnout, by = "id")

# 'Fortify' shapefile (allows plotting with ggplot) then re-join to full attribute table
electBoundariesFort <- fortify(electBoundaries, region = "id")
electBoundariesFull <- join(electBoundariesFort, electBoundaries@data, by = "id")

# Extract city-specific data
electBoundariesEdmonton <- electBoundariesFull[grep("Edmonton", electBoundariesFull$EDName2010),]
electBoundariesCalgary <- electBoundariesFull[grep("Calgary", electBoundariesFull$EDName2010),]
electBoundariesRedDeer <- electBoundariesFull[grep("Red Deer", electBoundariesFull$EDName2010),]
electBoundariesLethbridge <- electBoundariesFull[grep("Lethbridge", electBoundariesFull$EDName2010),]

# Wrapper function for plotting maps
PlotWrapper <- function(data, title = NULL, guide = F, margin.bottom = 0) {
  plot <- ggplot(data) + 
    aes(long, lat, group = group, fill = percent.turnout) + 
    geom_polygon() +
    geom_path(size = 0.2, color = "grey90") +
    coord_equal() +
    xlab(NULL) +
    ylab(NULL) +
    ggtitle(title) +
    scale_fill_gradient(guide = guide,
                        low = "white",
                        high = rgb(0.15, 0, 0),
                        limits = c(min(voterTurnout$percent.turnout), max(voterTurnout$percent.turnout)),
                        labels = function(label) paste(label, "%", sep = ""),
                        name = "Voter\nTurnout") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.margin = unit(c(0, 0, margin.bottom, 0), "cm"),
          legend.justification = c(0, 0),
          legend.position = c(0, 0),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11))
  return(plot)
}

# Create individual plots for Alberta and four largest cities
p1 <- PlotWrapper(electBoundariesFull, title = NULL, guide = "colourbar")
p2 <- PlotWrapper(electBoundariesEdmonton, title = "Edmonton")
p3 <- PlotWrapper(electBoundariesCalgary, title = "Calgary")
p4 <- PlotWrapper(electBoundariesRedDeer, title = "Red Deer", margin.bottom = 0.5)
p5 <- PlotWrapper(electBoundariesLethbridge, title = "Lethbridge", margin.bottom = 0.5)

# Plot
png("../figures/alberta-2015.png", height = 6.5, width = 9.2, units = "in", res = 300)
pushViewport(viewport(layout = grid.layout(3, 7)))
print(p1, vp = viewport(layout.pos.row = 1:3, layout.pos.col = 1:3))
print(p2, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 4:5))
print(p3, vp = viewport(layout.pos.row = 1:2, layout.pos.col = 6:7))
print(p4, vp = viewport(layout.pos.row = 3, layout.pos.col = 4:5))
print(p5, vp = viewport(layout.pos.row = 3, layout.pos.col = 6:7))
dev.off()

