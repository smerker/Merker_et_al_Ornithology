#### Range Map #####

library(terra)
library(raster)
library(sf)
library(maptools)
library(maps)
library(mapdata)
library(ggmap)
library(cleangeo)
library(rgeos)
library(GISTools)
library(prettymapr)

#bring in some curated data.
load("./btbw_howa_map_data.gzip")

png("HOWA_BTBW_range.png", height=6, width=7, units="in", res=600)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
map("worldHires","Canada", xlim=c(-129,-60), ylim=c(24.5,54), col="black", fill=FALSE, boundary=T)
#plot(can1, axes=TRUE, xlim=c(-97, -60), ylim=c(28, 50))
map("worldHires","USA", col="black", fill=FALSE, add=T)
map('state','North Carolina', add=T)
plot(BTBW.map, col=rgb(0,0,1,.8), add=TRUE)
plot(HOWA.map, col=rgb(.9,.1,0,.6), add=TRUE)
points(-83.8,35.05, pch=22, bg="black",col="black", cex=.35)
#points(-72,43.9, pch=22, bg="white",col="black")
raster::scalebar(d = 1000, # distance in km
                 xy = c(-120, 28),
                 type = "bar", 
                 divs = 2, 
                 below = "", 
                 lonlat = TRUE,
                 label = c(0,500,1000),
                 cex.lab=.8,
                 # adj=c(0, -0.75), 
                 lwd = 2, cex=.6)
text("km", x=-114.3, y=27.3, cex=.6)
text("North Carolina", x=-79.8,y=35.7, cex=.3)
addnortharrow("bottomright",padin=c(1.06,.81),col="black", lwd=1, scale=.5)
legend(-77,30, c("BTBW breeding range", "HOWA breeding range", "Range Overlap",  "Nantahala National Forest"),
       cex=.5, 
       pch=c(22,22,22,22), 
       pt.cex=.7,
       pt.bg=c(rgb(0,0,1,.8),rgb(.9,.1,0,.6), "darkmagenta", "black"), 
       col="black")
box(which = "plot", lty = "solid")
dev.off()

system("open HOWA_BTBW_range.png")
