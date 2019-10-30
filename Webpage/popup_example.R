library(ggplot2)
library(stats)
data(meuse)
coordinates(meuse) <- ~ x + y
proj4string(meuse) <- CRS("+init=epsg:28992")

## create plots with points colored according to feature id
p <- plot(meuse@data$cadmium, meuse@data$copper, col = "gray") #ggplot(data = meuse@data, aes(x = cadmium, y = copper), col = "grey") +
  #geom_point()
p <- mget(rep("p", length(meuse)))

clr <- rep("grey", length(meuse))
# p <- lapply(1:length(p), function(i) {
#   clr[i] <- "red"
#   update(p[[i]], col = clr)
# })

mapview(meuse, popup = popupGraph(p, type = "svg", width = 3, height = 2.5))