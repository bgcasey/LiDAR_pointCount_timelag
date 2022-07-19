library(sf)
load("0_data/manual/bird/ss_xy.rData")
#station bounding box
bb<-st_bbox(ss_xy)
bb<-st_as_sfc(bb)
bb<-st_as_sf(bb)
plot(bb)
x<-as.numeric(st_area(bb))
x
#113269538 [m^2]
library(measurements)
help("conv_unit")
conv_unit(x, "m2", "km2")
conv_unit(x, "m2", "hectare")

#get centroid coordinates
c<-st_centroid(bb)

#put in degree, minute, second, format
library(GEOmap)

cc<--as.vector(as(c, "Spatial")@coords)


