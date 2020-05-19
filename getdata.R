# get some data in a reasonable format

library(sf)
library(rgdal)
library(dplyr)


# merge the segments back to being transects
at <- read_sf("raw/transects/aerialtransects082112.shp")
# remove seg data
at$Segment <- NULL
at$Id <- NULL
at$z <- NULL
at$Length <- NULL

at <- st_transform(at, 6348)

# this code is horrendous
ll <- list()
tr <- 1
for(i in unique(at$Transect)){

  dd <- subset(at, Transect==i)


  this_tr <- st_line_merge(st_union(st_cast(dd,"MULTILINESTRING")))

  if(length(st_cast(this_tr, "LINESTRING"))==1){
    ll[[tr]] <- st_sf(st_cast(st_line_sample(this_tr, sample=c(0,1)), "LINESTRING"),
                      data.frame(Transect=tr))
    names(ll[[tr]]) <- c("Transect", "geometry")
    attr(ll[[tr]], "sf_column") <- "geometry"
    tr <- tr + 1
  }else{
    tmp <- st_cast(this_tr, "LINESTRING")
    for(ii in 1:length(tmp)){
      ll[[tr]] <- st_sf(st_cast(st_line_sample(tmp[ii,], sample=c(0,1)), "LINESTRING"),
                        data.frame(Transect=tr))
      names(ll[[tr]]) <- c("Transect", "geometry")
      attr(ll[[tr]], "sf_column") <- "geometry"
      tr <- tr + 1
    }
  }

}

at_sf <- do.call(rbind, ll)
at_sf <- st_transform(at_sf, 4326)


# write to disk
st_write(at_sf, dsn="data/ri_transects.shp", driver="ESRI Shapefile")


# fiddle with the coast

coast <- read.table("raw/ri_coast.dat", header=TRUE)

ind <- which(is.na(coast$lon))
ii <- diff(c(1, ind, nrow(coast)+1))
coast$thing <- rep(1:length(ii), ii)

coast <- coast[-which(is.na(coast$lon)), ]
coast <- st_as_sf(coast, coords=c("lon","lat"))


coast_sf <- coast %>%
  group_by(thing) %>%
  summarize(do_union=FALSE) %>%
  st_cast("MULTILINESTRING")


st_crs(coast_sf) <- st_crs(at_sf)

st_write(coast_sf, dsn="data/ri_coast.shp", driver="ESRI Shapefile")


