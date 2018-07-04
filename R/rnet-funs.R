#' Decomposes a route network into its component parts
#'
#' @param l A route network provided as an \code{sf} object with linestring geometries
#'
#' @examples
#' routes_fast_decomposed <- rnet_decompose(routes_fast_sf)
rnet_decompose <- function(l) {
  if(is.null(l$highway)) l$highway <- "tertiary"
  g <- dodgr::weight_streetnet(sf_lines = l, wt_profile = 1)
}

# testing
library(stplanr)
library(dodgr)
library(sf)
class(hampi)
hampi # error...
hampi_sf = st_sf(hampi)
# weight_streetnet(hampi) # works
os_roads_bristol
plot(os_roads_bristol)

# from ?os_roads_open
colnm <- "formOfWay" # name of column used to determine weights
wts <- c (0.1, 0.2, 0.8, 1)
names (wts) <- unique (os_roads_bristol [[colnm]])
os_roads_bristol$highway = "Motorway"
weight_streetnet(os_roads_bristol) # fails wih unhelpful error msg
net <- weight_streetnet (os_roads_bristol, wt_profile = wts, # fails wih unhelpful error msg
                         type_col = colnm, id_col = "identifier")
os_roads_bristol$highway <- NULL
net <- weight_streetnet (os_roads_bristol, wt_profile = wts, # works!!!!
                         type_col = colnm, id_col = "identifier")
# another example...
data_dir <- system.file("extdata", package = "stplanr")
unzip(file.path(data_dir, "sydroads.zip"), exdir = tempdir())
sydroads <- read_sf(tempdir(), "roads")
plot(sydroads$geometry)
names(sydroads)
colnm <- "type"
road_types <- unique (sydroads[[colnm]])
wts <- rep(1, length(road_types))
names (wts) <- road_types
weight_streetnet(sydroads)  # fails
net1 <- weight_streetnet(sydroads, type_col = "type") # works?
net <- weight_streetnet (sydroads, wt_profile = wts,
                         type_col = colnm, id_col = "osm_id")
