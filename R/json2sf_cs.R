txt2coords = function(txt) { # helper function to document...
  coords_split <- stringr::str_split(txt, pattern = " |,")[[1]]
  matrix(as.numeric(coords_split), ncol = 2, byrow = TRUE)
}
#' Convert output from CycleStreets.net into sf object
#'
#' @param obj Object from CycleStreets.net read-in with:
#' \code{jsonlite::read_json("inst/extdata/res_json.json", simplifyVector = T)}
#'
#' @export
#' @examples
#' # res_json = stplanr::route_cyclestreet("Berlin", "Potsdam", silent = FALSE, save_raw = TRUE)
#' # jsonlite::write_json(res_json, "inst/extdata/res_json.json")
#' obj = jsonlite::read_json("inst/extdata/res_json.json", simplifyVector = T)
#' rsf = json2sf_cs(obj)
#' sf:::plot.sf(rsf)
json2sf_cs <- function(obj) {
  coord_list = lapply(obj$marker$`@attributes`$points[-1], txt2coords)
  rsflx = lapply(coord_list, sf::st_linestring)
  rsfl = sf::st_sfc(rsflx)

  b = sapply(obj$marker$`@attributes`$busynance[-1], as.numeric)
  n = obj$marker$`@attributes`$name[-1]

  # todo: create more segment-level statistics (vectors) +
  # add them to the data frame (d) below

  d = data.frame(busynance = b, name = n)

  rsf = sf::st_sf(d, geometry = rsfl)

  return(rsf)

}

# test code (to delete / tidy) ----
# res_json = jsonlite::read_json("inst/extdata/res_json.json", simplifyVector = T)
# str(res_json)
# length(res_json)
# length(res_json$marker$`@attributes`) # the segments
# str(res_json$waypoint) # data frame: origin and destination
# names(res_json$marker$`@attributes`) # routing data for all legs
# res_json$marker$`@attributes`$coordinates[[1]] # all coords...
# obj = res_json
# coords <- obj$marker$`@attributes`$coordinates[1]
# coords <- stringr::str_split(coords, pattern = " |,")[[1]]
# coords <- matrix(as.numeric(coords), ncol = 2, byrow = TRUE)
# rsp = sp::SpatialLines(list(sp::Lines(list(sp::Line(coords)), ID = 1)))
# rsf = sf::st_linestring(coords)
# # sf: much faster
# microbenchmark::microbenchmark(
#   sp::SpatialLines(list(sp::Lines(list(sp::Line(coords)), ID = 1))),
#   sf::st_linestring(coords)
#   )
#
# h <-  obj$marker$`@attributes`$elevations # hilliness
# h <- stringr::str_split(h[[1]], pattern = ",") #only take first set of data
# h <- as.numeric(unlist(h)[-1])
# hdif <- diff(h)
# htot <- sum(abs(hdif))
# hchng <- h[length(h)] - h[1]
# hmxmn <- max(h) - min(h)
# hup <- sum(hdif[which(hdif>0)])
# hdown <- -1* sum(hdif[which(hdif<0)])
#
# # busyness overall
# bseg <- obj$marker$`@attributes`$busynance
# bseg <- stringr::str_split(bseg, pattern = ",")
# bseg <- as.numeric(unlist(bseg)[-1])
# bseg <- sum(bseg)
#
# df <- data.frame(
#   plan = obj$marker$`@attributes`$plan[1],
#   start = obj$marker$`@attributes`$start[1],
#   finish = obj$marker$`@attributes`$finish[1],
#   length = as.numeric(obj$marker$`@attributes`$length[1]),
#   time = as.numeric(obj$marker$`@attributes`$time[1]),
#   waypoint = nrow(coords),
#   cum_hill = htot, #total up and down
#   change_elev = hchng, # diff between start and end
#   dif_max_min = hmxmn, # diff between highest and lowest
#   up_tot = hup, # total climbing
#   down_tot = hdown, # total descending
#   av_incline = htot / as.numeric(obj$marker$`@attributes`$length[1]),
#   co2_saving = as.numeric(obj$marker$`@attributes`$grammesCO2saved[1]),
#   calories = as.numeric(obj$marker$`@attributes`$calories[1]),
#   busyness = bseg
# )
#
# # only the first coordinates object contains the coordinates...
# txt = obj$marker$`@attributes`$coordinates[[1]]
# obj$marker$`@attributes`$coordinates[[2]]
#
# obj2 = jsonlite::read_json("inst/extdata/res_json.json")
# length(obj2$marker[[2]]$`@attributes`$coordinates)
# length(obj2$marker[[1]]$`@attributes`$points)
# length(obj2$marker[[2]]$`@attributes`$points)
# length(obj2$marker[[3]]$`@attributes`$coordinates)
# txt2coords = function(txt) {
#   coords_split <- stringr::str_split(txt, pattern = " |,")[[1]]
#   matrix(as.numeric(coords_split), ncol = 2, byrow = TRUE)
# }
#
# coords = txt2coords(txt)
# nrow(coords)
# head(coords)
#
# coords1 = txt2coords(txt = obj2$marker[[2]]$`@attributes`$points)
# coords12 = txt2coords(obj$marker$`@attributes`$points[[2]])
#
# stplanr::n_vertices(rsp)
# stplanr::n_vertices(rsf) # fail
#
# coord_list = lapply(obj$marker$`@attributes`$points[-1], txt2coords)
# rsfm = sf::st_multilinestring(coord_list)
# microbenchmark::microbenchmark(
#   sf::st_multilinestring(coord_list)
# )
# # finding: slower than st_linestring but faster than sp
# rsflx = lapply(coord_list, sf::st_linestring)
# rsfl = sf::st_sfc(rsflx)
# library(sf)
# plot(rsfm, lwd = 5) # invalid
# plot(rsfl, col = "red")
#
# sapply(obj$marker$`@attributes`, length) # all attributes have nseg elements
# nseg = length(obj$marker$`@attributes`$busynance)
