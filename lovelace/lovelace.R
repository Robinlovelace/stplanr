# setup -------------------------------------------------------------------
## install.packages("stplanr")
library(stplanr)
if(dir.exists("vignettes/")) {
  wd_old <- setwd("vignettes/")
}
# stplanr_funs = ls("package:stplanr")
# sel_core = grep(pattern = "od_|^line_|route_", x = stplanr_funs)
# core_funs = stplanr_funs[sel_core]
# args(name = core_funs[1])
fun_table <- readr::read_csv("fun_table.csv")
knitr::kable(fun_table, caption = "Selection of functions for working with or generating OD, line and route data types.")

# stats19 example ---------------------------------------------------------
# Generate input data for paper (commented)
# dl_stats19() # download and extract stats19 road traffic casualty data
# ac <- read_stats19_ac()
# ca <- read_stats19_ca()
# ve <- read_stats19_ve()
#
# library(dplyr)
# purrr::map(ac, class)
# summary(ac$Time)
# ca_ac <- inner_join(ca, ac)
# ca_cycle <- ca_ac %>%
#  filter(Casualty_Severity == "Fatal" & !is.na(Latitude)) %>%
#  select(Age = Age_of_Casualty, Mode = Casualty_Type, Longitude, Latitude)
# ca_sp <- sp::SpatialPointsDataFrame(coords = ca_cycle[3:4], data = ca_cycle[1:2])
#
# data("route_network")
# sp::proj4string(ca_sp) <- sp::proj4string(route_network)
# bb <- bb2poly(route_network)
# sp::proj4string(bb) <- sp::proj4string(route_network)
# ca_local <- ca_sp[bb,]
#
if(!file.exists("reqfiles.RData"))
  download.file("https://github.com/Robinlovelace/stplanr/raw/rjournal-responses/lovelace/reqfiles.RData", "reqfiles.RData", mode = "wb")
load("reqfiles.RData")
rnet_buff_100 <- buff_geo(route_network, width = 100)
ca_buff <- ca_local[rnet_buff_100,]
bb <- bb2poly(route_network)
plot(bb, lty = 4)
plot(rnet_buff_100, col = "grey", add = TRUE)
points(ca_local, pch = 4)
points(ca_buff, cex = 3)


# od2line examples --------------------------------------------------------
data("flow", package = "stplanr")
head(flow[c(1:3, 12)])
data("cents", package = "stplanr")
as.data.frame(cents[1:3, -c(3,4)])
l <- od2line(flow = flow, zones = cents)

library(nycflights13)
airports_sf <- sf::st_as_sf(airports, coords = c("lon", "lat"), crs = 4326)
ny_buff <- geo_buffer(airports_sf[airports_sf$faa == "NYC",], dist = 1e6)
airports_near <- airports_sf[ny_buff,]
flights_near <- flights[flights$dest %in% airports_near$faa,]
flights_agg <- dplyr::group_by(flights_near, origin, dest) %>%
  dplyr::summarise(Flights = n())
flights_sf = od2line(flow = flights_agg, zones = airports_near)
plot(flights_sf, lwd = flights_sf$Flights / 1e3)

library(tmap)
tmap_mode("view")
pal <- viridis::viridis(n = 3, option = "C")
tm_shape(flights_sp) +
  tm_lines(lwd = "Flights", col = "Flights", scale = 12, n = 3, palette = "YlGnBu")

# route examples (not run, needs api keys) --------------------------------
# route_bl <- route_cyclestreet(from = "Bradford, Yorkshire", to = "Leeds, Yorkshire")
# route_c1_c2 <- route_cyclestreet(cents[1,], cents[2,])
# route_bl_raw <- route_cyclestreet(from = "Bradford", to = "Leeds", save_raw = TRUE)

# route network example ---------------------------------------------------
plot(route_network, lwd=0)
plot(l, lwd = l$All / 10, add = TRUE)
lines(routes_fast, col = "red")
routes_fast$All <- l$All
rnet <- overline(routes_fast, "All", fun = sum)
rnet$flow <- rnet$All / mean(rnet$All) * 3
plot(rnet, lwd = rnet$flow / mean(rnet$flow))

# graphhopper example -----------------------------------------------------
# ny2oaxaca1 <- route_graphhopper("New York", "Oaxaca", vehicle = "bike")
# ny2oaxaca2 <- route_graphhopper("New York", "Oaxaca", vehicle = "car")
# rbind(ny2oaxaca1@data, ny2oaxaca2@data)
# nytab = rbind(ny2oaxaca1@data, ny2oaxaca2@data)
# nytab = cbind(Mode = c("Cycle", "Car"), nytab)
# xtnyoa = xtable(nytab, caption = "Attribute data from the route\\_graphhopper function, from New York to Oaxaca, by cycle and car.", label = "tab:xtnyoa")
# print.xtable(xtnyoa, include.rownames = FALSE)
# plot(ny2oaxaca1)
# plot(ny2oaxaca2, add = TRUE, col = "red")
# ny2oaxaca1@data
# ny2oaxaca2@data

# watershed example -------------------------------------------------------
data_dir <- system.file("extdata", package = "stplanr")
unzip(file.path(data_dir, 'smallsa1.zip'))
unzip(file.path(data_dir, 'testcycleway.zip'))
sa1income <- rgdal::readOGR(".", "smallsa1")
testcycleway <- rgdal::readOGR(".", "testcycleway")
# Remove unzipped files
file.remove(list.files(pattern = "^(smallsa1|testcycleway).*"))

catch800m <- calc_catchment(
  polygonlayer = sa1income,
  targetlayer = testcycleway,
  calccols = c('Total'),
  distance = 800,
  projection = 'austalbers',
  dissolve = TRUE
)

plot(sa1income, col = "light grey")
plot(catch800m, col = rgb(1, 0, 0, 0.5), add = TRUE)
plot(testcycleway, col = "green", add = TRUE)

unzip(file.path(data_dir, 'sydroads.zip'))
sydroads <- rgdal::readOGR(".", "roads")
file.remove(list.files(pattern = "^(roads).*"))
sydnetwork <- SpatialLinesNetwork(sydroads)

netcatch800m <- calc_network_catchment(
  sln = sydnetwork,
  polygonlayer = sa1income,
  targetlayer = testcycleway,
  calccols = c('Total'),
  maximpedance = 800,
  distance = 100,
  projection = 'austalbers'
)

plot(sa1income, col = "light grey")
plot(catch800m, col = rgb(1, 0, 0, 0.5), add = TRUE)
plot(netcatch800m, col = rgb(0, 0, 1, 0.5), add = TRUE)
plot(testcycleway, col = "green", add = TRUE)


# modelling example -------------------------------------------------------
l$d_euclidean <- line_length(l)
l$d_rf <- routes_fast$length
## routes_slow <- line2route(l, route_cyclestreet, plan = "quietest")
l$d_rq <- routes_slow$length # quietest route distance
Q <- mean(l$d_rf / l$d_euclidean, na.rm = TRUE)
QDF <- mean(l$d_rq / l$d_rf, na.rm = TRUE)
Q
QDF
(QDFt <- mean(routes_slow$time / routes_fast$time, na.rm = TRUE))

## l$pwalk <- l$On.foot / l$All
## plot(l$d_euclidean, l$pwalk, cex = l$All / 50,
##   xlab = "Euclidean distance (m)", ylab = "Proportion of trips by foot")

par(mfrow = c(1, 2))
lgb <- sp::spTransform(l, CRSobj = CRS("+init=epsg:27700"))
l$d_euclidean <- rgeos::gLength(lgb, byid = T)
l$d_rf <- routes_fast@data$length
plot(l$d_euclidean, l$d_rf,
  xlab = "Euclidean distance", ylab = "Route distance")
abline(a = 0, b = 1)
abline(a = 0, b = 1.2, col = "green")
abline(a = 0, b = 1.5, col = "red")
l$pwalk <- l$On.foot / l$All
plot(l$d_euclidean, l$pwalk, cex = l$All / 50,
  xlab = "Euclidean distance (m)", ylab = "Proportion of trips by foot")

lm1 <- lm(pwalk ~ d_euclidean, data = l@data, weights = All)
lm2 <- lm(pwalk ~ d_rf, data = l@data, weights = All)
lm3 <- glm(pwalk ~ d_rf + I(d_rf^0.5),
           data = l@data, weights = All, family = quasipoisson(link = "log"))

## summary(lm1)
## summary(lm2)
## summary(lm3)

plot(l$d_euclidean, l$pwalk, cex = l$All / 50,
  xlab = "Euclidean distance (m)", ylab = "Proportion of trips by foot")
l2 <- data.frame(d_euclidean = 1:5000, d_rf = 1:5000)
lm1p <- predict(lm1, l2)
lm2p <- predict(lm2, l2)
lm3p <- predict(lm3, l2)
lines(l2$d_euclidean, lm1p)
lines(l2$d_euclidean, exp(lm2p), col = "green")
lines(l2$d_euclidean, exp(lm3p), col = "red")


# end ---------------------------------------------------------------------
if(exists(("wd_old"))) setwd(wd_old)
