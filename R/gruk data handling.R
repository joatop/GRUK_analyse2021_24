boks <- st_bbox(c(xmin = 194100, xmax = 263300, ymax = 6649700, ymin = 6549000), crs = st_crs(25833))

boks.Oslo <- st_bbox(c(xmin = 247100, xmax = 263300, ymax = 6649700, ymin = 6628000), crs = st_crs(25833))
boks.mid <- st_bbox(c(xmin = 234000, xmax = 257300, ymax = 6621000, ymin = 6590000), crs = st_crs(25833))
boks.south <- st_bbox(c(xmin = 194100, xmax = 204800, ymax = 6568700, ymin = 6545000), crs = st_crs(25833))

tm_shape(regnor, bbox=boks) +
  tm_fill('GID_0', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_graticules() +
  tm_shape(res.natopen.GRUK2) +
  tm_dots('Light2',midpoint=NA, palette=tmaptools::get_brewer_pal("RdYlGn", 7, plot = FALSE), scale=3, legend.show = FALSE) + # 
  tm_layout(main.title = "Light index (upper), natopen GRUK",legend.position = c("right", "bottom"), main.title.size=1.2) + 
  tm_add_legend(type = "fill", 
                col = c(tmaptools::get_brewer_pal("RdYlGn", 7, plot = FALSE),'grey'),
                labels = c("0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", 
                           "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0", ">1"),
                title = "index values")

tm_shape(regnor, bbox=boks.Oslo) +
  tm_fill('GID_0', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_graticules() +
  tm_shape(res.natopen.GRUK2) +
  tm_dots('Light2',midpoint=NA, palette=tmaptools::get_brewer_pal("RdYlGn", 7, plot = FALSE), scale=4, legend.show = FALSE) + # 
  tm_layout(main.title = "Light index (upper), natopen GRUK",legend.position = c("right", "bottom"), main.title.size=1.2) + 
  tm_add_legend(type = "fill", 
                col = c(tmaptools::get_brewer_pal("RdYlGn", 7, plot = FALSE),'grey'),
                labels = c("0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", 
                           "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0", ">1"),
                title = "index values")

tm_shape(regnor, bbox=boks.mid) +
  tm_fill('GID_0', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_graticules() +
  tm_shape(res.natopen.GRUK2) +
  tm_dots('Light2',midpoint=NA, palette=tmaptools::get_brewer_pal("RdYlGn", 7, plot = FALSE), scale=4, legend.show = FALSE) + # 
  tm_layout(main.title = "Light index (upper), natopen GRUK",legend.position = c("right", "bottom"), main.title.size=1.2) + 
  tm_add_legend(type = "fill", 
                col = c(tmaptools::get_brewer_pal("RdYlGn", 7, plot = FALSE),'grey'),
                labels = c("0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", 
                           "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0", ">1"),
                title = "index values")

tm_shape(regnor, bbox=boks.south) +
  tm_fill('GID_0', labels="", title="", legend.show = FALSE) + 
  tm_borders() +
  tm_graticules() +
  tm_shape(res.natopen.GRUK2) +
  tm_dots('Light2',midpoint=NA, palette=tmaptools::get_brewer_pal("RdYlGn", 7, plot = FALSE), scale=4, legend.show = FALSE) + # 
  tm_layout(main.title = "Light index (upper), natopen GRUK",legend.position = c("right", "bottom"), main.title.size=1.2) + 
  tm_add_legend(type = "fill", 
                col = c(tmaptools::get_brewer_pal("RdYlGn", 7, plot = FALSE),'grey'),
                labels = c("0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", 
                           "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0", ">1"),
                title = "index values")

#YlOrRd