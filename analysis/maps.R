library('sf')        # to import kml layers
library('geobr')     # for maps of Brazil
library('ctmm')      # using the github version (0.6.1)
library('raster')    # to import human footprint index raster
library('dplyr')     # for data wrangling
library('lubridate') # makes working with dates smoother
library('purrr')     # for functional mapping
library('move')      # for movement data
library('ggplot2')   # for plotting
library('ggmap')     # for maps
# library('magrittr')
theme_set(theme_bw())
N <- 74 # number of tapirs

tapirs <- readRDS('models/tapirs-final.rds') %>%
  mutate(proj = map(data, function(d) CRS(d@info$projection)),
         akde.df =
           map(1:N,
               function(i)
                 SpatialPolygonsDataFrame.UD(akde[[i]],
                                             proj4string = proj[[i]]) %>%
                 spTransform(CRS("+proj=longlat")) %>%
                               fortify()))

# plot AKDES: https://groups.google.com/g/ctmm-user/c/RlEF60S3mvg/m/NLzqBRWOBgAJ
# map example: https://methodsblog.com/2021/03/22/mapping-animal-movement-in-r-the-science-and-the-art/
#              https://github.com/pratikunterwegs/elemove/blob/master/R/04_plot_code.R

# Brazil maps
sa <- filter(spData::world, continent == 'South America')
#br <- read_state(code_state = 'all', year = 2020) %>% unionSpatialPolygons()
# amazon <- read_amazon(year = 2012, simplified = FALSE)

# human footprint index
hfi <- raster('data/hfi-layers/ml_hfi_v1_2019.nc') %>%
  crop(st_bbox(sa)) %>%
  rasterToPoints(spatial = TRUE) %>%
  data.frame() %>%
  fortify() %>%
  rename(hfi = X__xarray_dataarray_variable__)

ggplot() +
  geom_raster(aes(x, y, fill = hfi), hfi) +
  geom_sf(data = sa, size = 0.15, color = 'grey70', fill = 'transparent') +
  theme(legend.position = 'top') +
  scale_fill_viridis_c('Human Footprint Index', option = 'B')

# import raw movement data
raw <-
  map_dfr(1:N,
          function(i) {
            mutate(suppressWarnings(as_tibble(tapirs$data[[i]])),
                   region = tapirs$region[i],
                   region = case_when(region == 'atlantica' ~ 'Mata Atlantica',
                                      region == 'pantanal' ~ 'Pantanal',
                                      region == 'cerrado' ~ 'Cerrado'),
                   name = tapirs$name.short[i]) %>%
              dplyr::select(timestamp, longitude, latitude, t, x, y, region,
                            name)
          }) %>%
  mutate(day = date(timestamp))

# wide temporal range within Mata Atlantica and Pantanal
group_by(raw, region) %>%
  summarize(min = min(day), max = max(day)) %>%
  mutate(range = max - min)

# wide temporal range between animals
group_by(raw, name, region) %>%
  mutate(y = decimal_date(day)) %>%
  summarize(min = min(y), max = max(y)) %>%
  mutate(range = max - min) %>%
  ggplot(aes(region, range, color = region)) +
  geom_point() +
  labs(x = NULL, 'Years of tracking')

# maps ####
# locator inset map
inset <-
  ggplot() +
  geom_raster(aes(x, y, fill = hfi), hfi) +
  geom_sf(data = sa, size = 0.15, color = 'grey70', fill = 'transparent') +
  geom_sf(data = filter(sa, name_long == 'Brazil'), size = 0.4,
          color = 'white', fill = 'transparent') +
  geom_point(aes(longitude, latitude, shape = region),
             group_by(raw, region) %>%
               summarize(longitude = median(longitude),
                         latitude = median(latitude)),
             size = 2.5, color = 'grey90', show.legend = FALSE) +
  coord_sf(xlim = c(-90, -30), ylim = c(-35, 11)) +
  scale_shape_manual(values = c('C', 'M', 'P')) +
  theme_dark() +
  theme(legend.position = 'top', panel.grid = element_blank()) +
  scale_fill_viridis_c('Human Footprint Index', option = 'B') +
  labs(x = 'Longitude', y = 'Latitude')

# Mata Atlantica
ggmap(atl) +
  geom_path(aes(longitude, latitude, group = name),
            alpha = 0.3, filter(raw, region == 'Mata Atlantica')) +
  geom_point(aes(longitude, latitude), alpha = 0.1,
             filter(raw, region == 'Mata Atlantica'))

# layer plots ####
atl.akdes <- bind_rows(tapirs$akde.df) %>% filter(grepl('AF_', group))
pan.akdes <- bind_rows(tapirs$akde.df) %>% filter(grepl('PA_', group))
cer.akdes <- bind_rows(tapirs$akde.df) %>% filter(grepl('CE_', group))

plot.akde <- function(reg) {
  if(reg == 'Mata Atlantica') {
    r <- 'AF_'
    BOX <- c(left = -52.6, bottom = -22.7, right = -52, top = -22.3)
  } else if(reg == 'Pantanal') {
    r <- 'PA_'
    BOX <- c(left = -56.0, bottom = -19.45, right = -55.6, top = -19.15)
  } else {
    r <- 'CE_'
    BOX <- c(left = -54, bottom = -21.85, right = -53.4, top = -21.45)
  }
  akdes <- bind_rows(tapirs$akde.df) %>%
    filter(grepl(r, group), grepl('est', group))
  
  hfi.cropped <- filter(hfi,
                        x >= BOX['left'], x <= BOX['right'],
                        y >= BOX['bottom'], y <= BOX['top'])
  
  ggplot() +
    geom_raster(aes(x, y, fill = hfi), hfi.cropped) +
    geom_polygon(aes(x = long, y = lat, group = group), fill = 'transparent',
                 alpha = 0.15, color = 'white', akdes) +
    scale_fill_viridis_c('Human Footprint Index', option = 'B') +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = 'Longitude', y = 'Latitude') +
    theme(legend.position = 'none',
          panel.background = element_rect('grey'),
          panel.grid = element_line('grey'))
}

atl.plt <- plot.akde('Mata Atlantica')
pan.plt <- plot.akde('Pantanal')
cer.plt <- plot.akde('Cerrado')

fig1 <-
  cowplot::plot_grid(inset, atl.plt, pan.plt, cer.plt, ncol = 1,
                     rel_heights = c(1.3, 1, 1, 1),
                     labels = c(NA, 'M.', 'P.', 'C.'), label_fontface = 'plain')
ggsave('figures/map.png', plot = fig1, width = 5, height = 15)

# regional density plots
get_map(c(left = -52.5, bottom = -22.7, right = -52.1, top = -22.35),
        zoom = 12) %>%
  ggmap() +
  stat_bin_2d(aes(longitude, latitude, fill = after_stat(density),group = name),
              filter(raw, region == 'Mata Atlantica'), binwidth = c(5e-3, 5e-3)) +
  scale_fill_viridis_c()

ggmap(atl) +
  stat_bin_2d(aes(longitude, latitude, fill = after_stat(density)),
              filter(raw, region == 'Mata Atlantica'), binwidth = c(5e-3, 5e-3)) +
  scale_fill_viridis_c()
ggmap(pan) +
  stat_bin_2d(aes(longitude, latitude, fill = after_stat(density)),
              filter(raw, region == 'Pantanal'), binwidth = c(3e-3, 3e-3)) +
  scale_fill_viridis_c()
ggmap(cer) +
  stat_bin_2d(aes(longitude, latitude, fill = after_stat(density),group = name),
              filter(raw, region == 'Cerrado'), binwidth = c(5e-3, 5e-3)) +
  scale_fill_viridis_c()

# animated plots ####
# cannot project data ####
library('moveVis')
library('gganimate')

jb.mv <- df2move(filter(raw, name == 'JAMESBOND')[1:10, ],
                 proj = tapirs$data[[5]]@info$projection, # UNSUPPORTED
                 # proj = '+proj=tpeqd +lat_1=-22.5274467735988 +lon_1=-52.3538856952028 +lat_2=-22.5972064330171 +lon_2=-52.2185466037466 +x_0=0 +y_0=0 +datum=WGS84',
                 x = 'longitude', y = 'latitude', time = 'timestamp') %>%
  align_move(res = 'min')

# create spatial frames with a OpenStreetMap watercolour map
frames <-
  frames_spatial(jb.mv, path_colours = 'red', map_service = 'osm',
                 map_type = 'watercolor', alpha = 0.5) %>%
  add_labels(x = 'Longitude', y = 'Latitude') %>%
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(m, type = 'label') %>%
  add_progress()

jb <-
  get_map(c(left = -52.2, bottom = -22.635, right = -52.15, top = -22.6),
          zoom = 12) %>%
  ggmap() +
  geom_point(aes(longitude, latitude, group = timestamp),
             filter(raw, name == 'JAMESBOND')) +
  transition_time(as.POSIXct(timestamp)) + # use decdate?
  labs(title = 'Date: {frame_time}')

animate(jb, duration = 8)
