library('sf')        # to import kml layers
library('geobr')     # for maps of Brazil
library('ctmm')      # using the github version (0.6.1)
library('raster')    # to import human footprint index raster
library('dplyr')     # for data wrangling
library('tidyr')     # for tibble pivoting functions
library('lubridate') # makes working with dates smoother
library('purrr')     # for functional mapping
library('move')      # for movement data
library('ggplot2')   # for plotting
library('ggmap')     # for maps
library('ggsn')      # for map scalebar and north arrow
library('cowplot')   # for overlapping plots & plot grids
theme_set(theme_bw() +
            theme(text = element_text(size = 14)))
N <- 74 # number of tapirs

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

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

# spatial elements
sa <- filter(spData::world,
             grepl('America', continent),
             ! name_long %in% c('Canada', 'United States', 'Greenland',
                                'Mexico'))
#br <- read_state(code_state = 'all', year = 2020) %>% unionSpatialPolygons()
# amazon <- read_amazon(year = 2012, simplified = FALSE)

# human footprint index
hfi <- raster('data/hfi-layers/ml_hfi_v1_2019.nc') %>%
  crop(st_bbox(sa)) %>%
  rasterToPoints() %>%
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
                   region =  case_when(region == 'atlantica' ~ 'Mata Atlantica',
                                       region == 'pantanal' ~ 'Pantanal',
                                       region == 'cerrado' ~ 'Cerrado'),
                   name = tapirs$name[i],
                   name.short = tapirs$name.short[i]) %>%
              dplyr::select(timestamp, longitude, latitude, t, x, y, region,
                            name, name.short)
          }) %>%
  mutate(day = date(timestamp))

# extents of each group
boxes <- group_by(raw, region) %>%
  summarize(xmin = min(longitude),
            ymin = min(latitude),
            xmax = max(longitude),
            ymax = max(latitude),
            .groups = 'drop')

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
  # add satellite image of BR to have water as a background # # # # # # # # # #
  geom_raster(aes(x, y, fill = hfi), hfi) + # geom_tile() needs too much memory
  geom_sf(data = sa, size = 0.25, color = 'grey70', fill = 'transparent') +
  geom_sf(data = filter(sa, name_long == 'Brazil'), size = 0.4,
          color = 'white', fill = 'transparent') +
  geom_point(aes(longitude, latitude), size = 2.5, color = 'white', pch = 0,
             group_by(raw, region) %>%
               summarize(longitude = median(longitude),
                         latitude = median(latitude))) +
  coord_sf(xlim = c(-90, -30), ylim = c(-35, 11)) +
  scale_color_manual('Region', values = c(pal[1:3], 'black')) +
  theme(legend.position = 'top', panel.grid = element_blank(),
        panel.background = element_rect(fill = 'dodgerblue4'),
        legend.key.width = unit(0.5, 'in')) +
  scale_fill_viridis_c('ml-HFI', option = 'B', limits = c(0, 1),
                       labels = c('0', '0.25', '0.50', '0.75', '1')) +
  labs(x = 'Longitude', y = 'Latitude')

plot.data <- function(reg) {
  if(reg == 'Mata Atlantica') {
    r <- 'AF_'
    BOX <- c(left = -52.6, bottom = -22.7, right = -52, top = -22.3)
  } else if(reg == 'Pantanal') {
    r <- 'PA_'
    BOX <- c(left = -56.0, bottom = -19.45, right = -55.6, top = -19.15)
  } else if(reg == 'Cerrado'){
    r <- 'CE_'
    BOX <- c(left = -54, bottom = -21.85, right = -53.4, top = -21.45)
  } else {
    stop('invalid region name')
  }
  d <- filter(raw, region == reg)
  MAP <- get_map(BOX, zoom = 11)
  scalebar.loc <- 0
  
  ggmap(MAP) +
    geom_point(aes(x = longitude, y = latitude, color = name), d, alpha = 0.1,
               lwd = 0.5, show.legend = FALSE) +
    scale_color_viridis_d(option = 'B') +
    labs(x = 'Longitude', y = 'Latitude', title = reg) +
    theme_map() +
    scalebar(x.min = BOX['left'],
             x.max = if(reg == 'Pantanal') BOX['right'] - 0.04 else
               BOX['right'] - 0.05,
             y.min = BOX['bottom'] + 0.03, y.max = BOX['top'] - 0.03,
             dist = 10, dist_unit = 'km', transform = TRUE, border.size = 0.01,
             st.size = 2, location = 'topright', height = -0.05, st.dist = 0.1)+
    theme(panel.border = element_rect(colour = 'black', fill = 'transparent'))
}
plot.data('Mata Atlantica') + theme_bw()
plot.data('Pantanal') + theme_bw()
plot.data('Cerrado') + theme_bw()

br.map <-
  plot_grid(inset,
            plot_grid(plot.data('Pantanal'), plot.data('Cerrado'),
                      plot.data('Mata Atlantica'), nrow = 1, align = 'right',
                      scale = 0.85, label_fontface = 'plain'),
            ncol = 1, rel_heights = c(0.75, 0.25))
ggsave('figures/data-map.png', plot = br.map, width = 6.86, height = 8,
       dpi = 300, bg = 'white')
beepr::beep()

# home range plots ####
atl.akdes <- bind_rows(tapirs$akde.df) %>% filter(grepl('AF_', group))
pan.akdes <- bind_rows(tapirs$akde.df) %>% filter(grepl('PA_', group))
cer.akdes <- bind_rows(tapirs$akde.df) %>% filter(grepl('CE_', group))

plot.akde <- function(reg) {
  # use BOXes in `if` statement if using hfi raster
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
  
  # box expansion factor 
  k <- case_when(reg == 'Pantanal' ~ 0.6,
                 reg == 'Cerrado' ~ 0.1,
                 reg == 'Mata Atlantica' ~ 0.5)
  
  BOX <- akdes %>%
    summarize(left = min(long),
              bottom = min(lat),
              right = max(long),
              top = max(lat)) %>%
    
    # expand boxes slightly
    mutate(left = left - k * (right - left),
           right = right + k * (right - left),
           bottom = bottom - 0.1 * (top - bottom),
           top = top + 0.1 * (top - bottom)) %>%
    unlist()
  
  MAP <- get_map(BOX, zoom = 11)
  # hfi.cropped <- filter(hfi,
  #                       x >= BOX['left'], x <= BOX['right'],
  #                       y >= BOX['bottom'], y <= BOX['top'])
  
  ggmap(MAP) +
    # geom_raster(aes(x, y, fill = hfi), hfi.cropped) +
    geom_polygon(aes(x = long, y = lat, fill = group), color = 'black',
                 alpha = 0.5, akdes, lwd = 0.15) +
    scale_fill_viridis_d('Human Footprint Index', option = 'B') +
    labs(x = 'Longitude', y = 'Latitude', title = reg) +
    scalebar(x.min = BOX['left'],
             x.max = if(reg == 'Pantanal') BOX['right'] - 0.02 else
               BOX['right'] - 0.05,
             y.min = BOX['bottom'] + 0.03, y.max = BOX['top'] - 0.02,
             dist = 5, dist_unit = 'km', transform = TRUE, border.size = 0.01,
             st.size = 2, location = 'topright', height = -0.05, st.dist = 0.1)+
    theme(panel.border = element_rect(colour = 'black', fill = 'transparent'),
          legend.position = 'none')
}

hr.map <- plot_grid(plot.akde('Mata Atlantica'), plot.akde('Pantanal'),
                    plot.akde('Cerrado'), ncol = 1)
ggsave('figures/hr-map.png', plot = hr.map, width = 3.23, height = 6, scale = 2,
       bg = 'white', dpi = 300)

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
