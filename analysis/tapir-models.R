library('R.utils') # for withTimeout()
library('readxl')  # for read_xlsx()
library('ctmm')    # using the github version (0.6.1)
library('dplyr')   # for data wrangling
library('purrr')   # for vectorization
library('furrr')   # for parallel vectorization
library('tidyr')   # for data wrangling
library('ggplot2') # for plotting
theme_set(theme_bw() +
            theme(panel.grid = element_blank()))
N <- 74 # number of tapirs

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

# for local parallel computation (windows)
library('furrr') # for parallel computation
NCORES <- 4 # number of cores, not number of logical processors!
plan(multisession, workers = NCORES)

# import data and convert to `telemetry` format, then plot by position and time
atlantica <- read.csv('data/cleaned/atlantica.csv') %>% # degraded
  as.telemetry(timeformat = '%Y-%m-%d %H:%M', mark.rm = TRUE)

pantanal <- read.csv('data/cleaned/pantanal.csv') %>% # agriculture
  arrange(individual.local.identifier, timestamp) %>% # to avoid warning
  as.telemetry(timeformat = '%Y-%m-%d %H:%M:%S', mark.rm = TRUE)

cerrado <- read.csv('data/cleaned/cerrado.csv') %>% # reference
  as.telemetry(timeformat = '%Y-%m-%d %H:%M:%S', mark.rm = TRUE)

# add User Equivalent Range Error (UERE)
pantanal.calib <- read.csv('data/CALIBRATION_Pantanal.csv') %>% as.telemetry()
pantanal.uere <- uere.fit(pantanal.calib)
uere(pantanal) <- pantanal.uere

cerrado.calib <- read.csv('data/CALIBRATION_Cerrado.csv') %>% as.telemetry()
cerrado.uere <- uere.fit(cerrado.calib)
uere(cerrado) <- cerrado.uere

traits <-
  bind_rows(read_xlsx('data/RESULTS - SPATIAL ECOLOGY.xlsx',
                      sheet = 'ATLANTIC FOREST (11)',
                      range = 'A3:D13',
                      col_names = c('name', 'method', 'sex', 'age')) %>%
              mutate(region = 'atlantica'),
            read_xlsx('data/RESULTS - SPATIAL ECOLOGY.xlsx',
                      sheet = 'PANTANAL (46)',
                      range = 'A3:D48',
                      col_names = c('name', 'method', 'sex', 'age')) %>%
              mutate(region = 'pantanal'),
            read_xlsx('data/RESULTS - SPATIAL ECOLOGY.xlsx',
                      sheet = 'CERRADO (19)',
                      range = 'A3:D21',
                      col_names = c('name', 'method', 'sex', 'age')) %>%
              mutate(region = 'cerrado')) %>%
  mutate(name.short = map_chr(name,
                              function(x) substr(x,
                                                 gregexpr(' ', x)[[1]][1],
                                                 100)),
         name.short = gsub(' ', '', name.short),
         name.short = gsub('-', '', name.short))

# two individuals not in Cerrado dataset
filter(traits, name.short %in% c('SILVIO', 'SOFIA'))

# fit models ####
if(FALSE) {
  # for remote parallel computation (linux)
  NCORES <- 15
  Sys.setenv(MC_CORES = NCORES) # set max number of cores to NCORES
  Sys.getenv('MC_CORES')        # should return NCORES
  
  tapirs <-
    bind_rows(tibble(region = 'atlantica', name = names(atlantica)),
              tibble(region = 'pantanal', name = names(pantanal)),
              tibble(region = 'cerrado', name = names(cerrado))) %>%
    mutate(name.short = map_chr(name,
                                function(x) substr(x,
                                                   gregexpr('_', x)[[1]][2],
                                                   100)),
           name.short = gsub('_', '', name.short)) %>%
    left_join(select(traits, -region, -name), by = 'name.short') %>%
    # no map_***() available for telemetry, variogram, and ctmm objects
    mutate(data = map2(region, name,
                       function(x, y) get(x)[[y]]))
  
  # rename data classes to fix error that occurs during fitting
  for(i in 1:N) {
    d <- tapirs$data[[i]]
    if(! is.null(levels(d$class))){
      levels(d$class)[levels(d$class) == "Succeeded [HDOP] [vertical] [speed]"] <-
        "Succeeded [speed]"
      levels(d$class)[levels(d$class) == "QFP [HDOP] [vertical] [NA-speed]"] <-
        "QFP [NA-speed]"
      tapirs$data[[i]] <- d
      rm(d)
    }
  }
  
  tapirs <-
    mutate(tapirs,
           calib = region != 'atlantica',
           svf = map(data, variogram),
           theta0 = map(1:N,
                        function(i) ctmm.guess(data = data[[i]],
                                               CTMM = ctmm(error = calib[i]),
                                               variogram = svf[[i]],
                                               interactive = FALSE)))
  
  # not unsing map() because a single error loses all progress
  tictoc::tic() # 4.8 days in total (last few, >= 69, took the longest)
  for(i in 1:N) {
    tryCatch({ # to print errors as warnings and continue fitting models
      tapirs$model[i] <-
        list(ctmm.select(data = tapirs$data[[i]], CTMM = tapirs$theta0[[i]],
                         control = list(method = 'pNewton', cores = NCORES)))
      cat(i, '\n')
    }, error = function(e){cat("ERROR:",conditionMessage(e), "\n")})
  }
  tictoc::toc()
  saveRDS(tapirs, file = 'models/tapirs-calibrated.rds')
  
  # Nine warnings:
  # In ctmm.fit(data, GUESS, trace = trace2, ...) :
  # pREML failure: indefinite ML Hessian or divergent REML gradient.
} else {
  tapirs <- readRDS('models/tapirs-akdes.rds')
}

# plot of times
raw <-
  bind_rows(read.csv('data/1_ATLANTICFOREST_11.csv') %>%
              transmute(region = 'Mata Atlantica',
                        name = individual.local.identifier,
                        timestamp = timestamp),
            read.csv('data/2_PANTANAL_ERRORDATASET_FINAL.csv') %>%
              arrange(individual.local.identifier, timestamp) %>%
              transmute(region = 'Pantanal',
                        name = individual.local.identifier,
                        timestamp = timestamp),
            read.csv('data/3_CERRADO_ERRORDATASET_FINAL.csv') %>%
              transmute(region = 'Cerrado',
                        name = individual.local.identifier,
                        timestamp = timestamp)) %>%
  mutate(region = factor(region,
                         levels = c('Mata Atlantica', 'Pantanal', 'Cerrado')),
         name2 = lag(name),
         t = as.POSIXct(timestamp, format = '%Y-%m-%d %H:%M'),
         t2 = lag(t),
         diff.mins = if_else(name == name2, t - t2, NA_real_) %>% as.numeric(),
         diff.hours = diff.mins / 60,
         diff.hours.2d = if_else(diff.hours < 24 * 2, diff.hours, 24 * 2)) %>%
  as_tibble()

# times between measurements
ggplot(raw, aes(diff.hours.2d)) +
  facet_grid(region ~ .) +
  geom_density(bw = 0.5, fill = 'red', alpha = 0.5) +
  labs(x = 'Hours', y = 'Density',
       title = 'Time between measurements, to a max of 48 hours')
ggsave('figures/waiting-times-density.png', width = 5, height = 6)

save.plots <- function(i) {
  r <- tapirs$region[[i]]
  n <- tapirs$name[[i]]
  fun <- tapirs$svf[[i]]
  model <- tapirs$model[[i]]
  
  pdf(file = paste0('models/diagnostics/', r, '-', n, '.pdf'), width = 16,
      height = 9) # save to pdf
  layout(t(1:2)) # plot both diagnostic plots in one page
  diagn <- outlie(tapirs$data[[i]]) # data w diagn
  plot(diagn, units = FALSE) # speed vs distance diagnostic
  
  # plot best model at 3 different zooms
  layout(1)
  plot(fun, CTMM = model, col.CTMM = 'red', fraction = 5e-4)
  plot(fun, CTMM = model, col.CTMM = 'red', fraction = 0.01)
  plot(fun, CTMM = model, col.CTMM = 'red', fraction = 0.5)
  dev.off()
  print(i)
}

# open plots with SumatraPDF, use Ctrl+Shift+Right to move to next file
### for outlier analysis, make sure speeds are < 1 m/s = 3.6 km/h
### small clusters that deviate from the main cluster are ok
### remove measurement errors/outliers e.g. due to GPS error
future_map(1:N, save.plots, .options = furrr_options(seed = NULL))
dev.off() # make sure all pdf devices are closed

# withTimeout() cannot be used with map()
tictoc::tic() # ~ 3 minutes without weights, ~ 25 minutes with weights
for(i in 1:N) {
  tapirs$akde[[i]] <-
    withTimeout(akde(data = tapirs$data[[i]],
                     CTMM = tapirs$model[[i]],
                     # mata atlantica has VHF data => irregular lags
                     weights = ! tapirs$calib[i]),
                substitute = TRUE,
                timeout = 60 * 60 * 4, # warning if t > 4 hours for a single est
                onTimeout = 'warning')
  cat(i, '\n')
}
tictoc::toc()
#saveRDS(tapirs, file = 'models/tapirs-calibrated.rds')

plot.estimates <- function(ROW) {
  AKDE <- tapirs[ROW, 'akde'][[1]]
  DATA <- tapirs[ROW, 'data'][[1]]
  
  pdf(paste0('figures/akde/', tapirs[ROW, 'region'], '-', tapirs[ROW, 'name'],
             '.pdf'), width = 12, height = 6.75)
  plot(AKDE) # 95% quantile of home range distribution with 95% CIs
  plot(DATA, add = TRUE) # add datapoints
  dev.off()
}

# extract estimates
foo <- function(x) {
  cnames <- c('speed.est', 'speed.high', 'speed.low', 'tau.position.est', 'tau.position.high', 'tau.position.low',
              'tau.velocity.est', 'tau.velocity.high', 'tau.velocity.low')
  
  # extract estimates and CIs from summaries
  SUM <- summary(x, units = FALSE)$CI %>% as.data.frame()
  SUM <- mutate(SUM,
                param = rownames(SUM),
                param = substr(param, 2, 3)) %>% # tau causes issues
    dplyr::filter(!(grepl('r', param))) %>%
    mutate(param = case_when(param == '[p' ~ 'tau.position',
                             param == '[v' ~ 'tau.velocity',
                             param == 'pe' ~ 'speed'),
           param.h = paste0(param, '.high'),
           param.l = paste0(param, '.low'),
           param = paste0(param, '.est')) %>%
    pivot_wider(names_from = param.l, values_from = low) %>%
    pivot_wider(names_from = param, values_from = est) %>%
    pivot_wider(names_from = param.h, values_from = high) %>%
    colSums(na.rm = TRUE) %>%
    t() %>%
    as_tibble()
  NAs <- cnames[! cnames %in% colnames(SUM)]
  NAs
  if(length(NAs) > 0){
    SUM <- SUM %>%
      bind_cols(rep(NA_real_, length(NAs)) %>%
                  t() %>%
                  matrix(dimnames = list(NULL, NAs), nrow = 1) %>%
                  as_tibble())
  }
  SUM[cnames]
}
foo(tapirs$model[[1]])
tapirs <-
  select(tapirs, -speed.est, -tau.position, -tau.velocity, -x.df, -y.df) %>%
  bind_cols(map_dfr(tapirs$model, .f = foo) %>% # tau and speed parameters
              mutate(speed.low = speed.low * 86.4,
                     speed.est = speed.est * 86.4,
                     speed.high = speed.high * 86.4),
            map_dfr(1:N,
                    function(i) tapirs$akde[[i]]$DOF.area %>% # akde df
                      t() %>%
                      as.data.frame()) %>%
              rename(x.df = x, y.df = y))

# AKDE plots with data, open plots with SumatraPDF
with(tapirs, future_map(1:N, plot.estimates,
                        .options = furrr_options(seed = NULL)))
dev.off() # make sure all pdf devices are closed

# summary plots ####
tapirs <- mutate(tapirs,
                 adult = if_else(age %in% c('ADULT', 'ADULT-OLD'), 'Yes', 'No'))

# speed estimates (only one speed estimate in mata atlantica)
s.box <-
  tapirs %>%
  dplyr::select(region, name, low, est, high, age, sex, adult) %>%
  pivot_longer(low:high, names_to = 'parameter') %>%
  mutate(parameter = factor(parameter, levels = c('low', 'est', 'high')))%>%
  ggplot() +
  facet_grid(. ~ parameter) +
  scale_color_brewer(palette = 6, type = 'qual') +
  labs(x = NULL, y = 'Speed (km/day)') +
  theme(legend.position = 'top')
ggsave('figures/speed-boxplots.png',
       plot = s.box + geom_boxplot(aes(x = region, y = value)),
       width = 5, height = 4)
ggsave('figures/speed-boxplots-sex.png',
       plot = s.box + geom_boxplot(aes(x = region, y = value, color = sex)),
       width = 5, height = 4)
ggsave('figures/speed-boxplots-age.png',
       plot = s.box + geom_boxplot(aes(x = region, y = value, color = adult)),
       width = 5, height = 4)

# tau estimates (only one speed estimate in mata atlantica)
t.box <- tapirs %>%
  dplyr::select(region, name, tau.position, tau.velocity, sex, adult) %>%
  pivot_longer(tau.position:tau.velocity, names_to = 'parameter') %>%
  mutate(parameter = case_when(parameter == 'tau.position' ~ 'Position',
                               parameter == 'tau.velocity' ~ 'Velocity')) %>%
  ggplot() +
  facet_wrap(. ~ parameter, scales = 'free_y') +
  scale_y_log10() +
  labs(x = NULL, y = expression(Autocoerrelation~parameter~(tau))) +
  scale_color_brewer(palette = 6, type = 'qual') +
  theme(legend.position = 'top')
ggsave('figures/tau-boxplots.png',
       plot = t.box + geom_boxplot(aes(x = region, y = value)),
       width = 5, height = 4)
ggsave('figures/tau-boxplots-sex.png',
       plot = t.box + geom_boxplot(aes(x = region, y = value, color = sex)),
       width = 5, height = 4)
ggsave('figures/tau-boxplots-age.png',
       plot = t.box + geom_boxplot(aes(x = region, y = value, color = adult)),
       width = 5, height = 4)

# home range degrees of freedom
df.box <- tapirs %>%
  select(region, name, x.df, y.df, sex, adult) %>%
  pivot_longer(c(x.df, y.df), names_to = 'parameter') %>%
  ggplot() +
  facet_wrap(. ~ parameter, scales = 'free_y') +
  labs(x = NULL, y = 'AKDE degrees of freedom') +
  scale_color_brewer(palette = 6, type = 'qual') +
  theme(legend.position = 'top')
ggsave('figures/adke-df-boxplots.png',
       plot = df.box + geom_boxplot(aes(x = region, y = value)),
       width = 5, height = 4)
ggsave('figures/adke-df-boxplots-sex.png',
       plot = df.box + geom_boxplot(aes(x = region, y = value, color = sex)),
       width = 5, height = 4)
ggsave('figures/adke-df-boxplots-age.png',
       plot = df.box + geom_boxplot(aes(x = region, y = value, color = adult)),
       width = 5, height = 4)

# home range comparisons and figures
pdf('figures/akde/all-atlantica.pdf', width = 8, height = 8)
plot(tapirs$akde[1:11])
dev.off()
pdf('figures/akde/all-pantanal.pdf', width = 8, height = 8)
plot(tapirs$akde[12:57])
dev.off()
pdf('figures/akde/all-cerrado.pdf', width = 8, height = 8)
plot(tapirs$akde[58:74])
dev.off()

tapirs <-
  mutate(tapirs,
         region.lab = case_when(region == 'atlantica' ~ 'Mata Atlantica',
                                region == 'pantanal' ~ 'Pantanal',
                                region == 'cerrado' ~ 'Cerrado'),
         # use units = FALSE to force SI units
         bind_cols(map_dfr(tapirs$akde,
                           function(x) summary(x, units = FALSE)$CI %>%
                             as_tibble()) %>%
                     transmute(area.low = low / 1e6,
                               area.est = est / 1e6,
                               area.high = high / 1e6))) # convert m^2 to km^2
#saveRDS(tapirs, 'models/tapirs-final.rds')

# Figure 2 ####
# add correct averages for speeds and tau parameters ###########################
tap <-
  bind_rows(
    tapirs,
    bind_cols(
      name = c('Mata Atlantica', 'Pantanal', 'Cerrado', 'Overall'),
      region.lab = c('Mata Atlantica', 'Pantanal', 'Cerrado', 'Overall'),
      bind_rows(
        meta(tapirs$akde[tapirs$region == 'atlantica'], plot = FALSE)[1, ],
        meta(tapirs$akde[tapirs$region == 'pantanal'], plot = FALSE)[1, ],
        meta(tapirs$akde[tapirs$region == 'cerrado'], plot = FALSE)[1, ],
        meta(tapirs$akde, plot = FALSE)[1, ]) %>%
        rename(area.low = low, area.est = est, area.high = high),
      group_by(tapirs, region) %>%
        summarize(tau.pos.est = mean(tau.position.est, na.rm = TRUE),
                  tau.pos.sd = sd(tau.position.est, na.rm = TRUE) / sqrt(n()),
                  tau.vel.est = mean(tau.velocity.est, na.rm = TRUE),
                  tau.vel.sd = sd(tau.velocity.est, na.rm = TRUE) / sqrt(n()),
                  spe.est = mean(speed.est, na.rm = TRUE),
                  spe.sd = sd(speed.est, na.rm = TRUE) / sqrt(n())) %>%
        rename(tau.position.est = tau.pos.est,
               tau.velocity.est = tau.vel.est,
               speed.est = spe.est) %>%
        mutate(tau.position.low = tau.position.est - 1.96 * tau.pos.sd,
               tau.position.high = tau.position.est + 1.96 * tau.pos.sd,
               tau.velocity.low = tau.velocity.est - 1.96 * tau.vel.sd,
               tau.velocity.high = tau.velocity.est + 1.96 * tau.vel.sd,
               speed.low = speed.est - 1.96 * spe.sd,
               speed.high = speed.est + 1.96 * spe.sd) %>%
        bind_rows(
          group_by(tapirs) %>%
            summarize(tau.pos.est = mean(tau.position.est, na.rm = TRUE),
                      tau.pos.sd = sd(tau.position.est, na.rm = TRUE)/sqrt(n()),
                      tau.vel.est = mean(tau.velocity.est, na.rm = TRUE),
                      tau.vel.sd = sd(tau.velocity.est, na.rm = TRUE)/sqrt(n()),
                      spe.est = mean(speed.est, na.rm = TRUE),
                      spe.sd = sd(speed.est, na.rm = TRUE) / sqrt(n())) %>%
            rename(tau.position.est = tau.pos.est,
                   tau.velocity.est = tau.vel.est,
                   speed.est = spe.est) %>%
            mutate(tau.position.low = tau.position.est - 1.96 * tau.pos.sd,
                   tau.position.high = tau.position.est + 1.96 * tau.pos.sd,
                   tau.velocity.low = tau.velocity.est - 1.96 * tau.vel.sd,
                   tau.velocity.high = tau.velocity.est + 1.96 * tau.vel.sd,
                   speed.low = speed.est - 1.96 * spe.sd,
                   speed.high = speed.est + 1.96 * spe.sd) %>%
            select(-tau.pos.sd, -tau.vel.sd, -spe.sd)) %>%
        select(-tau.pos.sd, -tau.pos.sd, -tau.vel.sd, -tau.vel.sd))) %>%
      mutate(
        name = factor(name,
                      levels = c(unique(tapirs$name), 'Mata Atlantica',
                                 'Pantanal', 'Cerrado', 'Overall')),
        region.lab = factor(region.lab,
                            levels = c('Mata Atlantica', 'Pantanal', 'Cerrado',
                                       'Overall')),
        average = name %in% c('Mata Atlantica','Pantanal','Cerrado','Overall'))

# 2a) meta() of areas
p.areas <-
  ggplot(tap) +
  geom_segment(aes(x = area.low, xend = area.high, y = name, yend = name,
                   color = region.lab), lwd = 2) +
  geom_point(aes(x = area.est, y = name, shape = average), col = 'black', size = 1.5) +
  geom_point(aes(x = area.est, y = name, shape = average), col = 'white', size = 1) +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c(pal[1:3], 'black')) +
  scale_y_discrete(limits = rev,
                   labels = c('Means', 'Cerrado', 'Pantanal', 'Mata Atlantica'),
                   breaks = c('Mata Atlantica', 'CE_15_KURUKA',
                              'PA_33_GABRIELA', 'AF_14_JAMESBOND')) +
  theme(legend.position = 'none', axis.ticks.y = element_blank()) +
  labs(x = bquote('Estimated 95% home range area'~(km^2)),
       y = NULL); p.areas

# 2b) home range crossing times
p.tau.pos <-
  ggplot(tap) +
  geom_segment(aes(x = tau.position.low / (60^2 * 24),
                   xend = tau.position.high / (60^2 * 24), y = name, yend = name,
                   color = region.lab), lwd = 2) +
  geom_point(aes(x = tau.position.est / (60^2 * 24), y = name, shape = average),
             col = 'black', size = 1.5) +
  geom_point(aes(x = tau.position.est / (60^2 * 24), y = name, shape = average),
             col = 'white', size = 1) +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c(pal[1:3], 'black')) +
  scale_y_discrete(limits = rev,
                   labels = c('Means', 'Cerrado', 'Pantanal', 'Mata Atlantica'),
                   breaks = c('Mata Atlantica', 'CE_15_KURUKA',
                              'PA_33_GABRIELA', 'AF_14_JAMESBOND')) +
  theme(legend.position = 'none', axis.ticks.y = element_blank()) +
  labs(x = 'Range crossing time (days)', y = NULL); p.tau.pos

# 2c) velocity autocorrelation timescale
p.tau.vel <-
  ggplot(tap) +
  geom_segment(aes(x = tau.velocity.low / 60^2,
                   xend = tau.velocity.high / 60^2, y = name, yend = name,
                   color = region.lab), lwd = 2) +
  geom_point(aes(x = tau.velocity.est / 60^2, y = name, shape = average),
             col = 'black', size = 1.5) +
  geom_point(aes(x = tau.velocity.est / 60^2, y = name, shape = average),
             col = 'white', size = 1) +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c(pal[1:3], 'black')) +
  scale_y_discrete(limits = rev,
                   labels = c('Means', 'Cerrado', 'Pantanal', 'Mata Atlantica'),
                   breaks = c('Mata Atlantica', 'CE_15_KURUKA',
                              'PA_33_GABRIELA', 'AF_14_JAMESBOND')) +
  theme(legend.position = 'none', axis.ticks.y = element_blank()) +
  labs(x = 'Directional persistence timescale (hours)', y = NULL); p.tau.vel

# 2d) mean movement speeds
p.speeds <-
  ggplot(tap) +
  geom_segment(aes(x = speed.low, xend = speed.high,
                   y = name, yend = name, color = region.lab), lwd = 2,
               show.legend = FALSE) +
  geom_point(aes(x = speed.est, y = name, shape = average), col = 'black',
             size = 1.5, show.legend = FALSE) +
  geom_point(aes(x = speed.est, y = name, shape = average), col = 'white',
             size = 1, show.legend = FALSE) +
  scale_shape_manual(values = c(19, 17)) +
  scale_color_manual(values = c(pal[1:3], 'black')) +
  scale_y_discrete(limits = rev,
                   labels = c('Mata Atlantica', 'Pantanal', 'Cerrado', 'Means'),
                   breaks = c('AF_17_ESPERTA', 'PA_33_GABRIELA', 'CE_15_KURUKA',
                              'Mata Atlantica')) +
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = 'Estimated average speed (km/day)', y = NULL); p.speeds

cowplot::plot_grid(p.areas,
                   p.tau.pos,
                   p.tau.vel,
                   p.speeds,
                   labels = c('a.', 'b.', 'c.', 'd.'),
                   label_fontface = 'plain',
                   ncol = 2, byrow = TRUE)

ggsave('figures/meta.png', width = 10, height = 8)
