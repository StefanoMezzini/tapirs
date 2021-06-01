library('readxl') # for read_xlsx()
library('ctmm') # using the github version (0.6.1)
library('dplyr') # for data wrangling
library('purrr') # for vectorization
library('tidyr') # for data wrangling

if(FALSE){
  # create new files for "cleaned" data ####
  # import
  atl <- read.csv('data/1_ATLANTICFOREST_11.csv') %>%
    mutate(Outlier = FALSE, row = 1:n())
  pan <- read.csv('data/2_PANTANAL_ERRORDATASET_FINAL.csv') %>%
    arrange(individual.local.identifier, timestamp) %>% # to avoid warning
    mutate(Outlier = Outlier, row = 1:n())
  cer <- read.csv('data/3_CERRADO_ERRORDATASET_FINAL.csv') %>%
    mutate(Outlier = Outlier, row = 1:n())
  
  # write new files (beware of over-writing!)
  # write.csv(atl, 'data/cleaned/atlantica.csv', row.names = TRUE)
  # write.csv(pan, 'data/cleaned/pantanal.csv', row.names = TRUE)
  # write.csv(cer, 'data/cleaned/cerrado.csv', row.names = TRUE)
  
  # remove original datasets
  rm(atl, pan, cer)
}

# function to plot diagnostics
diagn <- function(i, region) {
  d <- get(paste0(region, '.tel'))
  layout(t(1:2))
  o <- outlie(d[[i]])
  title(names(d)[[i]])
  plot(o, units = FALSE)
  layout(1)
  suppressWarnings(return(as_tibble(o)))
}

# Mata Atlantica ####
# 0 initial outliers, added 8
atl.df <- read.csv('data/cleaned/atlantica.csv')
atl.tel <- as.telemetry(atl.df, timeformat = '%Y-%m-%d %H:%M',
                        mark.rm = TRUE, keep = c('row', 'Outlier'))
atl.tib <- bind_rows(map(1:length(atl.tel),
                         function(i) as_tibble(atl.tel[[i]]) %>%
                           mutate(name = names(atl.tel)[[i]])))
unique(warnings())

# check for outliers using "cleaning" section

out <- c(2089, 3429, 3426, 4070, 3874, 4030, 4041, 3548)
new <- mutate(atl.df, Outlier = if_else(X %in% out, TRUE, Outlier))
write.csv(new, 'data/cleaned/atlantica.csv', row.names = FALSE)
rm(out, new)

rm(atl, atl.df, atl.tel, atl.tib, new)

# Pantanal ####
# 912 initial outliers, added 2
pan.df <- read.csv('data/cleaned/pantanal.csv')
pan.tel <- arrange(pan.df, individual.local.identifier, timestamp) %>%
  as.telemetry(timeformat = '%Y-%m-%d %H:%M:%S',
               mark.rm = TRUE, keep = c('row', 'Outlier'))
pan.tib <- bind_rows(map(1:length(pan.tel),
                         function(i) as_tibble(pan.tel[[i]]) %>%
                           mutate(name = names(pan.tel)[[i]])))
unique(warnings())

# add User Equivalent Range Error (UERE)
pantanal.calib <- read.csv('data/CALIBRATION_Pantanal.csv') %>% as.telemetry()
pantanal.uere <- uere.fit(pantanal.calib)
uere(pan.tel) <- pantanal.uere

# most datasets have few rows, so open the diagnostic files with SumatraPDF

out <- c(108942, 123977)
new <- mutate(pan.df, Outlier = if_else(X %in% out, TRUE, Outlier))
write.csv(new, 'data/cleaned/pantanal.csv', row.names = FALSE)
rm(out, new)

# Cerrado ####
# 193 initial outliers, added 
cer.df <- read.csv('data/cleaned/cerrado.csv')
cer.tel <- as.telemetry(cer.df, timeformat = '%Y-%m-%d %H:%M:%S',
                        mark.rm = TRUE, keep = c('row', 'Outlier'))
cer.tib <- bind_rows(map(1:length(cer.tel),
                         function(i) {
                           as_tibble(cer.tel[[i]]) %>%
                             mutate(name = names(cer.tel)[[i]])
                         }))
unique(warnings())

# add User Equivalent Range Error (UERE)
cerrado.calib <- read.csv('data/CALIBRATION_Cerrado.csv') %>% as.telemetry()
cerrado.uere <- uere.fit(cerrado.calib)
uere(cer.tel) <- cerrado.uere

# some very big datasets, it is faster to look at saved pdf plots

# no outliers found
# out <- c()
# new <- mutate(cer.df, Outlier = if_else(X %in% out, TRUE, Outlier))
# write.csv(new, 'data/cleaned/cerrado.csv', row.names = FALSE)
# rm(out, new)

# cleaning ####
o <- diagn(10, 'atl')
filter(atl.tib, name == names(pan.tel)[42]) %>%
  filter(x > - 1500,
         y < 1000) %>%
  select(name, x, y, row, Outlier)
filter(o) %>% filter(speed > 1) %>%
  left_join(mutate(atl.tib, t = as.numeric(timestamp)) %>%
              select(t, row))

# check speeds ####
max.speed <- function(i, region) {
  d <- get(paste0(region, '.tel'))
  d0 <- d[[i]]
  o <- outlie(d0, plot = FALSE)
  return(max(o$speed))
}

atl.speed <- map_dbl(1:length(atl.tel),
                     function(x) max.speed(i = x, region = 'atl'))
pan.speed <- map_dbl(1:length(pan.tel),
                     function(x) max.speed(i = x, region = 'pan'))
cer.speed <- map_dbl(1:length(cer.tel),
                     function(x) max.speed(i = x, region = 'cer'))
names(atl.tel)[which(atl.speed > 1)]
names(pan.tel)[which(pan.speed > 1)]
names(cer.tel)[which(cer.speed > 1)]
