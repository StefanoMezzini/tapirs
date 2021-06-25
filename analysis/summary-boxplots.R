library('ctmm')    # using the github version (0.6.1)
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('ggplot2') # for plotting
library('cowplot') # for grid of plots
theme_set(theme_bw() +
            theme(panel.grid = element_blank()))

tapirs <- readRDS('models/tapirs-final.rds') %>%
  mutate(adult = if_else(adult == 'Yes', 'Adult', 'Young'))

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

# speed estimates (only one speed estimate in mata atlantica)
s.box <-
  tapirs %>%
  select(region.lab, name, speed.est, age, sex, adult) %>%
  ggplot() +
  scale_color_manual('Region', values = pal) +
  scale_y_continuous('Speed (km/day)') +
  theme(legend.position = 'none')
spe.s <- s.box + geom_boxplot(aes(x = sex, y = speed.est, color = region.lab)) +
  scale_x_discrete(NULL, labels = c('Female', 'Male'))
spe.a <- s.box + geom_boxplot(aes(x = adult, y = speed.est, color = region.lab)) +
  scale_x_discrete(NULL)

# tau estimates (only one speed estimate in mata atlantica)
t.box <-
  tapirs %>%
  select(region.lab, name, area.est, sex, adult) %>%
  ggplot() +
  labs(y = expression(Estiated~home~range~(km^2))) +
  scale_color_manual('Region', values = pal) +
  theme(legend.position = 'none')
hr.s <- t.box +
  geom_boxplot(aes(x = sex, y = area.est, color = region.lab)) +
  scale_x_discrete(NULL, labels = c('Female', 'Male'))
hr.a <- t.box + geom_boxplot(aes(x = adult, y = area.est, color = region.lab)) +
  scale_x_discrete(NULL)

plot_grid(get_legend(spe.s + theme(legend.position = 'top')),
          plot_grid(spe.s, spe.a, hr.s, hr.a, ncol = 2),
          ncol = 1, rel_heights = c(0.1, 1))

ggsave('figures/boxplots.png', width = 6.86, height = 4.5, dpi = 300,
       bg = 'white', scale = 1.3)
