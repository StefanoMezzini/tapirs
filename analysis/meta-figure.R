library('ctmm')    # using the github version (0.6.1)
library('dplyr')   # for data wrangling
library('ggplot2') # for plotting
library('cowplot') # for plot grids
theme_set(theme_bw() +
            theme(text = element_text(size = 14),
                  panel.grid = element_blank(),
                  legend.position = 'none',
                  axis.ticks.y = element_blank(),
                  # axis.text.y = element_text(angle = 90),
                  axis.text.y = element_blank()))

# color palette
pal <- c('#4477AA', '#ff8c00', '#66CCEE', '#009900',
         '#CCBB44', '#EE6677', '#AA3377', '#BBBBBB')

# add correct averages for speeds and tau parameters ###########################
tapirs <- readRDS('models/tapirs-final.rds') %>%
  mutate(region.lab = if_else(region.lab == 'Mata Atlantica', 'Atlantic forest',
                              region.lab))
tap <-
  tapirs %>%
  bind_rows(
    bind_cols(
      name = c('Atlantic forest', 'Pantanal', 'Cerrado', 'Overall'),
      region.lab = c('Atlantic forest', 'Pantanal', 'Cerrado', 'Overall'),
      bind_rows(
        meta(tapirs$model[tapirs$region == 'atlantica'], plot = FALSE)[1, ],
        meta(tapirs$model[tapirs$region == 'pantanal'], plot = FALSE)[1, ],
        meta(tapirs$model[tapirs$region == 'cerrado'], plot = FALSE)[1, ],
        meta(tapirs$model, plot = FALSE)[1, ]) %>%
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
                  levels = c(unique(tapirs$name), 'Atlantic forest',
                             'Pantanal', 'Cerrado', 'Overall')),
    region.lab = factor(region.lab,
                        levels = c('Atlantic forest', 'Pantanal', 'Cerrado',
                                   'Overall')),
    average = if_else(name %in% c('Atlantic forest','Pantanal','Cerrado','Overall'),
                      'Group mean', 'Individual') %>%
      factor(levels = c('Individual', 'Group mean')))

# 2a) meta() of areas
p.areas <-
  ggplot(tap) +
  geom_segment(aes(x = area.low, xend = area.high, y = name, yend = name,
                   color = region.lab), lwd = 2) +
  geom_point(aes(x = area.est, y = name, shape = average), col = 'black', size = 1.2) +
  geom_point(aes(x = area.est, y = name, shape = average), col = 'white', size = 0.7) +
  scale_shape_manual(element_blank(), values = c(19, 17)) +
  scale_color_manual('Region', values = c(pal[1:3], 'black'),
                     labels = c('Atlantic forest', 'Pantanal', 'Cerrado', 'Overall')) +
  scale_y_discrete(limits = rev,
                   labels = c('Means', 'Cerrado', 'Pantanal', 'Atlantic forest'),
                   breaks = c('Atlantic forest', 'CE_15_KURUKA',
                              'PA_33_GABRIELA', 'AF_14_JAMESBOND')) +
  labs(x = bquote('Estimated 95% home range area'~(km^2)),
       y = NULL); p.areas

# 2b) home range crossing times
p.tau.pos <-
  ggplot(tap) +
  geom_segment(aes(x = tau.position.low / (60^2 * 24),
                   xend = tau.position.high / (60^2 * 24), y = name, yend = name,
                   color = region.lab), lwd = 2) +
  geom_point(aes(x = tau.position.est / (60^2 * 24), y = name, shape = average),
             col = 'black', size = 1.2) +
  geom_point(aes(x = tau.position.est / (60^2 * 24), y = name, shape = average),
             col = 'white', size = 0.7) +
  scale_shape_manual(element_blank(), values = c(19, 17)) +
  scale_color_manual('Region', values = c(pal[1:3], 'black'),
                     labels = c('Atlantic forest', 'Pantanal', 'Cerrado', 'Overall')) +
  scale_y_discrete(limits = rev,
                   labels = c('Means', 'Cerrado', 'Pantanal', 'Atlantic forest'),
                   breaks = c('Atlantic forest', 'CE_15_KURUKA',
                              'PA_33_GABRIELA', 'AF_14_JAMESBOND')) +
  labs(x = 'Range crossing time (days)', y = NULL); p.tau.pos

# 2c) velocity autocorrelation timescale
p.tau.vel <-
  ggplot(tap) +
  geom_segment(aes(x = tau.velocity.low / 60^2,
                   xend = tau.velocity.high / 60^2, y = name, yend = name,
                   color = region.lab), lwd = 2) +
  geom_point(aes(x = tau.velocity.est / 60^2, y = name, shape = average),
             col = 'black', size = 1.2) +
  geom_point(aes(x = tau.velocity.est / 60^2, y = name, shape = average),
             col = 'white', size = 0.7) +
  scale_shape_manual(element_blank(), values = c(19, 17)) +
  scale_color_manual('Region', values = c(pal[1:3], 'black'),
                     labels = c('Atlantic forest', 'Pantanal', 'Cerrado', 'Overall')) +
  scale_y_discrete(limits = rev,
                   labels = c('Means', 'Cerrado', 'Pantanal', 'Atlantic forest'),
                   breaks = c('Atlantic forest', 'CE_15_KURUKA',
                              'PA_33_GABRIELA', 'AF_14_JAMESBOND')) +
  labs(x = 'Directional persistence timescale (hours)', y = NULL); p.tau.vel

# 2d) mean movement speeds
p.speeds <-
  ggplot(tap) +
  geom_segment(aes(x = speed.low, xend = speed.high,
                   y = name, yend = name, color = region.lab), lwd = 2,
               show.legend = FALSE) +
  geom_point(aes(x = speed.est, y = name, shape = average), col = 'black',
             size = 1.2, show.legend = FALSE) +
  geom_point(aes(x = speed.est, y = name, shape = average), col = 'white',
             size = 0.7, show.legend = FALSE) +
  scale_shape_manual(element_blank(), values = c(19, 17)) +
  scale_color_manual('Region', values = c(pal[1:3], 'black'),
                     labels = c('Atlantic forest', 'Pantanal', 'Cerrado', 'Overall')) +
  scale_y_discrete(limits = rev,
                   labels = c('Atlantic forest', 'Pantanal', 'Cerrado', 'Means'),
                   breaks = c('AF_17_ESPERTA', 'PA_33_GABRIELA', 'CE_15_KURUKA',
                              'Atlantic forest')) +
  labs(x = 'Estimated average speed (km/day)', y = NULL); p.speeds

plot_grid(get_legend(p.areas +
                       theme(legend.position = 'top') +
                       guides(shape = guide_legend(order = 1),
                              col = guide_legend(order = 2))),
          plot_grid(p.areas,
                    p.tau.pos,
                    p.tau.vel,
                    p.speeds,
                    labels = c('a)', 'b)', 'c)', 'd)'),
                    label_fontface = 'plain',
                    ncol = 2, byrow = TRUE, label_y = 1.04),
          ncol = 1, rel_heights = c(0.05, 1))

ggsave('figures/meta.png', width = 6.86, height = 8, scale = 1.5, bg = 'white')
