library(tidyverse)
library(ggdist)
library(showtext)
library(rcartocolor)
library(usefunc)

df <- tibble(read_csv("2022/2022-02-Olympic-Luge/Luge_Data.csv"))

# add fonts
font_add_google(name = "Bungee", family = "bungee")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

# prep data
plot_data <- df %>%
  filter(!is.na("Speed (mph)")) %>%
  mutate(medal = factor(Medal, levels = c(0, 3, 2, 1),
                        labels = c("No Medal", "Bronze", "Silver", "Gold")))

# plot
plot_data %>%
  ggplot(aes(x = medal, y = "Speed (mph)", fill = medal)) +
  stat_gradientinterval(position = "dodge",
                        colour = NA,
                        width = 1) +
  stat_halfeye(adjust = .3,
                       width = .3,
                       .width = 0,
                       justification = -.3,
                       point_colour = 'NA',
                       slab_fill=NA,
                       slab_colour='black',
                       slab_size=0.4) +
  geom_boxplot(width = .15,
               outlier.shape = NA,
               fill='lightgrey') +
  stat_dots(
    side = "left",
    dotsize = .8,
    justification = 1.15,
    binwidth = .2,
    colour='black'
  ) +
  coord_cartesian(xlim = c(1, NA),
                  ylim = c(50, 90),
                  clip = "off") +
  scale_fill_carto_d(palette = "Bold") +
  guides(fill="none", alpha='none') +
  labs(x = "", y = "Speed (mph)",
       title = "Top Speeds in Luge",
       subtitle = str_wrap_break("Lugers compete against a timer in one of the most precisely timed sports in the world -- going as accurate as to the thousandth of a second. Across the 2014 and 2018 Winter Olympics, the top speed was 87.5 mph!\n\nN. Rennie | Data: www.sportsvizsunday.com", 80)) +
  theme_minimal() +
  theme(axis.title.y = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black",
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black",
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black"),
        plot.title = element_text(family = "bungee", hjust = 0, size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0, size = 12, color = "black"),
        plot.background = element_rect(fill = "lightgrey", colour="lightgrey"),
        panel.background = element_rect(fill = "lightgrey", colour="lightgrey"),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        legend.position = "none",
        axis.ticks = element_blank()
        )
