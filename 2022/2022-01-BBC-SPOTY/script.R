library(tidyverse)
library(readxl)
library(cowplot)
library(showtext)
library(magick)
library(ggtext)

# read in data
df <- tibble(read_excel("2022/2022-01-BBC-SPOTY/data.xlsx"))

# add fonts
font_add_google(name = "Just Another Hand", family = "justanotherhand")
showtext_auto()

# prep data
plot_data <- df %>%
  select(Year, Gender)

# visualise
x <- 1:17
y <- 1:4
df_plot <- expand.grid(x=x, y=y)
df_plot$Gender <- plot_data$Gender
df_plot$Year <- plot_data$Year


p <- ggplot() +
  geom_tile(data=df_plot, mapping=aes(x=x, y=y, fill=Gender), colour="white", height=0.8, width=0.8) +
  scale_fill_manual("", values=c("Male"="black", "Female"="White", "Duo"="gray50")) +
  labs(title = "",
       subtitle = "<span>Since 1954 the British Broadcasting Corporation have celebrated UK sporting <br> excellence through the Sports Personality of the Year Award. Recipients <br> are overwhelmingly  male, winning almost 80% of awards. And the trend <br> for male winners is becoming stronger over time. **Female** and <span style = 'color:#7f7f7f'>**Duo**</span> winners <br> are highlighted below.",
       caption = " ") +
  theme_void() +
  coord_fixed() +
  theme(plot.subtitle = element_markdown(family = "sans", hjust = 0.5, size = 12, color = "white", lineheight = 1.4, margin = margin(50,2,15,2)),
        plot.caption = element_markdown(family = "sans", hjust = 0.5, size = 10, color = "white", lineheight = 1.4, margin = margin(85,2,0,2)),
        plot.background = element_rect(fill = "black", colour="black"),
        panel.background = element_rect(fill = "black", colour="black"),
        plot.tag.position = c(0.5, 0.6),
        plot.margin = unit(c(0,3,0,3), "cm"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
  )
p

dev.new(width=9.5,height=6.5,unit="in", noRStudioGD = TRUE)


img1 <- image_read("2022/2022-01-BBC-SPOTY/arrow1.png")
img2 <- image_read("2022/2022-01-BBC-SPOTY/arrow2.png")
img3 <- image_read("2022/2022-01-BBC-SPOTY/arrow3.png")
logo <- image_read("2022/2022-01-BBC-SPOTY/logo.png")

q <- ggdraw() +
  draw_plot(p) +
  draw_label(label=str_wrap("In 1962, English swimmer Anita Lonsbrough becomes the first female recipient of the BBC SPOTY.", 30),
             x=0.31, y=0.19, hjust=0.5, fontfamily="justanotherhand", size=16, colour = "white") +
  draw_label(label=str_wrap("In 1984, figure skaters Torvill and Dean became the first, and so far only, duo to be awarded the BBC SPOTY.", 30),
             x=0.8, y=0.15, hjust=0.5, fontfamily="justanotherhand", size=16, colour = "white") +
  draw_label(label=str_wrap("In 2021, tennis player Emma Raducanu won after 14 consecutive years of male winners.", 30),
             x=0.9, y=0.63, hjust=0.5, fontfamily="justanotherhand", size=16, colour = "white") +
  draw_label(label=str_wrap("N. Rennie | Data: sportsvizsunday.com", 60),
             x=0.5, y=0.03, hjust=0.5, fontfamily="sans", size=10, colour = "white") +
  draw_image(img1, -0.04, -0.28, scale=0.18) +
  draw_image(img2, 0.19, -0.23, scale=0.18) +
  draw_image(img3, 0.43, 0.03, scale=0.18) +
  draw_image(logo, 0, 0.37, scale=0.35) +
  theme(plot.background = element_rect(fill = "black", colour="black"))
q





