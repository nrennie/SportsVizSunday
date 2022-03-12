library(tidyverse)
library(readxl)
library(showtext)
library(rcartocolor)
library(usefunc)
library(treemapify)
library(purrr)
library(patchwork)
library(cowplot)

df <- tibble(read_xlsx("2022/2022-03-Netball/SVS Suncorp Super Netball.xlsx"))

# add fonts
font_add_google(name = "VT323", family = "VT323")
showtext_auto()

# prep data
prep_data <- function(year){
 df1 <- df %>%
 filter(Season == paste0("Suncorp Super Netball ", year)) %>%
 select(HomeTeam, AwayTeam, HomeTeamScore, AwayTeamScore)
 plot_data <- tibble(Team = c(df1$HomeTeam, df1$AwayTeam),
                     Score = c(df1$HomeTeamScore, df1$AwayTeamScore)) %>%
 group_by(Team) %>%
 summarise(Total_Score = sum(Score)) %>%
 mutate(label = paste0(Team, "\n(", Total_Score, ")"))
 return(plot_data)
}

plot_treemap <- function(year){
  # prep data
  p_data <- prep_data(year)
  # make plot
  ggplot(data = p_data,
    mapping = aes(area = Total_Score, fill = Team, label = label)) +
  geom_treemap(colour = "black",
               size = 4) +
  geom_treemap_text(place = "center", family = "VT323", size = 20) +
  coord_fixed(expand = F) +
  scale_fill_carto_d(name = "", palette = "Bold") +
  ggtitle(paste0(year)) +
  theme(plot.title = element_text(hjust = 0.5, colour = "white", size = 28, family = "VT323"),
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"),
        legend.position = "none")
}


# map over years
years <- c(2017, 2018, 2019, 2020, 2021)
all_years_plots <- map(.x = years, .f = ~plot_treemap(.x))

# plot all
p <- wrap_plots(all_years_plots) &
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"))
p

# add title
q <- ggdraw() +
  draw_plot(p) +
  draw_label(label=str_wrap_break("Suncorp Super Netball", 20),
             x=0.83, y=0.32, hjust=0.5, fontfamily="VT323", size=32, colour = "white") +
  draw_label(label=str_wrap_break("Total scores per season for each team in the Suncorp Super Netball league.\n\nN. Rennie | Data: sportsvizsunday.com/", 40),
             x=0.83, y=0.2, hjust=0.5, fontfamily="VT323", size=14, colour = "white")
q
