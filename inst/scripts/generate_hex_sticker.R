
# Load packages -----------------------------------------------------------
## Place holder sticker.
install.packages("getTBinR")
library(getTBinR)
library(hexSticker)
library(magrittr)
library(ggplot2)

# Get TB data -------------------------------------------------------------
tb_df <- get_tb_burden()



# Make map ----------------------------------------------------------------

## Tweaked map from package to control for line size
map_tb <- map_tb_burden()$data %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat,
                   fill = e_inc_100k, group = group),
               color = "black", size = 0.05) +
  scale_fill_viridis_c(direction = -1, begin = 0.1, end = 0.9, option = "cividis") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme_void() + theme_transparent()+
  theme(legend.position = "none",
        panel.background = element_blank())

# Make sticker ------------------------------------------------------------

sticker(map_tb,
        package = "idmodelr",
        p_size = 23,
        p_color = "#FFFFFFDD",
        s_x = 1,
        s_y=.75,
        s_width=1.6,
        s_height=0.8,
        h_fill = "#b3ccff",
        h_color = "#646770",
        filename="./man/figures/logo.png",
        url = "https://samabbott.co.uk/idmodelr",
        u_color = "#FFFFFFDD",
        u_size = 3.5)


