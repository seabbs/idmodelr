
# Load packages -----------------------------------------------------------
library(hexSticker)
library(magrittr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(idmodelr)


# Define model ------------------------------------------------------------

##Model Input
S_0 <- 989
I_0 <- 1
R_0 <- 0
beta <- 3
tau <- 0.5
mu <- 1/81


parameters <- c(beta = beta, tau = tau, mu = mu)

inits <- c(S = S_0, I = I_0, R_0 = R_0)

tbegin <- 0
tend <- 50
times <- seq(tbegin, tend, 0.1)

# Simulate model ----------------------------------------------------------

traj <- solve_ode(model = SIR_demographics_ode,
          inits, parameters,
          times, as.data.frame = TRUE)


# Plot model --------------------------------------------------------------

plot <- traj %>%
  gather(key = "Compartment", value = "Population", -time) %>%
  mutate(Compartment = factor(Compartment)) %>%
  ggplot(aes(x = time, y = Population, col = Compartment)) +
  geom_line(size = 1) +
  theme_minimal() +
  scale_color_viridis_d(end = 0.9) +
  theme(legend.position = "none")



# Tweak plot for sticker --------------------------------------------------

hex_fig <- plot +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none",
        panel.background = element_blank())

# Make sticker ------------------------------------------------------------

sticker(hex_fig,
        package = "idmodelr",
        p_size = 23,
        p_color = "#FFFFFFDD",
        s_x = 1,
        s_y = .8,
        s_width = 1.5,
        s_height = 0.7,
        h_fill = "#646770",
        h_color ="#b3ccff",
        filename = "./man/figures/logo.png",
        url = "https://samabbott.co.uk/idmodelr",
        u_color = "#FFFFFFDD",
        u_size = 3.5)
