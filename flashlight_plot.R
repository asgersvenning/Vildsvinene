library(tidyverse)
library(metR)
library(ggforce)
library(extrafont)

plot_calc_angle <- Vectorize(function(x, y) {
  angle_three(c(x, 0, 0), c(y, 0, -1), type = "turn")
}, USE.NAMES = F, SIMPLIFY = T)

flashlight_binwidth <- 0.05
ncp <- 100

x <- seq(-1, 1, 1 / (ncp - 1))
y <- seq(-1, 1, 1 / (ncp - 1))

# plot(x,y,type="l")

turning_viz <- tibble(
  x = x,
  y = y
) %>% 
  complete(x,y) %>% 
  mutate(
    angle = plot_calc_angle(x, y)
  ) %>% 
  filter(sqrt(x^2 + y^2) < 1) %>% 
  ggplot(
    aes(x, y, 
        z = angle)
  ) +
  geom_contour_fill(
    aes(fill = after_stat(level),
        color = after_scale(fill)),
    breaks = MakeBreaks(binwidth = flashlight_binwidth),
    linewidth = 0,
    alpha = 1
  ) +
  geom_contour(
    color = "gray50",
    alpha = .3,
    linewidth = .15,
    binwidth = flashlight_binwidth) +
  annotate("segment",
           x = 0, y = 0,
           xend = 0, yend = -1,
           linewidth = 1.5,
           color = viridis::magma(1/flashlight_binwidth)[1]) +
  geom_circle(
    aes(
      x0 = 0, y0 = 0,
      r = .995
    ),
    inherit.aes = F,
    color = "white",
    linewidth = 3,
    data = tibble()
  ) +
  geom_circle(
    aes(
      x0 = 0, y0 = 0,
      r = .97873
    ),
    inherit.aes = F,
    color = "#7777774C",
    linewidth = .5,
    data = tibble()
  ) +
  geom_circle(
    aes(
      x0 = 0, y0 = 0,
      r = 0.025
    ),
    inherit.aes = F,
    color = "white",
    fill = "forestgreen",
    linewidth = 1,
    data = tibble()
  ) +
  annotate("segment",
           x = 0, y = 0,
           xend = 0, yend = 0.3,
           linewidth = 1.5,
           arrow = arrow(30,
                         length = unit(4, "mm"),
                         type = "closed"),
           color = "forestgreen") +
  scale_fill_manual(values = viridis::magma(1/flashlight_binwidth, direction = -1)) +
  coord_equal(expand = F, clip = "off",
              xlim = c(-.99, .99), ylim = c(-.99, .99)) +
  ggpubr::theme_pubr(base_family = "CMU Serif") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.box.margin = margin(),
        legend.text = element_text(size = 12,
                                   margin = margin()),
        legend.title = element_text(size = 16, 
                                    face = "bold",
                                    margin = margin()),
        legend.position = "right",
        legend.key.height = unit(.74, "cm"),
        legend.key.width = unit(1, "cm"),
        plot.margin = margin()) +
  labs(fill = latex2exp::TeX("\\textbf{Turning} ($\\alpha$)"))


ggsave("turning_vizualization.pdf", align_legend(turning_viz),
       device = cairo_pdf,
       width = 4, height = 3.2, scale = 2) 
  