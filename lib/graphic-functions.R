# Colors ------------------------------------------------------------------

# This palette was generated with 7 values from scico's "roma" palette, but
# ignoring the extra light greenish 4th color
# https://github.com/thomasp85/scico
#
# scico(7, palette = "roma")
ngo_red <- "#7E1900"
ngo_orange <- "#AC7825"
ngo_yellow <- "#D9D26A"
ngo_blue_lt <- "#60C3D4"
ngo_blue <- "#3877B6"
ngo_blue_dk <- "#1A3399"


# ggplot themes -----------------------------------------------------------

update_geom_defaults("label", list(family = "Roboto Condensed Light"))
update_geom_defaults("text", list(family = "Roboto Condensed Light"))

theme_ngo <- function(base_size = 9, base_family = "Roboto Condensed") {
  ret <- theme_bw(base_size, base_family) +
    theme(plot.title = element_text(size = rel(1.4), face = "bold",
                                    family = "Roboto Condensed"),
          plot.subtitle = element_text(size = rel(1), face = "plain",
                                       family = "Roboto Condensed Light"),
          plot.caption = element_text(size = rel(0.8), color = "grey50", face = "plain",
                                      family = "Roboto Condensed Light",
                                      margin = margin(t = 10)),
          panel.border = element_rect(color = "grey50", fill = NA, size = 0.15),
          panel.spacing = unit(1, "lines"),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = rel(0.9), hjust = 0,
                                    family = "Roboto Condensed", face = "bold"),
          strip.background = element_rect(fill = "#ffffff", colour = NA),
          axis.ticks = element_blank(),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.text = element_text(family = "Roboto Condensed Light", face = "plain"),
          legend.key = element_blank(),
          legend.text = element_text(size = rel(1), family = "Roboto Condensed Light", face = "plain"),
          legend.spacing = unit(0.1, "lines"),
          legend.box.margin = margin(t = -0.5, unit = "lines"),
          legend.margin = margin(t = 0),
          legend.position = "bottom")
  
  ret
}

theme_ngo_map <- function(base_size = 11, base_family = "Roboto Condensed") {
  ret <- theme_void(base_size, base_family) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = rel(1), family = "Roboto Condensed Light", face = "plain"),
          legend.title = element_text(family = "Roboto Condensed", face = "bold"),
          legend.key.size = unit(0.7, "lines"))
  
  ret
}
