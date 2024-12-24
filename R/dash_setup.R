xfun::pkg_attach(
  "tidyverse",
  "targets",
  "here",
  "scales",
  "qs",
  "zoo",
  "patchwork",
  "shiny",
  "shinyjs",
  "rlang",
  "ggrepel",
  "ggtext",
  "ggfittext",
  "kableExtra",
  "glue",
  "targets"
)

dashpal <-  c("#771155", "#117777")
rpal <- c("#771155", "#AA4488", "#CC99BB", 
          "#114477", "#4477AA", "#77AADD", 
          "#117777", "#44AAAA", "#77CCCC", 
          "#117744", "#44AA77", "#88CCAA", 
          "#777711", "#AAAA44", "#DDDD77", 
          "#774411", "#AA7744", "#DDAA77", 
          "#771122", "#AA4455", "#DD7788")

dens_pal <- list(c(`Union member` = dashpal[2], `Not member` = "#DADDEF"),
                 c(`Union member` = dashpal[1], `Not member` = "#DADDEF"))
bar_pal <- list(
  c(`yes` = dashpal[2], `no` = "#DADDEF"),  # private
  c(`yes` = dashpal[1], `no` = "#DADDEF")  # public
)

theme_rr_dash <-
  function(base_size = 14, legend = 'none') {
    theme_minimal() %+replace%
      theme(
        text = element_text(size = 6),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.title = element_text(size = 14),
        plot.title = element_text(
          size = 20,
          hjust = 0,
          face = 'bold',
          margin = margin(b = 6)
        ),
        plot.subtitle = element_text(
          size = 18,
          hjust = 0,
          margin = margin(b = 10)
        ),
        plot.title.position = 'plot',
        plot.caption = element_text(
          size = 12,
          face = 'italic',
          hjust = 1,
          margin = margin(t = 10)
        ),
        panel.grid.minor = element_blank(),
        legend.position = legend,
        strip.text = element_text(
          size = 18,
          face = "bold",
          color = "#353935",
          hjust = 0,
          margin = margin(b = 5, unit = "mm")
        )
      )
  }