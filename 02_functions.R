# PHOSP-COVID analysis: FUNCTIONS
# Any bespoke function is placed here. 
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# Functions require library(tidyverse), requires() nor :: not currently written in.  

# 1. ggplot templates / extraction functions
# 2. Table defaults
# 3. Prognostic scoring

# ggplot templates -------------------------------------------------------------------------
# Lisa's lovely Lancet-style ggplot template (Lisa Norman) ---------------------------------------------
ggplot_lancet <- function(...)
  ggplot2::ggplot(...) +
  scale_fill_brewer(palette = "Blues") +
  scale_colour_brewer(palette = "Blues") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.title.x       = element_text(margin = margin(10, 0, 0, 0, "pt")),
    axis.title.y       = element_text(margin = margin(0, 10, 0, 0, "pt")),
    axis.text          = element_text(size = 10),
    axis.ticks.length  = unit(5, "pt"),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.background   = element_blank(),
    strip.placement    = "outside",
    strip.text.x       = element_text(hjust = 0,
                                      face  = "bold",
                                      size  = 10),
    strip.text.y       = element_text(hjust = 0,
                                      size  = 10),
    text = element_text(
      family = "Merriweather",
      size = 10,
      face = "plain"
    ),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.direction = "horizontal",
    legend.key.size = unit(10, "pt")
  ) +
  guides(fill = guide_legend(title.position = "top"))


# Table defaults ---------------------------------------------------------------------------
# This makes table resize or continue over multiple pages in all output types
# PDF powered by kableExtra, Word by flextable
mytable = function(x, caption = "", row.names = FALSE, longtable = TRUE, 
                   latex_options = c("hold_position"), font_size = 7.0, ...){
  
  # if not latex or html then else is Word
  if (is_latex_output()) { 
    knitr::kable(x, row.names = row.names, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r", "r", "r"), 
                 booktabs = TRUE, caption = caption, #longtable = longtable,
                 linesep = "", ...) %>%
      kableExtra::kable_styling(font_size = font_size,
                                latex_options = latex_options)
  } else if(is_html_output()) {
    knitr::kable(x, row.names = row.names, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r", "r", "r"), 
                 booktabs = TRUE, caption = caption, longtable = longtable,
                 linesep = "", ...) %>%
      kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"))
  } else {
    flextable::flextable(x) %>% 
      flextable::autofit() %>% 
      flextable::width(j = 1, width = 1.5) %>% 
      flextable::height(i = 1, height = 0.5, part = "header")
  }
}

