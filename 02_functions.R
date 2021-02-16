# PHOSP-COVID analysis: FUNCTIONS
# Any bespoke function is placed here. 
# Centre for Medical Informatics, Usher Institute, University of Edinburgh 2020

# Functions require library(tidyverse), requires() nor :: not currently written in.  

# 1. ggplot templates / extraction functions
# 2. Table defaults
# 3. Prognostic scoring

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