
# libs --------------------------------------------------------------------

v_pkgs = c("reshape", "dplyr", "kirkegaard", "broom", "ggplot2", "purrr", "magrittr")
v_null = sapply(v_pkgs, library, character.only = T)

#set default theme
theme_set(theme_bw())