
# these dependencies casuing tidyverse not to work?
install.packages(
  c("ragg",
    "textshaping"
    )
)

install.packages(
  c("tidyverse",
    "sf",
    "googlesheets4",
    "janitor",
    "tmap",
    "blastula",
    "googledrive",
    "ggtext",
    "glue",
    "remotes", # needed to install gghdx
    "showtext", # needed to set fonts in gghdx
    "rhdx"
  )
)
remotes::install_github("dickoa/rhdx")
remotes::install_github("OCHA-DAP/gghdx")
# library(here)

# library(gt)
