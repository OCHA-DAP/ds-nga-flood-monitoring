
# these dependencies causing tidyverse install to fail on ubuntu runner in GHA
# therefore installing separately first
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
# remotes::install_github("r-tmap/tmap@v4") # not going to do this
# library(here)

# library(gt)
