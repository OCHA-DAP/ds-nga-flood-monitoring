GAUGES_TO_REMOVE <- c(
  "hybas_1120794570",
  "hybas_1120741070",
  "hybas_1120946640",
  "hybas_1120974450",
  "hybas_1120981190"
)
# install_github("r-tmap/tmap@v4")
# libs --------------------------------------------------------------------
# in dedicated monitoring repo should consider setting up w/ {renv}
# have to load tidyverse packages separate for GHA for some reason
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(tidyr) # pivot wider
library(stringr)
library(lubridate)
library(sf)
library(googlesheets4)
library(janitor)
library(tmap)
library(here)
library(blastula)
library(googledrive)
library(ggtext) # colored title
library(glue)
library(gghdx)
library(rhdx)
gghdx()

source(file.path("R","email_funcs.R"))
source(file.path("src","email","email_utils.R"))


# Get Data ----------------------------------------------------------------

## Authenticate google APIs ####
walk(list(drive_auth, gs4_auth), \(f){
  f(
    path = Sys.getenv("GFF_JSON")
  )
})

drive_dribble <- drive_ls(
  corpus = "user"
)


## Static Layers ####
# load layers for mapping from hdx
L <- hdx_map_viz_layers()

# Basins
drive_download(
  as_id("1AsB_Cf9QQb3vkp9ebIFvTPZ5gBYqYRJR"),
  path = basin_fp <- tempfile(fileext = ".rds")
)

gdf_basins_poly <- read_rds(basin_fp) %>%
  filter_basins() %>%
  # clip to country
  st_intersection(
    L$west_central_africa %>%
      filter(admin0Pcod == "NG")
  )

## Live Data ####
### Google forecast Workbook ####
gauge_df_list <- read_gauge_googlesheets(
  url = Sys.getenv("GFF_GAUGE_URL")
)

### Gauge Locations ####
gdf_gauge <- st_as_sf(gauge_df_list$metadata,
  coords = c("longitude", "latitude"),
  crs = 4326
) %>%
  filter(
    !gauge_id %in% GAUGES_TO_REMOVE
  ) %>%
  st_join(
    gdf_basins_poly %>%
      select(hybas_id, basin_name)
  )

### Discharge Forecasts ####
df_forecast_long <- gauge_df_list %>%
  keep_at(at = ~ str_detect(.x, "hybas_")) %>%
  bind_rows() %>%
  # remove potential duplication issue where two discharge values may get appended to googlesheed w/ same date
  mutate(
    date = dmy(date),
    update_time_utc = ymd_hms(update_time_utc)
  ) %>%
  group_by(gauge_id) %>%
  filter(
    date == max(date), # get max date per gauge
    update_time_utc == max(update_time_utc) # get max update time (in case duplicates)
  ) %>%
  ungroup() %>%
  filter(
    !gauge_id %in% GAUGES_TO_REMOVE
  ) %>%
  # add basin informatino to gauge/forecast data
  left_join(
    gdf_gauge %>%
      st_drop_geometry(),
    by = "gauge_id"
  ) %>%
  pivot_longer(
    cols = matches("discharge"),
    names_to = "forecast_cat",
    values_to = "Q"
  ) %>%
  mutate(
    leadtime = replace_na(parse_number(forecast_cat), 0) %>%
      suppressWarnings(), # don't need the parse_number warning
    date_predict = date + leadtime,
    Q_pct_rp2 = Q / x2_years_return_period,
    gte_2_rp = Q >= x2_years_return_period,
    gte_5_rp = Q >= x5_years_return_period,
    gte_20_rp = Q >= x20_years_return_period
  )


gauge_ids_breaching <- df_forecast_long %>%
  filter(gte_2_rp) %>%
  distinct(gauge_id) %>%
  pull(gauge_id)

gdf_gauge_pts <- gdf_gauge %>%
  mutate(
    lgl_gauge_status = gauge_id %in% gauge_ids_breaching,
    `Gauge status` = if_else(lgl_gauge_status, "Threshold exceeded", "Below threshold"),
    aes_dot_size = if_else(lgl_gauge_status, 0.01, .005),
    aes_dot_alpha = if_else(lgl_gauge_status, 1, 0.5)
  )

# Aggregate Alert Status to basin level.
df_basin_alert_status <- accum_pct_gauges_breached(
  df = df_forecast_long,
  date = "date_predict",
  lgl_var = "gte_2_rp"
) %>%
  group_by(basin_name) %>%
  slice_max(
    order_by = cum_pct,
    with_ties = F
  ) %>%
  ungroup() %>%
  mutate(
    `Basin alert status` = if_else(cum_pct >= 0.8, "Warning", "No warning")
  )

gdf_basin_alert_poly <- gdf_basins_poly %>%
  left_join(
    df_basin_alert_status,
    by = "basin_name"
  )

gdf_basin_alert_lines <- st_cast(gdf_basin_alert_poly, "MULTILINESTRING")

# Alert Map ---------------------------------------------------------------
natl_border_col <-  "#CCCCCC"
subnatl_border_col <- "#E0E0E0"
surrounding_country_fill_col = "#F1F1EE"
ocean_fill_color <-  "#99DAEA"
footnote <- "The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the United Nations."

m_basin_alerts <- nga_base_map(
  west_africa_adm0 = L$west_central_africa,
  country_fill = "white",
  surrounding_fill = surrounding_country_fill_col,
  surrounding_label = NULL,extend_bottom = 0.5
) +
  tm_shape(
    gdf_basin_alert_poly
  ) +
  tm_polygons(
    col = "Basin alert status",
    palette = c(
      `No Warning` = hdx_hex("mint-ultra-light"),
      `Warning` = hdx_hex("tomato-hdx")
    ),
    alpha = 0.5,
    border.col = NULL, 
    legend.show = FALSE
  ) +
  tm_shape(
    gdf_basin_alert_lines
  ) +
  tm_lines(
    col = "Basin alert status",
    legend.col.show = F,
    palette = c(
      hdx_hex("mint-hdx"),
      hdx_hex("tomato-dark")
    ),
    lwd = 3, alpha = 0.3
  ) +
  tm_shape(L$admin_1) +
  tm_borders(
    col = subnatl_border_col,
    alpha = 1
  ) +
  tm_shape(L$river) +
  tm_lines(
    col = hdx_hex("sapphire-light"), 
    lwd = 3,
    alpha = 0.5
  ) +
  # gauge locations
  tm_shape(gdf_gauge_pts,
    legend.show = FALSE
  ) +
  tm_dots(
    col = "Gauge status",
    size = 0.1,
    palette = c(
      "#bababaff",
      "black"
    ),
    legend.show= FALSE,
    legend.size.show = FALSE
  ) +
  tm_shape(
    L$west_central_africa %>% 
      filter(admin0Pcod=="NG") %>% 
      mutate(
        lab = str_to_upper(admin0Name)
      )
  )+
  tm_text(text = "lab",ymod =5,
          xmod = 2,size = 2,col = "grey"
          
  )+
  tm_shape(gdf_basin_alert_poly) +
  tm_text(text = "basin_name",
          fontfamily  = "Source Sans 3",size = 1.2,
          shadow = TRUE,) +
  tm_shape(
    L$west_central_africa %>% 
             filter(admin0Pcod=="NG")
           )+
  tm_borders(
    col = natl_border_col
  )+
  tm_add_legend(type ="fill",
                title = "Basin alert status",
                labels= c("No warning","Warning"),
                col = c(
                  hdx_hex("mint-ultra-light"),
                        hdx_hex("tomato-hdx") 
                        ),
                alpha = 0.5,
                border.lwd = 0,
                # border.col=c(
                #   `No Warning` = hdx_hex("mint-hdx"),
                #   `Warning` = hdx_hex("tomato-hdx")# looks like it's just taking top color which i think is fine
                #   ),
                group = "alert"
                )+
  tm_add_legend(type ="symbol",
                title = "Gauge status",
                labels= c("Below threshold","Threshold exceeded"),
                col = c( "#bababaff",
                         "black"),
                border.col = "grey",group="alert"
                )+
  tm_add_legend(type ="line",
                labels= c("River (DCW/ ESRI)",
                          "State Boundary (UN OCHA/OSGOF)"),
                col = c(
                  hdx_hex("sapphire-light"),
                  natl_border_col
                  ),
                )+
  tm_credits(text = footnote, 
             size = 1,
             # bg.color = "white",
             width = 0.4,
             fontfamily = "Source Sans 3",
             # fontface = "plain",
             col = "black",
             position = c(0.0,0.0012))+
  
  tm_layout(
    scale = 1,
    title.size = 1.2,
    outer.margins = c(0, 0, 0, 0),
    inner.margins = c(0, 0, 0, 0),
    # legend.position = c("right", "bottom"),
      legend.position=c(0.76,0.05),
    legend.bg.color = "white",
    legend.text.fontfamily = "Source Sans 3",
    legend.bg.alpha = 1,
    # legend.text.fontface = "plain",
    # legend.text.fontfamily = "serif",
    legend.frame = "white",
    # fontface = "plain",
    # fontfamily = "serif",
    legend.height = 0.5,
    legend.title.size = 0.9,
    legend.title.fontface = "bold",
    legend.text.size = 0.9,
    bg.color = ocean_fill_color
  )

# Plot --------------------------------------------------------------------
p_discharge <- plot_average_discharge_normalized(
  df = df_forecast_long,
  date = "date_predict",
  threshold = 0.8,
  basin_palette = basin_pal()
)

txt_warning_status <- ifelse(
  any(df_basin_alert_status$`Basin alert status` == "Warning"), "Warning", "No flood warning"
)

# config email ------------------------------------------------------------

date_prediction_made <- df_forecast_long$date %>% unique()
dt_made_chr <- trimws(format(as_date(date_prediction_made), "%e %B %Y"))
# Generate conditional email subject
subj_email <- paste0(
  "Nigeria Riverine Flood Monitoring: ",
  dt_made_chr
)

drive_download(
  as_id("1A1WPSWBPJKFDBqZb1OXYHipsYxEErR-7"),
  email_receps_fp <- tempfile(fileext = ".csv")
)
email_receps_df <- read_csv(email_receps_fp)

email_to <-  email_receps_df %>% 
  filter(to) %>% 
  pull(email_address)
email_to <-  str_subset(email_to,"^z")

# Load in e-mail credentials
email_creds <- creds_envvar(
  user = Sys.getenv("CHD_DS_EMAIL_USERNAME"),
  pass_envvar = "CHD_DS_EMAIL_PASSWORD",
  host = Sys.getenv("CHD_DS_HOST"),
  port = Sys.getenv("CHD_DS_PORT"),
  use_ssl = TRUE
)
email_rmd_fp <- "email_flood_monitoring.Rmd"

render_email(
  input = email_rmd_fp,
  envir = parent.frame()
) %>%
  smtp_send(
    to = email_to,
    # bcc = filter(df_recipients, !to)$email,
    from = "data.science@humdata.org",
    subject = subj_email,
    credentials = email_creds
  )  