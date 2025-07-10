# Nigeria Flood Monitoring System

This repository contains an automated flood monitoring system for Nigeria that generates email reports based on Google Flood Forecasting data and gauge discharge information. This project serves as the **initial pilot implementation** carried out by OCHA's Centre for Humanitarian Data (CHD) using Google's flood prediction/monitoring system ([floodhub](https://sites.research.google/floods/l/0/0/3)). More information/visualzation of the analyese performed here can be found in this [slide deck](https://docs.google.com/presentation/d/1rB8aOX8XntChfCqIH0wm7Xwf-JXfpQz68sNGk-RifVw/edit?slide=id.g112845f0d8c_1_0#slide=id.g112845f0d8c_1_0)

## Methodology

### Analysis Level
The flood monitoring analysis operates at the **basin level** (level 4) using HydroBASINS data. Each monitoring point (gauge) is associated with a specific hydrological basin, allowing for watershed-based flood risk assessment.

### Threshold Setting
Flood thresholds are determined through:

- **Basin Level**: Analysis operates at **HydroBASINS Level 4** watersheds
- **Individual Gauge Threshold**: 2-year return period discharge values
- **Basin Alert Trigger**: Warning activated when **≥80% of gauges** within a basin exceed 2-year return period

## Environment Variables

The following environment variables must be requested from the team and configured before running the system:

### Required Environment Variables

- **`GFF_JSON`** - Path to Google API service account JSON key file for authentication with Google Sheets and Google Drive APIs
- **`GFF_GAUGE_URL`** - URL to the Google Sheets document containing gauge forecast data
- **`CHD_DS_EMAIL_USERNAME`** - SMTP username for sending monitoring emails
- **`CHD_DS_HOST`** - SMTP host server for email delivery
- **`CHD_DS_PORT`** - SMTP port number for email server connection

### Setting Environment Variables

Create a `.env` file in the project root or set these variables in your system environment:

```bash
export GFF_JSON="/path/to/google_flood_pilot_key.json"
export GFF_GAUGE_URL="xxxx"
export CHD_DS_EMAIL_USERNAME="xxxxx"
export CHD_DS_HOST="xxxx"
export CHD_DS_PORT="xxxx"
```

## Repository Structure

```
ds-nga-flood-monitoring/
├── README.md                           # This file
├── LICENSE.md                          # Project license
├── ds-nga-flood-monitoring.Rproj       # RStudio project file
├── google_flood_pilot_key.json         # Google API credentials (not in version control)
├── email_flood_monitoring.Rmd          # R Markdown template for email reports
├── test_plot.png                       # Sample plot output
├── test.png                           # Test image file
├── exploration/                        # Data exploration and analysis
│   └── 01_google_gauge_discharge_review.html
├── R/                                  # Core R functions
│   └── email_funcs.R                  # Email and data processing functions
└── src/                               # Source code
    ├── gauge_monitoring_email.R       # Main script for generating monitoring emails
    └── email/                         # Email-specific utilities
        └── email_utils.R              # HTML email formatting functions
```

### Key Components

#### Main Scripts
- **`src/gauge_monitoring_email.R`** - Primary execution script that:
  - Authenticates with Google APIs
  - Downloads gauge data from Google Sheets
  - Loads spatial data layers from HDX
  - Generates visualizations and maps
  - Renders and sends email reports

#### Core Functions
- **`R/email_funcs.R`** - Contains utility functions for:
  - Loading HDX spatial data layers
  - Processing gauge forecast data
  - Reading Google Sheets data
  - Data filtering and transformation

#### Email Templates
- **`email_flood_monitoring.Rmd`** - R Markdown template for generating HTML email reports
- **`src/email/email_utils.R`** - Helper functions for:
  - Custom image formatting in emails
  - Responsive HTML generation
  - Plot embedding in email content

#### Data Sources
The system integrates data from:
- **Google Flood Forecasting Data** - Real-time gauge discharge predictions
- **HDX (Humanitarian Data Exchange)** - Spatial data layers including:
  - West & Central Africa boundaries
  - Nigeria administrative boundaries (Admin 1)
  - River network data
  - Basin polygon data


## Dependencies

Key R packages required:
- `tidyverse` (dplyr, ggplot2, readr, etc.)
- `sf` - Spatial data handling
- `googlesheets4` - Google Sheets API
- `googledrive` - Google Drive API
- `blastula` - Email rendering and sending
- `tmap` - Thematic mapping
- `rhdx` - HDX data access
- `gghdx` - HDX visualization theme
- `lubridate` - Date/time handling

## Usage

1. Script is set to run automatically on CRON job scheduled through GHA

## Security Notes

- The `google_flood_pilot_key.json` file contains sensitive API credentials and should never be committed to version control
- Environment variables should be securely managed and not exposed in code
- Email credentials should be stored securely and rotated regularly
