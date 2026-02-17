
# Script:      {{script_name}}
# Author:      {{author}}
# Created:     {{date}}
# Description: Description of script.
#
# Shortcuts:
#   alt + shift + k: shortcut guide
#   alt + o:         collapse all sections
#   alt + shift + o: expand all sections
#   ctrl + alt + t:  run code section


# Setup -----------------------------------------------------------------------

devtools::install_github(
  repo = "johnsonpc2/pcjtools",
  upgrade = "always",
  force = FALSE
)

devtools::install_github(
  repo = "bcdudek/bcdstats",
  upgrade = "never",
  force = FALSE
)

pcjtools::load_packages(c("bcdstats", "data.table", "ggplot2",
                          "gtsummary", "pcjtools", "psych"))

pcjtools::pavlovia_pull()

clean_workspace(confirm = FALSE)


# Load Data -------------------------------------------------------------------



# Data Wrangling --------------------------------------------------------------



# Visualize -------------------------------------------------------------------


# Push to GitHub --------------------------------------------------------------

pcjtools::git_commit(push = TRUE)