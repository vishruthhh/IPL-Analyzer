# Run once from the project root:  source("data-raw/prepare_data.R")
# If you downloaded the zip manually (https://cricsheet.org/downloads/ipl_csv2.zip):
#   build_ipl_data(zip_path = "path/to/ipl_csv2.zip")
source("src/data.R")
build_ipl_data()
