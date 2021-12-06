# PROJECT SETUP

#1) Clone repo from: https://github.com/USAID-OHA-SI/maskless-achv.git

#2) Setup SI Project Structure

# glamr::si_setup()

#3) Make sure your global dataset folders are also set

# glamr::set_paths()

#4) Load MSDs (Site, PSNU, OU x IM) into your glamr::si_path(type = "path_msd")


# GLOBAL Variables

dir_merdata <- glamr::si_path("path_msd")

file_ou_im <- glamr::return_latest(
  folderpath = dir_merdata,
  pattern = "OU_IM_FY19-22_\\d{8}_v\\d{1}_\\d{1}"
)

curr_pd <- source_info(file_ou_im, return = "period")

# PROJECT Variables

dir_data <- "./Data"
dir_dataout <- "./Dataout"
dir_images <- "./Images"
dir_graphics <- "./Graphics"
