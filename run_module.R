# this script allows developers to test their module easily
wd <- file.path(getwd(), "modules")

# remotes::install_github('iNZightVIT/iNZight')
# devtools::load_all("../iNZight")
try(ui$close(), TRUE)
ui <- iNZight(addonDir = wd)
