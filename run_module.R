# this script allows developers to test their module easily

# remotes::install_github('iNZightVIT/iNZight')
# devtools::load_all("../iNZight")

wd <- file.path(getwd(), "modules")

try(ui$close(), TRUE)
iNZight(addonDir = wd)
