# this script allows developers to test their module easily

# remotes::install_github('iNZightVIT/iNZight')

try(ui$close(), TRUE)
ui <- iNZGUI$new()
wd <- file.path(getwd(), "modules")

ui$initializeGui(iris, addonDir = wd)
