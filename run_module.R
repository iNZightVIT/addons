# this script allows developers to test their module easily

library(devtools)
load_all('../iNZightModules')
load_all('../iNZight')

try(ui$close(), TRUE)
ui <- iNZGUI$new()
wd <- getwd()
# ui$initializeGui(iris, addonDir = wd)

# demest module testing
# remotes::install_github('StatisticsNZ/demdata')
# remotes::install_github('StatisticsNZ/dembase')
# remotes::install_github('StatisticsNZ/demest')
nzincome <- demdata::nz.income

ui$initializeGui(nzincome, addonDir = wd)

# dataset > aggregate > [reorder and select]



# ui$initializeGui(gapminder, addonDir = wd)

#x <- capture.output(z <- demest::listContents(ui$activeModule$model_file))

# ui$plotWidget$plotNb$children[[1]]$add_handler_clicked(function(h, ...) {
#     print(h$obj)
# })

# RGtk2::gSignalConnect(
#     ui$plotWidget$plotNb$children[[1]]$widget,
#     "motion-notify-event",
#     f = function(w, e) {
#         # print(w)
#         print(e)
#     }
# )
