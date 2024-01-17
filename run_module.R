# this script allows developers to test their module easily
wd <- file.path(getwd(), "modules")

devtools::load_all("../iNZight")
ui <- iNZight(census.at.school.500, addonDir = wd)

# manually install deps
# for (mod in list.dirs(wd, recursive = FALSE)) {
#     f <- file.path(mod, "DESCRIPTION")
#     deps <- desc::desc_get_deps(f)$package
#     gh_deps <- desc::desc_get_field("Github", file = f)
#     remotes::install_github(gh_deps, upgrade = "never")
#     deps <- deps[!deps %in% row.names(installed.packages())]
#     if (length(deps)) install.packages(deps)
# }

# tdata <- iNZightTools::smart_read('tourism.csv')

# remotes::install_github('iNZightVIT/iNZight')
devtools::load_all("../iNZightTS")
devtools::load_all("../iNZight")

visitorsQ <- iNZightTS::visitorsQ
visitorsQ_long <- visitorsQ |>
    iNZightTools::reshape_data("long", cols = names(visitorsQ)[-1]) |>
    iNZightTools::rename_vars(c(visitors = "value")) |>
    dplyr::mutate(
        gdp = visitors * runif(dplyr::n(), 0.7, 1.2),
        cat = sample(c("A", "B", "C"), dplyr::n(), replace = TRUE)
    )

try(ui$close(), TRUE)
ui <- iNZight(visitorsQ, addonDir = wd)
ui <- iNZight(visitorsQ_long, addonDir = wd)





# test installing modules
mod_dir <- file.path(tempdir(), "modules")
dir.create(mod_dir)

# devtools::load_all("../iNZight")
try(ui$close(), TRUE)
ui <- iNZight(addonDir = mod_dir)
m <- NewModuleManager$new(ui)
