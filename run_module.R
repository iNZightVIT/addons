# this script allows developers to test their module easily
wd <- file.path(getwd(), "modules")

# manually install deps
for (mod in list.dirs(wd, recursive = FALSE)) {
    f <- file.path(mod, "DESCRIPTION")
    deps <- desc::desc_get_deps(f)$package
    gh_deps <- desc::desc_get_field("Github", file = f)
    remotes::install_github(gh_deps, upgrade = "never")
    deps <- deps[!deps %in% row.names(installed.packages())]
    if (length(deps)) install.packages(deps)
}

# remotes::install_github('iNZightVIT/iNZight')
# devtools::load_all("../iNZight")
try(ui$close(), TRUE)
ui <- iNZight(addonDir = wd)


# test installing modules
mod_dir <- file.path(tempdir(), "modules")
dir.create(mod_dir)

# devtools::load_all("../iNZight")
try(ui$close(), TRUE)
ui <- iNZight(addonDir = mod_dir)
m <- NewModuleManager$new(ui)
