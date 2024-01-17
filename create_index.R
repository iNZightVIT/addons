pkgs <- c(
    "desc",
    "git2r",
    "yaml"
)

for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# create a list of packages
mods <- list.dirs("modules", recursive = FALSE)
d <- getwd()

index <- lapply(mods, \(mod) {
    message("Module: ", mod)
    setwd(mod)
    on.exit(setwd(d))

    system("git fetch -p && git fetch --tags")
    branches <- git2r::branches(flags = "remote")
    branch_names <- gsub("origin/", "", sapply(branches, \(x) x$name), fixed = TRUE)

    latest_branch <- ""
    if ("main" %in% branch_names) {
        latest_branch <- "main"
    } else if ("master" %in% branch_names) {
        latest_branch <- "master"
    }

    dev_branch <- ""
    if ("develop" %in% branch_names) {
        dev_branch <- "develop"
    } else if ("dev" %in% branch_names) {
        dev_branch <- "dev"
    } else if ("development" %in% branch_names) {
        dev_branch <- "development"
    }

    git2r::checkout(branch = latest_branch)

    info <- list(
        title = desc::desc_get_field("Title", file = "DESCRIPTION"),
        author = desc::desc_get_field("Author", file = "DESCRIPTION"),
        description = desc::desc_get_field("Description", file = "DESCRIPTION"),
        versions = as.list(names(git2r::tags())),
        latest = as.character(desc::desc_get_version("DESCRIPTION")),
        stable = latest_branch,
        development = dev_branch,
        url = git2r::remote_url(),
        exclude = as.logical(desc::desc_get_field("Exclude",
            file = "DESCRIPTION", default = "FALSE"
        ))
    )
})
names(index) <- gsub("modules/", "", mods, fixed = TRUE)

yaml::write_yaml(index, "modules.yml")
