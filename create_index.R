# # creates an index of the modules
# `%||%` <- function(a, b)
#     if (is.null(a)) b else a

# ## This comes from iNZightModules :
# getModules <- function(dir) {
#     mods <- list.files(dir, pattern = "*.R", full.names = TRUE)
#     # mods <- mods[!grepl("DemoModule", mods)]
#     mod_list <- lapply(mods, getmodule)
#     mod_list <- mod_list[!sapply(mod_list, is.null)]
#     names(mod_list) <- sapply(mod_list, function(x) x$name)
#     mod_list
# }

# getmodule <- function(f) {
#     ## check if file is a Module
#     t <- readLines(f)
#     mi <- grep("^#'", t)
#     meta <- NULL
#     if (length(mi)) {
#         meta <- parse_meta(t[mi])
#         t <- t[-mi]
#     }
#     t <- paste(collapse = "\n", t)
#     if (!grepl("^[a-zA-Z]+[a-zA-Z0-9]*\\s*<-\\s*setRefClass", t)) return(NULL)

#     # ## load module into an environment to avoid clashes
#     e <- new.env()
#     # eval(parse(text = t), e)

#     # ## fetch the module's name
#     # objs <- ls(e)
#     # obj <- objs[which(sapply(objs, function(o) {
#     #     ob <- e[[o]]
#     #     pclass <- try(ob@generator$def@contains$refClass@by, silent = TRUE)
#     #     if (inherits(pclass, "try-error")) return(FALSE)
#     #     pclass == "CustomModule"
#     # }))]
#     # if (length(obj) != 1) {
#     #     warning("Couldn't find module class.")
#     #     return(NULL)
#     # }
#     # e$name <- obj
#     # e$display_name <- e[[obj]]@className[1]
#     e$meta <- meta
#     # e$module <- e[[obj]]
#     e$path <- f
#     e
# }

# parse_meta <- function(x) {
#     # remove comment
#     x <- gsub("^#' ", "", x)
#     m <- regexpr("^@[a-zA-Z]+", x)
#     names <- substr(x, m + 1, attr(m, "match.length"))
#     values <- substr(x, m + attr(m, "match.length") + 1, nchar(x))
#     names(values) <- names
#     as.list(values)
# }
# ## ***

# create_index <- function() {
#     mods <- getModules(".")
#     tbl <- lapply(mods,
#         function(mod) {
#             as.data.frame(
#                 list(
#                     Name = mod$meta$name %||% basename(mod$path),
#                     Version = numeric_version(mod$meta$version %||% 0),
#                     Description = mod$meta$desc %||% "",
#                     Author = mod$meta$author %||% "",
#                     filename = gsub("^\\.\\/", "", mod$path)
#                 ),
#                 stringsAsFactors = FALSE
#             )
#         }
#     )
#     do.call(rbind, tbl)
# }

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
    system("git status")
    system("git branch -a")
    branches <- git2r::branches(flags = "remote")
    branch_names <- gsub("origin/", "", sapply(branches, \(x) x$name), fixed = TRUE)

    print(sapply(branches, \(x) x$name))

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
        development = dev_branch
    )
})
names(index) <- gsub("modules/", "", mods, fixed = TRUE)

print(index)

yaml::write_yaml(index, "modules.yml")
