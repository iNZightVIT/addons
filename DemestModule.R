#' @name Demographic Modelling Module
#' @author Tom Elliott
#' @version 0.0.1
#' @desc Bayesian small domain estimation
DemestModule <- setRefClass(
    "Demographic Modelling Module",
    contains = "CustomModule",
    properties(
        fields = list(
            GUI = "ANY",
            varnames = "character", vartypes = "character",
            model_types = "list",
            response = "character", response_box = "ANY",
            model = "character", model_box = "ANY",
            model_fw = "character", model_fw_box = "ANY",
            secondaryvar = "character", secondaryvar_box = "ANY",
            exposure_lbl = "ANY",
            binomial_type = "character", binomial_type_chk = "ANY",
            reponse_confirmed = "logical"
            # response_type = "ANY",
            # exposure_label = "ANY", exposure_var = "ANY",
            # tab_data = "ANY",
            # exposure = "ANY",
            # used_vars = "ANY",
            # vars_ok_btn = "ANY",
            # response_lhfun = "ANY",
            # response_lhfmla = "ANY"
        ),
        prototype = list(
            response = NA_character_,
            model = NA_character_,
            model_fw = NA_character_,
            secondaryvar = NA_character_,
            binomial_type = NA_character_,
            reponse_confirmed = FALSE
        )
    ),
    methods = list(
        initialize = function(gui, name) {
            callSuper(gui,
                name = name,
                embedded = TRUE
            )

            initFields(
                varnames = names(get_data()),
                vartypes = sapply(get_data(), iNZightTools::vartype)
            )

            install_dependencies(
                c("tidyr")
                # github = c(
                #     "StatisticsNZ/dembase",
                #     "StatisticsNZ/demest"
                # )
            )

            ## The main code for your module goes here,
            ## inside a top-level container called "mainGrp"
            mainGrp$set_borderwidth(5)

            ### -------------------------------------- Response
            g_response <- gexpandgroup("Response options",
                container = mainGrp
            )
            font(g_response) <- list(weight = "bold")

            g_response$set_borderwidth(5)
            tbl_response <- glayout(container = g_response,
                expand = TRUE)
            ii <- 1L

            response_box <<- gcombobox(
                varnames[vartypes == "num"],
                selected = 0L,
                handler = function(h, ...) response <<- svalue(h$obj)
            )
            lbl <- glabel("Response: ")
            tbl_response[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- response_box
            ii <- ii + 1L

            model_types <<- list(
                "Births" = "Poisson",
                "Deaths" = "Poisson",
                "Migration" = "Poisson",
                "Income" = "Normal",
                "School" = c("Normal", "Binomial", "Poisson"),
                "Other" = c("Normal", "Binomial", "Poisson")
            )
            model_box <<- gcombobox(
                names(model_types),
                selected = 0L,
                handler = function(h, ...) {
                    if (length(svalue(h$obj)))
                        model <<- svalue(h$obj)
                    else
                        model <<- NA_character_
                }
            )
            enabled(model_box) <<- FALSE
            lbl <- glabel("Type: ")
            tbl_response[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- model_box
            ii <- ii + 1L

            model_fw_box <<- gcombobox(
                model_types$Other,
                selected = 0L,
                handler = function(h, ...) {
                    if (length(svalue(h$obj)))
                        model_fw <<- svalue(h$obj)
                    else
                        model_fw <<- NA_character_
                }
            )
            enabled(model_fw_box) <<- FALSE
            lbl <- glabel("Framework: ")
            tbl_response[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- model_fw_box
            ii <- ii + 1L

            secondaryvar_box <<- gcombobox(
                character(),
                selected = 0L,
                handler = function(h, ...) {

                }
            )
            visible(secondaryvar_box) <<- FALSE
            exposure_lbl <<- glabel("Exposure: ")
            visible(exposure_lbl) <<- FALSE
            binomial_type_chk <<- gradio(
                c("Total", "Failures"),
                horizontal = TRUE,
                handler = function(h, ...) binomial_type <<- svalue(h$obj)
            )
            visible(binomial_type_chk) <<- FALSE
            econt <- gvbox()
            add(econt, exposure_lbl, anchor = c(1, 0))
            add(econt, binomial_type_chk, anchor = c(1, 0))
            tbl_response[ii, 1L, anchor = c(1, 0), expand = TRUE] <- econt
            tbl_response[ii, 2:3, expand = TRUE] <- secondaryvar_box
            ii <- ii + 1L


            ### -------------------------------------- Variable info
            g_vars <- gexpandgroup("Variable information",
                container = mainGrp
            )
            visible(g_vars) <- FALSE
            font(g_vars) <- list(weight = "bold")

            g_vars$set_borderwidth(5)
            tbl_vars <- glayout(container = g_vars,
                expand = TRUE)
            ii <- 1L


            ### -------------------------------------- Model specification
            g_likelihood <- gexpandgroup("Model specification",
                container = mainGrp
            )
            visible(g_likelihood) <- FALSE
            font(g_likelihood) <- list(weight = "bold")
            g_likelihood$set_borderwidth(5)

            tbl_likelihood <- glayout(container = g_likelihood,
                expand = TRUE)
            ii <- 1L


            ## --------- initialize signals for when things change:
            addResponseObserver(function() {
                # trigger setting response model (based on type)
                print(glue::glue("Response variable: {response}"))
                type <- vartypes[varnames == response]
                print(glue::glue(" * type: {type}"))

                model_names <- model_box$get_items()
                model_names <- tolower(gsub("s$", "", model_names)) # remove plural

                rt_match <- sapply(model_names, grepl, x = tolower(response))
                if (any(rt_match)) {
                    model_box$set_index(which(rt_match))
                } else {
                    model_box$set_index(0L)
                }

                enabled(model_box) <<- !is.na(response)

                # update list of available variables in 'secondary outcome var'
                # (i.e., exposure/totals)


                # update list of available variables in "Variable List"

            })
            addModelObserver(function() {
                if (is.na(model)) {
                    # disable things
                    enabled(model_fw_box) <<- FALSE
                    model_fw_box$set_index(0L)
                    model_fw_box$set_items(model_types$Other)
                } else if (model == "Other") {
                    print(glue::glue(" * model: other"))
                    enabled(model_fw_box) <<- TRUE
                } else {
                    print(glue::glue(" * model: {model}"))
                    fw_opts <- model_types[[model]]

                    blockHandlers(model_fw_box)
                    model_fw_box$set_index(0L)
                    model_fw_box$set_items(fw_opts)
                    unblockHandlers(model_fw_box)

                    if (length(fw_opts) == 1L)
                        model_fw_box$set_index(1L)
                    enabled(model_fw_box) <<- length(fw_opts) > 1L
                }

            })
            addModelFWObserver(function() {
                if (is.na(model_fw) || model_fw == "Normal") {
                    print(glue::glue(" * framework: unknown"))
                    # disable secondary var stuff:
                    visible(secondaryvar_box) <<- FALSE
                    visible(exposure_lbl) <<- FALSE
                    visible(binomial_type_chk) <<- FALSE

                    return()
                }
                print(glue::glue(" * framework: {model_fw}"))

                numvars <- varnames[vartypes == "num" & varnames != response]
                w <- sapply(numvars, function(v) all(get_data()[[v]] == round(get_data()[[v]])))
                intvars <- numvars[w]

                secondaryvar_box$set_items(c("", intvars))
                secondaryvar_box$set_index(1L)

                visible(secondaryvar_box) <<- TRUE
                visible(exposure_lbl) <<- model_fw == "Poisson"
                visible(binomial_type_chk) <<- model_fw == "Binomial"

            })
        },
        updatePlot = function() {
            cat("+------+\n")
            cat("| plot |\n")
            cat("+------+\n")
        },
        close = function() {
            # any module-specific clean up?
            # delete temp files, etc ...

            callSuper()
        },
        addResponseObserver = function(FUN, ...) {
            .self$responseChanged$connect(FUN, ...)
        },
        addModelObserver = function(FUN, ...) {
            .self$modelChanged$connect(FUN, ...)
        },
        addModelFWObserver = function(FUN, ...) {
            .self$model_fwChanged$connect(FUN, ...)
        }
    )
)

invisible(list(
    x = list(
        y = function() {
            varnames <- colnames(raw_data)
            vartypes <- sapply(raw_data, iNZightTools::vartype)
            response_var <<- gcombobox(varnames[vartypes == "num"],
                selected = 0L,
                handler = function(h, ...) {
                    set_vars()
                    vn <- svalue(h$obj)
                    rt_match <- sapply(response_type$get_items(),
                        function(x) grepl(tolower(x), tolower(vn)))
                    if (any(rt_match)) {
                        response_type$set_index(which(rt_match))
                    }
                    enabled(response_type) <<- TRUE
                }
            )
            lbl <- glabel("Response variable: ")
            tbl_response[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- response_var
            ii <- ii + 1L

            response_type <<- gcombobox(
                c("Births", "Deaths", "Migration", "Income", "School", "Counts", "Values"),
                selected = 0L,
                handler = function(h, ...) {
                    if (svalue(h$obj) %in% c("Births", "Deaths", "Counts")) {
                        response_lhfun$set_items(c("Poisson", "Binomial"))
                    } else {
                        response_lhfun$set_items(c("Normal"))
                        response_lhfun$set_index(1L)
                    }
                    enabled(response_lhfun) <<- TRUE
                }
            )
            enabled(response_type) <<- FALSE
            lbl <- glabel("Response type: ")
            tbl_response[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- response_type
            ii <- ii + 1L


            response_lhfun <<- gcombobox(c("Normal", "Poisson", "Binomial"),
                selected = 0L,
                handler = function(h, ...) {
                    if (h$obj$get_index()) {
                        svalue(exposure_label) <<-
                            ifelse(svalue(h$obj) == "Binomial",
                                "Sample size: ",
                                "Exposure variable: "
                            )
                        enabled(exposure_var) <<- TRUE
                    } else {
                        svalue(exposure_label) <<- ""
                        enabled(exposure_var) <<- FALSE
                    }
                }
            )
            enabled(response_lhfun) <<- FALSE
            lbl <- glabel("Response framework: ")
            tbl_response[ii, 1L, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl_response[ii, 2:3, expand = TRUE] <- response_lhfun
            ii <- ii + 1L

            exposure_var <<- gcombobox(c("None", varnames[vartypes == "num"]),
                selected = 1L,
                handler = function(h, ...) set_vars()
            )
            enabled(exposure_var) <<- FALSE
            exposure_label <<- glabel("")
            tbl_response[ii, 1L, anchor = c(1, 0), expand = TRUE] <- exposure_label
            tbl_response[ii, 2:3, expand = TRUE] <- exposure_var
            ii <- ii + 1L

            ### -------------------------------------- Variable info
            g_vars <- gexpandgroup("Variable information",
                container = mainGrp
            )
            visible(g_vars) <- FALSE
            font(g_vars) <- list(weight = "bold")

            g_vars$set_borderwidth(5)
            tbl_vars <- glayout(container = g_vars,
                expand = TRUE)
            ii <- 1L

            used_vars <<- gtable(varnames, multiple = TRUE)
            addHandlerSelectionChanged(used_vars,
                handler = function(h, ...) set_vars()
            )
            tbl_vars[ii, 3L, expand = TRUE] <- used_vars
            size(used_vars) <<- c(-1, 200)
            ii <- ii + 1L

            vars_ok_btn <<- gbutton("Continue",
                handler = function(h, ...) {
                    visible(g_vars) <- FALSE
                    visible(g_likelihood) <- TRUE
                }
            )
            tbl_vars[ii, 3L, expand = TRUE] <- vars_ok_btn
            ii <- ii + 1L

            ### -------------------------------------- Variable info
            g_likelihood <- gexpandgroup("Model likelihood",
                container = mainGrp
            )
            visible(g_likelihood) <- FALSE
            font(g_likelihood) <- list(weight = "bold")
            g_likelihood$set_borderwidth(5)

            tbl_likelihood <- glayout(container = g_likelihood,
                expand = TRUE)
            ii <- 1L

            # Values: normal
            # Counts: Binomial, Poisson
            # response_lhfun <<- gcombobox("Select response variable")
            response_lhfmla <<- gedit("")

            tbl_likelihood[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- glabel("Response type :")
            # tbl_likelihood[ii, 2:3, expand = TRUE] <- response_lhfun
            ii <- ii + 1L

            tbl_likelihood[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- glabel("Formula :")
            tbl_likelihood[ii, 2:3, expand = TRUE] <- response_lhfmla

            updatePlot()
        },
        set_vars = function() {
            if (response_var$get_index() == 0) return()
            if (response_type$get_index() == 0) return()

            var_y <- svalue(response_var)
            var_n <- if (exposure_var$get_index() > 1L) svalue(exposure_var) else NULL
            r_type <- svalue(response_type)

            vars_all <- names(raw_data)
            vars <- vars_all[vars_all %notin% c(var_y, var_n)]

            chosen_vars <- svalue(used_vars)
            blockHandlers(used_vars)
            if (length(chosen_vars)) {
                chosen_vars <- chosen_vars[chosen_vars %in% vars]
                used_vars$set_items(vars)
                used_vars$set_value(chosen_vars)
            } else {
                chosen_vars <- vars
                used_vars$set_items(vars)
            }
            unblockHandlers(used_vars)

            d_types <- NULL
            d_scales <- NULL

            tab_data <<- make_dem_array(raw_data, var_y, vars, r_type, d_types, d_scales)
            exposure <<- make_dem_array(raw_data, var_n, vars, "Counts", d_types, d_scales)

            # set likelihood info
            response_lhfun$set_items(
                if (r_type == "Values") "Normal" else c("Poisson", "Binomial")
            )
            response_lhfun$set_index(1L)
            response_lhfmla$set_value(
                sprintf("(%s)^2", paste(chosen_vars, collapse = " + "))
            )

            updatePlot()
        },
        make_dem_array = function(data, y, x, type, dimtypes, dimscales) {
            if (is.null(y)) return(NULL)
            d <- raw_data
            arr <- tapply(raw_data[[y]], raw_data[x], c)
            f <- switch(type,
                Counts = dembase::Counts,
                Values = dembase::Values
            )
            f(arr, dimtypes, dimscales)
        },
        ## add new methods to simplify your code
        updatePlot = function() {
            if (is.null(tab_data)) {
                text(-1, 1, "Select a response variable", adj = 0)
                return()
            }

            # This function is linked to the "refresh plot" button.
            # It should generate a plot purely from the values
            # of variables stored in "fields".
            # The UI then alters the values of the fields and calls this function.

            dtypes <- dembase::dimtypes(tab_data)
            tvars <- c("age", "time")
            tvar <- which(tvars %in% dtypes)
            if (length(tvar)) tvar <- tvar[1]
            else return() # no time variable?
            vars <- names(dtypes)
            ovar <- vars[-tvar]
            tvar <- vars[tvar]
            vars <- c(tvar, ovar)

            tmp <- tab_data
            etmp <- exposure
            if (length(svalue(used_vars)) && !is.null(exposure)) {
                if (length(svalue(used_vars)) < length(vars)) {
                    cvar <- vars[vars %notin% svalue(used_vars)]
                    vars <- vars[vars %in% svalue(used_vars)]
                    tmp <- dembase::collapseDimension(
                        tmp,
                        dimension = cvar,
                        weights = exposure
                    )
                    etmp <- dembase::collapseDimension(exposure, dimension = cvar)
                }
            }
            if (length(vars) > 4) {
                # collapse down dimensions; requires counts
                cvar <- vars[vars %notin% svalue(used_vars)[1:4]]
                vars <- vars[vars %in% svalue(used_vars)[1:4]]
                if (svalue(response_type) == "Counts") {
                    tmp <- dembase::collapseDimension(tmp, dimension = cvar)
                } else if (!is.null(exposure)) {
                    tmp <- dembase::collapseDimension(
                        tmp,
                        dimension = cvar,
                        weights = exposure
                    )
                    etmp <- dembase::collapseDimension(exposure, dimension = cvar)
                } else {
                    stop("Please specify exposure variable")
                }
            }
            if (!is.null(exposure)) {
                tmp <- tmp / etmp
            }

            df <- as.data.frame(tmp,
                direction = "long",
                midpoints = tvar)

            # print(vars)
            p <- ggplot2::ggplot(df,
                ggplot2::aes_(
                    as.name(vars[1]),
                    ~value,
                    colour = if (length(vars) > 1) as.name(vars[2]) else NULL
                )
            ) +
                ggplot2::geom_path() +
                ggplot2::ylab(svalue(response_var)) +
                ggplot2::theme_minimal()

            if (length(vars) == 3) {
                p <- p +
                    ggplot2::facet_wrap(
                        ggplot2::vars(!!rlang::sym(vars[3]))
                    )
            } else if (length(vars) == 4) {
                p <- p +
                    ggplot2::facet_grid(
                        ggplot2::vars(!!rlang::sym(vars[3])),
                        ggplot2::vars(!!rlang::sym(vars[4]))
                    )
            }

            print(p)

            # gvar <- ovar[1] # grouping variable
            # if (length(ovar) > 2L) # subsetting variable(s)
            #     svar <- paste(" |", paste(ovar[-(1L:2L)], collapse = " + "))
            # else
            #     svar <- ""

            # fmla <- sprintf("~ %s%s", tvar, svar)
            # print(fmla)
            # dembase::dplot(eval(parse(text = fmla)),
            #     data = tab_data,
            #     groups = eval(rlang::ensym(gvar))
            # )

        },
        close = function() {
            # any module-specific clean up?
            # delete temp files, etc ...

            callSuper()
        }
    )
    ## This is currently required to get around namespace locking
    # where = e
))
