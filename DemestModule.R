#' @name Demographic Modelling Module
#' @author Tom Elliott
#' @version 0.0.1
#' @desc Bayesian small domain estimation
DemestModule <- setRefClass(
    "Demographic Modelling Module",
    contains = "CustomModule",
    properties(
        fields = list(
            GUI = "ANY", data = "data.frame",
            g_response = "ANY", g_vars = "ANY", g_model = "ANY",
            g_fit = "ANY",
            varnames = "character", vartypes = "character",
            model_types = "list",
            # -- response
            response = "character", response_box = "ANY",
            model = "character", model_box = "ANY",
            model_fw = "character", model_fw_box = "ANY",
            secondaryvar = "character", secondaryvar_box = "ANY",
            exposure_lbl = "ANY",
            binomial_type = "character", binomial_type_chk = "ANY",
            response_confirmed = "logical", response_conf_btn = "ANY",
            response_state = "list",
            # -- variables
            variable_df = "ANY",
            variables = "character", dim_info = "list",
            var_table = "data.frame",
            variables_confirmed = "logical", variables_conf_btn = "ANY",
            # -- model info
            demarray = "ANY", altarray = "ANY",
            model_lbl_likelihood = "ANY",
            model_lbl_mean = "ANY", model_lbl_var = "ANY",
            model_fmla = "character", model_fmla_box = "ANY",
            model_confirmed = "logical", model_confirmed_btn = "ANY",
            # -- fit specs
            fit_niter = "integer", fit_niter_spec = "ANY",
            fit_nburn = "integer", fit_nburn_spec = "ANY",
            fit_nthin = "integer", fit_nthin_spec = "ANY",
            fit_nchain = "integer", fit_nchain_spec = "ANY",
            fit_ncore = "integer", fit_ncore_spec = "ANY",
            model_object = "ANY",
            model_file = "character",
            fit_model_btn = "ANY",
            model_exists = "logical"
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
            response_confirmed = FALSE,
            variables = NA_character_,
            var_table = data.frame(
                Use = logical(),
                Variable = character(),
                Type = factor(levels = c("state", "age", "sex", "time")),
                Scale = factor(levels = c("Categories", "Intervals", "Points", "Sexes"))
            ),
            variables_confirmed = FALSE,
            model_confirmed = FALSE,
            fit_niter = 1e2L, fit_nburn = 1e2L,
            fit_nthin = 2L, fit_nchain = 4L, fit_ncore = 1L,
            model_exists = FALSE
        )
    ),
    methods = list(
        initialize = function(gui, name) {
            callSuper(gui,
                name = name,
                embedded = TRUE
            )

            data <<- get_data()
            varnames <<- names(data)
            vartypes <<- sapply(data, iNZightTools::vartype)
            model_file <<- tempfile()

            install_dependencies(
                c("tidyr", "ggplot2")
                # github = c(
                #     "StatisticsNZ/dembase",
                #     "StatisticsNZ/demest"
                # )
            )

            ## The main code for your module goes here,
            ## inside a top-level container called "mainGrp"
            mainGrp$set_borderwidth(5)

            ### -------------------------------------- Response
            g_response <<- gexpandgroup("Response options",
                container = mainGrp
            )
            font(g_response) <<- list(weight = "bold")

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
                    if (length(svalue(h$obj)) && svalue(h$obj) != "")
                        secondaryvar <<- svalue(h$obj)
                    else
                        secondaryvar <<- NA_character_
                }
            )
            visible(secondaryvar_box) <<- FALSE
            exposure_lbl <<- glabel("")
            tbl_response[ii, 1L, anchor = c(1, 0), expand = TRUE] <- exposure_lbl
            tbl_response[ii, 2:3, expand = TRUE] <- secondaryvar_box
            ii <- ii + 1L

            response_conf_btn <<- gbutton("Save",
                handler = function(h, ...) {
                    response_confirmed <<- !response_confirmed
                    blockHandlers(response_conf_btn)
                    svalue(response_conf_btn) <<- ifelse(
                        response_confirmed,
                        "Modify",
                        "Save"
                    )
                    unblockHandlers(response_conf_btn)
                }
            )
            visible(response_conf_btn) <<- FALSE
            tbl_response[ii, 2:3, expand = TRUE] <- response_conf_btn
            ii <- ii + 1L


            ### -------------------------------------- Variable info
            g_vars <<- gexpandgroup("Variable information",
                container = mainGrp,
                horizontal = FALSE
            )
            visible(g_vars) <<- FALSE
            font(g_vars) <<- list(weight = "bold")

            g_vars$set_borderwidth(5)
            # tbl_vars <- glayout(container = g_vars,
            #     expand = TRUE)
            # ii <- 1L

            variable_df <<- gdf(var_table)
            add(g_vars, variable_df, expand = TRUE)
            size(variable_df) <<- c(-1, 180)

            addHandlerChanged(variable_df,
                function(h, ...) {
                    if (variable_df$get_dim()[1] == 0L) return()
                    var_table <<- variable_df$get_frame()
                    i <- h$obj$get_selected()
                    if (length(i))
                        plot_scale(i)
                }
            )

            variables_conf_btn <<- gbutton("Save",
                handler = function(h, ...) {
                    variables_confirmed <<- !variables_confirmed
                    blockHandlers(variables_conf_btn)
                    svalue(variables_conf_btn) <<- ifelse(
                        variables_confirmed,
                        "Modify",
                        "Save"
                    )
                    unblockHandlers(variables_conf_btn)
                }
            )

            addSpace(g_vars, 5)
            g_var_btns <- ggroup(container = g_vars)
            addSpring(g_var_btns)
            add(g_var_btns, variables_conf_btn)
            # tbl_vars[ii, 2:3, expand = TRUE] <- variables_conf_btn
            # ii <- ii + 1L


            ### -------------------------------------- Model specification
            g_model <<- gexpandgroup("Model specification",
                container = mainGrp
            )
            visible(g_model) <<- FALSE
            font(g_model) <<- list(weight = "bold")
            g_model$set_borderwidth(5)

            tbl_likelihood <- glayout(container = g_model,
                expand = TRUE)
            ii <- 1L

            model_lbl_likelihood <<- glabel("Choose response first")
            tbl_likelihood[ii, 1:3, expand = TRUE, anchor = c(0, 0)] <-
                model_lbl_likelihood
            ii <- ii + 1L

            model_lbl_mean <<- glabel("")
            model_fmla_box <<- gedit("",
                handler = function(h, ...) model_fmla <<- svalue(h$obj)
            )
            tbl_likelihood[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- model_lbl_mean
            tbl_likelihood[ii, 2:3, expand = TRUE, fill = TRUE] <- model_fmla_box
            ii <- ii + 1L

            # only for Normal models ... usually just going to be fixed..?
            model_lbl_var <<- glabel("")

            model_confirmed_btn <<- gbutton("Save",
                handler = function(h, ...) {
                    model_confirmed <<- !model_confirmed
                    blockHandlers(model_confirmed_btn)
                    svalue(model_confirmed_btn) <<- ifelse(
                        model_confirmed,
                        "Modify",
                        "Save"
                    )
                    unblockHandlers(model_confirmed_btn)
                }
            )
            tbl_likelihood[ii, 2:3, expand = TRUE] <- model_confirmed_btn
            ii <- ii + 1L


            ### -------------------------------------- Fit specifications
            g_fit <<- gexpandgroup("Simulation parameters",
                container = mainGrp
            )
            visible(g_fit) <<- FALSE
            font(g_fit) <<- list(weight = "bold")
            g_fit$set_borderwidth(5)

            tbl_params <- glayout(container = g_fit,
                expand = TRUE)
            ii <- 1L

            lbl <- glabel("Iterations: ")
            fit_niter_spec <<- gspinbutton(1e2L, 1e6L, by = 1e2L,
                value = fit_niter,
                handler = function(h, ...) {
                    fit_niter <<- svalue(h$obj)
                }
            )
            tbl_params[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl_params[ii, 2:3, expand = TRUE, fill = TRUE] <- fit_niter_spec
            ii <- ii + 1L

            lbl <- glabel("Burn-in: ")
            fit_nburn_spec <<- gspinbutton(1e2L, 1e6L, by = 1e2L,
                value = fit_nburn,
                handler = function(h, ...) {
                    fit_nburn <<- svalue(h$obj)
                }
            )
            tbl_params[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl_params[ii, 2:3, expand = TRUE, fill = TRUE] <- fit_nburn_spec
            ii <- ii + 1L

            lbl <- glabel("Thinning Interval: ")
            fit_nthin_spec <<- gspinbutton(1L, 1e2L, by = 1L,
                value = fit_nthin,
                handler = function(h, ...) {
                    fit_nthin <<- svalue(h$obj)
                }
            )
            tbl_params[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl_params[ii, 2:3, expand = TRUE, fill = TRUE] <- fit_nthin_spec
            ii <- ii + 1L

            lbl <- glabel("Chains: ")
            fit_nchain_spec <<- gspinbutton(1L, 10L,
                by = 1L,
                value = fit_nchain,
                handler = function(h, ...) {
                    fit_nchain <<- svalue(h$obj)
                }
            )
            tbl_params[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl_params[ii, 2:3, expand = TRUE, fill = TRUE] <- fit_nchain_spec
            ii <- ii + 1L

            lbl <- glabel("CPU Cores: ")
            fit_nchain_spec <<- gslider(1L, parallel::detectCores(),
                by = 1L,
                value = fit_ncore,
                handler = function(h, ...) {
                    fit_nchain <<- svalue(h$obj)
                }
            )
            tbl_params[ii, 1L, expand = TRUE, anchor = c(1, 0)] <- lbl
            tbl_params[ii, 2:3, expand = TRUE, fill = TRUE] <- fit_nchain_spec
            ii <- ii + 1L

            # info about total sample size after thinning


            fit_model_btn <<- gbutton("Fit Model",
                handler = function(h, ...) fit_model())
            tbl_params[ii, 2:3, expand = TRUE, fill = TRUE] <- fit_model_btn
            ii <- ii + 1L



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
            })
            addModelObserver(function() {
                if (is.na(model)) {
                    # disable things
                    enabled(model_fw_box) <<- FALSE
                    model_fw_box$set_index(0L)
                    model_fw_box$set_items(model_types$Other)
                } else {
                    print(glue::glue(" * model: {model}"))
                    fw_opts <- model_types[[model]]

                    blockHandlers(model_fw_box)
                    v <- model_fw_box$get_value()
                    model_fw_box$set_index(0L)
                    model_fw_box$set_items(fw_opts)

                    if (length(fw_opts) == 1L) {
                        model_fw_box$set_index(1L)
                    } else if (length(v) && v %in% fw_opts) {
                        model_fw_box$set_value(v)
                    }
                    enabled(model_fw_box) <<- length(fw_opts) > 1L

                    unblockHandlers(model_fw_box)
                    model_fw_box$invoke_change_handler()
                }

            })
            addModelFWObserver(function() {
                visible(response_conf_btn) <<- !is.na(model_fw)
                if (is.na(model_fw) || model_fw == "Normal") {
                    print(glue::glue(" * framework: unknown"))
                    visible(secondaryvar_box) <<- FALSE
                    svalue(exposure_lbl) <<- ""
                    return()
                }
                print(glue::glue(" * framework: {model_fw}"))

                numvars <- varnames[vartypes == "num" & varnames != response]
                w <- sapply(numvars,
                    function(v) all(data[[v]] == round(data[[v]]))
                )
                intvars <- numvars[w]

                secondaryvar_box$set_items(c("", intvars))
                secondaryvar_box$set_index(1L)

                visible(secondaryvar_box) <<- TRUE
                enabled(secondaryvar_box) <<- TRUE
                svalue(exposure_lbl) <<- switch(model_fw,
                    "Poisson" = "Exposure: ",
                    "Binomial" = "Total: "
                )
            })
            addSecondaryVarObserver(function() {
                vtype <- switch(model_fw,
                    "Poisson" = "exposure",
                    "Binomial" = "total",
                    ""
                )
                if (vtype == "") return()

                if (is.na(secondaryvar)) {
                    print(glue::glue(" * {vtype}: unknown"))
                    # visible(response_conf_btn) <<- FALSE
                    return()
                }
                print(glue::glue(" * {vtype}: {secondaryvar}"))
                # visible(response_conf_btn) <<- TRUE
            })
            addResponseConfirmedObserver(function() {
                if (response_confirmed) {
                    x <- c("response_box", "model_box", "model_fw_box", "secondaryvar_box")
                    response_state <<- lapply(x, function(z) enabled(.self[[z]]))
                    names(response_state) <<- x
                    enabled(response_box) <<- FALSE
                    enabled(model_box) <<- FALSE
                    enabled(model_fw_box) <<- FALSE
                    enabled(secondaryvar_box) <<- FALSE

                    # set variables
                    set_variables()

                    # set model spec
                    set_model()
                } else {
                    enabled(response_box) <<- response_state$response_box
                    enabled(model_box) <<- response_state$model_box
                    enabled(model_fw_box) <<- response_state$model_fw_box
                    enabled(secondaryvar_box) <<- response_state$secondaryvar_box
                    svalue(model_lbl_likelihood) <<- "Select response first"
                }
                visible(g_response) <<- !response_confirmed
                visible(g_vars) <<- response_confirmed

                variables_confirmed <<- FALSE
            })

            addVarTableObserver(function() {
                # print(var_table)
            })
            addVariablesConfirmedObserver(function() {
                enabled(variable_df) <<- !variables_confirmed
                visible(g_vars) <<- !variables_confirmed
                visible(g_model) <<- variables_confirmed

                if (variables_confirmed)
                    make_arrays()
            })

            addModelConfirmedObserver(function() {
                enabled(model_fmla_box) <<- !model_confirmed
                visible(g_model) <<- !model_confirmed
                visible(g_fit) <<- model_confirmed

                if (model_confirmed)
                    save_model()
            })
        },
        set_variables = function() {
            vars <- varnames[varnames %notin% c(response, secondaryvar)]
            variables <<- vars
            dtypes <- dembase:::inferDimtypes(vars)
            dim_info <<- sapply(seq_along(vars),
                function(i) {
                    dembase::inferDimScale(dtypes[i],
                        labels = as.character(unique(data[[i]])),
                        name = vars[i]
                    )
                }
            )
            dscale_names <- sapply(dim_info, dembase::dimscales)
            var_table <<- data.frame(
                Use = rep(TRUE, length(vars)),
                Variable = vars,
                Type = factor(dtypes,
                    levels = c("state", "age", "sex", "time")),
                Scale = factor(dscale_names,
                    levels = c("Categories", "Intervals", "Points", "Sexes"))
            )
            variable_df$set_frame(var_table)
            variable_df$cmd_coerce_column(1L, as.logical)
            variable_df$hide_row_names(TRUE)
        },
        plot_scale = function(i) {
            d <- dim_info[[i]]
            s <- dembase::dimscales(d)
            x <- dembase:::dimvalues(d)
            switch(s,
                "Intervals" = {
                    y <- cut(x[is.finite(x)],
                        breaks = x,
                        right = FALSE,
                        include.lowest = max(x) < Inf
                    )
                    plot(unique(y), yaxt = "n",
                        xlab = var_table$Variable[i])
                },
                {
                    plot(factor(x, levels = x), yaxt = "n",
                        xlab = var_table$Variable[i])
                }
            )
        },
        make_dem_array = function(data, y, x, fw, dimtypes, dimscales) {
            if (is.null(y)) return(NULL)
            arr <- tapply(data[[y]], data[x], c)
            f <- switch(fw,
                "Normal" = ,
                "Values" = dembase::Values,
                "Poisson" = ,
                "Binomial" = ,
                "Counts" = dembase::Counts
            )
            f(arr, dimtypes, dimscales)
        },
        make_arrays = function() {
            vars <- var_table$Variable[var_table$Use]
            vtypes <- as.character(var_table$Type[var_table$Use])
            vscales <- as.character(var_table$Scale[var_table$Use])
            names(vtypes) <- names(vscales) <- vars
            demarray <<- make_dem_array(
                data,
                response,
                vars,
                model_fw,
                vtypes,
                vscales
            )
            altarray <<- NULL
            if (model_fw %in% c("Poisson", "Binomial") &&
                !is.na(secondaryvar)) {

                altarray <<- make_dem_array(
                    data,
                    secondaryvar,
                    vars,
                    "Counts",
                    vtypes,
                    vscales
                )
                print(altarray)
            }

            # collapsedimensions...
            # if (!all(var_table$Use) && !is.null(altarray)) {
            #     demarray <<- dembase::collapseDimension(
            #         demarray,
            #         dimension = var_table$Variable[!var_table$Use],
            #         weights = altarray
            #     )
            #     altarray <<- dembase::collapseDimension(
            #         altarray,
            #         dimension = var_table$Variable[!var_table$Use]
            #     )
            # }

            updatePlot()
        },
        updatePlot = function() {
            cat("++ Response: ")
            cat(response)
            if (!is.na(secondaryvar)) cat(" /", secondaryvar)

            vt <- var_table[var_table$Use,]

            cat("\n++ Variables: ")
            vars <- vt$Variable
            cat(paste(vars, collapse = ", "))


            # first figure out the x-axis: usually age
            x_var <- NA_character_
            if (any(vt$Type == "age")) {
                x_var <- vt$Variable[vt$Type == "age"]
            } else if (any(vt$Scale == "Intervals")) {
                x_var <- vt$Variable[vt$Scale == "Intervals"]
            } else {
                stop("No continuous variable to use as x-variable")
            }
            x_var <- x_var[1L] # in case length > 1L

            # now the colour variable: default gender
            c_var <- NA_character_
            if (any(vt$Type == "sex")) {
                c_var <- vt$Variable[vt$Type == "sex"]
            } else {
                message("No colour variable (sex) detected")
            }

            # finally, subsetting variables
            svars <- vars[vars %notin% c(x_var, c_var)]
            if (length(svars) > 2L) {
                svars <- svars[1:2]
            }
            subset <- "NONE"
            if (length(svars))
                subset <- paste(svars, collapse = " + ")

            # and the fmla:
            fmla <- glue::glue("{response} ~ {x_var} | {subset}")

            cat("\n++ formula: ")
            cat(fmla)

            cat("\n\n")

            tarr <- demarray
            ylab <- response
            if (!is.na(secondaryvar)) {
                tarr <- tarr / altarray
                ylab <- paste(ylab, secondaryvar, sep = " / ")
            }

            # if age is Intervals (not points):
            df <- as.data.frame(tarr,
                direction = "long",
                midpoints = x_var
            )

            cname <- names(df)[ncol(df)]
            print(cname)
            print(head(df))
            p <- ggplot2::ggplot(df,
                    ggplot2::aes_(
                        x = as.name(x_var),
                        y = as.name(cname),
                        colour = if (is.na(c_var)) NULL else as.name(c_var)
                    )
                )

            if (model_exists) {
                # grab means/rates from results:
                v <- switch(model_fw,
                    "Poisson" = "rate",
                    "Binomial" = "prob",
                    "Normal" = "mean"
                )
                fitted_y <- demest::fetch(model_file,
                    where = c("model", "likelihood", v)
                )
                alpha <- 0.95
                a <- 1 - alpha
                fitted_q <- dembase::collapseIterations(
                    fitted_y,
                    prob = c(a / 2, 0.5, 1 - a / 2)
                )
                fitted_df <- as.data.frame(fitted_q,
                    direction = "long",
                    midpoints = x_var
                )
                fitted_df$quantile <- as.factor(fitted_df$quantile)
                levels(fitted_df$quantile) <- c("lower", "median", "upper")
                fitted_df_wide <- tidyr::pivot_wider(fitted_df,
                    names_from = quantile, values_from = value
                )
                p <- p +
                    ggplot2::geom_ribbon(
                        ggplot2::aes_(
                            y = NULL, ymin = ~lower, ymax = ~upper,
                            fill = if (is.na(c_var)) NULL else as.name(c_var)
                        ),
                        alpha = 0.1,
                        colour = NA,
                        data = fitted_df_wide,
                        na.rm = TRUE
                    ) +
                    ggplot2::geom_path(
                        ggplot2::aes_(
                            y = ~median,
                            colour = if (is.na(c_var)) NULL else as.name(c_var)
                        ),
                        data = fitted_df_wide,
                        na.rm = TRUE
                    )
            }

            p <- p +
                ggplot2::geom_point() +
                ggplot2::geom_path(na.rm = TRUE) +
                ggplot2::theme_minimal() +
                ggplot2::ylab(ylab)

            if (length(svars)) {
                f1 <- ggplot2::vars(.data[[svars[1]]])
                f2 <- if (length(svars) > 1L)
                    ggplot2::vars(.data[[svars[2]]])
                    else NULL
                p <- p +
                    ggplot2::facet_grid(f1, f2)
            }

            print(p)

        },
        set_model = function() {
            Model <- model_fw
            Exposure <- ifelse(is.na(secondaryvar), "", secondaryvar)
            par <- switch(Model,
                "Poisson" = "\U03BB",
                "Normal" = "\U03BC",
                "Binomial" = "\U03C0"
            )
            param <- switch(Model,
                "Poisson" =
                    paste0(ifelse(is.na(secondaryvar), "", "n"), par),
                "Normal" = paste0(par, ", \U03C3\U00B2"),
                "Binomial" =
                    paste0(ifelse(is.na(secondaryvar), "", "n, "), par)
            )
            lbl <- glue::glue("{response} ~ {Model}({param})")
            svalue(model_lbl_likelihood) <<- lbl

            svalue(model_lbl_mean) <<- glue::glue("{par} = ")
            svalue(model_fmla_box) <<- paste(
                var_table$Variable[var_table$Use],
                collapse = " + "
            )
        },
        save_model = function() {
            args <- ""
            if (model_fw == "Poisson") {
                args <- glue::glue("{args}, useExpose = {!is.na(secondaryvar)}")
            }
            model_expr <- glue::glue(
                "demest::Model(
                    y ~ demest::{model_fw}(mean ~ {model_fmla}{args})
                )"
            )
            cat(" * Model formula: ")
            cat(model_expr)
            cat("\n")
            model_object <<- eval(parse(text = model_expr))
            print(model_object)
        },
        fit_model = function() {
            blockHandlers(fit_model_btn)
            enabled(fit_model_btn) <<- FALSE
            svalue(fit_model_btn) <<- "Running simulations ..."

            exp_expr <- ifelse(is.na(secondaryvar), "",
                "exposure = altarray,"
            )
            exp <- glue::glue("demest::estimateModel(
                model = model_object,
                y = demarray,
                {exp_expr}
                filename = '{model_file}',
                nBurnin = {fit_nburn},
                nSim = {fit_niter},
                nChain = {fit_nchain},
                nThin = {fit_nthin},
                nCore = {fit_ncore}
            )")

            eval(parse(text = exp))

            svalue(fit_model_btn) <<- "Fit Model"
            enabled(fit_model_btn) <<- TRUE
            unblockHandlers(fit_model_btn)

            print(demest::fetchSummary(model_file))
            model_exists <<- TRUE
            updatePlot()
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
        },
        addSecondaryVarObserver = function(FUN, ...) {
            .self$secondaryvarChanged$connect(FUN, ...)
        },
        addResponseConfirmedObserver = function(FUN, ...) {
            .self$response_confirmedChanged$connect(FUN, ...)
        },
        addVarTableObserver = function(FUN, ...) {
            .self$var_tableChanged$connect(FUN, ...)
        },
        addVariablesConfirmedObserver = function(FUN, ...) {
            .self$variables_confirmedChanged$connect(FUN, ...)
        },
        addModelConfirmedObserver = function(FUN, ...) {
            .self$model_confirmedChanged$connect(FUN, ...)
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
