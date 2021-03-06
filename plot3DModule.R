#' @name 3D Plotting
#' @version 0.0.1
#' @author Tom Elliott
#' @desc Explore three variables in a three-dimensional plot.
Plot3DModule <- setRefClass(
    "3D Scatter Plot",
    contains = "CustomModule",
    fields = list(
        GUI = "ANY",
        # The User Interface (UI) should modify these "fields",
        # which can be used by other components of the module
        # (for example, plotting)
        data = "ANY",
        var_x = "character",
        var_y = "character",
        var_z = "character",
        var_group = "character",
        phi = "numeric",
        theta = "numeric"
    ),
    methods = list(
        initialize = function(gui, name) {
            callSuper(gui,
                name = name,
                embedded = TRUE
            )
            initFields(
                data = get_data(),
                var_x = "",
                var_y = "",
                var_z = "",
                var_group = "",
                phi = 40, theta = 40
            )

            ## something about installing packages
            install_dependencies(
                c("plot3D"),
                optional = c("plot3Drgl")
            )

            ## The main code for your module goes here,
            ## inside a top-level container called "mainGrp"
            mainGrp$set_borderwidth(5)

            ## --- variable list


            ## --- chosen variable boxes
            tbl <- glayout()
            ii <- 1

            numvars <- names(data)[sapply(data, iNZightTools::is_num)]
            catvars <- names(data)[sapply(data, iNZightTools::is_cat)]

            lbl <- glabel("x variable :")
            xvarDrop <- gcombobox(
                c("Select X Variable", numvars),
                selected = 1,
                handler = function(h, ...) {
                    if (svalue(h$obj, index = TRUE) == 1) var_x <<- ""
                    else var_x <<- svalue(h$obj)
                    updatePlot()
                }
            )
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:3, expand = TRUE] <- xvarDrop
            ii <- ii + 1

            lbl <- glabel("y variable :")
            yvarDrop <- gcombobox(
                c("Select Y Variable", numvars),
                selected = 1,
                handler = function(h, ...) {
                    if (svalue(h$obj, index = TRUE) == 1) var_y <<- ""
                    else var_y <<- svalue(h$obj)
                    updatePlot()
                }
            )
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:3, expand = TRUE] <- yvarDrop
            ii <- ii + 1

            lbl <- glabel("z variable :")
            zvarDrop <- gcombobox(
                c("Select Z Variable", numvars),
                selected = 1,
                handler = function(h, ...) {
                    if (svalue(h$obj, index = TRUE) == 1) var_z <<- ""
                    else var_z <<- svalue(h$obj)
                    updatePlot()
                }
            )
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:3, expand = TRUE] <- zvarDrop
            ii <- ii + 1

            lbl <- glabel("Colour by :")
            colvarDrop <- gcombobox(c("", catvars),
                selected = 1,
                handler = function(h, ...) {
                    if (svalue(h$obj, index = TRUE) == 1) var_group <<- ""
                    else var_group <<- svalue(h$obj)
                    updatePlot()
                }
            )
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:3, expand = TRUE] <- colvarDrop
            ii <- ii + 1

            ## viewing angle
            lbl <- glabel("Rotate vertical :")
            phiSld <- gslider(-90, 90, by = 1, value = phi,
                handler = function(h, ...) {
                    phi <<- svalue(h$obj)
                    updatePlot()
                }
            )
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:3, expand = TRUE] <- phiSld
            ii <- ii + 1

            lbl <- glabel("Rotate horizontal :")
            thetaSld <- gslider(-90, 90, by = 1, value = theta,
                handler = function(h, ...) {
                    theta <<- svalue(h$obj)
                    updatePlot()
                }
            )
            tbl[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            tbl[ii, 2:3, expand = TRUE] <- thetaSld
            ii <- ii + 1


            ## Render in 3D (using rgl)
            if (requireNamespace('plot3Drgl', quietly = TRUE)) {
                btn <- gbutton("Open interactive plot",
                    container = mainGrp,
                    handler = function(h, ...) {
                        updatePlot(interactive = TRUE)
                    }
                )
                tbl[ii, 1:2, expand=T] <- btn
                ii <- ii + 1
            }
            add(mainGrp, tbl)

            ## The UI provides a way for users to change values of "fields"

            ## if you can do it without any user input, draw a basic plot
            updatePlot()
        },
        ## add new methods to simplify your code
        updatePlot = function(interactive = FALSE) {
            # This function is linked to the "refresh plot" button.
            # It should generate a plot purely from the values
            # of variables stored in "fields".
            # The UI then alters the values of the fields and calls this function.

            x <- if (var_x == "") rep(0, nrow(data)) else data[[var_x]]
            y <- if (var_y == "") rep(0, nrow(data)) else data[[var_y]]
            z <- if (var_z == "") rep(0, nrow(data)) else data[[var_z]]

            if (var_group == "") {
                colvar <- NULL
                col <- "black"
                colkey <- NULL
                pcol <- NULL
            } else {
                colvar <- as.integer(data[[var_group]])
                palettes <- iNZightPlots::cat_palette_names()
                col <-  iNZightPlots::inzpalette(names(palettes)[1])(length(unique(colvar)))
                colkey <- list(plot = FALSE)
                pcol <- col[colvar]
            }

            if (!interactive) {
                dev.hold()
                plot3D::scatter3D(x, y, z,
                    colvar = colvar, col = col, colkey = FALSE,
                    theta = theta, phi = phi,
                    xlab = var_x, ylab = var_y, zlab = var_z,
                    bty = "b2", pch = 19
                )
                if (var_group != "") {
                    legend("right",
                        levels(data[[var_group]]),
                        col = col,
                        pch = 19,
                        bty = "n",
                        title = var_group
                    )
                }
                dev.flush()
                return()
            }

            ## otherwise, rgl!
            rgl::plot3d(x, y, z,
                col = pcol,
                xlab = var_x, ylab = var_y, zlab = var_z,
                type = "s", size = 1
            )
        },
        close = function() {
            cat("Closing module\n")
            try(rgl::rgl.close(), silent = TRUE)
            callSuper()
        }
    ),
    ## This is currently required to get around namespace locking
    where = e
)
