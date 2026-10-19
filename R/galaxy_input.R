# Galaxy staged-input helpers for miaDash.
#
# The Galaxy tool wrapper (gxit/interactivetool_miadash.xml in
# usegalaxy-eu/microbiome-ITs) stages the raw tables the user attached to the
# tool form into the job directory and points these environment variables at
# them. When they are set, the landing page imports them at session start
# through the same constructor the "Raw Data" tab uses, so the session starts
# from the user's data instead of an empty dashboard.
#
# Everything degrades to a no-op when the variables are absent, so a plain
# `docker run` or `shiny::runApp()` behaves exactly as upstream.

# The staged file paths, as a list. An env var that is unset, empty or points
# at nothing becomes NULL: the tool XML only exports the variables for the
# datasets that were actually staged.
galaxy_staged_paths <- function() {
    paths <- list(
        assay = Sys.getenv("MIADASH_ASSAY", unset = ""),
        coldata = Sys.getenv("MIADASH_COLDATA", unset = ""),
        rowdata = Sys.getenv("MIADASH_ROWDATA", unset = ""),
        rowtree = Sys.getenv("MIADASH_ROWTREE", unset = "")
    )
    lapply(paths, function(path) {
        if (!nzchar(path) || !file.exists(path)) NULL else path
    })
}

# TRUE when an abundance table was staged. The other tables are optional.
galaxy_staged_active <- function() {
    !is.null(galaxy_staged_paths()$assay)
}

# The "derive rowData from the assay row names" switch, which is a tool form
# checkbox by the time it reaches here.
galaxy_taxa_from_rownames <- function() {
    tolower(Sys.getenv("MIADASH_TAXA_FROM_ROWNAMES", unset = "")) %in%
        c("1", "true", "yes")
}

# Notifications need a live Shiny session; the tests and any headless use of
# this file do not have one, so fall back to the console there.
.galaxy_notify <- function(message, type = "message", duration = 8) {
    domain <- shiny::getDefaultReactiveDomain()
    if (is.null(domain)) {
        message("[miadash] ", message)
        return(invisible(NULL))
    }
    shiny::showNotification(message, type = type, duration = duration)
    invisible(NULL)
}

# Imports the staged raw tables into rObjects$tse, mirroring the "raw" branch
# of .create_import_observers(). Returns TRUE when a TreeSE was built, and
# FALSE when nothing was staged or the staged tables could not be read - in
# which case rObjects$tse is left untouched and the app stays usable, so the
# user can still import through the UI.
.galaxy_staged_import <- function(rObjects, pObjects) {
    paths <- galaxy_staged_paths()
    if (is.null(paths$assay)) {
        return(invisible(FALSE))
    }

    if (is.null(pObjects$commands) || !length(pObjects$commands)) {
        pObjects$commands <- .setupCommand
    }

    assay_name <- sub("\\.tsv$", "", basename(paths$assay))

    tse <- tryCatch({
        assay_list <- list(
            as.matrix(utils::read.table(paths$assay, row.names = 1,
                header = TRUE, sep = "\t"))
        )
        names(assay_list) <- assay_name

        coldata <- .set_optarg(paths$coldata,
            alternative = S4Vectors::DataFrame(
                row.names = colnames(assay_list[[1]])),
            loader = utils::read.table, row.names = 1, header = TRUE, sep = "\t")

        rowdata <- .set_optarg(paths$rowdata,
            loader = utils::read.table, row.names = 1, header = TRUE, sep = "\t")

        rowtree <- .set_optarg(paths$rowtree, loader = ape::read.tree)

        fun_args <- list(assays = assay_list, colData = coldata,
            rowData = rowdata, rowTree = rowtree, colTree = NULL)

        tse <- do.call(
            TreeSummarizedExperiment::TreeSummarizedExperiment, fun_args)

        if (galaxy_taxa_from_rownames()) {
            tse <- .rownames2taxa(tse)
        }
        tse
    }, error = function(e) e)

    if (inherits(tse, "error")) {
        galaxy_ie_log(
            "staged input import failed path=", paths$assay,
            " error=", conditionMessage(tse)
        )
        .galaxy_notify(
            paste0("The Galaxy input tables could not be imported: ",
                conditionMessage(tse),
                " You can still import data through the Import panel."),
            type = "error", duration = 30)
        return(invisible(FALSE))
    }

    rObjects$tse <- tse
    # A readable entry for the tracked-code script, with the staged paths in
    # place of the file inputs a manual "Raw Data" import would record.
    pObjects$commands <- c(pObjects$commands, paste0(
        "# Import the raw tables staged by the Galaxy tool\n",
        "assay <- as.matrix(read.table(\"", paths$assay,
        "\", row.names = 1, header = TRUE, sep = \"\\t\"))\n",
        "tse <- TreeSummarizedExperiment::TreeSummarizedExperiment(\n",
        "    assays = list(`", assay_name, "` = assay),\n",
        "    colData = ",
        if (is.null(paths$coldata)) {
            paste0("S4Vectors::DataFrame(row.names = colnames(assay))")
        } else {
            paste0("read.table(\"", paths$coldata,
                "\", row.names = 1, header = TRUE, sep = \"\\t\")")
        }, ",\n",
        "    rowData = ",
        if (is.null(paths$rowdata)) "NULL" else {
            paste0("read.table(\"", paths$rowdata,
                "\", row.names = 1, header = TRUE, sep = \"\\t\")")
        }, ",\n",
        "    rowTree = ",
        if (is.null(paths$rowtree)) "NULL" else {
            paste0("ape::read.tree(\"", paths$rowtree, "\")")
        }, ")"))

    .galaxy_notify(paste0(
        "Loaded the Galaxy input: ", nrow(tse), " features x ",
        ncol(tse), " samples (", assay_name, ")."))
    invisible(TRUE)
}
