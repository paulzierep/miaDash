test_that("staged paths resolve only for existing files", {

    vars <- c("MIADASH_ASSAY", "MIADASH_COLDATA", "MIADASH_ROWDATA",
        "MIADASH_ROWTREE", "MIADASH_TAXA_FROM_ROWNAMES")
    on.exit(Sys.unsetenv(vars))
    Sys.unsetenv(vars)

    paths <- galaxy_staged_paths()
    expect_true(all(vapply(paths, is.null, logical(1L))))
    expect_false(galaxy_staged_active())

    # An env var pointing at nothing is the same as one that is not set.
    Sys.setenv(MIADASH_ASSAY = file.path(tempdir(), "missing.tsv"))
    expect_null(galaxy_staged_paths()$assay)
    expect_false(galaxy_staged_active())

    assay <- tempfile(fileext = ".tsv")
    writeLines(c("taxon\ts1\ts2", "t1\t1\t2", "t2\t3\t4"), assay)
    Sys.setenv(MIADASH_ASSAY = assay)
    expect_identical(galaxy_staged_paths()$assay, assay)
    expect_true(galaxy_staged_active())

    expect_false(galaxy_taxa_from_rownames())
    Sys.setenv(MIADASH_TAXA_FROM_ROWNAMES = "True")
    expect_true(galaxy_taxa_from_rownames())
})

test_that("staged raw tables build a TreeSE", {

    vars <- c("MIADASH_ASSAY", "MIADASH_COLDATA", "MIADASH_ROWDATA",
        "MIADASH_ROWTREE", "MIADASH_TAXA_FROM_ROWNAMES")
    on.exit(Sys.unsetenv(vars))
    Sys.unsetenv(vars)

    assay <- tempfile(fileext = ".tsv")
    coldata <- tempfile(fileext = ".tsv")
    writeLines(c(
        "taxon\ts1\ts2\ts3",
        "t1\t1\t2\t3",
        "t2\t4\t5\t6",
        "t3\t7\t8\t9"), assay)
    writeLines(c(
        "sample\tGroup",
        "s1\ta",
        "s2\tb",
        "s3\ta"), coldata)

    rObjects <- new.env()
    pObjects <- new.env()

    # Nothing staged: no-op, and nothing is written into rObjects.
    expect_false(.galaxy_staged_import(rObjects, pObjects))
    expect_null(rObjects$tse)

    Sys.setenv(MIADASH_ASSAY = assay)
    expect_true(.galaxy_staged_import(rObjects, pObjects))
    expect_s4_class(rObjects$tse, "TreeSummarizedExperiment")
    expect_identical(dim(rObjects$tse), c(3L, 3L))
    # Without colData, the constructor derives one from the assay columns.
    expect_identical(colnames(rObjects$tse), c("s1", "s2", "s3"))

    # The tracked-code script gets the setup plus the staged import.
    expect_true(length(pObjects$commands) >= 2L)
    expect_true(any(grepl("staged by the Galaxy tool", pObjects$commands)))

    Sys.setenv(MIADASH_COLDATA = coldata)
    expect_true(.galaxy_staged_import(rObjects, pObjects))
    expect_identical(as.character(SummarizedExperiment::colData(
        rObjects$tse)$Group), c("a", "b", "a"))
})

test_that("a taxonomy tree and rowData are picked up when staged", {

    vars <- c("MIADASH_ASSAY", "MIADASH_ROWDATA", "MIADASH_ROWTREE",
        "MIADASH_TAXA_FROM_ROWNAMES")
    on.exit(Sys.unsetenv(vars))
    Sys.unsetenv(vars)

    assay <- tempfile(fileext = ".tsv")
    rowdata <- tempfile(fileext = ".tsv")
    tree <- tempfile(fileext = ".tree")
    writeLines(c(
        "taxon\ts1\ts2",
        "k__Bacteria;p__Firmicutes\t1\t2",
        "k__Bacteria;p__Proteobacteria\t3\t4"), assay)
    writeLines(c(
        "taxon\tabundance",
        "k__Bacteria;p__Firmicutes\t10",
        "k__Bacteria;p__Proteobacteria\t20"), rowdata)
    writeLines("(k__Bacteria;p__Firmicutes:0.1,k__Bacteria;p__Proteobacteria:0.1);",
        tree)

    Sys.setenv(
        MIADASH_ASSAY = assay,
        MIADASH_ROWDATA = rowdata,
        MIADASH_ROWTREE = tree,
        MIADASH_TAXA_FROM_ROWNAMES = "False")

    rObjects <- new.env()
    pObjects <- new.env()
    expect_true(.galaxy_staged_import(rObjects, pObjects))
    expect_identical(dim(rObjects$tse), c(2L, 2L))
    expect_identical(as.character(SummarizedExperiment::rowData(
        rObjects$tse)$abundance), c("10", "20"))
    expect_identical(
        TreeSummarizedExperiment::rowTreeNames(rObjects$tse), "phylo")

    # The switch reuses the Raw Data tab's rownames-to-taxa conversion.
    Sys.setenv(MIADASH_TAXA_FROM_ROWNAMES = "True")
    expect_true(.galaxy_staged_import(rObjects, pObjects))
    expect_true("Phylum" %in% names(SummarizedExperiment::rowData(
        rObjects$tse)))
})

test_that("an unreadable staged table is reported, not swallowed", {

    vars <- c("MIADASH_ASSAY", "MIADASH_COLDATA")
    on.exit(Sys.unsetenv(vars))
    Sys.unsetenv(vars)

    assay <- tempfile(fileext = ".tsv")
    coldata <- tempfile(fileext = ".tsv")
    # The colData sample ids do not match the assay columns, so the TreeSE
    # constructor has to refuse.
    writeLines(c("taxon\ts1\ts2", "t1\t1\t2", "t2\t3\t4"), assay)
    writeLines(c("sample\tGroup", "x1\ta", "x2\tb"), coldata)

    rObjects <- new.env()
    pObjects <- new.env()
    Sys.setenv(MIADASH_ASSAY = assay, MIADASH_COLDATA = coldata)

    expect_error(
        expect_false(.galaxy_staged_import(rObjects, pObjects)),
        NA)
    expect_null(rObjects$tse)
})

test_that("the Galaxy send wiring is present and stays inert outside Galaxy", {

    js <- galaxy_ie_send_js()
    expect_match(js, "miadash_galaxy_send", fixed = TRUE)
    expect_match(js, "shiny-download-link", fixed = TRUE)
    # Downloads are rendered when the export modal is opened, so the injection
    # has to keep watching.
    expect_match(js, "MutationObserver", fixed = TRUE)
    expect_false(grepl("%1$s", js, fixed = TRUE))
    expect_false(grepl("%2$s", js, fixed = TRUE))

    # An allow-list means iSEE's own internal download links get no button.
    allowed <- galaxy_ie_send_js(ids = "download")
    expect_match(allowed, '["download"]', fixed = TRUE)
    expect_match(allowed, "isAllowed", fixed = TRUE)

    # No history id or API key: nothing that cannot work is offered.
    vars <- c("HISTORY_ID", "API_KEY", "GALAXY_IE_PUT")
    on.exit(Sys.unsetenv(vars))
    Sys.unsetenv(vars)
    expect_false(galaxy_ie_ready())
    expect_null(galaxy_ie_send_ui(ids = "download"))

    put <- tempfile()
    writeLines("#!/bin/sh\nexit 0", put)
    Sys.chmod(put, "0755")
    Sys.setenv(HISTORY_ID = "h1", API_KEY = "k1", GALAXY_IE_PUT = put)
    expect_true(galaxy_ie_ready())
    expect_s3_class(galaxy_ie_send_ui(ids = "download"), "shiny.tag")

    # With nothing registered there is no button to attach.
    expect_null(galaxy_ie_send_ui(ids = character(0)))
})

test_that("registered downloads are reproduced by the registry", {

    registry <- galaxy_ie_registry()
    handler <- galaxy_ie_download_with(
        registry, "download",
        filename = function() "treese-test.rds",
        content = function(file) saveRDS("object", file))

    expect_s3_class(handler, "shiny.render.function")
    expect_identical(ls(registry), "download")

    path <- tempfile(fileext = ".rds")
    registry$download$content(path)
    expect_identical(readRDS(path), "object")
    expect_identical(
        galaxy_ie_download_name(registry$download, "download"),
        "treese-test.rds")

    # A name without an extension gains one, because Galaxy names the dataset
    # after it.
    expect_identical(
        galaxy_ie_download_name(list(filename = function() "plot"), "plot"),
        "plot.txt")
})
