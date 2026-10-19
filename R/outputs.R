#' Outputs
#'
#' \code{.render_overview} and \code{.render_download} create the output of the
#'   UI, which consists of the overview of the TreeSE object and the download
#'   object, respectively.
#'
#' @param output The Shiny output object from the server function.
#'
#' @return Adds a rendered item to \code{output}.
#'   A \code{NULL} value is invisibly returned.
#'
#' @name render_output
#' @keywords internal

#' @rdname render_output
.render_overview <- function(output, rObjects) {
  
    # nocov start
    output$object <- renderPrint({
        rObjects$tse
    })
    # nocov end

    invisible(NULL)
}

#' @rdname render_output
#' @importFrom mia convertToPhyloseq
.render_download <- function(input, output, rObjects, registry = NULL){
    # nocov start
    filename <- function(){
        suffix <- switch(input$export_format, TreeSE = "rds",
            phyloseq = "rds", BIOM = "biom", Mothur = "zip", QIIME2 = "zip")
        
        paste0(tolower(input$export_format), "-", Sys.Date(), ".", suffix)
    }
    content <- function(file) switch(
        input$export_format,
        TreeSE = saveRDS(rObjects$tse, file),
        phyloseq = {
            pseq <- convertToPhyloseq(rObjects$tse, assay.type = input$export.assay)
            saveRDS(pseq, file)
        },
        .write_foreign(file, rObjects$tse, input$export.assay, input$export_format)
    )
    
    if (is.null(registry)) {
        output$download <- downloadHandler(
            filename = filename, content = content)
    } else {
        # Same handler, additionally registered with the Galaxy download
        # registry, so the "Send to Galaxy" button next to the download button
        # reproduces the file without a second implementation of any export
        # format. See R/galaxy_ie.R.
        output$download <- galaxy_ie_download_with(
            registry, "download", filename = filename, content = content)
    }
    # nocov end
    invisible(NULL)
}

#' @importFrom utils zip
#' @importFrom SummarizedExperiment assay assay<-
#' @importFrom rbiom as_rbiom write_biom write_mothur write_qiime2
.write_foreign <- function(file, tse, assay.type, as){
    
    assay(tse) <- assay(tse, assay.type)
    biom <- as_rbiom(tse)
    
    if( as == "BIOM" ){
        write_biom(biom, file)
        return(invisible(NULL))
    }
    
    temp_dir <- tempfile()
    
    FUN <- switch(as, Mothur = write_mothur, QIIME2 = write_qiime2)
    FUN(biom, temp_dir)
    
    zip(file, files = list.files(temp_dir, full.names = TRUE), flags = "-r9Xj")
    invisible(NULL)
}
