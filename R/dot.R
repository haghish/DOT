#' @title Render and Export DOT Plots in R
#' @usage dot(DOT, file = NULL, return = NULL)
#' @description DOT (graph description language) is a simplified and intuitive plain text graphical language. This package not only renders the DOT markup language in R, but also provides the possibility to export the graphs in PostScript and SVG (Scalable Vector Graphics) formats. The DOT package also supports literate programming packages such as Knitr and R2HTML. Visit \url{http://haghish.com/dot} for downloading examples of creating algorithms and several graphs for \code{Rmarkdown} and \code{R HTML} to generate dynamic procedural graphs in dynamic documents using the \code{DOT} package.
#'
#' @param DOT DOT (graph description language) is a simplified and intuitive plain text graphical language.This argument takes a string containing the DOT script. It is advised to use single quotation mark for the DOT string since the script often includes double quotations which can crash the function.
#'
#' @param file defines the file name for exporting the graph. The acceptable file extensions are \code{"ps"} for PostScript and \code{"svg"} for SVG format (see examples below).
#'
#' @param return specifies if PS or SVG script should be printed in R console. The acceptable values are \code{"verbatim"} and \code{"cat"}. \code{"verbatim"} returns a single string which is assignable to an object. \code{"cat"} returns a concatenated string which is printed in multiple lines in the R console. Use the \code{"cat"} to append SVG graphics directly in dynamic document packages such as \code{knitr, Rmarkdown}, etc.
#'
#' @return By default, the function only renders and loads the DOT plot in RStudio but does not return any PS or SVG script. If the \code{return} argument is specified, it returns PostScript or SVG script. Note that for assigning the script returned from \code{dot()} to an object, only \code{"verbatim"} value can be used to create a string object.
#'
#' @author E. F. Haghish \cr
#' Medical Informatics and Biostatistics (IMBI) \cr
#' University of Freiburg, Germany \cr
#' \email{haghish@imbi.uni-freiburg.de} \cr
#' \cr
#' Department of Mathematics and Computer Science \cr
#' University of Southern Denmark \cr
#' \email{haghish@imada.sdu.dk}
#'
#' @examples
#' #create a simple DOT graph and load it in RStudio
#' dot("digraph {A -> B;}")
#'
#' #create a DOT graph and export a SVG file to the working directory
#' dot("digraph {A -> B; B -> C; B -> D;}", file = "myfile.svg")
#'
#' #export the example above in PostScript format
#' dot("digraph {A -> B; B -> C; B -> D;}", file = "myfile.ps")
#'
#' #create a DOT graph and save the script in a string object in R
#' myString <- dot("digraph {A -> B;}", return = "verbatim")
#'
#' #while working in Rmarkdown or RHTML to produce a dynamic document use:
#' dot("digraph {A -> B;}", return = "cat")
#'
#' @export
#' @import V8
#' @importFrom tools file_ext

dot <- function(DOT, file = NULL, return = NULL) {

    # ---------------------------------------------------------
    # SYNTAX PROCESSING
    #   - create temporary directory
    #   - obtain the full path to the JS library in the package
    #   - check the file extension
    #   - load V8 if there is a demand for exporting graphics
    # =========================================================

    if(!requireNamespace("tools")) stop("tools package is required.", call. = FALSE)
    requireNamespace("tools", quietly = TRUE)

    extension <- ""
    if (!is.null(file)) {
        extension <- file_ext(file)
    }

    if (!is.null(file) & extension != "svg" & extension != "ps") {
        err <- paste("\n", extension, "is not a valid file extension.",
            "DOT recognizes 'svg' and 'ps' file extensions only" , sep = " ")
        stop(err)
    }

    if (!is.null(file)) {
        if(!requireNamespace("V8")) stop("V8 package is required", call. = FALSE)
        requireNamespace("V8", quietly = TRUE)
        if (!(packageVersion("V8") >= "1.0")) {
            stop('your "V8" package is too old. The DOT package requires V8
            package version 1.0 or newer', call. = FALSE)
        }
    }


    # ---------------------------------------------------------
    # DOT PROCESSING
    #   - remove the "\n" from DOT source
    #   - detecting DOT engine (digraph vs graph)
    # =========================================================
    scriptDOT <- gsub("[\r\n]", " ", DOT)
    List <- strsplit(DOT, " ")[[1]]
    engine <- List[[1]]
    if (engine == "graph") {
        DOT <- paste("'", scriptDOT, "'", ', { engine: "neato" }', sep="")
    }
    else {
        DOT <- paste("'", scriptDOT, "'", sep="")
    }

    # ---------------------------------------------------------
    # CREATING THE GRAPH
    #   - obtain the full path to the JS library in the package
    #   - create temporary directory and temporary SVG file
    #   - obtain the full path to the JS library in the package
    #   - create the dot.svg file
    #   - load in the viewer or web-browser
    # =========================================================


    tempDir <- tempfile()
    dir.create(tempDir)
    dotFile <- file.path(tempDir, "dot.svg")
    file.create(dotFile)
    JS <- system.file("lib/viz.js", package = "DOT")
    ENV <- v8();
    ENV$source(JS)
    call <- paste("Viz(", DOT, ")", sep = "")
    content <- ENV$eval(call)
    write(content, file=dotFile, append=TRUE)

    viewer <- getOption("viewer")
    if (!is.null(viewer)) {
        viewer(dotFile)
    }
    #else {
    #    utils::browseURL(dotFile)
    #}


    # ---------------------------------------------------------
    # EXPORTING THE GRAPH
    # =========================================================
    if (!is.null(file)) {
        if (extension == "ps") {
            scriptDOT <- paste("'", scriptDOT, "'", ' , { format: "ps" }', sep = "")
            if (engine == "graph") {
                DOT <- paste(scriptDOT, ' , { engine: "neato" }', sep="")
            }
            else {
                DOT <- paste(scriptDOT, sep="")
            }
            call <- paste("Viz(", DOT, ")", sep = "")
            content <- ENV$eval(call)
        }

        export <- file.path(getwd(), file)
        file.create(export)
        write(content, file=export, append=TRUE)
    }

    if (!is.null(return)) {
        if (return == "cat") {
            cat(content)
        }
        else if (return == "verbatim") {
            return(content)
        }
    }
}






