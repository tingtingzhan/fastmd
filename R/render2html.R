

#' @title Create and Render R Markdown file
#' 
#' @description ..
#' 
#' @param x an R object
#' 
#' @param path \link[base]{character} scalar 
#' 
#' @param file \link[base]{character} scalar
#' 
#' @param template,package template (from which package) to use; see function \link[rmarkdown]{draft}
#' 
#' @param rmd.rm \link[base]{logical} scalar, whether to remove the R markdown `'.rmd'` file,
#' default `TRUE`
#' 
#' @param bib.rm \link[base]{logical} scalar, whether to remove the bibliography `'.bib'` file,
#' default `TRUE`
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom rmarkdown draft render
#' @export
render2html <- function(
    x, 
    path = tempdir(),
    file = stop('must specify `file` explicitly'),
    template = 'txz003', package = 'fastmd', 
    rmd.rm = TRUE,
    bib.rm = TRUE,
    ...
) {
  
  x <- x[lengths(x) > 0L]
  if (!(nx <- length(x))) return(invisible())
  
  path <- file.path(path, 'html')
  dir.create(path = path, showWarnings = FALSE, recursive = TRUE)
  # path |> sprintf(fmt = 'open \'%s\'') |> system()

  if (length(file) != 1L || !is.character(file) || is.na(file)) stop('`file` must be len-1 character')
  if (grepl(pattern = '\\:', x = file)) stop('`file` must not contain ', sQuote(':'))
  
  foo_date <- \(path, file, fileext) {
    f <- Sys.Date() |>
      format.Date() |>
      sprintf(fmt = '%s %s.%s', . = _, file, fileext) |>
      file.path(path, . = _)
    if (file.exists(f)) {
      file.remove(f)
      basename(f) |> 
        col_cyan() |> style_bold() |>
        message('Existing ', . = _, ' removed')
    }
    return(f)
  }
  
  frmd <- foo_date(path = path, file = file, fileext = 'rmd')
  fhtml <- foo_date(path = path, file = file, fileext = 'html')
  
  z <- x |>
    md_.list(xnm = 'x', nm_level = '#')
  
  fbib <- file.path(path, 'bibliography.bib')
  z@bibentry |>
    sink2.bibentry(file = fbib)
  
  draft(file = frmd, template = template, package = package, edit = FALSE)
  z |>
    sink2.md_lines(file = frmd, append = TRUE)
  
  render(input = frmd, output_file = fhtml, intermediates_dir = path, quiet = TRUE)
  
  fhtml |> 
    normalizePath() |> 
    sprintf(fmt = 'open \'%s\'') |> 
    system()
  
  if (rmd.rm) file.remove(frmd) else {
    frmd |>
      normalizePath() |>
      sprintf(fmt = 'open \'%s\'') |> 
      system()
  }
  
  if (length(fbib)) { # surely will have a bib entry for R !
    if (bib.rm) file.remove(fbib) else {
      fbib |>
        normalizePath() |>
        sprintf(fmt = 'open \'%s\'') |> 
        lapply(FUN = system) # in case tzh creates *multiple* .bib files in future!!
    }
  }
  
  # **must** remove .css file!!
  file.path(path, 'styles.css') |>
    file.remove()
  
  return(invisible())
  
}

# template 'txz003'
# CSS Rule from
# https://stackoverflow.com/questions/34906002








#' @title Which Package Created This Object?
#' 
#' @param x an R object
#' 
#' @returns 
#' Function [fromPackage()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @export
fromPackage <- function(x) {
  
  if (isS4(x)) {
    return(x |> 
             class() |> 
             attr(which = 'package', exact = TRUE))
  } 
  
  f <- getCall(x)[[1L]]
  pkg <- tryCatch(expr = {
    f |>
      eval() |> # function call could be un-exported, e.g., nlme:::lme.formula, and err
      environment() |>
      getNamespaceName()
  }, error = \(e) {
    aw <- f |>
      as.character() |>
      getAnywhere()
    if (length(aw$where) > 1L) stop('really shouldnt happen...')
    (aw$where) |>
      gsub(pattern = '^namespace\\:', replacement = '')
  })
  
  pkg |>
    # unname() |> # ?base::setdiff removes names anyway
    setdiff(y = installed.packages(priority = 'base') |> rownames())
  
}


#' @title Text for Package to Create an R Object
#' 
#' @param x \link[base]{character} scalar, the returned object of function [fromPackage()]
#' 
#' @returns 
#' Function [pkg_text()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @export
pkg_text <- function(x) {
  
  if (!length(x)) return('<u>**`R`**</u>')
  
  x |> 
    sprintf(fmt = '<u>**`R`**</u> package <u>**`%s`**</u>')
  
}







