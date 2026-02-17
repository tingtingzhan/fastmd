

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
  
  if (length(file) != 1L || !is.character(file) || is.na(file) ||
      grepl(pattern = '\\:', x = file)) stop('`file` must be len-1 character, without ', sQuote(':'))
  
  frmd <- file.path(path, sprintf(fmt = '%s %s.rmd', format.Date(Sys.Date()), file))
  if (file.exists(frmd)) {
    file.remove(frmd)
    message('Existing ', sQuote(basename(frmd)), ' removed')
  }
  
  fout <- file.path(path, sprintf(fmt = '%s %s.%s', format.Date(Sys.Date()), file, 'html'))
  if (file.exists(fout)) {
    file.remove(fout)
    message('Existing ', sQuote(basename(fout)), ' removed')
  }

  nm <- names(x)
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('names must be complete')
  
  # **not** [md_.list()]; as we need section titles!!!
  md <- nx |> 
    seq_len() |>
    lapply(FUN = \(i) {
      c.md_lines(
        nm[i] |> 
          sprintf(fmt = '\n# %s\n') |> 
          new(Class = 'md_lines'), # must use an extra '\n' to separate from previous 'character'
        md_(x = x[[i]], xnm = sprintf(fmt = 'x[[%d]]', i))
      )
    }) |> 
    do.call(what = c.md_lines, args = _)
  # end of **not** [md_.list()]
  
  bib_file <- file.path(path, 'bibliography.bib')
  md@bibentry |>
    sink2.bibentry(file = bib_file)
  
  draft(file = frmd, template = template, package = package, edit = FALSE)
  sink(file = frmd, append = TRUE) # ?base::writeLines cannot append
  md |>
    print.md_lines()
  sink()
  
  render(input = frmd, output_file = fout, intermediates_dir = path, quiet = TRUE)
  
  fout |> 
    normalizePath() |> 
    sprintf(fmt = 'open \'%s\'') |> 
    system()
  
  if (rmd.rm) file.remove(frmd) else {
    frmd |>
      normalizePath() |>
      sprintf(fmt = 'open \'%s\'') |> 
      system()
  }
  
  if (length(bib_file)) { # surely will have a bib entry for R !
    if (bib.rm) file.remove(bib_file) else {
      bib_file |>
        normalizePath() |>
        sprintf(fmt = 'open \'%s\'') |> 
        lapply(FUN = system)
    }
  }
  
  return(invisible(fout))
  
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







