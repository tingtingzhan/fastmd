

# https://yihui.org/rmarkdown-cookbook/package-template
# usethis::use_rmarkdown_template('abc')

# template 'report'
# CSS Rule from
# https://stackoverflow.com/questions/34906002


#' @title Analyses in HTML Report 
#' 
#' @description
#' Draft and render one or more analyses in R into 
#' an HyperText Markup Language (HTML) file.
#' 
#' @param x a \link[base]{list}, 
#' containing one or more analyses results
#' 
#' @param path \link[base]{character} scalar,
#' the parent directory 
#' (default value is the \link[base]{tempdir}).
#' The `.rmd` and `.html` files will be created in the sub-directory
#' `[path]/html/`.
#' 
#' @param file \link[base]{character} scalar, 
#' the output HTML file name (without the file extension `.html`).
#' Default value is the \link[base]{basename} of a \link[base]{tempfile}
#' 
#' @param template,package template (from which package) to use; 
#' see function \link[rmarkdown]{draft} for details, 
#' default value is the template `'report'` in this package
#' 
#' @param author (an R object convertible to) a \link[utils]{person} object,
#' default value is the author of this package
#' 
#' @param trace \link[base]{logical} scalar.
#' If `FALSE` (default value), the `.rmd` and `.bib` files will be removed.
#' If `TRUE`, these files will be opened for inspection on a Unix-based operating system.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details
#' Function [render2html()] 
#' first \link[rmarkdown]{draft}s an `.rmd` file from a template,
#' then \link[rmarkdown]{render}s it into a single `.html` file.
#' 
#' @returns 
#' Function [render2html()] \link[base]{invisible}y returns
#' the full path of the output `.html` file.
#' 
#' @keywords internal
#' @importFrom rmarkdown draft render
#' @export
render2html <- function(
    x, 
    path = tempdir(),
    file = tempfile() |> basename(),
    template = 'report', package = 'fastmd', 
    author = person(given = 'Tingting', family = 'Zhan', email = 'tingting.zhan@jefferson.edu'),
    trace = FALSE,
    ...
) {
  
  x <- x[lengths(x) > 0L]
  if (!(nx <- length(x))) return(invisible())
  
  path <- file.path(path, 'html')
  dir.create(path = path, showWarnings = FALSE, recursive = TRUE)
  # path |> sprintf(fmt = 'open \'%s\'') |> system()

  if (length(file) != 1L || !is.character(file) || is.na(file)) stop('`file` must be len-1 character')
  if (grepl(pattern = '\\:', x = file)) stop('`file` must not contain ', sQuote(':'))
  
  file_date <- \(path, file, fileext) {
    f <- Sys.Date() |>
      format.Date() |>
      sprintf(fmt = '%s %s.%s', . = _, file, fileext) |>
      file.path(path, . = _)
    rm_file_exist(f)
    return(f)
  }
  dir_date <- \(path, file) {
    d <- Sys.Date() |>
      format.Date() |>
      sprintf(fmt = '%s %s', . = _, file) |>
      file.path(path, . = _)
    rm_dir_exist(d)
    return(d)
  }
  rm_file_exist <- \(file) {
    if (file.exists(file)) {
      file.remove(file)
      file |>
        basename() |> 
        col_cyan() |> style_bold() |>
        message('Existing ', . = _, ' removed')
    }
  }
  rm_dir_exist <- \(path) {
    if (dir.exists(paths = path)) {
      path |> 
        unlink(recursive = TRUE, force = TRUE)
      path |>
        basename() |> 
        col_magenta() |> style_bold() |>
        message('Existing ', . = _, ' removed')
    }
  }
   
    
  frmd <- file_date(path = path, file = file, fileext = 'rmd')
  fhtml <- file_date(path = path, file = file, fileext = 'html')
  dir <- dir_date(path = path, file = file |> sprintf(fmt = '%s_files')) # only on `windows`
  fcss <- file.path(path, 'styles.css')
  rm_file_exist(fcss)
  fbib <- file.path(path, 'bibliography.bib')
  rm_file_exist(fbib)
  ftempl <- file.path(path, 'skeleton.rmd')
  rm_file_exist(ftempl)
  
  z <- x |>
    md_.list(xnm = 'x', nm_level = '#')
  
  z@bibentry |>
    sink2.bibentry(file = fbib) 
  # even if (!length(z@bibentry)),
  # because `bibliography.bib` is hard coded in template!!!
  
  title <- file
  author <- author |> as.person()
  draft(
    file = frmd, 
    template = template, package = package, 
    edit = FALSE
  )
  z |>
    sink2.md_lines(file = frmd, append = TRUE)
  
  render(
    input = frmd, 
    output_file = fhtml, 
    intermediates_dir = path, 
    quiet = TRUE
  )
  
  fhtml |> 
    browseURL() # both `windows` and `unix`
  
  fmt_open <- switch(
    EXPR = .Platform$OS.type, 
    unix = 'open \'%s\'', 
    windows = NULL #stop('tzh needs to learn..')
  )
  
  if (!trace) {
    file.remove(frmd, fbib) 
  } else if (.Platform$OS.type == 'unix') {
    frmd |>
      sprintf(fmt = fmt_open) |> 
      system()
    fbib |>
      sprintf(fmt = fmt_open) |> 
      lapply(FUN = system) # in case tzh creates *multiple* .bib files in future!!
  }
  
  # **must** remove .css file!!
  fcss |>
    file.remove()
  
  switch(
    EXPR = .Platform$OS.type, 
    windows = {
      dir |> 
        rm_dir_exist()
    })
  
  return(invisible(fhtml))
  
}


