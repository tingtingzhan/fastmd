
#' @title \link[stats]{anova}
#' 
#' @examples
#' list(
#'  '`anova`' = lm(dist ~ speed, data = cars) |> anova()
#' ) |> render2html()
#' 
#' @name anova
NULL



#' @export
as_flextable.anova <- function(x, row.title = ' ', ...) {
  
  # ?stats:::print.anova, then ?stats::printCoefmat
  
  x0 <- as.data.frame(x)
  x0[['Pr(>F)']] <- x[['Pr(>F)']] |> 
    label_pvalue_sym()()
  
  x1 <- data.frame(
    ' ' = x |> row.names.data.frame() |> trimws(), 
    x0, 
    row.names = NULL, check.names = FALSE, 
    fix.empty.names = FALSE, stringsAsFactors = FALSE)
  
  names(x1)[1L] <- row.title
  
  x1 |>
    flextable() |>
    autofit(part = 'all') |>
    colformat_double(j = 3:5, digits = 3L)
  
}

#' @export
as_flextable.aov <- function(x, ...) {
  x |> 
    summary.aov() |> 
    as_flextable.summary.aov(...)
}


#' @export
as_flextable.summary.aov <- function(x, ...) {
  if (length(x) != 1L) stop('deal with this')
  if (!inherits(x[[1L]], what = 'anova')) stop('deal with this')
  (x[[1L]]) |> 
    as_flextable.anova(...)
}


#' @export
as_flextable.aovlist <- function(x, ...) {
  x |> 
    summary() |> # ?stats:::summary.aovlist
    as_flextable.summary.aovlist(...)
}



#' @importFrom patchwork plot_layout
#' @export
as_flextable.summary.aovlist <- function(x, ...) {
  id <- x |>
    vapply(FUN = inherits, what = 'summary.aov', FUN.VALUE = NA)
  if (!all(id)) stop('deal with this')
  z <- .mapply(
    FUN = as_flextable.summary.aov, 
    dots = list(
      x = x,
      row.title = names(x)
    ), 
    MoreArgs = list(...)
  ) |>
    lapply(FUN = wrap_flextable) |>
    Reduce(f = `+`) # fancy `+` in \CRANpkg{patchwork}
  z + plot_layout(ncol = 1L)
}




