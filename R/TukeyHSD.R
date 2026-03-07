

#' @title \link[stats]{TukeyHSD}
#' 
#' @examples
#' m = aov(breaks ~ wool + tension, data = warpbreaks)
#' list(
#'   'aov' = m,
#'   '`TukeyHSD`' = m |> TukeyHSD(which = 'tension', ordered = TRUE)
#' ) |> render2html()
#' 
#' @name TukeyHSD
NULL



#' @importFrom scales label_percent
#' @export
as_flextable.TukeyHSD <- function(x, ...) {
  
  x0 <- unclass(x)
  
  conf.level <- attr(x, which = 'conf.level', exact = TRUE)
  
  edp <- attr(x, which = 'orig.call', exact = TRUE)$formula[[2L]]
  
  ret0 <- .mapply(FUN = \(x, nm) {
    d <- data.frame(
      sprintf(fmt = '%s \u2e22%s\u2e25', nm, rownames(x)),
      sprintf(fmt = '%.2f (%.2f, %.2f)', x[,1L], x[,2L], x[,3L]),
      'Signif.' = x[,4L] |> label_pvalue_sym()()
    )
    names(d)[1:2] <- c(
      edp |> deparse1(),
      label_percent(prefix = 'Difference (', suffix = '% CI)')(conf.level)
    )
    return(d)
  }, dots = list(x = x0, nm = names(x0)), MoreArgs = NULL)
  # list of 'data.frame'
  
  #if (length(x0) == 1L) or not..
  ret0 |>
    do.call(what = rbind.data.frame) |>
    flextable() |>
    autofit(part = 'all') |>
    vline(j = 1L) |>
    hline(i = vapply(x0, FUN = nrow, FUN.VALUE = NA_integer_)[-length(x0)])
  
}





#' @export
md_.TukeyHSD <- function(x, xnm, ...) {
  
  '[@Tukey49 Honest Significant Differences (HSD) test](https://en.wikipedia.org/wiki/Tukey%27s_range_test) is provided using <u>**`R`**</u>.' |>
    new(Class = 'md_lines') |>
    c(
      . = _, 
      md_flextable_(x = x, xnm = xnm, ...) # to get default bib_()
    ) # [c.md_lines()]
  
}


#' @export
bib_.TukeyHSD <- function(x) .tukey49()








