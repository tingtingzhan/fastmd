#' @title `htest` Object
#' 
#' @seealso
#' \link[flextable]{as_flextable.htest}
#' 
#' @examples
#' list(
#'  '$t$-test' = t.test(mpg ~ am, data = mtcars)
#' ) |> render2html()
#' 
#' @name htest
NULL


md_htest_ <- \(x, ...) {
  
  pv <- x |> 
    getElement(name = 'p.value')
  if (is.na(pv)) return(new(Class = 'md_lines')) # exception handling
  
  p <- pv |>
    label_pvalue_sym(add_p = TRUE)()
  
  x$method |>
    switch(EXPR = _, 'Fisher\'s Exact Test for Count Data' = {
      # stats::fisher.test
      p |>
        sprintf(fmt = '[@Fisher22 exact test](https://en.wikipedia.org/wiki/Fisher\'s_exact_test) (%s)') |>
        new(Class = 'md_lines', bibentry = .fisher22())
      
    }, 'Pearson\'s Chi-squared test' = {
      # stats::chisq.test
      p |>
        sprintf(fmt = '[@Pearson1900 \u03c7\u00b2 test](https://en.wikipedia.org/wiki/Pearson\'s_chi-squared_test) (%s)') |>
        new(Class = 'md_lines', bibentry = .pearson1900())
      
    }, 'Pearson\'s Chi-squared test with Yates\' continuity correction' = {
      # stats::chisq.test
      p |>
        sprintf(fmt = '[@Pearson1900 \u03c7\u00b2 test](https://en.wikipedia.org/wiki/Pearson\'s_chi-squared_test) with [@Yates34 continuity correction](https://en.wikipedia.org/wiki/Yates\'s_correction_for_continuity) (%s)') |>
        new(Class = 'md_lines', bibentry = c(.pearson1900(), .yates34()))
      
    }, 'One Sample t-test' = {
      # stats::t.test; one-sample
      p |> 
        sprintf(fmt = '[@Student08 Student\'s *t*-test](https://en.wikipedia.org/wiki/Welch\'s_t-test) (%s)') |>
        new(Class = 'md_lines', bibentry = .student08())
      
    }, 'Welch Two Sample t-test' = {
      # stats::t.test; two-sample, unequal variance
      p |> 
        sprintf(fmt = '[@Welch47 - @Satterthwaite46 *t*-test](https://en.wikipedia.org/wiki/Welch\'s_t-test) (%s)') |>
        new(Class = 'md_lines', bibentry = c(.welch47(), .satterthwaite46()))
      
    }, 'Wilcoxon signed rank exact test' = {
      # stats::wilcox.test
      p |> 
        sprintf(fmt = '[@Wilcoxon45 signed-rank test](https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test) (%s)') |>
        new(Class = 'md_lines', bibentry = .wilcoxon45())
    }, 'Wilcoxon rank sum exact test' =,
    'Wilcoxon rank sum test with continuity correction' = {
      # stats::wilcox.test
      p |> 
        sprintf(fmt = '[@Wilcoxon45 - @MannWhitney47 rank-sum $U$-test](https://en.wikipedia.org/wiki/Mann\u2013Whitney_U_test) (%s)') |>
        new(Class = 'md_lines', bibentry = c(.wilcoxon45(), .mann_whitney47()))
    }, stop('un-written')) 
}

if (FALSE) {
  '$p$' |> ftExtra::as_paragraph_md() # error!!
}



#' @export
md_.htest <- function(x, ...) {
  c(
    # md_htest_(x),
    md_int(x, engine = 'print', ...)
  )
}
# do *not* want to use
# ?flextable:::as_flextable.htest



