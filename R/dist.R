

# Inspired by the `S3` method \link[stats]{print.dist}.
#' @export
as_flextable.dist <- function(x, ...) {
  Labels <- x |> attr(which = 'Labels', exact = TRUE)
  Size <- x |> attr(which = 'Size', exact = TRUE)
  if (!length(Labels)) 
    Labels <- Size |> seq_len() |> sprintf(fmt = '[%d]')
  .dim <- c(Size, Size)
  ret <- array('', dim = .dim, dimnames = list(Labels, Labels))
  lo <- .row(.dim) - .col(.dim) # ?base::lower.tri
  ret[lo > 0L] <- format(x, ...) # ?stats:::format.dist
  ret[-1L, -Size, drop = FALSE] |>
    as_flextable.matrix(...)
}




