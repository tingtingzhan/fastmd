
#' @title Convert \link[stats]{ftable} to \link[flextable]{flextable}
#' 
#' @param x an \link[stats]{ftable}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' The `S3` method [as_flextable.ftable()] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @importFrom zoo na.locf
#' @export as_flextable.ftable
#' @export
as_flextable.ftable <- function(x, ...) {
  
  atr <- attributes(x)
  nr <- length(atr$row.vars) # lowest group
  rseq <- seq_len(nr)
  
  xf <- x |>
    format(quote = FALSE, method = 'compact', lsep = '', ...) # ?stats:::format.ftable
  xf[] <- trimws(xf)
  cnm <- xf[1L, , drop = TRUE]
  xf <- xf[-1L, , drop = FALSE]
  colnames(xf) <- cnm
  xf[,rseq][!nzchar(xf[,rseq])] <- NA_character_

  h_i <- seq.int(from = 0, to = nrow(xf), by = length(atr$row.vars[[nr]]))
  
  xf |>
    as.data.frame.matrix() |>
    na.locf(na.rm = FALSE) |> # ?zoo:::na.locf.data.frame
    flextable() |>
    autofit(part = 'all') |>
    vline(j = nr) |>
    hline(i = h_i[-c(1L, length(h_i))]) |>
    merge_v(part = 'body', j = seq_len(nr))
  
}





