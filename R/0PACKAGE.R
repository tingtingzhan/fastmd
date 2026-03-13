


#' @keywords internal
#'
#' @importFrom cli col_blue col_cyan col_magenta col_br_red style_bold
#' @importFrom flextable as_flextable flextable autofit hline vline merge_h merge_v align fix_border_issues highlight set_caption
#' @importFrom flextable add_footer_lines add_header_row colformat_double wrap_flextable
#' @importFrom flextable init_flextable_defaults
#' 
#' @importFrom methods new show slot
#' @importFrom stats fisher.test chisq.test
#' @importFrom stats formula getCall p.adjust p.adjust.methods
#' @importFrom utils bibentry toBibtex person as.person browseURL citation
#' @importFrom utils getAnywhere getS3method installed.packages
#'
'_PACKAGE'

# clash
# flextable:::as_flextable.data.frame
# ftExtra:::as_flextable.data.frame