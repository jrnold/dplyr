#' Proportion of observations by group
#'
#' \code{prop} calculates the proportion of observations by group.
#' This is analagous to \code{\link[dplyr]{count}} which counts the number of
#' rows by group. The function \code{prop} is to \code{\link[dplyr]{count}} as
#' as \code{\link[base]{table}} is to \code{\link[base]{prop.table}}.
#'
#' @param x A tbl
#' @param wt If omitted, will the number of rows. If specified, will perform a "weighted" proportion by summing the (non-missing) values of variable \code{wt}.
#' @param vars,... Variables to group by
#' @param sort If \code{TRUE}, sort output in descending order of \code{n}.
#' @return A tbl with the grouping variables and column \code{.prop} with the
#'   proportion in each group.
#' @export
prop <- function(x, ..., wt = NULL, sort = FALSE) {
  vars <- lazyeval::lazy_dots(...)
  wt <- substitute(wt)
  prop_(x, vars, wt, sort = sort)
}

#' @rdname prop
#' @export
prop_ <- function(x, vars, wt = NULL, sort = FALSE) {
  p <- ungroup(count_(x, vars, wt = wt))
  p <- mutate_(p, .prop = ~ n / sum(n))
  p <- select_(p, ~ -n)
  if (sort) {
    p <- arrange_(p, desc(~ .prop))
  }
  p
}
