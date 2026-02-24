#' Build a Dyad Variable Names (dvn) List Without a Data Frame
#'
#' Creates a dvn structure for use with dySEM population scripters and simulation
#' workflows. Variable names are generated directly from naming conventions without
#' requiring an actual data frame. Compatible with \code{scrapeVarCross} naming patterns.
#'
#' @param n_items_x Number of indicators per partner (X construct; future Y support).
#' @param x_order Order of (S)tem, (I)tem, (P)artner in variable names. One of \code{"sip"}, \code{"spi"}, \code{"psi"}. Default \code{"sip"} produces \code{x1_A}, \code{x2_A}, etc.
#' @param x_stem Character stem of indicator variables (e.g., \code{"x"}).
#' @param x_delim1 Character separating stem from item or partner. Use \code{""} for none.
#' @param x_delim2 Character separating item from partner (sip) or partner from item (spi).
#' @param distinguish_1 Character identifying the first partner (e.g., \code{"A"}).
#' @param distinguish_2 Character identifying the second partner (e.g., \code{"B"}).
#' @return A list compatible with dySEM dvn structure:
#'   \item{p1xvarnames}{Character vector of variable names for partner 1}
#'   \item{p2xvarnames}{Character vector of variable names for partner 2}
#'   \item{xindper}{Number of indicators per partner}
#'   \item{dist1}{First partner distinguisher}
#'   \item{dist2}{Second partner distinguisher}
#'   \item{indnum}{Total number of indicators}
#' @family variable-scraping functions
#' @export
#' @examples
#' dvn <- build_dvn(
#'   n_items_x = 3,
#'   x_order = "sip",
#'   x_stem = "x",
#'   x_delim1 = "",
#'   x_delim2 = "_",
#'   distinguish_1 = "A",
#'   distinguish_2 = "B"
#' )
#' # Produces x1_A, x2_A, x3_A and x1_B, x2_B, x3_B
build_dvn <- function(n_items_x,
                      x_order = "sip",
                      x_stem = "x",
                      x_delim1 = "",
                      x_delim2 = "_",
                      distinguish_1 = "A",
                      distinguish_2 = "B") {
  if (!is.numeric(n_items_x) || length(n_items_x) != 1 || n_items_x < 1) {
    stop("`n_items_x` must be a positive integer.")
  }
  n_items_x <- as.integer(n_items_x)

  if (!x_order %in% c("sip", "spi", "psi")) {
    stop("`x_order` must be one of: 'sip', 'spi', or 'psi'.")
  }

  # Generate variable names for each partner based on order
  make_vars <- function(distinguish) {
    items <- seq_len(n_items_x)
    switch(x_order,
      sip = paste0(x_stem, x_delim1, items, x_delim2, distinguish),
      spi = paste0(x_stem, x_delim1, distinguish, x_delim2, items),
      psi = paste0(distinguish, x_delim1, x_stem, x_delim2, items)
    )
  }

  p1xvarnames <- make_vars(distinguish_1)
  p2xvarnames <- make_vars(distinguish_2)

  list(
    p1xvarnames = p1xvarnames,
    p2xvarnames = p2xvarnames,
    xindper = n_items_x,
    dist1 = distinguish_1,
    dist2 = distinguish_2,
    indnum = 2L * n_items_x
  )
}
