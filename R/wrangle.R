## Handle some configurations of missing values
#' @export
handle_missings <- function(
  x,
  ## If FALSE, rowwise: At 1st NA from left to right, make all further rightwise columns into NAs; else
  ## If TRUE, remove the intermittently valued rows described above
  remove_intermittent = TRUE
)
{
  r <- x %>% plyr::adply(1,
    function(a)
    {
      ww <- sapply(a, is.na) %>% which
      w <- ww %>% head(1)
      if (!keystone::is_invalid(w)) {
        ## These next few lines prevent an attributes warning:
        NAs <- as.list(sapply(a, function(b) { NA %>% `mode<-`(mode(b)) }))
        attrs <- sapply(a, attributes, simplify = FALSE)
        plyr::l_ply(seq(NCOL(a)), function(b) { attributes(NAs[[b]]) <<- attrs[[b]] })

        if (remove_intermittent) {
          niSeq <- w:NCOL(a)
          if (!all(rep(ww, length.out = length(niSeq)) == niSeq))
            a[, 1:NCOL(a)] <- NAs
        } else {
          a[, w:NCOL(a)] <- NAs %>% `[`(w:NCOL(a))
        }
      }

      a
    }) %>% tibble::as_tibble() %>%
    ## Remove rows w/ all NAs
    ## V. https://stackoverflow.com/questions/6437164/removing-empty-rows-of-a-data-file-in-r/6437778#6437778
    dplyr::filter(rowSums(is.na(.)) != NCOL(.))

  r
}


#' @export
make_long <- function(
  x,
  ...
)
{
  structure(ggsankey::make_long(x, !!names(x)) %>% keystone::add_class("sankeypro"), nrow = NROW(x))
}


#' @export
categorize_nodes <- function(
  d,
  cutpoints,
  labels,
  na_level = "Dropout/Withdrawal",
  ...
)
{
  recode_levels <- function(x, y = structure(levels(x), .Names = labels))
  {
    if (is.null(names(y)))
      return (x)

    if (!is.null(names(labels)))
      y <- labels

    forcats::fct_recode(x, !!!y)
  }

  if (keystone::is_invalid(cutpoints)) {
    cutpoints <- median(d$node, na.rm = TRUE)
  }

  if (keystone::is_invalid(labels)) {
    labels <- c(na_level, levels(keystone::cut2q(d$node, cuts = cutpoints, ...)))
  }

  r <- d %>% dplyr::mutate(across(ends_with("node"), ~ keystone::cut2q(.x, cuts = cutpoints, ...))) %>%
    dplyr::mutate(across(ends_with("node"), ~ forcats::fct_explicit_na(.x, na_level = na_level))) %>%
    dplyr::mutate(across(ends_with("node"), ~ forcats::fct_relevel(.x, na_level))) %>%
    dplyr::mutate(across(ends_with("node"), ~ recode_levels(.x))) %>% # N.B. The 'fct_recode()' call must be in an external fn
    dplyr::add_count(x, node) %>%
    dplyr::mutate(pct = n/attr(d, "nrow"))

  r
}
