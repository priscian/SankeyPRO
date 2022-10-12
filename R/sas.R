#' @export
prepare_sas_data <- function(
  sas_dir,
  id_var,
  sas_files_re = "\\.sas7bdat$",
  recursive = FALSE,
  catalog_file = NULL,
  long_to_wide... = list(),
  list_variable_name,
  keep_long = FALSE,
  verbose = TRUE
)
{
  sas_files <- keystone::list_files(sas_dir, sas_files_re, ignore.case = TRUE, full.names = TRUE, recursive = recursive)
  l <- sapply(sas_files,
    function(a)
    {
      if (verbose) {
        cat(sprintf("Processing file %s...", basename(a))); utils::flush.console()
      }

      d <- rio::import(a, catalog_file = catalog_file)
      ## 'capture.output()' & 'sink()' don't work with 'readr::type_convert()' at all!
      invisible(capture.output({ dd <- d %>% readr::type_convert() }))
      plyr::l_ply(names(d), function(b) { mostattributes(dd[[b]]) <<- attributes(d[[b]]) })

      ## Make R factors from variables with SAS attributes
      ddd <- dd %>% dplyr::mutate_if(is_sas_factor, r_ify_sas_factor)
      ddd0 <- rlang::duplicate(ddd, shallow = FALSE)
      ## Make long data sets into wide ones for summarizing
      if (anyDuplicated(ddd[[id_var]])) { # 'anyDuplicated()' can handle NULL variables
        long_to_wideArgs <- list(x = ddd, id_cols = id_var)
        long_to_wideArgs <- utils::modifyList(long_to_wideArgs, long_to_wide..., keep.null = TRUE)

        ddd <- do.call(long_to_wide, long_to_wideArgs)
        cat("Long to wide: ", basename(a), "\n", sep = "")
      }

      if (verbose) {
        cat(". Done.", fill = TRUE); utils::flush.console()
      }

      if (!keep_long)
        tibble::as_tibble(ddd)
      else
        sapply(list(wide = ddd, long = ddd0), tibble::as_tibble)
    }, simplify = FALSE)

  assign(list_variable_name, l)

  l
}


#' @export
long_to_wide <- function(
  x,
  long_var = "Assessment",
  id_cols = c("gapid"),
  non_value_cols = NULL
)
{
  names_glue <- NULL
  valuesFrom <- x %>% dplyr::select(-any_of(c(id_cols, long_var, non_value_cols))) %>% colnames
  ## Always include variable name as a prefix:
  if (length(valuesFrom) == 1)
    names_glue <- "{.value}_{.name}"
  xx <- x %>%# dplyr::mutate(!!long_var := as.character(.[[long_var]])) %>%
    dplyr::filter(!is.na(!!long_var)) %>%
    tidyr::pivot_wider(
      id_cols = any_of(id_cols),
      names_glue = names_glue,
      names_from = !!long_var,
      #values_from = -any_of(c(id_cols, long_var, non_value_cols))
      values_from = !!valuesFrom
    )

  xx
}


#' @export
is_sas_factor <- function(
  x, # A vector with SAS attributes imported via 'haven::read_sas()'
  attribute_name = "labels"
)
{
  !is.null(attr(x, attribute_name, exact = TRUE))
}


#' @export
r_ify_sas_factor <- function(
  x, #  A vector with SAS attributes imported via 'haven::read_sas()'
  label_re = stringr::regex("^(\\s*\\d+\\:\\s*)", ignore_case = TRUE)
)
{
  if (!is_sas_factor(x))
    return (x)

  l <- attr(x, "labels", exact = TRUE)
  label_map <-
    structure(stringr::str_replace(names(l), label_re, ""), .Names = as.character(l))
  ## Handle probable multiple-choice variables here.
  ul <- stringr::str_extract(l, "^(\\s*\\d+)") %>% trimws
  if (x %>% unique %>% as.character %>% trimws %neq% ul %>% na.omit %>% any) { # If TRUE then this may be multiple-choice variable
    xx <- make_mchoice_variable(x, pattern = " ")
    if (label_map[levels(xx)] %>% duplicated %>% any) # Probably NOT multiple-choice
      xx <- x
    else
      levels(xx) <- label_map[levels(xx)]
  } else {
    xx <- label_map[as.character(x)]
    #mostattributes(xx) <- attributes(x)
    xx <- xx %>% sjmisc::to_factor()
    xx <- forcats::fct_relevel(xx, label_map %>% unique) # 'unique()' so this doesn't fail
  }
  xx <- structure(xx, label = Hmisc::label(x), format.sas = attr(x, "format.sas"), labels = attr(x, "labels"))

  xx
}


#' @export
make_mchoice_variable <- function(
  x,
  pattern = ";",
  ...,
  max_choice_length = 97L) # Leave 3 characters for ellipsis "..."
{
  xx <- stringr::str_split(x, pattern)
  max_length_xx <- sapply(xx, length) %>% max
  xxx <- sapply(xx,
    function(a)
    {
      length(a) <- max_length_xx
      a[is.na(a)] <- ""

      if (!is.null(max_choice_length))
        a <- stringr::str_trunc(a, max_choice_length)

      a
    }, simplify = FALSE)

  do.call(Hmisc::mChoice, c(Reduce(rbind, xxx) %>% dataframe %>% as.list, sort.levels = "alphabetic"))
}


#' @export
toSAS.mChoice <- function(
  x,
  format = SASxport::SASformat(x),
  format.info = NULL
)
{
  as.character(x)
}


#' @export
export_dataset <- function(
  x,
  r_varname = "d_out",
  out_path_sans_ext = NULL
)
{
  # SASxport::write.xport(d, file = out_path_sans_ext %_% ".xps", verbose = TRUE)
  d_out <- x %>%
    dplyr::mutate_if(Hmisc::is.mChoice,
    function(a)
    {
      aa <- structure(
        as.character(a),
        label = attr(a, "label"),
        format.sas = attr(a, "format.sas"),
        SASformat = attr(a, "SASformat")
      )

      aa
    })
  ## Write SAS file.
  #haven::write_sas(d_out, out_path_sans_ext %_% ".sas7bdat")
  ## Use 'rio::import()' to confim.

  foreign::write.foreign(
    df = d_out,
    datafile = out_path_sans_ext %_% ".txt",
    codefile = out_path_sans_ext %_% ".sas",
    #dataname = paste("u01ml", basename(out_path_sans_ext), sep = "."), # SAS library
    dataname = basename(out_path_sans_ext),
    package = "SAS",
    libpath = dirname(out_path_sans_ext)
  )

  ## Write SPSS file.
  haven::write_sav(
    structure(d_out,
      .Names = stringr::str_trunc(vctrs::vec_as_names(names(d_out), repair = "universal"), 64)),
    out_path_sans_ext %_% ".sav"
  )
  ## Use 'rio::import()' to confim.

  ## Write RData file.
  e <- new.env()
  assign(r_varname, d_out, envir = e); e[["RVAR"]] = as.name(r_varname)
  substitute(save(RVAR, file = out_path_sans_ext %_% ".RData", envir = e), env = e) %>% eval

  ## Write CSV file.
  rio::export(d_out, out_path_sans_ext %_% ".csv")

  keystone::nop()
}
