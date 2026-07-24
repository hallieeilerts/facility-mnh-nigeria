

# render rmarkdown report and archive previous ----------------------------

fn_render_dated_report <- function(input) {
  base_name <- tools::file_path_sans_ext(basename(input))
  
  archive_dir <- here::here("gen", "monitoring", "reports", "archive")
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  
  dated_file <- paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"), ".html")
  
  # Render to the archive with the dated filename
  rmarkdown::render(
    input,
    output_dir = archive_dir,
    output_file = dated_file,
    envir = globalenv()
  )
  
  # Copy that same file up one level as the stable "latest" version
  file.copy(
    from = file.path(archive_dir, dated_file),
    to = here::here("gen", "monitoring", "reports", paste0(base_name, "_latest.html")),
    overwrite = TRUE
  )
}

fn_render_dated_word <- function(input) {
  base_name <- tools::file_path_sans_ext(basename(input))
  
  archive_dir <- here::here("gen", "monitoring", "tables", "archive")
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  
  dated_file <- paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"), ".docx")
  
  # Render to the archive with the dated filename
  rmarkdown::render(
    input,
    output_format = "officedown::rdocx_document",
    output_dir = archive_dir,
    output_file = dated_file,
    envir = globalenv()
  )
  
  # Copy that same file up one level as the stable "latest" version
  file.copy(
    from = file.path(archive_dir, dated_file),
    to = here::here("gen", "monitoring", "tables", paste0(base_name, "_latest.docx")),
    overwrite = TRUE
  )
}

fn_save_dated_workbook <- function(wb, base_name) {
  
  # Archive location (dated, permanent record)
  archive_dir <- here::here("gen", "monitoring", "tables", "archive")
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  
  dated_file <- paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
  dated_path <- file.path(archive_dir, dated_file)
  
  # Save the dated version to archive
  saveWorkbook(wb, dated_path, overwrite = TRUE)
  
  # Stable "latest" location (what your Rmd reads from, always same filename)
  latest_dir <- here::here("gen", "monitoring", "tables")
  dir.create(latest_dir, recursive = TRUE, showWarnings = FALSE)
  
  file.copy(
    from = dated_path,
    to = file.path(latest_dir, paste0(base_name, "_latest.xlsx")),
    overwrite = TRUE
  )
  
  invisible(dated_path)
}


# compact tables for rmarkdown report -------------------------------------


compact_kable <- function(df, caption = NULL, col.names = NULL) {
  if (!is.null(caption)) {
    cat(sprintf('<div class="table-caption">%s</div>', caption))
  }
  
  if (is.null(col.names)) {
    kbl_obj <- df %>% kable()
  } else {
    kbl_obj <- df %>% kable(col.names = col.names)
  }
  
  kbl_obj %>%
    kable_styling(
      bootstrap_options = c("condensed", "striped", "hover"),
      full_width = FALSE,
      position = "left",
      font_size = 13
    )
}


# log data quality checks -------------------------------------------------

fn_log_qc <- function(dat, sn, variable, type, description = NULL, 
                      dat_enu = dat_enu_dedup, 
                      dat_wom_bths = dat_wom_bths, 
                      dat_chld = dat_chld) {
  
  # dat - dataframe with the problematic observations
  # sn - short name of file (dat_chld, dat_wom_bths, dat_dths, dat_enu)
  # variable - variable being quality checked
  # type - type of error (missing, plausibility, error, clarification)
  # description - description of error (optional)
  # dat_enu - household mortality data, must be de-duplicated
  
  # abort log if no IDs in error data frame
  if (nrow(dat) == 0) return(invisible(NULL))
  
  # add hhhid and cluster
  if(sn == "dat_enu"){
    df_cluster_hhd <- dat_enu %>%
      select(hhid, key, cluster)
    # drop any columns from df_cluster_hhd that already exist in dat (except the join key)
    dupe_cols <- intersect(names(df_cluster_hhd), names(dat))
    dupe_cols <- setdiff(dupe_cols, "key")   # never drop the join key itself
    if (length(dupe_cols) > 0) {
      df_cluster_hhd <- df_cluster_hhd %>% select(-all_of(dupe_cols))
    }
    dat <- dat %>%
      left_join(df_cluster_hhd, by = "key")
  }
  if(sn == "dat_chld"){
    df_cluster_hhd <- dat_chld %>%
      select(parent_key, key) %>%
      rename(mth_key = parent_key) %>%
      left_join(dat_wom_bths %>% select(parent_key, key), by = c("mth_key" = "key"))  %>%
      rename(hhd_key = parent_key) %>%
      left_join(dat_enu %>% select(key, hhid, cluster), by = c("hhd_key" = "key")) %>%
      select(key, hhid, cluster)
    dat <- dat %>%
      left_join(df_cluster_hhd, by = "key")
  }
  if(sn == "dat_wom_bths"){
    df_cluster_hhd <- dat_wom_bths %>%
      select(parent_key, key) %>%
      rename(hhd_key = parent_key) %>%
      left_join(dat_enu %>% select(key, hhid, cluster), by = c("hhd_key" = "key")) %>%
      select(key, hhid, cluster)
    dat <- dat %>%
      left_join(df_cluster_hhd, by = "key")
  }
  
  # create entry
  entry <- tibble(
    DATE = Sys.time(),
    FILENAME_RAW = subset(df_name_mapping, short_name == sn)$raw_name,
    FILENAME_CLEAN = subset(df_name_mapping, short_name == sn)$clean_name,
    CLUSTER = dat$cluster,
    HHID = dat$hhid,
    KEY = dat$key,
    VARIABLE = variable,
    TYPE = type,
    DESCRIPTION = if (is.null(description)) NA_character_ else description
  )
  
  # store in list
  l_qc_log[[length(l_qc_log) + 1]] <<- entry
  invisible(entry)
}
