# Carry existing fits across the removal of the indirect cache
# =============================================================
#
# Run this from the project directory, once, before the first tar_make() with
# the version of nlmixr2targets that passes the simplified model object between
# targets instead of caching it and passing an md5.  From an R session in the
# project directory:
#
#     source(system.file(
#       "scripts", "convert-indirect-cache.R", package = "nlmixr2targets"
#     ))
#
# Run with Rscript instead and these flags apply: --dry-run reports and changes
# nothing, --no-backup skips the copy of the metadata, and --store=PATH and
# --script=PATH override the targets config.
#
# What it does
# ------------
# In a store written by an earlier version every `_object_simple` value is a
# character md5 where an `rxUi` is now expected, and the generated commands all
# carried a cache directory that no longer exists.  `targets` compares the
# recorded command and dependency hashes against the ones the pipeline produces
# now, so without help it re-runs every fit, the one expensive target in each
# group.
#
# The generated helper targets are cheap, so they are rebuilt rather than
# converted.  Once they hold their new values, each estimation target's two
# stale fields can be recomputed and written, which leaves the fit itself
# untouched on disk and up to date in the metadata.
#
# The hashes are obtained the way targets obtains them:
#
#   command  read straight off the target object in the pipeline this version
#            builds, so nothing is reimplemented
#   depend   siphash13 of the dependency names followed by their recorded data
#            hashes, over the dependencies that have a record, in the order the
#            target lists them
#
# tests/testthat/test-convert.R pins the second against a pipeline it builds
# and runs, so a change in how targets hashes fails this package's own tests
# rather than corrupting a store.
#
# What it touches
# ---------------
# Only the command and depend fields of the estimation targets' rows in
# <store>/meta/meta.  The fit objects under <store>/objects/ are never read or
# written.  A timestamped copy of the metadata is made first unless
# --no-backup.  The cache directory is removed last, once the rewrite has
# succeeded.

# The separator targets uses between metadata fields.
convert_separator <- function() {
  rawToChar(as.raw(124L))
}

convert_empty <- function() {
  data.frame(
    name = character(0),
    command_old = character(0),
    command_new = character(0),
    depend_old = character(0),
    depend_new = character(0),
    stringsAsFactors = FALSE
  )
}

# targets hashes the dependency names followed by their recorded data hashes.
# A dependency with no record, such as a base function the command calls,
# contributes nothing: targets builds the list with lapply() and unlist() drops
# the NULLs.
convert_depend_hash <- function(deps, meta) {
  keep <- deps[deps %in% meta$name]
  if (!length(keep)) {
    return(NA_character_)
  }
  hashes <- meta$data[match(keep, meta$name)]
  missing <- is.na(hashes) | !nzchar(hashes)
  if (any(missing)) {
    stop(
      "These dependencies have no stored value, so the dependency hash ",
      "cannot be computed: ", paste(keep[missing], collapse = ", "),
      call. = FALSE
    )
  }
  secretbase::siphash13(x = paste(c(keep, hashes), collapse = ""))
}

convert_flatten <- function(value, accumulator) {
  if (inherits(value, "tar_target")) {
    accumulator$targets[[value$settings$name]] <- value
  } else if (is.list(value)) {
    for (element in value) {
      convert_flatten(element, accumulator)
    }
  }
  invisible()
}

# The pipeline as this version of the package builds it.  Sourcing the script
# would otherwise stop at the very check that sends people here, so the check
# stands down for the duration.
convert_pipeline_targets <- function(script) {
  if (!file.exists(script)) {
    stop("No targets script at ", script, ".", call. = FALSE)
  }
  previous <- getOption("nlmixr2targets.allow_indirect_cache")
  options(nlmixr2targets.allow_indirect_cache = TRUE)
  on.exit(
    options(nlmixr2targets.allow_indirect_cache = previous),
    add = TRUE
  )
  value <- eval(parse(script), envir = new.env(parent = globalenv()))
  accumulator <- new.env(parent = emptyenv())
  accumulator$targets <- list()
  convert_flatten(value, accumulator)
  if (!length(accumulator$targets)) {
    stop("The script ", script, " defines no targets.", call. = FALSE)
  }
  accumulator$targets
}

# The estimation targets' own dependencies that this package generates and that
# the current version writes differently.  Identified by what the command calls
# rather than by the target's name, which is configurable, and by the command
# hash having moved, so a dependency the pipeline still writes the same way is
# left alone and an expensive target of the user's own is never caught.
convert_stale_upstream <- function(pipeline, fits, meta) {
  generators <- c(
    "nlmixr_object_simplify",
    "nlmixr_object_covariates",
    "nlmixr_data_simplify"
  )
  deps <- unique(unlist(lapply(fits, function(name) pipeline[[name]]$deps)))
  deps <- intersect(deps, names(pipeline))
  if (!length(deps)) {
    return(character(0))
  }
  keep <- vapply(
    deps,
    function(name) {
      target <- pipeline[[name]]
      generated <- any(vapply(
        generators,
        function(generator) {
          grepl(generator, target$command$string, fixed = TRUE)
        },
        logical(1)
      ))
      recorded <- meta$command[match(name, meta$name)]
      generated && !identical(recorded, target$command$hash)
    },
    logical(1),
    USE.NAMES = FALSE
  )
  deps[keep]
}

# One row per estimation target that has a stored fit worth keeping.
convert_report <- function(pipeline, fits, meta, depend = TRUE) {
  rows <- list()
  for (name in fits) {
    row <- match(name, meta$name)
    if (is.na(row) || is.na(meta$data[row]) || !nzchar(meta$data[row])) {
      next
    }
    target <- pipeline[[name]]
    rows[[name]] <- data.frame(
      name = name,
      command_old = meta$command[row],
      command_new = target$command$hash,
      depend_old = meta$depend[row],
      depend_new = if (depend) {
        convert_depend_hash(target$deps, meta)
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, c(list(convert_empty()), rows))
}

convert_backup <- function(meta_path) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  destination <- paste0(meta_path, ".before-conversion-", stamp)
  if (!file.copy(from = meta_path, to = destination)) {
    stop("Could not copy ", meta_path, " to ", destination, ".", call. = FALSE)
  }
  message("Copied the metadata to ", destination, ".")
  destination
}

# Rewrites the two fields, and only on the last row of each named target, which
# is the one targets reads.  Splitting and rejoining every field keeps a stray
# separator in a later column intact.
convert_write_meta <- function(meta_path, report) {
  separator <- convert_separator()
  lines <- readLines(meta_path, warn = FALSE)
  if (!length(lines)) {
    stop("The metadata at ", meta_path, " is empty.", call. = FALSE)
  }
  header <- strsplit(lines[[1L]], separator, fixed = TRUE)[[1L]]
  index <- match(c("name", "command", "depend"), header)
  if (anyNA(index)) {
    stop(
      "The metadata header has no name, command or depend field.",
      call. = FALSE
    )
  }
  names(index) <- c("name", "command", "depend")
  split <- lapply(
    lines,
    function(line) strsplit(line, separator, fixed = TRUE)[[1L]]
  )
  recorded <- vapply(
    split,
    function(fields) {
      if (length(fields) < index[["name"]]) {
        ""
      } else {
        fields[[index[["name"]]]]
      }
    },
    character(1)
  )
  for (row in seq_len(nrow(report))) {
    name <- report$name[[row]]
    matches <- which(recorded == name)
    matches <- matches[matches > 1L]
    if (!length(matches)) {
      stop("No metadata row for ", name, ".", call. = FALSE)
    }
    last <- matches[[length(matches)]]
    fields <- split[[last]]
    if (!identical(fields[[index[["command"]]]], report$command_old[[row]])) {
      stop(
        "The metadata for ", name, " changed while converting. Restore the ",
        "backup, if one was made, and start again.",
        call. = FALSE
      )
    }
    fields[[index[["command"]]]] <- report$command_new[[row]]
    fields[[index[["depend"]]]] <- report$depend_new[[row]]
    lines[[last]] <- paste(fields, collapse = separator)
  }
  connection <- file(meta_path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeLines(lines, con = connection)
  invisible(nrow(report))
}

convert_indirect_cache <- function(store = targets::tar_config_get("store"),
                                   script = targets::tar_config_get("script"),
                                   backup = TRUE,
                                   dry_run = FALSE) {
  directory <- file.path(store, "user", "nlmixr2")
  if (!dir.exists(directory)) {
    message("No indirect cache at ", directory, ", so there is nothing to do.")
    return(invisible(convert_empty()))
  }
  meta_path <- file.path(store, "meta", "meta")
  if (!file.exists(meta_path)) {
    stop(
      "No targets metadata at ", meta_path, ", so there are no fits to keep. ",
      "Remove ", directory, " and build normally.",
      call. = FALSE
    )
  }
  pipeline <- convert_pipeline_targets(script = script)
  fits <- grep("_fit_simple$", names(pipeline), value = TRUE)
  if (!length(fits)) {
    stop(
      "The pipeline in ", script, " has no estimation targets, so the cache ",
      "at ", directory, " belongs to a different project. Point the store and ",
      "the script at the same one.",
      call. = FALSE
    )
  }
  # Every column rather than a selection: tar_meta() selects fields with
  # tidyselect, which deprecates being handed a character vector.
  meta <- targets::tar_meta(store = store, targets_only = FALSE)
  upstream <- convert_stale_upstream(
    pipeline = pipeline,
    fits = fits,
    meta = meta
  )
  if (dry_run) {
    out <- convert_report(
      pipeline = pipeline,
      fits = fits,
      meta = meta,
      depend = FALSE
    )
    message(
      "Would adopt ", nrow(out), " fit(s) and rebuild ", length(upstream),
      " cheap target(s)."
    )
    if (length(upstream)) {
      message("  ", paste(upstream, collapse = "\n  "))
    }
    message(
      "The new dependency hashes are not shown: they are taken over the ",
      "rebuilt values, which a dry run does not produce."
    )
    return(invisible(out))
  }
  if (backup) {
    convert_backup(meta_path = meta_path)
  }
  if (length(upstream)) {
    message("Rebuilding ", length(upstream), " cheap target(s).")
    # targets validates `names` as a character vector and then selects with
    # tidyselect, which deprecates being handed one.  That warning is about
    # targets' own call and says nothing about this conversion, so it alone
    # is muffled and every other warning still reaches the user.
    # callr_function = NULL keeps the build in this process. The check that
    # sends people here stands down through an option, and an option does
    # not cross into a callr subprocess, so a subprocess would re-source
    # `_targets.R` and stop at that very check. These targets take seconds,
    # so there is nothing a subprocess would buy.
    withCallingHandlers(
      targets::tar_make(
        names = upstream,
        store = store,
        script = script,
        callr_function = NULL,
        reporter = "silent"
      ),
      warning = function(condition) {
        if (grepl(
          "external vector in selections",
          conditionMessage(condition),
          fixed = TRUE
        )) {
          invokeRestart("muffleWarning")
        }
      }
    )
    meta <- targets::tar_meta(store = store, targets_only = FALSE)
  }
  out <- convert_report(pipeline = pipeline, fits = fits, meta = meta)
  if (!nrow(out)) {
    message("No stored fits to adopt. Removing ", directory, ".")
    unlink(directory, recursive = TRUE)
    return(invisible(out))
  }
  convert_write_meta(meta_path = meta_path, report = out)
  unlink(directory, recursive = TRUE)
  message("Adopted ", nrow(out), " fit(s) and removed ", directory, ".")
  message(
    "Confirm with targets::tar_outdated(): the estimation targets should not ",
    "be listed. If they are, restore the metadata backup."
  )
  invisible(out)
}

convert_main <- function(arguments = commandArgs(trailingOnly = TRUE)) {
  if (!requireNamespace("secretbase", quietly = TRUE)) {
    stop(
      "The secretbase package is needed for the hash targets uses. It ships ",
      "with targets, so installing secretbase is all that is required.",
      call. = FALSE
    )
  }
  value <- function(flag, fallback) {
    hit <- grep(paste0("^", flag, "="), arguments, value = TRUE)
    if (length(hit)) {
      sub(paste0("^", flag, "="), "", hit[[1L]])
    } else {
      fallback
    }
  }
  convert_indirect_cache(
    store = value("--store", targets::tar_config_get("store")),
    script = value("--script", targets::tar_config_get("script")),
    backup = !("--no-backup" %in% arguments),
    dry_run = "--dry-run" %in% arguments
  )
}

# Runs when the file is executed, and only defines the functions when it is
# sourced from inside a function, as the tests do.
if (sys.nframe() == 0L) {
  convert_main()
}
