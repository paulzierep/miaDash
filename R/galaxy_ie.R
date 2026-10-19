# Galaxy Interactive Tools glue for miaDash.
#
# This file is the single implementation of the Galaxy-facing behaviour:
#
#   1. "Send to Galaxy" buttons next to the download button the app offers
#   2. uploading a file back into the current history, non-blocking
#
# It is adapted from the shared implementation used by the other microbiome
# Galaxy tools (see R/galaxy_ie.R in paulzierep/MicrobiomeProfiler,
# paulzierep/CMiNetShinyAPP and paulzierep/MetaDAVis). Unlike those apps,
# miaDash gets no import-from-history picker: every miaDash input is a Shiny
# fileInput(), which cannot be filled programmatically, so importing from the
# history is done through the raw tables the tool XML stages at launch (see
# R/galaxy_input.R).
#
# Nothing here changes what a download produces: the upload path re-invokes the
# content function that downloadHandler() was given, so the file that is sent is
# byte-for-byte the file a click on the download button would have written.
#
# galaxy_ie_helpers (https://github.com/bgruening/galaxy_ie_helpers) does the
# talking to Galaxy. It knows how to reach the instance from inside a tool
# container and which history the session belongs to. The tool XML exports:
#
#   HISTORY_ID       the history this interactive session belongs to
#   GALAXY_URL       the instance URL ($DOCKER_HOST is substituted in as well)
#   GALAXY_WEB_PORT  fallback port when that URL does not answer
#   API_KEY          a key that may write into that history
#
# Everything degrades to a no-op when those are absent, so a plain `docker run`
# or `shiny::runApp()` behaves exactly as it did before.

# --- the app that is using this file ---------------------------------------
#
# `prefix` is the app name used for log lines and notification text, and for
# the CSS/JS ids, so two of these apps can never collide in one browser page.
# `output_env` is the env var holding the Galaxy job's output directory.
# Declare it with galaxy_ie_app() at the top of the app's server function, so
# there is nothing to remember to assign.
galaxy_ie_app <- function(prefix, output_env) {
  galaxy_ie$app <- list(
    prefix = prefix,
    output_env = output_env
  )
  invisible(galaxy_ie$app)
}

# The default configuration. An app that loads this file without calling
# galaxy_ie_app() still gets a working "Send to Galaxy".
galaxy_ie <- new.env(parent = emptyenv())
galaxy_ie$app <- galaxy_ie_app(prefix = "miadash", output_env = "MIADASH_OUTPUT_DIR")

# --- the upload command ------------------------------------------------------
#
# galaxy_ie_helpers is installed into its own virtualenv in the image. Use its
# supported console command; the Python package has no __main__ module.
galaxy_ie_put <- function() {
  candidates <- c(
    Sys.getenv("GALAXY_IE_PUT", unset = ""),
    "/opt/galaxy_ie_helpers/bin/put",
    "put"
  )
  for (candidate in candidates) {
    if (!nzchar(candidate)) next
    path <- if (grepl("/", candidate, fixed = TRUE)) candidate else Sys.which(candidate)
    if (nzchar(path) && file.access(path, mode = 1L) == 0L) {
      return(path)
    }
  }
  NA_character_
}

galaxy_ie_history <- function() Sys.getenv("HISTORY_ID", unset = "")
galaxy_ie_key <- function() Sys.getenv("API_KEY", unset = "")
galaxy_ie_output_dir <- function() Sys.getenv(galaxy_ie$app$output_env, unset = "")

galaxy_ie_output_path <- function(name) {
  output_dir <- galaxy_ie_output_dir()
  if (!nzchar(output_dir)) {
    return(NA_character_)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(output_dir, basename(name))
}

# Never logs the API key itself, only whether one is present.
galaxy_ie_log <- function(...) {
  line <- paste0(
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), " ",
    galaxy_ie$app$prefix, " ", paste0(..., collapse = "")
  )
  message(line)
  output_dir <- galaxy_ie_output_dir()
  if (nzchar(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    cat(line, "\n", file = file.path(output_dir, "galaxy_upload.log"), append = TRUE)
  }
}

# TRUE when this session can upload back to Galaxy. Checked in the UI as well, so
# a plain `docker run` offers no upload buttons that cannot work.
galaxy_ie_ready <- function() {
  nzchar(galaxy_ie_history()) &&
    nzchar(galaxy_ie_key()) &&
    !is.na(galaxy_ie_put())
}

galaxy_ie_not_ready_message <- function() {
  paste0(
    "This session is not running as a Galaxy interactive tool ",
    "(no history id or API key), so files cannot be sent back to Galaxy."
  )
}

# --- uploading ---------------------------------------------------------------

# Write `path` into the current history under `name`. Returns a list with ok and
# message so the caller can tell the user what happened.
galaxy_ie_send <- function(path, name = basename(path), filetype = "auto") {
  if (!file.exists(path)) {
    return(list(ok = FALSE, message = paste0("there is no file to send at ", path)))
  }
  put <- galaxy_ie_put()
  if (is.na(put)) {
    return(list(
      ok = FALSE,
      message = "galaxy_ie_helpers is not installed, so nothing can be sent back to Galaxy"
    ))
  }
  if (!nzchar(galaxy_ie_history()) || !nzchar(galaxy_ie_key())) {
    return(list(ok = FALSE, message = galaxy_ie_not_ready_message()))
  }

  galaxy_ie_log(
    "upload start name=", name,
    " path=", normalizePath(path, mustWork = FALSE),
    " bytes=", file.info(path)$size,
    " filetype=", filetype,
    " history=", galaxy_ie_history(),
    " url=", Sys.getenv("GALAXY_URL", unset = "<unset>"),
    " put=", put,
    " api_key_present=", nzchar(galaxy_ie_key())
  )

  output <- tryCatch(
    system2(
      put,
      c(
        "-p", shQuote(path),
        "-t", shQuote(filetype),
        "--history-id", shQuote(galaxy_ie_history())
      ),
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) structure(character(), status = 1L, error = conditionMessage(e))
  )

  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  error_detail <- attr(output, "error")
  output_text <- paste(as.character(output), collapse = " | ")
  galaxy_ie_log(
    "upload finish name=", name,
    " exit_status=", status,
    if (!is.null(error_detail)) paste0(" error=", error_detail) else "",
    if (nzchar(output_text)) paste0(" output=", output_text) else " output=<empty>"
  )
  if (status != 0L) {
    detail <- paste(utils::tail(as.character(output), 20L), collapse = "\n")
    return(list(
      ok = FALSE,
      message = paste0(
        "the upload failed with exit code ", status,
        if (nzchar(detail)) paste0(":\n", detail)
      )
    ))
  }
  list(ok = TRUE, message = paste0(name, " was added to the Galaxy history"))
}

# The blocking helper above waits for the API call, which would freeze the Shiny
# session on a slow Galaxy. Run it in a background Rscript instead.
#
# The child deliberately does NOT source this file: a Shiny app installed as an
# R package (which is what miaDash is in the container) has no source tree on
# disk at runtime, so a child that re-sourced the helper would fail exactly where
# the upload matters most. Everything the child needs is passed in as arguments,
# and the environment (HISTORY_ID, API_KEY, ...) is inherited from the parent.
galaxy_ie_send_async <- function(path, name = basename(path), filetype = "auto") {
  rscript <- file.path(R.home("bin"), "Rscript")
  if (!nzchar(name)) {
    return(invisible(FALSE))
  }
  put <- galaxy_ie_put()
  if (is.na(put)) {
    galaxy_ie_log("async upload not possible, put not found; uploading synchronously name=", name)
    return(invisible(galaxy_ie_send(path, name, filetype)$ok))
  }

  # Exactly the same call galaxy_ie_send() makes: `put` derives the dataset name
  # from the file's basename, so the path has to be the saved copy in the output
  # directory (which is named after the download) rather than a temp file.
  expression <- sprintf(
    "invisible(system2(%s, c(\"-p\", %s, \"-t\", %s, \"--history-id\", %s), stdout = FALSE, stderr = FALSE))",
    encodeString(put, quote = "\""),
    encodeString(normalizePath(path, mustWork = FALSE), quote = "\""),
    encodeString(filetype, quote = "\""),
    encodeString(galaxy_ie_history(), quote = "\"")
  )
  system2(
    rscript,
    c("--vanilla", "-e", shQuote(expression)),
    stdout = FALSE,
    stderr = FALSE,
    wait = FALSE
  )
  galaxy_ie_log("async upload started name=", name, " path=", path)
  invisible(TRUE)
}

# --- the download registry ---------------------------------------------------
#
# A download button cannot be reused for an upload: its content function is only
# ever called while a download is in flight. So every handler is registered on
# the way through, which is what lets the generic "Send to Galaxy" handler below
# produce the file again without a second implementation of any export format.
galaxy_ie_registry <- function() {
  new.env(parent = emptyenv())
}

galaxy_ie_register <- function(registry, id, filename, content, contentType = "application/octet-stream") {
  assign(
    id,
    list(filename = filename, content = content, contentType = contentType),
    envir = registry
  )
}

# downloadHandler() itself, plus the registration. Defined here rather than at
# the call sites so the call sites keep reading exactly as they did: the
# arguments and the return value are downloadHandler()'s.
galaxy_ie_download_with <- function(registry, id, filename, content, contentType = "application/octet-stream") {
  galaxy_ie_register(registry, id, filename, content, contentType)
  shiny::downloadHandler(filename = filename, content = content, contentType = contentType)
}

# The file name a download button would use, as a plain string. Galaxy names the
# dataset after it, so it has to survive a path and keep its extension.
galaxy_ie_download_name <- function(handler, id) {
  name <- tryCatch(
    if (is.function(handler$filename)) handler$filename() else as.character(handler$filename),
    error = function(e) NULL
  )
  if (is.null(name) || length(name) != 1L || !nzchar(name)) {
    name <- paste0(id, ".txt")
  }
  name <- basename(as.character(name))
  if (!grepl("\\.[A-Za-z0-9]+$", name)) {
    name <- paste0(name, ".txt")
  }
  name
}

# --- the "Send to Galaxy" UI ------------------------------------------------
#
# One small piece of javascript, added to the page once, instead of a second
# button next to every download button: every download link shiny renders is an
# <a class="shiny-download-link" id="<output id>">, so the companion buttons can
# be injected wherever they appear - including the one inside the export modal,
# which is only rendered once the user asks for it.
#
# `ids` is the allow-list of download output ids that get a button, i.e. the
# keys of the download registry. It has to be an allow-list rather than a
# deny-list because iSEE's own machinery also renders download links
# (iSEE_INTERNAL_memory_export and friends), which the injected button must not
# attach to: there is nothing sensible to upload for those.
#
# The button asks the server for the file by output id
# (input$<prefix>_galaxy_send), which regenerates it with the registered content
# function and uploads it.
galaxy_ie_send_ui <- function(ids = NULL) {
  if (!galaxy_ie_ready()) {
    # NULL outside Galaxy, so a plain `docker run` does not offer a button that
    # cannot work.
    return(NULL)
  }
  if (!is.null(ids) && !length(ids)) {
    # Nothing registered, so nothing to attach to.
    return(NULL)
  }
  shiny::tags$script(shiny::HTML(galaxy_ie_send_js(ids)))
}

galaxy_ie_send_js <- function(ids = NULL) {
  prefix <- galaxy_ie$app$prefix
  # The prefix is interpolated into the JS strings below, so keep it to
  # identifier characters only.
  prefix <- gsub("[^A-Za-z0-9_]", "_", prefix)
  # NULL becomes JS null, which means "every download link"; otherwise a JS
  # array of the registered output ids. Output ids are plain identifiers, so
  # quoting them here cannot produce invalid javascript.
  ids_js <- if (is.null(ids)) {
    "null"
  } else {
    paste0("[", paste(sprintf('"%s"', ids), collapse = ", "), "]")
  }
  sprintf("
(function () {
  var inputName = '%1$s_galaxy_send';
  var allowed = %2$s;
  var pending = {};

  function idOf(anchor) {
    if (anchor.id) return anchor.id;
    var href = anchor.getAttribute('href') || '';
    var match = /[?&]w2=([^&]+)/.exec(href);
    return match ? decodeURIComponent(match[1]) : null;
  }

  function isAllowed(id) {
    if (allowed === null) return true;
    return allowed.indexOf(id) !== -1;
  }

  function buttonFor(anchor, id) {
    var button = document.createElement('button');
    button.id = '%1$s_gx_send_' + id;
    button.type = 'button';
    button.className = 'btn btn-default btn-sm %1$s-gx-send';
    button.style.marginLeft = '6px';
    button.style.whiteSpace = 'nowrap';
    button.textContent = 'Send to Galaxy';
    button.title = 'Add this file to the Galaxy history of this session';
    button.addEventListener('click', function (event) {
      event.preventDefault();
      if (pending[id]) return;
      pending[id] = true;
      button.disabled = true;
      button.textContent = 'Sending...';
      Shiny.setInputValue(inputName, id, { priority: 'event' });
    });
    return button;
  }

  function release(id) {
    delete pending[id];
    var button = document.getElementById('%1$s_gx_send_' + id);
    if (!button) return;
    button.disabled = false;
    button.textContent = 'Send to Galaxy';
  }

  function inject() {
    var links = document.querySelectorAll('a.shiny-download-link');
    for (var i = 0; i < links.length; i++) {
      var anchor = links[i];
      var id = idOf(anchor);
      if (!id) continue;
      if (!isAllowed(id)) continue;
      var key = '%1$s_gx_send_' + id;
      if (document.getElementById(key)) continue;
      anchor.parentNode.insertBefore(buttonFor(anchor, id), anchor.nextSibling);
    }
  }

  Shiny.addCustomMessageHandler('%1$s_galaxy_send_done', function (id) {
    release(id);
  });

  $(document).on('shiny:connected', function () { window.setTimeout(inject, 250); });
  $(document).on('shiny:value', function () { window.setTimeout(inject, 0); });
  $(document).on('shiny:bound', function () { window.setTimeout(inject, 0); });

  // The export modal's download button is rendered and removed as the modal is
  // opened and closed, so new links have to be picked up while the session runs.
  var observer = new MutationObserver(function () { window.setTimeout(inject, 0); });
  observer.observe(document.documentElement, { childList: true, subtree: true });
})();
", prefix, ids_js)
}

# --- the server side of "Send to Galaxy" ------------------------------------
#
# Wires up the registry and the observer. Returns the registry so the app can
# register its own downloadHandler()s with
# galaxy_ie_download_with(registry, id, ...).
#
# `label` is what the collection is called in the user-facing messages.
galaxy_ie_send_server <- function(input, output, session, label = "results") {
  registry <- galaxy_ie_registry()
  observeEvent(input[[paste0(galaxy_ie$app$prefix, "_galaxy_send")]], {
    id <- input[[paste0(galaxy_ie$app$prefix, "_galaxy_send")]]
    # Always let the browser re-enable its button, whatever happens below.
    on.exit(
      session$sendCustomMessage(paste0(galaxy_ie$app$prefix, "_galaxy_send_done"), id),
      add = TRUE
    )

    handler <- get0(id, envir = registry, inherits = FALSE)
    if (is.null(handler)) {
      showNotification(
        paste0("There is nothing to send for ", id, "."),
        type = "warning"
      )
      return()
    }

    name <- galaxy_ie_download_name(handler, id)
    # Write directly into the Galaxy job's discovered output directory. This
    # keeps the result available when the interactive job ends even if the API
    # upload below cannot reach Galaxy.
    path <- galaxy_ie_output_path(name)
    if (is.na(path)) {
      path <- file.path(tempdir(), paste0("galaxy_ie_", session$token, "_", name))
    }
    produced <- tryCatch(
      {
        handler$content(path)
        TRUE
      },
      error = function(e) {
        showNotification(
          paste0("The file could not be produced: ", conditionMessage(e)),
          type = "error",
          duration = 15
        )
        FALSE
      }
    )
    if (!produced || !file.exists(path)) {
      return()
    }

    persisted <- identical(
      dirname(path),
      normalizePath(galaxy_ie_output_dir(), mustWork = FALSE)
    )
    if (persisted) {
      upload_started <- tryCatch(
        {
          galaxy_ie_send_async(path, name)
          TRUE
        },
        error = function(e) {
          galaxy_ie_log("could not start direct upload name=", name, " error=", conditionMessage(e))
          FALSE
        }
      )
      galaxy_ie_log(
        "saved output name=", name,
        " path=", normalizePath(path, mustWork = FALSE),
        " bytes=", file.info(path)$size,
        " direct_upload=", if (upload_started) "started asynchronously" else "not started"
      )
      result <- list(
        ok = TRUE,
        message = paste0(
          "Saved ", name, ". Direct history upload was started in the background; ",
          "the file will also appear in ", label, " when this interactive job ends."
        )
      )
    } else {
      # Not running as a Galaxy interactive tool: there is no job directory to
      # discover, so send it and let the API be the only destination.
      result <- galaxy_ie_send(path, name)
    }

    showNotification(
      result$message,
      type = if (result$ok) "message" else "error",
      duration = if (result$ok) 8 else 15
    )
  })

  registry
}
