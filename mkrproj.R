#!/usr/bin/env Rscript

(function(...) {
  
  # ---- Variable Definitions ----
  # Set any of these values as environment variables. The values below are defaults
  # If of the format "ask:PROMPT:VALUE", value will be set interactively
  vars <- list(
    #MKPROJ_SOURCE="https://github.com/ecohealthalliance/mkrproj-template/", #URL to ZIP, ZIP file, or unzipped directory (can be ".")
    MKRPROJ_SOURCE=".",
    MKRPROJ_PROJECT_NAME="xxxxxxxxxxxxxx", #ask:Project/directory name? (no value for current directory):",
    MKRPROJ_GIT="yes",
    MKRPROJ_GITHUB="yes", #ask:Initiate GitHub repository?:yes",
    MKRPROJ_GITHUB_ORG="", #ask:What GitHub organization? (no value for personal account):",
    MKRPROJ_GITHUB_PRIVATE="yes",
    MKRPROJ_GITCRYPT="yes",#ask:Initiate git-crypt encryption?:yes",
    MKRPROJ_GPG_USER="",#ask:What GPG key should encrypt the repo (no value for default git email):",
    MKRPROJ_PARENT_DIR="~/projects", #ask:Parent directory for project? (no value for current directory):",
    MKRPROJ_SHORTCUTS="no",# "ask:Install default RStudio shortcuts?:yes",
    MKRPROJ_VERBOSE="FALSE",
    MKRPROJ_PLACEHOLDER="xxxxxxxxxxxxxx",
    MKRPROJ_OPEN_RSTUDIO="yes",
    MKRPROJ_REMOVE="mkrproj.R;mkrproj.sh;.github/workflows/make-shellscript.yml;.github/workflows/update-packages.yml;" #semicolon-separated files in the template that should be removed
  )
  
  # ---- Utility Functions ----
  
  get_os <- function() {
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
      os <- sysinf['sysname']
      if (os == 'Darwin')
        os <- "osx"
    } else { ## mystery machine
      os <- .Platform$OS.type
      if (grepl("^darwin", R.version$os))
        os <- "osx"
      if (grepl("linux-gnu", R.version$os))
        os <- "linux"
    }
    tolower(os)
  }
  
  #' Read input interactively even in non-interactive sessions
  read_input <- function(prompt) {
    if (interactive()) {
      out <- readline(prompt)
    } else {
      cat(prompt);
      out <- readLines("stdin",n=1);
      cat( "\n" )
    }
    if(out %in% c('q', 'Q', 'quit')) stop("Aborted")
    out
  }
  
  #' Convert free yes/no responses to logical
  response_to_logical <- function(response, stop = FALSE) {
    if (grepl("^\\s*(y|yes|t|true)\\s*$", response, ignore.case = TRUE)) {
      return(TRUE)
    } else if(grepl("^\\s*(n|no|f|false)\\s*$", response, ignore.case = TRUE)) {
      return(FALSE)
    } else if(stop) {
      stop("'", response, "' not recognizable as TRUE or FALSE")
    } else {
      return(response)
    }
  }
  
  #' Set variables based on env vars and interactive prompts
  collect_vars <- function(vars) {
    for (v in names(vars)) {
      vars[[v]] <- Sys.getenv(v, vars[[v]])
      if(grepl("^ask:[^:]+:.*$", vars[[v]])) {
        prompt <- gsub("^ask:([^:]+):.*$", "\\1", vars[[v]])
        default <- gsub("^ask:[^:]+:(.*)$", "\\1", vars[[v]])
        fullprompt <- paste0(prompt, " (default = '", default, "'): ")
        vars[[v]] <- read_input(fullprompt)
      }
      vars[[v]] <- response_to_logical(vars[[v]])
      
    }
    vars
  }
  
  # ---- Workflow Functions ----
  
  #' Fetch the template and return the temporary directory
  get_template <- function(source, verbose = TRUE) {
    temp_directory <- tempfile(pattern = "dir")
    dir.create(temp_directory)
    
    if (grepl("^((http|ftp)s?|sftp)://", source)) {
      if (verbose) message("• Downloding template...\n")
      zipfile <- tempfile(fileext = ".zip")
      download.file(source, quiet = TRUE, destfile = zipfile)
      source <- zipfile
    }
    
    if (file.exists(source) && tools::file_ext(source) == "zip") {
      unzip(source, exdir = temp_directory)
      dirs <- list.dirs(temp_directory, full.names = TRUE, recursive = FALSE)
      if(length(dirs) == 1) file.rename(dirs, temp_directory)
    } else if (dir.exists(source)) {
      files <- list.files(source, all.files = TRUE, include.dirs = TRUE, full.names = TRUE)
      files <- files[!basename(files) %in% c(".git", ".Rproj.user", "_targets", ".Rhistory", ".DS_Store", "renv", ".", "..")]
      for (f in files) {
        file.copy(f, temp_directory, overwrite = TRUE, recursive = TRUE) 
      }
      if(dir.exists(file.path(source, "renv"))) {
        dir.create(file.path(temp_directory, "renv"))
        file.copy(file.path(source, "renv", c("activate.R", ".gitignore", "settings.dcf")), file.path(temp_directory, "renv"))
      }
    } else {
      stop("Template not found or invalid. It must be a ZIP, directory, or URL of ZIP")
    }
    
    temp_directory
  }
  
  #' Replace filenames and text inside files with 'placeholder' with the project name
  rename_placeholders <- function(project_name, placeholder, verbose = TRUE, dir = getwd()) {
    if (verbose) message("• Modifying template to use '", project_name, "' as name...")
    projfiles <- list.files(dir, all.files = TRUE, full.names = TRUE, recursive = TRUE)
    for (pfile in projfiles) {
      # Rename any files with the template name to the project name
      if(grepl(placeholder, pfile)) {
        new_name <- gsub(placeholder, project_name, pfile, fixed = TRUE)
        file.rename(pfile, new_name)
        pfile <- new_name
      }
      # Replace any text of placeholders with the project name
      lines <- readLines(pfile, warn = FALSE)
      updated_lines <- try(gsub(placeholder, project_name, lines, fixed = TRUE), silent = TRUE)
      cat(paste(updated_lines, collapse = "\n"), file = pfile)
    }
    
    # Any files ending with `-template.XXX`, or `-template` will have that part lopped off
    templates <- list.files(dir, "-template(\\.\\w+)?$", all.files = TRUE, full.names = TRUE)
    file.rename(templates, gsub("-template" ,"", templates))
    
    return(0)
  }
  
  bootstrap_packages <- function(verbose = TRUE) {
    if(verbose) message("• Installing packages...")
    txt <- capture.output(renv::restore(prompt = FALSE))
    return(0)
  }
  
  run_pipeline <- function(verbose = TRUE) {
    if(!file.exists("_targets.R")) {
      message("No _targets.R in template, skipping pipeline.")
      return(1)
    }
    message("• Testing `targets` pipeline...")
    targets::tar_make(reporter = ifelse(verbose, "silent", "verbose"))
    
    return(0)
  }
  
  setup_git <- function(verbose = TRUE, dir = getwd()) {
    if(verbose) message("• Initiating git repository...")
    gert::git_init(dir)
    gert::git_add(".", repo = dir)
    gert::git_commit("Initial commit of project template", repo = dir)
    
    return(0)
  }
  
  setup_crypt <- function(gpg_user, verbose = TRUE) {
    
    if(verbose) message("• Setting up encryption with git-crypt...")
    git_crypt <- sys::exec_wait("git-crypt", "version", std_out = FALSE) == 0
    if(!git_crypt) {
      message("git-crypt not found, project will not be encrypted. Get it at <https://www.agwa.name/projects/git-crypt/>")
      return(FALSE)
    }
    
    if(is.null(gpg_user) || gpg_user == "") gpg_user <- whoami::email_address()
    gpg_id <- gpg::gpg_list_keys(search = gsub("('|\\\")", "", gpg_user))
    
    
    sys::exec_wait("git-crypt", "init", std_out = FALSE, std_err = FALSE)
    if(verbose) message("  • Adding ", gpg_id$email, "'s user key...")
    sys::exec_wait("git-crypt", c("add-gpg-user", gpg_id$fingerprint), std_out = FALSE)
    
    if(verbose) {
      encrypted <- character(0)
      if(file.exists(".gitattributes")) {
        gitattr <- stringi::stri_subset_fixed(
          readLines(".gitattributes", warn = FALSE),
          "filter=git-crypt diff=git-crypt")
        encrypted <- stringi::stri_extract_first_regex(gitattr, "^[^\\s]+")
      }
      if(length(encrypted)) {
        message("  • Paths to be encrypted: ", paste(encrypted, collapse = ", "))
      } else {
        message("  • No paths set to be encrypted, modify `.gitattributes` to do so (see  <https://www.agwa.name/projects/git-crypt/>)")
      }
    }
    
    return(TRUE)
  }
  
  setup_github <- function(project_name, github_org, private = TRUE, verbose = TRUE) {
    message("• Setting up GitHub repository...")
    if(github_org == "") github_org <- NULL
    if(gh::gh_token() == "") {
      message("  • No GITHUB_PAT env var found, GitHub repo will not be created.")
      return(FALSE)
    }
    if (is.null(github_org)) {
      create <- try(gh::gh("POST /user/repos", name = project_name), silent = TRUE)
    }
    else {
      create <- try(gh::gh("POST /orgs/{org}/repos", org = github_org, 
                           name = repo_name), silent = TRUE)
    }
    if(inherits(create, "try-error")) {
      message("  • Error communicating with GitHub, repo will not be created.")
      message(paste0("    ", strsplit(create, "\n")[[1]], collapse = "\n"))
      return(FALSE)
    }
    #origin_url <- switch(usethis::git_protocol(), https = create$clone_url, 
    #                     ssh = create$ssh_url)
    #usethis::use_git_remote("origin", origin_url, overwrite = TRUE)
    gert::git_remote_add(url = create$clone_url, name = "origin")
    gert::git_push(remote = "origin", set_upstream = TRUE, 
                   verbose = verbose)
    message("  • Repo created at ", create$html_url)
    return(create)
  }
  
  setup_secrets <- function(repo, owner) {
    message("• Setting up GitHub Actions secrets...")
    public_key <- gh::gh("/repos/{owner}/{repo}/actions/secrets/public-key", repo = project_name)
    tkey <- tempfile()
    sys::exec_wait("git-crypt", c("export-key", tkey), std_out = FALSE, std_err = FALSE)
    GIT_CRYPT_KEY64 <- base64enc::base64encode(tkey)
    file.remove(tkey)
  }
  
  cleanup <- function(remove, dir = getwd()) {
    message("• Cleaning up packages and files...")
    unlink(file.path(dir, strsplit(remove, ";")[[1]]), recursive = TRUE)
    txt <- capture.output({
      renv::clean(dir, actions = c("package.locks", "library.tempdirs", "system.library", "unused.packages"), prompt = FALSE)
      renv::snapshot(type = "implicit", prompt = FALSE)
    })
  }
  
  move_directory <- function(tmp_dir, parent_dir, project_name) {
    path <- file.path(parent_dir, project_name)
    message("• Moving project to ", path, "...")
    if(!dir.exists(parent_dir)) {
      dir.create(parent_dir, showWarnings = FALSE, recursive = TRUE)
    }
    fs::dir_copy(tmp_dir, path, overwrite = FALSE)
    return(0)
  }
  
  # ---- Setup ----
  
  # Minimize impact on working environment
  wd <- getwd()
  libs <- .libPaths()
  op <- options()
  on.exit({
    .libPaths(libs)
    setwd(wd)
    options(op)
  })
  
  vars <- collect_vars(vars)
  list2env(
    stats::setNames(vars, gsub("MKRPROJ_", "", names(vars), fixed = TRUE)), 
    environment())
  
  # ---- Workflow ----
  
  
  tmp_dir <- get_template(SOURCE)
  
  setwd(tmp_dir)
  rename_placeholders(project_name = PROJECT_NAME, placeholder = PLACEHOLDER)
  options(
    renv.config.startup.quiet = TRUE,
    renv.config.synchronized.check = FALSE,
    renv.config.auto.snapshot = FALSE)
  
  message("• Setting up renv...")
  suppressMessages({
    if(file.exists(".Rprofile")) {
      source(".Rprofile")
    } else if (file.exists("renv/activate.R")) {
      source("renv/activate.R")
    }
  })
  
  bootstrap_packages()
  run_pipeline(verbose = TRUE)
  if (GIT) setup_git()
  if (GITCRYPT) gitcrypt_success <- setup_crypt(gpg_user = GPG_USER)
  if (GITHUB) {
    github_repo <- setup_github(project_name = PROJECT_NAME,
                 github_org = GITHUB_ORG,
                 private = GITHUB_PRIVATE)
  }
  browser()
  if (GITCRYPT && gitcrypt_success && GITHUB && inherits(github_repo, "gh_response")) setup_secrets(github_repo)
  cleanup(remove = REMOVE)
  
  setwd(wd)
  move_directory(tmp_dir, PARENT_DIR, PROJECT_NAME)
  
  
  
})()

