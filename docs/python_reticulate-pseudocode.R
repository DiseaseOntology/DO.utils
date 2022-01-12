# Code & notes about how reticulate manages python

# How reticulate installs python
install_python()

    pyenv_find()
        pyenv_find_impl() # searches for installation of pyenv --> if found, returns path, else installs
            getOption("reticulate.pyenv", default = NULL) # 1. check user set global option
            Sys.which("pyenv") # 2. check system location
            Sys.getenv("PYENV_ROOT", unset = "~/.pyenv") # 3. check environment variables OR default location
            pyenv_root() # 4. checks user-specific data directory; if missing, creates r-reticulate/pyenv subdir 
                rappdirs::user_data_dir("r-reticulate")
                file.path(norm, "pyenv")
            penv_bootstrap() # INSTALLS pyenv, returns path to pyenv (REQUIRES git!!!)
                pyenv_bootstrap_windows() OR pyenv_bootstrap_unix()
                    # windows clones repo to pyenv_root() location
                    # unix copies download script to tmp dir, executes to install to pyenv_root() location
                    # NOTE: pyenv_install script exits with non-zero status EVEN when successful -->
                    #   requires install_python() to be executed twice OR step-wise execution of
                    #   reticulate:::pyenv_find(), followed by install_python()

    pyenv_install(version, force, pyenv = pyenv) # INSTALLS python using pyenv
        system2(pyenv, args)

    pyenv_python(version = version) # identifies python installation location
        # identify python install dir & executable (differs for Windows/Mac)
        # Mac
            # directory
            Sys.getenv("PYENV_ROOT", unset = "~/.pyenv") # check environment variables OR default location
            file.path(root, "versions", version) # location -> ~/.pyenv/versions/{version}
            # executable
            "bin/python"
        # Windows
            # directory
            file.path(pyenv_find(), "../../versions", version)
            # executable
            "python.exe"
    

# How reticulate identifies python installations
py_discover_config()
    # check for running? python and return info?

    # 1. returns config info for ONLY python already running
    main_process_python_info()
        ...

    # 2. returns config info for ONLY python specified by environment variable
    py_session_initialized_binary()
        Sys.getenv("PYTHON_SESSION_INITIALIZED", unset = NA)
        ...

    # 3. returns config info for ONLY python specified by environment variable 
    Sys.getenv("RETICULATE_PYTHON", unset = NA)
        ...

    # 4. returns config info for ONLY python specified by environment variable
    Sys.getenv("RETICULATE_PYTHON_ENV", unset = NA) 
        ...

    # 5. returns config info for ONLY python specified by pipfile (improved requirements.txt)
    pipenv_pipfile_path()
        # identify pipfile or try default (in current dir)
        getOption("reticulate.pipenv.pipfile")
        tryCatch(here::here("Pipfile"), error = function(e) "")
        # if Pipfile identified, get config info by running Pipenv with file
        python <- pipenv_python()
            envpath <- system("pipenv --venv", intern = TRUE)
            virtualenv_python(envpath)
        python_config(python, required_module, forced = "Pipfile")
        return(config)

    # 6. returns config info for ONLY python specified by reticulate global variable
    .globals$required_python_version
        ...

    # 7. returns config info for ONLY python specified by environment variable
    Sys.getenv("RETICULATE_PYTHON_FALLBACK", unset = NA)
        ...

    # 8. list versions of python used by reticulate
        python_versions <- reticulate_python_versions() # FINDS python when subdir of directory? or maybe when linked to R project? (not sure how)
            .globals$use_python_versions # where my ./pyenv is identified (doesn't work if ./pyenv doesn't exist)
                # .globals may be saved to disk (possibly in package data, in renv?) and loaded when pkg loaded?
        python_virtualenvs <- python_virtualenv_versions() # list reticulate-python virtual environments in WORKON_HOME or default virtual env location
            python_environments(virtualenv_root())
    
# ------- review of code completed to this point ----------
    if (!is.null(required_module)) {
        envnames <- c(required_module, paste0("r-", required_module), 
            use_environment)
        module_python_envs <- python_virtualenvs[python_virtualenvs$name %in% 
            envnames, ]
        python_versions <- c(python_versions, module_python_envs$python)
    }
    
    # 9. list versions of python in conda environments
    python_conda_versions() # I have not looked into the details of this...


    if (!is.null(required_module)) {
        envnames <- c(required_module, paste0("r-", required_module), 
            use_environment)
        module_python_envs <- python_condaenvs[python_condaenvs$name %in% 
            envnames, ]
        python_versions <- c(python_versions, module_python_envs$python)
    }
    
    miniconda <- miniconda_conda()
    
    if (!file.exists(miniconda)) {
        can_install_miniconda <- is_interactive() && length(python_versions) == 
            0 && miniconda_enabled() && miniconda_installable()
        if (can_install_miniconda) 
            miniconda_install_prompt()
    }
    if (file.exists(miniconda)) {
        envpath <- miniconda_python_envpath()
        if (!file.exists(envpath)) {
            python <- miniconda_python_package()
            conda_create(envpath, packages = c(python, "numpy"), 
                conda = miniconda)
        }
        miniconda_python <- conda_python(envpath, conda = miniconda)
        config <- python_config(miniconda_python, NULL, miniconda_python)
        return(config)
    }
    if (conda_installed() && nrow(conda_list()) == 0) {
        python <- miniconda_python_package()
        conda_create("r-reticulate", packages = c(python, "numpy"), 
            conda = conda_binary())
        python_condaenvs <- python_conda_versions()
        r_reticulate_python_envs <- python_condaenvs[python_condaenvs$name == 
            "r-reticulate", ]
        python_versions <- c(python_versions, r_reticulate_python_envs$python)
    }
    python_envs <- rbind(python_virtualenvs, python_condaenvs)
    python <- as.character(Sys.which("python3"))
    if (nzchar(python)) 
        python_versions <- c(python_versions, python)
    
    # 10. List standard python locations
        # A. on Windows
    if (is_windows()) {
        python_versions <- c(python_versions, py_versions_windows()$executable_path)
    }
    else {
        # B. on UNIX
        python_versions <- c(python_versions, "/usr/bin/python3", 
            "/usr/local/bin/python3", "/opt/python/bin/python3", 
            "/opt/local/python/bin/python3", "/usr/bin/python", 
            "/usr/local/bin/python", "/opt/python/bin/python", 
            "/opt/local/python/bin/python", path.expand("~/anaconda3/bin/python"), 
            path.expand("~/anaconda/bin/python"))
    }
    python_versions <- c(python_versions, python_envs$python)
    python_versions <- unique(python_versions)
    if (length(python_versions) > 0) 
        python_versions <- python_versions[file.exists(python_versions)]
    if (is_windows()) {
        info <- suppressWarnings(file.info(python_versions))
        size <- ifelse(is.na(info$size), 0, info$size)
        python_versions <- python_versions[size != 0]
    }
    valid_python_versions <- c()
    for (python_version in python_versions) {
        config <- python_config(python_version, required_module, 
            python_versions)
        has_python_gte_27 <- as.numeric_version(config$version) >= 
            "2.7"
        has_compatible_arch <- !is_incompatible_arch(config)
        has_preferred_numpy <- !is.null(config$numpy) && config$numpy$version >= 
            "1.6"
        if (has_compatible_arch && has_preferred_numpy) 
            valid_python_versions <- c(valid_python_versions, 
                python_version)
        has_required_module <- is.null(config$required_module) || 
            !is.null(config$required_module_path)
        if (has_python_gte_27 && has_compatible_arch && has_preferred_numpy && 
            has_required_module) 
            return(config)
    }
    if (length(valid_python_versions) > 0) 
        return(python_config(valid_python_versions[[1]], required_module, 
            python_versions))
    else if (length(python_versions) > 0) 
        return(python_config(python_versions[[1]], required_module, 
            python_versions))
    else return(NULL)
}
