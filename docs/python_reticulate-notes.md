# Understanding r-reticulate + Python

- ~/.Renviron with "RETICULATE_PYTHON = {path}" can cause pain, try to avoid using it.
- Setting "RETICULATE_PYTHON" environment variable will **FORCE** reticulate to _always_ use that version (even if other virtual environments or python versions are specified). _Avoid it's use._

- Refer to `reticulate_python_pseudocode.R` to understand how reticulate installs and manages python. INCLUDES:
    1. How to identify where reticulate installs python and its install procedure; see `install_python()`.
    2. How reticulate identifies available python installs on a system, including those _not_ installed by reticulate; see `py_discover_config()`.
