#' {func_name}
#'
#' Equivalent to {module_name}${pyclass}
#' See `reticulate::py_help({module_name}${pyclass})` for help
#' @md
#' {if (params[1] != "") "@param" else ""} {params[1]} {if (params[1] != "") glue("Parameters for class constructor function. See `reticulate::py_help({module_name}${pyclass})` for help") else ""}
#' {if (params[2] != "") "@param" else ""} {params[2]} {if (params[2] != "") glue("Parameters for class methods. See `reticulate::py_help({module_name}${pyclass}$<method_name>)` for help") else ""}
#' @export
