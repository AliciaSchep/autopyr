#' {func_name}
#'
#' Equivalent to {module_name}${pyclass}
#' See `reticulate::py_help({module_name}${pyclass})` for help
#' @md
#' {if (length(params) != 0 && params != "") "@param" else ""} {params[1]} 
#' {if (length(params) != 0 && params != "") glue("Parameters for class constructor function or methods. See `reticulate::py_help({module_name}${pyclass})` or `reticulate::py_help({module_name}${pyclass}$<method_name>)` for help") else ""}
#' @export
