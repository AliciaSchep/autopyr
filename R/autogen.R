Undefined <- NULL
inspect <- NULL

#' @import reticulate glue purrr
is_py_class <- function(pyattr){
  py_str(py_get_attr(pyattr, "__class__")) == "<class 'type'>"
}

is_py_method <- function(pyattr){
  py_str(py_get_attr(pyattr, "__class__")) == "<class 'function'>"
}


get_py_classes <- function(pymodule) {
  attributes <- py_list_attributes(pymodule)
  classes <- attributes[purrr::map_lgl(attributes, ~is_py_class(py_get_attr(pymodule,.)))]
  classes
}

get_py_methods <- function(pyclass, pymodule) {
  x <- py_get_attr(pymodule, pyclass)
  stopifnot(is_py_class(x))
  attributes <- py_list_attributes(x)
  attributes <- attributes[!grepl("^_",attributes)]
  methods <- attributes[purrr::map_lgl(attributes, ~is_py_method(py_get_attr(x,.)))]
  methods
}

get_arg_spec <- function(pyfun){
  py_to_r(inspect$getfullargspec(pyfun))
}

add_comma_plus <- function(x, y){
  if (x == ""){
    return(y)
  } else{
    return(paste(x,y,sep = ", "))
  }
}

add_comma_if <- function(x, y){
  if (y == ""){
    return(x)
  } else{
    return(paste(x,y,sep = ", "))
  }
}

sanitize_arg <- function(x){
  paste0('`',x,'`')
}

write_method_arglist <- function(pyfun){
  args <- tryCatch(get_arg_spec(pyfun), error = function(e) NULL)
  if (is.null(args)){
    warning(glue::glue("No signature found for {pyfun}"))
    return(list(inner = "", outer = ""))
  }
  len_defaults <- length(args$defaults)
  len_args <- length(args$args)
  mandatory <- len_args - len_defaults
  out = ""
  out2 = ""
  if (mandatory >= 2) {
    out = paste(sanitize_arg(args$args[2:mandatory]), sep = ",")
    out2 = paste(paste0("r_to_py(",sanitize_arg(args$args[2:mandatory]),")"), sep = ",")}
  if (mandatory < len_args) {
    out <- add_comma_plus(out,
                          paste(sanitize_arg(args$args[(mandatory + 1):len_args]),
                                lapply(args$defaults, as.character),
                                sep = " = ", collapse = ", "))
    out2 <- add_comma_plus(out2,
                          paste(sanitize_arg(args$args[(mandatory + 1):len_args]),
                                paste0("r_to_py(",sanitize_arg(args$args[(mandatory + 1):len_args]),")"),
                                sep = " = ", collapse = ", "))
  }
  if (!is.null(args$varargs) || !is.null(args$varkw)){
    out = add_comma_plus(out, "...")
    out2 = add_comma_plus(out2, "...")
  }
  return(list(inner = out2, outer = out))
}

write_constructor <- function(pyclass, module_name, pymodule, prefix ){
  arg_lists <- write_method_arglist(py_get_attr(pymodule,pyclass))
  func_name <- glue::glue("{prefix}_{pyclass}")
  glue::glue(
    "#' @export
    {func_name} <- function ({arg_lists$outer}) {{
    {module_name}$`{pyclass}`({arg_lists$inner})
    }}")
}

write_s3_method <- function(pymethod, pyclass, pymodule, prefix, module_abbreviation){
  module_name <- py_get_attr(py_get_attr(pymodule,pyclass),"__module__")
  method_name <- glue::glue("{prefix}_{pymethod}.{module_name}.{pyclass}")
  arg_lists <- write_method_arglist(py_get_attr(py_get_attr(pymodule,pyclass),pymethod))
  #s3_doc <- readr::read_file(system.file("templates/s3_method.R", package = "autoreticulate"))
  outer_arg <- add_comma_if("pyr_object", arg_lists$outer)
  glue::glue(
    "#' @export
    {method_name} <- function ({outer_arg}) {{
    out <- pyr_object$`{pymethod}`({arg_lists$inner})
    out
    }}")
}

write_s3_methods <- function(pymethods, pyclass, pymodule, prefix, module_abbreviation){
  purrr::map_chr(pymethods, write_s3_method, pyclass = pyclass,
             pymodule = pymodule, prefix = prefix,
             module_abbreviation = module_abbreviation)
}

write_s3_generic <- function(method, prefix) {
  glue::glue(
    "#' @export
    {prefix}_{method} <- function (pyr_object, ...) {{
    UseMethod('{prefix}_{method}', pyr_object)
    }}")
}


#' Generate an R api based on a python module
#'
#' @param pymodule A python module, imported via reticulate
#'
#' @param prefix prefix to give all functions and methods for generated api
#'
#' @return A string with function and method definitions
#'
#' @export
generate_r_api <- function(pymodule, prefix = py_get_attr(pymodule,"__name__")){

  classes <- get_py_classes(pymodule)

  # Get constructors
  module_name <- substitute(pymodule)
  constructors <- purrr::map_chr(classes, write_constructor, module_name = module_name,
                                 pymodule = pymodule, prefix = prefix)

  # Get methods
  methods <- purrr::map(classes, get_py_methods, pymodule = pymodule)
  s3_methods <- purrr::flatten_chr(purrr::map2(methods, classes, write_s3_methods,
                                               pymodule = pymodule, prefix = prefix,
                                               module_abbreviation = module_name))


  # Get generics
  all_methods <- unique(purrr::flatten_chr(methods))
  s3_generic <- purrr::map_chr(all_methods, write_s3_generic, prefix = prefix)

  message <- "# Auto-generated api"
  paste(c(message, constructors, s3_generic, s3_methods), sep = "\n", collapse = "\n")
}



