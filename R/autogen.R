Undefined <- NULL
inspect <- NULL

#' @import reticulate glue purrr readr
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

get_py_functions <- function(pymodule) {
  attributes <- py_list_attributes(pymodule)
  attributes <- attributes[!grepl("^_",attributes)]
  funcs <- attributes[purrr::map_lgl(attributes, ~is_py_method(py_get_attr(pymodule,.)))]
  funcs
}

get_arg_spec <- function(pyfun){
  py_to_r(inspect$getfullargspec(pyfun))
}

add_comma_plus <- function(x, y, space = TRUE){
  if (x == "") {
    return(y)
  } else{
    return(paste(x,y,sep = if (space) ", " else ","))
  }
}

add_comma_if <- function(x, y, space = TRUE){
  if (y == "") {
    return(x)
  } else{
    return(paste(x,y,sep = if (space) ", " else ","))
  }
}

sanitize_arg <- function(x){
  paste0('`',x,'`')
}

get_args <- function(pyfun, ignore_first = TRUE){
  out <- tryCatch(get_arg_spec(pyfun)$args, error = function(e) NULL)
  if (!is.null(out)){
    ixs <- if (ignore_first) 2 else 1
    if (length(out) >= ixs) {
      out <- out[ixs:length(out)]}
    else { 
      out <- NULL
    }
  }
  return(out)
}

get_all_class_args <- function(pyclass, pymodule){
   # Gets args for both the class itself and all methods...
   cl <- py_get_attr(pymodule,pyclass)
   args1 <- get_args(cl)
  
   methods <- get_py_methods(pyclass, pymodule = pymodule)
   args2 <- purrr::flatten_chr(purrr::map(methods, ~get_args(py_get_attr(cl, .))))
   
   if (length(methods) > 0){
     args2 <- c(args2, "...")
   }
   
   paste(unique(c(args1, args2)), collapse = ",")
}

write_method_arglist <- function(pyfun, ignore_first = TRUE, always_dots = FALSE){
  ixs <- if (ignore_first) 2 else 1
  args <- tryCatch(get_arg_spec(pyfun), error = function(e) NULL)
  if (is.null(args)) {
    warning(glue::glue("No signature found for {pyfun}"))
    return(list(inner = "", outer = ""))
  }
  len_defaults <- length(args$defaults)
  len_args <- length(args$args)
  mandatory <- len_args - len_defaults
  out = ""
  out2 = ""
  if (mandatory >= ixs) {
    out = paste(sanitize_arg(args$args[ixs:mandatory]), collapse = ",")
    out2 = paste(paste0("r_to_py(",sanitize_arg(args$args[ixs:mandatory]),")"), collapse = ",")}
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
  if (!is.null(args$varargs) || !is.null(args$varkw) || always_dots) {
    out = add_comma_plus(out, "...")
    out2 = add_comma_plus(out2, "...")
  }
  return(list(inner = out2, outer = out))
}

write_constructor <- function(pyclass, module_name, pymodule, prefix ){
  arg_lists <- write_method_arglist(py_get_attr(pymodule,pyclass))
  params <- get_all_class_args(pyclass, pymodule)
  func_name <- glue::glue("{prefix}_{pyclass}")
  docs <- glue::glue(readr::read_file(system.file("templates/constructor.R", package = "autopyr")))
  glue::glue(
    "{docs}
    {func_name} <- function ({arg_lists$outer}) {{
    {module_name}$`{pyclass}`({arg_lists$inner})
    }}")
}

write_function <- function(pyfunc, module_name, pymodule, prefix ){
  arg_lists <- write_method_arglist(py_get_attr(pymodule,pyfunc), ignore_first = FALSE)
  params <- paste(get_args(py_get_attr(pymodule,pyfunc), ignore_first = FALSE),collapse = ",")
  func_name <- glue::glue("{prefix}_{pyfunc}")
  docs <- glue::glue(readr::read_file(system.file("templates/function.R", package = "autopyr")))
  glue::glue(
    "{docs}
    {func_name} <- function ({arg_lists$outer}) {{
    {module_name}$`{pyfunc}`({arg_lists$inner})
    }}")
}

write_s3_method <- function(pymethod, pyclass, pymodule, prefix, module_abbreviation){
  module_name <- py_get_attr(py_get_attr(pymodule,pyclass),"__module__")
  method_name <- glue::glue("{prefix}_{pymethod}.{module_name}.{pyclass}")
  arg_lists <- write_method_arglist(py_get_attr(py_get_attr(pymodule,pyclass),pymethod), always_dots = TRUE)
  docs <- glue::glue(readr::read_file(system.file("templates/s3_method.R", package = "autopyr")))
  outer_arg <- add_comma_if("pyr_object", arg_lists$outer)
  glue::glue(
    "{docs}
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

  # Get functions
  all_funcs <- get_py_functions(pymodule) 
  funcs <- purrr::map_chr(all_funcs, write_function, module_name = module_name,
                          pymodule = pymodule, prefix = prefix)
  
  message <- "# API auto-generated by autopyr package"
  paste(c(message, constructors, s3_generic, s3_methods, funcs), sep = "\n", collapse = "\n")
}



