#' @export
computervision_endpoint <- function(url, ...)
{
    cognitive_endpoint(url, service_type="ComputerVision", ...)
}


#' @export
customvision_training_endpoint <- function(url, ...)
{
    cognitive_endpoint(url, service_type="CustomVision.Training", ...)
}


#' @export
customvision_prediction_endpoint <- function(url, ...)
{
    cognitive_endpoint(url, service_type="CustomVision.Prediction", ...)
}


#' @export
face_endpoint <- function(url, ...)
{
    cognitive_endpoint(url, service_type="Face", ...)
}


#' @export
call_cognitive_endpoint.computervision_endpoint <- function(endpoint, ...)
{
    NextMethod()
}


#' @export
call_cognitive_endpoint.face_endpoint <- function(endpoint, ...)
{
    NextMethod()
}


#' @export
call_cognitive_endpoint.customvision_training_endpoint <- function(endpoint, ...)
{
    NextMethod(auth_header="training-key")
}


#' @export
call_cognitive_endpoint.customvision_prediction_endpoint <- function(endpoint, ...)
{
    NextMethod(auth_header="prediction-key")
}


do_training_op <- function(project, op, ...)
{
    op <- file.path("training/projects", project$project$id, op)
    call_cognitive_endpoint(project$endpoint, op, ...)
}


is_any_uri <- function(string)
{
    uri <- httr::parse_url(string)
    !is.null(uri$scheme) && !is.null(uri$hostname)
}
