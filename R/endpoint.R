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


# #' @export
# face_endpoint <- function(url, ...)
# {
#     cognitive_endpoint(url, service_type="Face", ...)
# }


#' @export
call_cognitive_endpoint.computervision_endpoint <- function(endpoint, ...)
{
    NextMethod()
}


# #' @export
# call_cognitive_endpoint.face_endpoint <- function(endpoint, ...)
# {
#     NextMethod()
# }


#' @export
call_cognitive_endpoint.customvision_training_endpoint <- function(endpoint, ..., auth_header="training-key")
{
    NextMethod(auth_header=auth_header)
}


#' @export
call_cognitive_endpoint.customvision_prediction_endpoint <- function(endpoint, ..., auth_header="prediction-key")
{
    NextMethod(auth_header=auth_header)
}


do_training_op <- function(project, op, ...)
{
    if(!inherits(project, "customvision_project"))
        stop("First argument must be a Custom Vision project", call.=FALSE)
    op <- file.path("training/projects", project$project$id, op)
    call_cognitive_endpoint(project$endpoint, op, ...)
}


do_prediction_op <- function(object, op, ...)
{
    if(!inherits(object, "customvision_predictive_service"))
        stop("First argument must be a Custom Vision predictive service", call.=FALSE)

    op <- file.path("prediction", object$project_id, op)
    call_cognitive_endpoint(object$endpoint, op, ...)
}


is_any_uri <- function(string)
{
    uri <- httr::parse_url(string)
    !is.null(uri$scheme) && !is.null(uri$hostname)
}
