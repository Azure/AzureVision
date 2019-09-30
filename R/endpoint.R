call_cognitive_endpoint.computervision_endpoint <- function(endpoint, ...)
{
    NextMethod()
}


call_cognitive_endpoint.face_endpoint <- function(endpoint, ...)
{
    NextMethod()
}


call_cognitive_endpoint.customvision_training_endpoint <- function(endpoint, ...)
{
    NextMethod(auth_header="training-key")
}


call_cognitive_endpoint.customvision_prediction_endpoint <- function(endpoint, ...)
{
    NextMethod(auth_header="prediction-key")
}


is_any_uri <- function(string)
{
    uri <- httr::parse_url(string)
    !is.null(uri$scheme) && !is.null(uri$hostname)
}
