call_cognitive_endpoint.customvision_training_endpoint <- function(endpoint, ...)
{
    class(endpoint) <- class(endpoint)[-1]
    call_cognitive_endpoint(endpoint, ..., auth_header="training-key")
}


call_cognitive_endpoint.customvision_prediction_endpoint <- function(endpoint, ...)
{
    class(endpoint) <- class(endpoint)[-1]
    call_cognitive_endpoint(endpoint, ..., auth_header="prediction-key")
}


is_any_uri <- function(string)
{
    uri <- httr::parse_url(string)
    !is.null(uri$scheme) && !is.null(uri$hostname)
}
