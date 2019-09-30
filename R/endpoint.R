vision_endpoint <- function(url, key=NULL, aad_token=NULL, cognitive_token=NULL)
{
    obj <- cognitive_endpoint(url, service_type="ComputerVision", key=key, aad_token=aad_token,
                              cognitive_token=cognitive_token)
    class(object) <- c("vision_endpoint", class(obj))
    object
}


customvision_training_endpoint <- function(url, key=NULL, aad_token=NULL, cognitive_token=NULL)
{
    obj <- cognitive_endpoint(url, service_type="CustomVision", key=key, aad_token=aad_token,
                              cognitive_token=cognitive_token)
    class(obj) <- c("customvision_training_endpoint", class(obj))
    obj
}


customvision_prediction_endpoint <- function(url, key=NULL, aad_token=NULL, cognitive_token=NULL)
{
    obj <- cognitive_endpoint(url, service_type="CustomVision", key=key, aad_token=aad_token,
                              cognitive_token=cognitive_token)
    class(obj) <- c("customvision_prediction_endpoint", class(obj))
    obj
}


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
