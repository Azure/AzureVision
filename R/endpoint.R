#' Endpoint objects for computer vision services
#'
#' @param url The URL of the endpoint.
#' @param key A subscription key. Can be single-service or multi-service.
#' @param aad_token For the Computer Vision endpoint, an OAuth token object, of class [`AzureAuth::AzureToken`]. You can supply this as an alternative to a subscription key.
#' @param ... Other arguments to pass to [`AzureCognitive::cognitive_endpoint`].
#' @details
#' These are functions to create service-specific endpoint objects. Computer Vision supports authentication via either a subscription key or Azure Active Directory (AAD) token; Custom Vision only supports subscription key. Note that there are _two_ kinds of Custom Vision endpoint, one for training and the other for prediction.
#' @return
#' An object inheriting from `cognitive_endpoint`. The subclass indicates the type of service/endpoint: Computer Vision, Custom Vision training, or Custom Vision prediction.
#' @seealso
#' [`cognitive_endpoint`], [`call_cognitive_endpoint`]
#' @rdname endpoint
#' @examples
#'
#' computervision_endpoint("https://myaccount.cognitiveservices.azure.com", key="key")
#'
#' customvision_training_endpoint("https://westus.api.cognitive.microsoft.com", key="key")
#'
#' customvision_prediction_endpoint("https://westus.api.cognitive.microsoft.com", key="key")
#'
#' @export
computervision_endpoint <- function(url, key=NULL, aad_token=NULL, ...)
{
    endp <- cognitive_endpoint(url, service_type="ComputerVision", key=key, aad_token=aad_token, ...)
    endp$url$path <- file.path("vision", getOption("azure_computervision_api_version"))
    endp
}


#' @rdname endpoint
#' @export
customvision_training_endpoint <- function(url, key=NULL, ...)
{
    endp <- cognitive_endpoint(url, service_type="CustomVision.Training", key=key, ..., auth_header="training-key")
    endp$url$path <- file.path("customvision", getOption("azure_customvision_training_api_version"))
    endp
}


#' @rdname endpoint
#' @export
customvision_prediction_endpoint <- function(url, key=NULL, ...)
{
    endp <- cognitive_endpoint(url, service_type="CustomVision.Prediction", key=key, ..., auth_header="prediction-key")
    endp$url$path <- file.path("customvision", getOption("azure_customvision_prediction_api_version"))
    endp
}


#' Carry out a Custom Vision operation
#'
#' @param project For `do_training_op`, a Custom Vision project.
#' @param service For `do_prediction_op`, a Custom Vision predictive service.
#' @param op,... Further arguments passed to `call_cognitive_endpoint`, and ultimately to the REST API.
#' @details
#' These functions provide low-level access to the Custom Vision REST API. `do_training_op` is for working with the training endpoint, and `do_prediction_op` with the prediction endpoint. You can use them if the other tools in this package don't provide what you need.
#' @seealso
#' [`customvision_training_endpoint`], [`customvision_prediction_endpoint`],
#' [`customvision_project`], [`customvision_predictive_service`], [`call_cognitive_endpoint`]
#' @rdname do_customvision_op
#' @export
do_training_op <- function(project, ...)
{
    UseMethod("do_training_op")
}


#' @rdname do_customvision_op
#' @export
do_training_op.customvision_project <- function(project, op, ...)
{
    op <- file.path("training/projects", project$project$id, op)
    call_cognitive_endpoint(project$endpoint, op, ...)
}


#' @rdname do_customvision_op
#' @export
do_prediction_op <- function(service, ...)
{
    UseMethod("do_prediction_op")
}


#' @rdname do_customvision_op
#' @export
do_prediction_op.customvision_predictive_service <- function(service, op, ...)
{
    op <- file.path("prediction", service$project, op)
    call_cognitive_endpoint(service$endpoint, op, ...)
}


is_any_uri <- function(string)
{
    uri <- httr::parse_url(string)
    !is.null(uri$scheme) && !is.null(uri$hostname)
}
