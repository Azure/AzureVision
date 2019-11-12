#' Get predictions from a Custom Vision model
#'
#' @param object A Custom Vision object from which to get predictions. See 'Details' below.
#' @param images The images for which to get predictions.
#' @param type The type of prediction: either class membership (the default), the class probabilities, or a list containing all information returned by the prediction endpoint.
#' @param save_result For the predictive service methods, whether to store the predictions on the server for future use.
#' @param ... Further arguments passed to lower-level functions; not used.
#' @details
#' AzureVision defines prediction methods for both Custom Vision model training objects (of class `customvision_model`) and prediction services (`classification_service` and `object_detection_service`). The method for model training objects calls the "quick test" endpoint, and is meant only for testing purposes.
#'
#' The prediction endpoints accept a single image per request, so supplying multiple images to these functions will call the endpoints multiple times, in sequence. The images can be specified as:
#' - A vector of local filenames. All common image file formats are supported.
#' - A vector of publicly accessible URLs.
#' - A raw vector, or a list of raw vectors, holding the binary contents of the image files.
#' @seealso
#' [`train_model`], [`publish_model`], [`classification_service`], [`object_detection_service`]
#' @examples
#' \dontrun{
#'
#' # predicting with the training endpoint
#' endp <- customvision_training_endpoint(url="endpoint_url", key="key")
#' myproj <- get_project(endp, "myproject")
#' mod <- get_model(myproj)
#'
#' predict(mod, "testimage.jpg")
#' predict(mod, "https://mysite.example.com/testimage.jpg", type="prob")
#'
#' imgraw <- readBin("testimage.jpg", "raw", file.size("testimage.jpg"))
#' predict(mod, imgraw, type="list")
#'
#' # predicting with the prediction endpoint
#' # you'll need either the project object or the ID
#' proj_id <- myproj$project$id
#' pred_endp <- customvision_prediction_endpoint(url="endpoint_url", key="pred_key")
#' pred_svc <- classification_service(pred_endp, proj_id, "iteration1")
#' predict(pred_svc, "testimage.jpg")
#'
#' }
#' @aliases predict
#' @rdname customvision_predict
#' @export
predict.customvision_model <- function(object, images, type=c("class", "prob", "list"), ...)
{
    type <- match.arg(type)
    images <- images_to_bodies(images)
    files <- !is.null(images[[1]]$content)
    op <- file.path("quicktest", if(files) "image" else "url")
    opts <- list(iterationId=object$id)

    out <- if(files)
        lapply(images, function(f)
            do_training_op(object$project, op, options=opts, body=f$content, http_verb="POST", simplifyVector=TRUE))
    else lapply(images, function(f)
       do_training_op(object$project, op, options=opts, body=f, http_verb="POST", simplifyVector=TRUE))

    normalize_predictions(out, type)
}


#' @rdname customvision_predict
#' @export
predict.classification_service <- function(object, images, type=c("class", "prob", "list"), save_result=FALSE, ...)
{
    type <- match.arg(type)
    customvision_predict_internal(object, images, type, save_result, verb="classify")
}


#' @rdname customvision_predict
#' @export
predict.object_detection_service <- function(object, images, type=c("class", "prob", "list"), save_result=FALSE, ...)
{
    type <- match.arg(type)
    customvision_predict_internal(object, images, type, save_result, verb="detect")
}


customvision_predict_internal <- function(object, images, type, save_result, verb)
{
    images <- images_to_bodies(images)
    files <- !is.null(images[[1]]$content)
    op <- file.path(verb, "iterations", object$name, if(files) "image" else "url")
    if(!save_result)
        op <- file.path(op, "nostore")

    out <- if(files)
        lapply(images, function(f)
            do_prediction_op(object, op, body=f$content, http_verb="POST", simplifyVector=TRUE))
    else lapply(images, function(f)
        do_prediction_op(object, op, body=f, http_verb="POST", simplifyVector=TRUE))

    normalize_predictions(out, type)
}


#' Connect to a Custom Vision predictive service
#'
#' @param endpoint A prediction endpoint object, of class `customvision_prediction_endpoint`.
#' @param project The project underlying this predictive service. Can be either an object of class `customvision_project`, or a string giving the ID of the project.
#' @param name The published name of the service.
#' @details
#' These functions are handles to a predictive service that was previously published from a trained model. They have `predict` methods defined for them.
#' @return
#' An object of class `classification_service` or `object_detection_service`, as appropriate. These are subclasses of `customvision_predictive_service`.
#' @seealso
#' [`customvision_prediction_endpoint`], [`customvision_project`]
#'
#' [`predict.classification_service`], [`predict.object_detection_service`], [`do_prediction_op`]
#'
#' [`train_model`], [`publish_model`]
#' @examples
#' \dontrun{
#'
#' endp <- customvision_training_endpoint(url="endpoint_url", key="key")
#' myproj <- get_project(endp, "myproject")
#'
#' # getting the ID from the project object -- in practice you would store the ID separately
#' pred_endp <- customvision_prediction_endpoint(url="endpoint_url", key="pred_key")
#' classification_service(pred_endp, myproj$project$id, "publishedname")
#'
#' }
#' @aliases customvision_predictive_service
#' @rdname customvision_predictive_service
#' @export
classification_service <- function(endpoint, project, name)
{
    if(inherits(project, "classification_project"))
        project <- project$project$id
    else if(!is_guid(project))
        stop("Must supply a classification project object or ID", call.=FALSE)

    structure(
        list(endpoint=endpoint, project=project, name=name),
        class=c("classification_service", "customvision_predictive_service")
    )
}


#' @rdname customvision_predictive_service
#' @export
object_detection_service <- function(endpoint, project, name)
{
    if(inherits(project, "object_detection_project"))
        project <- project$project$id
    else if(!is_guid(project))
        stop("Must supply an object detection project object or ID", call.=FALSE)

    structure(
        list(endpoint=endpoint, project=project, name=name),
        class=c("object_detection_service", "customvision_predictive_service")
    )
}


normalize_predictions <- function(lst, type)
{
    names(lst) <- NULL
    lst <- lapply(lst, `[[`, "predictions")

    if(type == "list")
        lst
    else if(type == "prob")
    {
        tagnames <- sort(lst[[1]]$tagName)
        out <- t(sapply(lst, function(df) df$probability[order(df$tagName)]))
        colnames(out) <- tagnames
        out
    }
    else sapply(lst, function(df) df$tagName[which.max(df$probability)])
}
