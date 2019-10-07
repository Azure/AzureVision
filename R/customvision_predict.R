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


predict.classification_service <- function(object, images, type=c("class", "prob", "list"), save_result=FALSE, ...)
{
    type <- match.arg(type)
    customvision_predict_internal(object, images, type, save_result, verb="classify")
}


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
