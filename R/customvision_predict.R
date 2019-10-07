predict.customvision_model <- function(object, images, type=c("class", "prob", "list"), ...)
{
    type <- match.arg(type)
    images <- images_to_bodies(images)
    opts <- list(iterationId=object$id)

    out <- if(!is.null(images[[1]]$content))
    {
        lapply(images, function(f)
            do_training_op(object$project, "quicktest/image", options=opts, body=f$content,
                           http_verb="POST",
                           simplifyVector=TRUE))
    }
    else
    {
        lapply(images, function(f)
            do_training_op(object$project, "quicktest/url", options=opts, body=f,
                           http_verb="POST",
                           simplifyVector=TRUE))
    }

    lapply(out, `[[`, "predictions")
}


predict.classification_service <- function(object, images, save_result=FALSE, ...)
{
    customvision_predict_internal(object, images, save_result, verb="classify")
}


predict.object_detection_service <- function(object, images, save_result=FALSE, ...)
{
    customvision_predict_internal(object, images, save_result, verb="detect")
}


customvision_predict_internal <- function(object, images, save_result, verb)
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
    out
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
    labels <- lst[[1]]$name
}
