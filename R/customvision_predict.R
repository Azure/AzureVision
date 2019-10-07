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
    names(out) <- names(images)
    normalize_predictions(lapply(out, `[[`, "predictions"), type)
}


predict.classification_service <- function(object, images, save_result=FALSE, ...)
{
    type <- image_type(images)
    op <- file.path("classify/iterations", object$name, if(type == "files") "image" else "url")
    if(!save_result)
        op <- file.path(op, "nostore")
    call_cognitive_endpoint(object$endpoint, op)
}


predict.object_detection_service <- function(object, images, save_result=FALSE, ...)
{
    type <- image_type(images)
    op <- file.path("detect/iterations", object$name, if(type == "files") "image" else "url")
    if(!save_result)
        op <- file.path(op, "nostore")
    call_cognitive_endpoint(object$endpoint, op)
}


classification_service <- function(endpoint, project, name)
{
    if(inherits(project, "classification_project"))
        project <- project$project$id
    else if(!is.character(project))
        stop("Must supply a classification project", call.=FALSE)

    structure(
        list(endpoint=endpoint, project=project, name=name),
        class=c("classification_service", "customvision_predictive_service")
    )
}


object_detection_service <- function(endpoint, project, name)
{
    if(inherits(project, "object_detection_project"))
        project <- project$project$id
    else if(!is.character(project))
        stop("Must supply an object detection project", call.=FALSE)

    structure(
        list(endpoint=endpoint, project=project, name=name),
        class=c("object_detection_service", "customvision_predictive_service")
    )
}


normalize_predictions <- function(lst, type)
{
    labels <- lst[[1]]$name
}
