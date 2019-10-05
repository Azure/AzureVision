predict.customvision_model <- function(object, images, ...)
{
    files <- validate_images(images) == "files"
    options <- list(iterationId=object$id)
    if(length(images) > 1)
        warning("Predicting with the training endpoint is only for testing purposes;\n",
                "to get multiple predictions efficiently, publish to a prediction resource", call.=FALSE)

    out <- if(files)
    {
        mapply(function(f, size)
            do_training_op(object$project, "quicktest/image", options=options, body=readBin(f, "raw", size),
                           http_verb="POST",
                           simplifyVector=TRUE),
            images, file.size(images), SIMPLIFY=FALSE)
    }
    else
    {
        lapply(files, function(f)
            do_training_op(object$project, "quicktest/url", options=options, body=list(url=f),
                           http_verb="POST",
                           simplifyVector=TRUE))
    }
    names(out) <- images
    lapply(out, function(x) x$predictions)
}


predict.classification_service <- function(object, images, save_result=FALSE, ...)
{
    type <- validate_images(images)
    op <- file.path("classify/iterations", object$name, if(type == "files") "image" else "imageUrl")
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


object_detection_service <- function(endpoint, name)
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

