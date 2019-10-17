#' @export
print.customvision_model <- function(x, ...)
{
    cat("Azure Custom Vision model\n")
    cat("  Project/iteration: ", x$project$project$name, "/", x$id, "\n", sep="")
    invisible(x)
}


#' Create, retrieve and delete a model iteration
#'
#' @param project A Custom Vision project.
#' @param object For the `delete_model` methods, A Custom Vision project or model, as appropriate.
#' @param training_method The training method to use. The default "quick" is faster but may be less accurate. The "advanced" method is slower but produces better results.
#' @param max_time For advanced training, the maximum training time in hours.
#' @param force For advanced training, whether to refit the model even if the data has not changed since the last iteration.
#' @param email For advanced training, an email address to notify when the training is complete.
#' @param wait whether to wait until training is complete (or the maximum training time has elapsed) before returning.
#' @param iteration For `get_model` and `delete_model.customvision_project`, the iteration ID.
#' @param as For `list_models`, the format in which to return results: as a named vector of model iteration IDs, or a list of model objects.
#' @param confirm For the `delete_model` methods, whether to ask for confirmation first.
#' @param ... Arguments passed to lower-level functions.
#' @details
#' Training a Custom Vision model results in a _model iteration_. Each iteration is based on the current set of images uploaded to the endpoint. Successive model iterations trained on different image sets do not overwrite previous ones.
#'
#' By default, AzureVision will use the latest model iteration for actions such as prediction, showing performance statistics, and so on. You can list the model iterations with `list_models`, and retrieve a specific iteration by passing the iteration ID to `get_model`.
#' @return
#' For `train_model` and `get_model`, an object of class `customvision_model` which is a handle to the iteration.
#'
#' For `list_models`, based on the `as` argument: `as="ids"` returns a named vector of model iteration IDs, while `as="list"` returns a list of model objects.
#' @seealso
#' [`show_model`], [`show_training_performance`], [`publish_model`]
#' @rdname customvision_train
#' @export
train_model <- function(project, training_method=c("quick", "advanced"), max_time=1, force=FALSE, email=NULL,
                        wait=(training_method == "quick"))
{
    training_method <- match.arg(training_method)
    opts <- if(training_method == "advanced")
        list(
            trainingType="advanced",
            reservedBudgetInHours=max_time,
            forceTrain=force,
            notificationEmailAddress=email
        )
    else list()

    res <- do_training_op(project, "train", options=opts, http_verb="POST")

    if(wait)
    {
        message("Waiting for training to complete")
        interval <- 10
        for(i in 1:(max_time * (3600/interval)))
        {
            message(".", appendLF=FALSE)
            Sys.sleep(interval)
            res <- do_training_op(project, file.path("iterations", res$id))
            if(res$status != "Training")
                break
        }
        message("\n")
        if(!(res$status %in% c("Training", "Completed")))
            stop("Unable to train model, final status '", res$status, "'", call.=FALSE)
        if(res$status == "Training")
            warning("Training not yet completed")
    }

    make_model_iteration(res, project)
}


#' @rdname customvision_train
#' @export
list_models <- function(project, as=c("ids", "list"))
{
    as <- match.arg(as)
    res <- do_training_op(project, "iterations")
    times <- sapply(res, `[[`, "lastModified")
    names(res) <- sapply(res, `[[`, "name")
    if(as == "ids")
        sapply(res[order(times, decreasing=TRUE)], `[[`, "id")
    else lapply(res[order(times, decreasing=TRUE)], make_model_iteration, project=project)
}


#' @rdname customvision_train
#' @export
get_model <- function(project, iteration=NULL)
{
    if(is.null(iteration))
        iteration <- list_models(project)[1]

    res <- do_training_op(project, file.path("iterations", iteration))
    make_model_iteration(res, project)
}


#' @rdname customvision_train
#' @export
delete_model <- function(object, ...)
{
    UseMethod("delete_model")
}


#' @rdname customvision_train
#' @export
delete_model.customvision_project <- function(object, iteration=NULL, confirm=TRUE, ...)
{
    if(!confirm_delete("Are you sure you want to delete this model iteration?", confirm))
        return(invisible(NULL))

    if(is.null(iteration))
        iteration <- list_models(object)[[1]]$id

    do_training_op(object, file.path("iterations", iteration), http_verb="DELETE")
    invisible(NULL)
}


#' @rdname customvision_train
#' @export
delete_model.customvision_model <- function(object, confirm=TRUE, ...)
{
    if(!confirm_delete("Are you sure you want to delete this model iteration?", confirm))
        return(invisible(NULL))

    do_training_op(object$project, file.path("iterations", object$iteration), http_verb="DELETE")
    invisible(NULL)
}


#' Display model iteration details
#'
#' @param model,object A Custom Vision model iteration object.
#' @param threshold For a classification model, the probability threshold to assign an image to a class.
#' @param overlap For an object detection model, the overlap threshold for distinguishing between overlapping objects.
#' @param ... Arguments passed to lower-level functions.
#' @details
#' `show_model` displays the metadata for a model iteration: the name (assigned by default), model training status, publishing details, and so on. `show_training_performance` displays summary statistics for the model's performance on the training data. The `summary` method for Custom Vision model objects simply calls `show_training_performance`.
#'
#' @return
#' For `show_model`, a list containing the metadata for the model iteration. For `show_training_performance` and `summary.customvision_model`, a list of performance diagnostics.
#' @seealso
#' [`train_model`]
#' @rdname customvision_train_result
#' @export
show_model <- function(model)
{
    res <- do_training_op(model$project, file.path("iterations", model$id))
    res$created <- as_datetime(res$created)
    res$lastModified <- as_datetime(res$lastModified)
    res$trainedAt <- as_datetime(res$trainedAt)
    res
}


#' @rdname customvision_train_result
#' @export
show_training_performance <- function(model, threshold=0.5, overlap=NULL)
{
    op <- file.path("iterations", model$id, "performance")
    do_training_op(model$project, op, options=list(threshold=threshold, overlapThreshold=overlap),
                   simplifyVector=TRUE)
}


#' @rdname customvision_train_result
#' @export
summary.customvision_model <- function(object, ...)
{
    show_training_performance(object, ...)
}


#' Publish, export and unpublish a Custom Vision model iteration
#'
#' @param model A Custom Vision model iteration object.
#' @param name For `publish_model`, the name to assign to the published model on the prediction endpoint.
#' @param prediction_resource For `publish_model`, the Custom Vision prediction resource to publish to. This can either be a string containing the Azure resource ID, or an AzureRMR resource object.
#' @param format For `export_model`, the format to export to. See below for supported formats.
#' @param download For `export_model`, whether to download the exported model.
#' @param destfile For `export_model`, the destination file for downloading (if `download` is TRUE).
#' @param confirm For `unpublish_model`, whether to ask for confirmation first.
#' @details
#' Publishing a model makes it available to clients as a predictive service. Exporting a model serialises it to a file of the given format in Azure storage, which can then be downloaded. Each iteration of the model can be published or exported separately.
#'
#' The `format` argument to `export_model` can be one of the following. Note that exporting a model requires that the project was created with support for it.
#' - "onnx 1.0", "onnx 1.2": ONNX 1.0 or 1.2
#' - "coreml": CoreML, for iOS 11 devices
#' - "tensorflow": TensorFlow
#' - "linux docker", "windows docker", "arm docker": A Docker image for the given platform (Raspberry Pi 3 in the case of ARM)
#' - "vaidk": Vision AI Development Kit
#' @return
#' `export_model` returns the URL of the exported file, invisibly if `download=TRUE`.
#' @seealso
#' [`train_model`], [`get_model`], [`customvision_predictive_service`], [`predict.classification_service`], [`predict.object_detection_service`]
#' @rdname customvision_publish
#' @export
publish_model <- function(model, name, prediction_resource)
{
    if(is_resource(prediction_resource))
        prediction_resource <- prediction_resource$id

    op <- file.path("iterations", model$id, "publish")
    options <- list(publishName=name, predictionId=prediction_resource)
    do_training_op(model$project, op, options=options, http_verb="POST")
}


#' @rdname customvision_publish
#' @export
unpublish_model <- function(model, confirm=TRUE)
{
    if(!confirm_delete("Are you sure you want to unpublish the model?", confirm))
        return(invisible(NULL))

    op <- file.path("iterations", model$id, "publish")
    do_training_op(model$project, op, http_verb="DELETE")
    invisible(NULL)
}


#' @rdname customvision_publish
#' @export
export_model <- function(model, format, download=TRUE, destfile=NULL)
{
    settings <- model$project$project$settings

    if(!is_compact_domain(settings$domainId))
        stop("Project was not created with support for exporting", call.=FALSE)

    platform <- get_export_platform(format)
    if(platform$platform == "VAIDK" && is_empty(settings$targetExportPlatforms))
        stop("Project does not support exporting to Vision AI Dev Kit format", call.=FALSE)

    op <- file.path("iterations", model$id, "export")
    for(i in 1:500)
    {
        res <- do_training_op(model$project, op, options=platform, http_verb="POST")
        status <- res$status
        if(res$status %in% c("Done", "Failed"))
            break
        Sys.sleep(10)
    }
    if(res$status != "Done")
        stop("Error exporting model", call.=FALSE)

    if(download)
    {
        if(is.null(destfile))
            destfile <- basename(httr::parse_url(res$downloadUri)$path)
        message("Downloading to ", destfile)
        utils::download.file(res$downloadUri, destfile)
        invisible(res$downloadUri)
    }
    else res$downloadUri
}


as_datetime <- function(x, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
{
    as.POSIXct(x, format=format, tz=tz)
}


make_model_iteration <- function(iteration, project)
{
    structure(
        list(project=project, id=iteration$id),
        class="customvision_model"
    )
}


get_export_platform <- function(format)
{
    switch(tolower(format),
        "coreml"=list(platform="CoreML"),
        "arm docker"=list(platform="DockerFile", flavor="ARM"),
        "linux docker"=list(platform="DockerFile", flavor="Linux"),
        "windows docker"=list(platform="DockerFile", flavor="Windows"),
        "onnx 1.0"=list(platform="ONNX", flavor="ONNX10"),
        "onnx 1.2"=list(platform="ONNX", flavor="ONNX12"),
        "tensorflow"=list(platform="TensorFlow"),
        "vaidk"=list(platform="VAIDK"),
        stop("Unrecognised export format '", format, "'", call.=FALSE)
    )
}

