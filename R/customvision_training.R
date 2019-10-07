#' @export
print.customvision_model <- function(x, ...)
{
    cat("Azure Custom Vision model iteration", x$id, "\n")
    cat("  Project:", x$project$project$name, "\n")
    invisible(x)
}


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
            message("Training not yet completed")
    }

    make_model_iteration(res, project)
}


#' @export
list_models <- function(project)
{
    res <- do_training_op(project, "iterations")
    times <- sapply(res, function(x) x$lastModified)
    names(res) <- sapply(res, `[[`, "name")
    lapply(res[order(times, decreasing=TRUE)], make_model_iteration, project=project)
}


#' @export
get_model <- function(project, iteration=NULL)
{
    if(is.null(iteration))
        iteration <- list_models(project)[[1]]$id

    res <- do_training_op(project, file.path("iterations", iteration))
    make_model_iteration(res, project)
}


#' @export
show_model <- function(model)
{
    res <- do_training_op(model$project, file.path("iterations", model$id))
    structure(res, class="customvision_model_info")
}


#' @export
training_performance <- function(model, threshold=0.5, overlap=NULL)
{
    op <- file.path("iterations", model$id, "performance")
    do_training_op(model$project, op, options=list(threshold=threshold, overlapThreshold=overlap),
                   simplifyVector=TRUE)
}


#' @export
delete_model <- function(model, confirm=TRUE)
{
    if(!confirm_delete("Are you sure you want to delete this model iteration?", confirm))
        return(invisible(NULL))

    do_training_op(model$project, file.path("iterations", model$iteration), http_verb="DELETE")
    invisible(NULL)
}


#' @export
publish_model <- function(model, name, prediction_resource)
{
    if(!is_resource(prediction_resource))
        stop("Must supply an Azure prediction resource object", call.=FALSE)

    op <- file.path("iterations", model$id, "publish")
    options <- list(publishName=name, predictionId=prediction_resource$id)
    return(do_training_op(model$project, op, options=options, http_verb="POST", http_status_handler="pass"))

    pred_endp <- prediction_resource$properties$endpoint
    if(is_classification_project(model$project$project))
        classification_service(pred_endp, model$project$project$id, name)
    else object_detection_service(pred_endp, model$project$project$id, name)
}


#' @export
unpublish_model <- function(model, confirm=TRUE)
{
    if(!confirm_delete("Are you sure you want to unpublish the model?", confirm))
        return(invisible(NULL))

    op <- file.path("iterations", model$id, "publish")
    do_training_op(model$project, op, http_verb="DELETE")
    invisible(NULL)
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
