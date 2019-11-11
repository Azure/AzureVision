#' @export
print.customvision_model <- function(x, ...)
{
    cat("Azure Custom Vision model\n")
    cat("  Project/iteration: ", x$project$project$name, "/", x$name, " (", x$id, ")", "\n", sep="")
    invisible(x)
}


#' Create, retrieve, rename and delete a model iteration
#'
#' @param project A Custom Vision project.
#' @param model A Custom Vision model.
#' @param object For the `delete_model` method, a Custom Vision project or model, as appropriate.
#' @param training_method The training method to use. The default "quick" is faster but may be less accurate. The "advanced" method is slower but produces better results.
#' @param max_time For advanced training, the maximum training time in hours.
#' @param force For advanced training, whether to refit the model even if the data has not changed since the last iteration.
#' @param email For advanced training, an email address to notify when the training is complete.
#' @param wait whether to wait until training is complete (or the maximum training time has elapsed) before returning.
#' @param iteration For `get_model` and `delete_model.customvision_project`, either the iteration name or ID.
#' @param name For `rename_model`, the new name for the model.
#' @param as For `list_models`, the format in which to return results: as a named vector of model iteration IDs, or a list of model objects.
#' @param confirm For the `delete_model` methods, whether to ask for confirmation first.
#' @param ... Arguments passed to lower-level functions.
#' @details
#' Training a Custom Vision model results in a _model iteration_. Each iteration is based on the current set of images uploaded to the endpoint. Successive model iterations trained on different image sets do not overwrite previous ones.
#'
#' You must have at least 5 images per tag for a classification project, and 15 images per tag for an object detection project, before you can train a model.
#'
#' By default, AzureVision will use the latest model iteration for actions such as prediction, showing performance statistics, and so on. You can list the model iterations with `list_models`, and retrieve a specific iteration by passing the iteration ID to `get_model`.
#' @return
#' For `train_model`, `get_model` and `rename_model`, an object of class `customvision_model` which is a handle to the iteration.
#'
#' For `list_models`, based on the `as` argument: `as="ids"` returns a named vector of model iteration IDs, while `as="list"` returns a list of model objects.
#' @seealso
#' [`show_model`], [`show_training_performance`], [`publish_model`]
#' @examples
#' \dontrun{
#'
#' endp <- customvision_training_endpoint(url="endpoint_url", key="key")
#' myproj <- get_project(endp, "myproject")
#'
#' train_model(myproj)
#' train_model(myproj, method="advanced", force=TRUE, email="me@example.com")
#'
#' list_models(myproj)
#'
#' mod <- get_model(myproj)
#' rename(mod, "mymodel")
#' mod <- get_model(myproj, "mymodel")
#'
#' delete_model(mod)
#'
#' }
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
    iteration <- find_model_iteration(iteration, project)
    res <- do_training_op(project, file.path("iterations", iteration))
    make_model_iteration(res, project)
}


#' @rdname customvision_train
#' @export
rename_model <- function(model, name, ...)
{
    res <- do_training_op(model$project, file.path("iterations", model$id), body=list(name=name), http_verb="PATCH")
    make_model_iteration(res, model$project)
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

    iteration <- find_model_iteration(iteration, object)
    do_training_op(object, file.path("iterations", iteration), http_verb="DELETE")
    invisible(NULL)
}


#' @rdname customvision_train
#' @export
delete_model.customvision_model <- function(object, confirm=TRUE, ...)
{
    if(!confirm_delete("Are you sure you want to delete this model iteration?", confirm))
        return(invisible(NULL))

    do_training_op(object$project, file.path("iterations", object$id), http_verb="DELETE")
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
#' @examples
#' \dontrun{
#'
#' endp <- customvision_training_endpoint(url="endpoint_url", key="key")
#' myproj <- get_project(endp, "myproject")
#' mod <- get_model(myproj)
#'
#' show_model(mod)
#'
#' show_training_performance(mod)
#' summary(mod)
#'
#' }
#' @rdname customvision_train_result
#' @export
show_model <- function(model)
{
    res <- do_training_op(model$project, file.path("iterations", model$id),
        simplifyVector=TRUE, simplifyDataFrame=FALSE)
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


find_model_iteration <- function(iteration=NULL, project)
{
    iters <- list_models(project)

    if(is.null(iteration))
        return(iters[1])

    if(is_guid(iteration))
    {
        if(!(iteration %in% iters))
            stop("Invalid model iteration ID", call.=FALSE)
        return(iteration)
    }
    else
    {
        if(!(iteration %in% names(iters)))
            stop("Invalid model iteration name", call.=FALSE)
        return(iters[iteration])
    }
}


