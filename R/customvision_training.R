train_model <- function(project, training_method=c("quick", "advanced"), max_time=1, force=FALSE, email=NULL,
                        wait=(training_method == "advanced"))
{
    opts <- if(match.arg(training_method) == "advanced")
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
        interval <- 10
        for(i in 1:(max_time * (3600/interval)))
        {
            Sys.sleep(interval)
            res <- do_training_op(project, file.path("iterations", res$id))
            if(res$status != "Training")
                break
        }
        if(res$status != "Completed")
            stop("Error training model, final status '", res$status, "'", call.=FALSE)
    }

    as_model_iteration(res, project)
}


list_models <- function(project)
{
    res <- do_training_op(project, "iterations")
    times <- sapply(res, function(x) x$lastModified)
    names(res) <- sapply(res, `[[`, "name")
    lapply(res[order(times, decreasing=TRUE)], as_model_iteration, project=project)
}


get_model <- function(project, iteration=NULL)
{
    if(is.null(iteration))
        iteration <- list_models(project)[[1]]$id

    res <- do_training_op(project, file.path("iterations", iteration))
    as_model_iteration(res, project)
}


show_model <- function(model=NULL)
{
    res <- do_training_op(model$project, file.path("iterations", model$id))
    structure(res, class="customvision_model_info")
}


delete_model <- function(model, confirm=TRUE)
{
    if(!confirm_delete("Are you sure you want to delete the model?", confirm))
        return(invisible(NULL))

    do_training_op(model$project, file.path("iterations", model$iteration), http_verb="DELETE")
    invisible(NULL)
}


as_datetime <- function(x, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
{
    as.POSIXct(x, format=format, tz=tz)
}


as_model_iteration <- function(iteration, project)
{
    structure(
        list(project=project, id=iteration$id),
        class="customvision_model"
    )
}
