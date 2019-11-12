#' Publish, export and unpublish a Custom Vision model iteration
#'
#' @param model A Custom Vision model iteration object.
#' @param name For `publish_model`, the name to assign to the published model on the prediction endpoint.
#' @param prediction_resource For `publish_model`, the Custom Vision prediction resource to publish to. This can either be a string containing the Azure resource ID, or an AzureRMR resource object.
#' @param format For `export_model`, the format to export to. See below for supported formats.
#' @param destfile For `export_model`, the destination file for downloading. Set this to NULL to skip downloading.
#' @param confirm For `unpublish_model`, whether to ask for confirmation first.
#' @details
#' Publishing a model makes it available to clients as a predictive service. Exporting a model serialises it to a file of the given format in Azure storage, which can then be downloaded. Each iteration of the model can be published or exported separately.
#'
#' The `format` argument to `export_model` can be one of the following. Note that exporting a model requires that the project was created with support for it.
#' - `"onnx"`: ONNX 1.2
#' - `"coreml"`: CoreML, for iOS 11 devices
#' - `"tensorflow"`: TensorFlow
#' - `"tensorflow lite"`: TensorFlow Lite for Android devices
#' - `"linux docker"`, `"windows docker"`, `"arm docker"`: A Docker image for the given platform (Raspberry Pi 3 in the case of ARM)
#' - `"vaidk"`: Vision AI Development Kit
#' @return
#' `export_model` returns the URL of the exported file, invisibly if it was downloaded.
#'
#' `list_model_exports` returns a data frame detailing the formats the current model has been exported to, along with their download URLs.
#' @seealso
#' [`train_model`], [`get_model`], [`customvision_predictive_service`], [`predict.classification_service`], [`predict.object_detection_service`]
#' @examples
#' \dontrun{
#'
#' endp <- customvision_training_endpoint(url="endpoint_url", key="key")
#' myproj <- get_project(endp, "myproject")
#' mod <- get_model(myproj)
#'
#' export_model(mod, "tensorflow", download=FALSE)
#' export_model(mod, "onnx", destfile="onnx.zip")
#'
#' rg <- AzureRMR::get_azure_login("yourtenant")$
#'     get_subscription("sub_id")$
#'     get_resource_group("rgname")
#'
#' pred_res <- rg$get_cognitive_service("mycustvis_prediction")
#' publish_model(mod, "mypublishedmod", pred_res)
#'
#' unpublish_model(mod)
#'
#' }
#' @rdname customvision_publish
#' @export
publish_model <- function(model, name, prediction_resource)
{
    if(is_resource(prediction_resource))
        prediction_resource <- prediction_resource$id

    op <- file.path("iterations", model$id, "publish")
    options <- list(publishName=name, predictionId=prediction_resource)
    do_training_op(model$project, op, options=options, http_verb="POST")
    invisible(NULL)
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
export_model <- function(model, format, destfile=basename(httr::parse_url(dl_link)$path))
{
    settings <- model$project$project$settings

    if(!is_compact_domain(settings$domainId))
        stop("Project was not created with support for exporting", call.=FALSE)

    plat <- get_export_platform(format)
    if(plat$platform == "VAIDK" && is_empty(settings$targetExportPlatforms))
        stop("Project does not support exporting to Vision AI Dev Kit format", call.=FALSE)

    # if already exported, don't export again
    exports <- list_model_exports(model)
    this_exp <- find_model_export(plat, exports)
    if(is_empty(this_exp))
    {
        op <- file.path("iterations", model$id, "export")
        res <- do_training_op(model$project, op, options=plat, http_verb="POST")

        # wait for it to appear in the list of exports
        for(i in 1:500)
        {
            exports <- list_model_exports(model)
            this_exp <- find_model_export(plat, exports)
            if(is_empty(this_exp))
                stop("Exported model not found", call.=FALSE)

            status <- exports$status[this_exp]
            if(status %in% c("Done", "Failed"))
                break
            Sys.sleep(5)
        }
        if(status != "Done")
            stop("Unable to export model", call.=FALSE)
    }

    dl_link <- exports$downloadUri[this_exp]
    if(!is.null(destfile))
    {
        message("Downloading to ", destfile)
        utils::download.file(dl_link, destfile)
        invisible(dl_link)
    }
    else dl_link
}


#' @rdname customvision_publish
#' @export
list_model_exports <- function(model)
{
    op <- file.path("iterations", model$id, "export")
    do_training_op(model$project, op, simplifyVector=TRUE)
}


as_datetime <- function(x, format="%Y-%m-%dT%H:%M:%S", tz="UTC")
{
    as.POSIXct(x, format=format, tz=tz)
}


make_model_iteration <- function(iteration, project)
{
    structure(
        list(project=project, id=iteration$id, name=iteration$name),
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
        "onnx"=list(platform="ONNX"),
        "tensorflow"=list(platform="TensorFlow", flavor="TensorFlowNormal"),
        "tensorflow lite"=list(platform="TensorFlow", flavor="TensorFlowLite"),
        "vaidk"=list(platform="VAIDK"),
        stop("Unrecognised export format '", format, "'", call.=FALSE)
    )
}


find_model_export <- function(platform, exports)
{
    this_plat <- exports$platform == platform$platform
    this_flav <- if(!is.null(platform$flavor))
        exports$flavor == platform$flavor
    else TRUE
    which(this_plat & this_flav)
}


