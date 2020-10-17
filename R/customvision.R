#' @export
print.customvision_project <- function(x, ...)
{
    cat("Azure Custom Vision project '", x$project$name, "' (", x$project$id, ")\n", sep="")
    cat("  Endpoint:", httr::build_url(x$endpoint$url), "\n")

    domain_id <- x$project$settings$domainId
    compact <- is_compact_domain(domain_id)

    domains <- if(compact) unlist(.compact_domain_ids) else unlist(.domain_ids)
    domain_name <- names(domains)[domains == domain_id]
    if(compact)
        domain_name <- paste0(domain_name, ".compact")

    domain_name <- paste0(domain_name, " (", domain_id, ")")
    cat("  Domain:", domain_name, "\n")

    export_type <- if(!compact)
        "none"
    else if(is_empty(x$project$settings$targetExportPlatforms))
        "standard"
    else "Vision AI Dev Kit"
    cat("  Export target:", export_type, "\n")

    classtype <- if(get_purpose_from_domain_id(domain_id) == "object_detection")
        NA_character_
    else x$project$settings$classificationType
    cat("  Classification type:", classtype, "\n")

    invisible(x)
}


#' Create, retrieve, update and delete Azure Custom Vision projects
#'
#' @param endpoint A custom vision endpoint.
#' @param object For `delete_customvision_project`, either an endpoint, or a project object.
#' @param name,id The name and ID of the project. At least one of these must be specified for `get_project`, `update_project` and `delete_project`. The name is required for `create_project` (the ID will be assigned automatically).
#' @param domain What kinds of images the model is meant to apply to. The default "general" means the model is suitable for use in a generic setting. Other, more specialised domains for classification include "food", "landmarks" and "retail"; for object detection the other possible domain is "logo".
#' @param export_target What formats are supported when exporting the model.
#' @param multiple_tags For classification models, Whether multiple categories (tags/labels) for an image are allowed. The default is `FALSE`, meaning an image represents one and only one category. Ignored for object detection models.
#' @param description An optional text description of the project.
#' @param ... Further arguments passed to lower-level methods.
#' @details
#' A Custom Vision project contains the metadata for a model: its intended purpose (classification vs object detection), the domain, the set of training images, and so on. Once you have created a project, you upload images to it, and train models based on those images. A trained model can then be published as a predictive service, or exported for standalone use.
#'
#' By default, a Custom Vision project does not support exporting the model; this allows it to be more complex, and thus potentially more accurate. Setting `export_target="standard"` enables exporting to the following formats:
#' - ONNX 1.2
#' - CoreML, for iOS 11 devices
#' - TensorFlow
#' - TensorFlow Lite, for Android devices
#' - A Docker image for the Windows, Linux or Raspberry Pi 3 (ARM) platform
#'
#' Setting `export_target="vaidk"` allows exporting to Vision AI Development Kit format, in addition to the above.
#' @return
#' `delete_project` returns NULL invisibly, on a successful deletion. The others return an object of class `customvision_project`.
#' @seealso
#' [`customvision_training_endpoint`], [`add_images`], [`train_model`], [`publish_model`], [`predict.customvision_model`], [`do_training_op`]
#'
#' - [CustomVision.ai](https://www.customvision.ai/): An interactive site for building Custom Vision models, provided by Microsoft
#' - [Training API reference](https://southcentralus.dev.cognitive.microsoft.com/docs/services/Custom_Vision_Training_3.0/operations/5c771cdcbf6a2b18a0c3b7fa)
#' - [Prediction API reference](https://southcentralus.dev.cognitive.microsoft.com/docs/services/Custom_Vision_Prediction_3.0/operations/5c82db60bf6a2b11a8247c15)
#' @examples
#' \dontrun{
#'
#' endp <- customvision_training_endpoint(url="endpoint_url", key="key")
#'
#' create_classification_project(endp, "myproject")
#' create_classification_project(endp, "mymultilabelproject", multiple_tags=TRUE)
#' create_object_detection_project(endp, "myobjdetproj")
#'
#' create_classification_project(endp, "mystdproject", export_target="standard")
#'
#' list_projects(endp)
#'
#' get_project(endp, "myproject")
#'
#' update_project(endp, "myproject", export_target="vaidk")
#'
#' }
#' @aliases customvision_project
#' @rdname customvision_project
#' @export
create_classification_project <- function(endpoint, name,
                                          domain="general",
                                          export_target=c("none", "standard", "vaidk"),
                                          multiple_tags=FALSE,
                                          description=NULL)
{
    export_target <- match.arg(export_target)
    create_project(endpoint, name, domain, export_target, multiple_tags, description,
                                purpose="classification")
}


#' @rdname customvision_project
#' @export
create_object_detection_project <- function(endpoint, name,
                                            domain="general",
                                            export_target=c("none", "standard", "vaidk"),
                                            description=NULL)
{
    export_target <- match.arg(export_target)
    create_project(endpoint, name, domain, export_target, multiple_tags=FALSE, description,
                                purpose="object_detection")
}


create_project <- function(endpoint, name,
                           domain="general",
                           export_target=c("none", "standard", "vaidk"),
                           multiple_tags=FALSE,
                           description=NULL,
                           purpose=c("classification", "object_detection"))
{
    purpose <- match.arg(purpose)
    export_target <- match.arg(export_target)
    domain_id <- get_domain_id(domain, purpose, export_target)
    type <- if(purpose == "object_detection")
        NULL
    else if(multiple_tags)
        "multilabel"
    else "multiclass"

    opts <- list(
        name=name,
        domainId=domain_id,
        classificationType=type,
        description=description
    )
    obj <- call_cognitive_endpoint(endpoint, "training/projects", options=opts, http_verb="POST")

    # if export target is Vision AI Dev Kit, must do a separate update
    if(export_target == "vaidk")
        return(update_project(endpoint, id=obj$id, export_target="VAIDK"))

    make_customvision_project(obj, endpoint)
}


#' @rdname customvision_project
#' @export
list_projects <- function(endpoint)
{
    lst <- named_list(call_cognitive_endpoint(endpoint, "training/projects"))
    sapply(lst, make_customvision_project, endpoint=endpoint, simplify=FALSE)
}


#' @rdname customvision_project
#' @export
get_project <- function(endpoint, name=NULL, id=NULL)
{
    if(is.null(id))
        id <- get_project_id_by_name(endpoint, name)

    obj <- call_cognitive_endpoint(endpoint, file.path("training/projects", id))
    make_customvision_project(obj, endpoint)
}


#' @rdname customvision_project
#' @export
update_project <- function(endpoint, name=NULL, id=NULL,
                           domain="general",
                           export_target=c("none", "standard", "vaidk"),
                           multiple_tags=FALSE,
                           description=NULL)
{
    if(is.null(id))
        id <- get_project_id_by_name(endpoint, name)

    project <- get_project(endpoint, id=id)
    newbody <- list()

    if(!is.null(name) && name != project$name)
        newbody$name <- name

    if(!missing(description))
        newbody$description <- description

    newbody$settings <- project$settings

    newtarget <- !missing(export_target)
    newdomain <- !missing(domain)
    newclasstype <- !missing(multiple_tags)

    export_target <- if(newtarget)
        match.arg(export_target)
    else if(!is_compact_domain(project$settings$domainId))
        "none"
    else if(is_empty(project$settings$targetExportPlatforms))
        "standard"
    else "vaidk"

    if(newtarget || newdomain)
    {
        purpose <- get_purpose_from_domain_id(project$settings$domainId)
        newbody$settings$domainId <- get_domain_id(domain, purpose, export_target)
    }

    if(newclasstype)
        newbody$settings$classificationType <- if(multiple_tags) "Multilabel" else "Multiclass"

    if(export_target == "vaidk")
        newbody$settings$targetExportPlatforms <- I("VAIDK")

    obj <- call_cognitive_endpoint(endpoint, file.path("training/projects", id), body=newbody, http_verb="PATCH")
    make_customvision_project(obj, endpoint)
}


#' @rdname customvision_project
#' @export
delete_project <- function(object, ...)
{
    UseMethod("delete_project")
}


#' @export
delete_project.customvision_training_endpoint <- function(object, name=NULL, id=NULL, confirm=TRUE, ...)
{
    if(is.null(id))
        id <- get_project_id_by_name(object, name)

    msg <- sprintf("Are you sure you really want to delete the project '%s'?", if(!is.null(name)) name else id)
    if(!confirm_delete(msg, confirm))
        return(invisible(NULL))

    call_cognitive_endpoint(object, file.path("training/projects", id), http_verb="DELETE")
    invisible(NULL)
}


#' @export
delete_project.customvision_project <- function(object, confirm=TRUE, ...)
{
    name <- object$project$name
    id <- object$project$id
    msg <- sprintf("Are you sure you really want to delete the project '%s'?", name)
    if(!confirm_delete(msg, confirm))
        return(invisible(NULL))

    call_cognitive_endpoint(object$endpoint, file.path("training/projects", id), http_verb="DELETE")
    invisible(NULL)
}


.domain_ids <- list(
    classification=c(
        general="ee85a74c-405e-4adc-bb47-ffa8ca0c9f31",
        food="c151d5b5-dd07-472a-acc8-15d29dea8518",
        landmarks="ca455789-012d-4b50-9fec-5bb63841c793",
        retail="b30a91ae-e3c1-4f73-a81e-c270bff27c39"
    ),
    object_detection=c(
        general="da2e3a8a-40a5-4171-82f4-58522f70fbc1",
        logo="1d8ffafe-ec40-4fb2-8f90-72b3b6cecea4"
    )
)

.compact_domain_ids <- list(
    classification=c(
        general="0732100f-1a38-4e49-a514-c9b44c697ab5",
        food="8882951b-82cd-4c32-970b-d5f8cb8bf6d7",
        landmarks="b5cfd229-2ac7-4b2b-8d0a-2b0661344894",
        retail="6b4faeda-8396-481b-9f8b-177b9fa3097f"
    ),
    object_detection=c(
        general="a27d5ca5-bb19-49d8-a70a-fec086c47f5b"
    )
)


get_domain_id <- function(domain, purpose, export_target)
{
    domainlst <- if(export_target == "none") .domain_ids else .compact_domain_ids

    ids <- domainlst[[purpose]]
    i <- which(domain == names(ids))
    if(is_empty(i))
        stop(sprintf("Domain '%s' not found", domain), call.=FALSE)
    ids[i]
}


get_purpose_from_domain_id <- function(id)
{
    domainlst <- if(is_compact_domain(id)) .compact_domain_ids else .domain_ids

    i <- which(sapply(domainlst, function(domains) id %in% domains))
    names(domainlst)[i]
}


is_compact_domain <- function(id)
{
    id %in% unlist(.compact_domain_ids)
}


is_classification_project <- function(project)
{
    domain_id <- project$settings$domainId
    domains <- if(is_compact_domain(domain_id)) unlist(.compact_domain_ids) else unlist(.domain_ids)
    domain_name <- names(domains)[domains == domain_id]
    substr(domain_name, 1, 5) == "class"
}


get_project_id_by_name <- function(endpoint, name=NULL)
{
    if(is.null(name))
        stop("Either name or ID must be supplied", call.=FALSE)

    lst <- list_projects(endpoint)
    i <- which(sapply(lst, function(obj) obj$project$name == name))
    if(is_empty(i))
        stop(sprintf("Project '%s' not found", name), call.=FALSE)

    lst[[i]]$project$id
}


make_customvision_project <- function(object, endpoint)
{
    projclass <- if(is_classification_project(object))
        "classification_project"
    else "object_detection_project"
    classes <- c(projclass, "customvision_project")

    if(projclass == "classification_project")
        classes <- c(paste0(tolower(object$settings$classificationType), "_project"), classes)

    structure(list(endpoint=endpoint, project=object), class=classes)
}
