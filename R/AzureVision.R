#' @import AzureRMR
#' @import AzureCognitive
NULL

#' @export
AzureCognitive::cognitive_endpoint

#' @export
AzureCognitive::call_cognitive_endpoint

utils::globalVariables("id")


.onLoad <- function(libname, pkgname)
{
    options(azure_computervision_api_version="v2.1")
    options(azure_customvision_training_api_version="v3.1")
    options(azure_customvision_prediction_api_version="v3.0")
}


confirm_delete <- function(msg, confirm)
{
    if(!interactive() || !confirm)
        return(TRUE)

    ok <- if(getRversion() < numeric_version("3.5.0"))
    {
        msg <- paste(msg, "(yes/No/cancel) ")
        yn <- readline(msg)
        if(nchar(yn) == 0)
            FALSE
        else tolower(substr(yn, 1, 1)) == "y"
    }
    else utils::askYesNo(msg, FALSE)
    isTRUE(ok)
}

