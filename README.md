# AzureVision <img src="man/figures/logo.png" align="right" width=150 />

[![Build Status](https://asiadatascience.visualstudio.com/AzureR/_apis/build/status/Azure.AzureVision?branchName=master)](https://asiadatascience.visualstudio.com/AzureR/_build/latest?definitionId=13&branchName=master)

An R frontend to [Azure Computer Vision](https://azure.microsoft.com/services/cognitive-services/computer-vision/) and [Azure Custom Vision](https://azure.microsoft.com/services/cognitive-services/custom-vision-service/), building on the low-level functionality provided by the [AzureCognitive](https://github.com/Azure/AzureCognitive) package.

## Computer Vision

To communicate with the Computer Vision service, call the `computervision_endpoint` function with the service URL and key. Rather than a key, you can also supply an OAuth token obtained with the AzureAuth package.

```r
library(AzureVision)

vis <- computervision_endpoint(
    url="https://accountname.cognitiveservices.azure.com/",
    key="account_key"
)

# images can be specified as a filename, Internet URL, or raw vector
bill_url <- "https://news.microsoft.com/uploads/2014/09/billg1_print.jpg"
analyze(vis, bill_url)
# $categories
#      name    score
# 1 people_ 0.953125

describe(vis, bill_url)
# $tags
#  [1] "person"   "man"      "suit"     "clothing" "necktie"  "wearing"  "glasses"  "looking"  "holding"  "standing" "older"   
# [12] "posing"   "business" "old"      "dressed"  "front"    "sitting"  "black"    "hat"      "white"    "sign"     "phone"   

# $captions
#                                text confidence
# 1 Bill Gates wearing a suit and tie  0.9954072
```

## Custom Vision

To communicate with the Custom Vision service, call the `customvision_endpoint` function with the service URL and key.

----
<p align="center"><a href="https://github.com/Azure/AzureR"><img src="https://github.com/Azure/AzureR/raw/master/images/logo2.png" width=800 /></a></p>

