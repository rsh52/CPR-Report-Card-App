# Cicerone Setup

library(cicerone)

guide <- Cicerone$
  new()$ 
  step(
    el = "downloadSOP",
    title = "Download Instructions",
    description = "Click here to download the instructions for the application."
  )$
  step(
    "downloadCCs",
    "Download Sample File",
    "Click here to download a sample post-processed compression file."
  )$
  step(
    "ageinput",
    "Age Input",
    "Specify the age of the patient under analysis."
  )$
  step(
    "epochfx",
    "Specify Epoch Function",
    "Toggle the type of statistical summary for application to the rolling window."
  )$
  step(
    "epochwin",
    "Epoch Window Increment",
    "Specify the rolling window to be applied for epoch specification."
  )$
  step(
    "main",
    "Results and User Guide",
    "Toggle between tabs to display results of file upload or additional user information.",
    tab_id = "main",
    tab = "Data"
  )