setClass(
  "edf_header",
  representation(
    version = "numeric",
    patient_id = "character",
    rec_id = "character",
    startdate = "character",
    starttime = "character",
    header_bytes = "numeric",
    reserved = "character",
    num_records = "numeric",
    data_duration = "numeric",
    num_signals = "numeric"
    )
  )

setClass(
  "edf_signals",
  representation(
    labels = "character",
    type = "character",
    physical_dim = "character",
    physical_min = "numeric",
    physical_max = "numeric",
    digital_min = "numeric",
    digital_max = "numeric",
    pre_filtering = "character",
    num_samples = "numeric",
    reserved = "character"
    )
  )

setClass(
  "edf",
  representation(
    "header" = "edf_header",
    "signals" = "edf_signals",
    "data" = "data.frame",
    "annotations" = "data.frame"
    )
  )