read.edf <-
function(file) {
  
  f = file(file, "rb")
  
  header = new(
    "edf_header",
    version = as.numeric(readChar(f, 8)),
    patient_id = trim(readChar(f, 80)),
    rec_id = trim(readChar(f, 80)),
    startdate = readChar(f, 8),
    starttime = readChar(f, 8),
    header_bytes = as.numeric(readChar(f, 8)),
    reserved = trim(readChar(f, 44)),
    num_records = as.numeric(readChar(f, 8)),
    data_duration = as.double(readChar(f, 8)),
    num_signals = as.numeric(readChar(f, 4))
    )
  
  signals = new(
    "edf_signals",
    labels = sapply(1:attr(header, "num_signals"), function(x) {trim(readChar(f,16))}),
    type = sapply(1:attr(header, "num_signals"), function(x) {trim(readChar(f,80))}),
    physical_dim = sapply(1:attr(header, "num_signals"), function(x) {trim(readChar(f,8))}),
    physical_min = sapply(1:attr(header, "num_signals"), function(x) {as.numeric(readChar(f,8))}),
    physical_max = sapply(1:attr(header, "num_signals"), function(x) {as.numeric(readChar(f,8))}),
    digital_min = sapply(1:attr(header, "num_signals"), function(x) {as.numeric(readChar(f,8))}),
    digital_max = sapply(1:attr(header, "num_signals"), function(x) {as.numeric(readChar(f,8))}),
    pre_filtering = sapply(1:attr(header, "num_signals"), function(x) {trim(readChar(f,80))}),
    num_samples = sapply(1:attr(header, "num_signals"), function(x) {as.numeric(readChar(f,8))}),
    reserved = sapply(1:attr(header, "num_signals"), function(x) {trim(readChar(f,32))})
    )

  edfplus = ifelse(substr(attr(header,"reserved"),1,4)=="EDF+",T,F)
  if (edfplus)
    attr(signals, "num_samples")[which(attr(signals, "labels")=="EDF Annotations")] = attr(signals, "num_samples")[which(attr(signals, "labels")=="EDF Annotations")]*2
  
  nsamp = sum(attr(signals, "num_samples"))*attr(header, "num_records")
  
  recs = rep(NA, nsamp)
  sigs = rep(NA, nsamp)
  samps = rep(NA, nsamp)
  tsamps = rep(NA, nsamp)
  vals = rep(NA, nsamp)
  
  start = 1
  end = 1
  for (r in 1:attr(header, "num_records")) {
    num_signals = length(attr(signals, "labels"))
    for (s in 1:num_signals) {
      signal = attr(signals, "labels")[s]
      num_samples = attr(signals, "num_samples")[s]
      end = start + num_samples - 1
      tstart = (r-1)*num_samples + 1
      tend = tstart + num_samples -1
      bin = NULL
      if (signal == "EDF Annotations") {
        bin = sapply(readBin(f, raw(), num_samples), rawToChar)
      } else {
        bin = readBin(f, integer(), num_samples, size=2)
      }
      recs[start:end] = rep(r, num_samples)
      sigs[start:end] = rep(signal, num_samples)
      samps[start:end] = 1:num_samples
      tsamps[start:end] = tstart:tend
      vals[start:end] = as.character(bin)
      start = end + 1
    }
  }
  
  data = data.frame(record = recs, signal = sigs, sample = samps, tsample = tsamps, value = vals)
  rate = attr(header, "data_duration") * 60 / max(data$tsample)
  data$time = (data$tsample-1) * rate
  annotations = data[data$signal=="EDF Annotations",]
  data = data[data$signal!="EDF Annotations",]
  data$value = as.numeric(as.character(data$value))

  close(f)
  
  new(
    "edf",
    header = header,
    signals = signals,
    data = data,
    annotations = annotations
    )

}

