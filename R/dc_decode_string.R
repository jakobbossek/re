split_string = function(x, delim) {
  # build regular expression for split
  split = re::collapse(delim, sep = "|")

  # explode scheme
  exploded = strsplit(x, split = split)[[1L]]
  return(exploded)
}

split_scheme = function(scheme, delim) {
  # build regular expression for split
  exploded = split_string(scheme, delim)

  names = sapply(exploded, function(e) {
    gsub("\\{.*\\}", "", e)
  })

  # FIXME: this is horrible
  types = unname(sapply(exploded, function(e) {
    gsub("\\{|\\}", "", regmatches(e, regexec("\\{(.)\\}", e))[[1L]][1L])
  }))

  type_to_fun = c(
    "i" = as.integer,
    "c" = as.character,
    "l" = as.logical,
    "d" = as.numeric,
    "f" = as.numeric,
    "r" = as.numeric)

  converter = type_to_fun[types]
  names(converter) = names
  return(converter)
}

decode_string = function(x, scheme, delim) {
  converter = split_scheme(scheme, delim)
  n = length(converter)
  exploded = split_string(x, delim)

  decode_single_string = function(x, as.type) {
    as.type(x)
  }

  res = lapply(seq_len(n), function(i) {
    decode_single_string(exploded[i], converter[[i]])
  })

  names(res) = names(converter)
  return(res)
}
