camel_to_snake = function(x) {
  gsub("^_+", "", tolower(gsub("([A-Z])", "_\\1", x)))
}

as_data_frame = function(x) {
  ret = as.data.frame(x, stringsAsFactors=FALSE)
  names(ret) = camel_to_snake(names(ret))
  ret
}

result_to_df= function(result) {
  for(i in seq_along(result)) {
    for (j in seq_along(result[[i]]))
      if (is.null(result[[i]][[j]]))
        result[[i]][[j]] = NA
  }
  result = Reduce(rbind, Map(as_data_frame, result))
  names(result) = camel_to_snake(names(result))
  result
}

timestamp_to_posix = function(x) {
  as.POSIXct(strptime(x, format="%Y-%m-%dT%H:%M:%OS", tz="GMT"))
}

public_url = "https://bittrex.com/api/v1.1/public"
account_url = "https://bittrex.com/api/v1.1/account"
market_url = "https://bittrex.com/api/v1.1/market"

