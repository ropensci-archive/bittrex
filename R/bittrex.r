
camel_to_snake = function(x) {
  gsub("^_+", "", tolower(gsub("([A-Z])", "_\\1", x)))
}

as_data_frame = function(x) {
  ret = as.data.frame(x, stringsAsFactors=FALSE)
  names(ret) = camel_to_snake(names(ret))
  ret
}

result_to_df= function(result) {
  for(i in 1:length(result)) {
    for (j in 1:length(result[[i]]))
      if (is.null(result[[i]][[j]]))
        result[[i]][[j]] = NA
  }
  result = Reduce(rbind, Map(as_data_frame, result))
  names(result) = camel_to_snake(names(result))
  result
}

timestamp_to_posix = function(x) {
  strptime(x, format="%Y-%m-%dT%H:%M:%OS", tz="GMT")
}

public_url = "https://bittrex.com/api/v1.1/public"

account_url = "https://bittrex.com/api/v1.1/account"

market_url = "https://bittrex.com/api/v1.1/market"

#' @importFrom httr GET content add_headers
#' @importFrom openssl sha512
priv_req = function(req) {
  str_time = as.character(as.integer(Sys.time()))
  req = paste0(req, "&nonce=", str_time)
  sig = sha512(req, Sys.getenv("BITTREX_SECRET_KEY"))
  content(GET(req, add_headers(apisign=sig)), type="application/json")
}

#' @importFrom httr GET content
#' @export
getmarkets = function() {
  resp = content(GET(paste(public_url, "getmarkets", sep="/")),
    type="application/json")
  if (resp$success) resp$result = result_to_df(resp$result)
  resp
}

#' @importFrom httr GET content
#' @export
getcurrencies = function() {
  resp = content(GET(paste(public_url, "getcurrencies", sep="/")),
    type="application/json")
  if (resp$success) resp$result = result_to_df(resp$result)
  resp
}

#' @importFrom httr GET content
#' @export
getticker = function(market) {
  resp = content(GET(
    paste(public_url, paste0("getticker?market=", market), sep="/")),
    type="application/json")
  if (resp$success) resp$result = as_data_frame(resp$result)
  resp
}

#' @importFrom httr GET content
#' @export
getmarketsummaries = function() {
  resp = content(GET(
    paste(public_url, "getmarketsummaries", sep="/")),
    type="application/json")
  if (resp$success) {
    resp$result = result_to_df(resp$result)
    resp$result$time_stamp = timestamp_to_posix(resp$result$time_stamp)
    resp$result$created = timestamp_to_posix(resp$result$created)
  }
  resp
}

#' @importFrom httr GET content
#' @export
getmarketsummary = function(market) {
  resp = content(GET(paste(public_url,
    paste0("getmarketsummary?market=", market), sep="/")),
    type="application/json")
  if (resp$success) {
    resp$result = as_data_frame(resp$result)
    resp$result$time_stamp = timestamp_to_posix(resp$result$time_stamp)
    resp$result$created= timestamp_to_posix(resp$result$created)
  }
  resp
}

#' @importFrom httr GET content
#' @export
getorderbook = function(market, type=c("both", "buy", "sell"), depth=50) {
  resp = content(GET(paste(public_url, 
    paste0("getorderbook?market=", market, "&type=", type[1], "&depth=", depth),    sep="/")), type="application/json")
  if (resp$success) {
    if (any(c("both", "buy") %in% names(resp$result))) {
      buy = Reduce(rbind, Map(as_data_frame, resp$result$buy))
      names(buy) = tolower(names(buy))
    }
    if (any(c("both", "sell") %in% names(resp$result))) {
      sell= Reduce(rbind, Map(as_data_frame, resp$result$sell))
      names(sell) = tolower(names(sell))
    }
    if (any(c("both", "buy") %in% names(resp$result)))
      resp$result$buy = buy
    if (any(c("both", "sell") %in% names(resp$result)))
      resp$result$sell = sell
  }
  resp
}

#' @importFrom httr GET content
#' @export
getmarkethistory = function(market, count=20) {
  resp = content(GET(paste(public_url, 
    paste0("getmarkethistory?market=", market, "&count=", count), sep="/")),
    type="application/json")
  if (resp$succes) {
    resp$result = result_to_df(resp$result)
    resp$result$time_stamp = timstamp_to_posix(resp$result$time_stamp)
  }
  resp
}

#' @export
bittrex_authenticate = function(api_key, secret_key) {
  Sys.setenv("BITTREX_API_KEY"=api_key)
  Sys.setenv("BITTREX_SECRET_KEY"=secret_key)
  invisible(TRUE)
}

#' @import from httr GET content
#' @export
buy = function(market, quantity, type=c("market", "limit"), rate) {
  req = market_url
  if (type[1] == "market") {
    if (!missing(rate)) {
      warning("Rate parameter is ignored for market orders.")
    }
    req = paste(req, 
      paste0("buymarket?apikey=", Sys.getenv("BITTREX_API_KEY"),
             "&market=", market, "&quantity=", quantity), sep="/")
  } else if (type[1] == "limit") {
    if (missing(rate)) {
      stop("Rate must be specified for limit orders.")
    }
    req = paste(req, 
      paste0("buylimit?apikey=", Sys.getenv("BITTREX_API_KEY"),
             "&market=", market, "&quantity=", quantity,
             "&rate=", rate), sep="/")
  } else {
    stop("Unknown buy type")
  }
  priv_req(req)
}

#' @import from httr GET content
#' @export
sell = function(market, quantity, type=c("market", "limit"), rate) {
  req = market_url
  if (type[1] == "market") {
    if (!missing(rate)) {
      warning("Rate parameter is ignored for market orders.")
    }
    req = paste(req, 
      paste0("sellmarket?apikey=", Sys.getenv("BITTREX_API_KEY"),
             "&market=", market, "&quantity=", quantity), sep="/")
  } else if (type[1] == "limit") {
    if (missing(rate)) {
      stop("Rate must be specified for limit orders.")
    }
    req = paste(req, 
      paste0("selllimit?apikey=", Sys.getenv("BITTREX_API_KEY"),
             "&market=", market, "&quantity=", quantity,
             "&rate=", rate), sep="/")
  } else {
    stop("Unknown buy type")
  }
  priv_req(req)
}

#' @import from httr GET content
#' @export
cancel = function(uuid) {
  req = paste(market_url, 
    paste0("cancel?apikey=", Sys.getenv("BITTREX_API_KEY"),
           "&uuid=", uuid), sep="/")
  priv_req(req)
}

#' @import from httr GET content
#' @export
getopenorders = function(market) {
  if (missing(market)) {
    req = paste(market_url, 
      paste0("getopenorders?apikey=", Sys.getenv("BITTREX_API_KEY")), sep="/")
  } else {
    req = paste(market_url, 
      paste0("getopenorders?apikey=", Sys.getenv("BITTREX_API_KEY"),
             "&market=", market), sep="/")
  }
  ret = priv_req(req)
  # TODO: check that this right
  if ( (ret$sucess == TRUE) && (length(ret$result) > 0)) {
    ret$result = result_to_df(ret$result)
  }
  ret
}

#' @import from httr GET content
#' @export
getbalances = function() {
  req = paste(account_url,
    paste0("getbalances?apikey=", Sys.getenv("BITTREX_API_KEY")), sep="/")
  ret = priv_req(req)
  if (ret$success) {
    if (length(ret$result) == 1)
      ret$result = as_data_frame(ret$result)
    else if (length(ret$result) > 1) 
      ret$result = result_to_df(ret$result)
  }
  ret
}
