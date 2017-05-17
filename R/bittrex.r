#' @title Client for the Bittrex Crypto-Currency Exchange
#' @name bittrex-package
#' @aliases bittrex-package bittrex
#' @docType package
#' @references \url{https://bittrex.com/}
#' \url{https://github.com/kaneplusplus/bittrex}
#' @description
#' This software is in no way affiliated, endorsed, or approved by
#' the Bittrex crypto-currency exchange or any of its affiliates. It comes with
#' absolutely no warranty and should not be used in actual trading
#' unless the user can read and understand the source and know what you are
#' doing.
#' 
#' Package 'bittrex' is an R implementation of the REST interface used 
#' by the Bittrex crypto-currency exchange \url{https://bittrex.com/}. It 
#' provides functions for all endpoints currently (as of May 16, 2017) 
#' supported by the exchange. This includes the ability 
#' to retrieve price, volume, and orderbook information as well as the ability
#' to trade crypto-currencies.
#' 
#' Calls to the exchange are categorized as either public, which includes 
#' requests for price, volume, and order book information, and private, which 
#' includes all requests requiring an account including placing buy or sell 
#' orders. Public calls can be used directly by installing the package. 
#' Private calls require creating an account at 
#' \url{https://https://bittrex.com/account/Register} and creating an API and 
# secret key with appropriate permissions.
#' 
#' Private calls retrieve the API and secret key using the BITTREX_API_KEY and 
#' BITTREX_SECRET_KEY environment variables. These may be set by the user 
#' before opening the R session or, they can be set using the 
#' 'bittrex_authenticate' function.
#' 
#' Public Function Calls
#' \itemize{
#' \item{getcurrencies: }{all supported currencies at Bittrex along with other meta data}
#' \item{getmarkethistory: }{the latest trades that have occured for a specified market}
#' \item{getmarkets: }{the open and available trading markets at Bittrex along with other meta data}
#' \item{getmarketsummaries: }{the last 24 hour summary of all active exchanges}
#' \item{getmarketsummary: }{the last 24 hour summary of all active exchanges}
#' \item{getorderbook: }{the orderbook for a given market}
#' \item{getticker: }{the current tick values for a market}
#' }
#' Private Function Calls
#' \itemize{
#' \item{bittrex_authenticate: }{provide user authentication data}
#' \item{buylimit: }{place a buy limit order}
#' \item{cancel: }{cancel buy or sell order}
#' \item{getbalances: }{account balances for currencies}
#' \item{getbalance: }{account balance for a specified currency}
#' \item{getdepositaddress: }{retrieve or generate an address for a specific currency}
#' \item{getdeposithistory: }{retrieve your deposit historye
#' \item{getopenorders: }{order data for all open orders}
#' \item{getorder: }{retrieve a single order by uuid}
#' \item{getorderhistory: }{recent order history for an account }
#' \item{getwithdrawlhistory: }{retrieve your withdrawal history}
#' \item{selllimit: }{place a sell limit order}
#' \item{withdraw: }{withdraw funds from your account}
#' }
NULL

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

# PUBLIC API

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
getmarkethistory = function(market) {
  resp = content(GET(paste(public_url, 
    paste0("getmarkethistory?market=", market), sep="/")),
    type="application/json")
  if (resp$succes) {
    resp$result = result_to_df(resp$result)
    resp$result$time_stamp = timestamp_to_posix(resp$result$time_stamp)
  }
  resp
}

# PRIVATE API

#' @importFrom httr GET content add_headers
#' @importFrom openssl sha512
priv_req = function(req) {
  str_time = as.character(as.integer(Sys.time()))
  req = paste0(req, "&nonce=", str_time)
  sig = sha512(req, Sys.getenv("BITTREX_SECRET_KEY"))
  content(GET(req, add_headers(apisign=sig)), type="application/json")
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
