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
#' by the Bittrex crypto-currency exchange (\url{https://bittrex.com/}). It 
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
#' \item{buy: }{place a buy limit order}
#' \item{cancel: }{cancel buy or sell order}
#' \item{getbalances: }{account balances for currencies}
#' \item{getbalance: }{account balance for a specified currency}
#' \item{getdepositaddress: }{retrieve or generate an address for a specified 
#'  currency}
#' \item{getdeposithistory: }{retrieve your deposit history}
#' \item{getopenorders: }{order data for all open orders}
#' \item{getorder: }{retrieve a single order by uuid}
#' \item{getorderhistory: }{recent order history for an account }
#' \item{getwithdrawlhistory: }{retrieve your withdrawal history}
#' \item{sell: }{place a sell limit order}
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
  strptime(x, format="%Y-%m-%dT%H:%M:%OS", tz="GMT")
}

public_url = "https://bittrex.com/api/v1.1/public"
account_url = "https://bittrex.com/api/v1.1/account"
market_url = "https://bittrex.com/api/v1.1/market"

# PUBLIC API

#' @title Available Markets and Other Meta Data
#' @description The \code{getmarkets} function returns all of the available
#' markets currently available currently available on the 
#' Bittrex crypto-currency exchange (\url{https://bittrex.com}) along with
#' other information including, among other information, when the exchange
#' was created and the minimum order size.
#' @references \url{https://bittrex.com/api/v1.1/public/getmarkets}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{A \code{data.frame} with the market currencies, 
#'    base currencies, base currency long name, minimum trade size, maket name,
#'    if the market is active, when the market was created, market notices,
#'    if the market is sponsored, and the location of the market logo.}
#' }
#' @examples
#' \dontrun{
#' markets = getmarkets()$result
#' head(markets)
#' }
#' @importFrom httr GET content
#' @export
getmarkets = function() {
  resp = content(GET(paste(public_url, "getmarkets", sep="/")),
    type="application/json")
  if (resp$success) resp$result = result_to_df(resp$result)
  resp
}

#' @title Retrieve all Available Currencies on the Exchange
#' @description The \code{getcurrencies} function returns the available
#' currencies on the Bittrex crypto-currency exchange 
#' (\url{https://bittrex.com}).
#' @references \url{https://bittrex.com/api/v1.1/public/getcurrencies}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{A \code{data.frame} with the currency ticker, 
#'    currency, a minimum confirmation number, the transaction fee, if the
#'    currency is active, the coin type, the base address, and currency 
#'    notices.}
#' }
#' @examples
#' \dontrun{
#' currencies = getcurrencies()$result
#' head(markets)
#' }
#' @importFrom httr GET content
#' @export
getcurrencies = function() {
  resp = content(GET(paste(public_url, "getcurrencies", sep="/")),
    type="application/json")
  if (resp$success) resp$result = result_to_df(resp$result)
  resp
}

#' Get the Ticker Values for a Market
#' @description The \code{getticker} function returns the bid, ask, and last
#' transaction price for a specified market on the Bittrex crypto-currency 
#' exchange (\url{https://bittrex.com}).
#' @references \url{https://bittrex.com/api/v1.1/public/getticker}
#' @param market the market to get the ticker for.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{A \code{data.frame} with the bid, ask, and last 
#'                   trasaction price.}
#' }
#' @examples
#' \dontrun{
#' getticker("btc-ltc")
#' head(markets)
#' }
#' @importFrom httr GET content
#' @export
getticker = function(market) {
  resp = content(GET(
    paste(public_url, paste0("getticker?market=", market), sep="/")),
    type="application/json")
  if (resp$success) resp$result = as_data_frame(resp$result)
  resp
}

#' @title Summary of All Active Markets
#' @description the \code{getmarketsummaries} retrieves a summary of all
#' active markets on the Bittrex crypto-currency 
#' exchange (\url{https://bittrex.com}) for the last 24 hours.
#' @references \url{https://bittrex.com/api/v1.1/public/getmarketsummaries}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{A \code{data.frame} with one row per market and, for
#'                   each market: the market name, the high, the low, the
#'                   volume, the last trade, the last trade price, the 
#'                   base currency volume, a time stamp for the last 
#'                   transaction, the current bid, the current ask, the number
#'                   of open buy orders, the number of open sell orders, the
#'                   the previous day close, and when the market was created.
#'  }
#' }
#' @examples
#' \dontrun{
#' ms = getmarketsummaries()$result
#' head(ms)
#' }
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

#' @title Check the Connection to the Bittrex Exchange
#' @description The \code{bittrex_api_check} function checks to see
#' if you can sucessfully connect to the 
#' Bittrex crypto-currency exchange 
#' \url{https://bittrex.com}. 
#' @param warn if the request is not successful, should a warning be provided
#' with the status code.
#' @return A named logical indicating if you can connect to the exchange
#' through the public interface.
#' @examples
#' \dontrun{
#' bittrex_api_check()
#' }
#' @importFrom httr GET status_code
#' @export
bittrex_api_check = function(warn=TRUE) {
  resp = GET(paste(public_url, "getmarketsummaries", sep="/"))
  if (status_code(resp) != 200) {
    if (warn) warning(paste("Status code:", status_code(resp)))
  } else {
    TRUE
  }
}

#' @title Summary of a Markets
#' @description the \code{getmarketsummary} retrieves a summary for a specified
#' markets on the Bittrex crypto-currency 
#' exchange (\url{https://bittrex.com}).
#' @references \url{https://bittrex.com/api/v1.1/public/getmarketsummary}
#' @param market the market to retrieve the summary for.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{A \code{data.frame} with one row and columns corresponding
#'                   to: the market name, the high, the low, the
#'                   volume, the last trade, the last trade price, the 
#'                   base currency volume, a time stamp for the last 
#'                   transaction, the current bid, the current ask, the number
#'                   of open buy orders, the number of open sell orders, the
#'                   the previous day close, and when the market was created.
#'  }
#' }
#' @examples
#' \dontrun{
#' ms = getmarketsummary("btc-eth")$result
#' head(ms)
#' }
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

#' @title Order Book for a Market
#' @description The \code{getorderbook} function returns the order book 
#' for a specified market on the Bittrex crypto-currency 
#' exchange (\url{https://bittrex.com}).
#' @references \url{https://bittrex.com/api/v1.1/public/getorderbook?market=BTC-LTC&type=both&depth=50}
#' @param market the market from which the order book will be retrieved.
#' @param type type of orders to retrieve (default is "both")
#' @param depth how deep should the returned order book be (default and 
#' maximum are 50).
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{A named list with the buy and sell orders (depending
#'    on the specified \code{type} parameter. If \code{type} is "buy" or
#'    "both" then the list will contain a element named "buy" with
#'    a \code{data.frame} of the buy orders.}
#' }
#' @examples
#' \dontrun{
#' ob = getorderbook("usd-btc")$result
#' head(ob$buy)
#' head(ob$sell)
#' }
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

#' @title Recent History for a Market
#' @description the \code{getmarkethistory} function retrieves recent trade
#' information for a specified market on the Bittrex crypto-currency exchange 
#' (\url{https://bittrex.com}).
#' @references \url{https://bittrex.com/api/v1.1/public/getmarkethistory?market=BTC-DOGE}
#' @param market the market from which history data will be retrieved.
#' is 100).
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{A code{data.frame} containing recent trade information
#'    including the order type, time, quantity, price, and fill type.}
#' }
#' @examples
#' \dontrun{
#' mh = getmarkethistory("usd-btc")$result
#' head(mh)
#' }
#' @importFrom httr GET content
#' @export
getmarkethistory = function(market) {
  resp = content(GET(paste(public_url, 
    paste0("getmarkethistory?market=", market), sep="/")),
    type="application/json")
  if (resp$succes) {
    if (length(resp$result) > 0) {
      resp$result = result_to_df(resp$result)
      resp$result$time_stamp = timestamp_to_posix(resp$result$time_stamp)
    } else {
      resp$result = NULL
    }
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

#' @title Provide User Authentication Data
#' @description The \code{bittrex_authenicate} function sets the 
#' BITTREX_API_KEY and BITTREX_SECRET_KEY environment variables in your current
#' session to access your account information on the Bittrex crypto-currency
#' exchange (\url{https://bittrex.com}).
#' @param api_key the api key provided by the exchange
#' @param secret_key the secret key provided by the exchange
#' @export
bittrex_authenticate = function(api_key, secret_key) {
  Sys.setenv("BITTREX_API_KEY"=api_key)
  Sys.setenv("BITTREX_SECRET_KEY"=secret_key)
  invisible(TRUE)
}

#' @title Place a Buy Limit Order
#' @description The \code{buy} function places a buy order onto the 
#' Bittrex crypto-currency exchange \url{https://bittrex.com}. This function
#' only works after you have set up authentication.
#'
#' NOTE: market orders are currently disabled.
#' @seealso \code{link{bittrex_authenticate}} \code{\link{sell}}
#' \code{\link{getorder}} \code{\link{getopenorders}} 
#' \code{\link{getorderhistory}}
#' @references \url{https://bittrex.com/api/v1.1/market/buylimit}
#' @param market the market to place the buy limit order on.
#' @param quantity the quantity of the transaction currency to buy.
#' @param rate the price you are willing to pay per unit of the 
#' transaction currency.
#' @param type either "market" or "limit". Note that market orders are currently
#' disabled.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{a named list, with element "uuid" whose element is an 
#'    integer identifying the order. This value is used to query the status of
#'    of the order with either the \code{link{getorder}} or 
#'    \code{link{getopenorders}} function. When the order is fulfilled it
#'    appears in the order history \code{data.frame} returned by the
#'    \code{link{getorderhistory}} function.
#'  }
#' }
#' @examples
#' \dontrun{
#' # Buy one litecoin for 1169 bitcoins. 
#' order = buy("btc-ltc", 1, 0.0001)
#' }
#' @export
buy = function(market, quantity, rate, type=c("limit", "market")) {
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

#' @title Place a Sell Limit Order
#' @description The \code{sell} function places a buy order onto the 
#' C-Cex crypto-currency exchange \url{https://bittrex.com}. This function
#' only works if you have set up authentication. 
#' 
#' NOTE: market orders are currently disabled.
#' @seealso \code{link{bittrex_authenticate}} \code{\link{buy}}
#' \code{\link{getopenorders}} \code{\link{getorderhistory}}
#' @references \url{https://bittrex.com/api/v1.1/market/selllimit}
#' @param market the market to place the buy limit order on.
#' @param quantity the quantity of the reference currency to sell. 
#' @param rate the price you would like to get per unit of the 
#' transaction 
#' currency.
#' @param type either "market" or "limit". Note that market orders are 
#' discouraged. (default is limit)
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{a named list, called "uuid" whose element is an integer
#'    identifying the order. This value is used to query the status of
#'    of the order with either the \code{link{getorder}} or 
#'    \code{link{getopenorders}} function. When the order is fulfilled it
#'    appears in the order history \code{data.frame} returned by the
#'    \code{link{getorderhistory}} function.
#'  }
#' }
#' @examples
#' \dontrun{
#' # Sell one litecoin for 0.000001 bitcoins. 
#' order = sell("btc-ltc", 1, 0.000001)
#' }
#' @export
sell = function(market, quantity, rate, type=c("limit", "market")) {
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

#' @title Cancel an Open Order
#' @description The \code{cancel} function cancels an open order on the
#' C-Cex crypto-currency exchange \url{https://bittrex.com}. This function
#' is called after providing information to authenticate your account and 
#' after an order is placed using either 
#' the \code{link{buy}} or \code{link{sell}} functions.
#' @seealso \code{\link{bittrex_authenticate}} \code{\link{getopenorders}}
#' @references \url{https://bittrex.com/api/v1.1/account/cancel}
#' @param uuid the uuid of the order you would like to cancel.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{always NULL}
#' }
#' @examples
#' \dontrun{
#' cancel(uuid) 
#' }
#' @export
cancel = function(uuid) {
  req = paste(market_url, 
    paste0("cancel?apikey=", Sys.getenv("BITTREX_API_KEY"),
           "&uuid=", uuid), sep="/")
  priv_req(req)
}

#' @title Order Data for all Open Orders
#' @description The \code{getopenorders} function retrieves all open orders
#' on the Bittrex crypto-currency 
#' exchange \url{https://bittrex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{bittrex_authenticate}} 
#' @references \url{https://bittrex.com/api/v1.1/market/getopenorders}
#' @param market (optional) the market on which you would like to see all 
#' open orders. If not specified, then all open orders
#' for all markets are returned.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{a \code{data.frame} providing information about the 
#'    open orders including (but not limited to) the market, quantity remaining
#'    in the order, the type of order, and when the order was opened.
#'  }
#' }
#' @examples
#' \dontrun{
#' getopenorders()
#' }
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
  if ( (ret$success == TRUE) && (length(ret$result) > 0)) {
    ret$result = result_to_df(ret$result)
  }
  ret
}

#' Account Balances for All Currencies
#' @description The \code{getbalances} function retrieves the account balance
#' for all currencies on the Bittrex crypto-currency 
#' exchange \url{https://bittrex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{bittrex_authenticate}}
#' @references \url{https://bittrex.com/api/v1.1/account/getbalances}
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{a \code{data.frame} with the currencies, balances, 
#'    available funds, the amount of any pending transactions, and 
#'    crypographic addresses that can be used to receive funding.
#'  }
#' }
#' @examples
#' \dontrun{
#' balances = getbalances()$result
#' }
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

#' Account Balance for a Specified Currency
#' @description The \code{getbalance} function retrieves the account balance
#' for a specified currency on the Bittrex crypto-currency 
#' exchange \url{https://bittrex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{bittrex_authenticate}}
#' @references \url{https://bittrex.com/api/v1.1/account/getbalances}
#' @param currency currency to retrieve the account balance for.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{a \code{data.frame} with the currency, balance, 
#'    available funds, the amount of any pending transactions, and 
#'    crypographic addresses that can be used to receive funding.
#'  }
#' }
#' @examples
#' \dontrun{
#' balances = getbalance("btc")$result
#' }
#' @export
getbalance = function(currency) {
  req = paste(account_url,
    paste0("getbalance?apikey=", Sys.getenv("BITTREX_API_KEY"), 
      "&currency=", currency), sep="/")
  ret = priv_req(req)
  if (ret$success) {
    ret$result = as_data_frame(ret$result)
  }
  ret
}

#' @title Retrieve the Address for a Specified Currency
#' @description The \code{getdepositaddress} retrieves or creates the account 
#' deposit address for a specified currency.
#' @references \url{https://bittrex.com/api/v1.1/account/getdepositaddress?apikey=API_KEY&currency=VTC}
#' @param currency currency to get the deposit address
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string. If an 
#'                   address has not been generated this field will have value
#'                   "ADDRESS_GENERATING" until it is available.}
#'  \item{result:  }{a \code{data.frame} with the one row and columns 
#'                   providing the currency and the address.}
#' }
#' @examples
#' \dontrun{
#' # Get the address of the bitcoin currency.
#' getdepositaddress("btc")
#' }
#' @export
getdepositaddress = function(currency) {
  req = paste(account_url,
    paste0("getdepositaddress?apikey=", Sys.getenv("BITTREX_API_KEY"),
      "&currency=", currency), sep="/")
  ret = priv_req(req)
  if (ret$success) {
    ret$result = as_data_frame(ret$result)    
  }
  ret
}

#' @title Withdraw Funds from an Account
#' @description The \code{withdraw} function moves funds from a 
#' Bittrex (\url{https://bittrex.com} account to a specified address.
#' It does not include the transaction fee.
#' @seealso \code{link{currency}}
#' @references \url{https://bittrex.com/api/v1.1/account/withdraw}
#' @param currency the currency to withdraw.
#' @param quantity the quantity of the currency to withdraw.
#' @param address where to send the funds.
#' @param paymentid CryptoNotes/BitShareX/Nxt optional field (memo/paymentid).
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{a named list, with element "uuid" whose element is an 
#'    integer identifying the order.}
#'  }
#' @examples
#' \dontrun{
#' # Send the author your bitcoins.
#' widthdraw("btc", 10, "1Q6WissSMNF7NCNw3sDXQ2F7AbrSCYouj2")
#' }
#' @export
withdraw = function(currency, quantity, address, paymentid) {
  req = paste(account_url, 
    paste0("withdraw?apikey=", Sys.getenv("BITTREX_API_KEY"),
      "&currency=", currency, "&quantity=", quantity, "&address=", address),
      sep="/")
  if (!missing(paymentid))
    req = paste0(req, "&paymentid=", paymentid)
  priv_req(req)
}

#' @title Order Data for a Specified Order
#' @description The \code{getorder} function retrieves open order data 
#' on the Bittrex crypto-currency 
#' exchange \url{https://bittrex}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{bittrex_authenticate}} \code{\link{getopenorders}}.
#' @references \url{https://bittrex.com/api/v1.1/account/getorder&apikey=API_KEY&uuid=0cb4c4e4-bdc7-4e13-8c13-430e587d2cc1}
#' @param uuid the uuid of the order.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a \code{data.frame} providing information about the 
#'    open order including (but not limited to) the market, quantity remaining
#'    in the order, the type of order, and when the order was opened.
#'  }
#' }
#' @examples
#' \dontrun{
#' getorder(uuid)
#' }
#' @export
getorder = function(uuid) {
  req = paste(account_url, paste0("getorder?apikey=", 
    Sys.getenv("BITTREX_API_KEY"), "&uuid=", uuid), sep="/")
  resp = priv_req(req)
  ret = NULL
  if (length(resp$result) > 0) {
    for(i in seq_along(resp$result)) {
      for (j in seq_along(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) resp$result[[i]][[j]] = NA
      }
    }
    ret = Reduce(rbind, Map(as_data_frame, resp$result))
    names(ret) = camel_to_snake(names(ret))
  }
  resp$result = ret
  resp
}

#' @title Order History for an Account
#' @description The \code{getorderhistory} function retrieves order history
#' data on the Bittrex crypto-currency exchange \url{https://bittrex.com}. This 
#' function can be used after you provide authentication information.
#' @seealso \code{\link{bittrex_authenticate}}
#' @references \url{https://bittrex.com/api/v1.1/account/getorderhistory}
#' @param market (optional) the market on which you would like to see all 
#' open orders. If not specified, then completed orders for all markets are 
#' returned.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a \code{data.frame} providing data about 
#'    previously completed orders including the order uuid, the exchange
#'    the time of the order, the order type, the limit, the quantity, the
#'    quantity remaining, the commission, the price, the price per unit,
#'    and whether or not it was a conditional trade.
#'  }
#' }
#' @examples
#' \dontrun{
#' getorderhistory()
#' }
#' @export
getorderhistory = function(market) {
  req = paste(account_url, paste0("getorderhistory?apikey=",
    Sys.getenv("BITTREX_API_KEY")), sep="/")
  if (!missing(market)) 
    req = paste0(req, "&markte=", market)
  resp = priv_req(req)
  ret = list()
  if (length(resp$result) > 0) {
    for(i in seq_along(resp$result)) {
      for (j in seq_along(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) resp$result[[i]][[j]] = NA
      }
    }
    ret = Reduce(rbind, Map(as_data_frame, resp$result))
    names(ret) = camel_to_snake(names(ret))
    ret$time_stamp = strptime(ret$time_stamp, "%Y-%m-%d %H:%M:%OS", tz="GMT")
  }
  resp$result = ret
  resp
}

#' @title Retrieve Withdrawal History
#' @description The \code{getwithdrawalhistory} function retrieves the
#' withdraw history for an account on the Bittrex crypto-currency exchange
#' \url{https://bittrex.com}. This function can be used after you 
#' provide authentication information.
#' @seealso \code{link{bittrex_authenticate}}
#' @references \url{https://bittrex.com/api/v1.1/account/getwithdrawalhistory?apikey=API_KEY?currency=BTC}
#' @param currency (optional) the currency to retrieve the withdraw for. If this
#' is not specified then withdraw history for all currencies is retrieved.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a \code{data.frame} providing data about 
#'    previously completed orders including the order uuid, the currency,
#'    the time of the withdraw, the quantity, etc.
#'  }
#' }
#' @examples
#' \dontrun{
#' getwithdrawalhistory()
#' }
#' @export
getwithdrawalhistory = function(currency) {
  req = paste(account_url, paste0("getwithdrawalhistory?apikey=",
    Sys.getenv("BITTREX_API_KEY")), sep="/")
  if (!missing(currency)) 
    req = paste0(req, "&currency=", currency)
  resp = priv_req(req)
  ret = list()
  if (length(resp$result) > 0) {
    for(i in seq_along(resp$result)) {
      for (j in seq_along(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) resp$result[[i]][[j]] = NA
      }
    }
    ret = Reduce(rbind, Map(as_data_frame, resp$result))
    names(ret) = camel_to_snake(names(ret))
    ret$time_stamp = strptime(ret$time_stamp, "%Y-%m-%d %H:%M:%OS", tz="GMT")
  }
  resp$result = ret
  resp
}

#' @title Retrieve Deposit History
#' @description The \code{getdeposithistory} function retrieves the
#' deposit history for an account on the Bittrex crypto-currency exchange
#' \url{https://bittrex.com}. This function can be used after you 
#' provide authentication information.
#' @seealso \code{link{bittrex_authenticate}}
#' @references \url{https://bittrex.com/api/v1.1/account/getdeposithistory?apikey=API_KEY?currency=BTC}
#' @param currency (optional) the currency to retrieve the deposits for. If this
#' is not specified then deposit history for all currencies is retrieved.
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string."}
#'  \item{result:  }{a \code{data.frame} providing data about 
#'    previously completed orders including the order uuid, the currency,
#'    the time of the withdraw, the quantity, etc.
#'  }
#' }
#' @examples
#' \dontrun{
#' getdeposithistory()
#' }
#' @export
getdeposithistory = function(currency) {
  req = paste(account_url, paste0("getdeposithistory?apikey=",
    Sys.getenv("BITTREX_API_KEY")), sep="/")
  if (!missing(currency)) 
    req = paste0(req, "&currency=", currency)
  resp = priv_req(req)
  ret = list()
  if (length(resp$result) > 0) {
    for(i in seq_along(resp$result)) {
      for (j in seq_along(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) resp$result[[i]][[j]] = NA
      }
    }
    ret = Reduce(rbind, Map(as_data_frame, resp$result))
    names(ret) = camel_to_snake(names(ret))
    ret$last_updated = strptime(ret$last_updated, "%Y-%m-%dT%H:%M:%OS",tz="GMT")
  }
  resp$result = ret
  resp
}

