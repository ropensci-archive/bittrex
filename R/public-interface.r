#' @title Available Markets and Other Meta Data
#' @description The \code{bt_getmarkets} function returns all of the available
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
#' markets = bt_getmarkets()$result
#' head(markets)
#' }
#' @importFrom httr GET content
#' @export
bt_getmarkets = function() {
  resp = content(GET(paste(public_url, "getmarkets", sep="/")),
    type="application/json")
  if (resp$success) resp$result = result_to_df(resp$result)
  resp
}

#' @title Retrieve all Available Currencies on the Exchange
#' @description The \code{bt_getcurrencies} function returns the available
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
#' currencies = bt_getcurrencies()$result
#' head(markets)
#' }
#' @importFrom httr GET content
#' @export
bt_getcurrencies = function() {
  resp = content(GET(paste(public_url, "getcurrencies", sep="/")),
    type="application/json")
  if (resp$success) resp$result = result_to_df(resp$result)
  resp
}

#' Get the Ticker Values for a Market
#' @description The \code{bt_getticker} function returns the bid, ask, and last
#' transaction price for a specified market on the Bittrex crypto-currency 
#' exchange (\url{https://bittrex.com}). The complete list of 
#' markets is available via the \code{\link{bt_getmarkets}} function.
#' @seealso \code{\link{bt_getmarkets}}
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
bt_getticker = function(market) {
  resp = content(GET(
    paste(public_url, paste0("getticker?market=", market), sep="/")),
    type="application/json")
  if (resp$success) resp$result = as_data_frame(resp$result)
  resp
}

#' @title Summary of All Active Markets
#' @description the \code{bt_getmarketsummaries} retrieves a summary of all
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
#' ms = bt_getmarketsummaries()$result
#' head(ms)
#' }
#' @importFrom httr GET content
#' @export
bt_getmarketsummaries = function() {
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
#' @description The \code{bt_api_check} function checks to see
#' if you can sucessfully connect to the 
#' Bittrex crypto-currency exchange 
#' \url{https://bittrex.com}. 
#' @param warn if the request is not successful, should a warning be provided
#' with the status code.
#' @return A named logical indicating if you can connect to the exchange
#' through the public interface.
#' @examples
#' \dontrun{
#' bt_api_check()
#' }
#' @importFrom httr GET status_code
#' @export
bt_api_check = function(warn=TRUE) {
  resp = GET(paste(public_url, "getmarketsummaries", sep="/"))
  if (status_code(resp) != 200) {
    if (warn) warning(paste("Status code:", status_code(resp)))
  } else {
    TRUE
  }
}

#' @title Summary of a Markets
#' @description the \code{bt_getmarketsummary} retrieves a summary for a specified
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
#' ms = bt_getmarketsummary("btc-eth")$result
#' head(ms)
#' }
#' @importFrom httr GET content
#' @export
bt_getmarketsummary = function(market) {
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
#' @description The \code{bt_getorderbook} function returns the order book 
#' for a specified market on the Bittrex crypto-currency 
#' exchange (\url{https://bittrex.com}).
#' @references \url{https://bittrex.com/api/v1.1/public/getorderbook?market=BTC-LTC&type=both&depth=50}
#' @param market the market from which the order book will be retrieved.
#' @param type type of orders to retrieve (default is "both")
#' @param depth how deep should the returned order book be (default and 
#' maximum are 50). This is the size and price of bids whose price is 
#' lower than the highest bid and higher than the lowest ask.
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
#' ob = bt_getorderbook("btc-eth")$result
#' head(ob$buy)
#' head(ob$sell)
#' }
#' @importFrom httr GET content
#' @export
bt_getorderbook = function(market, type=c("both", "buy", "sell"), depth=50) {
  resp = content(GET(paste(public_url, 
    paste0("getorderbook?market=", market, "&type=", type[1], "&depth=", depth),    sep="/")), type="application/json")
  if (resp$success) {
    if (any(c("both", "buy") %in% names(resp$result))) {
      buy = Reduce(rbind, Map(as_data_frame, resp$result$buy))
      names(buy) = tolower(names(buy))
      buy$type = "BUY"
    }
    if (any(c("both", "sell") %in% names(resp$result))) {
      sell= Reduce(rbind, Map(as_data_frame, resp$result$sell))
      names(sell) = tolower(names(sell))
      sell$type = "SELL"
    }
    if (type[1] == "both")
      resp$result = rbind(sell, buy)
    else if (type[1] == "buy")
      resp$result = buy
    else # type[1] == "sell"
      resp$result = sell
  }
  resp
}

#' @title Recent History for a Market
#' @description the \code{bt_getmarkethistory} function retrieves recent trade
#' information for a specified market on the Bittrex crypto-currency exchange 
#' (\url{https://bittrex.com}).
#' @references \url{https://bittrex.com/api/v1.1/public/getmarkethistory?market=BTC-DOGE}
#' @param market the market from which history data will be retrieved.
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
#' mh = bt_getmarkethistory("btc-eth")$result
#' head(mh)
#' }
#' @importFrom httr GET content
#' @export
bt_getmarkethistory = function(market) {
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
