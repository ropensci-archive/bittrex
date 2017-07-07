#' @importFrom httr GET content add_headers
#' @importFrom openssl sha512
priv_req = function(req) {
  str_time = as.character(as.integer(Sys.time()))
  req = paste0(req, "&nonce=", str_time)
  sig = sha512(req, Sys.getenv("BITTREX_SECRET_KEY"))
  content(GET(req, add_headers(apisign=sig)), type="application/json")
}

#' @title Provide User Authentication Data
#' @description The \code{bt_authenicate} function sets the 
#' BITTREX_API_KEY and BITTREX_SECRET_KEY environment variables in your current
#' session to access your account information on the Bittrex crypto-currency
#' exchange (\url{https://bittrex.com}).
#' @param api_key the api key provided by the exchange
#' @param secret_key the secret key provided by the exchange
#' @export
bt_authenticate = function(api_key, secret_key) {
  Sys.setenv("BITTREX_API_KEY"=api_key)
  Sys.setenv("BITTREX_SECRET_KEY"=secret_key)
  invisible(TRUE)
}

#' @title Place a Buy Limit Order
#' @description The \code{bt_buy} function places a buy order onto the 
#' Bittrex crypto-currency exchange \url{https://bittrex.com}. This function
#' only works after you have set up authentication.
#'
#' NOTE: market orders are disabled as of July 7, 2017.
#' @seealso \code{link{bt_authenticate}} \code{\link{bt_sell}}
#' \code{\link{bt_getorder}} \code{\link{bt_getopenorders}} 
#' \code{\link{bt_getorderhistory}}
#' @references \url{https://bittrex.com/api/v1.1/market/buylimit}
#' @param market the market to place the buy limit order on.
#' @param quantity the quantity of the transaction currency to buy.
#' @param rate the price you are willing to pay per unit of the 
#' transaction currency.
#' @param type either "market" or "limit". Note that market orders are 
#' disabled as of July 7, 2017. (default is "limit")
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{a named list, with element "uuid" whose element is an 
#'    integer identifying the order. This value is used to query the status of
#'    of the order with either the \code{link{getorder}} or 
#'    \code{link{bt_getopenorders}} function. When the order is fulfilled it
#'    appears in the order history \code{data.frame} returned by the
#'    \code{link{getorderhistory}} function.
#'  }
#' }
#' @examples
#' \dontrun{
#' # Buy one litecoin for 1169 bitcoins. 
#' order = bt_buy("btc-ltc", 1, 0.0001)
#' }
#' @export
bt_buy = function(market, quantity, rate, type=c("limit", "market")) {
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
#' @description The \code{bt_sell} function places a buy order onto the 
#' C-Cex crypto-currency exchange \url{https://bittrex.com}. This function
#' only works if you have set up authentication. 
#' 
#' NOTE: market orders are disabled as of July 7, 2017.
#' @seealso \code{link{bt_authenticate}} \code{\link{bt_buy}}
#' \code{\link{bt_getopenorders}} \code{\link{bt_getorderhistory}}
#' @references \url{https://bittrex.com/api/v1.1/market/selllimit}
#' @param market the market to place the buy limit order on.
#' @param quantity the quantity of the reference currency to sell. 
#' @param rate the price you would like to get per unit of the 
#' transaction 
#' currency.
#' @param type either "market" or "limit". Note that market orders are 
#' disabled as of July 7, 2017. (default is limit)
#' @return A named list with the following elements:
#' \itemize{
#'  \item{success: }{a boolean indicating if the request successful?}
#'  \item{message: }{a string describing the error if the request was not 
#'                   successful, otherwise and empty string.}
#'  \item{result:  }{a named list, called "uuid" whose element is an integer
#'    identifying the order. This value is used to query the status of
#'    of the order with either the \code{link{bt_getorder}} or 
#'    \code{link{bt_getopenorders}} function. When the order is fulfilled it
#'    appears in the order history \code{data.frame} returned by the
#'    \code{link{bt_getorderhistory}} function.
#'  }
#' }
#' @examples
#' \dontrun{
#' # Sell one litecoin for 0.000001 bitcoins. 
#' order = bt_sell("btc-ltc", 1, 0.000001)
#' }
#' @export
bt_sell = function(market, quantity, rate, type=c("limit", "market")) {
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
#' @description The \code{bt_cancel} function cancels an open order on the
#' C-Cex crypto-currency exchange \url{https://bittrex.com}. This function
#' is called after providing information to authenticate your account and 
#' after an order is placed using either 
#' the \code{link{bt_buy}} or \code{link{bt_sell}} functions.
#' @seealso \code{\link{bt_authenticate}} \code{\link{bt_getopenorders}}
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
#' bt_cancel(uuid) 
#' }
#' @export
bt_cancel = function(uuid) {
  req = paste(market_url, 
    paste0("cancel?apikey=", Sys.getenv("BITTREX_API_KEY"),
           "&uuid=", uuid), sep="/")
  priv_req(req)
}

#' @title Order Data for all Open Orders
#' @description The \code{bt_getopenorders} function retrieves all open orders
#' on the Bittrex crypto-currency 
#' exchange \url{https://bittrex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{bt_authenticate}} 
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
#' bt_getopenorders()
#' }
#' @export
bt_getopenorders = function(market) {
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
#' @description The \code{bt_getbalances} function retrieves the account balance
#' for all currencies on the Bittrex crypto-currency 
#' exchange \url{https://bittrex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{bt_authenticate}}
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
#' balances = bt_getbalances()$result
#' }
#' @export
bt_getbalances = function() {
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
#' @description The \code{bt_getbalance} function retrieves the account balance
#' for a specified currency on the Bittrex crypto-currency 
#' exchange \url{https://bittrex.com}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{bt_authenticate}}
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
#' balances = bt_getbalance("btc")$result
#' }
#' @export
bt_getbalance = function(currency) {
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
#' @description The \code{bt_getdepositaddress} retrieves or creates the account 
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
#' bt_getdepositaddress("btc")
#' }
#' @export
bt_getdepositaddress = function(currency) {
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
#' @description The \code{bt_withdraw} function moves funds from a 
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
#' bt_widthdraw("btc", 10, "1Q6WissSMNF7NCNw3sDXQ2F7AbrSCYouj2")
#' }
#' @export
bt_withdraw = function(currency, quantity, address, paymentid) {
  req = paste(account_url, 
    paste0("withdraw?apikey=", Sys.getenv("BITTREX_API_KEY"),
      "&currency=", currency, "&quantity=", quantity, "&address=", address),
      sep="/")
  if (!missing(paymentid))
    req = paste0(req, "&paymentid=", paymentid)
  priv_req(req)
}

#' @title Order Data for a Specified Order
#' @description The \code{bt_getorder} function retrieves open order data 
#' on the Bittrex crypto-currency 
#' exchange \url{https://bittrex}. This function
#' can be used after you provide information for authentication.
#' @seealso \code{\link{bt_authenticate}} \code{\link{bt_getopenorders}}.
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
#' bt_getorder(uuid)
#' }
#' @export
bt_getorder = function(uuid) {
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
#' @description The \code{bt_getorderhistory} function retrieves order history
#' data on the Bittrex crypto-currency exchange \url{https://bittrex.com}. This 
#' function can be used after you provide authentication information.
#' @seealso \code{\link{bt_authenticate}}
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
#' bt_getorderhistory()
#' }
#' @export
bt_getorderhistory = function(market) {
  req = paste(account_url, paste0("getorderhistory?apikey=",
    Sys.getenv("BITTREX_API_KEY")), sep="/")
  if (!missing(market)) 
    req = paste0(req, "&market=", market)
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
#' @description The \code{bt_getwithdrawalhistory} function retrieves the
#' withdraw history for an account on the Bittrex crypto-currency exchange
#' \url{https://bittrex.com}. This function can be used after you 
#' provide authentication information.
#' @seealso \code{link{bt_authenticate}}
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
#' bt_getwithdrawalhistory()
#' }
#' @export
bt_getwithdrawalhistory = function(currency) {
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
#' @description The \code{bt_getdeposithistory} function retrieves the
#' deposit history for an account on the Bittrex crypto-currency exchange
#' \url{https://bittrex.com}. This function can be used after you 
#' provide authentication information.
#' @seealso \code{link{bt_authenticate}}
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
#' bt_getdeposithistory()
#' }
#' @export
bt_getdeposithistory = function(currency) {
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

