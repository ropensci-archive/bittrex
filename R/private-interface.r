#' @importFrom httr GET content add_headers
#' @importFrom openssl sha512
priv_req <- function(req) {
  str_time <- as.character(as.integer(Sys.time()))
  req <- paste0(req, "&nonce=", str_time)
  sig <- sha512(req, Sys.getenv("BITTREX_SECRET_KEY"))
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
bt_authenticate <- function(api_key, secret_key) {
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
#' @seealso \code{\link{bt_authenticate}} \code{\link{bt_sell}}
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
#'    \code{link{getorderhistory}} function.}
#' }
#' @examples
#' \dontrun{
#' # Note you must authenticate first. 
#' # Buy one Litecoins for 0.0001 Ethereum.
#' bt_buy("eth-ltc", 1, 0.001)
#' # $success
#' # [1] TRUE
#' # 
#' # $message
#' # [1] ""
#' # 
#' # $result
#' # $result$uuid
#' # [1] "2d6169e9-17fb-4f2a-8aff-37418b515624"
#' }
#' @export
bt_buy <- function(market, quantity, rate, type=c("limit", "market")) {
  req <- market_url
  if (type[1] == "market") {
    if (!missing(rate)) {
      warning("Rate parameter is ignored for market orders.")
    }
    req <- paste(req, 
      paste0("buymarket?apikey=", Sys.getenv("BITTREX_API_KEY"),
             "&market=", market, "&quantity=", quantity), sep="/")
  } else if (type[1] == "limit") {
    if (missing(rate)) {
      stop("Rate must be specified for limit orders.")
    }
    req <- paste(req, 
      paste0("buylimit?apikey=", Sys.getenv("BITTREX_API_KEY"),
             "&market=", market, "&quantity=", quantity,
             "&rate=", rate), sep="/")
  } else {
    stop("Unknown buy type")
  }
  priv_req(req)
}

#' @title Place a Sell Order
#' @description The \code{bt_sell} function places a sell order onto the 
#' bittrex crypto-currency exchange \url{https://bittrex.com}. This function
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
#' # Note you must authenticate first.
#' # Sell one tenth of one Ethereum coin for 1 Bitcoin. 
#' bt_sell("eth-btc", 0.1, 1)
#' # $success
#' # [1] TRUE
#' # 
#' # $message
#' # [1] ""
#' # 
#' # $result
#' # $result$uuid
#' # [1] "2d6l69e9-17fb-4f2a-8aff-37418b515624"
#' }
#' @export
bt_sell <- function(market, quantity, rate, type=c("limit", "market")) {
  req <- market_url
  if (type[1] == "market") {
    if (!missing(rate)) {
      warning("Rate parameter is ignored for market orders.")
    }
    req <- paste(req, 
      paste0("sellmarket?apikey=", Sys.getenv("BITTREX_API_KEY"),
             "&market=", market, "&quantity=", quantity), sep="/")
  } else if (type[1] == "limit") {
    if (missing(rate)) {
      stop("Rate must be specified for limit orders.")
    }
    req <- paste(req, 
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
#' # Note you must authenticate and define a uuid first.
#' bt_cancel(uuid) 
#' # $success
#' # [1] TRUE
#' # 
#' # $message
#' # [1] ""
#' # 
#' # $result
#' }
#' @export
bt_cancel <- function(uuid) {
  req <- paste(market_url, 
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
#' # Note you must authenticate first.
#' bt_getopenorders()
#' # $success
#' # [1] TRUE
#' # 
#' # $message
#' # [1] ""
#' # 
#' # $result
#' #   uuid                           order_uuid exchange order_type quantity
#' # 1   NA 2d6169e9-17fb-4f2a-8aff-37418b515624  ETH-LTC  LIMIT_BUY        1
#' #   quantity_remaining limit commission_paid price price_per_unit
#' # 1                  1 0.001               0     0             NA
#' #                   opened closed cancel_initiated immediate_or_cancel
#' # 1 2017-07-11T18:53:30.07     NA            FALSE               FALSE
#' #   is_conditional condition condition_target
#' # 1          FALSE      NONE               NA
#' }
#' @export
bt_getopenorders <- function(market) {
  if (missing(market)) {
    req <- paste(market_url, 
      paste0("getopenorders?apikey=", Sys.getenv("BITTREX_API_KEY")), sep="/")
  } else {
    req <- paste(market_url, 
      paste0("getopenorders?apikey=", Sys.getenv("BITTREX_API_KEY"),
             "&market=", market), sep="/")
  }
  ret <- priv_req(req)
  if ( (ret$success == TRUE) && (length(ret$result) > 0)) {
    ret$result <- result_to_df(ret$result)
  } else if (ret$success == TRUE) {
    ret$result <- data.frame(uuid=character(), order_uuid=character(), 
      exchange=character(), order_type=character(), quantity=numeric(),
      quantity_remaining=numeric(), limit=numeric(),
      commission_paid=numeric(),
      price=numeric(), price_per_unit=numeric(),
      opened=as.POSIXct(strptime(character(), "%Y-%m-%d %H:%M:%OS", tz="GMT")),
      closed=as.POSIXct(strptime(character(), "%Y-%m-%d %H:%M:%OS", tz="GMT")),
      cancel_initiated=logical(),
      immediate_or_cancel=logical(), is_conditional=logical(),
      condition=character(), condition_target=character())
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
#' # Note you must authenticate first.
#' balances <- bt_getbalances()$result
#' # $success
#' # [1] TRUE
#' # 
#' # $message
#' # [1] ""
#' # 
#' # $result
#' #   currency   balance available pending
#' # 1      BTC 0.0000000 0.0000000       0
#' # 2      ETH 0.2187638 0.2187638       0
#' # 3      LTC 0.0000000 0.0000000       0
#' #                               crypto_address
#' # 1         1Q6WissSMNF7NCNw3sDXQ2F7AbrSCYouj2
#' # 2 0x0ceac821a72037b07df691a53e201d797252b5a6
#' # 3         Li71CUBjxFH6PfEZn2phqfPhoasydfNfqF
#' }
#' @export
bt_getbalances <- function() {
  req <- paste(account_url,
    paste0("getbalances?apikey=", Sys.getenv("BITTREX_API_KEY")), sep="/")
  ret <- priv_req(req)
  if (ret$success) {
    if (length(ret$result) == 1) {
      ret$result <- as_data_frame(ret$result)
    } else if (length(ret$result) > 1) {
      ret$result <- result_to_df(ret$result)
    } else { # length(ret$result) == 0
      ret$result <- data.frame(currency=character(), balance=numeric(), 
        available=numeric(), pending=numeric(), crypt_address=character())
    }
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
#' Note you must authenticate first.
#' bt_getbalance("btc")$result
#' #   currency balance available pending                     crypto_address
#' # 1      BTC       0         0       0 1Q6WissSMNF7NCNw3sDXQ2F7AbrSCYouj2

#' }
#' @export
bt_getbalance <- function(currency) {
  req <- paste(account_url,
    paste0("getbalance?apikey=", Sys.getenv("BITTREX_API_KEY"), 
      "&currency=", currency), sep="/")
  resp <- priv_req(req)
  if (resp$success && is.null(resp$result$Balance)) {
    for (i in seq_along(resp$result)) {
      if (is.null(resp$result[[i]])) resp$result[[i]] <- 0
      resp$result$CryptoAddress <- NA
    }
  }
  if (resp$success) {
    resp$result <- as_data_frame(resp$result)
  }
  resp
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
#' # Note you must authenticate first.
#' bt_getdepositaddress("btc")
#' # $success
#' # [1] TRUE
#' # 
#' # $message
#' # [1] ""
#' # 
#' # $result
#' #   currency                            address
#' # 1      BTC 1Q6WissSMNF7NCNw3sDXQ2F7AbrSCYouj2
#' }
#' @export
bt_getdepositaddress <- function(currency) {
  req <- paste(account_url,
    paste0("getdepositaddress?apikey=", Sys.getenv("BITTREX_API_KEY"),
      "&currency=", currency), sep="/")
  ret <- priv_req(req)

  # If the enpoint is queried and an address does not exist, one is generated.
  if (ret$message == "ADDRESS_GENERATING") 
    ret <- bt_getdepositaddress(currency)
  if (ret$success) {
    ret$result <- as_data_frame(ret$result)    
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
#' # Note you must authenticate first.
#' # Send the author your bitcoins.
#' bt_widthdraw("btc", 10, "1Q6WissSMNF7NCNw3sDXQ2F7AbrSCYouj2")
#' }
#' @export
bt_withdraw <- function(currency, quantity, address, paymentid) {
  req <- paste(account_url, 
    paste0("withdraw?apikey=", Sys.getenv("BITTREX_API_KEY"),
      "&currency=", currency, "&quantity=", quantity, "&address=", address),
      sep="/")
  if (!missing(paymentid)) {
    req <- paste0(req, "&paymentid=", paymentid)
  }
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
#' # Note you must authenticate and define a uuid first.
#' bt_getorder(uuid)
#' # $success
#' # [1] TRUE
#' # 
#' # $message
#' # [1] ""
#' # 
#' # $result
#' #   account_id                           order_uuid exchange      type quantity
#' # 1         NA 63181c27-dd14-476e-960c-1bd8366bb312  ETH-LTC LIMIT_BUY        1
#' #   quantity_remaining limit reserved reserve_remaining commission_reserved
#' # 1                  1 0.001    0.001             0.001             2.5e-06
#' #   commission_reserve_remaining commission_paid price price_per_unit
#' # 1                      2.5e-06               0     0             NA
#' #                    opened closed is_open                             sentinel
#' # 1 2017-07-11T17:05:55.583     NA    TRUE 78b210fa-a156-4f4d-8043-51163740056f
#' #   cancel_initiated immediate_or_cancel is_conditional condition
#' # 1            FALSE               FALSE          FALSE      NONE
#' #   condition_target
#' # 1               NA
#' }
#' @export
bt_getorder <- function(uuid) {
  req <- paste(account_url, paste0("getorder?apikey=", 
    Sys.getenv("BITTREX_API_KEY"), "&uuid=", uuid), sep="/")
  resp <- priv_req(req)
  ret <- data.frame(account_id=character(), order_uuid=character(), 
    exchange=character(), type=character(),
    quantity=numeric(), quantity_remaining=numeric(), limit=numeric(), 
    reserved=numeric(), reserve_remaining=numeric(), 
    commission_reserved=numeric(), 
    commission_reserve_remaining=numeric(), commission_paid=numeric(), 
    price=numeric(), price_per_unit=numeric(), 
    opened=as.POSIXct(strptime(character(), "%Y-%m-%d %H:%M:%OS", tz="GMT")), 
    closed=as.POSIXct(strptime(character(), "%Y-%m-%d %H:%M:%OS", tz="GMT")),
    is_open=logical(), sentinel=character(), cancel_initiated=logical(), 
    immediate_or_cancel=logical(), is_conditional=logical(),
    condition=character(), condition_target=character())
  if (length(resp$result) > 0) {
    for(i in seq_along(resp$result)) {
      if (is.null(resp$result[[i]])) {
        resp$result[[i]] <- NA
      }
    }
    ret <- data.frame(resp$result)
    names(ret) <- camel_to_snake(names(ret))
  }
  resp$result <- ret
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
#' # $success
#' # [1] TRUE
#' # 
#' # $message
#' # [1] ""
#' # 
#' # $result
#' #                             order_uuid exchange time_stamp order_type   limit
#' # 1 c04bc07b-e6a9-4f47-a2c8-f9eb3c9a7fa1  BTC-ETH       <NA> LIMIT_SELL 0.11771
#' # 2 319fcc92-0b0d-43f1-a538-18f790d85ffa  BTC-ETH       <NA> LIMIT_SELL 0.14170
#' # 3 cd052594-d655-4e79-bec6-383ba7be8302  BTC-ETH       <NA> LIMIT_SELL 0.14020
#' #   quantity quantity_remaining commission      price price_per_unit
#' # 1    0.400                  0 0.00011772 0.04708801      0.1177200
#' # 2    0.100                  0 0.00003542 0.01417000      0.1417000
#' # 3    0.175                  0 0.00006133 0.02453499      0.1401999
#' #   is_conditional condition condition_target immediate_or_cancel
#' # 1          FALSE      NONE               NA               FALSE
#' # 2          FALSE      NONE               NA               FALSE
#' # 3          FALSE      NONE               NA               FALSE
#' #                    closed
#' # 1 2017-06-22T20:06:23.973
#' # 2   2017-06-13T15:33:20.4
#' # 3 2017-06-13T14:59:13.923
#' }
#' @export
bt_getorderhistory <- function(market) {
  req <- paste(account_url, paste0("getorderhistory?apikey=",
    Sys.getenv("BITTREX_API_KEY")), sep="/")
  if (!missing(market)) {
    req <- paste0(req, "&market=", market)
  }
  resp <- priv_req(req)
  ret <- data.frame(order_uuid=character(), exchange=character(),
    time_stamp=as.POSIXct(strptime(character(),"%Y-%m-%d %H:%M:%OS", tz="GMT")),
    order_type=character(), limit=numeric(), quantity=numeric(), 
    quantity_remaining=numeric(), commission=numeric(), price=numeric(),
    price_per_unit=numeric(), is_conditional=logical(),
    condition=character(), condition_target=character(),
    immediate_or_cancel=logical(),
    closed=as.POSIXct(strptime(character(), "%Y-%m-%d %H:%M:%OS", tz="GMT")))
  if (length(resp$result) > 0) {
    for(i in seq_along(resp$result)) {
      for (j in seq_along(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) {
          resp$result[[i]][[j]] <- NA
        }
      }
    }
    ret <- Reduce(rbind, Map(as_data_frame, resp$result))
    names(ret) <- camel_to_snake(names(ret))
    ret$time_stamp <- as.POSIXct(
      strptime(ret$time_stamp, "%Y-%m-%d %H:%M:%OS", tz="GMT"))
  }
  resp$result <- ret
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
#' # Note you must authenticate first.
#' bt_getwithdrawalhistory()
#' # $success
#' # [1] TRUE
#' # 
#' # $message
#' # [1] ""
#' # 
#' # $result
#' #                           payment_uuid currency     amount
#' # 1 ba0c85de-1fd8-423e-939d-e34d2aad34fd      BTC 0.04597029
#' # 2 f10c3536-fcf2-48eb-9ce4-253271d2c8e8      BTC 0.01313458
#' # 3 5dae07ad-7a8f-40a0-ac6e-225a3d0d6d8a      BTC 0.02347405
#' #                              address              opened authorized
#' # 1 1C31WQL12CDZqnidra9tMfs4DLkebAuNgc 2017-06-22 20:08:26       TRUE
#' # 2 1C31WQL12CDZqnidra9tMfs4DLkebAuNgc 2017-06-13 15:34:47       TRUE
#' # 3 1C31WQL12CDZqnidra9tMfs4DLkebAuNgc 2017-06-13 15:03:23       TRUE
#' #   pending_payment tx_cost
#' # 1           FALSE   0.001
#' # 2           FALSE   0.001
#' # 3           FALSE   0.001
#' #                                                              tx_id canceled
#' # 1 e628848ed92be4baee877f97e3a48b22f5ee2f7ca35c2908282b8c9ee2f4b94a    FALSE
#' # 2 fbafe847d02761d089b19a2cafecff561030219ded1eb03cc796c8c2eac0dd5c    FALSE
#' # 3 c981f7dc569188db16753cff4ab24aef148039964b68428603e2bfd18c754df4    FALSE
#' #   invalid_address
#' # 1           FALSE
#' # 2           FALSE
#' # 3           FALSE
#' }
#' @export
bt_getwithdrawalhistory <- function(currency) {
  req <- paste(account_url, paste0("getwithdrawalhistory?apikey=",
    Sys.getenv("BITTREX_API_KEY")), sep="/")
  if (!missing(currency)) {
    req <- paste0(req, "&currency=", currency)
  }
  resp <- priv_req(req)
  ret <- data.frame(payment_uuid=character(), currency=character(), 
    amount=numeric(), address=character(), opened=character(), 
    authorized=logical(), pending_payment=logical(), tx_cost=numeric(),
    tx_id=character(), canceled=logical(), invalid_address=logical())
  if (length(resp$result) > 0) {
    for(i in seq_along(resp$result)) {
      for (j in seq_along(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) {
          resp$result[[i]][[j]] <- NA
        }
      }
    }
    ret <- Reduce(rbind, Map(as_data_frame, resp$result))
    names(ret) <- camel_to_snake(names(ret))
    ret$opened <- strptime(ret$opened, "%Y-%m-%dT%H:%M:%OS", tz="GMT")
  }
  resp$result <- ret
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
#' # $success
#' # [1] TRUE
#' # 
#' # $message
#' # [1] ""
#' # 
#' # $result
#' #         id     amount currency confirmations        last_updated
#' # 1 20774372 0.39125728      ETH            49 2017-06-22 16:05:50
#' # 2 18255803 0.05936286      BTC             6 2017-05-19 16:28:36
#' #                                                                tx_id
#' # 1 0xbecc44384d8b94f1d03834ffb9324e97e4fa2a8161e17e61116aaabd5fb35050
#' # 2   7084cad99373475d8137547ce947b1472bfcb2d23b5160b05010f1f15e3c6287
#' #                               crypto_address
#' # 1 0x0ceac821a72037b07df691a53e201d797252b5a6
#' # 2         1Q6WissSMNF7NCNw3sDXQ2F7AbrSCYouj2
#' }
#' @export
bt_getdeposithistory <- function(currency) {
  req <- paste(account_url, paste0("getdeposithistory?apikey=",
    Sys.getenv("BITTREX_API_KEY")), sep="/")
  if (!missing(currency)) {
    req <- paste0(req, "&currency=", currency)
  }
  resp <- priv_req(req)
  ret <- data.frame(id=integer(), amount=numeric(), currency=character(),
    confirmation=integer(), 
    last_updated=as.POSIXct(
      strptime(character(), "%Y-%m-%d %H:%M:%OS", tz="GMT")),
    tx_id=character(), crypto_address=character())
  if (length(resp$result) > 0) {
    for(i in seq_along(resp$result)) {
      for (j in seq_along(resp$result[[i]])) {
        if (is.null(resp$result[[i]][[j]])) {
          resp$result[[i]][[j]] <- NA
        }
      }
    }
    ret <- Reduce(rbind, Map(as_data_frame, resp$result))
    names(ret) <- camel_to_snake(names(ret))
    ret$last_updated <- as.POSIXct(
      strptime(ret$last_updated, "%Y-%m-%dT%H:%M:%OS",tz="GMT"))
  }
  resp$result <- ret
  resp
}

