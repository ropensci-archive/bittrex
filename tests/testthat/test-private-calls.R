context("Bittrex Private Calls")

api_key <- "bunk"
secret_key <- "bunk"

test_that('The "bt_authenticate" function works.', {
  expect_true(bt_authenticate(api_key, secret_key))
  expect_equal(Sys.getenv("BITTREX_API_KEY"), api_key)
  expect_equal(Sys.getenv("BITTREX_SECRET_KEY"), secret_key)
})

test_that('The "bt_buy" and "bt_cancel" functions work.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_buy("btc-ltc", 100, 0.00001)
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success, FALSE)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    bt_cancel(resp$result$uuid)
    expect_equal(resp$success, TRUE)
  }
  Sys.sleep(2)
})

test_that('The "bt_buy" functions errors with invalid type.', {
  bt_authenticate(api_key, secret_key)
  expect_error(bt_buy("btc-ltc", 100, 0.0001, type="bunk"))
  Sys.sleep(2)
})

test_that('The "bt_cancel" function work.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_cancel("1234")
  expect_false(resp$success)
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_equal(resp$message, 'APIKEY_INVALID') 
  } else {
    expect_equal(resp$message, 'ACCESS_DENIED')
  }
  Sys.sleep(2)
})

test_that('The "bt_getbalances" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_getbalances()
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success, FALSE)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
    expect_is(resp$result, 'data.frame')
  }
  Sys.sleep(2)
})

test_that('The "bt_getbalance" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_getbalance("btc")
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success, FALSE)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
    expect_is(resp$result, 'data.frame')
  }
  Sys.sleep(2)
})

test_that('The "bt_getdepositaddress" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_getdepositaddress("btc")
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
    expect_is(resp$result, 'data.frame')
  }
  Sys.sleep(2)
})

test_that('The "bt_getdeposithistory" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_getdeposithistory("btc")
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
    expect_is(resp$result, 'data.frame')
  }
  Sys.sleep(2)
})

test_that('The "bt_withdraw" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_withdraw("some-currency-that-does-not-exist", 1, 1)
  expect_false(resp$success)
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_equal(resp$message, 'INVALID_PERMISSION')
  }
  Sys.sleep(2)
})

test_that('The "bt_getorder" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_getorder("1234")
  expect_false(resp$success)
  Sys.sleep(2)
})

test_that('The "getorderhistory" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_getorderhistory()
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
  }
  Sys.sleep(2)
})

test_that('The "bt_getwithdrawalhistory" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_getwithdrawalhistory()
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
  }
  Sys.sleep(2)
})

test_that('The "bt_getwithdrawalhistory" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_getwithdrawalhistory()
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
  }
  Sys.sleep(2)
})

test_that('The "bt_getopenorders" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_getopenorders()
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success, FALSE)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
  }
  Sys.sleep(2)
})

test_that('The "bt_getopenorders" function works for a market.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_getopenorders('btc-ltc')
  expect_false(resp$success)
  Sys.sleep(2)
})

test_that('The "bt_sell" function works.', {
  bt_authenticate(api_key, secret_key)
  resp <- bt_sell("btc-ltc", 1, 1e6)
  expect_false(resp$success)
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success, FALSE)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_equal(resp$message, 'ACCESS_DENIED')
  }
  Sys.sleep(2)
})

test_that('The "bt_sell" function error with invalid type.', {
  bt_authenticate(api_key, secret_key)
  expect_error(bt_sell("btc-ltc", 1, 1e6, type="bunk"))
  Sys.sleep(2)
})

