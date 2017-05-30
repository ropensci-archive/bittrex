context("Bittrex Private Calls")

api_key = "bunk"
secret_key = "bunk"

test_that('The "bittrex_authenticate" function works.', {
  expect_true(bittrex_authenticate(api_key, secret_key))
  expect_equal(Sys.getenv("BITTREX_API_KEY"), api_key)
  expect_equal(Sys.getenv("BITTREX_SECRET_KEY"), secret_key)
})

test_that('The "buy" and "cancel" functions works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = buy("btc-ltc", 100, 0.0001)
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success, FALSE)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    cancel(resp$result$uuid)
    expect_equal(resp$success, TRUE)
  }
  Sys.sleep(2)
})

test_that('The "cancel" function work.', {
  bittrex_authenticate(api_key, secret_key)
  resp = cancel("1234")
  expect_false(resp$success)
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_equal(resp$message, 'APIKEY_INVALID') 
  } else {
    expect_equal(resp$message, 'ACCESS_DENIED')
  }
  Sys.sleep(2)
})

test_that('The "getbalances" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getbalances()
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success, FALSE)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
    expect_is(resp$result, 'data.frame')
  }
  Sys.sleep(2)
})

test_that('The "getbalance" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getbalance("btc")
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success, FALSE)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
    expect_is(resp$result, 'data.frame')
  }
  Sys.sleep(2)
})

test_that('The "getdepositaddress" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getdepositaddress("btc")
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
    expect_is(resp$result, 'data.frame')
  }
  Sys.sleep(2)
})

test_that('The "getdeposithistory" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getdeposithistory("btc")
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
    expect_is(resp$result, 'data.frame')
  }
  Sys.sleep(2)
})

test_that('The "withdraw" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = withdraw("some-currency-that-does-not-exist", 1, 1)
  expect_false(resp$success)
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_equal(resp$message, 'INVALID_PERMISSION')
  }
  Sys.sleep(2)
})

test_that('The "getorder" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getorder("1234")
  expect_true(resp$success)
  Sys.sleep(2)
})

test_that('The "getorderhistory" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getorderhistory()
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
  }
  Sys.sleep(2)
})

test_that('The "getwithdrawalhistory" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getwithdrawalhistory()
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
  }
  Sys.sleep(2)
})

test_that('The "getwithdrawalhistory" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getwithdrawalhistory()
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
  }
  Sys.sleep(2)
})

test_that('The "getopenorders" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getopenorders()
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success, FALSE)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_true(resp$success)
  }
  Sys.sleep(2)
})

test_that('The "sell" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = sell("btc-ltc", 1, 1e6)
  expect_false(resp$success)
  if (api_key == "bunk" && secret_key == "bunk") {
    expect_false(resp$success, FALSE)
    expect_equal(resp$message, "APIKEY_INVALID")
  } else {
    expect_equal(resp$message, 'ACCESS_DENIED')
  }
  Sys.sleep(2)
})

