context("Bittrex Private Calls")

api_key = "bunk"
secret_key = "bunk"

test_that('The "buy" and "cancel" functions work.', {
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

test_that('The "sell" and "cancel" function work.', {
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

# Start here
test_that('The "getopenorders" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getopenorders()
  expect_true(resp$success)
  Sys.sleep(2)
})

test_that('The "getorder" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getorder("1234")
  expect_false(resp$success)
  expect_equal(resp$message, 'UUID_INVALID')
  Sys.sleep(2)
})

test_that('The "getorderhistory" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = getorderhistory()
  expect_true(resp$success)
  Sys.sleep(2)
})

test_that('The "mytrades" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = mytrades("usd-btc")
  expect_true(resp$success)
  Sys.sleep(2)
})

test_that('The "sellimit" function works.', {
  bittrex_authenticate(api_key, secret_key)
  resp = selllimit("usd-btc", 100, 1)
  expect_false(resp$success)
  expect_equal(resp$message, 'ACCESS_DENIED')
  Sys.sleep(2)
})

