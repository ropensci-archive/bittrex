---
title: 'bittrex: An R client for the Bittrex Crypto-Currency Exchange'
tags:
  - finance
  - crypto-currency
  - open exchanges
authors:
 - name: Michael Kane
   orcid: 0000-0003-1899-6662
   affiliation: 1
affiliations:
 - name: Yale University
   index: 1
date: 2 June 2017
bibliography: paper.bib
---

# Summary

Package ```bittrex``` [@bittrex] is a RESTful R [@R] client for the
Bittrex crypto-currency exchange [@bittrex_site]. The package provides 
functions for all endpoints supported by the exchange including the
ability to retrieve price, volume, and order book information as well as
the ability to trade crypto-currencies.

Calls to the exchange are categorized as either public, which includes
requests for price, volume, and order book information, and private,
which includes all requests requiring an account including placing buy
or sell orders. Public calls can be used directly by installing the
package. Private calls require that you [create an
account](https://https://bittrex.com/account/Register) and create an API 
and secret key with appropriate permissions.

Private calls retrieve the API and secret key using the BITTREX\_API\_KEY
and BITTREX\_SECRET\_KEY environment variables. These may be set by the
user before opening the R session or, they can be set using the
'bittrex\_authenticate' function.

# References
  
