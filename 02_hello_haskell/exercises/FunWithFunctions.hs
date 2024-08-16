module FunWithFunctions where

z = 7

x = y ^ 2

waxOn = x * 5

y = z + 8

waxOnF = x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

triple _x = _x * 3

waxOff _x = triple _x
