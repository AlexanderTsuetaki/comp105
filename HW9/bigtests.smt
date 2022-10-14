; 3 Natural Tests

; Summary: checking that 0 + 22 doesn't equal 38

(check-print (((Natural fromSmall: 0) + (Natural fromSmall: 22)) =
                                              (Natural fromSmall: 38)) <False>)

; Summary: checking 39 * 23 = 897

(check-print ((Natural fromSmall: 39) * (Natural fromSmall: 23)) 897)

; Summary:

(check-print ((Natural fromSmall: 0) isZero) <True>)

; 3 LargeInt Tests

; Summary: Multiplying a positive and negative large integer returns negative
;          value

(check-print (((LargeInteger fromSmall: 29) * (LargeInteger fromSmall: -5))
                                                             isNegative) <True>)

; Summary: Testing sdiv: on LargeInteger

(check-print ((LargeInteger fromSmall: 51036) sdiv: 11) 4639)

; Summary: Testing if 0 is strictly positive

(check-print ((LargeInteger fromSmall: 0) isStrictlyPositive) <False>)

; 3 tests for mixed arithmetic

; Summary: adding two large smallintegers will cause overflow and properly
;          return a LargeInteger

(check-print ((205315100 + 2358359200) =
                                ((LargeInteger fromSmall: 2035315100) +
                                (LargeInteger fromSmall: 2358359200))) <True>)

; Summary: subtracting LargeInteger with SmallInteger without causing overflow

(check-print ((LargeInteger fromSmall: 0) - 31553) -31553)

; Summary: adding a LargeInteger with a SmallInteger

(check-print (38 + (LargeInteger fromSmall: 59)) 97)
