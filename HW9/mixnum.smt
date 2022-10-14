(use bignum.smt)

; goals: -make arithmetic work between small and large integers
;        -if smallintegers overflow, the system moves to largeintegers
;        -small integers support sdiv: and smod: like large integers

(SmallInteger addSelector:withMethod: 'asLargeInteger
    (compiled-method () (LargeInteger fromSmall: self))
)

(SmallInteger addSelector:withMethod: '+
    (compiled-method (aNumber) (aNumber addSmallIntegerTo: self))
)

(SmallInteger addSelector:withMethod: '-
    (compiled-method (aNumber) (aNumber subSmallIntegerTo: self))
)

(SmallInteger addSelector:withMethod: '*
    (compiled-method (aNumber) (aNumber multiplySmallIntegerTo: self))
)

(SmallInteger addSelector:withMethod: 'sdiv:
    (compiled-method (aNumber) (primitive div self aNumber ))
)

(SmallInteger addSelector:withMethod: 'smod:
    (compiled-method (aNumber) (primitive mod self aNumber ))
)

(SmallInteger addSelector:withMethod: 'addSmallIntegerTo:
    (compiled-method (anInteger) (
        (primitive addWithOverflow self anInteger
            {((self asLargeInteger) + anInteger)}) value)))

(SmallInteger addSelector:withMethod: 'multiplySmallIntegerTo:
    (compiled-method (anInteger) (
        (primitive mulWithOverflow self anInteger
            {((self asLargeInteger) * anInteger)}) value)))

(SmallInteger addSelector:withMethod: 'subSmallIntegerTo:
    (compiled-method (anInteger) (
        (primitive subWithOverflow self anInteger
            {((self asLargeInteger) - anInteger)}) value)))
