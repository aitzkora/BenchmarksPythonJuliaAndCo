from math import exp
#pythran export g(float)
def g(x):
    if x<0.5:
        h=-exp(-x)
    else:
        h= exp(x)
    return h*x*x
