#!/usr/bin/env sage
# -*- coding: utf-8 -*-

"""
This module defines and solves the symbolic equations for the Flow Swap Reactive CFMM, including:

- defining CLP equation,
- defining STABLESWAP equation,
- solving rtb equation,
- solving instant_swap equation,
- ...
"""

from sage.all import var, assume, function, solve


def CLP(x, y, x_prime, y_prime):
    """Constant Liquidity Product Swap"""
    return x * y == x_prime * y_prime

def STABLESWAP(x, y, x_prime, y_prime):
    """Generalized Stable Swap"""
    return x + y + (x * y) == x_prime + y_prime + (x_prime * y_prime)

L_a, L_b, T, r, r_a, r_b, t_0, t = var("L_a", "L_b", "T", "r", "r_a", "r_b", "t_0", "t")
assume(t >= t_0)

def solve_rtb_unidir():
    print("# Solve rtb_unidir equation\n")
    f = function("f", nargs = 2)(r, t)
    clp = CLP(
        L_a,
        L_b,
        L_a + r * (t - t_0),
        L_b + f
    )

    solution1 = solve(clp, f)
    assert(len(solution1) == 1)
    print(solution1[0])
    print("\n")

def solve_rtb_bidir():
    print("# Solve rtb_bidir equation\n")
    cf_a = r_a * (t - t_0)
    cf_b = r_b * (t - t_0)

    q = var("q")
    clp = CLP(
        L_a,
        L_b,
        L_a + cf_a + q * cf_b,
        L_b + cf_b + 1/q * cf_a
    )
    sols = solve(clp, q)
    print("L_aΔ =", cf_a + (q * cf_b).subs(sols[0]))
    print("L_bΔ =", cf_b + (1/q * cf_a).subs(sols[0]))
    print("\n")

def solve_ss_rtb_bidir():
    print("# Solve solve_ss_rtb_bidir equation\n")
    cf_a = r_a * (t - t_0)
    cf_b = r_b * (t - t_0)

    q = var("q")
    amm = STABLESWAP(
        L_a,
        L_b,
        L_a + cf_a + q * cf_b,
        L_b + cf_b + 1/q * cf_a
    )
    sols = solve(amm, q)
    print("L_aΔ =", cf_a + (1/q * cf_a).subs(sols[0]))
    print("L_bΔ =", cf_b + (q * cf_b).subs(sols[0]))
    print("\n")

solve_rtb_unidir()
solve_rtb_bidir()
solve_ss_rtb_bidir()
