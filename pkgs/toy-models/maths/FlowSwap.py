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

# let's define a handful of symbols

L_a, L_b, T, r, r_a, r_b, t_0, t = var("L_a", "L_b", "T", "r", "r_a", "r_b", "t_0", "t")
assume(t >= t_0)

################################################################################
# Reactive Constant Liquidity Product
################################################################################

def CLP(x, y, x_prime, y_prime):
    """Constant Liquidity Product Swap"""
    return x * y == x_prime * y_prime

def solve_clp_a4b():
    print("# Solve CLP equation for selling A for B instantly\n")
    v_a = var("a_Δ")
    v_b = var("b_Δ")
    clp = CLP(
        L_a,
        L_b,
        L_a + v_a,
        L_b + v_b
    )
    sols = solve(clp, v_b)
    assert(len(sols) == 1)
    print(sols[0])
    print("\n")

def solve_rclp_rtb_unidir():
    print("# Solve Reactive CLP rtb_unidir equation\n")
    f = function("f", nargs = 2)(r, t)
    clp = CLP(
        L_a,
        L_b,
        L_a + r * (t - t_0),
        L_b + f
    )

    sols = solve(clp, f)
    assert(len(sols) == 1)
    print(sols[0])
    print("\n")

def solve_rclp_rtb_bidir():
    print("# Solve Reactive CLP rtb_bidir equation\n")
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
    print("L_{flowswap_a} =", (1/q * cf_a).subs(sols[0]))
    print("L_{flowswap_b} =", (q * cf_b).subs(sols[0]))
    print("\n")

solve_clp_a4b()
solve_rclp_rtb_unidir()
solve_rclp_rtb_bidir()

################################################################################
# Reactive Stable Swap (WIP, Broken)
################################################################################

def STABLESWAP(x, y, x_prime, y_prime):
    """Generalized Stable Swap"""
    return x + y + (x * y) == x_prime + y_prime + (x_prime * y_prime)

def solve_rss_rtb_bidir():
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
    print("L_{aΔ} =", cf_a + (1/q * cf_a).subs(sols[0]))
    print("L_{bΔ} =", cf_b + (q * cf_b).subs(sols[0]))
    print("\n")

#solve_rss_rtb_bidir()
