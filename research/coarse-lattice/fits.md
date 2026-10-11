## Exponents alpha in q ~ n^alpha, least squares on n >= 8 (local slope of the last doubling in brackets)


### inputs max, plain

| adder | clocks | rewrites | peak sites | peak strands | agents at load | ideal depth | ideal work |
|---|---|---|---|---|---|---|---|
| ripple carry, tree | 0.89 (0.67, n<=32) | 1.05 (1.03, n<=32) | 0.63 (0.29, n<=32) | 0.66 (0.42, n<=32) | 0.11 (0.15, n<=32) | 0.39 (0.41, n<=64) | 1.04 (1.02, n<=64) |
| ripple carry, list | 1.06 (1.20, n<=64) | 0.97 (0.98, n<=64) | 0.55 (1.13, n<=64) | 0.63 (1.17, n<=64) | 0.16 (0.25, n<=64) | 0.96 (0.98, n<=64) | 0.98 (0.99, n<=64) |
| carry select, tree | 1.31 (1.31, n<=16) | 1.07 (1.07, n<=16) | 0.82 (0.82, n<=16) | 0.77 (0.77, n<=16) | 0.06 (0.06, n<=16) | 0.27 (0.22, n<=64) | 1.04 (1.02, n<=64) |
| carry select, list | 1.03 (1.09, n<=64) | 0.98 (0.99, n<=64) | 0.33 (0.18, n<=64) | 0.38 (0.29, n<=64) | 0.13 (0.21, n<=64) | 0.95 (0.97, n<=64) | 0.98 (0.99, n<=64) |
| carry lookahead, tree | 0.84 (0.95, n<=32) | 1.05 (1.04, n<=32) | 0.81 (0.81, n<=32) | 0.73 (0.69, n<=32) | 0.09 (0.11, n<=32) | 0.29 (0.24, n<=64) | 1.04 (1.02, n<=64) |
| carry lookahead, list | 1.07 (1.16, n<=64) | 0.96 (0.99, n<=64) | 0.49 (1.51, n<=64) | 0.53 (1.42, n<=64) | 0.12 (0.20, n<=64) | 0.95 (0.98, n<=64) | 0.98 (0.99, n<=64) |
| relay, tree | 0.53 (0.69, n<=64) | 1.02 (1.00, n<=64) | 0.69 (0.54, n<=64) | 0.75 (0.64, n<=64) | 0.28 (0.43, n<=64) | 0.35 (0.36, n<=64) | 1.02 (1.01, n<=64) |
| relay, list | 0.95 (0.98, n<=64) | 0.99 (0.98, n<=64) | 0.31 (0.48, n<=64) | 0.27 (0.47, n<=64) | 0.28 (0.42, n<=64) | 0.95 (0.98, n<=64) | 0.97 (0.99, n<=64) |
| relay unrolled, tree | 0.69 (0.59, n<=64) | 1.02 (1.00, n<=64) | 1.07 (1.11, n<=64) | 1.10 (1.14, n<=64) | 1.01 (1.01, n<=64) | 0.39 (0.47, n<=64) | 1.02 (1.01, n<=64) |
| relay unrolled, list | 0.87 (0.91, n<=32) | 0.95 (0.95, n<=32) | 1.10 (1.21, n<=32) | 1.11 (1.20, n<=32) | 0.95 (0.97, n<=32) | 0.90 (0.95, n<=64) | 0.97 (0.99, n<=64) |

### inputs max, views=1

| adder | clocks | rewrites | peak sites | peak strands | agents at load | ideal depth | ideal work |
|---|---|---|---|---|---|---|---|
| ripple carry, tree | 0.73 (1.01, n<=64) | 1.03 (1.01, n<=64) | 0.69 (0.70, n<=64) | 0.66 (0.45, n<=64) | 0.28 (0.42, n<=64) | 0.27 (0.22, n<=64) | 1.04 (1.02, n<=64) |
| ripple carry, list | 0.98 (1.01, n<=64) | 0.98 (1.00, n<=64) | 0.19 (0.41, n<=64) | 0.11 (0.18, n<=64) | 0.28 (0.42, n<=64) | 0.96 (0.98, n<=64) | 0.98 (0.99, n<=64) |
| carry select, tree | 1.04 (0.04, n<=64) | 1.03 (1.03, n<=64) | 0.82 (0.42, n<=64) | 0.74 (0.62, n<=64) | 0.24 (0.37, n<=64) | 0.26 (0.22, n<=64) | 1.04 (1.02, n<=64) |
| carry select, list | 0.96 (0.98, n<=64) | 0.97 (0.99, n<=64) | 0.06 (0.20, n<=64) | 0.04 (0.16, n<=64) | 0.24 (0.36, n<=64) | 0.94 (0.97, n<=64) | 0.98 (0.99, n<=64) |
| carry lookahead, tree | 0.98 (1.16, n<=32) | 1.04 (1.01, n<=32) | 0.72 (0.57, n<=32) | 0.61 (0.41, n<=32) | 0.17 (0.21, n<=32) | 0.28 (0.24, n<=64) | 1.04 (1.02, n<=64) |
| carry lookahead, list | 0.96 (1.03, n<=64) | 0.98 (0.98, n<=64) | 0.11 (0.30, n<=64) | 0.05 (0.03, n<=64) | 0.22 (0.35, n<=64) | 0.94 (0.97, n<=64) | 0.98 (0.99, n<=64) |
| relay, tree | 0.54 (0.52, n<=64) | 1.01 (1.01, n<=64) | 0.63 (0.37, n<=64) | 0.72 (0.46, n<=64) | 0.44 (0.60, n<=64) | 0.43 (0.48, n<=64) | 1.02 (1.01, n<=64) |
| relay, list | 0.95 (0.97, n<=64) | 0.98 (0.97, n<=64) | 0.32 (0.54, n<=64) | 0.22 (0.48, n<=64) | 0.43 (0.60, n<=64) | 0.93 (0.97, n<=64) | 0.98 (0.99, n<=64) |
| relay unrolled, tree | 0.70 (0.60, n<=64) | 1.02 (1.01, n<=64) | 1.05 (1.06, n<=64) | 1.09 (1.08, n<=64) | 1.01 (1.01, n<=64) | 0.47 (0.57, n<=64) | 1.02 (1.01, n<=64) |
| relay unrolled, list | 0.89 (0.97, n<=32) | 0.97 (0.98, n<=32) | 1.06 (1.11, n<=32) | 1.08 (1.12, n<=32) | 0.95 (0.97, n<=32) | 0.88 (0.94, n<=64) | 0.97 (0.99, n<=64) |

### inputs mixed, plain

| adder | clocks | rewrites | peak sites | peak strands | agents at load | ideal depth | ideal work |
|---|---|---|---|---|---|---|---|
| ripple carry, tree | 0.75 (0.95, n<=64) | 1.03 (1.01, n<=64) | 0.82 (0.92, n<=64) | 0.77 (0.70, n<=64) | 0.16 (0.25, n<=64) | 0.27 (0.23, n<=64) | 1.04 (1.02, n<=64) |
| ripple carry, list | 1.05 (1.17, n<=64) | 0.97 (0.99, n<=64) | 0.46 (0.88, n<=64) | 0.53 (0.73, n<=64) | 0.16 (0.25, n<=64) | 0.96 (0.98, n<=64) | 0.98 (0.99, n<=64) |
| carry select, tree | 1.20 (1.28, n<=32) | 1.04 (1.01, n<=32) | 1.04 (0.92, n<=32) | 0.88 (0.75, n<=32) | 0.09 (0.12, n<=32) | 0.27 (0.23, n<=64) | 1.04 (1.02, n<=64) |
| carry select, list | 1.03 (1.15, n<=64) | 0.97 (0.99, n<=64) | 0.50 (1.18, n<=64) | 0.52 (1.18, n<=64) | 0.13 (0.20, n<=64) | 0.95 (0.97, n<=64) | 0.98 (0.99, n<=64) |
| carry lookahead, tree | 0.83 (0.76, n<=32) | 1.05 (1.02, n<=32) | 0.82 (0.66, n<=32) | 0.71 (0.51, n<=32) | 0.09 (0.11, n<=32) | 0.28 (0.23, n<=64) | 1.04 (1.02, n<=64) |
| carry lookahead, list | 1.01 (1.06, n<=64) | 0.96 (0.98, n<=64) | 0.20 (0.38, n<=64) | 0.23 (0.47, n<=64) | 0.12 (0.20, n<=64) | 0.95 (0.98, n<=64) | 0.98 (0.99, n<=64) |
| relay, tree | 0.50 (0.77, n<=64) | 1.02 (1.02, n<=64) | 0.59 (0.39, n<=64) | 0.69 (0.46, n<=64) | 0.28 (0.42, n<=64) | 0.29 (0.29, n<=64) | 1.04 (1.02, n<=64) |
| relay, list | 0.96 (0.90, n<=64) | 0.98 (1.01, n<=64) | 0.24 (0.30, n<=64) | 0.19 (0.31, n<=64) | 0.28 (0.42, n<=64) | 0.95 (0.98, n<=64) | 0.98 (0.99, n<=64) |
| relay unrolled, tree | 0.58 (0.30, n<=64) | 1.02 (1.04, n<=64) | 1.06 (1.13, n<=64) | 1.10 (1.18, n<=64) | 1.01 (1.01, n<=64) | 0.27 (0.32, n<=64) | 1.02 (1.01, n<=64) |
| relay unrolled, list | 0.88 (0.96, n<=32) | 0.96 (0.98, n<=32) | 1.09 (1.15, n<=32) | 1.09 (1.15, n<=32) | 0.95 (0.97, n<=32) | 0.90 (0.95, n<=64) | 0.97 (0.98, n<=64) |

### inputs mixed, views=1

| adder | clocks | rewrites | peak sites | peak strands | agents at load | ideal depth | ideal work |
|---|---|---|---|---|---|---|---|
| ripple carry, tree | 0.75 (0.90, n<=64) | 1.03 (1.00, n<=64) | 0.61 (0.64, n<=64) | 0.67 (0.78, n<=64) | 0.28 (0.42, n<=64) | 0.27 (0.22, n<=64) | 1.04 (1.02, n<=64) |
| ripple carry, list | 0.96 (1.01, n<=64) | 0.98 (0.98, n<=64) | 0.15 (0.36, n<=64) | 0.13 (0.26, n<=64) | 0.28 (0.41, n<=64) | 0.96 (0.98, n<=64) | 0.98 (0.99, n<=64) |
| carry select, tree | 0.77 (0.93, n<=64) | 1.04 (1.01, n<=64) | 0.70 (0.75, n<=64) | 0.64 (0.64, n<=64) | 0.24 (0.36, n<=64) | 0.27 (0.22, n<=64) | 1.04 (1.02, n<=64) |
| carry select, list | 0.96 (0.94, n<=64) | 0.98 (0.98, n<=64) | 0.05 (0.21, n<=64) | 0.02 (0.09, n<=64) | 0.24 (0.36, n<=64) | 0.94 (0.97, n<=64) | 0.98 (0.99, n<=64) |
| carry lookahead, tree | 0.98 (1.38, n<=64) | 1.03 (1.00, n<=64) | 0.75 (0.73, n<=64) | 0.70 (0.66, n<=64) | 0.23 (0.35, n<=64) | 0.28 (0.24, n<=64) | 1.04 (1.02, n<=64) |
| carry lookahead, list | 0.98 (1.00, n<=64) | 0.98 (1.00, n<=64) | 0.13 (0.25, n<=64) | 0.12 (0.22, n<=64) | 0.22 (0.34, n<=64) | 0.94 (0.97, n<=64) | 0.98 (0.99, n<=64) |
| relay, tree | 0.53 (0.75, n<=64) | 1.01 (0.99, n<=64) | 0.55 (0.28, n<=64) | 0.62 (0.28, n<=64) | 0.44 (0.60, n<=64) | 0.34 (0.36, n<=64) | 1.03 (1.02, n<=64) |
| relay, list | 0.95 (0.95, n<=64) | 0.99 (1.00, n<=64) | 0.33 (0.47, n<=64) | 0.25 (0.44, n<=64) | 0.43 (0.59, n<=64) | 0.93 (0.97, n<=64) | 0.98 (0.99, n<=64) |
| relay unrolled, tree | 0.60 (0.58, n<=64) | 1.03 (1.02, n<=64) | 1.05 (1.04, n<=64) | 1.08 (1.03, n<=64) | 1.01 (1.01, n<=64) | 0.34 (0.42, n<=64) | 1.01 (1.00, n<=64) |
| relay unrolled, list | 0.88 (0.90, n<=32) | 0.96 (0.97, n<=32) | 1.08 (1.15, n<=32) | 1.09 (1.16, n<=32) | 0.95 (0.97, n<=32) | 0.88 (0.95, n<=64) | 0.97 (0.98, n<=64) |

## Unary add a + 1: exponent beta in q ~ a^beta, least squares on a >= 15

| sets | clocks | rewrites | peak sites | peak strands | agents | ideal depth | ideal work |
|---|---|---|---|---|---|---|---|
| plain | 0.99 (a<=512) | 0.99 (a<=512) | 0.28 (a<=512) | 0.25 (a<=512) | 0.39 (a<=512) | 0.99 (a<=1024) | 0.99 (a<=1024) |
| views=1 | 0.97 (a<=256) | 0.98 (a<=256) | 0.30 (a<=256) | 0.26 (a<=256) | 0.45 (a<=256) | 0.99 (a<=1024) | 0.99 (a<=1024) |

## Ideal depth (rounds) by width, inputs max: the increase per doubling (constant means log n, doubling means linear)

| adder | n=1 | n=2 | n=4 | n=8 | n=16 | n=32 | n=64 |
|---|---|---|---|---|---|---|---|
| ripple carry, tree | 138 | 314 (+176) | 480 (+166) | 648 (+168) | 842 (+194) | 1092 (+250) | 1454 (+362) |
| ripple carry, list | 304 | 470 (+166) | 802 (+332) | 1466 (+664) | 2794 (+1328) | 5450 (+2656) | 10762 (+5312) |
| carry select, tree | 143 | 317 (+174) | 483 (+166) | 645 (+162) | 807 (+162) | 969 (+162) | 1131 (+162) |
| carry select, list | 316 | 468 (+152) | 766 (+298) | 1362 (+596) | 2554 (+1192) | 4938 (+2384) | 9706 (+4768) |
| carry lookahead, tree | 140 | 298 (+158) | 458 (+160) | 625 (+167) | 797 (+172) | 970 (+173) | 1143 (+173) |
| carry lookahead, list | 297 | 450 (+153) | 748 (+298) | 1343 (+595) | 2535 (+1192) | 4919 (+2384) | 9687 (+4768) |
| relay, tree | 74 | 135 (+61) | 195 (+60) | 258 (+63) | 327 (+69) | 414 (+87) | 533 (+119) |
| relay, list | 131 | 194 (+63) | 320 (+126) | 572 (+252) | 1076 (+504) | 2084 (+1008) | 4100 (+2016) |
| relay unrolled, tree | 43 | 86 (+43) | 104 (+18) | 127 (+23) | 158 (+31) | 205 (+47) | 284 (+79) |
| relay unrolled, list | 80 | 104 (+24) | 150 (+46) | 246 (+96) | 438 (+192) | 822 (+384) | 1590 (+768) |

## Ideal depth = A log2(n) + B n + C, least squares on n >= 2: A rounds per doubling (tree level), B rounds per bit (carry path)

| adder | inputs | ideal depth A, B | ideal depth views A, B |
|---|---|---|---|
| ripple carry, tree | max | 144, 6.7 | 82, -0.0 |
| ripple carry, tree | mixed | 167, -0.0 | 82, -0.0 |
| ripple carry, list | max | -0, 166.0 | -0, 82.0 |
| ripple carry, list | mixed | -0, 166.0 | -0, 82.0 |
| carry select, tree | max | 163, -0.1 | 77, -0.0 |
| carry select, tree | mixed | 163, -0.0 | 79, -0.1 |
| carry select, list | max | -0, 149.0 | -0, 72.0 |
| carry select, list | mixed | 1, 148.9 | 0, 72.0 |
| carry lookahead, tree | max | 165, 0.4 | 82, 0.2 |
| carry lookahead, tree | mixed | 169, -0.1 | 85, 0.1 |
| carry lookahead, list | max | -0, 149.0 | -0, 72.0 |
| carry lookahead, list | mixed | 5, 147.9 | 0, 72.0 |
| relay, tree | max | 55, 2.0 | 25, 2.0 |
| relay, tree | mixed | 59, 0.8 | 26, 0.9 |
| relay, list | max | -0, 63.0 | 0, 27.0 |
| relay, list | mixed | -0, 63.0 | 0, 27.0 |
| relay unrolled, tree | max | 15, 2.0 | 10, 2.0 |
| relay unrolled, tree | mixed | 23, 0.6 | 11, 0.9 |
| relay unrolled, list | max | -1, 24.0 | -0, 14.0 |
| relay unrolled, list | mixed | 0, 24.0 | 0, 14.0 |

## Lattice clocks per idealised round (clocks / ideal depth), inputs max: the travel each round costs

| adder | n=1 | n=2 | n=4 | n=8 | n=16 | n=32 | n=64 | slope n>=8 |
|---|---|---|---|---|---|---|---|---|
| ripple carry, tree | 29 | 32 | 36 | 42 | 70 | 86 | - | 0.52 |
| ripple carry, list | 33 | 32 | 31 | 32 | 34 | 35 | 41 | 0.10 |
| carry select, tree | 29 | 34 | 43 | 53 | 105 | - | - | 0.99 |
| carry select, list | 37 | 37 | 37 | 38 | 40 | 42 | 46 | 0.08 |
| carry lookahead, tree | 34 | 37 | 43 | 56 | 72 | 115 | - | 0.52 |
| carry lookahead, list | 36 | 36 | 37 | 37 | 40 | 42 | 48 | 0.12 |
| relay, tree | 28 | 32 | 37 | 41 | 44 | 48 | 61 | 0.18 |
| relay, list | 32 | 33 | 33 | 34 | 33 | 34 | 34 | 0.01 |
| relay unrolled, tree | 22 | 23 | 38 | 49 | 61 | 83 | 90 | 0.31 |
| relay unrolled, list | 24 | 26 | 31 | 31 | 31 | 31 | - | -0.00 |
| unary add (a) | a=1: 27, a=3: 25, a=7: 25, a=15: 24, a=31: 24, a=63: 24, a=127: 24, a=255: 25, a=511: 25 |

## Crossover against unary add (inputs max, N = 2^n - 1 = a): the N where the binary adder gets cheaper

| adder | clocks | rewrites | peak sites | ideal depth | ideal work |
|---|---|---|---|---|---|
| ripple carry, tree | <= 1 | <= 1 | > range | <= 1 | <= 1 |
| ripple carry, list | ~3.5 | ~12.4 | > range | ~1.6 | ~11.5 |
| carry select, tree | <= 1 | <= 1 | > range | <= 1 | <= 1 |
| carry select, list | ~4.4 | ~15.8 | > range | ~1.7 | ~16.0 |
| carry lookahead, tree | <= 1 | <= 1 | > range | <= 1 | <= 1 |
| carry lookahead, list | ~4.0 | ~15.1 | > range | ~1.4 | ~15.4 |
| relay, tree | <= 1 | <= 1 | <= 1 | <= 1 | <= 1 |
| relay, list | <= 1 | ~1.2 | <= 1 | <= 1 | ~1.2 |
| relay unrolled, tree | <= 1 | <= 1 | <= 1 | <= 1 | <= 1 |
| relay unrolled, list | <= 1 | <= 1 | <= 1 | <= 1 | <= 1 |

## Movement agents (views=1) against plain: ratio of means, inputs max

| adder | n | clocks | rewrites | peak sites | peak strands | agents |
|---|---|---|---|---|---|---|
| unary add, unary | 1 | 0.52 | 0.48 | 0.45 | 0.46 | 0.48 |
| unary add, unary | 2 | 0.58 | 0.50 | 0.51 | 0.50 | 0.49 |
| unary add, unary | 3 | 0.56 | 0.48 | 0.52 | 0.53 | 0.49 |
| unary add, unary | 4 | 0.57 | 0.48 | 0.47 | 0.46 | 0.49 |
| unary add, unary | 7 | 0.55 | 0.48 | 0.48 | 0.51 | 0.50 |
| unary add, unary | 8 | 0.57 | 0.48 | 0.54 | 0.54 | 0.51 |
| unary add, unary | 15 | 0.57 | 0.48 | 0.53 | 0.52 | 0.53 |
| unary add, unary | 16 | 0.57 | 0.48 | 0.57 | 0.57 | 0.53 |
| unary add, unary | 31 | 0.56 | 0.48 | 0.47 | 0.48 | 0.57 |
| unary add, unary | 32 | 0.57 | 0.48 | 0.58 | 0.57 | 0.57 |
| unary add, unary | 63 | 0.56 | 0.48 | 0.58 | 0.57 | 0.63 |
| unary add, unary | 64 | 0.57 | 0.48 | 0.56 | 0.55 | 0.64 |
| unary add, unary | 128 | 0.56 | 0.49 | 0.60 | 0.58 | 0.72 |
| unary add, unary | 256 | 0.56 | 0.48 | 0.76 | 0.71 | 0.81 |
| carry lookahead, list | 1 | 0.57 | 0.48 | 0.39 | 0.42 | 0.47 |
| carry lookahead, list | 2 | 0.57 | 0.51 | 0.39 | 0.41 | 0.47 |
| carry lookahead, list | 4 | 0.53 | 0.51 | 0.46 | 0.47 | 0.48 |
| carry lookahead, list | 8 | 0.56 | 0.51 | 0.44 | 0.46 | 0.49 |
| carry lookahead, list | 16 | 0.52 | 0.51 | 0.42 | 0.45 | 0.51 |
| carry lookahead, list | 32 | 0.50 | 0.52 | 0.42 | 0.41 | 0.55 |
| carry lookahead, list | 64 | 0.45 | 0.52 | 0.18 | 0.16 | 0.61 |
| carry lookahead, tree | 1 | 0.59 | 0.50 | 0.46 | 0.46 | 0.47 |
| carry lookahead, tree | 2 | 0.57 | 0.49 | 0.45 | 0.49 | 0.47 |
| carry lookahead, tree | 4 | 0.60 | 0.51 | 0.61 | 0.60 | 0.48 |
| carry lookahead, tree | 8 | 0.53 | 0.51 | 0.67 | 0.67 | 0.49 |
| carry lookahead, tree | 16 | 0.56 | 0.52 | 0.70 | 0.69 | 0.51 |
| carry lookahead, tree | 32 | 0.65 | 0.51 | 0.59 | 0.57 | 0.55 |
| relay, list | 1 | 0.50 | 0.48 | 0.57 | 0.61 | 0.50 |
| relay, list | 2 | 0.54 | 0.50 | 0.52 | 0.53 | 0.51 |
| relay, list | 4 | 0.55 | 0.56 | 0.65 | 0.70 | 0.52 |
| relay, list | 8 | 0.55 | 0.55 | 0.60 | 0.68 | 0.55 |
| relay, list | 16 | 0.57 | 0.53 | 0.66 | 0.69 | 0.60 |
| relay, list | 32 | 0.56 | 0.54 | 0.60 | 0.62 | 0.67 |
| relay, list | 64 | 0.55 | 0.53 | 0.63 | 0.63 | 0.75 |
| relay, tree | 1 | 0.37 | 0.28 | 0.50 | 0.48 | 0.50 |
| relay, tree | 2 | 0.51 | 0.48 | 0.56 | 0.58 | 0.50 |
| relay, tree | 4 | 0.55 | 0.51 | 0.56 | 0.58 | 0.52 |
| relay, tree | 8 | 0.55 | 0.53 | 0.67 | 0.64 | 0.55 |
| relay, tree | 16 | 0.63 | 0.53 | 0.64 | 0.62 | 0.59 |
| relay, tree | 32 | 0.63 | 0.52 | 0.65 | 0.66 | 0.66 |
| relay, tree | 64 | 0.56 | 0.53 | 0.58 | 0.58 | 0.75 |
| relay unrolled, list | 1 | 0.53 | 0.46 | 0.48 | 0.46 | 0.52 |
| relay unrolled, list | 2 | 0.56 | 0.49 | 0.53 | 0.49 | 0.52 |
| relay unrolled, list | 4 | 0.55 | 0.49 | 0.51 | 0.46 | 0.52 |
| relay unrolled, list | 8 | 0.60 | 0.52 | 0.51 | 0.48 | 0.52 |
| relay unrolled, list | 16 | 0.59 | 0.52 | 0.51 | 0.49 | 0.52 |
| relay unrolled, list | 32 | 0.61 | 0.53 | 0.47 | 0.46 | 0.52 |
| relay unrolled, list | 64 | - | - | - | - | - |
| relay unrolled, tree | 1 | 0.53 | 0.45 | 0.50 | 0.46 | 0.51 |
| relay unrolled, tree | 2 | 0.56 | 0.49 | 0.47 | 0.45 | 0.52 |
| relay unrolled, tree | 4 | 0.55 | 0.50 | 0.49 | 0.46 | 0.52 |
| relay unrolled, tree | 8 | 0.62 | 0.53 | 0.47 | 0.45 | 0.52 |
| relay unrolled, tree | 16 | 0.58 | 0.53 | 0.48 | 0.47 | 0.52 |
| relay unrolled, tree | 32 | 0.61 | 0.53 | 0.47 | 0.46 | 0.52 |
| relay unrolled, tree | 64 | 0.62 | 0.54 | 0.46 | 0.44 | 0.52 |
| ripple carry, list | 1 | 0.50 | 0.47 | 0.35 | 0.39 | 0.47 |
| ripple carry, list | 2 | 0.54 | 0.47 | 0.41 | 0.43 | 0.48 |
| ripple carry, list | 4 | 0.56 | 0.48 | 0.42 | 0.46 | 0.49 |
| ripple carry, list | 8 | 0.52 | 0.49 | 0.40 | 0.45 | 0.50 |
| ripple carry, list | 16 | 0.51 | 0.49 | 0.43 | 0.41 | 0.53 |
| ripple carry, list | 32 | 0.49 | 0.49 | 0.32 | 0.30 | 0.57 |
| ripple carry, list | 64 | 0.43 | 0.50 | 0.19 | 0.15 | 0.64 |
| ripple carry, tree | 1 | 0.47 | 0.46 | 0.41 | 0.41 | 0.47 |
| ripple carry, tree | 2 | 0.56 | 0.47 | 0.44 | 0.48 | 0.48 |
| ripple carry, tree | 4 | 0.52 | 0.49 | 0.65 | 0.59 | 0.48 |
| ripple carry, tree | 8 | 0.48 | 0.49 | 0.59 | 0.55 | 0.50 |
| ripple carry, tree | 16 | 0.31 | 0.49 | 0.48 | 0.49 | 0.52 |
| ripple carry, tree | 32 | 0.32 | 0.49 | 0.63 | 0.62 | 0.57 |
| carry select, list | 1 | 0.53 | 0.47 | 0.38 | 0.39 | 0.45 |
| carry select, list | 2 | 0.52 | 0.49 | 0.35 | 0.37 | 0.46 |
| carry select, list | 4 | 0.55 | 0.49 | 0.30 | 0.33 | 0.46 |
| carry select, list | 8 | 0.52 | 0.50 | 0.42 | 0.43 | 0.47 |
| carry select, list | 16 | 0.51 | 0.48 | 0.39 | 0.40 | 0.50 |
| carry select, list | 32 | 0.48 | 0.49 | 0.26 | 0.25 | 0.53 |
| carry select, list | 64 | 0.45 | 0.49 | 0.26 | 0.23 | 0.60 |
| carry select, tree | 1 | 0.53 | 0.48 | 0.40 | 0.40 | 0.45 |
| carry select, tree | 2 | 0.52 | 0.47 | 0.39 | 0.38 | 0.45 |
| carry select, tree | 4 | 0.53 | 0.48 | 0.45 | 0.52 | 0.46 |
| carry select, tree | 8 | 0.53 | 0.50 | 0.68 | 0.71 | 0.47 |
| carry select, tree | 16 | 0.39 | 0.49 | 0.69 | 0.71 | 0.49 |
| carry select, tree | 32 | - | - | - | - | - |

## Runs that did not finish or were wrong

- add:unary:1023+1 []: 0/2 ok, ['timed out']
- add:unary:1024+1 []: 0/2 ok, ['timed out']
- added:tree:16:m4 []: 0/2 ok, ['timed out']
- added:tree:16:m8 []: 0/2 ok, ['timed out']
- added:tree:8:m6 []: 1/2 ok, ['ok', 'timed out']
- added:tree:8:m8 []: 0/2 ok, ['timed out']
- relay_for:list:64:max []: 0/2 ok, ['out of memory at load (5 GB cap)']
- relay_for:list:64:max [views=1]: 0/1 ok, ['out of memory at the 1.8 GB cap']
- relay_for:list:64:mixed []: 0/2 ok, ['out of memory at load (5 GB cap)']
- relay_for:list:64:mixed [views=1]: 0/1 ok, ['out of memory at the 1.8 GB cap']
- saved:tree:16:m4 []: 0/2 ok, ['timed out']
- saved:tree:16:m8 []: 0/2 ok, ['timed out']
- saved:tree:8:m6 []: 0/2 ok, ['timed out']
- saved:tree:8:m8 []: 0/2 ok, ['timed out']
- select_add:tree:32:max []: 0/2 ok, ['timed out']
- select_add:tree:32:mixed []: 2/3 ok, ['ok', 'timed out']
