#!/usr/bin/env python3

import random, argparse

"""
Generate a CNF expression of form `(x || !y) ...`.
"""
def generate_cnf_expr(num_vars: int, num_clauses: int, var_range_per_clause: tuple[int, int]) -> str:
    variables = [f"x{i}" for i in range(num_vars)]

    clauses = []
    for _ in range(num_clauses):
        clause = []
        for _ in range(random.randint(*var_range_per_clause)):
            var = random.choice(variables)
            if random.choice([True, False]):
                var = f"!{var}"
            clause.append(var)
        clauses.append(f"({" || ".join(clause)})")

    return " && ".join(clauses)

parser = argparse.ArgumentParser(
    prog='generate_expr',
    description='Generate CNF expressions to solve with desat',
)

parser.add_argument(
    'num_vars',
    type=int,
    help='Number of variables in the expression',
)

parser.add_argument(
    'num_clauses',
    type=int,
    help='Number of clauses in the expression',
)

parser.add_argument(
    'num_vars_per_clause_range',
    type=int,
    nargs=2,
    help='Range of variables per clause',
)

if __name__ == '__main__':
    args = parser.parse_args()
    print(generate_cnf_expr(args.num_vars, args.num_clauses, args.num_vars_per_clause_range))
