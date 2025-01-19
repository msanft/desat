import generate_expr
import argparse, subprocess, time
import matplotlib.pyplot as plt
import numpy as np
import os

def run_benchmark() -> None:
    try: os.stat("benchmark-results")
    except FileNotFoundError:
        print(("Results directory (`./benchmark-results`) not found. Creating it..."))
        os.mkdir("benchmark-results")

    configs = [
        ("Small", [10, 10, (1, 10)]),
        ("Medium", [50, 50, (1, 50)]),
        ("Large", [100, 100, (1, 100)]),
    ]
    iterations = 10
    timeout_threshold = 10

    for config in configs:
        times = []
        timeout_count = 0

        for i in range(iterations):
            expr = generate_expr.generate_cnf_expr(*config[1])
            now = time.time()
            try:
                result = subprocess.run(["desat", expr], capture_output=True, timeout=timeout_threshold)
                elapsed = time.time() - now
                times.append(elapsed)
            except subprocess.TimeoutExpired:
                timeout_count += 1

        fig, ax = plt.subplots(figsize=(4, 6))

        bp = ax.boxplot(times, patch_artist=True)

        plt.setp(bp["boxes"], facecolor="lightblue", alpha=0.7)
        plt.setp(bp["medians"], color="navy")
        plt.setp(bp["fliers"], marker="o", markerfacecolor="red", alpha=0.5)

        print(f"Results for {config[0]}:")
        if times:
            mean = np.mean(times)
            stddev = np.std(times)
            min_time = min(times)
            max_time = max(times)

            print(f"\tMean: {mean}s")
            print(f"\tMin: {min_time}s")
            print(f"\tMax: {max_time}s")
            print(f"\tStddev: {stddev}s")
            print(f"\tTimeouts: {timeout_count}/{iterations}")

            padding = (max_time - min_time) * 0.1  # 10% padding
            ax.set_ylim(max(0, min_time - padding), max_time + padding)
        else:
            print("\tNo results")

        ax.set_title(f"Performance Distribution - Benchmark: {config[0]} (Excluding Timeouts)")
        ax.set_ylabel("Time (Seconds)")

        plt.tight_layout()
        plt.savefig("benchmark-results/desat_benchmark_" + config[0].lower() + ".png", bbox_inches="tight")
        plt.close()

    print(f"Results saved to `./benchmark-results`")

parser = argparse.ArgumentParser(
    prog="benchmark",
    description="Benchmark desat",
)

if __name__ == "__main__":
    args = parser.parse_args()
    run_benchmark()
