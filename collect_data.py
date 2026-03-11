import os
import subprocess
import csv
import re
import shutil

ITERATIONS = 10
ASSEMBLY_DIR = "assembly"
BIN_DIR = "binaries"
OUTPUT_CSV = "benchmark_results.csv"
VERSIONS = [
    "unoptimised",
    "optimised",
    "optimised-binary",
    "optimised-registers",
    "optimised-branches",
    "optimised-memory"
]

def run_perf(binary_path):
    cmd = ["perf", "stat", "-e", "instructions,cycles", "-x", ",", binary_path]
    result = subprocess.run(cmd, stderr=subprocess.PIPE, stdout=subprocess.DEVNULL, text=True)
    
    instructions, cycles, time_sec = 0, 0, 0.0
    for line in result.stderr.splitlines():
        parts = line.split(',')
        if len(parts) < 3 or parts[0] == '<not supported>': continue
        
        value = parts[0]
        event = parts[2]
        if event == 'instructions': instructions = int(value)
        elif event == 'cycles': cycles = int(value)
            
    time_match = re.search(r'([0-9.]+)\s+seconds time elapsed', result.stderr)
    if time_match: time_sec = float(time_match.group(1))
    return instructions, cycles, time_sec

def phase_one_build():
    print("\n=== PHASE 1: Build & Verify ===")
    
    # Use unoptimised as the master list of benchmarks
    master_dir = os.path.join(ASSEMBLY_DIR, "unoptimised")
    if not os.path.exists(master_dir):
        print(f"Error: Master directory '{master_dir}' not found")
        return []

    benchmarks = [f.replace(".s", "") for f in os.listdir(master_dir) if f.endswith(".s")]
    valid_benchmarks = []

    for bench in benchmarks:
        print(f"Checking: {bench}...", end=" ")
        all_versions_exist = True
        
        # Verify all ablation folders have this benchmark before starting
        for v in VERSIONS:
            if not os.path.exists(os.path.join(ASSEMBLY_DIR, v, f"{bench}.s")):
                all_versions_exist = False
                break
        
        if not all_versions_exist:
            print("FAILED (Missing versions)")
            continue

        # Compile each version
        for v in VERSIONS:
            os.makedirs(os.path.join(BIN_DIR, v), exist_ok=True)
            asm_file = os.path.join(ASSEMBLY_DIR, v, f"{bench}.s")
            bin_file = os.path.join(BIN_DIR, v, bench)
            
            try:
                subprocess.run(["gcc", "-o", bin_file, asm_file], check=True, capture_output=True)
            except subprocess.CalledProcessError:
                print(f"FAILED (GCC error on {v})")
                all_versions_exist = False
                break
        
        if all_versions_exist:
            print("OK")
            valid_benchmarks.append(bench)

    return valid_benchmarks

def phase_two_benchmark(valid_benchmarks):
    print(f"\n=== PHASE 2: Benchmarking ({ITERATIONS} iterations) ===")

    with open(OUTPUT_CSV, mode='w', newline='') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(["Benchmark", "Version", "Instructions", "Cycles", "IPC", "Time (sec)"])

        for bench in valid_benchmarks:
            print(f"\n--- Benchmark: {bench} ---")
            for v in VERSIONS:
                bin_file = os.path.join(BIN_DIR, v, bench)
                total_inst, total_cycles, total_time = 0, 0, 0.0
                
                for _ in range(ITERATIONS):
                    inst, cyc, t = run_perf(bin_file)
                    total_inst += inst
                    total_cycles += cyc
                    total_time += t
                
                avg_inst = total_inst // ITERATIONS
                avg_cycles = total_cycles // ITERATIONS
                avg_time = total_time / ITERATIONS
                ipc = avg_inst / avg_cycles if avg_cycles > 0 else 0
                
                writer.writerow([bench, v, avg_inst, avg_cycles, round(ipc, 3), round(avg_time, 5)])
                print(f"  {v.ljust(20)}: {avg_inst:10} inst | {avg_cycles:10} cyc | IPC: {ipc:.2f}")

def main():
    if shutil.which("perf") is None:
        print("Error: 'perf' not found")
        return

    valid_benchmarks = phase_one_build()
    if not valid_benchmarks: return
        
    phase_two_benchmark(valid_benchmarks)
    
    if os.path.exists(BIN_DIR):
        shutil.rmtree(BIN_DIR)
        print("Temporary binaries removed.")

    print(f"Done! Output in {OUTPUT_CSV}")

if __name__ == "__main__":
    main()