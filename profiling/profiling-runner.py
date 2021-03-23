"""
1. run suite (maybe mutate criterion group before running, would require editing and recompilation maybe)
    - wait return
    - log stderr, grep for counts prefix, redirect to $suite-time.txt output (todo: requires worker stats setup)
1. pre-process $suite-time.txt into n benches files; $suite-time/$bench_name.txt
1. run counts on all $suite-time/$bench_name.txt's, redirecting to $suite-time/$bench_name-counts-results.txt
1. copy target/criterion/$suite-time to benches/results/$suite-time/
1. collect regress/improve into easy-to-digest format, maybe print to stdout along with counts stats if available?
"""

import subprocess

USE_COUNTS = True

subprocess.run(f'cargo bench --manifest-path ../atl-checker/Cargo.toml {"--features use-counts" if USE_COUNTS else ""}', shell=True)
