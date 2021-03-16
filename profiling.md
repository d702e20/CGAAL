# Profiling

Testing depends on which metrics you want. If you're optimising the algorithm e.g. for better negation edge termination
you may only be interested in the hitcounts of each action. You could also be interested in the specific time that 
a function takes to complete a given task.

Common is that the context; e.g. model and formula should be static across your tests.

Remember to always compile for release when profiling, we want the compiler to do optimisations.

## Counts
To count events, we use the [counts](https://github.com/nnethercote/counts/) program, install with cargo and use binary
placement to execute (e.g. `~/.cargo/bin/counts`).

Counts takes a log file as input. This is just a file consisting of strings signifying events, e.g. (worker explore).
To emit these, use `eprintln` in relevant sections, Counts uses string equality to match events, there's no nested behaviour:
```rust
#[cfg(feature = "use-counts")]
eprintln!("worker processing_hyper-edge");
```

Use the `count-atlsolver.sh` script (for inspiration) to run atlsolver and only redirect stderr to a file.


1. Recompile atlsolver for release `cargo build --features use-counts --release`
1. Generate count-data and redirect stderr to file `./count-atlsolver.sh count.log` 
1. Run counts on resulting file `~/.cargo/bin/counts -w count.log`

Example result (mexi4-1hp on sui):
```txt
738 counts:
(  1)      112 (15.2%, 15.2%): worker explore
(  2)       92 (12.5%, 27.6%): worker receive_message
(  3)       80 (10.8%, 38.5%): worker receive_hyper-edge
(  4)       80 (10.8%, 49.3%): worker processing_hyper-edge
(  5)       77 (10.4%, 59.8%): worker receive_negation_edge
(  6)       75 (10.2%, 69.9%): worker mark_interest
(  7)       63 ( 8.5%, 78.5%): worker final_assign
(  8)       61 ( 8.3%, 86.7%): worker process_request
(  9)       49 ( 6.6%, 93.4%): worker succ
( 10)       16 ( 2.2%, 95.5%): worker create
( 11)       16 ( 2.2%, 97.7%): worker start
( 12)        8 ( 1.1%, 98.8%): worker processing negation edge
( 13)        1 ( 0.1%, 98.9%): 14
( 14)        1 ( 0.1%, 99.1%): thread 'worker succ
( 15)        1 ( 0.1%, 99.2%): ', Failed to return weight to controller: "SendError(..)"src/com.rsworker final_assign
( 16)        1 ( 0.1%, 99.3%): ', :src/com.rsthread '74:worker received_termination
( 17)        1 ( 0.1%, 99.5%): worker received_termination
( 18)        1 ( 0.1%, 99.6%): <unnamed>:74' panicked at '14:worker received_termination
( 19)        1 ( 0.1%, 99.7%): ' panicked at '<unnamed>Failed to return weight to controller: "SendError(..)"' panicked at 'worker receive_message
( 20)        1 ( 0.1%, 99.9%): Failed to return weight to controller: "SendError(..)"
( 21)        1 ( 0.1%,100.0%): <unnamed>thread 'worker received_termination
```

Note that this output has 'normal' errors interleaved. This is not a problem as these are naturally fewer than hot
regions, and therefore bubble down the results. If you adhere to a strict schema you can grep for prefix; `grep -e "^worker"`.
The `count-atlsolver.sh` script does this automatically for you.

## Time
Measuring runtime of the entire execution of the solver is useful for comparing high-level performance between code-bases.
For this purpose we use Criterion, at least until `cargo bench` becomes more mature and `#[bench]` attribute is no longer nightly-only.
Benchmarking is set up as single runs of model-formula pairs, either with just NUM_CPU threads with the `bench_lcgs` macro
or using the `bench_lcgs_threads` macro for individual tests for all thread-counts from 1 to NUM_CPU.

Usage is fairly simple, use `bench_lcgs!` or `bench_lcgs_threads!` and give arguments; name, model-, and formula- paths. 
The macro sets up all boilerplate and you can now add this benchmark to a `criterion_group!` for running by `criterion_main!`.

Output of benchmarking is a very readable HTML-report, CSV/JSON output if we want to further post-process the measurements
or stdout readable metrics:
```
mexican_standoff_lcgs_alive_till_not_threads/16                                                                            
                        time:   [24.017 ms 25.128 ms 26.284 ms]
                        change: [-59.416% -55.977% -52.171%] (p = 0.00 < 0.05)
                        Performance has improved.
Found 5 outliers among 100 measurements (5.00%)
  5 (5.00%) high mild

```

Time profiling is mostly useful for comparisons to some other code-base. Criterion always saves the last run and compares any new run to the previous one.
You can also use [baselines](https://bheisler.github.io/criterion.rs/book/user_guide/command_line_options.html), so you can easily compare your feature-branch's performance to that of main:
```shell
git checkout main
cargo bench -- --save-baseline main
git checkout feature
cargo bench
cargo bench -- --load-baseline new --baseline main # does not rerun benchmark, only generates data and report
```

It is very important to be aware of the impact the system load and performance characteristics have on results. 
A quiet, dedicated machine is preferable to obtain precise and repeatable results. 
For this purpose running the benchmarks on the AAU MMC in a dedicated job is optimal for final results. 
Intermediate results from benchmarking on your own machine can be useful, especially for huge performance changes, but be cautious.

## Profiling
IDE's can do various profiling which may be useful depending on your goal. CLion can generate flamegraphs, call trees, and method lists.
Microsoft Visual Studio can do similar profiling. Plenty of documentation is available for these use-cases.

### Flamegraph
These are useful for identifying hot sections that are candidates for optimisation. Width is a function of time, and height is call-stack.
A particularly wide and tall function is therefore a good place to start. Note that you may need to turn on symbols in Cargo.toml if you get mangled function names.

### Call tree
Identify the call stack of the program. Shows percentage of total time spent for the given function and number of invocations (samples).
This clashes with the Counts functionality somewhat. However, both can be useful depending on what you're investigating:
- If you want a general overview, use the builtin profiling call tree.
- If you want to investigate specific parts of the program and maybe record values, use Counts.

The method list view of CLion is similar to call tree but sorts them by cumulative sample time. 