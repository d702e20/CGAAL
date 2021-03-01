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
TBD