use crate::common::Edges;
use crate::edg::{ExtendedDependencyGraph, Vertex};
use std::collections::{HashSet, VecDeque};
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::collections::hash_map::DefaultHasher;
use serde::export::fmt::Display;

fn print_vertex<V: Hash + Display, W: Write>(vertex: V, mut output: W) -> std::io::Result<()> {
    println!("{} = {}", hash_name(&vertex), vertex);

    output.write(format!("v{}[label=\"{}\"];\n", hash_name(&vertex), vertex).as_bytes())?;
    Ok(())
}

fn hash_name<V: Hash>(vertex: &V) -> String {
    let mut hasher = DefaultHasher::new();
    vertex.hash::<DefaultHasher>(&mut hasher);
    hasher.finish().to_string()
}

pub(crate) fn print_graph<
    V: Vertex,
    G: ExtendedDependencyGraph<V>,
    W: Write,
>(
    graph: G,
    v0: V,
    mut output: W,
) -> std::io::Result<()> {
    output.write("digraph edg {\n".as_bytes())?;

    let mut visited: HashSet<V> = HashSet::new();
    let mut queue = VecDeque::new();
    let mut hyper_idx = 0usize;

    for edge in graph.succ(&v0) {
        queue.push_back(edge);
    }
    print_vertex(&v0, &mut output)?;
    visited.insert(v0);

    while !queue.is_empty() {
        let edge = queue
            .pop_front()
            .expect("non-empty queue failed to pop element");

        // TODO maybe print labels on edges
        match edge {
            Edges::HYPER(hyper) => {
                if hyper.targets.is_empty() {
                    output.write(format!("v{} -> ∅;\n", hash_name(&hyper.source)).as_bytes())?;
                } else {
                    output.write(format!("h{}[shape=none,label=\"\",width=0,height=0];\n", hyper_idx).as_bytes())?;
                    output.write(format!("v{} -> h{}[dir=none];\n", hash_name(&hyper.source), hyper_idx).as_bytes())?;
                    for target in hyper.targets {
                        output.write(format!("h{} -> v{};\n", hash_name(&hyper.source), hyper_idx).as_bytes())?;

                        if !visited.contains(&target) {
                            print_vertex(&target, &mut output);
                            visited.insert(target);
                        }
                    }
                    hyper_idx += 1;
                }
            }
            Edges::NEGATION(neg) => {
                output.write(format!("v{} -> v{}[style=dashed];\n", hash_name(&neg.source), hash_name(&neg.target)).as_bytes())?;

                if !visited.contains(&neg.target) {
                    print_vertex(&neg.target, &mut output);
                    visited.insert(neg.target);
                }
            }
        };
    }


    output.write("}\n".as_bytes())?;

    Ok(())
}
