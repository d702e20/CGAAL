use crate::common::Edges;
use crate::edg::{ExtendedDependencyGraph, Vertex};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashSet, VecDeque};
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::io::Write;

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

pub(crate) fn print_graph<V: Vertex, G: ExtendedDependencyGraph<V>, W: Write>(
    graph: G,
    v0: V,
    mut output: W,
) -> std::io::Result<()> {
    output.write("digraph edg {\n".as_bytes())?;

    let mut visited: HashSet<V> = HashSet::new();
    let mut queue: VecDeque<V> = VecDeque::new();
    let mut hyper_idx = 0usize;

    queue.push_back(v0.clone());

    print_vertex(&v0, &mut output)?;
    visited.insert(v0);

    while !queue.is_empty() {
        let vertex = queue
            .pop_front()
            .expect("non-empty queue failed to pop element");

        for edge in graph.succ(&vertex) {
            // TODO maybe print labels on edges
            match edge {
                Edges::HYPER(hyper) => {
                    if hyper.targets.is_empty() {
                        output
                            .write(format!("v{} -> ∅;\n", hash_name(&hyper.source)).as_bytes())?;
                    } else {
                        if hyper.targets.len() == 1 {
                            let target = hyper.targets.get(0).unwrap();

                            output.write(
                                format!(
                                    "v{} -> v{};\n",
                                    hash_name(&hyper.source),
                                    hash_name(&target)
                                )
                                .as_bytes(),
                            )?;

                            if !visited.contains(&target) {
                                print_vertex(&target, &mut output);
                                queue.push_back(target.clone());
                                visited.insert(target.clone());
                            }
                        } else {
                            output.write(
                                format!(
                                    "h{}[shape=none,label=\"\",width=0,height=0];\n",
                                    hyper_idx
                                )
                                .as_bytes(),
                            )?;
                            output.write(
                                format!(
                                    "v{} -> h{}[dir=none];\n",
                                    hash_name(&hyper.source),
                                    hyper_idx
                                )
                                .as_bytes(),
                            )?;
                            for target in hyper.targets {
                                output.write(
                                    format!("h{} -> v{};\n", hyper_idx, hash_name(&target))
                                        .as_bytes(),
                                )?;

                                if !visited.contains(&target) {
                                    print_vertex(&target, &mut output);
                                    queue.push_back(target.clone());
                                    visited.insert(target);
                                }
                            }
                            hyper_idx += 1;
                        }
                    }
                }
                Edges::NEGATION(neg) => {
                    output.write(
                        format!(
                            "v{} -> v{}[style=dashed];\n",
                            hash_name(&neg.source),
                            hash_name(&neg.target)
                        )
                        .as_bytes(),
                    )?;

                    if !visited.contains(&neg.target) {
                        print_vertex(&neg.target, &mut output);
                        queue.push_back(neg.target.clone());
                        visited.insert(neg.target);
                    }
                }
            };
        }
    }

    output.write("}\n".as_bytes())?;

    Ok(())
}
