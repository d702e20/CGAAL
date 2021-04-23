use std::collections::hash_map::DefaultHasher;
use std::collections::{HashSet, VecDeque};
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::io::Write;

use crate::edg::{Edge, ExtendedDependencyGraph, Vertex};

// The graph can be rendered using the `dot` command graphviz, specifically like this `dot -Tpng graph.dot -O -Nfontname=noto`

fn print_vertex<V: Hash + Display, W: Write>(vertex: V, mut output: W) -> std::io::Result<()> {
    output.write_all(
        format!("v{}[label=\"{}\",shape=box];\n", hash_name(&vertex), vertex).as_bytes(),
    )?;
    Ok(())
}

fn hash_name<V: Hash>(vertex: &V) -> String {
    let mut hasher = DefaultHasher::new();
    vertex.hash::<DefaultHasher>(&mut hasher);
    hasher.finish().to_string()
}

pub fn print_graph<V: Vertex, G: ExtendedDependencyGraph<V>, W: Write>(
    graph: G,
    v0: V,
    mut output: W,
) -> std::io::Result<()> {
    output.write_all(b"digraph edg {\n")?;

    let mut visited: HashSet<V> = HashSet::new();
    let mut queue: VecDeque<V> = VecDeque::new();

    queue.push_back(v0.clone());

    print_vertex(&v0, &mut output)?;
    visited.insert(v0);

    while !queue.is_empty() {
        let vertex = queue
            .pop_front()
            .expect("non-empty queue failed to pop element");

        for edge in graph.succ(&vertex) {
            // TODO maybe print labels on edges
            let hyper_id = hash_name(&edge);

            match edge {
                Edge::HYPER(hyper) => {
                    print_vertex(&hyper.source, &mut output)?;

                    if hyper.targets.is_empty() {
                        let empty_id = hash_name(&hyper);
                        output.write_all(
                            format!(
                                "empty{}[shape=none,label=\"∅\"];\nv{} -> empty{};\n",
                                empty_id,
                                hash_name(&hyper.source),
                                empty_id
                            )
                            .as_bytes(),
                        )?;
                    } else if hyper.targets.len() == 1 {
                        let target = hyper.targets.get(0).unwrap();

                        output.write_all(
                            format!(
                                "v{} -> v{};\n",
                                hash_name(&hyper.source),
                                hash_name(&target)
                            )
                            .as_bytes(),
                        )?;

                        if !visited.contains(&target) {
                            print_vertex(&target, &mut output)?;
                            queue.push_back(target.clone());
                            visited.insert(target.clone());
                        }
                    } else {
                        output.write_all(
                            format!("h{}[shape=none,label=\"\",width=0,height=0];\n", hyper_id)
                                .as_bytes(),
                        )?;
                        output.write_all(
                            format!(
                                "v{} -> h{}[dir=none];\n",
                                hash_name(&hyper.source),
                                hyper_id
                            )
                            .as_bytes(),
                        )?;
                        for target in hyper.targets {
                            output.write_all(
                                format!("h{} -> v{};\n", hyper_id, hash_name(&target)).as_bytes(),
                            )?;

                            if !visited.contains(&target) {
                                print_vertex(&target, &mut output)?;
                                queue.push_back(target.clone());
                                visited.insert(target);
                            }
                        }
                    }
                }
                Edge::NEGATION(neg) => {
                    print_vertex(&neg.source, &mut output)?;

                    output.write_all(
                        format!(
                            "v{} -> v{}[style=dashed];\n",
                            hash_name(&neg.source),
                            hash_name(&neg.target)
                        )
                        .as_bytes(),
                    )?;

                    if !visited.contains(&neg.target) {
                        print_vertex(&neg.target, &mut output)?;
                        queue.push_back(neg.target.clone());
                        visited.insert(neg.target);
                    }
                }
            };
        }
    }

    output.write_all(b"}\n")?;

    Ok(())
}
