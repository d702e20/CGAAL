use crate::algorithms::solve_set::{minimum_solve_set, SolveSetAssignment};
use crate::atl::Phi;
use crate::edg::atlcgsedg::AtlVertex;
use crate::edg::{Edge, ExtendedDependencyGraph};

#[derive(Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum VertexData {
    #[serde(rename = "proposition")]
    Proposition { satisfied: bool },
    #[serde(rename = "negation")]
    Not { stats: PhiStats },
    #[serde(rename = "disjunction")]
    Or {
        stats: PhiStats,
        target_stats: Vec<PhiStats>,
    },
    #[serde(rename = "conjunction")]
    And {
        stats: PhiStats,
        target_stats: Vec<PhiStats>,
    },
    #[serde(rename = "despite next")]
    DespiteNext {
        player_count: u32,
        stats: PhiStats,
        target_stats: Vec<PhiStats>,
    },
    #[serde(rename = "enforce next")]
    EnforceNext {
        player_count: u32,
        stats: PhiStats,
        edge_stats: Vec<Vec<PhiStats>>,
    },
    #[serde(rename = "despite until")]
    DespiteUntil {
        player_count: u32,
        stats: PhiStats,
        until_stats: PhiStats,
        pre_stats: PhiStats,
        other_target_stats: Vec<PhiStats>,
    },
    #[serde(rename = "enforce until")]
    EnforceUntil {
        player_count: u32,
        stats: PhiStats,
        until_stats: PhiStats,
        pre_stats: PhiStats,
        other_edge_stats: Vec<Vec<PhiStats>>,
    },
    #[serde(rename = "despite eventually")]
    DespiteEventually {
        player_count: u32,
        stats: PhiStats,
        until_stats: PhiStats,
        other_target_stats: Vec<PhiStats>,
    },
    #[serde(rename = "enforce eventually")]
    EnforceEventually {
        player_count: u32,
        stats: PhiStats,
        until_stats: PhiStats,
        other_edge_stats: Vec<Vec<PhiStats>>,
    },
    #[serde(rename = "despite invariant")]
    DespiteInvariant { player_count: u32, stats: PhiStats },
    #[serde(rename = "enforce invariant")]
    EnforceInvariant { player_count: u32, stats: PhiStats },
    #[serde(rename = "partial")]
    Partial {
        stats: PhiStats,
        target_stats: Vec<PhiStats>,
    },
}

#[derive(Serialize, Deserialize)]
pub struct PhiStats {
    solve_set_size: u32,
    formula_size: u32,
    formula_depth: u32,
    qualifier_count: u32,
    qualifier_depth: u32,
}

/// Analyses the vertices of an EDG starting from the given root, and returns a Vec of data
/// describing each vertex with different data depending of the vertex' ATL formula.
pub fn analyse<G: ExtendedDependencyGraph<AtlVertex>>(edg: &G, root: AtlVertex) -> Vec<VertexData> {
    let mss = minimum_solve_set(edg, root);
    let mut data = vec![];
    for (v, mssa) in &mss {
        match v {
            AtlVertex::Full { formula, .. } => match formula.as_ref() {
                Phi::Proposition(..) => data.push(VertexData::Proposition {
                    // Propositions are assigned 1 in certain zero iff they have a positive signed
                    // solve set
                    satisfied: mss.get(v).unwrap().signed_len() > 0,
                }),
                Phi::Not(..) => data.push(VertexData::Not {
                    stats: phi_stats(mssa, v),
                }),
                Phi::Or(..) => data.push(VertexData::Or {
                    stats: phi_stats(mssa, v),
                    target_stats: edg
                        .succ(v)
                        .iter()
                        .map(|e| {
                            // Or-formula have multiple hyper-edges with one target
                            if let Edge::Hyper(e) = e {
                                phi_stats(mssa, &e.targets.get(0).unwrap())
                            } else {
                                unreachable!()
                            }
                        })
                        .collect(),
                }),
                Phi::And(..) => data.push(VertexData::And {
                    stats: phi_stats(mssa, v),
                    // And-formulae only have one hyper-edge with multiple targets
                    target_stats: if let Edge::Hyper(e) = edg.succ(v).get(0).unwrap() {
                        e.targets.iter().map(|t| phi_stats(mssa, t)).collect()
                    } else {
                        unreachable!()
                    },
                }),
                Phi::DespiteNext { players, .. } => data.push(VertexData::DespiteNext {
                    player_count: players.len() as u32,
                    stats: phi_stats(mssa, v),
                    // And-formulae only have one hyper-edge with multiple targets
                    target_stats: if let Edge::Hyper(e) = edg.succ(v).get(0).unwrap() {
                        e.targets.iter().map(|t| phi_stats(mssa, t)).collect()
                    } else {
                        unreachable!()
                    },
                }),
                Phi::EnforceNext { players, .. } => data.push(VertexData::EnforceNext {
                    player_count: players.len() as u32,
                    stats: phi_stats(mssa, v),
                    edge_stats: edg
                        .succ(v)
                        .iter()
                        .map(|e| e.targets().iter().map(|t| phi_stats(mssa, t)).collect())
                        .collect(),
                }),
                Phi::DespiteUntil { players, .. } => {
                    let edges = edg.succ(v);
                    // Use the fact that the `until` branch is the first edge in successors
                    let until_stats = phi_stats(mssa, &edges[0].targets().first().unwrap());
                    // Use the fact that `pre` target is the first target of the second edge
                    let second_edge_targets = edges.get(1).unwrap().targets();
                    let pre_target = second_edge_targets.first().unwrap();
                    let pre_stats = phi_stats(mssa, pre_target);
                    let other_target_stats: Vec<PhiStats> = second_edge_targets[1..]
                        .iter()
                        .map(|t| phi_stats(mssa, t))
                        .collect();

                    data.push(VertexData::DespiteUntil {
                        player_count: players.len() as u32,
                        stats: phi_stats(mssa, v),
                        until_stats,
                        pre_stats,
                        other_target_stats,
                    })
                }
                Phi::EnforceUntil { players, .. } => {
                    let edges = edg.succ(v);
                    // Use the fact that the `until` branch is the first edge in successors
                    let until_stats = phi_stats(mssa, &edges[0].targets().first().unwrap());
                    // Use the fact that `pre` target is the first target of the second edge
                    let second_edge_targets = edges.get(1).unwrap().targets();
                    let pre_target = second_edge_targets.first().unwrap();
                    let pre_stats = phi_stats(mssa, pre_target);
                    // Stats of the other edges and their targets (except `pre` target)
                    let other_edge_stats: Vec<Vec<PhiStats>> = edges[1..]
                        .iter()
                        .map(|e| {
                            // skip the first target, since it is the `pre` target
                            e.targets()
                                .iter()
                                .skip(1)
                                .map(|t| phi_stats(mssa, t))
                                .collect()
                        })
                        .collect();

                    data.push(VertexData::EnforceUntil {
                        player_count: players.len() as u32,
                        stats: phi_stats(mssa, v),
                        until_stats,
                        pre_stats,
                        other_edge_stats,
                    })
                }
                Phi::DespiteEventually { players, .. } => {
                    let edges = edg.succ(v);
                    // Use the fact that the `until` branch is the first edge in successors
                    let until_stats = phi_stats(mssa, &edges[0].targets().first().unwrap());
                    let second_edge_targets = edges.get(1).unwrap().targets();
                    let other_target_stats: Vec<PhiStats> = second_edge_targets[1..]
                        .iter()
                        .map(|t| phi_stats(mssa, t))
                        .collect();

                    data.push(VertexData::DespiteEventually {
                        player_count: players.len() as u32,
                        stats: phi_stats(mssa, v),
                        until_stats,
                        other_target_stats,
                    })
                }
                Phi::EnforceEventually { players, .. } => {
                    let edges = edg.succ(v);
                    // Use the fact that the `until` branch is the first edge in successors
                    let until_stats = phi_stats(mssa, &edges[0].targets().first().unwrap());
                    // Stats of the other edges and their targets
                    let other_edge_stats: Vec<Vec<PhiStats>> = edges[1..]
                        .iter()
                        .map(|e| e.targets().iter().map(|t| phi_stats(mssa, t)).collect())
                        .collect();

                    data.push(VertexData::EnforceEventually {
                        player_count: players.len() as u32,
                        stats: phi_stats(mssa, v),
                        until_stats,
                        other_edge_stats,
                    })
                }
                Phi::DespiteInvariant { players, .. } => {
                    // DespiteInvariant formulae only have one negation edge, so stats are boring
                    data.push(VertexData::DespiteInvariant {
                        player_count: players.len() as u32,
                        stats: phi_stats(mssa, v),
                    })
                }
                Phi::EnforceInvariant { players, .. } => {
                    // EnforceInvariant formulae only have one negation edge, so stats are boring
                    data.push(VertexData::EnforceInvariant {
                        player_count: players.len() as u32,
                        stats: phi_stats(mssa, v),
                    })
                }
                _ => {}
            },
            AtlVertex::Partial { .. } => data.push(VertexData::Partial {
                stats: phi_stats(mssa, v),
                target_stats: edg
                    .succ(v)
                    .iter()
                    .map(|e| {
                        // Partial vertices have multiple hyper-edges with one target
                        if let Edge::Hyper(e) = e {
                            phi_stats(mssa, &e.targets.get(0).unwrap())
                        } else {
                            unreachable!()
                        }
                    })
                    .collect(),
            }),
        }
    }
    data
}

/// Returns statistics about a given vertex
fn phi_stats(mssa: &SolveSetAssignment<AtlVertex>, v: &AtlVertex) -> PhiStats {
    let phi = v.formula();
    PhiStats {
        solve_set_size: mssa.len() as u32,
        formula_size: phi.size(),
        formula_depth: phi.depth(),
        qualifier_count: phi.path_qualifier_count(),
        qualifier_depth: phi.path_qualifier_depth(),
    }
}
