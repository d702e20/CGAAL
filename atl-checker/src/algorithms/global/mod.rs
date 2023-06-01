mod com;
pub mod multithread;
pub mod singlethread;

use crate::edg::{Edge, ExtendedDependencyGraph, HyperEdge, NegationEdge, Vertex};
use std::cmp::max;
use std::collections::{HashMap, HashSet, VecDeque};

pub trait GlobalAlgorithm<G: ExtendedDependencyGraph<V>, V: Vertex> {
    fn edg(&self) -> &G;
    fn edg_mut(&mut self) -> &mut G;
    fn v0(&self) -> &V;
    fn v0_mut(&mut self) -> &mut V;
    fn assignment(&self) -> &HashMap<V, bool>;
    fn assignment_mut(&mut self) -> &mut HashMap<V, bool>;
    fn dist(&self) -> &VecDeque<HashSet<V>>;
    fn dist_mut(&mut self) -> &mut VecDeque<HashSet<V>>;

    /// Firing the global algorithm, which simply reapplies the F_i function explained in the paper
    /// until the assignment does not change anymore.
    /// Lastly the assignment of the root, v0, is returned
    fn run(&mut self) -> bool {
        self.initialize();

        let components = self.dist().clone();
        components
            .iter()
            .rev()
            .for_each(|component| while self.f(component) {});

        *self.assignment().get(&self.v0()).unwrap()
    }

    /// The resemblance of the function described in the paper, but in order to
    /// know which component we are worker with and the assignments of the component
    /// before, these are both given as arguments.
    /// The function it self returns a boolean which is true if the assignments are changed.
    fn f(&mut self, component: &HashSet<V>) -> bool {
        let mut changed_flag = false;

        for vertex in component {
            for edge in self.edg().succ(vertex) {
                match edge {
                    Edge::Hyper(e) => changed_flag = max(self.process_hyper(e), changed_flag),
                    Edge::Negation(e) => changed_flag = max(self.process_negation(e), changed_flag),
                }
            }
        }
        changed_flag
    }

    /// Rising the value of the source vertex assignment if all targets are true or empty. If the
    /// source vertex already is true we simply return. The return value is based on if a changed
    /// have been made.
    fn process_hyper(&mut self, edge: HyperEdge<V>) -> bool {
        if *self.assignment().get(&edge.source).unwrap() {
            false
        } else {
            let mut final_ass = true;
            for target in edge.targets {
                if !self.assignment().get(&target).unwrap() {
                    final_ass = false;
                    break;
                }
            }
            self.update_assignment(edge.source, final_ass)
        }
    }

    /// Rising the value of the source vertex assignment if the target vertex was assigned
    /// false in the last component assignment. If the source vertex already is true we
    /// simply return. The return value is based on if a changed have been made.
    fn process_negation(&mut self, edge: NegationEdge<V>) -> bool {
        if *self.assignment().get(&edge.source).unwrap() {
            false
        } else {
            let new_ass = !*self.assignment().get(&edge.target).unwrap();
            self.update_assignment(edge.source, new_ass)
        }
    }

    /// Updating an assignment to the new_ass value, if the new_ass are
    /// equal to the old assignment, we simply return.
    /// The return value is based on whether the assignment of the given vertex is changed or not.
    fn update_assignment(&mut self, v: V, new_ass: bool) -> bool {
        self.assignment_mut()
            .get_mut(&v)
            .map(|ass| {
                let old = *ass;
                *ass = new_ass;
                new_ass != old
            })
            .unwrap()
    }

    /// Initialize the dist and assignments by traversing through all edges from root
    fn initialize(&mut self) {
        let mut queue = VecDeque::<(Edge<V>, u32)>::new();
        let mut curr_dist = HashSet::<V>::new();
        curr_dist.insert(self.v0().clone());
        self.dist_mut().push_front(curr_dist);
        let v = self.v0().clone();
        self.assignment_mut().insert(v, false);

        for edge in self.edg().succ(&self.v0()) {
            queue.push_back((edge, 0));
        }

        loop {
            match queue.pop_front() {
                None => break,
                Some((edge, dist)) => {
                    self.initialize_from_edge(edge, &mut queue, dist);
                }
            }
        }
    }

    /// A helper function to initialize(), which assign and targets `false` and insert
    /// them into the `dist` list. If the target already is know, it is simply skipped
    fn initialize_from_edge(
        &mut self,
        edge: Edge<V>,
        queue: &mut VecDeque<(Edge<V>, u32)>,
        dist: u32,
    ) {
        match edge {
            Edge::Hyper(e) => {
                for target in e.targets {
                    if self.assignment().get(&target).is_some() {
                        continue;
                    }
                    self.assignment_mut().insert(target.clone(), false);
                    self.insert_in_curr_dist(target.clone(), dist);
                    for target_edge in self.edg().succ(&target) {
                        queue.push_front((target_edge, dist));
                    }
                }
            }

            Edge::Negation(e) => {
                if self.assignment().get(&e.target).is_none() {
                    self.assignment_mut().insert(e.target.clone(), false);
                    self.insert_in_curr_dist(e.target.clone(), dist + 1);
                    for target_edge in self.edg().succ(&e.target) {
                        queue.push_front((target_edge, dist + 1))
                    }
                }
            }
        }
    }

    /// Inserts a vertex in the set at the index of curr_dist of dist.
    fn insert_in_curr_dist(&mut self, v: V, dist: u32) {
        match self.dist_mut().get_mut(dist as usize) {
            None => {
                let mut curr_dist = HashSet::<V>::new();
                curr_dist.insert(v);
                self.dist_mut().insert(dist as usize, curr_dist);
            }
            Some(set) => {
                set.insert(v);
            }
        }
    }
}
