use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use crate::algorithms::global::GlobalAlgorithm;
use crate::algorithms::global::multithread::MultithreadedGlobalAlgorithm;
use crate::edg::{Edge, ExtendedDependencyGraph, Vertex};

// Based on the global algorithm described in "Extended Dependency Graphs and Efficient Distributed Fixed-Point Computation" by A.E. Dalsgaard et al., 2017
pub struct SingleThreadedGlobalAlgorithm<G: ExtendedDependencyGraph<V>, V: Vertex> {
    edg: G,
    /// The root vertex, which we discover the graph from
    v0: V,
    /// The assignment for all vertices, we use this to keep track  of the current assignment
    assignment: HashMap<V, bool>,
    /// The distance of a vertex is represented as the index number of which set the vertex is located in
    dist: VecDeque<HashSet<V>>,
}
impl<G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static, V: Vertex + Send + Sync + 'static> SingleThreadedGlobalAlgorithm<G,V>{
    pub fn run(mut self)->bool{
        GlobalAlgorithm::<G,V>::run(&mut self)
    }

    pub fn new(edg: G,v0: V) -> Self {
        Self {
            edg,
            v0,
            assignment: HashMap::<V, bool>::new(),
            dist: VecDeque::<HashSet<V>>::new(),
        }
    }

    /// Initialize the dist and assignments by traversing through all edges from root
    fn initialize(&mut self) {
        let mut queue = VecDeque::<(Edge<V>, u32)>::new();
        let mut curr_dist = HashSet::<V>::new();
        curr_dist.insert(self.v0.clone());
        self.dist.push_front(curr_dist);

        self.assignment.insert(self.v0.clone(), false);

        for edge in self.edg.succ(&self.v0) {
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
                    if self.assignment.get(&target).is_some() {
                        continue;
                    }
                    self.assignment.insert(target.clone(), false);
                    self.insert_in_curr_dist(target.clone(), dist);
                    for target_edge in self.edg.succ(&target) {
                        queue.push_front((target_edge, dist));
                    }
                }
            }

            Edge::Negation(e) => {
                if self.assignment.get(&e.target).is_none() {
                    self.assignment.insert(e.target.clone(), false);
                    self.insert_in_curr_dist(e.target.clone(), dist + 1);
                    for target_edge in self.edg.succ(&e.target) {
                        queue.push_front((target_edge, dist + 1))
                    }
                }
            }
        }
    }

    /// Inserts a vertex in the set at the index of curr_dist of dist.
    fn insert_in_curr_dist(&mut self, v: V, dist: u32) {
        match self.dist.get_mut(dist as usize) {
            None => {
                let mut curr_dist = HashSet::<V>::new();
                curr_dist.insert(v);
                self.dist.insert(dist as usize, curr_dist);
            }
            Some(set) => {
                set.insert(v);
            }
        }
    }
}


impl<G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static, V: Vertex + Send + Sync + 'static> GlobalAlgorithm<G,V> for SingleThreadedGlobalAlgorithm<G,V> {
    fn edg(&self) -> &G {
        &self.edg
    }
    fn edg_mut(&mut self) -> &mut G {
        &mut self.edg

    }
    fn v_zero(&self) -> &V {
        &self.v0
    }
    fn assignment(&self) -> &HashMap<V, bool> {
        &self.assignment
    }
    fn assignment_mut(&mut self) -> &mut HashMap<V, bool> {
        &mut self.assignment
    }
    /// Firing the global algorithm, which simply reapplies the F_i function explained in the paper
    /// until the assignment does not change anymore. Lastly the assignment of the root, v0, is returned
    fn run(&mut self) -> bool {
        self.initialize();

        let components = self.dist.clone();
        components
            .iter()
            .rev()
            .for_each(|mut component| while self.f(&mut component) {});


        *self.assignment().get(&self.v0).unwrap()
    }
}
