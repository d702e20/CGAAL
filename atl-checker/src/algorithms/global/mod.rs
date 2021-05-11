use crate::edg::{Edge, ExtendedDependencyGraph, HyperEdge, NegationEdge, Vertex};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;

// Based on the global algorithm described in "Extended Dependency Graphs and Efficient Distributed Fixed-Point Computation" by A.E. Dalsgaard et al., 2017
struct GlobalAlgorithm<
    G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static,
    V: Vertex + Send + Sync + 'static,
> {
    edg: G,
    v0: V,
    assignment: HashMap<V, bool>,
    dist: VecDeque<HashSet<V>>,
    curr_dist: u32,
}

impl<
        G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static,
        V: Vertex + Send + Sync + 'static,
    > GlobalAlgorithm<G, V>
{
    pub fn new(edg: G, v0: V) -> Self {
        Self {
            edg,
            /// The root vertex, which we discover the graph from
            v0,
            /// The assignment for all vertices, we use this to keep track  of the current assignment
            assignment: HashMap::<V, bool>::new(),
            /// The distance of a vertex is represented as the index number of which set the vertex is located in
            dist: VecDeque::<HashSet<V>>::new(),
            /// The current distance is used to keep track of the current depth when discovering all
            /// the vertices in order to assign the vertex the correct distance
            curr_dist: 0,
        }
    }

    /// Firing the global algorithm, which simply reapplying the F_i function explained in the paper
    /// until the assignment does not change anymore. Lastly the assignment of the root, v0, is returned
    pub fn run(&mut self) -> bool {
        self.initialize();

        let mut components = self.get_components();

        while !components.is_empty() {
            let component = components.pop_front().unwrap();
            while self.F(component.clone(), self.assignment.clone()) {}
        }

        *self.assignment.get(&self.v0).unwrap()
    }

    /// Initialize the dist and assignments by traversing through all edges from root
    fn initialize(&mut self) {
        let mut curr_dist = HashSet::<V>::new();
        curr_dist.insert(self.v0.clone());
        self.dist.push_back(curr_dist);

        for edge in self.edg.succ(&self.v0) {
            self.initialize_from_edge(edge);
        }
    }

    /// A helper function to initialize(), which recursively discover all vertices by
    /// following all edges.
    fn initialize_from_edge(&mut self, edge: Edge<V>) {
        match edge {
            Edge::Hyper(e) => {
                for target in e.targets {
                    if let Some(_) = self.assignment.get(&target) {
                        break;
                    }
                    self.assignment.insert(target.clone(), false);
                    self.insert_in_curr_dist(target.clone());
                    for target_edge in self.edg.succ(&target) {
                        self.initialize_from_edge(target_edge)
                    }
                }
            }

            Edge::Negation(e) => {
                if let None = self.assignment.get(&e.target) {
                    self.assignment.insert(e.target.clone(), false);
                    self.curr_dist += 1;
                    self.insert_in_curr_dist(e.target.clone());
                    for target_edge in self.edg.succ(&e.target) {
                        self.initialize_from_edge(target_edge)
                    }
                    self.curr_dist -= 1;
                }
            }
        }
    }

    /// Inserts a vertex in the set at the index of curr_dist of dist.
    fn insert_in_curr_dist(&mut self, v: V) {
        match self.dist.get_mut(self.curr_dist as usize) {
            None => {
                let mut curr_dist = HashSet::<V>::new();
                curr_dist.insert(v);
                self.dist.push_back(curr_dist);
            }
            Some(set) => {
                set.insert(v);
            }
        }
    }

    /// The resemblance of the function described in the paper, but in order to
    /// know which component we are worker with and the assignments of the component
    /// before, these are both given as arguments. The function it self returns a boolean
    /// which is true if the assignments are changed.
    fn F(&mut self, component: HashSet<V>, ass_from_earlier: HashMap<V, bool>) -> bool {
        let mut changed_flag = false;

        for vertex in component {
            for edge in self.edg.succ(&vertex) {
                match edge {
                    Edge::Hyper(e) => changed_flag = self.process_hyper(e),
                    Edge::Negation(e) => {
                        changed_flag = self.process_negation(e, ass_from_earlier.clone())
                    }
                }
            }
        }
        changed_flag
    }

    /// Rising the value of the source vertex assignment if all targets are true or empty. If the
    /// source vertex already is true we simply return. The return value is based on if a changed
    /// have been made.
    fn process_hyper(&mut self, edge: HyperEdge<V>) -> bool {
        if *self.assignment.get(edge.source.clone()).unwrap() {
            false
        } else {
            let mut final_ass = true;
            for target in edge.targets {
                if !self.assignment.get(&target).unwrap() {
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
    fn process_negation(
        &mut self,
        edge: NegationEdge<V>,
        ass_from_earlier: HashMap<V, bool>,
    ) -> bool {
        if *self.assignment.get(edge.source.clone()).unwrap() {
            false
        } else {
            let mut final_ass = true;

            if *ass_from_earlier.get(&edge.target).unwrap() {
                final_ass = false;
            }

            self.update_assignment(edge.source, final_ass)
        }
    }

    /// Updating an assignment to the new_ass value, if the new_ass are
    /// equal to the old assignment, we simply return. The return value
    /// is based on whether the assignment of the given vertex is changed.
    fn update_assignment(&mut self, v: V, new_ass: bool) -> bool {
        self.assignment
            .get_mut(&v)
            .and_then(|mut ass| {
                if new_ass == *ass {
                    Some(false)
                } else {
                    *ass = new_ass.clone();
                    Some(true)
                }
            })
            .unwrap()
    }

    /// From the dist list the components are identified. It holds
    /// that all components that have a lower dist are included in those
    /// with higher. So we simply go through all sets in dist and whether
    /// we have engage in a new set we just save the current one as a component.
    fn get_components(&mut self) -> VecDeque<HashSet<V>> {
        let mut components = VecDeque::<HashSet<V>>::new();
        let mut component = HashSet::<V>::new();

        while !self.dist.is_empty() {
            for v in self.dist.pop_back().unwrap() {
                component.insert(v);
            }
            components.push_back(component.clone());
        }
        components
    }
}
