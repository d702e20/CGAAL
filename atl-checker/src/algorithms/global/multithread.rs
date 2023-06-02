use std::cmp::max;
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::thread;
use tracing::{span, trace, Level};

use crate::algorithms::global::com::GMessage::{Terminate, Updates};
use crate::algorithms::global::com::{
    GBroker, GBrokerManager, GChannelBroker, GChannelBrokerManager, GMessage,
};
use crate::algorithms::global::GlobalAlgorithm;
use crate::edg::{Edge, ExtendedDependencyGraph, Vertex};

pub struct GWorker<B: GBroker<V>, G: ExtendedDependencyGraph<V>, V: Vertex> {
    edg: G,
    assignment: HashMap<V, bool>,
    broker: B,
    iteration: usize
}

impl<B: GBroker<V>, G: ExtendedDependencyGraph<V>, V: Vertex> GlobalAlgorithm<G, V>
    for GWorker<B, G, V>
{
    fn edg(&self) -> &G {
        &self.edg
    }
    fn edg_mut(&mut self) -> &mut G {
        &mut self.edg
    }
    fn v0(&self) -> &V {
        panic!("A GWorker doesnt have a v0 value")
    }
    fn v0_mut(&mut self) -> &mut V {
        panic!("A GWorker doesnt have a v0 value")
    }
    fn assignment(&self) -> &HashMap<V, bool> {
        &self.assignment
    }
    fn assignment_mut(&mut self) -> &mut HashMap<V, bool> {
        &mut self.assignment
    }
    fn components(&self) -> &VecDeque<HashSet<V>> {
        panic!("A GWorker doesnt have a dist vecdeque")
    }
    fn components_mut(&mut self) -> &mut VecDeque<HashSet<V>> {
        panic!("A GWorker doesnt have a dist vecdeque")
    }
    fn run(&mut self) -> bool {
        self.run()
    }
}

impl<B: GBroker<V>, G: ExtendedDependencyGraph<V>, V: Vertex> GWorker<B, G, V> {
    pub fn new(edg: G, assignment: HashMap<V, bool>, broker: B) -> Self {
        trace!("new global worker");
        Self {
            edg,
            assignment,
            broker,
            iteration: 1
        }
    }

    pub fn run(&mut self) -> bool {
        let span = span!(Level::DEBUG, "worker run");
        let _enter = span.enter();
        trace!("worker start");
        emit_count!("worker start");

        let mut curr_task = None;
        loop {
            match self.broker.receive() {
                Ok(msg) => {
                    if let Some(Updates {
                        updates: assignment,
                                    iteration
                    }) = msg
                    {
                        trace!("Worker received updates");
                        self.assignment = assignment;
                        self.iteration = iteration;
                    } else if let Some(Terminate) = msg {
                        trace!("Worker received terminate");
                        return true;
                    }
                }
                Err(err) => panic!("{}", err),
            }

            match curr_task {
                Some((task,iteration)) if iteration <= self.iteration => {
                        for edge in self.edg.succ(&task) {
                            match edge {
                                Edge::Hyper(e) => {
                                    self.process_hyper(e);
                                }
                                Edge::Negation(e) => {
                                    self.process_negation(e);
                                }
                            }
                            trace!("Worker finished task");
                        }
                        self.broker
                            .send_result(task.clone(), *self.assignment.get(&task).unwrap());
                        curr_task = None;
                },
                None => {
                    if let Ok(Some((task, iteration ))) = self.broker.get_task() {
                        curr_task = Some((task, iteration));
                        trace!("Worker received task");
                    }
                }
                _ => {}
            }
        }
    }
}

pub struct MultithreadedGlobalAlgorithm<G: ExtendedDependencyGraph<V>, V: Vertex> {
    edg: G,
    worker_count: u64,
    v0: V,
    assignment: HashMap<V, bool>,
    dist: VecDeque<HashSet<V>>,
}

impl<
        G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static,
        V: Vertex + Send + Sync + 'static,
    > GlobalAlgorithm<G, V> for MultithreadedGlobalAlgorithm<G, V>
{
    fn edg(&self) -> &G {
        &self.edg
    }
    fn edg_mut(&mut self) -> &mut G {
        &mut self.edg
    }
    fn v0(&self) -> &V {
        &self.v0
    }
    fn v0_mut(&mut self) -> &mut V {
        &mut self.v0
    }
    fn assignment(&self) -> &HashMap<V, bool> {
        &self.assignment
    }
    fn assignment_mut(&mut self) -> &mut HashMap<V, bool> {
        &mut self.assignment
    }
    fn components(&self) -> &VecDeque<HashSet<V>> {
        &self.dist
    }
    fn components_mut(&mut self) -> &mut VecDeque<HashSet<V>> {
        &mut self.dist
    }

    fn run(&mut self) -> bool {
        self.initialize();

        let (mut brokers, manager) = GChannelBroker::new(self.worker_count);
        for _ in 0..self.worker_count {
            let mut worker = GWorker::new(
                self.edg.clone(),
                self.assignment.clone(),
                brokers.pop().unwrap(),
            );
            thread::spawn(move || {
                worker.run();
            });
        }

        let mut iteration: usize = 0;
        let components = self.dist.clone();
        components.iter().rev().for_each(|component| {
            let mut changed_flag = false;
            iteration = iteration + 1;
            manager.send_updates(self.assignment().clone(), iteration);
            for vertex in component {
                manager.queue_task(vertex.clone(), iteration);

            }

            let mut tasks_left: usize = component.len();

            loop {
                if tasks_left == 0 {
                    if !changed_flag {
                        break;
                    }
                    iteration = iteration + 1;
                    manager.send_updates(self.assignment().clone(), iteration);
                    tasks_left = component.len();
                    for vertex in component {
                        manager.queue_task(vertex.clone(), iteration);
                    }
                    changed_flag = false;
                }

                match manager.receive() {
                    Ok(msg) => {
                        if let Some(GMessage::Result { task, value }) = msg {
                            changed_flag =
                                max(self.update_assignment(task.clone(), value), changed_flag);
                            tasks_left = tasks_left - 1;
                        }
                    }
                    Err(err) => {
                        panic!("{}", err);
                    }
                }
            }
        });

        manager.terminate();
        *self.assignment().get(&self.v0).unwrap()
    }
}

impl<
        G: ExtendedDependencyGraph<V> + Send + Sync + Clone + Debug + 'static,
        V: Vertex + Send + Sync + 'static,
    > MultithreadedGlobalAlgorithm<G, V>
{
    pub fn new(edg: G, worker_count: u64, v0: V) -> Self {
        let mut assignment = HashMap::new();
        assignment.insert(v0.clone(), false);
        let dist = VecDeque::new();
        Self {
            edg,
            worker_count,
            v0,
            assignment,
            dist,
        }
    }
    pub fn run(&mut self) -> bool {
        GlobalAlgorithm::<G, V>::run(self)
    }
}

#[cfg(test)]
mod test {
    use test_env_log::test;
    #[allow(unused_macros)]
    macro_rules! edg_multi_assert {
        ( $v:ident, $assign:expr ) => {
                edg_multi_assert!([SimpleEDG, SimpleVertex] $v, $assign)
        };
        // With custom names
        ( [$edg_name:ident, $vertex_name:ident] $v:ident, $assign:expr) => {
            assert_eq!(
                crate::algorithms::global::multithread::MultithreadedGlobalAlgorithm::new($edg_name, 8,$vertex_name::$v).run(),
                $assign,
                "Vertex {}",
                stringify!($v)
            );
        };
    }

    #[test]
    fn test_multi_global_algorithm_empty_hyper_edge() {
        simple_edg![
            A => -> {};
        ];
        edg_multi_assert!(A, true);
    }

    #[test]
    fn test_multi_global_algorithm_no_successors() {
        simple_edg![
            A => ;
        ];
        edg_multi_assert!(A, false);
    }

    #[test]
    fn test_multi_global_algorithm_general_01() {
        simple_edg![
            A => -> {B, C} -> {D};
            B => ;
            C => .> D;
            D => -> {};
        ];
        edg_multi_assert!(A, true);
        edg_multi_assert!(B, false);
        edg_multi_assert!(C, false);
        edg_multi_assert!(D, true);
    }

    #[test]
    fn test_multi_global_algorithm_general_02() {
        simple_edg![
            A => -> {B, C};
            B => .> E;
            C => -> {};
            D => -> {} -> {C};
            E => .> D;
        ];
        edg_multi_assert!(A, true);
        edg_multi_assert!(B, true);
        edg_multi_assert!(C, true);
        edg_multi_assert!(D, true);
        edg_multi_assert!(E, false);
    }

    #[test]
    fn test_multi_global_algorithm_general_03() {
        simple_edg![
            A => -> {B} -> {E};
            B => -> {C};
            C => -> {F} -> {H};
            D => -> {E} -> {C};
            E => -> {D, F};
            F => -> {};
            G => .> A;
            H => -> {I};
            I => ;
        ];
        edg_multi_assert!(A, true);
        edg_multi_assert!(B, true);
        edg_multi_assert!(C, true);
        edg_multi_assert!(D, true);
        edg_multi_assert!(E, true);
        edg_multi_assert!(F, true);
        edg_multi_assert!(G, false);
        edg_multi_assert!(H, false);
        edg_multi_assert!(I, false);
    }

    #[test]
    fn test_multi_global_algorithm_general_04() {
        simple_edg![
            A => -> {B} -> {C};
            B => -> {D};
            C => ;
            D => -> {};
        ];
        edg_multi_assert!(A, true);
        edg_multi_assert!(B, true);
        edg_multi_assert!(C, false);
        edg_multi_assert!(D, true);
    }

    #[test]
    fn test_multi_global_algorithm_general_05() {
        simple_edg![
            A => -> {B};
            B => -> {C};
            C => -> {B};
        ];
        edg_multi_assert!(A, false);
        edg_multi_assert!(B, false);
        edg_multi_assert!(C, false);
    }

    #[test]
    fn test_multi_global_algorithm_general_06() {
        simple_edg![
            A => -> {B} -> {C};
            B => ;
            C => ;
        ];
        edg_multi_assert!(A, false);
        edg_multi_assert!(B, false);
        edg_multi_assert!(C, false);
    }

    #[test]
    fn test_multi_global_algorithm_general_07() {
        simple_edg![
            A => -> {B};
            B => -> {A, C};
            C => -> {D};
            D => -> {};
        ];
        edg_multi_assert!(A, false);
        edg_multi_assert!(B, false);
        edg_multi_assert!(C, true);
        edg_multi_assert!(D, true);
    }

    #[test]
    fn test_multi_global_algorithm_general_08() {
        simple_edg![
            A => -> {B, C};
            B => -> {C} -> {D};
            C => -> {B};
            D => -> {C} -> {};
        ];
        edg_multi_assert!(A, true);
        edg_multi_assert!(B, true);
        edg_multi_assert!(C, true);
        edg_multi_assert!(D, true);
    }

    #[test]
    fn test_multi_global_algorithm_negation_01() {
        simple_edg![
            A => .> B;
            B => -> {};
        ];
        edg_multi_assert!(A, false);
        edg_multi_assert!(B, true);
    }

    #[test]
    fn test_multi_global_algorithm_negation_02() {
        simple_edg![
            A => .> B;
            B => -> {C};
            C => -> {B} .> D;
            D => -> {E};
            E => -> {D};
        ];
        edg_multi_assert!(A, false);
        edg_multi_assert!(B, true);
        edg_multi_assert!(C, true);
        edg_multi_assert!(D, false);
        edg_multi_assert!(E, false);
    }

    #[test]
    fn test_multi_global_algorithm_negation_03() {
        simple_edg![
            A => .> B .> C;
            B => .> D;
            C => -> {D};
            D => ;
        ];
        edg_multi_assert!(A, true);
        edg_multi_assert!(B, true);
        edg_multi_assert!(C, false);
        edg_multi_assert!(D, false);
    }

    #[test]
    fn test_multi_global_algorithm_negation_04() {
        simple_edg![
            A => .> B;
            B => -> {B};
        ];
        edg_multi_assert!(A, true);
        edg_multi_assert!(B, false);
    }

    #[test]
    fn test_multi_global_algorithm_negation_05() {
        simple_edg![
            A => .> B;
            B => .> C;
            C => .> D;
            D => .> E;
            E => .> F;
            F => -> {F};
        ];
        edg_multi_assert!(A, true);
        edg_multi_assert!(B, false);
        edg_multi_assert!(C, true);
        edg_multi_assert!(D, false);
        edg_multi_assert!(E, true);
        edg_multi_assert!(F, false);
    }

    #[test]
    fn test_multi_global_algorithm_negation_to_undecided_01() {
        // A case where we might explore and find a negation edges to something that is
        // currently assigned undecided
        simple_edg![
            A => .> B .> E;
            B => -> {C};
            C => -> {D};
            D => .> E;
            E => -> {F};
            F => -> {G};
            G => -> {H};
            H => -> {I};
            I => -> {J};
            J => -> {K};
            K => -> {};
        ];
        edg_multi_assert!(A, true);
        edg_multi_assert!(B, false);
        edg_multi_assert!(C, false);
        edg_multi_assert!(D, false);
        edg_multi_assert!(E, true);
        edg_multi_assert!(F, true);
        edg_multi_assert!(G, true);
    }
}
