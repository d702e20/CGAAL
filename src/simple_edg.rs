// simple_edg![
//     A, B, C, D ::
//     A => -> {B, C} -> {D};
//     B => ;
//     C => .> D;
//     D => -> {};
// ];

macro_rules! simple_edg {
    [ $( $v:ident ),+ :: $( $rest:tt )* ] => {
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        struct SimpleEDG;
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        enum SimpleVertex {
            $( $v ),*
        }
        impl Display for SimpleVertex {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:?}", self)
            }
        }
        impl Vertex for SimpleVertex {}
        impl ExtendedDependencyGraph<SimpleVertex> for SimpleEDG {
            fn succ(
                &self,
                vertex: &SimpleVertex,
            ) -> HashSet<Edges<SimpleVertex>> {
                simple_edg![@match vertex $( $rest )*]
            }
        }
    };
    [ @match $vertex:ident $( $v:ident => $( -> { $( $t:ident ),* } )* $( .> $n:ident )* );*; ] => {
        match $vertex {
            $(SimpleVertex::$v => {
                #[allow(unused_mut)]
                let mut successors = HashSet::new();
                $(successors.insert(Edges::HYPER(HyperEdge {
                    source: SimpleVertex::$v,
                    targets: vec![$(SimpleVertex::$t),*],
                }));)*
                $(successors.insert(Edges::NEGATION(NegationEdge {
                    source: SimpleVertex::$v,
                    target: SimpleVertex::$n,
                }));)*
                successors
            }),*
        }
    };
}
