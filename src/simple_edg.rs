// simple_edg![
//     A, B, C, D ::
//     A => -> {B, C} -> {D};
//     B => ;
//     C => .> D;
//     D => -> {};
// ];

macro_rules! simple_edg {
    [ $( $v:ident ),+ :: $( $rest:tt )* ] => {
        simple_edg![[SimpleEDG, SimpleVertex] $( $v ),+ :: $( $rest )* ]
    };
    [ [ $edg_name:ident, $vertex_name:ident ] $( $v:ident ),+ :: $( $rest:tt )* ] => {
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        struct $edg_name;
        #[derive(Hash, Clone, Eq, PartialEq, Debug)]
        enum $vertex_name {
            $( $v ),*
        }
        impl Display for $vertex_name {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:?}", self)
            }
        }
        impl Vertex for $vertex_name {}
        impl ExtendedDependencyGraph<$vertex_name> for $edg_name {
            fn succ(
                &self,
                vertex: &$vertex_name,
            ) -> HashSet<Edges<$vertex_name>> {
                simple_edg![@match vertex_name=$vertex_name, vertex $( $rest )*]
            }
        }
    };
    [ @match vertex_name=$vertex_name:ident, $vertex:ident $( $v:ident => $( -> { $( $t:ident ),* } )* $( .> $n:ident )* );*; ] => {
        match $vertex {
            $($vertex_name::$v => {
                #[allow(unused_mut)]
                let mut successors = HashSet::new();
                $(successors.insert(Edges::HYPER(HyperEdge {
                    source: $vertex_name::$v,
                    targets: vec![$($vertex_name::$t),*],
                }));)*
                $(successors.insert(Edges::NEGATION(NegationEdge {
                    source: $vertex_name::$v,
                    target: $vertex_name::$n,
                }));)*
                successors
            }),*
        }
    };
}

macro_rules! edg_assert {
    ( $v:ident, $assign:ident ) => {
        edg_assert!([SimpleEDG, SimpleVertex] $v, $assign, 1)
    };
    ( $v:ident, $assign:ident, $wc:expr ) => {
        edg_assert!([SimpleEDG, SimpleVertex] $v, $assign, $wc)
    };
    ( [$edg_name:ident, $vertex_name:ident] $v:ident, $assign:ident ) => {
        edg_assert!([$edg_name, $vertex_name] $v, $assign, 1)
    };
    ( [$edg_name:ident, $vertex_name:ident] $v:ident, $assign:ident, $wc:expr ) => {
        assert_eq!(
            distributed_certain_zero($edg_name, $vertex_name::$v, $wc),
            crate::common::VertexAssignment::$assign,
            "Vertex {}",
            stringify!($v)
        );
    };
}

macro_rules! edg_test {
    { $test_name:ident, [ $( $edg:tt )* ], $( $v:ident => $assign:ident),* } => {
        #[test]
        fn $test_name() {
            simple_edg![$( $edg )* ];

            $( edg_assert!($v, $assign); )*
        }
    };
}
