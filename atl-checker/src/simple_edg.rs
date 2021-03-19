/// Defines a hardcoded EDG.
/// This allows us to easily create EDGs for testing and other purposes. It works well
/// together with the `edg_assert` macro.
///
/// # Example A
/// A simple EDG with 4 vertices A, B, C, D:
/// ```
/// simple_edg![
///     A => -> {B, C} -> {D};
///     B => ;
///     C => -> {B} .> D;
///     D => -> {};
/// ];
///
/// edg_assert!(A, TRUE);
/// edg_assert!(B, FALSE);
/// ```
/// We list the edges of each vertex, separating each vertex using a semicolon.
/// The syntax `-> {V, ..., W}` defines a hyper-edge with targets `V`,...,`W`, and the syntax
/// `.> P` defines a negation edge with target `P`. Declare multiple edges by writing them
/// after each other (as seen for vertex `A` and `C` above), but note that hyper-edges must
/// precede negation edges. Additionally, all targets must also be declared in the first line
/// of the macro, and the vertex declarations and the edge declarations are separated with `::`.
///
/// # Example B
/// By default the created EDG consists of a struct named `SimpleEDG` and enum named
/// `SimpleVertex`. This can be changed by adding `[EDG_NAME, VERTEX_NAME]` in the start of the
/// macro's arguments, where `EDG_NAME` is the desired name of the example, and `VERTEX_NAME` is
/// the desired name of the vertex enum. This allows us to have multiple hardcoded EDGs in the
/// same scope.
/// ```
/// simple_edg![
///     [MyEDG1, MyVertex1]
///     A => -> {B} .> {D};
///     B => -> {D, C};
///     C => ;
///     D => -> {C};
/// ];
///
/// edg_assert!([MyEDG1, MyVertex1] A, TRUE);
/// edg_assert!([MyEDG1, MyVertex1] B, FALSE);
/// ```
/// The `edg_assert` macro allows the same naming functionality.
macro_rules! simple_edg {
    // Standard use
    [ $( $v:ident => $( -> { $( $t:ident ),* } )* $( .> $n:ident )* );*; ] => {
        simple_edg![[SimpleEDG, SimpleVertex] $( $v => $( -> { $( $t ),* } )* $( .> $n )* );*; ]
    };
    // Defines struct and enum
    [ [ $edg_name:ident, $vertex_name:ident ] $( $v:ident => $( -> { $( $t:ident ),* } )* $( .> $n:ident )* );*; ] => {
        use std::collections::HashSet;
        #[allow(unused_imports)]
        use crate::common::{Edges, HyperEdge, NegationEdge};
        use crate::edg::{ExtendedDependencyGraph, Vertex};
        #[derive(Hash, Copy, Clone, Eq, PartialEq, Debug)]
        struct $edg_name;
        #[derive(Hash, Copy, Clone, Eq, PartialEq, Debug)]
        enum $vertex_name {
            $( $v ),*
        }
        impl std::fmt::Display for $vertex_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:?}", self)
            }
        }
        impl Vertex for $vertex_name {}
        impl ExtendedDependencyGraph<$vertex_name> for $edg_name {
            fn succ(
                &self,
                vertex: &$vertex_name,
            ) -> HashSet<Edges<$vertex_name>> {
                simple_edg![@match vertex_name=$vertex_name, vertex $( $v => $( -> { $( $t ),* } )* $( .> $n )* );*;]
            }
        }
    };
    // Constructs match expression
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

/// Defines an assertion which test the assignment of a vertex in an EDG using the
/// distributed certain zero algorithm.
/// This macro is intended to be used in conjunction with the `simple_edg` macro.
///
/// # Example
/// Simple usage:
/// ```
/// simple_edg![
///     A => .> B;
///     B => -> {};
/// ];
///
/// edg_assert!(A, FALSE);
/// edg_assert!(B, TRUE);
/// ```
/// Note that TRUE/FALSE must be capitalized.
///
/// # Worker count
/// You can set the worker count by supplying a third argument to the marco. The default
/// number of workers are 3.
/// ```
/// edg_assert!(A, FALSE, 5);
/// ```
///
/// # Custom names
/// By default the macro assumes that the EDG and vertices are defined by the struct `SimpleEDG`
/// and the enum `SimpleVertex` as is also default in the `simple_edg` macro.
/// This can be changed by adding `[EDG_NAME, VERTEX_NAME]` in the start of the
/// macro's arguments, where `EDG_NAME` is the desired name of the example, and `VERTEX_NAME` is
/// the desired name of the vertex enum. This allows us to have multiple hardcoded EDGs in the
/// same scope.
/// ```
/// simple_edg![
///     [MyEDG1, MyVertex1]
///     A, B, C, D ::
///     A => -> {B} .> {D};
///     B => -> {D, C};
///     C => ;
///     D => -> {C};
/// ];
///
/// edg_assert!([MyEDG1, MyVertex1] A, TRUE);
/// edg_assert!([MyEDG1, MyVertex1] B, FALSE);
/// ```
macro_rules! edg_assert {
    // Standard use, no names or worker count given
    ( $v:ident, $assign:ident ) => {
        edg_assert!([SimpleEDG, SimpleVertex] $v, $assign, 3)
    };
    // With worker count given
    ( $v:ident, $assign:ident, $wc:expr ) => {
        edg_assert!([SimpleEDG, SimpleVertex] $v, $assign, $wc)
    };
    // With custom names given
    ( [$edg_name:ident, $vertex_name:ident] $v:ident, $assign:ident ) => {
        edg_assert!([$edg_name, $vertex_name] $v, $assign, 3)
    };
    // With custom names and worker count
    ( [$edg_name:ident, $vertex_name:ident] $v:ident, $assign:ident, $wc:expr ) => {
        assert_eq!(
            distributed_certain_zero($edg_name, $vertex_name::$v, $wc),
            crate::common::VertexAssignment::$assign,
            "Vertex {}",
            stringify!($v)
        );
    };
}
