use std::cmp::{max, min};
use std::fmt::{Display, Formatter};
use std::ops::{Add, Range};

/// A [Span] describes the position of a slice of text in the original program.
/// Usually used to describe what text a token or an AST node was created from.
#[derive(Eq, PartialEq, Debug, Copy, Clone, Default)]
pub struct Span {
    pub begin: usize,
    pub end: usize,
}

impl Span {
    pub fn new(begin: usize, end: usize) -> Self {
        Span { begin, end }
    }

    /// Returns an equivalent range
    pub fn to_range(&self) -> Range<usize> {
        self.begin..self.end
    }

    /// Merge two spans into a new span that contains the original spans and everything in between.
    /// The + operator can also be used for this.
    pub fn merge(&self, other: Span) -> Span {
        Span {
            begin: min(self.begin, other.begin),
            end: max(self.end, other.end),
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({},{})", self.begin, self.end)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.to_range()
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Span::new(range.start, range.end)
    }
}

impl From<(usize, usize)> for Span {
    fn from((begin, end): (usize, usize)) -> Self {
        Span::new(begin, end)
    }
}

impl Add for Span {
    type Output = Span;

    fn add(self, rhs: Self) -> Self::Output {
        self.merge(rhs)
    }
}
