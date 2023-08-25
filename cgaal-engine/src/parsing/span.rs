use std::cmp::{max, min};
use std::fmt::{Display, Formatter};
use std::ops::Range;

/// A `Span` describes the position of a slice of text in the original program.
/// Usually used to describe what text an AST node was created from.
#[derive(Eq, PartialEq, Debug, Copy, Clone, Default)]
pub struct Span {
    pub begin: usize,
    pub end: usize,
}

impl Span {
    pub fn new(begin: usize, end: usize) -> Self {
        Span { begin, end }
    }

    /// Returns and equivalent range
    pub fn to_range(&self) -> Range<usize> {
        self.begin..self.end
    }

    /// Merge two spans into a new span that contains the original spans and everything in between
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl LineColumn {
    pub fn new(line: usize, column: usize) -> Self {
        LineColumn { line, column }
    }

    pub fn from_pos(pos: usize, orig_input: &str) -> Self {
        let mut line = 1;
        let mut column = 1;
        for (i, c) in orig_input.bytes().enumerate() {
            if i == pos {
                return LineColumn { line, column };
            }
            if c == b'\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        LineColumn { line, column }
    }
}

impl Display for LineColumn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}