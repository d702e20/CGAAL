use crate::parsing::span::Span;
use std::cell::RefCell;
use std::cmp::{max, min};
use std::error::Error;
use std::fmt::{Display, Formatter, Write};

/// A log of errors that occurred during parsing or semantic analysis.
/// Each [ErrorLogEntry] has a span that indicates its origin in the original input code.
/// Given the original input code, the error log can be converted to nicely presented error messages.
#[derive(Debug, Default, Clone)]
pub struct ErrorLog {
    errors: RefCell<Vec<ErrorLogEntry>>,
}

impl ErrorLog {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn log(&self, span: Span, msg: String) -> SeeErrorLog {
        self.errors.borrow_mut().push(ErrorLogEntry::new(span, msg));
        SeeErrorLog
    }

    pub fn log_entry(&self, entry: ErrorLogEntry) -> SeeErrorLog {
        self.errors.borrow_mut().push(entry);
        SeeErrorLog
    }

    pub fn log_msg(&self, msg: String) -> SeeErrorLog {
        self.errors.borrow_mut().push(ErrorLogEntry::msg_only(msg));
        SeeErrorLog
    }

    pub fn log_err(&self, err: SpannedError) -> SeeErrorLog {
        self.errors
            .borrow_mut()
            .push(ErrorLogEntry::new(err.span, err.msg));
        SeeErrorLog
    }

    pub fn len(&self) -> usize {
        self.errors.borrow().len()
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.borrow().is_empty()
    }

    pub fn is_empty(&self) -> bool {
        self.errors.borrow().is_empty()
    }

    /// Converts the error log to a nicely formatted string using the original input code.
    /// Example:
    /// ```md
    /// 3:13 Error: Unknown identifier 'robot'
    /// | player p1 = robot[x=0, y=1];
    /// |             ^^^^^
    /// ```
    /// where 3:13 indicates line 3, column 13 in the original input code.
    pub fn to_string(&self, orig_input: &str) -> String {
        let mut out = String::new();
        for entry in self.errors.borrow().iter() {
            match &entry.span {
                Some(span) => {
                    // Find the line and column of the error
                    let mut i = 0;
                    let mut col = 1;
                    let mut line = 1;
                    let mut line_start = 0;
                    let chars = orig_input.bytes().collect::<Vec<u8>>();
                    while i < span.begin {
                        if chars[i] == b'\n' {
                            line += 1;
                            line_start = i + 1;
                            col = 1;
                        } else {
                            col += 1;
                        }
                        i += 1;
                    }
                    writeln!(out, "\x1b[31m{}:{} Error:\x1b[0m {}", line, col, entry.msg).unwrap();

                    // Print the lines with the error, and underline the error span
                    while line_start < span.end && i <= chars.len() {
                        if i == chars.len() || chars[i] == b'\n' {
                            writeln!(out, "| {}", &orig_input[line_start..i]).unwrap();
                            let highlight_start = max(line_start, span.begin);
                            let highlight_end = min(i, span.end);
                            write!(out, "| ").unwrap();
                            write!(out, "{}", " ".repeat(highlight_start - line_start)).unwrap();
                            writeln!(out, "{}", "^".repeat(highlight_end - highlight_start))
                                .unwrap();
                            line += 1;
                            line_start = i + 1;
                        }
                        i += 1;
                    }
                }
                None => {
                    writeln!(out, "\x1b[31m@ Error:\x1b[0m {}", entry.msg).unwrap();
                }
            }
        }
        out
    }
}

/// A single error entry in the [ErrorLog].
#[derive(Debug, Clone)]
pub struct ErrorLogEntry {
    /// The span of the error in the original input code.
    span: Option<Span>,
    /// The error message.
    msg: String,
}

impl ErrorLogEntry {
    pub fn new(span: Span, msg: String) -> Self {
        ErrorLogEntry {
            span: Some(span),
            msg,
        }
    }

    pub fn msg_only(msg: String) -> Self {
        ErrorLogEntry { span: None, msg }
    }
}

/// An error with a span in the original input code.
/// Must be logged in an [ErrorLog] for prettier display.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SpannedError {
    pub span: Span,
    pub msg: String,
}

impl SpannedError {
    pub fn new(span: Span, msg: String) -> Self {
        SpannedError { span, msg }
    }
}

impl Display for SpannedError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl Error for SpannedError {}

/// A dummy error for when errors occur and they can be found in the passed ErrorLog.
/// Also required to make clippy stop complaining about Result<_, ()>.
#[derive(Copy, Clone, Eq, PartialEq, Default, Debug)]
pub struct SeeErrorLog;

impl Display for SeeErrorLog {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "See content of ErrorLog for errors")
    }
}

impl Error for SeeErrorLog {}

#[cfg(test)]
mod tests {
    use crate::parsing::errors::ErrorLog;
    use crate::parsing::span::Span;

    #[test]
    fn error_fmt_001() {
        // Does errors spanning one line print as expected
        let input = "\
            \n// Player declarations\
            \nplayer p1 = robot[x=0, y=1];\
            \n\
        ";
        let log = ErrorLog::new();
        let i = input.find("robot").unwrap();
        log.log(
            Span::new(i, i + 5),
            "Unknown identifier 'robot'".to_string(),
        );
        let out = log.to_string(input);
        assert_eq!(
            out,
            "\x1b[31m3:13 Error:\x1b[0m Unknown identifier 'robot'\n\
            | player p1 = robot[x=0, y=1];\n\
            |             ^^^^^\n"
        );
    }

    #[test]
    fn error_fmt_002() {
        // Does errors spanning two lines print as expected
        let input = "\
          \nlabel x = 123 +\
          \n          true;\
        ";
        let log = ErrorLog::new();
        let span = Span::new(input.find("123").unwrap(), input.find(";").unwrap());
        log.log(span, "RHS of '+' must be integer, found bool".to_string());
        let out = log.to_string(input);
        assert_eq!(
            out,
            "\x1b[31m2:11 Error:\x1b[0m RHS of '+' must be integer, found bool\n\
            | label x = 123 +\n\
            |           ^^^^^\n\
            |           true;\n\
            | ^^^^^^^^^^^^^^\n"
        );
    }
}
