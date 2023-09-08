use crate::parsing::span::Span;
use std::cmp::{max, min};
use std::fmt::Write;

/// A log of errors that occurred during parsing or semantic analysis.
/// Each error entry has a span that indicates its origin in the original input code.
/// Given the original input code, the error log can be converted to nicely presented error messages.
#[derive(Debug, Default)]
pub struct ErrorLog {
    errors: Vec<ErrorEntry>,
}

impl ErrorLog {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn log(&mut self, span: Span, msg: String) {
        self.errors.push(ErrorEntry::new(span, msg));
    }

    pub fn log_entry(&mut self, entry: ErrorEntry) {
        self.errors.push(entry);
    }

    pub fn log_msg(&mut self, msg: String) {
        self.errors.push(ErrorEntry::msg_only(msg));
    }

    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
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
        for entry in &self.errors {
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
#[derive(Debug)]
pub struct ErrorEntry {
    /// The span of the error in the original input code.
    span: Option<Span>,
    /// The error message.
    msg: String,
}

impl ErrorEntry {
    pub fn new(span: Span, msg: String) -> Self {
        ErrorEntry {
            span: Some(span),
            msg,
        }
    }

    pub fn msg_only(msg: String) -> Self {
        ErrorEntry { span: None, msg }
    }
}

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
        let mut log = ErrorLog::new();
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
        let mut log = ErrorLog::new();
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
