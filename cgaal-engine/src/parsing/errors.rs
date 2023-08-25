use std::cmp::{max, min};
use crate::parsing::span::Span;

#[derive(Debug, Default)]
pub struct ErrorLog {
    errors: Vec<ErrorEntry>,
}

impl ErrorLog {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn log(&mut self, entry: ErrorEntry) {
        self.errors.push(entry);
    }

    pub fn log_msg(&mut self, msg: String) {
        self.errors.push(ErrorEntry::msg_only(msg));
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn write_detailed(&self, orig_input: &str, f: &mut dyn std::fmt::Write) -> std::fmt::Result {
        for entry in &self.errors {
            match &entry.span {
                Some(span) => {
                    // Example:
                    // """
                    // 1:13 Error: Unknown identifier 'robot'
                    // | player p1 = robot[x=0, y=1];
                    // |             ^^^^^
                    // """

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
                    writeln!(f, "\x1b[93m{}:{} Error:\x1b[0m {}", line, col, entry.msg)?;

                    // Print the lines with the error, and underline the error span
                    while line_start < span.end && i <= chars.len() {
                        if i == chars.len() || chars[i] == b'\n' {
                            writeln!(f, "| {}", &orig_input[line_start..i])?;
                            let highlight_start = max(line_start, span.begin);
                            let highlight_end = min(i, span.end);
                            write!(f, "| ")?;
                            write!(f, "{}", " ".repeat(highlight_start - line_start))?;
                            writeln!(f, "{}", "^".repeat(highlight_end - highlight_start))?;
                            line += 1;
                            line_start = i + 1;
                        }
                        i += 1;
                    }
                }
                None => {
                    writeln!(f, "\x1b[93m@ Error:\x1b[0m {}", entry.msg)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ErrorEntry {
    span: Option<Span>,
    msg: String,
}

impl ErrorEntry {
    pub fn new(span: Span, msg: String) -> Self {
        ErrorEntry { span: Some(span), msg }
    }

    pub fn msg_only(msg: String) -> Self {
        ErrorEntry { span: None, msg }
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::errors::{ErrorEntry, ErrorLog};
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
        log.log(ErrorEntry::new(Span::new(i, i + 5), "Unknown identifier 'robot'".to_string()));
        let mut out = String::new();
        log.write_detailed(input, &mut out).unwrap();
        assert_eq!(
            out,
            "\x1b[93m3:13 Error:\x1b[0m Unknown identifier 'robot'\n\
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
        log.log(ErrorEntry::new(span, "RHS of '+' must be integer, found bool".to_string()));
        let mut out = String::new();
        log.write_detailed(input, &mut out).unwrap();
        assert_eq!(
            out,
            "\x1b[93m2:11 Error:\x1b[0m RHS of '+' must be integer, found bool\n\
            | label x = 123 +\n\
            |           ^^^^^\n\
            |           true;\n\
            | ^^^^^^^^^^^^^^\n"
        );
    }
}