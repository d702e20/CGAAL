/// Macro for emitting eprintlns for Counts statistics
macro_rules! emit_count {
    ($($msg: tt)*) => {
        #[cfg(feature = "use-counts")]
        eprintln!(concat!("[stats] ", $($msg)*));
    };
}