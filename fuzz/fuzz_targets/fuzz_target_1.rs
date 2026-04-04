#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| -> libfuzzer_sys::Corpus {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = macro_railroad::to_diagram(s);
        libfuzzer_sys::Corpus::Keep
    } else {
        libfuzzer_sys::Corpus::Reject
    }
});
