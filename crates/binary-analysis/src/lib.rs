//! Binary data forensic analysis library.
//!
//! Provides entropy calculation, magic-byte signature detection,
//! and byte distribution analysis for binary forensics.

mod distribution;
mod entropy;
mod signature;

pub use distribution::{byte_distribution, ByteDistribution};
pub use entropy::{entropy, DataClass, EntropyReport};
pub use signature::{detect_signature, Confidence, SignatureMatch};

/// Combined analysis report.
pub struct AnalysisReport {
    pub size: usize,
    pub entropy: EntropyReport,
    pub signatures: Vec<SignatureMatch>,
    pub distribution: ByteDistribution,
}

/// Run all analyses at once.
pub fn analyze(data: &[u8]) -> AnalysisReport {
    AnalysisReport {
        size: data.len(),
        entropy: entropy(data),
        signatures: detect_signature(data),
        distribution: byte_distribution(data),
    }
}
