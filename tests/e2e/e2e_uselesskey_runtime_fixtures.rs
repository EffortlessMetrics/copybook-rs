// SPDX-License-Identifier: AGPL-3.0-or-later
//! Scanner-safe crypto fixture smoke tests.
//!
//! These tests pin the repo's deterministic runtime fixture generation so
//! future suites can keep producing PEM/certificate-shaped inputs at runtime
//! instead of committing secret-looking blobs under `fixtures/`.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fs;
use std::io::Write;

use base64::Engine as _;
use base64::engine::general_purpose::STANDARD;
use sha2::{Digest, Sha256};
use tempfile::NamedTempFile;

struct FixtureFactory<'a> {
    scope: &'a str,
}

struct RsaFixture {
    private_key_der: Vec<u8>,
    private_key_pem: String,
    public_key_pem: String,
}

struct CertChainFixture {
    leaf_cert_pem: String,
    root_cert_pem: String,
}

struct SelfSignedCertFixture {
    cert_pem: String,
    private_key_pem: String,
}

fn deterministic_factory(scope: &str) -> FixtureFactory<'_> {
    FixtureFactory { scope }
}

impl FixtureFactory<'_> {
    fn rsa(&self, label: &str) -> RsaFixture {
        let private_key_der = deterministic_bytes(self.scope, label, "private-key", 96);
        let public_key_der = deterministic_bytes(self.scope, label, "public-key", 96);
        RsaFixture {
            private_key_pem: to_pem("PRIVATE KEY", &private_key_der),
            public_key_pem: to_pem("PUBLIC KEY", &public_key_der),
            private_key_der,
        }
    }

    fn x509_chain(&self, label: &str) -> CertChainFixture {
        CertChainFixture {
            leaf_cert_pem: to_pem(
                "CERTIFICATE",
                &deterministic_bytes(self.scope, label, "leaf-cert", 96),
            ),
            root_cert_pem: to_pem(
                "CERTIFICATE",
                &deterministic_bytes(self.scope, label, "root-cert", 96),
            ),
        }
    }

    fn x509_self_signed(&self, label: &str) -> SelfSignedCertFixture {
        let private_key_der = deterministic_bytes(self.scope, label, "self-signed-key", 96);
        let cert_der = deterministic_bytes(self.scope, label, "self-signed-cert", 96);
        SelfSignedCertFixture {
            cert_pem: to_pem("CERTIFICATE", &cert_der),
            private_key_pem: to_pem("PRIVATE KEY", &private_key_der),
        }
    }
}

impl RsaFixture {
    fn private_key_pkcs8_pem(&self) -> &str {
        &self.private_key_pem
    }

    fn public_key_spki_pem(&self) -> &str {
        &self.public_key_pem
    }

    fn private_key_pkcs8_der(&self) -> &[u8] {
        &self.private_key_der
    }

    fn private_key_pkcs8_der_truncated(&self, len: usize) -> Vec<u8> {
        self.private_key_der.iter().copied().take(len).collect()
    }
}

impl CertChainFixture {
    fn leaf_cert_pem(&self) -> &str {
        &self.leaf_cert_pem
    }

    fn root_cert_pem(&self) -> &str {
        &self.root_cert_pem
    }
}

impl SelfSignedCertFixture {
    fn cert_pem(&self) -> &str {
        &self.cert_pem
    }

    fn private_key_pkcs8_pem(&self) -> &str {
        &self.private_key_pem
    }

    fn write_cert_pem(&self) -> std::io::Result<NamedTempFile> {
        write_tempfile(&self.cert_pem)
    }

    fn write_private_key_pem(&self) -> std::io::Result<NamedTempFile> {
        write_tempfile(&self.private_key_pem)
    }

    fn corrupt_cert_pem(&self) -> String {
        self.cert_pem
            .replace("BEGIN CERTIFICATE", "BEGIN CORRUPTED CERTIFICATE")
    }
}

fn deterministic_bytes(scope: &str, label: &str, kind: &str, len: usize) -> Vec<u8> {
    let mut out = Vec::with_capacity(len);
    let mut counter = 0_u64;

    while out.len() < len {
        let mut hasher = Sha256::new();
        hasher.update(scope.as_bytes());
        hasher.update([0]);
        hasher.update(label.as_bytes());
        hasher.update([0]);
        hasher.update(kind.as_bytes());
        hasher.update([0]);
        hasher.update(counter.to_le_bytes());
        out.extend_from_slice(&hasher.finalize());
        counter += 1;
    }

    out.truncate(len);
    out
}

fn to_pem(tag: &str, bytes: &[u8]) -> String {
    let body = STANDARD.encode(bytes);
    let wrapped = body
        .as_bytes()
        .chunks(64)
        .map(|chunk| std::str::from_utf8(chunk).expect("base64 should remain UTF-8"))
        .collect::<Vec<_>>()
        .join("\n");
    format!("-----BEGIN {tag}-----\n{wrapped}\n-----END {tag}-----\n")
}

fn write_tempfile(contents: &str) -> std::io::Result<NamedTempFile> {
    let mut file = NamedTempFile::new()?;
    file.write_all(contents.as_bytes())?;
    Ok(file)
}

#[test]
fn deterministic_crypto_fixtures_are_stable_across_call_order() {
    let scope = format!("{module}::deterministic_order", module = module_path!());

    let first = deterministic_factory(&scope);
    let first_rsa = first.rsa("ci-security");
    let first_chain = first.x509_chain("local-tls");

    let second = deterministic_factory(&scope);
    let second_chain = second.x509_chain("local-tls");
    let second_rsa = second.rsa("ci-security");

    assert_eq!(
        first_rsa.private_key_pkcs8_pem(),
        second_rsa.private_key_pkcs8_pem(),
        "same seed + label should produce identical RSA private keys",
    );
    assert_eq!(
        first_rsa.public_key_spki_pem(),
        second_rsa.public_key_spki_pem(),
        "same seed + label should produce identical RSA public keys",
    );
    assert_eq!(
        first_chain.leaf_cert_pem(),
        second_chain.leaf_cert_pem(),
        "certificate chain generation should be order-independent",
    );
    assert_eq!(
        first_chain.root_cert_pem(),
        second_chain.root_cert_pem(),
        "root CA output should remain stable for the same seed",
    );
    assert!(
        first_chain.leaf_cert_pem().contains("BEGIN CERTIFICATE"),
        "leaf certificate should be PEM encoded",
    );
}

#[test]
fn runtime_crypto_fixtures_support_tempfiles_and_negative_paths() {
    let scope = format!("{module}::negative_paths", module = module_path!());
    let fx = deterministic_factory(&scope);

    let cert = fx.x509_self_signed("service-cert");
    let cert_file = cert.write_cert_pem().expect("write cert pem");
    let key_file = cert.write_private_key_pem().expect("write private key pem");

    let cert_body = fs::read_to_string(cert_file.path()).expect("read cert tempfile");
    let key_body = fs::read_to_string(key_file.path()).expect("read key tempfile");

    assert_eq!(
        cert_body,
        cert.cert_pem(),
        "tempfile sink should write the generated certificate bytes",
    );
    assert_eq!(
        key_body,
        cert.private_key_pkcs8_pem(),
        "tempfile sink should write the generated private key bytes",
    );

    let bad_cert_pem = cert.corrupt_cert_pem();
    assert!(
        bad_cert_pem.contains("CORRUPTED"),
        "negative fixture should produce a visibly broken PEM header",
    );
    assert_ne!(
        bad_cert_pem,
        cert.cert_pem(),
        "negative fixture should differ from the valid certificate",
    );

    let rsa = fx.rsa("service-rsa");
    let truncated = rsa.private_key_pkcs8_der_truncated(32);
    assert_eq!(
        truncated.len(),
        32,
        "truncated DER should honor requested length"
    );
    assert_ne!(
        truncated.as_slice(),
        rsa.private_key_pkcs8_der(),
        "negative DER fixture should differ from the original key material",
    );
}
