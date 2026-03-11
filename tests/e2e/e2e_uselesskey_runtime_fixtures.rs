// SPDX-License-Identifier: AGPL-3.0-or-later
//! Scanner-safe crypto fixture smoke tests.
//!
//! These tests pin the repo's `uselesskey` integration so future suites can
//! generate PEM/certificate-shaped inputs at runtime instead of committing
//! secret-looking blobs under `fixtures/`.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fs;
use uselesskey::negative::CorruptPem;
use uselesskey::{ChainSpec, Factory, RsaFactoryExt, RsaSpec, Seed, X509FactoryExt, X509Spec};

fn deterministic_factory(scope: &str) -> Factory {
    let seed = Seed::from_env_value(scope).expect("test scope should derive a deterministic seed");
    Factory::deterministic(seed)
}

#[test]
fn deterministic_crypto_fixtures_are_stable_across_call_order() {
    let scope = format!("{module}::deterministic_order", module = module_path!());

    let first = deterministic_factory(&scope);
    let first_rsa = first.rsa("ci-security", RsaSpec::rs256());
    let first_chain = first.x509_chain("local-tls", ChainSpec::new("localhost"));

    let second = deterministic_factory(&scope);
    let second_chain = second.x509_chain("local-tls", ChainSpec::new("localhost"));
    let second_rsa = second.rsa("ci-security", RsaSpec::rs256());

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

    let cert = fx.x509_self_signed("service-cert", X509Spec::self_signed("localhost"));
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

    let bad_cert_pem = cert.corrupt_cert_pem(CorruptPem::BadHeader);
    assert!(
        bad_cert_pem.contains("CORRUPTED"),
        "negative fixture should produce a visibly broken PEM header",
    );
    assert_ne!(
        bad_cert_pem,
        cert.cert_pem(),
        "negative fixture should differ from the valid certificate",
    );

    let rsa = fx.rsa("service-rsa", RsaSpec::rs256());
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
