use assert_cmd::Command;
use predicates::str::contains;

#[test]
fn unknown_flag_maps_to_cbke_exit_code() {
    Command::cargo_bin("copybook")
        .expect("binary exists")
        .arg("--no-such-flag")
        .assert()
        .failure()
        .code(3)
        .stderr(contains("error:"));
}
