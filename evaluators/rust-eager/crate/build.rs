// Invoke napi-build's link setup ONLY when building the native N-API addon (feature `napi`).
// For the wasm artifact and the native CLI/tests the feature is off and this is a no-op, so
// those builds are byte-for-byte untouched. Cargo can't expose `[features]` to a build script
// via `cfg!`, so we read the `CARGO_FEATURE_NAPI` env var it sets when the feature is active.
fn main() {
    if std::env::var_os("CARGO_FEATURE_NAPI").is_some() {
        napi_build::setup();
    }
}
