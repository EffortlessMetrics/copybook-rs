<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Feature Flags

This document describes the feature flag system in copybook-rs, which allows runtime control over experimental features, enterprise features, performance optimizations, debug capabilities, and testing hooks.

> **See Also**: [Testing Integration Summary](TESTING_INTEGRATION_SUMMARY.md) for comprehensive documentation on how feature flags integrate with all testing methodologies.

## Overview

Feature flags provide a mechanism to:
- Gradually roll out new features
- Enable/disable functionality without code changes
- Test features in production environments
- Control access to experimental or enterprise features

## Using Feature Flags

### Environment Variables

All feature flags can be controlled via environment variables with the prefix `COPYBOOK_FF_`:

```bash
# Enable verbose logging
export COPYBOOK_FF_VERBOSE_LOGGING=1

# Enable diagnostic output
export COPYBOOK_FF_DIAGNOSTIC_OUTPUT=1

# Disable LRU cache (enabled by default)
export COPYBOOK_FF_LRU_CACHE=0
```

### CLI Options

The CLI provides several options for controlling feature flags:

```bash
# Enable specific features
copybook parse --enable-features sign_separate,verbose_logging

# Disable specific features
copybook parse --disable-features lru_cache

# Enable all features in a category
copybook parse --enable-category debug

# Disable all features in a category
copybook parse --disable-category experimental

# List all available feature flags
copybook --list-features
```

### Configuration Files

Feature flags can be configured via TOML or JSON files:

**TOML format:**
```toml
[feature_flags]
enabled = ["sign_separate", "verbose_logging", "diagnostic_output"]
disabled = ["lru_cache"]
```

**JSON format:**
```json
{
  "feature_flags": {
    "enabled": ["sign_separate", "verbose_logging", "diagnostic_output"],
    "disabled": ["lru_cache"]
  }
}
```

Use the configuration file:
```bash
copybook parse --feature-flags-config /path/to/config.toml
```

### Library API

Feature flags can be controlled programmatically:

```rust
use copybook_core::{Feature, FeatureFlags, FeatureFlagsHandle};

// Get global feature flags
let flags = FeatureFlags::global();

// Check if a feature is enabled
if flags.is_enabled(Feature::VerboseLogging) {
    // Use verbose logging
}

// Create custom feature flags
let custom_flags = FeatureFlags::builder()
    .enable(Feature::SignSeparate)
    .disable(Feature::LruCache)
    .build();

// Use a handle for runtime modification
let handle = FeatureFlagsHandle::new();
handle.enable(Feature::VerboseLogging);
```

## Available Feature Flags

### Experimental Features

Experimental features are under active development and may change without notice.

| Flag | Default | Description |
|-------|----------|-------------|
| `sign_separate` | **Enabled** | SIGN SEPARATE clause support (promoted to stable v0.4.3) |
| `renames_r4_r6` | Disabled | Enable RENAMES R4-R6 advanced scenarios |
| `comp_1` | **Enabled** | COMP-1 single precision floating point (promoted to stable v0.4.3) |
| `comp_2` | **Enabled** | COMP-2 double precision floating point (promoted to stable v0.4.3) |

> **Note:** `sign_separate`, `comp_1`, and `comp_2` remain in the "experimental" category for organizational purposes but are **enabled by default since v0.4.3** (promoted to stable). They can still be disabled via `COPYBOOK_FF_SIGN_SEPARATE=0`, `COPYBOOK_FF_COMP_1=0`, or `COPYBOOK_FF_COMP_2=0` for compatibility testing.

### Enterprise Features

Enterprise features provide business-critical functionality for production deployments.

| Flag | Default | Description |
|-------|----------|-------------|
| `audit_system` | Disabled | Enable audit system for compliance tracking |
| `sox_compliance` | Disabled | Enable SOX compliance validation |
| `hipaa_compliance` | Disabled | Enable HIPAA compliance validation |
| `gdpr_compliance` | Disabled | Enable GDPR compliance validation |
| `pci_dss_compliance` | Disabled | Enable PCI DSS compliance validation |
| `security_monitoring` | Disabled | Enable security monitoring integration |

### Performance Features

Performance features control optimization modes and caching strategies.

| Flag | Default | Description |
|-------|----------|-------------|
| `advanced_optimization` | Disabled | Enable advanced optimization mode (SIMD, vectorization) |
| `lru_cache` | Enabled | Enable LRU cache for parsed copybooks |
| `parallel_decode` | Disabled | Enable parallel decoding for large files |
| `zero_copy` | Disabled | Enable zero-copy parsing where possible |

### Debug Features

Debug features provide diagnostic and profiling capabilities.

| Flag | Default | Description |
|-------|----------|-------------|
| `verbose_logging` | Disabled | Enable verbose logging with detailed diagnostics |
| `diagnostic_output` | Disabled | Enable diagnostic output for troubleshooting |
| `profiling` | Disabled | Enable CPU profiling hooks |
| `memory_tracking` | Disabled | Enable memory usage tracking |

### Testing Features

Testing features provide hooks for mutation testing and fuzzing.

| Flag | Default | Description |
|-------|----------|-------------|
| `mutation_testing` | Disabled | Enable mutation testing hooks |
| `fuzzing_integration` | Disabled | Enable fuzzing integration points |
| `coverage_instrumentation` | Disabled | Enable test coverage instrumentation |
| `property_based_testing` | Disabled | Enable property-based testing integration |

## Feature Flag Lifecycle

### Experimental

Features in the experimental stage are:
- Under active development
- May have breaking changes
- Not recommended for production use
- Subject to removal without notice

### Stable

Features in the stable stage are:
- Production-ready
- Backward compatible
- Have comprehensive tests
- Documented and supported

### Deprecated

Features in the deprecated stage are:
- Scheduled for removal
- Still functional but not recommended
- Will be removed in a future version
- Users should migrate to alternatives

## Precedence

Feature flag values are resolved in the following order (highest to lowest precedence):

1. CLI `--disable-features` (highest)
2. CLI `--enable-features`
3. CLI `--disable-category`
4. CLI `--enable-category`
5. Configuration file (disabled list)
6. Configuration file (enabled list)
7. Environment variables
8. Default values (lowest)

## Performance Impact

Feature flags are designed to have minimal performance impact when disabled:

- Flag checks use simple hash lookups
- No dynamic dispatch or runtime reflection
- Zero-cost abstractions for compile-time known flags
- Minimal overhead for environment variable parsing

## Best Practices

### For Development

1. Use feature flags for new features that may need quick rollback
2. Test with both enabled and disabled states
3. Document the purpose and lifecycle stage of each flag
4. Remove flags after features are stable and widely adopted

### For Production

1. Keep experimental features disabled
2. Use configuration files for consistent flag values across deployments
3. Monitor feature flag usage in production
4. Gradually enable features using staged rollouts
5. Have rollback plans for each enabled feature

### For Testing

1. Test both enabled and disabled states for each flag
2. Use CI to validate feature flag combinations
3. Include feature flag tests in your test matrix
4. Test feature flag precedence and overrides

## Examples

### Example 1: Enable Debug Features for Troubleshooting

```bash
# Enable all debug features via category
copybook parse --enable-category debug input.cpy

# Or enable specific debug features
copybook parse --enable-features verbose_logging,diagnostic_output input.cpy
```

### Example 2: Production Configuration

Create a production configuration file:

```toml
# production-flags.toml
[feature_flags]
enabled = ["lru_cache", "advanced_optimization"]
disabled = ["verbose_logging", "profiling", "mutation_testing"]
```

Use it:
```bash
copybook parse --feature-flags-config production-flags.toml input.cpy
```

### Example 3: Gradual Rollout

Stage 1: Enable for testing (features disabled by default)
```bash
export COPYBOOK_FF_RENAMES_R4_R6=1
copybook parse test.cpy
```

Stage 2: Enable for subset of production
```bash
# In production config
[feature_flags]
enabled = ["renames_r4_r6"]
```

Stage 3: Full rollout (remove flag once stable)

### Example 4: Library Integration

```rust
use copybook_core::{Feature, FeatureFlags};

fn process_copybook(text: &str) -> Result<Schema> {
    let flags = FeatureFlags::global();

    if flags.is_enabled(Feature::RenamesR4R6) {
        // Use advanced RENAMES R4-R6 scenarios
        parse_with_advanced_renames(text)
    } else {
        // Use standard parsing (R1-R3 always available)
        parse_copybook(text)
    }
}
```

## Troubleshooting

### Feature Flag Not Working

If a feature flag doesn't seem to be working:

1. Check the flag name is correct (use `--list-features`)
2. Verify environment variable syntax: `COPYBOOK_FF_<FLAG_NAME>=1`
3. Check precedence - CLI flags override environment variables
4. Verify the feature is actually gated by the flag

### Performance Issues

If you experience performance issues:

1. Disable experimental features
2. Disable debug features (especially `profiling` and `memory_tracking`)
3. Enable `lru_cache` if not already enabled
4. Consider enabling `advanced_optimization` for large workloads

### Testing Issues

If tests fail with feature flags:

1. Ensure tests run with both enabled and disabled states
2. Check that feature flag precedence is correct
3. Verify that feature flag initialization happens before use
4. Test with configuration files as well as environment variables

## Related Documentation

- [CLI Reference](CLI_REFERENCE.md) - Complete CLI command reference
- [Library API](https://docs.rs/copybook-core) - Rust library documentation
- [ROADMAP](ROADMAP.md) - Project roadmap and feature status
- [CONTRIBUTING](CONTRIBUTING.md) - Contribution guidelines

## Support

For questions or issues related to feature flags:
- Open an issue on GitHub
- Check existing issues for similar problems
- Review the troubleshooting section above
- Consult the library documentation for API details
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
