# copybook-cli-determinism

Retired 0.5 package for the determinism CLI implementation.

## Overview

The implementation is now owned directly by the binary-only `copybook-cli`
package. The published 0.5 artifact remains available; no 0.6 release or new
primary dependency is planned for this package.

## Usage

```text
copybook determinism decode --format fixed --codepage cp037 schema.cpy data.bin
```

The command continues to support decode, encode, and round-trip validation
through the CLI reference.

## License

AGPL-3.0-or-later
