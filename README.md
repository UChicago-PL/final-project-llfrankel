# final-project-llfrankel

## /docs

- `proposal.txt`: the original proposal for Part 1 of the project.

- `json-specs.md`: Specification of the JSON format exported by the Swift
  compiler and consumed by this tool.
  - Describes the input schema (IR program andbackend configuration)
  - Describes the output schema (annotated signals + swatches).

## /src

- `IR.hs`: Type definitions
  - for the WEFT intermediate representation (Program, Bundle, Strand, Spindle, Expr, etc.)
  - backend configuration types (BackendSpec, Dimension, PrimitiveSpec). - All JSON parsing (Aeson FromJSON/ToJSON instances) lives here.

- `DepGraph.hs`: Builds a bundle dependency graph from a Program
  - walking expressions and collecting bundle references.
  - Provides topological sort and transitive dependency lookup.

- `Annotate.hs`: The core analysis pass.
  - Propagates three annotations through the expression graph:
    - domain (which coordinate dimensions a signal uses)
    - hardware (what devices it requires)
    - statefulness (whether it uses feedback/cache).
  - Takes a Program + backend specs as input, outputs annotated signals.

- `Partition.hs`: Groups annotated bundles into swatches (compilation units) by backend.
  - Routes bundles based on hardware ownership, duplicates pure dependencies, and computes cross-domain buffer requirements.

## /app

- `Main.hs` — CLI entry point.
  - Reads JSON from stdin or a file argument
  - runs all three analysis passes,
  - writes annotated results as JSON to stdout.
