The top-level object has two keys:

    { "program": <Program>, "backends": [<BackendSpec>, ...] }

## Program

    {
      "bundles":       { <string>: <Bundle>, ... },
      "spindles":      { <string>: <Spindle>, ... },
      "order":         [<OrderEntry>, ...],
      "resources":     [<string>, ...],
      "textResources": [<string>, ...]
    }

- `bundles` — map from name to Bundle. Keys are bundle names like
  "display", "img", "tone".
- `spindles` — map from name to Spindle. Keys are spindle names.
- `order` — suggested evaluation order (informational, not authoritative).
- `resources` — texture resource identifiers (filenames/paths).
- `textResources` — text resource identifiers.

## Bundle

    { "name": <string>, "strands": [<Strand>, ...] }

- `name` — same as the key in the bundles map.
- `strands` — ordered list of output channels.

## Strand

    { "name": <string>, "index": <int>, "expr": <Expr> }

- `name` — field name (e.g. "r", "g", "b", "val").
- `index` — zero-based position in the strand array.
- `expr` — the expression computing this output.

## Spindle

    {
      "name":    <string>,
      "params":  [<string>, ...],
      "locals":  [<Bundle>, ...],
      "returns": [<Expr>, ...]
    }

- `name` — same as the key in the spindles map.
- `params` — formal parameter names.
- `locals` — intermediate bundles local to this spindle (may reference
  params and each other; used for cache/feedback patterns).
- `returns` — list of return expressions. A spindle with N returns has
  width N. Most spindles return 1 value.

## OrderEntry

    { "bundle": <string> }
    { "bundle": <string>, "strands": [<string>, ...] }

- `bundle` — bundle name.
- `strands` — optional list of strand names (omitted means all strands).

## Expr

Every expression is an object with a `"type"` field that determines which
other fields are present. There are 10 expression types.

### num

    { "type": "num", "value": <number> }

### param

    { "type": "param", "name": <string> }

### index

    { "type": "index", "bundle": <string>, "indexExpr": <Expr> }

Bundle field access. The bundle "me" is special — it provides coordinate
fields (me.x, me.y, me.t, etc.) whose meaning depends on the backend.

When indexExpr is `{"type": "param", "name": "r"}`, this is a static
field access like `img.r`. When indexExpr is a `num`, it's a positional
access like `img.0`.

### binary

    { "type": "binary", "op": <string>, "left": <Expr>, "right": <Expr> }

Binary operation. Operators: "+", "-", "\*", "/", "^", "%", "==", "!=",
"<", ">", "<=", ">=", "&&", "||".

### unary

    { "type": "unary", "op": <string>, "operand": <Expr> }

Unary operation. Operators: "-", "!".

### call

    { "type": "call", "spindle": <string>, "args": [<Expr>, ...] }

Spindle invocation. `spindle` is the spindle name. `args` are the
argument expressions, positionally matching the spindle's params.

### builtin

    { "type": "builtin", "name": <string>, "args": [<Expr>, ...] }

### extract

    { "type": "extract", "call": <Expr>, "index": <int> }

### remap

    { "type": "remap", "base": <Expr>, "substitutions": { <string>: <Expr>, ... } }

Coordinate remapping. Evaluates `base` with certain coordinate fieldsw
replaced. Substitution keys are prefixed with "me." (e.g. "me.x",
"me.t"). For annotation purposes: the remapped dimension is removed
from the base domain, and the substitution expression's domain is added.

### cacheRead

    { "type": "cacheRead", "cacheId": <string>, "tapIndex": <int> }

## BackendSpec

    {
      "identifier":      <string>,
      "hardwareOwned":   [<string>, ...],
      "outputSinks":     [<string>, ...],
      "coordinateSpecs": { <string>: <DimensionSpec>, ... },
      "primitiveSpecs":  { <string>: <PrimitiveSpec>, ... }
    }

- `identifier` — backend name. Currently "visual" or "audio".
- `hardwareOwned` — hardware tags this backend claims. Used during
  partitioning: a bundle requiring any of these gets routed here.
  Values: "camera", "gpu", "microphone", "speaker", or custom strings.
- `outputSinks` — bundle names that are output sinks for this backend
  (e.g. "display" for visual, "play"/"scope" for audio).
- `coordinateSpecs` — defines what `me.<field>` means in this domain.
  Keys are field names.
- `primitiveSpecs` — defines domain/hardware/state behavior of special
  builtins. Keys are builtin names.

## DimensionSpec

    { "name": <string>, "access": <string> }

- `name` — dimension name (e.g. "x", "y", "t", "i", "sampleRate").
- `access` — either "free" or "bound".
  - "free" means seekable: can sample at any value
  - "bound" means time-constrained: only the current value is available

## PrimitiveSpec

    {
      "name":             <string>,
      "outputDomain":     [<DimensionSpec>, ...],
      "hardwareRequired": [<string>, ...],
      "addsState":        <bool>
    }

- `name` — same as the key in primitiveSpecs.
- `outputDomain` — dimensions this primitive produces. If non-empty,
  replaces (not merges with) the argument domains. If empty and
  addsState is true, inherits domain from arguments (this is the
  cache pattern).
- `hardwareRequired` — hardware tags this primitive needs. Unioned
  with argument hardware during annotation.
- `addsState` — whether this primitive introduces statefulness.
  Currently only true for "cache".

# WEFT Analysis Output — JSON Specification

The top-level object:

    {
      "signals":      [<SignalAnnotation>, ...],
      "bundles":      [<BundleAnnotation>, ...],
      "swatches":     [<Swatch>, ...],
      "dependencies": { <string>: [<string>, ...], ... }
    }

## SignalAnnotation

    {
      "name":     <string>,
      "domain":   [<DimensionSpec>, ...],
      "hardware": [<string>, ...],
      "stateful": <bool>,
      "pure":     <bool>
    }

One entry per strand in the program, sorted by name. Each signal is
identified by `"bundle.strand"` (e.g. `"img.r"`, `"trail.val"`).

Signals with numeric strand indices also appear (e.g. `"img.0"`,
`"brightness.0"`) — these come from positional references like
`camera(...)` where the lowered IR uses index 0, 1, 2 rather than
named fields. Both the named and numeric forms are present when
both referencing styles exist.

- `name` — `"bundleName.strandName"` or `"bundleName.index"`.
- `domain` — list of dimensions this signal varies over. Each entry
  has `name` and `access` ("free" or "bound"). Sorted by name.
  Examples:
  - `[{name:"x", access:"free"}, {name:"y", access:"free"}]` — spatial
  - `[{name:"t", access:"bound"}]` — time-varying only
  - `[]` — constant
- `hardware` — hardware tags required by this signal, sorted.
  Empty means no hardware dependency. Values: "camera", "gpu",
  "microphone", "speaker", or custom strings.
- `stateful` — true if the signal depends on `cache` or `cacheRead`.
- `pure` — true if `hardware` is empty AND `stateful` is false.
  Redundant but convenient for diffchecking.

### Annotation propagation rules

Domain and hardware propagate bottom-up through expressions:

| Expression type           | Domain                                                   | Hardware                  |
| ------------------------- | -------------------------------------------------------- | ------------------------- |
| `num`                     | `[]`                                                     | `[]`                      |
| `param` (coordinate name) | `[that dimension]`                                       | `[]`                      |
| `me.field`                | `[coordinateSpecs[field]]`                               | `[]`                      |
| `bundle.strand`           | recursive from referenced strand                         | recursive                 |
| `binary`, `unary`         | union of operand domains                                 | union of operand hardware |
| math builtin (sin, etc.)  | union of arg domains                                     | union of arg hardware     |
| primitive builtin         | see below                                                | args hw UNION spec hw     |
| `call`                    | union of arg domains                                     | union of arg hardware     |
| `extract`                 | same as inner call                                       | same as inner call        |
| `remap`                   | base domain minus remapped dim, plus substitution domain | union                     |
| `cacheRead`               | `[]`                                                     | `[]`, stateful=true       |

Primitive builtin domain rules:

- If `outputDomain` is non-empty: **replaces** arg domains (e.g. `camera` produces `[x,y,t]`)
- If `outputDomain` is empty AND `addsState` is true: **inherits** arg domains (the `cache` pattern)
- If `outputDomain` is empty AND `addsState` is false: **merges** arg domains

Domain merging: dimensions are unioned by name. If the same dimension
appears with both "free" and "bound", "bound" wins (more restrictive).

Stateful propagation: `stateful` is true if any sub-expression is
stateful OR the builtin has `addsState: true`.

## BundleAnnotation

    {
      "name":     <string>,
      "backend":  <string | null>,
      "hardware": [<string>, ...],
      "pure":     <bool>,
      "isSink":   <bool>
    }

One entry per bundle in the program, sorted by name.

- `name` — bundle name.
- `backend` — the backend that owns this bundle, or `null` if pure.
  Determined by:
  1. Find the union of hardware across all strands in this bundle.
  2. Find the backend whose `hardwareOwned` intersects that hardware.
  3. If no intersection, check if the bundle is an output sink
     (e.g. "display" -> "visual", "play" -> "audio").
  4. If neither, `null` — the bundle is pure and gets duplicated
     into whichever swatches need it.
- `hardware` — union of hardware tags across all strands, sorted.
- `pure` — true if no hardware and no statefulness in any strand.
- `isSink` — true if this bundle name is an `outputSink` for any
  backend (e.g. "display", "play", "scope").

## Swatch

    {
      "backend":       <string>,
      "bundles":       [<string>, ...],
      "inputBuffers":  [<string>, ...],
      "outputBuffers": [<string>, ...],
      "isSink":        <bool>
    }

Swatches are compilation units — connected subgraphs of bundles
assigned to the same backend. Ordered topologically (dependencies
before dependents). All arrays are sorted alphabetically.

- `backend` — backend identifier ("visual" or "audio").
- `bundles` — bundle names in this swatch. Includes:
  - Bundles routed here by hardware ownership
  - Pure bundles pulled in as transitive dependencies
  - Output sink bundles for this backend
- `inputBuffers` — bundle names read from other swatches
  (cross-domain inputs). These are buffers that must be filled
  by another swatch before this one executes.
- `outputBuffers` — bundle names that other swatches read from
  this one. These bundles must be written to shared buffers.
- `isSink` — true if this swatch contains an output sink bundle
  for its backend.

### Partitioning algorithm

1. For each bundle, get hardware from annotations.
2. Route to the backend whose `hardwareOwned` intersects.
3. Pure bundles (no hardware) are unrouted initially.
4. For each backend with routed bundles or a sink:
   - Pull in all pure transitive dependencies.
   - A pure bundle may appear in multiple swatches (duplicated).
5. Compute cross-domain buffers: if swatch A references a bundle
   in swatch B, that bundle becomes an outputBuffer of B and an
   inputBuffer of A.

## Dependencies

    { "bundleName": ["dep1", "dep2", ...], ... }

Bundle-level dependency graph. Keys are all bundle names in the
program. Values are sorted lists of bundles that the key directly
depends on (self-references excluded).

Built by collecting all bundle references from each strand's
expression tree (excluding `me`).
