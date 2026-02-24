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

Literal number (always a float64, even for integers).

### param

    { "type": "param", "name": <string> }

Named parameter reference. Inside a spindle body, refers to a formal
parameter. Also used as the indexExpr inside "index" nodes to represent
static field access.

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

Built-in function call. Two categories:

Pure math builtins (no special domain behavior):
sin, cos, tan, abs, floor, ceil, sqrt, pow, min, max,
lerp, clamp, step, smoothstep, fract, mod, osc

Primitive builtins (have domain/hardware/state implications):
camera, texture, text, mouse, microphone, sample, cache

The distinction matters for annotation: primitive builtins are listed
in the backend's primitiveSpecs; everything else just merges its
arguments' annotations.

### extract

    { "type": "extract", "call": <Expr>, "index": <int> }

Extract a single return value from a multi-return spindle call.
`call` is typically a "call" expr. `index` is the zero-based return
position.

### remap

    { "type": "remap", "base": <Expr>, "substitutions": { <string>: <Expr>, ... } }

Coordinate remapping. Evaluates `base` with certain coordinate fieldsw
replaced. Substitution keys are prefixed with "me." (e.g. "me.x",
"me.t"). For annotation purposes: the remapped dimension is removed
from the base domain, and the substitution expression's domain is added.

### cacheRead

    { "type": "cacheRead", "cacheId": <string>, "tapIndex": <int> }

Read from a cache history buffer. Used internally to break cycles in
feedback effects. Always stateful.

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
  - "free" means seekable: can sample at any value (spatial coordinates).
  - "bound" means time-constrained: only the current value is available
    (time, resolution, sample rate).

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
