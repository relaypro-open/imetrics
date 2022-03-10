# OpenMetrics Project

**Goal:** convert imetrics to surface data using the [OpenMetrics spec](https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md).

#### Tasks:

- [x] Copy `varz` module to a new module called `openmetrics` to use as a starting point
- [ ] Modify `openmetrics` module to surface data compliant with the standard:
  - [x] Include `# EOF` as required by the standard
  - [x] Update `Content-Type` header
  - [x] Include `TYPE` metric descriptor and support for main metric types
    - [x] `counter`
    - [x] `gauge`
    - [x] `unknown`
  - [x] Update handling of mapped data to convert mapped counters and stats maps to tagged metric sets
  - [ ] Metric type for stats
- [x] Convert from `mod_esi` to `cowboy` to surface data at `/metrics` URL path (required by the standard)
- [x] Add `openmetrics_strict_compat` application environment variable (causes only forwards-incompatible metrics to exist on the legacy endpoint and not exist on the OpenMetrics endpoint)
- [ ] Extend `imetrics` to support additional OpenMetrics features
  - [ ] Exemplars
  - [ ] `UNIT` metric descriptor
  - [ ] `HELP` metric descriptor
  - [ ] Tags on a per-metric level
    - [x] `counter`
    - [x] `gauge` / `multigauge`
    - [ ] `stats`
    - [ ] `histogram`s
  - [ ] New metric types?
    - [ ] `summary` (as a way to surface precomputed percentiles from histograms)
    - [ ] `histogram` and `gaugehistogram`
    - [ ] `stateset`
    - [ ] `info`
  - [ ] More?
- [ ] (Optionally) Extend `imetrics` to surface OpenMetrics data via [Protobuf](https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md#protobuf-format) instead of UTF-8 text
