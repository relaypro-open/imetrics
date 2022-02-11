# OpenMetrics Project

**Goal:** convert imetrics to surface data using the [OpenMetrics spec](https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md).

#### Tasks:

- [x] Copy `varz` module to a new module called `openmetrics` to use as a starting point
- [ ] Modify `openmetrics` module to surface data compliant with the standard:
  - [x] Include `# EOF` as required by the standard
  - [x] Update `Content-Type` header
  - [ ] Include `TYPE` metric descriptor
    - [x] `counter`
    - [x] `gauge`
    - [x] `histogram`
    - [x] `unknown`
  - [ ] Update handling of mapped data to convert mapped counters and stats maps to tagged metric sets
- [x] Convert from `mod_esi` to `cowboy` to surface data at `/metrics` URL path (required by the standard)
- [ ] Extend `imetrics` to support additional OpenMetrics features
  - [ ] `HELP` metric descriptor
  - [ ] `UNIT` metric descriptor
  - [ ] Tags on a per-metric level
  - [ ] Exemplars
  - [ ] New metric types?
    - [ ] `gaugehistogram` (or realistically, providing a way to specify histogram type in `imetrics_hist` module)
    - [ ] `summary` (as a way to surface precomputed percentiles?)
    - [ ] `stateset`
    - [ ] `info`
  - [ ] More?
- [ ] (Optionally) Extend `imetrics` to surface OpenMetrics data via [Protobuf](https://github.com/OpenObservability/OpenMetrics/blob/main/specification/OpenMetrics.md#protobuf-format) instead of UTF-8 text
