title: Implementation Plan (Issue #19)
---

# Implementation Plan (Issue #19): Public API Docstrings

This page records the original high-level work plan for Issue #19.

## Goal

Add consistent FORD docstrings to the public procedures in
`src/core/fortplot.f90` so the generated docs are usable without reading
implementation files.

## Checklist

- Follow the existing FORD style used in the codebase.
- Document all arguments, optional defaults, and valid option strings.
- Add small examples for the core entry points (plot, scatter, figure, savefig).
- Keep documentation close to procedures and avoid duplicating full API
  references in standalone markdown.
