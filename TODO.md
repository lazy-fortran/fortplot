# TODO

- **Status:** ✅ FIXED - ASCII pie legend now shows clean unified legend with correct markers matching pie slice patterns.
- **Resolution:**
  ✅ **ELIMINATED duplicate legends**: Fixed annotation filtering to prevent pie chart labels from being rendered twice  
  ✅ **FIXED marker mapping**: Legend markers now use distinct characters per slice (`-`, `+`, `*`, `=`) for clear differentiation
  ✅ **ENSURED single legend positioned correctly**: Legend appears in consistent position outside plot area
- **Verification:** `make example ARGS="pie_chart_demo"` now shows clean output with single legend and matching markers
