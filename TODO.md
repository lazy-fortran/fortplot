Current status:

> commit and push. However, the PDF in my MacOS Preview shows garbled characters, and the PNG still shows the replacement "omega" literals etc

⏺ I need to investigate the issues you're seeing. Let me first check the current PNG output to see if it's actually showing the replacement literals:

⏺ Read(example/fortran/unicode_demo/unicode_demo.png)
  ⎿  Read image (67.6KB)

⏺ You're absolutely right! The PNG is still showing the ASCII replacement strings like "omega", "alpha", "beta", etc. instead of the actual Unicode characters. This means the LaTeX
   to Unicode conversion isn't working properly. Let me check the actual Unicode demo source to see what text is being processed:

⏺ Read(example/fortran/unicode_demo/unicode_demo.f90)
  ⎿  Read 50 lines (ctrl+r to expand)

⏺ I can see the issue! The demo is using LaTeX commands like \psi, \omega, \alpha, etc. These should be converted to Unicode characters, but it looks like the LaTeX to Unicode
  conversion is not working properly. Let me check what's happening with the LaTeX parser:

⏺ Bash(make test ARGS="--target test_latex_to_unicode_mapping" 2>&1 | grep -A 10 -B 10 "alpha\|beta\|gamma\|omega\|psi")
  ⎿  Error: Error

⏺ Let me trace through the text processing to see where the LaTeX to Unicode conversion is happening:

⏺ Search(pattern: "process_latex_in_text", path: "src")
  ⎿  Found 4 files (ctrl+r to expand)

⏺ The LaTeX to Unicode conversion is happening in process_latex_in_text, but notice that the PNG backend (fortplot_png.f90) is not using this function. Let me check the PNG backend
   text rendering:

⏺ Search(pattern: "process_latex_in_text", path: "src/fortplot_png.f90")

