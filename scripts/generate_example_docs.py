#!/usr/bin/env python3
"""
Generate FORD documentation pages for fortplotlib examples.
Extracts example descriptions from source files and creates markdown pages
with embedded outputs (PNG images, ASCII output, and PDF links).
"""

import os
import re
import glob
from pathlib import Path

def extract_example_info(fortran_file):
    """Extract description and other metadata from Fortran source."""
    with open(fortran_file, 'r') as f:
        content = f.read()
    
    # Extract program name
    prog_match = re.search(r'program\s+(\w+)', content)
    program_name = prog_match.group(1) if prog_match else Path(fortran_file).stem
    
    # Extract description from initial comment block
    desc_match = re.search(r'program\s+\w+\s*\n\s*!!\s*(.+?)(?:\n(?!\s*!!)|\n\s*use)', content, re.DOTALL)
    description = desc_match.group(1).strip() if desc_match else "Example demonstrating fortplotlib features."
    
    # Extract output files from savefig calls
    output_files = re.findall(r"savefig\(['\"](.*?)['\"]\)", content)
    
    return {
        'name': program_name,
        'description': description,
        'source_file': fortran_file,
        'output_files': output_files
    }

def generate_example_page(example_info, output_dir):
    """Generate a markdown page for a single example."""
    name = example_info['name']
    desc = example_info['description']
    source_file = example_info['source_file']
    
    # Read source code
    with open(source_file, 'r') as f:
        source_code = f.read()
    
    # Determine output files
    example_dir = Path(source_file).parent
    png_files = sorted(glob.glob(str(example_dir / "*.png")))
    txt_files = sorted(glob.glob(str(example_dir / "*.txt")))
    pdf_files = sorted(glob.glob(str(example_dir / "*.pdf")))
    
    # Start building the markdown content
    content = f"""title: {name.replace('_', ' ').title()}
---

# {name.replace('_', ' ').title()}

{desc}

## Source Code

```fortran
{source_code}
```

## Output

"""
    
    # Add PNG outputs
    if png_files:
        content += "### Graphical Output\n\n"
        for png in png_files:
            png_name = Path(png).name
            content += f"![{png_name}](../media/examples/{png_name})\n\n"
    
    # Add ASCII outputs
    if txt_files:
        content += "### ASCII Output\n\n"
        for txt in txt_files:
            txt_name = Path(txt).name
            try:
                with open(txt, 'r') as f:
                    ascii_content = f.read()
                content += f"**{txt_name}:**\n\n```\n{ascii_content}\n```\n\n"
            except:
                content += f"**{txt_name}:** *(ASCII output available in repository)*\n\n"
    
    # Add PDF links
    if pdf_files:
        content += "### PDF Output\n\n"
        for pdf in pdf_files:
            pdf_name = Path(pdf).name
            content += f"- [{pdf_name}](https://github.com/lazy-fortran/fortplot/blob/main/{pdf})\n"
        content += "\n"
    
    # Add link back to source
    rel_source = os.path.relpath(source_file, '.')
    content += f"""
---

Source: [{rel_source}](https://github.com/lazy-fortran/fortplot/blob/main/{rel_source})
"""
    
    # Write the markdown file
    output_file = output_dir / f"{name}.md"
    with open(output_file, 'w') as f:
        f.write(content)
    
    print(f"Generated: {output_file}")

def main():
    """Generate documentation for all examples."""
    # Create output directory
    output_dir = Path("doc/examples")
    output_dir.mkdir(exist_ok=True, parents=True)
    
    # Find all example files
    example_files = sorted(glob.glob("example/fortran/*/*.f90"))
    
    print(f"Found {len(example_files)} examples")
    
    # Process each example
    for example_file in example_files:
        example_info = extract_example_info(example_file)
        generate_example_page(example_info, output_dir)
    
    print(f"\nDocumentation generated in {output_dir}")

if __name__ == "__main__":
    main()