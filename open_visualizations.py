#!/usr/bin/env python3
"""
Quick launcher for NASA visualizations
Opens all visualization files in your default browser

Usage:
    python3 open_visualizations.py
"""

import os
import sys
import webbrowser
from pathlib import Path
import subprocess
import time

def find_visualizations():
    """Find all visualization HTML files"""
    viz_dir = Path('all_visualizations')

    if not viz_dir.exists():
        print("❌ No visualizations found. Run the pipeline first:")
        print("   python3 visualize_all_libraries.py")
        return []

    files = list(viz_dir.rglob('*.html'))
    return sorted(files)

def get_file_size(filepath):
    """Get human-readable file size"""
    size = filepath.stat().st_size
    for unit in ['B', 'KB', 'MB', 'GB']:
        if size < 1024.0:
            return f"{size:.1f} {unit}"
        size /= 1024.0
    return f"{size:.1f} TB"

def start_http_server(port=8000):
    """Start HTTP server in background"""
    print(f"🚀 Starting HTTP server on port {port}...")
    print(f"   Visit: http://localhost:{port}")
    print()

    try:
        os.chdir('all_visualizations')
        subprocess.Popen(
            [sys.executable, '-m', 'http.server', str(port)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        time.sleep(2)  # Give server time to start
        webbrowser.open(f'http://localhost:{port}')
        print("✅ Server started! Press Ctrl+C in terminal to stop.")
        print()
        return True
    except Exception as e:
        print(f"⚠️  Failed to start server: {e}")
        return False

def open_file_directly(filepath):
    """Open file in default browser"""
    abs_path = filepath.resolve()

    try:
        # Try different methods depending on OS
        if sys.platform == 'darwin':  # macOS
            subprocess.run(['open', str(abs_path)])
        elif sys.platform == 'win32':  # Windows
            os.startfile(str(abs_path))
        else:  # Linux
            subprocess.run(['xdg-open', str(abs_path)])
        return True
    except:
        # Fallback to webbrowser
        try:
            webbrowser.open(f'file://{abs_path}')
            return True
        except:
            return False

def main():
    """Main launcher"""
    print("=" * 80)
    print("🌌 NASA VISUALIZATION LAUNCHER")
    print("=" * 80)
    print()

    # Find files
    files = find_visualizations()

    if not files:
        return

    print(f"📊 Found {len(files)} visualization files:")
    print()

    # Group by library
    by_library = {}
    for f in files:
        lib = f.parent.name
        if lib not in by_library:
            by_library[lib] = []
        by_library[lib].append(f)

    # Display grouped files
    for lib, lib_files in sorted(by_library.items()):
        print(f"  {lib.upper()}:")
        for i, f in enumerate(lib_files, 1):
            size = get_file_size(f)
            print(f"    {i}. {f.name} ({size})")
        print()

    print("=" * 80)
    print("🚀 VIEWING OPTIONS")
    print("=" * 80)
    print()
    print("1. Start HTTP server (recommended - view all files)")
    print("2. Open individual file in browser")
    print("3. Show file paths only")
    print("4. Exit")
    print()

    choice = input("Choose option (1-4): ").strip()
    print()

    if choice == '1':
        # HTTP server
        start_http_server()
        print("Keep this terminal open to keep server running.")
        print("Press Ctrl+C to stop when done viewing.")

        # Keep running
        try:
            import time
            while True:
                time.sleep(1)
        except KeyboardInterrupt:
            print("\n\n✅ Server stopped.")

    elif choice == '2':
        # Open individual files
        print("Select file to open:")
        all_files_flat = []
        idx = 1
        for lib, lib_files in sorted(by_library.items()):
            for f in lib_files:
                print(f"  {idx}. {lib}/{f.name}")
                all_files_flat.append(f)
                idx += 1
        print()

        file_choice = input(f"Choose file (1-{len(all_files_flat)}): ").strip()

        try:
            file_idx = int(file_choice) - 1
            if 0 <= file_idx < len(all_files_flat):
                selected_file = all_files_flat[file_idx]
                print(f"Opening {selected_file.name}...")
                if open_file_directly(selected_file):
                    print("✅ File opened in browser!")
                else:
                    print(f"⚠️  Could not auto-open. Open manually:")
                    print(f"   {selected_file.resolve()}")
            else:
                print("❌ Invalid selection")
        except ValueError:
            print("❌ Invalid input")

    elif choice == '3':
        # Show paths
        print("📁 Visualization file paths:")
        print()
        for f in files:
            print(f"  {f.resolve()}")
        print()
        print("To open, use:")
        print("  xdg-open <path>    (Linux)")
        print("  open <path>        (Mac)")
        print("  start <path>       (Windows)")

    else:
        print("👋 Exiting")

    print()
    print("=" * 80)

if __name__ == "__main__":
    main()
