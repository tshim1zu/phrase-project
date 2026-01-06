#!/usr/bin/env python
"""
Build and distribution helper script for japhrase package.

This script provides utilities for building wheels, source distributions,
and checking package configuration.

Usage:
    python scripts/build.py --check      # Validate configuration
    python scripts/build.py --build      # Build wheel and source distribution
    python scripts/build.py --test-local # Test local installation
    python scripts/build.py --clean      # Clean build artifacts
"""

import os
import sys
import shutil
import subprocess
import argparse
from pathlib import Path


def get_project_root() -> Path:
    """Get the project root directory."""
    return Path(__file__).parent.parent


def run_command(cmd: list, description: str = "") -> int:
    """Run a shell command and return exit code."""
    if description:
        print(f"\n{'='*60}")
        print(f"  {description}")
        print(f"{'='*60}")
    print(f"Running: {' '.join(cmd)}\n")
    return subprocess.call(cmd)


def check_configuration() -> bool:
    """Validate package configuration files."""
    print("\n" + "="*60)
    print("  Checking Configuration")
    print("="*60)
    
    root = get_project_root()
    required_files = [
        "pyproject.toml",
        "README.md",
        "LICENSE",
        "MANIFEST.in",
        "requirements.txt",
        "CHANGELOG.md",
    ]
    
    all_exist = True
    for filename in required_files:
        filepath = root / filename
        status = "✓" if filepath.exists() else "✗"
        print(f"{status} {filename}")
        if not filepath.exists():
            all_exist = False
    
    # Check japhrase package
    japhrase_dir = root / "japhrase"
    print(f"\n{'✓' if japhrase_dir.exists() else '✗'} japhrase/ (package directory)")
    
    # Check tests
    tests_dir = root / "tests"
    print(f"{'✓' if tests_dir.exists() else '✗'} tests/ (test directory)")
    
    return all_exist


def build_distributions() -> bool:
    """Build wheel and source distributions."""
    root = get_project_root()
    os.chdir(root)
    
    # Check if build module is installed
    try:
        import build
    except ImportError:
        print("\nERROR: 'build' module not found.")
        print("Install it with: pip install build\n")
        return False
    
    # Clean previous builds
    for dirname in ["build", "dist", "*.egg-info"]:
        path = root / dirname if not dirname.startswith("*") else None
        if path and path.exists():
            shutil.rmtree(path)
    
    # Build distributions
    ret = run_command(
        [sys.executable, "-m", "build"],
        "Building wheel and source distribution"
    )
    
    if ret == 0:
        print("\n" + "="*60)
        print("  Build completed successfully!")
        print("="*60)
        dist_dir = root / "dist"
        if dist_dir.exists():
            print("\nGenerated files in dist/:")
            for f in dist_dir.glob("*"):
                print(f"  - {f.name}")
    
    return ret == 0


def test_local_installation() -> bool:
    """Test installation from built wheel."""
    root = get_project_root()
    
    print("\n" + "="*60)
    print("  Testing Local Installation")
    print("="*60)
    
    dist_dir = root / "dist"
    if not dist_dir.exists():
        print("ERROR: dist/ directory not found. Run --build first.")
        return False
    
    # Find the wheel file
    wheels = list(dist_dir.glob("*.whl"))
    if not wheels:
        print("ERROR: No wheel file found in dist/")
        return False
    
    wheel_file = wheels[-1]  # Use the latest wheel
    print(f"\nTesting with: {wheel_file.name}")
    
    # Install in temporary virtual environment
    venv_dir = root / ".test-venv"
    if venv_dir.exists():
        shutil.rmtree(venv_dir)
    
    print("\nCreating temporary virtual environment...")
    ret = subprocess.call([sys.executable, "-m", "venv", str(venv_dir)])
    if ret != 0:
        print("ERROR: Failed to create virtual environment")
        return False
    
    # Determine pip executable
    pip_exe = venv_dir / ("Scripts" if sys.platform == "win32" else "bin") / "pip"
    python_exe = venv_dir / ("Scripts" if sys.platform == "win32" else "bin") / "python"
    
    print("Installing package from wheel...")
    ret = subprocess.call([str(pip_exe), "install", str(wheel_file)])
    if ret != 0:
        print("ERROR: Installation failed")
        return False
    
    print("\nRunning basic import test...")
    ret = subprocess.call([str(python_exe), "-c", "import japhrase; print('✓ Import successful')"])
    
    # Cleanup
    if venv_dir.exists():
        shutil.rmtree(venv_dir)
    
    return ret == 0


def clean_build_artifacts() -> bool:
    """Clean all build artifacts."""
    root = get_project_root()
    
    print("\n" + "="*60)
    print("  Cleaning Build Artifacts")
    print("="*60)
    
    dirs_to_remove = [
        "build",
        "dist",
        ".test-venv",
        ".pytest_cache",
        ".mypy_cache",
        ".egg-info",
    ]
    
    for dirname in dirs_to_remove:
        path = root / dirname
        if path.exists():
            print(f"Removing {dirname}/")
            shutil.rmtree(path)
    
    # Remove __pycache__ directories
    for pycache in root.rglob("__pycache__"):
        shutil.rmtree(pycache)
    
    print("\nCleanup completed!")
    return True


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Build and distribution helper for japhrase package"
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Validate configuration"
    )
    parser.add_argument(
        "--build",
        action="store_true",
        help="Build wheel and source distribution"
    )
    parser.add_argument(
        "--test-local",
        action="store_true",
        help="Test installation from built wheel"
    )
    parser.add_argument(
        "--clean",
        action="store_true",
        help="Clean build artifacts"
    )
    parser.add_argument(
        "--full",
        action="store_true",
        help="Full build pipeline (clean -> check -> build -> test)"
    )
    
    args = parser.parse_args()
    
    # If no arguments, show help
    if not any(vars(args).values()):
        parser.print_help()
        return 1
    
    if args.full:
        clean_build_artifacts()
        check_configuration()
        if not build_distributions():
            return 1
        if not test_local_installation():
            return 1
        return 0
    
    if args.check:
        return 0 if check_configuration() else 1
    
    if args.build:
        return 0 if build_distributions() else 1
    
    if args.test_local:
        return 0 if test_local_installation() else 1
    
    if args.clean:
        return 0 if clean_build_artifacts() else 1
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
