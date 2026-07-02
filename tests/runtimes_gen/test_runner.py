#!/usr/bin/env python3
#
# Copyright (C) 2025-2026, AdaCore
#

"""Test runner for runtime generation."""
from __future__ import annotations

import argparse
import logging
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

from .tested_repository import AbstractTestedRepository, TargetInfo
from .bb_runtimes_repo import BbRuntimesRepository
from .certified_rts_repo import CertifiedRtsRepository
from .legacy_repos import setup_legacy_baseline
from .colored_logger import setup_colored_logging

# Setup colored logging
log = setup_colored_logging("runtimes_gen_test", logging.DEBUG)

DESC_DIR_REL = Path("lib") / "gnat"
SRC_TREE_REL = Path("include") / "rts-sources"


@dataclass
class TestConfig:
    """Test configuration."""

    repo: AbstractTestedRepository
    gcc_path: Path
    gnat_path: Path
    base_profiles: list[str]
    link_modes: list[bool]
    subset_mode: bool
    verbose: bool
    stop_at_first: bool = False


@dataclass
class TestResult:
    """Test result for a single combination."""

    repo_name: str
    platform: str
    base_profile: str
    link_mode: bool
    target_cli_name: str
    passed: bool
    diff_output: str = ""
    error_message: str = ""


def diff_dirs(dir1: Path, dir2: Path, verbose: bool = False) -> str:
    """Compare two directories and return diff output."""

    if not dir1.is_dir() or not dir2.is_dir():
        raise ValueError("Both dir1 and dir2 must be directories")

    cmd = ["diff", "-r"]
    if not verbose:
        cmd.append("--brief")
    cmd.extend([str(dir1), str(dir2)])

    completed = subprocess.run(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=False
    )

    if completed.returncode == 0:
        return ""
    elif completed.returncode == 1:
        return completed.stdout
    else:
        return f"Error running diff: {completed.stderr}"


def run_test_combination(
    config: TestConfig,
    platform: str,
    base_profile: str,
    link_mode: bool,
    baseline_dir: Path,
    candidate_dir: Path,
    target: TargetInfo,
) -> TestResult:
    """Run assembly and targetizer for one test combination."""
    # Log test start using repository-specific logging (only in verbose mode)
    config.repo.log_test_start(
        target=target,
        platform=platform,
        base_profile=base_profile,
        link_mode=link_mode,
        verbose=config.verbose,
        logger=log,
    )

    try:
        # Create output directory: targetized_runtimes/target
        # All targetized outputs are grouped under targetized_runtimes meta folder
        cand_output = candidate_dir / "targetized_runtimes" / target.cli_name

        # Run assembly step for candidate (only once per platform/top base profile)
        # Assembly output is shared across targets with the same platform
        # All assembly folders are grouped under assembly/platform/top_base_profile structure
        assembly_output = candidate_dir / "assembly" / platform / base_profile
        if not assembly_output.exists():
            if config.verbose:
                log.info("Step 1/2: Running assembly...")
            try:
                config.repo.run_assembly(
                    platform=platform,
                    base_profile=base_profile,
                    output_dir=assembly_output,
                    gcc_path=config.gcc_path,
                    gnat_path=config.gnat_path,
                    link_mode=link_mode,
                    verbose=config.verbose,
                )
                if config.verbose:
                    log.info("✓ Assembly completed successfully")
            except Exception as e:
                # If assembly fails, stop immediately - don't continue
                if config.verbose:
                    log.error("✗ Assembly failed")
                return TestResult(
                    repo_name=config.repo.__class__.__name__,
                    platform=platform,
                    base_profile=base_profile,
                    link_mode=link_mode,
                    target_cli_name=target.cli_name,
                    passed=False,
                    error_message=f"Assembly failed: {e}",
                )
        else:
            if config.verbose:
                log.info(
                    "Step 1/2: Assembly (skipped - already exists for %s/%s at %s)",
                    platform,
                    base_profile,
                    assembly_output,
                )

        # Run targetizer step for candidate in target-specific directory
        cand_desc = assembly_output / DESC_DIR_REL / "rts-sources.json"
        if cand_desc.exists():
            if config.verbose:
                log.info("Step 2/2: Running targetizer...")
            try:
                config.repo.run_targetizer(
                    descriptor_file=cand_desc,
                    target_cli_name=target.cli_name,
                    output_dir=cand_output,
                    base_profile=base_profile,
                    verbose=config.verbose,
                )
                if config.verbose:
                    log.info("✓ Targetizer completed successfully")
            except Exception as e:
                # If targetizer fails, stop immediately - don't continue
                if config.verbose:
                    log.error("✗ Targetizer failed")
                return TestResult(
                    repo_name=config.repo.__class__.__name__,
                    platform=platform,
                    base_profile=base_profile,
                    link_mode=link_mode,
                    target_cli_name=target.cli_name,
                    passed=False,
                    error_message=f"Targetizer failed: {e}",
                )

        # Compare with baseline if baseline repo exists
        if config.repo.baseline:
            base_output = baseline_dir / "targetized_runtimes" / target.cli_name
            if base_output.exists():
                diff_result = diff_dirs(base_output, cand_output, config.verbose)
                if diff_result:
                    # Fail if there are differences
                    return TestResult(
                        repo_name=config.repo.__class__.__name__,
                        platform=platform,
                        base_profile=base_profile,
                        link_mode=link_mode,
                        target_cli_name=target.cli_name,
                        passed=False,
                        diff_output=diff_result,
                    )
            else:
                # Baseline repo exists but baseline output doesn't - this is an error
                return TestResult(
                    repo_name=config.repo.__class__.__name__,
                    platform=platform,
                    base_profile=base_profile,
                    link_mode=link_mode,
                    target_cli_name=target.cli_name,
                    passed=False,
                    error_message=f"Baseline expected but not found at {base_output}",
                )

        return TestResult(
            repo_name=config.repo.__class__.__name__,
            platform=platform,
            base_profile=base_profile,
            link_mode=link_mode,
            target_cli_name=target.cli_name,
            passed=True,
        )

    except Exception as e:
        # Unexpected errors should also stop immediately
        log.error("Test combination failed with unexpected error: %s", e)
        return TestResult(
            repo_name=config.repo.__class__.__name__,
            platform=platform,
            base_profile=base_profile,
            link_mode=link_mode,
            target_cli_name=target.cli_name,
            passed=False,
            error_message=str(e),
        )


def run_tests(
    config: TestConfig, output_dir: Path, target_filter: str | None = None
) -> tuple[list[TestResult], set[tuple[str, str]], set[tuple[str, str]]]:
    """Run all test combinations and return results with baseline/current target sets."""
    results: list[TestResult] = []

    # Get targets to test
    if config.subset_mode:
        targets = config.repo.get_targets_list_subset()
        log.info("Subset mode: testing %d targets", len(targets))
    else:
        targets = config.repo.get_targets_list()
        log.info("Full mode: testing %d targets", len(targets))

    # Filter by specific target if requested
    if target_filter:
        targets = [t for t in targets if t.cli_name == target_filter]
        if not targets:
            log.warning(
                "Target '%s' not found in repo %s", target_filter, config.repo.root_path
            )
            return results, set(), set()
        log.info("Testing single target: %s", target_filter)

    if not targets:
        log.warning("No targets found to test!")
        return results, set(), set()

    # Sort targets by platform for organized test execution
    targets = sorted(targets, key=lambda t: (t.platform, t.cli_name))

    # Track which targets were successfully tested in baseline and current repo
    # Store tuples of (target_name, top_base_profile)
    baseline_tested_targets: set[tuple[str, str]] = set()
    current_tested_targets: set[tuple[str, str]] = set()

    # Create repository-specific directories to avoid cross-contamination
    # Each repository gets its own subdirectory based on root path name
    repo_subdir = config.repo.root_path.name

    # Generate baseline if baseline repo exists
    if config.repo.baseline:
        # Create baseline directory (repository-specific)
        baseline_dir = output_dir / "baseline" / config.repo.baseline.root_path.name
        baseline_dir.mkdir(parents=True, exist_ok=True)
        log.info("-" * 50)
        log.info(
            "Baseline exists ! Generating baseline outputs from %s...",
            config.repo.baseline.root_path,
        )
        # Generate baseline outputs for each target
        baseline_targets = config.repo.baseline.get_targets_list()
        if config.subset_mode or target_filter:
            # Filter baseline targets to match candidate targets
            baseline_targets = [
                bt
                for bt in baseline_targets
                if any(bt.cli_name == t.cli_name for t in targets)
            ]

        log.info("Generating baseline for %d targets...", len(baseline_targets))
        for link_mode in config.link_modes:
            for target in baseline_targets:
                # Log baseline test start using the same format as candidate
                config.repo.baseline.log_test_start(
                    target=target,
                    platform=target.platform,
                    base_profile=target.top_base_profile,
                    link_mode=link_mode,
                    verbose=False,
                    logger=log,
                )

                # Generate baseline assembly (shared per platform/top base profile)
                baseline_assembly_output = (
                    baseline_dir
                    / "assembly"
                    / target.platform
                    / target.top_base_profile
                )
                if not baseline_assembly_output.exists():
                    if config.verbose:
                        log.info("Step 1/2: Running assembly...")
                    try:
                        config.repo.baseline.run_assembly(
                            platform=target.platform,
                            base_profile=target.top_base_profile,
                            output_dir=baseline_assembly_output,
                            gcc_path=config.gcc_path,
                            gnat_path=config.gnat_path,
                            link_mode=link_mode,
                            verbose=config.verbose,
                        )
                        if config.verbose:
                            log.info(
                                "✓ Baseline assembly for %s/%s",
                                target.platform,
                                target.top_base_profile,
                            )
                    except Exception as e:
                        error_msg = f"Baseline assembly failed: {e}"
                        log.error(
                            "Failed to generate baseline assembly for %s/%s",
                            target.platform,
                            target.top_base_profile,
                        )
                        # Create a failed test result for baseline assembly failure
                        results.append(
                            TestResult(
                                repo_name=config.repo.baseline.__class__.__name__,
                                platform=target.platform,
                                base_profile=target.top_base_profile,
                                link_mode=link_mode,
                                target_cli_name=target.cli_name,
                                passed=False,
                                error_message=error_msg,
                            )
                        )
                        continue
                else:
                    if config.verbose:
                        log.info(
                            "✓ Baseline assembly for %s/%s (already exists)",
                            target.platform,
                            target.top_base_profile,
                        )

                # Generate baseline targetizer output (per target)
                baseline_desc = (
                    baseline_assembly_output / DESC_DIR_REL / "rts-sources.json"
                )
                if baseline_desc.exists():
                    baseline_targetized_output = (
                        baseline_dir / "targetized_runtimes" / target.cli_name
                    )
                    if not baseline_targetized_output.exists():
                        if config.verbose:
                            log.info("Step 2/2: Running targetizer...")
                        try:
                            config.repo.baseline.run_targetizer(
                                descriptor_file=baseline_desc,
                                target_cli_name=target.cli_name,
                                output_dir=baseline_targetized_output,
                                base_profile=target.top_base_profile,
                                verbose=config.verbose,
                            )
                            if config.verbose:
                                log.info(
                                    "✓ Baseline targetizer for %s", target.cli_name
                                )
                            # Track successfully generated baseline target with top base profile
                            baseline_tested_targets.add(
                                (target.cli_name, target.top_base_profile)
                            )
                        except Exception as e:
                            error_msg = f"Baseline targetizer failed: {e}"
                            log.error(
                                "Failed to generate baseline targetizer for %s",
                                target.cli_name,
                            )
                            # Create a failed test result for baseline targetizer failure
                            results.append(
                                TestResult(
                                    repo_name=config.repo.baseline.__class__.__name__,
                                    platform=target.platform,
                                    base_profile=target.top_base_profile,
                                    link_mode=link_mode,
                                    target_cli_name=target.cli_name,
                                    passed=False,
                                    error_message=error_msg,
                                )
                            )
                    else:
                        if config.verbose:
                            log.info(
                                "✓ Baseline targetizer for %s (cached)",
                                target.cli_name,
                            )
                        # Still track cached baseline targets
                        baseline_tested_targets.add(
                            (target.cli_name, target.top_base_profile)
                        )

        log.info("Baseline generation completed")
    else:
        # No baseline - create placeholder for consistency
        baseline_dir = output_dir / "baseline" / repo_subdir

    log.info("-" * 50)

    # Create candidate directory (repository-specific)
    candidate_dir = output_dir / "candidate" / repo_subdir
    candidate_dir.mkdir(parents=True, exist_ok=True)

    # Run tests for each combination
    # Note: Each target has its own platform and deduced top base profile
    total_combinations = len(config.link_modes) * len(targets)

    log.info(
        "Running %d combinations (%d link modes × %d targets)...",
        total_combinations,
        len(config.link_modes),
        len(targets),
    )
    for link_mode in config.link_modes:
        for target in targets:
            # Use target's own platform and deduced top base profile
            result = run_test_combination(
                config,
                target.platform,
                target.top_base_profile,  # Use target's deduced top base profile
                link_mode,
                baseline_dir,
                candidate_dir,
                target,
            )
            results.append(result)

            # Track successfully tested current repo targets with top base profile
            # We track targets that completed targetization (even if diff failed)
            if not result.error_message:
                current_tested_targets.add((target.cli_name, target.top_base_profile))

            # Stop at first failure if requested
            if config.stop_at_first and not result.passed:
                if result.error_message:
                    log.warning(
                        "Stopping at first failure: execution error (--stop-at-first enabled)"
                    )
                elif result.diff_output:
                    log.warning(
                        "Stopping at first failure: diff found (--stop-at-first enabled)"
                    )
                    if not config.verbose:
                        log.info("💡 Tip: Run with --verbose to see full diff output")
                return results, baseline_tested_targets, current_tested_targets

    return results, baseline_tested_targets, current_tested_targets


def print_results(
    results: list[TestResult],
    verbose: bool,
    baseline_tested: set[tuple[str, str]] | None = None,
    current_tested: set[tuple[str, str]] | None = None,
    target_filter: str | None = None,
) -> int:
    """Print test results summary and return exit code."""
    passed = sum(1 for r in results if r.passed)
    failed = sum(1 for r in results if not r.passed)
    execution_errors_count = sum(1 for r in results if not r.passed and r.error_message)
    diff_errors_count = sum(
        1 for r in results if not r.passed and not r.error_message and r.diff_output
    )

    separator = "=" * 70
    log.info("\n%s", separator)
    log.info(
        "Test Results: %d passed, %d failed (%d execution errors, %d diff errors)",
        passed,
        failed,
        execution_errors_count,
        diff_errors_count,
    )
    log.info("%s", separator)

    if failed > 0:
        # diff_errors materialized to a list because it is iterated twice
        # (loop + len/set-comp tip below).
        diff_errors = [
            r for r in results if not r.passed and not r.error_message and r.diff_output
        ]

        if execution_errors_count > 0:
            log.info("\nExecution Failures:")
            for result in (r for r in results if not r.passed and r.error_message):
                log.error(
                    "  ❌ %s: target: %s",
                    result.repo_name,
                    result.target_cli_name,
                )
                if verbose:
                    log.error("     Error: %s", result.error_message)

        if diff_errors:
            log.info("\nDiff Failures:")
            for result in diff_errors:
                log.error(
                    "  ❌ %s: target: %s",
                    result.repo_name,
                    result.target_cli_name,
                )
                # Add bold separator line between diff items
                separator = "\033[1m" + "=" * 80 + "\033[0m"
                formatted_output = result.diff_output.replace(
                    "diff -r ", f"\n{separator}\ndiff -r "
                ).replace("Only in ", f"\n{separator}\nOnly in ")
                log.error("     Diff:%s", formatted_output)

        if not verbose:
            log.info("💡 Tip: Run with --verbose to see full error output")
        if not target_filter and len(diff_errors) > 0:
            # Get unique target names from diff errors
            failing_targets = {r.target_cli_name for r in diff_errors}
            if len(failing_targets) == 1:
                target_name = next(iter(failing_targets))
                log.info(
                    "💡 Tip: Run with --target %s to test only this target",
                    target_name,
                )
            else:
                log.info("💡 Tip: Run with --target <name> to retest a specific target")

        return_code = 1
    else:
        log.info("\n✅ All tests passed!")
        return_code = 0

    # Print baseline comparison report if we have both sets
    if baseline_tested is not None and current_tested is not None and baseline_tested:
        log.info("\n%s", separator)
        log.info("Baseline Comparison Report")
        log.info("%s", separator)

        # Targets in baseline but not in current repo
        only_in_baseline = baseline_tested - current_tested
        if only_in_baseline:
            # Format as "target (top base profile)"
            targets_list = ", ".join(
                f"{target} ({profile})" for target, profile in sorted(only_in_baseline)
            )
            log.warning(
                "⚠️  %d target(s) tested in baseline but NOT in current repo: %s",
                len(only_in_baseline),
                targets_list,
            )
        else:
            log.info("✓ All baseline targets are also tested in current repo")

        # Targets in current repo but not in baseline
        only_in_current = current_tested - baseline_tested
        if only_in_current:
            # Format as "target (top base profile)"
            targets_list = ", ".join(
                f"{target} ({profile})" for target, profile in sorted(only_in_current)
            )
            log.warning(
                "⚠️  %d target(s) tested in current repo but NOT in baseline: %s",
                len(only_in_current),
                targets_list,
            )
        else:
            log.info("✓ All current targets are also in baseline")

        log.info("\n%s", separator)

    return return_code


def discover_certified_rts_repos(
    bb_runtimes_path: Path, logger: logging.Logger
) -> list[CertifiedRtsRepository]:
    """Discover certified-rts repository at ../../Cert/certified-rts."""
    repos: list[CertifiedRtsRepository] = []

    # Fixed path: ../../Cert/certified-rts
    cert_rts_path = bb_runtimes_path.parent.parent / "Cert" / "certified-rts"

    if not cert_rts_path.exists():
        logger.info("Certified-rts not found at %s (skipping)", cert_rts_path)
        return repos

    assert cert_rts_path.is_dir(), f"Expected {cert_rts_path} to be a directory"
    logger.info("Found certified-rts at %s", cert_rts_path)

    try:
        repo = CertifiedRtsRepository(
            root_path=cert_rts_path, bb_runtimes_root=bb_runtimes_path
        )
        # Log the targets found
        targets = repo.get_targets_list()
        target_names = sorted({t.cli_name for t in targets})
        for target_name in target_names:
            logger.info("  Found target: %s", target_name)
        repos.append(repo)
    except Exception as e:
        logger.warning(
            "Failed to initialize certified-rts repository: %s",
            str(e),
        )

    return repos


def main() -> int:
    """Main entry point for test runner."""
    parser = argparse.ArgumentParser(
        description="Test runtime generation across repository variants"
    )

    parser.add_argument(
        "--repo",
        type=Path,
        default=Path.cwd(),
        help="Path to bb_runtimes repository root (default: current directory)",
    )

    parser.add_argument(
        "--gcc",
        type=Path,
        required=False,
        help="Path to GCC toolchain (absolute path)",
    )

    parser.add_argument(
        "--gnat",
        type=Path,
        required=False,
        help="Path to GNAT toolchain (absolute path)",
    )

    parser.add_argument(
        "--subset",
        action="store_true",
        help="Test only a subset of targets for quick validation",
    )

    parser.add_argument(
        "--target",
        type=str,
        help="Test only a specific target by its CLI name",
    )

    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Enable verbose output including full diffs",
    )

    parser.add_argument(
        "--output-dir",
        type=Path,
        help="Output directory for test artifacts (default: temp directory)",
    )

    parser.add_argument(
        "--keep-output",
        action="store_true",
        help="Keep output directory after test completion",
    )

    parser.add_argument(
        "--stop-at-first",
        action="store_true",
        help="Stop testing at the first failure instead of continuing through all targets",
    )

    parser.add_argument(
        "--ci-mode",
        action="store_true",
        help="CI mode: Test only bb-runtimes (skip certified-rts and baseline comparison).",
    )

    args = parser.parse_args()

    # Setup output directory
    if args.output_dir:
        output_dir = args.output_dir
        output_dir.mkdir(parents=True, exist_ok=True)
    else:
        output_dir = Path(tempfile.mkdtemp(prefix="runtimes_gen_test_"))

    log.info("Output directory: %s", output_dir)

    try:
        # Initialize bb-runtimes repository
        bb_repo = BbRuntimesRepository(args.repo)

        # Discover certified-rts repositories (unless --ci-mode is set)
        if args.ci_mode:
            log.info("CI mode: Skipping certified-rts discovery")
            cert_repos = []
        else:
            cert_repos = discover_certified_rts_repos(args.repo, log)

        # Collect all repositories to test
        all_repos = [bb_repo] + cert_repos

        if not cert_repos:
            if not args.ci_mode:
                log.info(
                    "No certified-rts repositories found (continuing with bb-runtimes only)"
                )
        else:
            log.info(
                "Testing %d repositories: 1 bb-runtimes + %d certified-rts",
                len(all_repos),
                len(cert_repos),
            )

        # Setup legacy baselines for all repositories (unless --ci-mode is set)
        if args.ci_mode:
            log.info("CI mode: Skipping baseline setup")
        else:
            for repo in all_repos:
                setup_legacy_baseline(repo, log, args.repo)

        # Run tests for all repositories
        all_results: list[TestResult] = []
        all_baseline_tested: set[tuple[str, str]] = set()
        all_current_tested: set[tuple[str, str]] = set()

        for repo in all_repos:
            separator = "=" * 70
            log.info("%s", separator)
            log.info(
                "Testing repository: %s (%s)", repo.__class__.__name__, repo.root_path
            )
            log.info("%s", separator)

            # Configure test
            config = TestConfig(
                repo=repo,
                gcc_path=args.gcc,
                gnat_path=args.gnat,
                base_profiles=["embedded"],
                link_modes=[False],  # copy mode only for now
                subset_mode=args.subset,
                verbose=args.verbose,
                stop_at_first=args.stop_at_first,
            )

            # Run tests for this repo
            results, baseline_tested, current_tested = run_tests(
                config, output_dir, args.target
            )

            all_results.extend(results)
            all_baseline_tested.update(baseline_tested)
            all_current_tested.update(current_tested)

            # Stop at first failure across all repos if requested
            if args.stop_at_first and any(not r.passed for r in results):
                log.warning(
                    "Stopping all testing due to failure in %s", repo.__class__.__name__
                )
                break

        # Print combined results
        exit_code = print_results(
            all_results,
            args.verbose,
            all_baseline_tested,
            all_current_tested,
            args.target,
        )

        return exit_code

    finally:
        # Cleanup
        if not args.keep_output and not args.output_dir:
            if output_dir.exists():
                shutil.rmtree(output_dir)
                log.info("Cleaned up temporary directory")


if __name__ == "__main__":
    sys.exit(main())
