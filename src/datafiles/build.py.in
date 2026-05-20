import argparse
import glob
import os
import re
import subprocess
import sys

runtime_path = os.path.dirname(os.path.abspath(__file__))

def get_runtime_version():
    system_ali = os.path.join(runtime_path, "adalib", "system.ali")
    with open(system_ali, "r") as ali:
        return re.match("V \"GNAT Lib v([0-9]{2}\\.?[^\"]*)", ali.readline()).group(1)

def main(shared, build_flags):
    projects = glob.glob("*.gpr", root_dir=runtime_path)
    projects.remove("target_options.gpr")
    projects.remove("runtime_build.gpr")
    if "ravenscar_build.gpr" in projects:
        projects.remove("ravenscar_build.gpr")
        projects.append("ravenscar_build.gpr")
    else:
        projects.append("runtime_build.gpr")

    print(f"Building runtime {runtime_path}")
    obj_dir = os.path.join(runtime_path, "obj")
    for prj in projects:
        print(f"Building project {prj}")
        sys.stdout.flush()
        cmd = ["gprbuild", "-j0", "-p", "-v", "-P", os.path.join(runtime_path, prj)]
        if build_flags is not None:
            cmd += build_flags.split()
        subprocess.check_call(cmd)
        if shared:
            runtime_version = get_runtime_version()
            cmd.extend([
                "-f",
                "-XLIBRARY_TYPE=dynamic",
                "-XLIBRARY_VERSION=" + runtime_version,
                "-largs",
                "-L" + obj_dir,
            ])
            subprocess.check_call(cmd)
        cleanup_ext = (".o", ".ali", ".stdout", ".stderr", ".d", ".lexch", ".so")
        for fname in os.listdir(obj_dir):
            _, ext = os.path.splitext(fname)
            if ext in cleanup_ext:
                os.unlink(os.path.join(obj_dir, fname))
    if shared:
        runtime_version = get_runtime_version()
        shared_objects = [
            so[:-3] for so in glob.glob("*.so", root_dir=os.path.join(runtime_path, "adalib"))
            if runtime_version not in so
        ]
        for so in shared_objects:
            base_so = os.path.join(runtime_path, "adalib", f"{so}.so")
            versioned_so = os.path.join(runtime_path, "adalib", f"{so}-{runtime_version}.so")
            if os.path.islink(base_so) and os.path.isfile(versioned_so):
                continue
            elif os.path.isfile(base_so) and not os.path.islink(base_so):
                os.replace(base_so, versioned_so)
                os.symlink(os.path.basename(versioned_so), base_so)
            else:
                raise FileNotFoundError(f"{base_so} is neither a link nor a file {os.path.islink(base_so)}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--shared",
        action="store_true",
        help="Additionally build shared runtime "
        "(only available on platforms that support shared libraries)",
    )
    parser.add_argument("--build-flags", help="Flags passed to gprbuild")
    args = parser.parse_args()
    main(args.shared, args.build_flags)
