import sys, os, subprocess

if os.name == 'nt':
    _exe = ".exe"
else:
    _exe = ""

def _pipe(cmd):
    proc = subprocess.Popen(cmd,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    return proc.communicate()

def hgRoot():
    output, errors = _pipe(['hg', 'root'])
    if len(errors) != 0 and "(.hg not found)" in errors:
        output, errors = _pipe(['git', 'rev-parse', '--show-toplevel'])
    if len(errors) != 0:
        raise subprocess.CalledProcessError(errors)
    assert len(output.splitlines()) == 1
    return output.strip()

def jssrc():
    if os.path.exists("CVS"):
        raise ValueError("no longer suppporting cvs")

    root = hgRoot()
    srcdir = os.path.join(root, "js", "src")
    if not os.path.isdir(srcdir):
        raise ValueError("No js/src directory found in %r" % root)
    return srcdir

def executables(variants):
    srcdir = jssrc()
    executables = []
    for variant in variants:
        exe = os.path.join(srcdir, variant+'-objdir', 'js' + _exe)
        if not os.path.isfile(exe):
            raise ValueError("Variant %s not found at %s" % (variant, exe))
        executables.append(exe)
    return executables

def runMain(main):
    try:
        exitCode = main()
    except Exception:
        import traceback
        traceback.print_exc()
        exitCode = 2
    sys.exit(exitCode)
