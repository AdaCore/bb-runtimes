class Config(object):
    # Sources directories
    gnatdir = "../gnat"
    gccdir = "../gcc"
    shared_sources = "runtime-sources"
    bsp_sources = "bsps"

    # Output directory
    objdir = "install"

    # RTS installation directory
    prefix = None

    # Display actions
    verbose = False

    # common rts files to install
    rts_srcs = None

    link = False
    config = ""
