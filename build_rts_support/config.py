class Config(object):
    # Sources directories
    gnatdir = "../gnat"
    gccdir = "../gcc"
    crossdir = "../cross/bare_board/libbareboard"
    shared_sources = "runtime-sources"
    bsp_sources = "bsps"

    # Output directory
    objdir = "install"

    # Display actions
    verbose = False

    # create a common directory
    create_common = False
    # common rts files
    rts_srcs = None

    link = False
    config = ""
