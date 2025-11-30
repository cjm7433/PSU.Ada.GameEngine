/*
 * ============================================================
 * Wayland Wrapper Implementation
 * ============================================================
 * This file implements a simplified C wrapper around the
 * Wayland client protocol for creating desktop windows.
 * 
 * It includes the complete XDG Shell protocol implementation
 * (consolidated from the previously separate xdg-shell-protocol.c
 * file) and provides high-level functions for window management.
 *
 * Key components:
 * 1. XDG Shell protocol definitions (message structures)
 * 2. Event handlers for Wayland protocol events
 * 3. Shared memory buffer management
 * 4. Public API for window creation and management
 * ============================================================
 */

#include "wayland_wrapper_c.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <wayland-util.h>

/*
 * ============================================================
 * XDG Shell Protocol Implementation
 * ============================================================
 * This section contains the protocol definitions consolidated
 * from the xdg-shell-protocol.c generated file.
 *
 * The protocol is defined through message structures that
 * describe the requests (client to compositor) and events
 * (compositor to client) for each interface type.
 *
 * Each wl_interface structure includes:
 * - Name and version of the interface
 * - Request message array (what client can send)
 * - Event message array (what compositor can send)
 * ============================================================
 */

#ifndef __has_attribute
# define __has_attribute(x) 0  /* Compatibility with non-clang compilers */
#endif

/* Mark interfaces as hidden - not exported from shared library */
#if (__has_attribute(visibility) || defined(__GNUC__) && __GNUC__ >= 4)
#define WL_PRIVATE __attribute__ ((visibility("hidden")))
#else
#define WL_PRIVATE
#endif

/*
 * External interfaces from libwayland-client
 * These are standard Wayland protocol objects
 */
extern const struct wl_interface wl_output_interface;   /* Display output */
extern const struct wl_interface wl_seat_interface;     /* Input device seat */
extern const struct wl_interface wl_surface_interface;  /* Drawing surface */

/*
 * Forward declarations of XDG interfaces defined below
 * These allow the type array to reference them before definition
 */
const struct wl_interface xdg_popup_interface;
const struct wl_interface xdg_positioner_interface;
const struct wl_interface xdg_surface_interface;
const struct wl_interface xdg_toplevel_interface;

/*
 * XDG Shell type array
 * This maps message signature positions to interface types.
 * Used by the protocol to validate and create objects.
 * NULL entries are for basic types (int, uint, string, etc.)
 */
static const struct wl_interface *xdg_shell_types[] = {
    NULL,                           /* Basic types */
    NULL,
    NULL,
    NULL,
    &xdg_positioner_interface,      /* Positioner object */
    &xdg_surface_interface,         /* XDG surface object */
    &wl_surface_interface,          /* Wayland surface */
    &xdg_toplevel_interface,        /* Toplevel window */
    &xdg_popup_interface,           /* Popup menu */
    &xdg_surface_interface,
    &xdg_positioner_interface,
    &xdg_toplevel_interface,
    &wl_seat_interface,             /* Input seat */
    NULL,
    NULL,
    NULL,
    &wl_seat_interface,
    NULL,
    &wl_seat_interface,
    NULL,
    NULL,
    &wl_output_interface,           /* Display output */
    &wl_seat_interface,
    NULL,
    &xdg_positioner_interface,
    NULL,
};

/*
 * ============================================================
 * XDG WM Base Messages
 * ============================================================
 * The window manager base interface provides:
 * - Creating positioners for popup positioning
 * - Getting xdg_surface from wl_surface
 * - Pong responses to keep-alive ping requests
 * ============================================================
 */
static const struct wl_message xdg_wm_base_requests[] = {
    { "destroy", "", xdg_shell_types + 0 },                  /* Destroy the object */
    { "create_positioner", "n", xdg_shell_types + 4 },      /* Create positioner */
    { "get_xdg_surface", "no", xdg_shell_types + 5 },       /* Get XDG surface */
    { "pong", "u", xdg_shell_types + 0 },                   /* Respond to ping */
};

static const struct wl_message xdg_wm_base_events[] = {
    { "ping", "u", xdg_shell_types + 0 },                   /* Compositor ping */
};

/* Version 7 of the xdg_wm_base interface */
WL_PRIVATE const struct wl_interface xdg_wm_base_interface = {
    "xdg_wm_base", 7,
    4, xdg_wm_base_requests,
    1, xdg_wm_base_events,
};

/*
 * ============================================================
 * XDG Positioner Messages
 * ============================================================
 * Positioner is used for popup positioning (not used in this
 * simple wrapper, but required for protocol completeness)
 * ============================================================
 */
static const struct wl_message xdg_positioner_requests[] = {
    { "destroy", "", xdg_shell_types + 0 },
    { "set_size", "ii", xdg_shell_types + 0 },
    { "set_anchor_rect", "iiii", xdg_shell_types + 0 },
    { "set_anchor", "u", xdg_shell_types + 0 },
    { "set_gravity", "u", xdg_shell_types + 0 },
    { "set_constraint_adjustment", "u", xdg_shell_types + 0 },
    { "set_offset", "ii", xdg_shell_types + 0 },
    { "set_reactive", "3", xdg_shell_types + 0 },
    { "set_parent_size", "3ii", xdg_shell_types + 0 },
    { "set_parent_configure", "3u", xdg_shell_types + 0 },
};

WL_PRIVATE const struct wl_interface xdg_positioner_interface = {
    "xdg_positioner", 7,
    10, xdg_positioner_requests,
    0, NULL,  /* No events */
};

/*
 * ============================================================
 * XDG Surface Messages
 * ============================================================
 * The xdg_surface interface provides desktop window semantics.
 * It must be configured before use and requires acknowledgment.
 * ============================================================
 */
static const struct wl_message xdg_surface_requests[] = {
    { "destroy", "", xdg_shell_types + 0 },                  /* Destroy surface */
    { "get_toplevel", "n", xdg_shell_types + 7 },           /* Make it a window */
    { "get_popup", "n?oo", xdg_shell_types + 8 },           /* Make it a popup */
    { "set_window_geometry", "iiii", xdg_shell_types + 0 }, /* Set size */
    { "ack_configure", "u", xdg_shell_types + 0 },          /* Acknowledge config */
};

static const struct wl_message xdg_surface_events[] = {
    { "configure", "u", xdg_shell_types + 0 },              /* Configuration event */
};

WL_PRIVATE const struct wl_interface xdg_surface_interface = {
    "xdg_surface", 7,
    5, xdg_surface_requests,
    1, xdg_surface_events,
};

/*
 * ============================================================
 * XDG Toplevel Messages
 * ============================================================
 * The toplevel interface provides window-specific operations:
 * - Setting title, app ID, parent window
 * - Window state (maximize, minimize, fullscreen)
 * - User interactions (move, resize, show menu)
 * ============================================================
 */
static const struct wl_message xdg_toplevel_requests[] = {
    { "destroy", "", xdg_shell_types + 0 },                 /* Destroy window */
    { "set_parent", "?o", xdg_shell_types + 11 },          /* Set parent window */
    { "set_title", "s", xdg_shell_types + 0 },             /* Set window title */
    { "set_app_id", "s", xdg_shell_types + 0 },            /* Set application ID */
    { "show_window_menu", "ouii", xdg_shell_types + 12 },  /* Show context menu */
    { "move", "ou", xdg_shell_types + 16 },                /* Start move operation */
    { "resize", "ouu", xdg_shell_types + 18 },             /* Start resize */
    { "set_max_size", "ii", xdg_shell_types + 0 },         /* Set max size */
    { "set_min_size", "ii", xdg_shell_types + 0 },         /* Set min size */
    { "set_maximized", "", xdg_shell_types + 0 },          /* Maximize window */
    { "unset_maximized", "", xdg_shell_types + 0 },        /* Unmaximize */
    { "set_fullscreen", "?o", xdg_shell_types + 21 },      /* Go fullscreen */
    { "unset_fullscreen", "", xdg_shell_types + 0 },       /* Exit fullscreen */
    { "set_minimized", "", xdg_shell_types + 0 },          /* Minimize window */
};

static const struct wl_message xdg_toplevel_events[] = {
    { "configure", "iia", xdg_shell_types + 0 },           /* Size/state change */
    { "close", "", xdg_shell_types + 0 },                  /* Close request */
    { "configure_bounds", "4ii", xdg_shell_types + 0 },    /* Size limits */
    { "wm_capabilities", "5a", xdg_shell_types + 0 },      /* WM capabilities */
};

WL_PRIVATE const struct wl_interface xdg_toplevel_interface = {
    "xdg_toplevel", 7,
    14, xdg_toplevel_requests,
    4, xdg_toplevel_events,
};

/*
 * ============================================================
 * XDG Popup Messages
 * ============================================================
 * Popup interface for menus and tooltips
 * (Not used in this wrapper but required for completeness)
 * ============================================================
 */
static const struct wl_message xdg_popup_requests[] = {
    { "destroy", "", xdg_shell_types + 0 },                /* Destroy popup */
    { "grab", "ou", xdg_shell_types + 22 },                /* Grab input */
    { "reposition", "3ou", xdg_shell_types + 24 },         /* Reposition popup */
};

static const struct wl_message xdg_popup_events[] = {
    { "configure", "iiii", xdg_shell_types + 0 },          /* Configuration */
    { "popup_done", "", xdg_shell_types + 0 },             /* Popup dismissed */
    { "repositioned", "3u", xdg_shell_types + 0 },         /* Reposition complete */
};

const struct wl_interface xdg_popup_interface = {
    "xdg_popup", 7,
    3, xdg_popup_requests,
    3, xdg_popup_events,
};

/* End of XDG Shell Protocol Implementation */


/*
 * ============================================================
 * Window Context Structure
 * ============================================================
 * This structure holds all the state for a Wayland window.
 * It's kept opaque to the Ada code for encapsulation.
 * ============================================================
 */

/*
 * ============================================================
 * Window Context Structure
 * ============================================================
 * This structure holds all the state for a Wayland window.
 * It's kept opaque to the Ada code for encapsulation.
 * ============================================================
 */
struct wayland_window_context {
    /* Core Wayland objects */
    struct wl_display *display;         /* Connection to display server */
    struct wl_registry *registry;       /* Registry for global objects */
    struct wl_compositor *compositor;   /* Surface compositor */
    struct wl_shm *shm;                 /* Shared memory manager */
    
    /* XDG Shell objects for window management */
    struct xdg_wm_base *xdg_wm_base;    /* Window manager base */
    struct wl_surface *surface;         /* Drawing surface */
    struct xdg_surface *xdg_surface;    /* Desktop surface wrapper */
    struct xdg_toplevel *xdg_toplevel;  /* Top-level window */
    
    /* Buffer and rendering */
    struct wl_buffer *buffer;           /* Pixel buffer */
    void *shm_data;                     /* Shared memory pointer */
    
    /* Window state */
    int width;                          /* Window width in pixels */
    int height;                         /* Window height in pixels */
    int configured;                     /* Flag: received configure event */
};

/*
 * ============================================================
 * Registry Event Handlers
 * ============================================================
 * The registry is how clients discover what protocol interfaces
 * the compositor supports. When we connect, the compositor
 * announces all available "global" objects through the registry.
 * ============================================================
 */

/*
 * registry_global_handler
 * =======================
 * Called for each global object the compositor announces.
 * We bind to the interfaces we need:
 * - wl_compositor: For creating surfaces
 * - wl_shm: For shared memory buffers
 * - xdg_wm_base: For window management (XDG shell)
 */
static void registry_global_handler(
    void *data,
    struct wl_registry *registry,
    uint32_t name,
    const char *interface,
    uint32_t version)
{
    struct wayland_window_context *ctx = data;
    
    (void)version;  /* Suppress unused parameter warning */
    
    if (strcmp(interface, "wl_compositor") == 0) {
        /* Bind to compositor - creates surfaces */
        ctx->compositor = wl_registry_bind(
            registry, name, &wl_compositor_interface, 1);
        printf("Bound to wl_compositor\n");
    } else if (strcmp(interface, "wl_shm") == 0) {
        /* Bind to shared memory - for pixel buffers */
        ctx->shm = wl_registry_bind(
            registry, name, &wl_shm_interface, 1);
        printf("Bound to wl_shm\n");
    } else if (strcmp(interface, "xdg_wm_base") == 0) {
        /* Bind to XDG shell - for window management */
        ctx->xdg_wm_base = wl_registry_bind(
            registry, name, &xdg_wm_base_interface, 1);
        printf("Bound to xdg_wm_base\n");
    }
}

/*
 * registry_global_remove_handler
 * ===============================
 * Called when a global object is removed (hot-unplug, etc.)
 * Not used in this simple implementation.
 */
static void registry_global_remove_handler(
    void *data,
    struct wl_registry *registry,
    uint32_t name)
{
    (void)data;      /* Unused parameters */
    (void)registry;
    (void)name;
    /* Global object removed - we don't handle this case */
}

/* Registry listener callbacks */
static const struct wl_registry_listener registry_listener = {
    registry_global_handler,
    registry_global_remove_handler
};

/*
 * ============================================================
 * XDG Shell Event Handlers
 * ============================================================
 * These handlers respond to events from the XDG shell protocol.
 * ============================================================
 */

/*
 * xdg_wm_base_ping
 * ================
 * The compositor periodically pings us to check responsiveness.
 * We MUST respond with pong using the same serial number.
 * Failure to respond will cause the compositor to mark us as unresponsive.
 */
static void xdg_wm_base_ping(
    void *data,
    struct xdg_wm_base *xdg_wm_base,
    uint32_t serial)
{
    (void)data;  /* Unused parameter */
    xdg_wm_base_pong(xdg_wm_base, serial);
}

/* XDG WM Base listener callbacks */
static const struct xdg_wm_base_listener xdg_wm_base_listener = {
    xdg_wm_base_ping
};

/*
 * xdg_surface_configure
 * =====================
 * Called when the compositor has configured the xdg_surface.
 * We MUST acknowledge this with ack_configure before the surface can display.
 * This is the "handshake" that lets the compositor know we're ready.
 */
static void xdg_surface_configure(
    void *data,
    struct xdg_surface *xdg_surface,
    uint32_t serial)
{
    struct wayland_window_context *ctx = data;
    
    /* Acknowledge the configure event */
    xdg_surface_ack_configure(xdg_surface, serial);
    
    /* Mark that we've been configured and can now display content */
    ctx->configured = 1;
}

/* XDG Surface listener callbacks */
static const struct xdg_surface_listener xdg_surface_listener = {
    xdg_surface_configure
};

/*
 * xdg_toplevel_configure
 * ======================
 * Called when the compositor wants to change the window's size or state.
 * Width/height are suggestions (0 means no preference).
 * States array contains window states (maximized, fullscreen, etc.)
 * 
 * In this simple implementation, we ignore resize requests and use fixed size.
 */
static void xdg_toplevel_configure(
    void *data,
    struct xdg_toplevel *xdg_toplevel,
    int32_t width,
    int32_t height,
    struct wl_array *states)
{
    /* Unused parameters - we use fixed size */
    (void)data;
    (void)xdg_toplevel;
    (void)width;
    (void)height;
    (void)states;
    /* We're using fixed size, so ignore resize requests */
}

/*
 * xdg_toplevel_close
 * ==================
 * Called when the user requests the window to close (clicks X button).
 * A proper implementation should set a flag and exit gracefully.
 * Currently unhandled - user must use Ctrl+C to exit.
 */
static void xdg_toplevel_close(
    void *data,
    struct xdg_toplevel *xdg_toplevel)
{
    /* Unused parameters */
    (void)data;
    (void)xdg_toplevel;
    /* Window close requested - could set a flag to exit event loop */
}

/* XDG Toplevel listener callbacks */
static const struct xdg_toplevel_listener xdg_toplevel_listener = {
    xdg_toplevel_configure,
    xdg_toplevel_close
};

/*
 * ============================================================
 * Shared Memory Buffer Management
 * ============================================================
 * Wayland uses shared memory for efficient pixel buffer transfer.
 * We create a POSIX shared memory file, map it to our address space,
 * and share it with the compositor.
 * ============================================================
 */

/*
 * create_shm_file
 * ===============
 * Creates a POSIX shared memory file of the specified size.
 * The file is immediately unlinked so it's cleaned up on close.
 * 
 * Returns: File descriptor on success, -1 on error
 */
static int create_shm_file(size_t size)
{
    char name[32];
    int fd;
    int ret;
    
    /* Create unique name using process ID */
    snprintf(name, sizeof(name), "/wl_shm_ada_%d", getpid());
    
    /* Create shared memory object */
    fd = shm_open(name, O_RDWR | O_CREAT | O_EXCL, 0600);
    if (fd < 0) {
        fprintf(stderr, "shm_open failed: %s\n", strerror(errno));
        return -1;
    }
    
    /* Unlink immediately - file persists until we close it */
    shm_unlink(name);
    
    /* Set the size of the shared memory */
    ret = ftruncate(fd, size);
    if (ret < 0) {
        fprintf(stderr, "ftruncate failed: %s\n", strerror(errno));
        close(fd);
        return -1;
    }
    
    return fd;
}

/*
 * create_buffer
 * =============
 * Creates a Wayland buffer backed by shared memory.
 * 
 * Process:
 * 1. Calculate buffer size (width * height * 4 bytes per pixel)
 * 2. Create shared memory file
 * 3. Map the file into our address space (mmap)
 * 4. Create a Wayland shared memory pool
 * 5. Create a buffer from the pool with ARGB8888 format
 * 
 * Returns: wl_buffer on success, NULL on error
 */
static struct wl_buffer* create_buffer(
    struct wayland_window_context *ctx)
{
    int stride = ctx->width * 4;  /* 4 bytes per pixel (ARGB) */
    int size = stride * ctx->height;
    int fd;
    struct wl_shm_pool *pool;
    struct wl_buffer *buffer;
    
    /* Create shared memory file */
    fd = create_shm_file(size);
    if (fd < 0) {
        fprintf(stderr, "Failed to create shm file\n");
        return NULL;
    }
    
    /* Map shared memory into our address space */
    ctx->shm_data = mmap(
        NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (ctx->shm_data == MAP_FAILED) {
        fprintf(stderr, "mmap failed: %s\n", strerror(errno));
        close(fd);
        return NULL;
    }
    
    /* Create Wayland shared memory pool */
    pool = wl_shm_create_pool(ctx->shm, fd, size);
    
    /* Create buffer from pool with ARGB8888 pixel format */
    buffer = wl_shm_pool_create_buffer(
        pool, 0, ctx->width, ctx->height, stride,
        WL_SHM_FORMAT_ARGB8888);
    
    /* Destroy pool (buffer still valid) */
    wl_shm_pool_destroy(pool);
    
    /* Close fd (memory mapping still valid) */
    close(fd);
    
    return buffer;
}

/*
 * ============================================================
 * Public API Implementation
 * ============================================================
 * These functions are called from Ada code
 * ============================================================
 */

/*
 * wayland_create_window
 * =====================
 * Creates a complete Wayland window with XDG shell decoration.
 * 
 * The initialization sequence is critical:
 * 1. Connect to Wayland display
 * 2. Get registry and bind to required globals
 * 3. Create surface and XDG shell objects
 * 4. Register all event listeners
 * 5. Commit surface to trigger configure event
 * 6. Wait for configure (window can't display until configured)
 * 7. Create pixel buffer
 * 
 * Returns: Allocated context on success, NULL on failure
 */
wayland_window_context* wayland_create_window(
    int width,
    int height,
    const char* title)
{
    struct wayland_window_context *ctx;
    
    /* Allocate context structure */
    ctx = calloc(1, sizeof(*ctx));
    if (!ctx) {
        return NULL;
    }
    
    ctx->width = width;
    ctx->height = height;
    
    /*
     * Step 1: Connect to Wayland display server
     * NULL means use $WAYLAND_DISPLAY environment variable
     */
    ctx->display = wl_display_connect(NULL);
    if (!ctx->display) {
        fprintf(stderr, "Failed to connect to Wayland display\n");
        free(ctx);
        return NULL;
    }
    printf("Connected to Wayland display\n");
    
    /*
     * Step 2: Get registry and discover compositor capabilities
     * The registry_listener callbacks will be called for each global
     */
    ctx->registry = wl_display_get_registry(ctx->display);
    wl_registry_add_listener(ctx->registry, &registry_listener, ctx);
    
    /* Wait for registry events to complete */
    wl_display_roundtrip(ctx->display);
    
    /*
     * Step 3: Verify we got all required interfaces
     * Cannot proceed without compositor, shm, and xdg_wm_base
     */
    if (!ctx->compositor || !ctx->shm || !ctx->xdg_wm_base) {
        fprintf(stderr, "Required Wayland interfaces not available\n");
        wayland_destroy_window(ctx);
        return NULL;
    }
    printf("All required interfaces bound\n");
    
    /*
     * Step 4: Register XDG WM Base listener for ping events
     */
    xdg_wm_base_add_listener(ctx->xdg_wm_base, 
                             &xdg_wm_base_listener, ctx);
    
    /* Create surface */
    ctx->surface = wl_compositor_create_surface(ctx->compositor);
    if (!ctx->surface) {
        fprintf(stderr, "Failed to create surface\n");
        wayland_destroy_window(ctx);
        return NULL;
    }
    printf("Surface created\n");
    
    /* Create xdg_surface */
    ctx->xdg_surface = xdg_wm_base_get_xdg_surface(
        ctx->xdg_wm_base, ctx->surface);
    if (!ctx->xdg_surface) {
        fprintf(stderr, "Failed to create xdg_surface\n");
        wayland_destroy_window(ctx);
        return NULL;
    }
    xdg_surface_add_listener(ctx->xdg_surface, 
                            &xdg_surface_listener, ctx);
    
    /* Create xdg_toplevel (makes it a window) */
    ctx->xdg_toplevel = xdg_surface_get_toplevel(ctx->xdg_surface);
    if (!ctx->xdg_toplevel) {
        fprintf(stderr, "Failed to create xdg_toplevel\n");
        wayland_destroy_window(ctx);
        return NULL;
    }
    xdg_toplevel_add_listener(ctx->xdg_toplevel, 
                             &xdg_toplevel_listener, ctx);
    xdg_toplevel_set_title(ctx->xdg_toplevel, title);
    printf("XDG toplevel created\n");
    
    /* Commit the surface to trigger configure */
    wl_surface_commit(ctx->surface);
    
    /* Wait for configure event */
    ctx->configured = 0;
    while (!ctx->configured && wl_display_dispatch(ctx->display) != -1) {
        /* Wait for xdg_surface configure */
    }
    
    /* Create buffer */
    ctx->buffer = create_buffer(ctx);
    if (!ctx->buffer) {
        fprintf(stderr, "Failed to create buffer\n");
        wayland_destroy_window(ctx);
        return NULL;
    }
    printf("Buffer created\n");
    
    return ctx;
}

/*
 * wayland_paint_gradient
 * ======================
 * Demonstrates how to draw to the buffer and display it.
 * 
 * Process:
 * 1. Write pixels to shared memory (ARGB8888 format)
 * 2. Attach buffer to surface
 * 3. Mark surface as damaged (needs redraw)
 * 4. Commit to send changes to compositor
 */
void wayland_paint_gradient(wayland_window_context* ctx)
{
    uint32_t *pixels = ctx->shm_data;
    int x, y;
    
    if (!pixels) {
        return;
    }
    
    /*
     * Paint gradient pattern
     * Red increases left to right
     * Green increases top to bottom
     * Blue decreases left to right
     * Result: Blue->Red horizontally, dark->bright vertically
     */
    for (y = 0; y < ctx->height; y++) {
        for (x = 0; x < ctx->width; x++) {
            uint32_t r = (x * 255) / ctx->width;        /* 0 to 255 */
            uint32_t g = (y * 255) / ctx->height;       /* 0 to 255 */
            uint32_t b = 255 - ((x * 255) / ctx->width); /* 255 to 0 */
            
            /* ARGB8888 format: 0xAARRGGBB */
            pixels[y * ctx->width + x] = 
                0xFF000000 |        /* Alpha: fully opaque */
                (r << 16) |         /* Red channel */
                (g << 8) |          /* Green channel */
                b;                  /* Blue channel */
        }
    }
    
    /* Attach the buffer to the surface */
    wl_surface_attach(ctx->surface, ctx->buffer, 0, 0);
    
    /* Mark the entire surface as damaged (needs redraw) */
    wl_surface_damage(ctx->surface, 0, 0, ctx->width, ctx->height);
    
    /* Commit to send changes to the compositor */
    wl_surface_commit(ctx->surface);
    
    printf("Gradient painted and surface committed\n");
}

/*
 * wayland_dispatch_event
 * ======================
 * Processes one event from the display event queue.
 * This function blocks until an event arrives.
 * 
 * Should be called in a loop to handle:
 * - Configure events
 * - Input events (if implemented)
 * - Close requests
 * - Compositor pings
 * 
 * Returns: 0 on success, -1 if display connection lost
 */
int wayland_dispatch_event(wayland_window_context* ctx)
{
    if (!ctx || !ctx->display) {
        return -1;
    }
    
    /* Dispatch will call our registered event handlers */
    return wl_display_dispatch(ctx->display);
}

/*
 * wayland_destroy_window
 * ======================
 * Cleans up all resources associated with the window.
 * 
 * IMPORTANT: Objects must be destroyed in reverse order of creation
 * to avoid protocol errors. The cleanup order matters!
 * 
 * 1. Unmap shared memory
 * 2. Destroy buffer
 * 3. Destroy XDG shell objects (toplevel -> surface)
 * 4. Destroy Wayland surface
 * 5. Destroy protocol interfaces
 * 6. Destroy registry
 * 7. Disconnect from display
 * 8. Free context
 */
void wayland_destroy_window(wayland_window_context* ctx)
{
    if (!ctx) {
        return;
    }
    
    /* Unmap shared memory */
    if (ctx->shm_data) {
        int size = ctx->width * ctx->height * 4;
        munmap(ctx->shm_data, size);
    }
    
    /* Destroy buffer */
    if (ctx->buffer) {
        wl_buffer_destroy(ctx->buffer);
    }
    
    /* Destroy XDG shell objects (reverse order) */
    if (ctx->xdg_toplevel) {
        xdg_toplevel_destroy(ctx->xdg_toplevel);
    }
    
    if (ctx->xdg_surface) {
        xdg_surface_destroy(ctx->xdg_surface);
    }
    
    /* Destroy Wayland surface */
    if (ctx->surface) {
        wl_surface_destroy(ctx->surface);
    }
    
    /* Destroy protocol interfaces */
    if (ctx->xdg_wm_base) {
        xdg_wm_base_destroy(ctx->xdg_wm_base);
    }
    
    if (ctx->compositor) {
        wl_compositor_destroy(ctx->compositor);
    }
    
    if (ctx->shm) {
        wl_shm_destroy(ctx->shm);
    }
    
    /* Destroy registry */
    if (ctx->registry) {
        wl_registry_destroy(ctx->registry);
    }
    
    /* Disconnect from display server */
    if (ctx->display) {
        wl_display_disconnect(ctx->display);
    }
    
    /* Free the context structure */
    free(ctx);
    printf("Window destroyed\n");
}
