/*
 * ============================================================
 * Wayland Wrapper - Simple C interface for Wayland operations
 * ============================================================
 * This wrapper simplifies the Wayland protocol for use from Ada.
 * It provides a high-level API for creating windows and handling
 * the complex XDG shell protocol internally.
 *
 * The XDG Shell protocol is the modern standard for creating
 * desktop windows in Wayland. This file includes the essential
 * protocol definitions that were previously in separate
 * xdg-shell-client-protocol.h file.
 * ============================================================
 */

#ifndef WAYLAND_WRAPPER_H
#define WAYLAND_WRAPPER_H

#include <stdint.h>
#include <wayland-client.h>

/*
 * ============================================================
 * XDG Shell Protocol Definitions
 * ============================================================
 * These are the essential parts consolidated from the
 * xdg-shell-client-protocol.h generated file. The XDG Shell
 * protocol provides window management for Wayland compositors.
 *
 * Key concepts:
 * - xdg_wm_base: The window manager base interface
 * - xdg_surface: Extends wl_surface with desktop semantics
 * - xdg_toplevel: Makes a surface behave as a top-level window
 * - xdg_popup: For popup menus (not used in this wrapper)
 * ============================================================
 */

#ifdef  __cplusplus
extern "C" {
#endif

/* 
 * Forward declarations for XDG Shell types
 * These are opaque types handled internally by the protocol
 */
struct xdg_wm_base;      /* Window manager base interface */
struct xdg_positioner;   /* Popup positioning (unused here) */
struct xdg_surface;      /* Desktop surface with window semantics */
struct xdg_toplevel;     /* Top-level window (regular application window) */
struct xdg_popup;        /* Popup menu surface (unused here) */

/*
 * Interface declarations - defined in the .c file
 * Each wl_interface describes a protocol object to the Wayland runtime
 */
extern const struct wl_interface xdg_wm_base_interface;
extern const struct wl_interface xdg_positioner_interface;
extern const struct wl_interface xdg_surface_interface;
extern const struct wl_interface xdg_toplevel_interface;
extern const struct wl_interface xdg_popup_interface;

/*
 * ============================================================
 * XDG WM Base - Window Manager Base Interface
 * ============================================================
 * This is the entry point to the XDG shell protocol.
 * The compositor sends "ping" events to check if the client
 * is responsive. We must respond with "pong".
 * ============================================================
 */

struct xdg_wm_base_listener {
    /*
     * ping - compositor is checking if we're alive
     * We must respond with xdg_wm_base_pong() using the same serial
     */
    void (*ping)(void *data, struct xdg_wm_base *xdg_wm_base, uint32_t serial);
};

/*
 * Register event listener for xdg_wm_base
 * Must be called to receive ping events from the compositor
 */
static inline int
xdg_wm_base_add_listener(struct xdg_wm_base *xdg_wm_base,
                         const struct xdg_wm_base_listener *listener, void *data)
{
    return wl_proxy_add_listener((struct wl_proxy *) xdg_wm_base,
                                 (void (**)(void)) listener, data);
}

/*
 * Destroy the xdg_wm_base object
 * Should be called during cleanup
 */
static inline void
xdg_wm_base_destroy(struct xdg_wm_base *xdg_wm_base)
{
    wl_proxy_marshal((struct wl_proxy *) xdg_wm_base, 0);
    wl_proxy_destroy((struct wl_proxy *) xdg_wm_base);
}

/*
 * Create an xdg_surface from a wl_surface
 * This gives the surface desktop window semantics
 * The number 2 is the opcode for the "get_xdg_surface" request
 */
static inline struct xdg_surface *
xdg_wm_base_get_xdg_surface(struct xdg_wm_base *xdg_wm_base, struct wl_surface *surface)
{
    struct wl_proxy *id;
    id = wl_proxy_marshal_constructor((struct wl_proxy *) xdg_wm_base,
                                      2, &xdg_surface_interface, NULL, surface);
    return (struct xdg_surface *) id;
}

/*
 * Respond to a ping event with a pong
 * Must use the same serial number that was received in the ping
 * The number 3 is the opcode for the "pong" request
 */
static inline void
xdg_wm_base_pong(struct xdg_wm_base *xdg_wm_base, uint32_t serial)
{
    wl_proxy_marshal((struct wl_proxy *) xdg_wm_base, 3, serial);
}

/*
 * ============================================================
 * XDG Surface - Desktop Surface Interface
 * ============================================================
 * An xdg_surface extends a wl_surface with desktop-specific
 * functionality. It must be configured before the first commit.
 * The compositor sends "configure" events that we must acknowledge.
 * ============================================================
 */
extern const struct wl_interface xdg_surface_interface;

struct xdg_surface_listener {
    /*
     * configure - compositor has configured the surface
     * We MUST acknowledge this with xdg_surface_ack_configure()
     * using the same serial before the surface can be used
     */
    void (*configure)(void *data, struct xdg_surface *xdg_surface, uint32_t serial);
};

/*
 * Register event listener for xdg_surface
 * Must be called to receive configure events
 */
static inline int
xdg_surface_add_listener(struct xdg_surface *xdg_surface,
                        const struct xdg_surface_listener *listener, void *data)
{
    return wl_proxy_add_listener((struct wl_proxy *) xdg_surface,
                                 (void (**)(void)) listener, data);
}

/*
 * Destroy the xdg_surface object
 * Should be called during cleanup before destroying the wl_surface
 */
static inline void
xdg_surface_destroy(struct xdg_surface *xdg_surface)
{
    wl_proxy_marshal((struct wl_proxy *) xdg_surface, 0);
    wl_proxy_destroy((struct wl_proxy *) xdg_surface);
}

/*
 * Assign the toplevel role to this xdg_surface
 * This makes it a regular application window (not a popup)
 * The number 1 is the opcode for "get_toplevel"
 */
static inline struct xdg_toplevel *
xdg_surface_get_toplevel(struct xdg_surface *xdg_surface)
{
    struct wl_proxy *id;
    id = wl_proxy_marshal_constructor((struct wl_proxy *) xdg_surface,
                                      1, &xdg_toplevel_interface, NULL);
    return (struct xdg_toplevel *) id;
}

/*
 * Acknowledge a configure event
 * CRITICAL: Must be called in response to every configure event
 * before the surface can display content. Use the same serial.
 * The number 4 is the opcode for "ack_configure"
 */
static inline void
xdg_surface_ack_configure(struct xdg_surface *xdg_surface, uint32_t serial)
{
    wl_proxy_marshal((struct wl_proxy *) xdg_surface, 4, serial);
}

/*
 * ============================================================
 * XDG Toplevel - Top-Level Window Interface
 * ============================================================
 * An xdg_toplevel represents a regular application window.
 * It receives configure events when the compositor wants to
 * resize the window, and close events when the user closes it.
 * ============================================================
 */
extern const struct wl_interface xdg_toplevel_interface;

struct xdg_toplevel_listener {
    /*
     * configure - compositor suggests a new size or state
     * width/height are suggestions (can be 0 if no preference)
     * states is an array of window states (maximized, fullscreen, etc.)
     */
    void (*configure)(void *data, struct xdg_toplevel *xdg_toplevel,
                     int32_t width, int32_t height, struct wl_array *states);
    
    /*
     * close - user requested the window to close
     * Application should perform cleanup and exit gracefully
     */
    void (*close)(void *data, struct xdg_toplevel *xdg_toplevel);
};

/*
 * Register event listener for xdg_toplevel
 * Must be called to receive configure and close events
 */
static inline int
xdg_toplevel_add_listener(struct xdg_toplevel *xdg_toplevel,
                         const struct xdg_toplevel_listener *listener, void *data)
{
    return wl_proxy_add_listener((struct wl_proxy *) xdg_toplevel,
                                 (void (**)(void)) listener, data);
}

/*
 * Destroy the xdg_toplevel object
 * Should be called during cleanup before destroying the xdg_surface
 */
static inline void
xdg_toplevel_destroy(struct xdg_toplevel *xdg_toplevel)
{
    wl_proxy_marshal((struct wl_proxy *) xdg_toplevel, 0);
    wl_proxy_destroy((struct wl_proxy *) xdg_toplevel);
}

/*
 * Set the window title
 * This appears in the window's title bar and task switcher
 * The number 2 is the opcode for "set_title"
 */
static inline void
xdg_toplevel_set_title(struct xdg_toplevel *xdg_toplevel, const char *title)
{
    wl_proxy_marshal((struct wl_proxy *) xdg_toplevel, 2, title);
}

#ifdef  __cplusplus
}
#endif

/*
 * ============================================================
 * High-Level Wayland Window API
 * ============================================================
 * These functions provide a simplified interface for Ada code
 * to create and manage Wayland windows. All the XDG shell
 * protocol complexity is handled internally.
 * ============================================================
 */

/* Opaque handle to our window context - hides implementation details */
typedef struct wayland_window_context wayland_window_context;

/*
 * wayland_create_window
 * =====================
 * Creates and displays a Wayland window with the specified dimensions.
 * 
 * This function handles:
 * - Connecting to the Wayland display server
 * - Binding to required protocol interfaces (compositor, shm, xdg_wm_base)
 * - Creating the surface and XDG shell objects
 * - Registering event listeners
 * - Waiting for the initial configure event
 * - Allocating shared memory for the window buffer
 * 
 * Parameters:
 *   width  - Window width in pixels
 *   height - Window height in pixels
 *   title  - Window title (UTF-8 string)
 * 
 * Returns: Pointer to window context on success, NULL on failure
 */
wayland_window_context* wayland_create_window(
    int width,
    int height,
    const char* title);

/*
 * wayland_paint_gradient
 * ======================
 * Paints the window buffer with a colorful gradient pattern.
 * The gradient transitions from blue to red horizontally,
 * and from dark to bright vertically.
 * 
 * This is a demonstration function that shows how to:
 * - Write pixels to the shared memory buffer
 * - Attach the buffer to the surface
 * - Mark the surface as damaged (needs redraw)
 * - Commit the surface to display changes
 * 
 * Parameters:
 *   ctx - Window context from wayland_create_window()
 */
void wayland_paint_gradient(wayland_window_context* ctx);

/*
 * wayland_dispatch_event
 * ======================
 * Processes one event from the Wayland display event queue.
 * This function blocks until an event is available.
 * 
 * Should be called in a loop to handle:
 * - Configure events (window resize requests)
 * - Close events (user closing the window)
 * - Input events (keyboard, mouse - if implemented)
 * - Display sync events
 * 
 * Parameters:
 *   ctx - Window context from wayland_create_window()
 * 
 * Returns: 0 on success, -1 on error (display disconnected)
 */
int wayland_dispatch_event(wayland_window_context* ctx);

/*
 * wayland_destroy_window
 * ======================
 * Destroys the window and frees all associated resources.
 * 
 * This function handles:
 * - Unmapping shared memory
 * - Destroying XDG shell objects (toplevel, surface)
 * - Destroying Wayland objects (surface, buffer, shm, compositor)
 * - Disconnecting from the display
 * - Freeing the context structure
 * 
 * Parameters:
 *   ctx - Window context to destroy (can be NULL)
 */
void wayland_destroy_window(wayland_window_context* ctx);

#endif /* WAYLAND_WRAPPER_H */
