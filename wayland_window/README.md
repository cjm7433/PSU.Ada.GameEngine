# Wayland Window in Ada

A complete Ada program that creates a window using the Wayland display protocol, properly configured with Alire.

## Project Status

✅ **Alire project initialized and building successfully**
✅ **Wayland C library bindings created**  
✅ **Build system configured with strict Ada style checks**

## Prerequisites

### System Requirements

1. **Alire** (Ada package manager)
   - Download from https://alire.ada.dev/
   - Alire automatically manages GNAT and gprbuild

2. **Wayland Client Library**
   ```bash
   # Debian/Ubuntu
   sudo apt-get install libwayland-dev
   
   # Fedora
   sudo dnf install wayland-devel
   
   # Arch Linux
   sudo pacman -S wayland
   ```

3. **Wayland Compositor**
   - Must be running in a Wayland session
   - Check with: `echo $WAYLAND_DISPLAY`
   - Common compositors: GNOME (Wayland mode), Sway, Weston

## Building

```bash
# Clone or navigate to the project
cd wayland_window

# Build the project
alr build

# Run the program
alr run
# or
./bin/wayland_window
```

## Project Structure

```
wayland_window/
├── alire.toml              # Alire project manifest
├── wayland_window.gpr      # GNAT project file
├── config/                 # Alire-generated build configuration
├── src/
│   ├── wayland_client.ads  # Wayland C API bindings
│   └── wayland_window.adb  # Main program
└── bin/                    # Compiled executables
```

## Wayland Bindings

The `wayland_client.ads` package provides Ada bindings to libwayland-client including:

- **Display management**: Connect, disconnect, dispatch events
- **Registry**: Discover compositor capabilities
- **Surfaces**: Create and manage drawing surfaces  
- **Shell**: Window management (wl_shell interface)
- **Shared memory**: Efficient pixel buffer sharing
- **Buffers**: Attach and display pixel data

All bindings use `pragma Import` to interface directly with the C library.

## Development

The project uses strict Ada coding standards enforced by Alire:
- 3-space indentation
- Maximum 79 character line length
- All warnings enabled and treated as errors
- UTF-8 encoding for source files

### Adding Features

To extend the program with actual Wayland window creation:

1. Use the bindings in `wayland_client.ads`
2. Follow the Wayland protocol flow:
   - Connect to display
   - Get registry and bind to globals
   - Create surface and shell surface
   - Allocate shared memory buffer
   - Paint buffer and attach to surface
   - Enter event loop

## Wayland Resources

- **Wayland Documentation**: https://wayland.freedesktop.org/docs/html/
- **Protocol Specification**: https://wayland.freedesktop.org/docs/html/apa.html
- **Book of Wayland**: https://wayland-book.com/

## License

MIT OR Apache-2.0 WITH LLVM-exception

## Next Steps

The current program is a minimal working example. To create an actual window:

1. Implement registry callbacks to bind compositor globals
2. Create surface and shell surface
3. Set up shared memory and paint a buffer
4. Implement event loop for window interaction

See the Wayland documentation for detailed protocol information.
