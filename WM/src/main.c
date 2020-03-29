#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#include <memory.h>

#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xproto.h>
#include <X11/Xutil.h>
#include <X11/extensions/Xinerama.h>

typedef struct {
    int x, y, w, h;
} Rect;

typedef struct {

} Client;

struct Monitor;

typedef struct {
    const char* symbol;
    void (*arrange)(struct Monitor*);
} Layout;

typedef struct Monitor {
    char layout_symbol[16];

    float factor; // mfact

    int num_master;
    int number;
    int bar_y;

    Rect region;
    Rect window_region;

    unsigned int selected_tags;
    unsigned int selected_symbol; // sellt

    bool showbar;
    bool topbar;

    Client *clients;
    Client *sel;
    Client *stack;

    struct Monitor* next;

    Window barwindow;

    const Layout *layout[2];
} Monitor;

typedef struct {
    Display *display;
    Monitor *monitors, *selected_monitor;

    int (*x_error_lib)(Display*, XErrorEvent*);
} Context;

void die(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fprintf(stdout, fmt, args);
    va_end(args);

    exit(EXIT_FAILURE);
}

int xErrorStart(Display* d, XErrorEvent* ee) {
    die("Another window manager is already running");
    return -1;
}
               
int xErrorHandler(Display* d, XErrorEvent* ee) {
    if (ee->error_code == BadWindow
    || (ee->request_code == X_SetInputFocus && ee->error_code == BadMatch)
    || (ee->request_code == X_PolyText8 && ee->error_code == BadDrawable)
    || (ee->request_code == X_PolyFillRectangle && ee->error_code == BadDrawable)
    || (ee->request_code == X_PolySegment && ee->error_code == BadDrawable)
    || (ee->request_code == X_ConfigureWindow && ee->error_code == BadMatch)
    || (ee->request_code == X_GrabButton && ee->error_code == BadAccess)
    || (ee->request_code == X_GrabKey && ee->error_code == BadAccess)
    || (ee->request_code == X_CopyArea && ee->error_code == BadDrawable))
            return 0;
    die("Fatal error: request code [%d], error code [%d]\n", ee->request_code, ee->error_code);
    return -1;
}

void checkForOtherWM(Context* ctx) {
    ctx->x_error_lib = XSetErrorHandler(xErrorStart);

    // Create an error that accures when another window manager is running
    XSelectInput(ctx->display, DefaultRootWindow(ctx->display), SubstructureRedirectMask);
    XSync(ctx->display, False);
    XSetErrorHandler(xErrorHandler);
    XSync(ctx->display, False);
}

bool isUniqueGeom(XineramaScreenInfo* unique, size_t n, XineramaScreenInfo* info) {
    while (n--) {
        if (unique[n].x_org == info->x_org && unique[n].y_org == info->y_org
            && unique[n].width == info->width && unique[n].height == info->height)
            return true;
    }
    return false;
}

Monitor* createMonitor() { }

int updateMonitorGeometry(Context* ctx) {
    bool dirty = false;

    if (XineramaIsActive(ctx->display)) {
        int monitor_number = 0;
        XineramaScreenInfo* info = XineramaQueryScreens(ctx->display, &monitor_number);

        Monitor* m;
        int n;
        for (n = 0, m = ctx->monitors; m; m = m->next, n++);

        XineramaScreenInfo* unique =
            calloc(monitor_number, sizeof(XineramaScreenInfo));

        if (!unique) die("");

        int j, i;
        for (i = 0, j = 0; i < monitor_number; i++)
            if (isUniqueGeom(unique, j, &info[i])) {
                memcpy(&unique[j++], &info[i], sizeof(XineramaScreenInfo));
            }
        XFree(info);

        monitor_number = j; // set the new monitor number

        // Check for new monitors
        if (n < monitor_number) {
            for (i = 0; i < (monitor_number - n); i++) {
                for (m = ctx->monitors; m && m->next; m = m->next);

                if (m) m->next = createMonitor();
                else ctx->monitors = createMonitor();
            }

            for (i = 0, m = ctx->monitors;
                 i < monitor_number && m;
                 m = m->next, i++) {

                if (i >= n
                    || unique[i].x_org != m->region.x
                    || unique[i].y_org != m->region.y
                    || unique[i].width != m->region.w
                    || unique[i].height != m->region.h) {

                    dirty = true;
                    m->number = 1;
                    m->region.x = m->window_region.x = unique[i].x_org;
                }
            }
        }
    }
}

int main(const int argc, const char* argv[]) {
    Context context = (Context) {
        .display = XOpenDisplay(NULL),
        .monitors = NULL,
        .selected_monitor = NULL,
    };

    if (context.display == NULL)
        die("Could not open the display");

    checkForOtherWM(&context);

    printf("Starting WM\n");

    return 0;
}
