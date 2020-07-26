/* See LICENSE file for copyright and license details. */
//#define DEBUG
#include <stdlib.h>

/* appearance */
static const unsigned int borderpx = 4; /* border pixel of windows */
static const unsigned int snap = 16;    /* snap pixel */
static const int showbar = 1;           /* 0 means no bar */
static const int topbar = 1;            /* 0 means bottom bar */
static const char *fonts[] = {"monospace:size=10"};
static const char dmenufont[] = "monospace:size=10";
static const char col_gray1[] = "#222222";
static const char col_gray2[] = "#444444";
static const char col_gray3[] = "#bbbbbb";
static const char col_gray4[] = "#eeeeee";
static const char col_main[]  = "#008877";

static const char *colors[][3] = {
    /*               fg         bg         border   */
    [SchemeNorm] = {col_gray3, col_gray1, col_gray2},
    [SchemeSel] = {col_gray4, col_main, col_main},

    [SchemeStatus] = {col_gray3, col_gray1,
                      "#000000"}, // Statusbar right {text,background,not used
                                  // but cannot be empty}
    [SchemeTagsSel] = {col_gray4, col_main,
                       "#000000"}, // Tagbar left selected {text,background,not
                                   // used but cannot be empty}
    [SchemeTagsNorm] =
        {col_gray3, col_gray1,
         "#000000"}, // Tagbar left unselected {text,background,not used but
                     // cannot be empty}
    [SchemeInfoSel] =
        {col_gray4, col_main,
         "#000000"}, // infobar middle  selected {text,background,not used but
                     // cannot be empty}
    [SchemeInfoNorm] =
        {col_gray3, col_gray1,
         "#000000"}, // infobar middle  unselected {text,background,not used but
                     // cannot be empty}
    [SchemeTabActive] = {col_gray2, col_gray3, col_gray2},
    [SchemeTabInactive] = {col_gray2, col_gray3, col_gray2},
};

static const unsigned int gappih = 10; /* horiz inner gap between windows */
static const unsigned int gappiv = 10; /* vert inner gap between windows */
static const unsigned int gappoh =
    10; /* horiz outer gap between windows and screen edge */
static const unsigned int gappov =
    10; /* vert outer gap between windows and screen edge */
static const int smartgaps =
    0; /* 1 means no outer gap when there is only one window */

static const int swallowfloating = 0;

static const char *const autostart[] = {
    "start-dunst", NULL,
    "start-picom", NULL,
    "mpd", NULL,
    "st", NULL,
    "usb-watch", NULL,
    "wal -R", NULL,
    "statusbar", NULL,
    "notify-send", "💻Welcome Dustin!", NULL,
    NULL,
};

/* tagging */
static const char *tags[] = {"1", "2", "3", "4", "5", "6", "7", "8"};

#define DRAWCLASSICTAGS 1 << 0
#define DRAWTAGGRID 1 << 1

#define SWITCHTAG_UP 1 << 0
#define SWITCHTAG_DOWN 1 << 1
#define SWITCHTAG_LEFT 1 << 2
#define SWITCHTAG_RIGHT 1 << 3
#define SWITCHTAG_TOGGLETAG 1 << 4
#define SWITCHTAG_TAG 1 << 5
#define SWITCHTAG_VIEW 1 << 6
#define SWITCHTAG_TOGGLEVIEW 1 << 7

static const unsigned int drawtagmask =
    DRAWTAGGRID; /* | DRAWCLASSICTAGS to show classic row of tags */
static const int tagrows = 2;

static const Rule rules[] = {
    /* xprop(1):
     *	WM_CLASS(STRING) = instance, class
     *	WM_NAME(STRING) = title
     */
    /* class       instance  title         tags mask  isfloating isterminal
       noswallow monitor scratch key*/
    {"Gimp", NULL, NULL, 0, 1, 0, 0, -1, 0},
    {"Firefox", NULL, NULL, 1 << 8, 0, 0, 0, -1, 0},
    {"DevWindow", NULL, NULL, 0, 1, 0, 1, -1, 0},
    {"St", NULL, NULL, 0, 0, 1, 0, -1, 0},
    {NULL, NULL, "scratchpad", 0, 1, 0, 0, -1, 's'},
};

/* layout(s) */
static const float mfact = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster = 1;    /* number of clients in master area */
static const int resizehints =
    1; /* 1 means respect size hints in tiled resizals */

/* bartabgroup properties */
#define BARTAB_BORDERS 1
#define BARTAB_BOTTOMBORDER 1
#define BARTAB_TAGSINDICATOR 1
#define BARTAB_TAGSPX 5
#define BARTAB_TAGSROWS 3

static void (*bartabmonfns[])(Monitor *) = {monocle};
static void (*bartabfloatfns[])(Monitor *) = {NULL};

static const Layout layouts[] = {
    /* symbol     arrange function */
    {" λ Tile           ", tile},
    {" α Float          ", NULL},
    {" π Stack          ", monocle},
    {" φ Centered       ", centeredmaster},
    {" ψ Centered Float ", centeredfloatingmaster},
    {NULL, NULL},
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY, TAG)                                                      \
  {MODKEY, KEY, view, {.ui = 1 << TAG}},                                       \
      {MODKEY | ControlMask, KEY, toggleview, {.ui = 1 << TAG}},               \
      {MODKEY | ShiftMask, KEY, tag, {.ui = 1 << TAG}},                        \
      {MODKEY | ControlMask | ShiftMask, KEY, toggletag, {.ui = 1 << TAG}},

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd)                                                             \
  {                                                                            \
    .v = (const char *[]) { "/bin/sh", "-c", cmd, NULL }                       \
  }

/* commands */
static char dmenumon[2] =
    "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = {
    "dmenu_run", "-m",      dmenumon, "-fn",    dmenufont, "-nb",     col_gray1,
    "-nf",       col_gray3, "-sb", "#000000", "-sf",     col_gray4, NULL};
static const char *termcmd[] = {"st", NULL};
static const char *dmenuemojicmd[] = {"dmenu-emoji", NULL};

static const char *scratchpadcmd[] = {"s", "st", "-t", "scratchpad", NULL};

#define VOLUME_STEP "5"

static const char *volume_up_cmd[] = {"vol", "set", VOLUME_STEP "%+", NULL};
static const char *volume_down_cmd[] = {"vol", "set", VOLUME_STEP "%-", NULL};

static const char *spawn_browser[] = {"qutebrowser"};
static const char *spawn_browser_with_bookmark[] = { "dmenu-bookmark", "|", "qutebrowser"};

static Key keys[] = {
    /* modifier                     key        function        argument */
    {MODKEY, XK_p, spawn, {.v = dmenucmd}},
    {MODKEY | ShiftMask, XK_Return, spawn, {.v = termcmd}},
    {MODKEY, XK_e, spawn, {.v = dmenuemojicmd}},
    {MODKEY, XK_b, togglebar, {0}},
    {MODKEY, XK_j, focusstack, {.i = +1}},
    {MODKEY, XK_k, focusstack, {.i = -1}},
    {MODKEY, XK_i, incnmaster, {.i = +1}},
    {MODKEY, XK_d, incnmaster, {.i = -1}},
    {MODKEY, XK_h, setmfact, {.f = -0.05}},
    {MODKEY, XK_l, setmfact, {.f = +0.05}},
    {MODKEY, XK_Return, zoom, {0}},
    {MODKEY, XK_Tab, view, {0}},
    {MODKEY, XK_q, killclient, {0}},
    {MODKEY, XK_t, setlayout, {.v = &layouts[0]}},
    {MODKEY, XK_f, setlayout, {.v = &layouts[1]}},
    {MODKEY, XK_m, setlayout, {.v = &layouts[2]}},
    {MODKEY, XK_space, setlayout, {0}},
    {MODKEY | ShiftMask, XK_space, togglefloating, {0}},

    {MODKEY, XK_Escape, togglescratch, {.v = scratchpadcmd}},

    {MODKEY | ControlMask, XK_comma, cyclelayout, {.i = -1}},
    {MODKEY | ControlMask, XK_period, cyclelayout, {.i = +1}},

    {MODKEY, XK_minus, spawn, {.v = volume_down_cmd}},
    {MODKEY, XK_equal, spawn, {.v = volume_up_cmd}},

    {MODKEY | Mod1Mask, XK_h, incrgaps, {.i = +1}},
    {MODKEY | Mod1Mask, XK_l, incrgaps, {.i = -1}},
    {MODKEY | Mod1Mask | ShiftMask, XK_h, incrogaps, {.i = +1}},
    {MODKEY | Mod1Mask | ShiftMask, XK_l, incrogaps, {.i = -1}},
    {MODKEY | Mod1Mask | ControlMask, XK_h, incrigaps, {.i = +1}},
    {MODKEY | Mod1Mask | ControlMask, XK_l, incrigaps, {.i = -1}},
    {MODKEY | Mod1Mask, XK_0, togglegaps, {0}},
    {MODKEY | Mod1Mask | ShiftMask, XK_0, defaultgaps, {0}},
    {MODKEY, XK_y, incrihgaps, {.i = +1}},
    {MODKEY, XK_o, incrihgaps, {.i = -1}},
    {MODKEY | ControlMask, XK_y, incrivgaps, {.i = +1}},
    {MODKEY | ControlMask, XK_o, incrivgaps, {.i = -1}},
    {MODKEY | Mod1Mask, XK_y, incrohgaps, {.i = +1}},
    {MODKEY | Mod1Mask, XK_o, incrohgaps, {.i = -1}},
    {MODKEY | ShiftMask, XK_y, incrovgaps, {.i = +1}},
    {MODKEY | ShiftMask, XK_o, incrovgaps, {.i = -1}},

    {MODKEY, XK_0, view, {.ui = ~0}},
    {MODKEY | ShiftMask, XK_0, tag, {.ui = ~0}},
    {MODKEY, XK_comma, focusmon, {.i = -1}},
    {MODKEY, XK_period, focusmon, {.i = +1}},
    {MODKEY | ShiftMask, XK_comma, tagmon, {.i = -1}},
    {MODKEY | ShiftMask, XK_period, tagmon, {.i = +1}},
    TAGKEYS(XK_1, 0) TAGKEYS(XK_2, 1) TAGKEYS(XK_3, 2) TAGKEYS(XK_4, 3)
        TAGKEYS(XK_5, 4) TAGKEYS(XK_6, 5) TAGKEYS(XK_7, 6) TAGKEYS(XK_8, 7)
            TAGKEYS(XK_9, 8)

                {MODKEY | ShiftMask | ControlMask | Mod1Mask, XK_q, quit, {0}},
    {MODKEY | ShiftMask, XK_q, quit, {1}},

    {MODKEY | ControlMask,
     XK_Up,
     switchtag,
     {.ui = SWITCHTAG_UP | SWITCHTAG_VIEW}},
    {MODKEY | ControlMask,
     XK_Down,
     switchtag,
     {.ui = SWITCHTAG_DOWN | SWITCHTAG_VIEW}},
    {MODKEY | ControlMask,
     XK_Right,
     switchtag,
     {.ui = SWITCHTAG_RIGHT | SWITCHTAG_VIEW}},
    {MODKEY | ControlMask,
     XK_Left,
     switchtag,
     {.ui = SWITCHTAG_LEFT | SWITCHTAG_VIEW}},

    {MODKEY | Mod1Mask,
     XK_Up,
     switchtag,
     {.ui = SWITCHTAG_UP | SWITCHTAG_TAG | SWITCHTAG_VIEW}},
    {MODKEY | Mod1Mask,
     XK_Down,
     switchtag,
     {.ui = SWITCHTAG_DOWN | SWITCHTAG_TAG | SWITCHTAG_VIEW}},
    {MODKEY | Mod1Mask,
     XK_Right,
     switchtag,
     {.ui = SWITCHTAG_RIGHT | SWITCHTAG_TAG | SWITCHTAG_VIEW}},
    {MODKEY | Mod1Mask,
     XK_Left,
     switchtag,
     {.ui = SWITCHTAG_LEFT | SWITCHTAG_TAG | SWITCHTAG_VIEW}},

    /* Spawn commands */

    {MODKEY | ControlMask, XK_b, spawn, {.v = spawn_browser}},
    {MODKEY | ControlMask | ShiftMask, XK_b, spawn, {.v = spawn_browser_with_bookmark}},
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle,
 * ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click                event mask      button          function argument
     */
    {ClkLtSymbol, 0, Button1, setlayout, {0}},
    {ClkLtSymbol, 0, Button3, setlayout, {.v = &layouts[2]}},
    {ClkWinTitle, 0, Button2, zoom, {0}},
    {ClkStatusText, 0, Button2, spawn, {.v = termcmd}},
    {ClkClientWin, MODKEY, Button1, movemouse, {0}},
    {ClkClientWin, MODKEY, Button2, togglefloating, {0}},
    {ClkClientWin, MODKEY, Button3, resizemouse, {0}},
    {ClkTagBar, 0, Button1, view, {0}},
    {ClkTagBar, 0, Button3, toggleview, {0}},
    {ClkTagBar, MODKEY, Button1, tag, {0}},
    {ClkTagBar, MODKEY, Button3, toggletag, {0}},
};
