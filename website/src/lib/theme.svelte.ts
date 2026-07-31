/// Theme preference: 'system' follows the OS, 'light'/'dark' pin it.
/// The resolved value is mirrored onto <html data-theme> — app.css only ever
/// sees a concrete 'light' or 'dark', so it needs no prefers-color-scheme rule.
export type ThemePref = 'system' | 'light' | 'dark'
export type Theme = 'light' | 'dark'

export const THEME_KEY = 'disp-theme'

const DARK_QUERY = '(prefers-color-scheme: dark)'

const isPref = (v: unknown): v is ThemePref =>
  v === 'system' || v === 'light' || v === 'dark'

/// Reads the stored preference. Private-mode Safari throws on localStorage.
function storedPref(): ThemePref {
  try {
    const v = localStorage.getItem(THEME_KEY)
    return isPref(v) ? v : 'system'
  } catch {
    return 'system'
  }
}

function systemTheme(): Theme {
  return typeof matchMedia === 'function' && matchMedia(DARK_QUERY).matches
    ? 'dark'
    : 'light'
}

const resolve = (p: ThemePref): Theme => (p === 'system' ? systemTheme() : p)

class ThemeStore {
  /// SSR prerenders the light palette; the app.html head script fixes the DOM
  /// before first paint, and `sync()` re-reads storage once mounted.
  pref = $state<ThemePref>('system')
  system = $state<Theme>('light')

  get resolved(): Theme {
    return this.pref === 'system' ? this.system : this.pref
  }

  /// Adopt the real preference after hydration and follow later OS changes.
  sync() {
    this.pref = storedPref()
    this.system = systemTheme()
    this.apply()

    if (typeof matchMedia !== 'function') return
    const mq = matchMedia(DARK_QUERY)
    const onChange = (e: MediaQueryListEvent) => {
      this.system = e.matches ? 'dark' : 'light'
      this.apply()
    }
    mq.addEventListener('change', onChange)
    return () => mq.removeEventListener('change', onChange)
  }

  set(pref: ThemePref) {
    this.pref = pref
    try {
      if (pref === 'system') localStorage.removeItem(THEME_KEY)
      else localStorage.setItem(THEME_KEY, pref)
    } catch {
      // no persistence available; the in-memory preference still applies
    }
    this.apply()
  }

  /// light → dark → system → light. Starting from 'system', step to whichever
  /// explicit theme is NOT showing, so the first click always changes something.
  cycle() {
    if (this.pref === 'system') this.set(this.system === 'dark' ? 'light' : 'dark')
    else if (this.pref === 'dark') this.set('system')
    else this.set('dark')
  }

  private apply() {
    if (typeof document === 'undefined') return
    document.documentElement.dataset.theme = this.resolved
  }
}

export const theme = new ThemeStore()
