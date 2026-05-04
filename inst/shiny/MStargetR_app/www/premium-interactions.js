// MStargetR Shiny Application - Premium Interactions Layer
// =============================================================================
// Additive enhancement file. Does NOT modify app.js. Loaded alongside it.
// =============================================================================

(function () {
  'use strict';

  // ==========================================================================
  // Helpers & Shared State
  // ==========================================================================
  var prefersReducedMotion = window.matchMedia(
    '(prefers-reduced-motion: reduce)',
  );

  function reducedMotion() {
    return prefersReducedMotion.matches;
  }

  function announceToSR(msg) {
    var sr = document.getElementById('mst-sr-announcer');
    if (sr) {
      sr.textContent = '';
      setTimeout(function () {
        sr.textContent = msg;
      }, 50);
    }
  }

  function safeSessionGet(key) {
    try {
      return sessionStorage.getItem(key);
    } catch (e) {
      return null;
    }
  }

  function safeSessionSet(key, val) {
    try {
      sessionStorage.setItem(key, val);
    } catch (e) {
      /* noop */
    }
  }

  // Easing: easeOutExpo
  function easeOutExpo(t) {
    return t === 1 ? 1 : 1 - Math.pow(2, -10 * t);
  }

  // ==========================================================================
  // Inject Premium Styles
  // ==========================================================================
  function injectPremiumStyles() {
    var css = [
      // --- 1. Scroll Progress Bar ---
      '#mst-scroll-progress{position:fixed;top:0;left:0;height:2px;z-index:10002;' +
        'background:var(--mst-gradient-brand,linear-gradient(90deg,#0d9488,#1e40af));' +
        'width:0;pointer-events:none;transition:none;}',

      // --- 2. Mouse-following gradient on cards ---
      '.mst-card-glow{position:absolute;top:0;left:0;width:100%;height:100%;' +
        'pointer-events:none;border-radius:inherit;opacity:0;' +
        'transition:opacity 0.3s ease;z-index:0;}',

      // --- 3. Tab sliding pill ---
      '#mst-tab-pill{position:absolute;height:100%;border-radius:6px;' +
        'background:var(--mst-accent,#0d9488);opacity:0.12;' +
        'transition:left 0.35s cubic-bezier(0.4,0,0.2,1),width 0.35s cubic-bezier(0.4,0,0.2,1);' +
        'pointer-events:none;z-index:0;}',

      // --- 4. Plot fullscreen ---
      '.mst-plot-fs-btn{position:absolute;top:6px;right:6px;z-index:5;width:28px;height:28px;' +
        'border:1px solid var(--mst-border,#e2e8f0);border-radius:6px;' +
        'background:var(--mst-surface,#fff);color:var(--mst-text-muted,#64748b);' +
        'cursor:pointer;display:flex;align-items:center;justify-content:center;' +
        'opacity:0;transition:opacity 0.2s ease;font-size:14px;line-height:1;}',
      '.plotly:hover .mst-plot-fs-btn,.js-plotly-plot:hover .mst-plot-fs-btn{opacity:1;}',
      '.mst-plot-fullscreen{position:fixed!important;top:0!important;left:0!important;' +
        'width:100vw!important;height:100vh!important;z-index:10004!important;' +
        'background:var(--mst-bg,#f8fafc)!important;padding:20px!important;' +
        'box-sizing:border-box!important;}',
      '.mst-fs-backdrop{position:fixed;top:0;left:0;width:100%;height:100%;' +
        'background:rgba(0,0,0,0.6);z-index:10003;opacity:0;' +
        'transition:opacity 0.3s ease;pointer-events:auto;}',
      '.mst-fs-backdrop.show{opacity:1;}',

      // --- 5. Command Palette ---
      '#mst-cmd-overlay{position:fixed;top:0;left:0;width:100%;height:100%;' +
        'z-index:10005;display:none;align-items:flex-start;justify-content:center;' +
        'padding-top:min(20vh,160px);background:rgba(15,23,42,0.5);' +
        'backdrop-filter:blur(8px);-webkit-backdrop-filter:blur(8px);}',
      '#mst-cmd-overlay.open{display:flex;}',
      '#mst-cmd-box{width:min(520px,90vw);max-height:420px;' +
        'background:var(--mst-surface-elevated,#fff);border:1px solid var(--mst-border,#e2e8f0);' +
        'border-radius:var(--mst-radius-lg,14px);box-shadow:var(--mst-shadow-xl);' +
        'overflow:hidden;display:flex;flex-direction:column;}',
      '#mst-cmd-input{width:100%;padding:14px 18px;border:none;outline:none;' +
        'font-size:15px;background:transparent;color:var(--mst-text,#0f172a);' +
        'border-bottom:1px solid var(--mst-border,#e2e8f0);}',
      '#mst-cmd-input::placeholder{color:var(--mst-text-muted,#64748b);}',
      '#mst-cmd-list{list-style:none;margin:0;padding:6px;overflow-y:auto;flex:1;}',
      '#mst-cmd-list li{padding:10px 14px;border-radius:8px;cursor:pointer;' +
        'display:flex;align-items:center;gap:10px;font-size:14px;' +
        'color:var(--mst-text-secondary,#334155);}',
      '#mst-cmd-list li.active{background:var(--mst-surface-hover,#f1f5f9);}',
      '#mst-cmd-list li .cmd-icon{width:20px;text-align:center;flex-shrink:0;' +
        'color:var(--mst-text-muted,#64748b);}',
      '#mst-cmd-list li .cmd-shortcut{margin-left:auto;font-size:11px;' +
        'color:var(--mst-text-muted,#64748b);font-family:var(--mst-font-mono,monospace);}',

      // --- 6. Keyboard Shortcut Overlay ---
      '#mst-shortcut-overlay{position:fixed;top:0;left:0;width:100%;height:100%;' +
        'z-index:10006;display:none;align-items:center;justify-content:center;' +
        'background:rgba(15,23,42,0.5);backdrop-filter:blur(8px);' +
        '-webkit-backdrop-filter:blur(8px);}',
      '#mst-shortcut-overlay.open{display:flex;}',
      '#mst-shortcut-modal{width:min(500px,90vw);max-height:80vh;' +
        'background:var(--mst-surface-elevated,#fff);border:1px solid var(--mst-border,#e2e8f0);' +
        'border-radius:var(--mst-radius-lg,14px);box-shadow:var(--mst-shadow-xl);' +
        'overflow-y:auto;padding:24px 28px;}',
      '#mst-shortcut-modal h3{margin:0 0 16px 0;font-size:18px;font-weight:600;' +
        'color:var(--mst-text,#0f172a);}',
      '#mst-shortcut-modal table{width:100%;border-collapse:collapse;font-size:14px;}',
      '#mst-shortcut-modal td{padding:8px 4px;border-bottom:1px solid var(--mst-border-subtle,#f1f5f9);}',
      '#mst-shortcut-modal td:first-child{color:var(--mst-text-secondary,#334155);}',
      '#mst-shortcut-modal td:last-child{text-align:right;' +
        'font-family:var(--mst-font-mono,monospace);font-size:12px;' +
        'color:var(--mst-text-muted,#64748b);}',
      '#mst-shortcut-modal kbd{display:inline-block;padding:2px 6px;' +
        'border:1px solid var(--mst-border,#e2e8f0);border-radius:4px;' +
        'background:var(--mst-surface,#fff);font-size:11px;line-height:1.4;}',

      // --- 7. Console auto-scroll pin ---
      '.mst-console-pin{position:absolute;top:4px;right:4px;z-index:5;' +
        'padding:3px 8px;font-size:11px;border-radius:4px;cursor:pointer;' +
        'border:1px solid var(--mst-border,#e2e8f0);' +
        'background:var(--mst-surface,#fff);color:var(--mst-text-muted,#64748b);}',
      '.mst-console-pin.pinned{background:var(--mst-accent,#0d9488);color:#fff;border-color:transparent;}',

      // --- 8. Contextual help tooltip ---
      '.mst-help-tip{position:absolute;z-index:9999;max-width:280px;padding:10px 14px;' +
        'border-radius:8px;background:var(--mst-surface-elevated,#fff);' +
        'border:1px solid var(--mst-border,#e2e8f0);box-shadow:var(--mst-shadow-lg);' +
        'font-size:13px;color:var(--mst-text-secondary,#334155);' +
        'opacity:0;transform:translateY(4px);' +
        'transition:opacity 0.2s ease,transform 0.2s ease;pointer-events:none;}',
      '.mst-help-tip.show{opacity:1;transform:translateY(0);}',
      '.mst-help-tip .tip-title{font-weight:600;margin-bottom:4px;' +
        'color:var(--mst-text,#0f172a);font-size:13px;}',
      '.mst-help-tip .tip-example{font-family:var(--mst-font-mono,monospace);' +
        'font-size:12px;color:var(--mst-accent,#0d9488);margin-top:4px;}',

      // --- 9. Value box count animation ---
      '.mst-counting{transition:none!important;}',

      // --- Reduced motion overrides ---
      '@media(prefers-reduced-motion:reduce){' +
        '#mst-tab-pill{transition:none!important;}' +
        '.mst-plot-fullscreen,.mst-fs-backdrop,.mst-help-tip{transition:none!important;}' +
        '#mst-scroll-progress{display:none!important;}' +
        '}',
    ].join('\n');

    var style = document.createElement('style');
    style.id = 'mst-premium-styles';
    style.textContent = css;
    document.head.appendChild(style);
  }

  // ==========================================================================
  // Feature 1: Smooth Counting Animation for Value Boxes
  // ==========================================================================
  function initCountUpAnimation() {
    if (!('IntersectionObserver' in window)) return;

    var observed = new WeakSet();

    function animateValue(el, target, suffix) {
      if (reducedMotion()) {
        el.textContent = target + suffix;
        return;
      }
      var isDecimal = String(target).indexOf('.') !== -1;
      var decimals = isDecimal
        ? (String(target).split('.')[1] || '').length
        : 0;
      var start = 0;
      var duration = 1200;
      var startTime = null;

      function step(ts) {
        if (!startTime) startTime = ts;
        var progress = Math.min((ts - startTime) / duration, 1);
        var eased = easeOutExpo(progress);
        var current = start + (target - start) * eased;
        el.textContent =
          (isDecimal ? current.toFixed(decimals) : Math.round(current)) +
          suffix;
        if (progress < 1) {
          requestAnimationFrame(step);
        }
      }
      requestAnimationFrame(step);
    }

    var io = new IntersectionObserver(
      function (entries) {
        entries.forEach(function (entry) {
          if (!entry.isIntersecting) return;
          var el = entry.target;
          io.unobserve(el);

          var raw = (el.textContent || '').trim();
          // Extract numeric value and suffix (%, x, etc.)
          var match = raw.match(/^([+-]?\d[\d,]*\.?\d*)\s*(%|x|X)?$/);
          if (!match) return;

          var numStr = match[1].replace(/,/g, '');
          var num = parseFloat(numStr);
          var suffix = match[2] || '';
          if (isNaN(num) || num === 0) return;

          el.classList.add('mst-counting');
          animateValue(el, num, suffix);
        });
      },
      { threshold: 0.5 },
    );

    function scanValueBoxes() {
      // Target bslib value-box values, Shiny info/value boxes
      var selectors = [
        '.bslib-value-box .value-box-value',
        '.info-box-number',
        '.small-box .inner h3',
        '.value-box .value',
      ];
      selectors.forEach(function (sel) {
        document.querySelectorAll(sel).forEach(function (el) {
          if (!observed.has(el)) {
            observed.add(el);
            io.observe(el);
          }
        });
      });
    }

    // Initial scan + re-scan on Shiny value updates
    scanValueBoxes();
    var scanObserver = new MutationObserver(function () {
      scanValueBoxes();
    });
    scanObserver.observe(document.body, { childList: true, subtree: true });
  }

  // ==========================================================================
  // Feature 2: Mouse-Following Gradient on Cards
  // ==========================================================================
  function initCardGradient() {
    if (reducedMotion()) return;

    function attachGlow(card) {
      if (card._mstGlowAttached) return;
      card._mstGlowAttached = true;

      var glow = document.createElement('div');
      glow.className = 'mst-card-glow';
      card.style.position = card.style.position || 'relative';
      card.style.overflow = 'hidden';
      card.insertBefore(glow, card.firstChild);

      card.addEventListener('mousemove', function (e) {
        if (card._glowRAF) return;
        card._glowRAF = requestAnimationFrame(function () {
          var rect = card.getBoundingClientRect();
          var x = e.clientX - rect.left;
          var y = e.clientY - rect.top;
          glow.style.background =
            'radial-gradient(350px circle at ' +
            x +
            'px ' +
            y +
            'px,' +
            'rgba(13,148,136,0.07),transparent 70%)';
          glow.style.opacity = '1';
          card._glowRAF = null;
        });
      });

      card.addEventListener('mouseleave', function () {
        glow.style.opacity = '0';
      });
    }

    // Attach to all .card elements that are not inside navbars
    document.querySelectorAll('.card').forEach(function (card) {
      if (!card.closest('.navbar')) attachGlow(card);
    });

    // Watch for dynamically added cards
    var observer = new MutationObserver(function (mutations) {
      mutations.forEach(function (m) {
        m.addedNodes.forEach(function (node) {
          if (node.nodeType !== 1) return;
          var cards =
            node.classList && node.classList.contains('card')
              ? [node]
              : node.querySelectorAll
                ? Array.from(node.querySelectorAll('.card'))
                : [];
          cards.forEach(function (c) {
            if (!c.closest('.navbar')) attachGlow(c);
          });
        });
      });
    });
    observer.observe(document.body, { childList: true, subtree: true });
  }

  // ==========================================================================
  // Feature 3: Enhanced Tab Sliding Pill Indicator
  // ==========================================================================
  function initTabPill() {
    var navBar = document.querySelector('.navbar-nav, .navbar .nav');
    if (!navBar) return;

    // Need relative positioning on the nav container
    navBar.style.position = 'relative';

    var pill = document.createElement('div');
    pill.id = 'mst-tab-pill';
    navBar.appendChild(pill);

    function positionPill(tab) {
      if (!tab) return;
      var navRect = navBar.getBoundingClientRect();
      var tabRect = tab.getBoundingClientRect();
      pill.style.left = tabRect.left - navRect.left + 'px';
      pill.style.width = tabRect.width + 'px';
      pill.style.top = '0';
      pill.style.height = tabRect.height + 'px';
    }

    // Position on active tab initially
    var activeTab = navBar.querySelector('.nav-link.active');
    if (activeTab) {
      // No transition for initial placement
      pill.style.transition = 'none';
      positionPill(activeTab);
      requestAnimationFrame(function () {
        requestAnimationFrame(function () {
          pill.style.transition = '';
        });
      });
    }

    // Move pill on tab change
    if (typeof $ !== 'undefined' || typeof jQuery !== 'undefined') {
      var jq = typeof $ !== 'undefined' ? $ : jQuery;
      jq(document).on('shown.bs.tab', '.navbar .nav-link', function (e) {
        positionPill(e.target);
      });
    }

    // Reposition on window resize
    var resizeTimer;
    window.addEventListener(
      'resize',
      function () {
        clearTimeout(resizeTimer);
        resizeTimer = setTimeout(function () {
          var current = navBar.querySelector('.nav-link.active');
          if (current) positionPill(current);
        }, 100);
      },
      { passive: true },
    );
  }

  // ==========================================================================
  // Feature 4: Scroll Progress Indicator
  // ==========================================================================
  function initScrollProgress() {
    if (reducedMotion()) return;

    var bar = document.createElement('div');
    bar.id = 'mst-scroll-progress';
    document.body.appendChild(bar);

    var ticking = false;
    window.addEventListener(
      'scroll',
      function () {
        if (ticking) return;
        ticking = true;
        requestAnimationFrame(function () {
          var scrollTop = window.scrollY || document.documentElement.scrollTop;
          var docHeight =
            document.documentElement.scrollHeight -
            document.documentElement.clientHeight;
          var pct = docHeight > 0 ? (scrollTop / docHeight) * 100 : 0;
          bar.style.width = pct + '%';
          ticking = false;
        });
      },
      { passive: true },
    );
  }

  // ==========================================================================
  // Feature 5: Smart Sidebar Collapse Memory
  // ==========================================================================
  function initSidebarMemory() {
    var STORAGE_KEY = 'mst_sidebar_state';

    function getCurrentTabId() {
      var activeTab = document.querySelector('.navbar .nav-link.active');
      if (!activeTab) return 'default';
      return (
        activeTab.getAttribute('data-bs-target') ||
        activeTab.getAttribute('href') ||
        activeTab.textContent.trim()
      ).replace(/^#/, '');
    }

    function getStates() {
      var raw = safeSessionGet(STORAGE_KEY);
      if (!raw) return {};
      try {
        return JSON.parse(raw);
      } catch (e) {
        return {};
      }
    }

    function saveState(collapsed) {
      var states = getStates();
      states[getCurrentTabId()] = collapsed;
      safeSessionSet(STORAGE_KEY, JSON.stringify(states));
    }

    function restoreState() {
      var states = getStates();
      var tabId = getCurrentTabId();
      if (!(tabId in states)) return;

      var sidebar = document.querySelector(
        '.sidebar, .bslib-sidebar-layout > .sidebar',
      );
      var toggleBtn = document.querySelector(
        '.bslib-sidebar-toggle, [data-bs-toggle="collapse"][data-bs-target*="sidebar"]',
      );
      if (!sidebar) return;

      var isCollapsed =
        sidebar.classList.contains('collapsed') ||
        sidebar.getAttribute('aria-expanded') === 'false';

      if (states[tabId] !== isCollapsed && toggleBtn) {
        toggleBtn.click();
      }
    }

    // Observe sidebar toggle clicks
    document.addEventListener('click', function (e) {
      var btn = e.target.closest(
        '.bslib-sidebar-toggle, [data-bs-toggle="collapse"][data-bs-target*="sidebar"]',
      );
      if (!btn) return;
      // Small delay so the sidebar state has updated
      setTimeout(function () {
        var sidebar = document.querySelector(
          '.sidebar, .bslib-sidebar-layout > .sidebar',
        );
        if (!sidebar) return;
        var isCollapsed =
          sidebar.classList.contains('collapsed') ||
          sidebar.getAttribute('aria-expanded') === 'false';
        saveState(isCollapsed);
      }, 100);
    });

    // Restore on tab switch
    if (typeof $ !== 'undefined' || typeof jQuery !== 'undefined') {
      var jq = typeof $ !== 'undefined' ? $ : jQuery;
      jq(document).on('shown.bs.tab', '.navbar .nav-link', function () {
        setTimeout(restoreState, 150);
      });
    }

    // Restore on initial load
    setTimeout(restoreState, 500);
  }

  // ==========================================================================
  // Feature 6: Plot Fullscreen Toggle
  // ==========================================================================
  function initPlotFullscreen() {
    var attached = new WeakSet();

    function attachButton(plotEl) {
      if (attached.has(plotEl)) return;
      attached.add(plotEl);

      var wrapper = plotEl.closest('.plotly, .js-plotly-plot') || plotEl;
      wrapper.style.position = wrapper.style.position || 'relative';

      var btn = document.createElement('button');
      btn.className = 'mst-plot-fs-btn';
      btn.setAttribute('aria-label', 'Toggle fullscreen chart');
      btn.textContent = '\u26F6'; // square with corners
      wrapper.appendChild(btn);

      btn.addEventListener('click', function () {
        var isFS = wrapper.classList.contains('mst-plot-fullscreen');
        if (isFS) {
          exitFullscreen(wrapper);
        } else {
          enterFullscreen(wrapper);
        }
      });
    }

    function enterFullscreen(wrapper) {
      // Store original dimensions
      wrapper._origStyle = wrapper.getAttribute('style') || '';
      wrapper._origParent = wrapper.parentNode;
      wrapper._origNext = wrapper.nextSibling;

      // Create backdrop
      var backdrop = document.createElement('div');
      backdrop.className = 'mst-fs-backdrop';
      backdrop.addEventListener('click', function () {
        exitFullscreen(wrapper);
      });
      document.body.appendChild(backdrop);
      wrapper._backdrop = backdrop;

      requestAnimationFrame(function () {
        backdrop.classList.add('show');
      });

      // Move to body and fullscreen
      document.body.appendChild(wrapper);
      wrapper.classList.add('mst-plot-fullscreen');

      // Update button
      var btn = wrapper.querySelector('.mst-plot-fs-btn');
      if (btn) {
        btn.textContent = '\u2715'; // X
        btn.style.opacity = '1';
      }

      // Resize plotly
      if (window.Plotly && wrapper.querySelector('.js-plotly-plot')) {
        var plotDiv = wrapper.querySelector('.js-plotly-plot') || wrapper;
        setTimeout(function () {
          window.Plotly.Plots.resize(plotDiv);
        }, 50);
      }

      // ESC to close
      wrapper._escHandler = function (e) {
        if (e.key === 'Escape') exitFullscreen(wrapper);
      };
      document.addEventListener('keydown', wrapper._escHandler);
      announceToSR('Chart expanded to fullscreen. Press Escape to exit.');
    }

    function exitFullscreen(wrapper) {
      wrapper.classList.remove('mst-plot-fullscreen');

      // Remove backdrop
      if (wrapper._backdrop) {
        wrapper._backdrop.classList.remove('show');
        var bd = wrapper._backdrop;
        setTimeout(function () {
          if (bd.parentNode) bd.parentNode.removeChild(bd);
        }, 300);
        wrapper._backdrop = null;
      }

      // Restore in DOM
      if (wrapper._origParent) {
        if (wrapper._origNext) {
          wrapper._origParent.insertBefore(wrapper, wrapper._origNext);
        } else {
          wrapper._origParent.appendChild(wrapper);
        }
      }

      // Restore style
      if (wrapper._origStyle !== undefined) {
        wrapper.setAttribute('style', wrapper._origStyle);
      }

      // Update button
      var btn = wrapper.querySelector('.mst-plot-fs-btn');
      if (btn) {
        btn.textContent = '\u26F6';
        btn.style.opacity = '';
      }

      // Resize plotly back
      if (window.Plotly && wrapper.querySelector('.js-plotly-plot')) {
        var plotDiv = wrapper.querySelector('.js-plotly-plot') || wrapper;
        setTimeout(function () {
          window.Plotly.Plots.resize(plotDiv);
        }, 50);
      }

      // Remove ESC handler
      if (wrapper._escHandler) {
        document.removeEventListener('keydown', wrapper._escHandler);
        wrapper._escHandler = null;
      }
      announceToSR('Chart exited fullscreen.');
    }

    function scanPlots() {
      document
        .querySelectorAll('.js-plotly-plot, .plotly')
        .forEach(function (el) {
          attachButton(el);
        });
    }

    scanPlots();
    var observer = new MutationObserver(function () {
      scanPlots();
    });
    observer.observe(document.body, { childList: true, subtree: true });
  }

  // ==========================================================================
  // Feature 7: Command Palette (Ctrl+K / Cmd+K)
  // ==========================================================================
  function initCommandPalette() {
    // Build overlay
    var overlay = document.createElement('div');
    overlay.id = 'mst-cmd-overlay';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-label', 'Command palette');

    var box = document.createElement('div');
    box.id = 'mst-cmd-box';

    var input = document.createElement('input');
    input.id = 'mst-cmd-input';
    input.type = 'text';
    input.setAttribute('placeholder', 'Type a command or tab name\u2026');
    input.setAttribute('autocomplete', 'off');
    input.setAttribute('spellcheck', 'false');

    var list = document.createElement('ul');
    list.id = 'mst-cmd-list';
    list.setAttribute('role', 'listbox');

    box.appendChild(input);
    box.appendChild(list);
    overlay.appendChild(box);
    document.body.appendChild(overlay);

    var activeIndex = 0;
    var currentItems = [];

    function getCommands() {
      var cmds = [];
      // Tabs
      document.querySelectorAll('.navbar .nav-link').forEach(function (tab, i) {
        var name = tab.textContent.trim();
        if (!name) return;
        cmds.push({
          label: name,
          icon: '\u21B5',
          shortcut: i < 7 ? 'Ctrl+Shift+' + (i + 1) : '',
          action: function () {
            tab.click();
          },
        });
      });
      // Theme toggle
      cmds.push({
        label: 'Toggle Dark Mode',
        icon: '\u263E',
        shortcut: 'Ctrl+Shift+D',
        action: function () {
          var btn = document.getElementById('theme_toggle');
          if (btn) btn.click();
        },
      });
      // Scroll to top
      cmds.push({
        label: 'Scroll to Top',
        icon: '\u2191',
        shortcut: '',
        action: function () {
          window.scrollTo({ top: 0, behavior: 'smooth' });
        },
      });
      // Keyboard shortcuts help
      cmds.push({
        label: 'Keyboard Shortcuts',
        icon: '\u2328',
        shortcut: '?',
        action: function () {
          openShortcutOverlay();
        },
      });
      return cmds;
    }

    function renderList(filter) {
      while (list.firstChild) list.removeChild(list.firstChild);
      var commands = getCommands();
      var lowerFilter = (filter || '').toLowerCase();
      currentItems = commands.filter(function (cmd) {
        return (
          !lowerFilter || cmd.label.toLowerCase().indexOf(lowerFilter) !== -1
        );
      });
      activeIndex = 0;
      currentItems.forEach(function (cmd, i) {
        var li = document.createElement('li');
        li.setAttribute('role', 'option');
        if (i === 0) li.classList.add('active');

        var icon = document.createElement('span');
        icon.className = 'cmd-icon';
        icon.textContent = cmd.icon;
        li.appendChild(icon);

        var label = document.createElement('span');
        label.textContent = cmd.label;
        li.appendChild(label);

        if (cmd.shortcut) {
          var sc = document.createElement('span');
          sc.className = 'cmd-shortcut';
          sc.textContent = cmd.shortcut;
          li.appendChild(sc);
        }

        li.addEventListener('click', function () {
          closePalette();
          cmd.action();
        });
        li.addEventListener('mouseenter', function () {
          setActive(i);
        });
        list.appendChild(li);
      });
    }

    function setActive(idx) {
      var items = list.querySelectorAll('li');
      items.forEach(function (li) {
        li.classList.remove('active');
      });
      activeIndex = Math.max(0, Math.min(idx, items.length - 1));
      if (items[activeIndex]) {
        items[activeIndex].classList.add('active');
        items[activeIndex].scrollIntoView({ block: 'nearest' });
      }
    }

    function openPalette() {
      overlay.classList.add('open');
      input.value = '';
      renderList('');
      input.focus();
      announceToSR('Command palette opened. Type to search commands.');
    }

    function closePalette() {
      overlay.classList.remove('open');
      input.value = '';
    }

    function executeActive() {
      if (currentItems[activeIndex]) {
        closePalette();
        currentItems[activeIndex].action();
      }
    }

    input.addEventListener('input', function () {
      renderList(input.value);
    });

    input.addEventListener('keydown', function (e) {
      if (e.key === 'ArrowDown') {
        e.preventDefault();
        setActive(activeIndex + 1);
      } else if (e.key === 'ArrowUp') {
        e.preventDefault();
        setActive(activeIndex - 1);
      } else if (e.key === 'Enter') {
        e.preventDefault();
        executeActive();
      } else if (e.key === 'Escape') {
        e.preventDefault();
        closePalette();
      }
    });

    // Close on backdrop click
    overlay.addEventListener('click', function (e) {
      if (e.target === overlay) closePalette();
    });

    // Global shortcut: Ctrl+K / Cmd+K
    document.addEventListener('keydown', function (e) {
      if ((e.ctrlKey || e.metaKey) && e.key === 'k') {
        e.preventDefault();
        if (overlay.classList.contains('open')) {
          closePalette();
        } else {
          openPalette();
        }
      }
    });
  }

  // ==========================================================================
  // Feature 8: Smooth Page Transitions (crossfade/slide on tab switch)
  // ==========================================================================
  function initSmoothTabTransitions() {
    if (reducedMotion()) return;
    if (typeof $ === 'undefined' && typeof jQuery === 'undefined') return;
    var jq = typeof $ !== 'undefined' ? $ : jQuery;

    jq(document).on('hide.bs.tab', '.navbar .nav-link', function (e) {
      var href =
        e.target.getAttribute('data-bs-target') ||
        e.target.getAttribute('href');
      if (!href) return;
      var pane = document.querySelector(href);
      if (pane) {
        pane.style.transition = 'opacity 0.15s ease, transform 0.15s ease';
        pane.style.opacity = '0';
        pane.style.transform = 'translateY(6px)';
      }
    });

    jq(document).on('shown.bs.tab', '.navbar .nav-link', function (e) {
      var href =
        e.target.getAttribute('data-bs-target') ||
        e.target.getAttribute('href');
      if (!href) return;
      var pane = document.querySelector(href);
      if (pane) {
        pane.style.opacity = '0';
        pane.style.transform = 'translateY(6px)';
        pane.style.transition = 'opacity 0.3s ease, transform 0.3s ease';
        requestAnimationFrame(function () {
          requestAnimationFrame(function () {
            pane.style.opacity = '1';
            pane.style.transform = 'translateY(0)';
          });
        });
      }
    });
  }

  // ==========================================================================
  // Feature 9: Auto-scrolling Console with Pin Toggle
  // ==========================================================================
  function initConsoleAutoScroll() {
    var attached = new WeakSet();

    function attachConsole(el) {
      if (attached.has(el)) return;
      attached.add(el);

      var wrapper = el.closest('.shiny-text-output, .shiny-html-output') || el;
      wrapper.style.position = wrapper.style.position || 'relative';

      var pinned = true;

      var pinBtn = document.createElement('button');
      pinBtn.className = 'mst-console-pin pinned';
      pinBtn.textContent = '\u25BC Auto-scroll';
      pinBtn.setAttribute('aria-label', 'Toggle auto-scroll');
      wrapper.appendChild(pinBtn);

      pinBtn.addEventListener('click', function () {
        pinned = !pinned;
        pinBtn.classList.toggle('pinned', pinned);
        pinBtn.textContent = pinned ? '\u25BC Auto-scroll' : '\u25A0 Paused';
        if (pinned) {
          el.scrollTop = el.scrollHeight;
        }
      });

      var observer = new MutationObserver(function () {
        if (pinned) {
          requestAnimationFrame(function () {
            el.scrollTo({
              top: el.scrollHeight,
              behavior: reducedMotion() ? 'auto' : 'smooth',
            });
          });
        }
      });
      observer.observe(el, {
        childList: true,
        subtree: true,
        characterData: true,
      });
    }

    function scan() {
      // Console outputs typically have overflow set and monospace content
      document
        .querySelectorAll(
          '#console_output, #log_output, [id*="console"], [id*="log_output"], pre.shiny-text-output',
        )
        .forEach(function (el) {
          var style = window.getComputedStyle(el);
          if (
            style.overflow === 'auto' ||
            style.overflow === 'scroll' ||
            style.overflowY === 'auto' ||
            style.overflowY === 'scroll' ||
            el.scrollHeight > el.clientHeight
          ) {
            attachConsole(el);
          }
        });
    }

    scan();
    // Re-scan when Shiny sends new output
    if (typeof $ !== 'undefined' || typeof jQuery !== 'undefined') {
      var jq = typeof $ !== 'undefined' ? $ : jQuery;
      jq(document).on('shiny:value', function () {
        setTimeout(scan, 200);
      });
    }
  }

  // ==========================================================================
  // Feature 10: Contextual Help Tooltips on Sidebar Labels
  // ==========================================================================
  function initContextualTooltips() {
    var activeTip = null;

    function showTip(label) {
      hideTip();
      // Look for data attributes on the label or its associated input
      var controlId = label.getAttribute('for');
      var control = controlId ? document.getElementById(controlId) : null;
      var helpText =
        label.getAttribute('data-mst-help') ||
        (control && control.getAttribute('data-mst-help'));
      var helpExample =
        label.getAttribute('data-mst-example') ||
        (control && control.getAttribute('data-mst-example'));
      var placeholder = control && control.getAttribute('placeholder');

      // Build tip content from available info
      if (!helpText && !helpExample && !placeholder) return;

      var tip = document.createElement('div');
      tip.className = 'mst-help-tip';
      tip.setAttribute('role', 'tooltip');

      if (helpText) {
        var title = document.createElement('div');
        title.className = 'tip-title';
        title.textContent = helpText;
        tip.appendChild(title);
      }
      if (helpExample) {
        var ex = document.createElement('div');
        ex.className = 'tip-example';
        ex.textContent = 'e.g. ' + helpExample;
        tip.appendChild(ex);
      } else if (placeholder) {
        var ph = document.createElement('div');
        ph.className = 'tip-example';
        ph.textContent = 'e.g. ' + placeholder;
        tip.appendChild(ph);
      }

      document.body.appendChild(tip);

      // Position below label
      var rect = label.getBoundingClientRect();
      tip.style.top = rect.bottom + window.scrollY + 6 + 'px';
      tip.style.left = rect.left + window.scrollX + 'px';

      requestAnimationFrame(function () {
        tip.classList.add('show');
      });

      activeTip = tip;
    }

    function hideTip() {
      if (activeTip) {
        if (activeTip.parentNode) activeTip.parentNode.removeChild(activeTip);
        activeTip = null;
      }
    }

    // Delegate hover events on sidebar labels
    document.addEventListener(
      'mouseenter',
      function (e) {
        var label = e.target.closest
          ? e.target.closest(
              ".sidebar label, .well label, [class*='sidebar'] label, .bslib-sidebar label",
            )
          : null;
        if (label) showTip(label);
      },
      true,
    );

    document.addEventListener(
      'mouseleave',
      function (e) {
        var label = e.target.closest
          ? e.target.closest(
              ".sidebar label, .well label, [class*='sidebar'] label, .bslib-sidebar label",
            )
          : null;
        if (label) hideTip();
      },
      true,
    );
  }

  // ==========================================================================
  // Feature 12: Keyboard Shortcut Overlay (press "?")
  // ==========================================================================
  var shortcutOverlay = null;

  function initShortcutOverlay() {
    var overlay = document.createElement('div');
    overlay.id = 'mst-shortcut-overlay';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-label', 'Keyboard shortcuts');

    var modal = document.createElement('div');
    modal.id = 'mst-shortcut-modal';

    var heading = document.createElement('h3');
    heading.textContent = 'Keyboard Shortcuts';
    modal.appendChild(heading);

    var shortcuts = [
      ['Toggle dark mode', 'Ctrl+Shift+D'],
      ['Navigate to tab 1\u20137', 'Ctrl+Shift+1\u20137'],
      ['Command palette', 'Ctrl+K'],
      ['Show this help', '?'],
    ];

    var table = document.createElement('table');
    shortcuts.forEach(function (row) {
      var tr = document.createElement('tr');
      var tdLabel = document.createElement('td');
      tdLabel.textContent = row[0];
      var tdKey = document.createElement('td');

      // Build kbd elements for each key part
      var parts = row[1].split('+');
      parts.forEach(function (part, i) {
        var kbd = document.createElement('kbd');
        kbd.textContent = part;
        tdKey.appendChild(kbd);
        if (i < parts.length - 1) {
          tdKey.appendChild(document.createTextNode(' + '));
        }
      });
      tr.appendChild(tdLabel);
      tr.appendChild(tdKey);
      table.appendChild(tr);
    });
    modal.appendChild(table);
    overlay.appendChild(modal);
    document.body.appendChild(overlay);
    shortcutOverlay = overlay;

    // Close on backdrop click
    overlay.addEventListener('click', function (e) {
      if (e.target === overlay) closeShortcutOverlay();
    });

    // Listen for "?" key (only when no input is focused)
    document.addEventListener('keydown', function (e) {
      // Do not trigger if typing in an input, textarea, or contenteditable
      var tag = (e.target.tagName || '').toLowerCase();
      if (tag === 'input' || tag === 'textarea' || tag === 'select') return;
      if (e.target.isContentEditable) return;

      if (e.key === '?' && !e.ctrlKey && !e.metaKey && !e.altKey) {
        e.preventDefault();
        if (overlay.classList.contains('open')) {
          closeShortcutOverlay();
        } else {
          openShortcutOverlay();
        }
      }

      // ESC closes any open overlay
      if (e.key === 'Escape') {
        if (overlay.classList.contains('open')) {
          closeShortcutOverlay();
        }
      }
    });
  }

  function openShortcutOverlay() {
    if (shortcutOverlay) {
      shortcutOverlay.classList.add('open');
      announceToSR('Keyboard shortcuts overlay opened.');
    }
  }

  function closeShortcutOverlay() {
    if (shortcutOverlay) {
      shortcutOverlay.classList.remove('open');
    }
  }

  // ==========================================================================
  // Main Initialization
  // ==========================================================================
  document.addEventListener('DOMContentLoaded', function () {
    injectPremiumStyles();

    // Features that work immediately
    initScrollProgress();
    initTabPill();
    initCommandPalette();
    initShortcutOverlay();
    initSmoothTabTransitions();
    initSidebarMemory();
    initContextualTooltips();

    // Features that need the DOM to settle (Shiny renders asynchronously)
    setTimeout(function () {
      initCountUpAnimation();
      initCardGradient();
      initPlotFullscreen();
      initConsoleAutoScroll();
    }, 600);
  });
})();
