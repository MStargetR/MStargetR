// MStargetR Shiny Application - Custom JavaScript (Premium Edition)
// =============================================================================

(function () {
  'use strict';

  // ==========================================================================
  // Section 1: Constants & State
  // ==========================================================================
  var THEME_KEY = 'mstargetr_theme';
  var toastStack = [];
  var konamiSeq = [38, 38, 40, 40, 37, 39, 37, 39, 66, 65];
  var konamiPos = 0;

  var cardNavMap = {
    dash_card_convert: 'File Conversion',
    dash_card_peak: 'Peak Integration',
    dash_card_qc: 'Quality Control',
    dash_card_batch: 'Batch Correction',
    dash_card_results: 'Results',
    dash_card_utils: 'Utilities',
    dash_card_settings: 'Settings',
  };

  var typeColors = {
    success: '#28a745',
    warning: '#ffc107',
    danger: '#dc3545',
    info: '#17a2b8',
  };

  // ==========================================================================
  // Section 2: Theme Toggle with localStorage Persistence
  // ==========================================================================
  function getStoredTheme() {
    try {
      return localStorage.getItem(THEME_KEY);
    } catch (e) {
      return null;
    }
  }

  function setStoredTheme(theme) {
    try {
      localStorage.setItem(THEME_KEY, theme);
    } catch (e) {
      /* noop */
    }
  }

  function applyTheme(theme) {
    document.documentElement.setAttribute('data-bs-theme', theme);
    var btn = document.getElementById('theme_toggle');
    if (btn) {
      btn.setAttribute(
        'title',
        theme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode',
      );
      // Animate icon rotation
      btn.style.transition = 'transform 0.4s ease';
      btn.style.transform = 'rotate(360deg)';
      setTimeout(function () {
        btn.textContent = theme === 'dark' ? '\u2600' : '\u263E';
        btn.style.transform = 'rotate(0deg)';
      }, 200);
      // Ripple effect
      createRipple(btn);
    }
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('current_theme', theme);
    }
  }

  function createRipple(el) {
    var ripple = document.createElement('span');
    ripple.style.cssText =
      'position:absolute;border-radius:50%;background:rgba(255,255,255,0.4);' +
      'width:40px;height:40px;top:50%;left:50%;transform:translate(-50%,-50%) scale(0);' +
      'animation:mst-ripple 0.5s ease-out forwards;pointer-events:none;';
    el.style.position = 'relative';
    el.style.overflow = 'hidden';
    el.appendChild(ripple);
    setTimeout(function () {
      if (ripple.parentNode) ripple.parentNode.removeChild(ripple);
    }, 600);
  }

  // ==========================================================================
  // Section 3: Page Load Animation
  // ==========================================================================
  function animatePageLoad() {
    document.body.style.opacity = '0';
    document.body.style.transition = 'opacity 0.4s ease';
    requestAnimationFrame(function () {
      document.body.style.opacity = '1';
    });
    // Stagger workflow cards
    var cards = document.querySelectorAll(
      ".workflow-card, .dash-workflow-card, [id^='dash_card_']",
    );
    cards.forEach(function (card, i) {
      card.style.opacity = '0';
      card.style.transform = 'translateY(16px)';
      card.style.transition = 'opacity 0.4s ease, transform 0.4s ease';
      setTimeout(
        function () {
          card.style.opacity = '1';
          card.style.transform = 'translateY(0)';
        },
        200 + i * 100,
      );
    });
  }

  // ==========================================================================
  // Section 4: Inject Dynamic Styles
  // ==========================================================================
  function injectStyles() {
    var css = [
      // Theme transition on body
      'body{transition:background-color 0.3s ease,color 0.3s ease;}',
      // Ripple keyframe
      '@keyframes mst-ripple{to{transform:translate(-50%,-50%) scale(3);opacity:0;}}',
      // Toast styles
      '.mst-toast{position:fixed;right:20px;z-index:10000;min-width:280px;max-width:380px;' +
        'padding:14px 40px 14px 16px;border-radius:8px;background:var(--bs-body-bg,#fff);' +
        'box-shadow:0 8px 24px rgba(0,0,0,0.15);font-size:14px;opacity:0;' +
        'transform:translateX(40px);transition:opacity 0.3s ease,transform 0.3s ease;' +
        'color:var(--bs-body-color,#333);}',
      '.mst-toast.show{opacity:1;transform:translateX(0);}',
      '.mst-toast-close{position:absolute;top:8px;right:10px;background:none;border:none;' +
        'font-size:18px;cursor:pointer;color:inherit;opacity:0.6;line-height:1;}',
      '.mst-toast-close:hover{opacity:1;}',
      '.mst-toast-progress{position:absolute;bottom:0;left:0;height:3px;border-radius:0 0 8px 8px;' +
        'background:currentColor;opacity:0.3;transition:width linear;}',
      // Back to top
      '#mst-back-to-top{position:fixed;bottom:24px;right:24px;z-index:9999;width:40px;height:40px;' +
        'border-radius:50%;border:none;background:var(--bs-primary,#0d6efd);color:#fff;' +
        'font-size:20px;cursor:pointer;opacity:0;transform:translateY(12px);' +
        'transition:opacity 0.3s ease,transform 0.3s ease;display:flex;' +
        'align-items:center;justify-content:center;box-shadow:0 4px 12px rgba(0,0,0,0.2);}',
      '#mst-back-to-top.visible{opacity:1;transform:translateY(0);}',
      '#mst-back-to-top:hover{filter:brightness(1.1);}',
      // Top loading bar
      '#mst-loading-bar{position:fixed;top:0;left:0;height:3px;z-index:10001;background:' +
        'linear-gradient(90deg,var(--bs-primary,#0d6efd),var(--bs-info,#17a2b8));' +
        'width:0;transition:width 0.3s ease;pointer-events:none;}',
      // Workflow card hover tilt
      "[id^='dash_card_']{transition:transform 0.25s ease,box-shadow 0.25s ease;cursor:pointer;" +
        'perspective:600px;}',
      "[id^='dash_card_']:active{transform:scale(0.97)!important;}",
      // Skip link
      '#mst-skip-link{position:absolute;top:-100px;left:8px;z-index:10002;padding:8px 16px;' +
        'background:var(--bs-primary,#0d6efd);color:#fff;border-radius:4px;text-decoration:none;' +
        'transition:top 0.2s ease;font-size:14px;}',
      '#mst-skip-link:focus{top:8px;}',
      // Table copy button
      '.mst-copy-btn{position:absolute;top:6px;right:6px;z-index:5;padding:4px 10px;' +
        'font-size:12px;border:1px solid var(--bs-border-color,#dee2e6);border-radius:4px;' +
        'background:var(--bs-body-bg,#fff);color:var(--bs-body-color,#333);cursor:pointer;' +
        'opacity:0;transition:opacity 0.2s ease;}',
      '.dataTables_wrapper:hover .mst-copy-btn{opacity:1;}',
      // Confetti
      '.mst-confetti{position:fixed;top:0;left:0;width:100%;height:100%;pointer-events:none;' +
        'z-index:10003;overflow:hidden;}',
      '.mst-confetti-piece{position:absolute;width:8px;height:8px;opacity:0.9;will-change:transform,opacity;}',
      // Viewport reveal
      '.mst-reveal{opacity:0;transform:translateY(20px);transition:opacity 0.5s ease,transform 0.5s ease;}',
      '.mst-reveal.visible{opacity:1;transform:translateY(0);}',
      // Aria live region
      '#mst-sr-announcer{position:absolute;width:1px;height:1px;overflow:hidden;clip:rect(0,0,0,0);' +
        'white-space:nowrap;border:0;}',
    ].join('\n');

    var style = document.createElement('style');
    style.textContent = css;
    document.head.appendChild(style);
  }

  // ==========================================================================
  // Section 5: Notification System (Upgraded)
  // ==========================================================================
  window.MStargetR = window.MStargetR || {};

  window.MStargetR.notify = function (message, type, duration) {
    type = type || 'info';
    duration = duration || 4000;
    var color = typeColors[type] || typeColors.info;

    var toast = document.createElement('div');
    toast.className = 'mst-toast';
    toast.style.borderLeft = '4px solid ' + color;
    // Stack offset
    var topOffset = 20 + toastStack.length * 80;
    toast.style.top = topOffset + 'px';

    // Build toast content using safe DOM methods (no innerHTML)
    var contentRow = document.createElement('div');
    contentRow.style.cssText = 'display:flex;align-items:center;gap:8px;';

    var dot = document.createElement('span');
    dot.className = 'status-dot status-dot-' + type;
    contentRow.appendChild(dot);

    var span = document.createElement('span');
    if (message.indexOf('\n') > -1) {
      message.split('\n').forEach(function (line, i) {
        if (i > 0) span.appendChild(document.createElement('br'));
        span.appendChild(document.createTextNode(line));
      });
    } else {
      span.textContent = message;
    }
    contentRow.appendChild(span);

    toast.appendChild(contentRow);

    var closeBtn = document.createElement('button');
    closeBtn.className = 'mst-toast-close';
    closeBtn.setAttribute('aria-label', 'Close notification');
    closeBtn.textContent = '\u00D7';
    closeBtn.addEventListener('click', function () {
      dismissToast(toast);
    });
    toast.appendChild(closeBtn);

    var bar = document.createElement('div');
    bar.className = 'mst-toast-progress';
    bar.style.width = '100%';
    toast.appendChild(bar);

    document.body.appendChild(toast);
    toastStack.push(toast);
    requestAnimationFrame(function () {
      toast.classList.add('show');
    });

    // Progress bar countdown
    bar.style.transitionDuration = duration + 'ms';
    requestAnimationFrame(function () {
      requestAnimationFrame(function () {
        bar.style.width = '0%';
      });
    });

    // Auto-dismiss
    toast._autoTimeout = setTimeout(function () {
      dismissToast(toast);
    }, duration);

    // Announce to screen readers
    announceToSR(message);
  };

  function dismissToast(toast) {
    if (!toast.parentNode) return;
    if (toast._autoTimeout) {
      clearTimeout(toast._autoTimeout);
      toast._autoTimeout = null;
    }
    toast.style.opacity = '0';
    toast.style.transform = 'translateX(40px)';
    setTimeout(function () {
      if (toast.parentNode) toast.parentNode.removeChild(toast);
      var idx = toastStack.indexOf(toast);
      if (idx > -1) toastStack.splice(idx, 1);
      repositionToasts();
    }, 300);
  }

  function repositionToasts() {
    toastStack.forEach(function (t, i) {
      t.style.top = 20 + i * 80 + 'px';
    });
  }

  // ==========================================================================
  // Section 6: Accessibility Helpers
  // ==========================================================================
  function createA11yElements() {
    // Skip-to-content link
    var skip = document.createElement('a');
    skip.id = 'mst-skip-link';
    skip.href = '#main-content';
    skip.textContent = 'Skip to content';
    document.body.insertBefore(skip, document.body.firstChild);

    // Mark main content area
    var tabContent = document.querySelector('.tab-content');
    if (tabContent) {
      tabContent.setAttribute('role', 'main');
      tabContent.id = tabContent.id || 'main-content';
    } else {
      var main = document.querySelector(
        ".container-fluid, main, [role='main']",
      );
      if (main && !main.id) main.id = 'main-content';
    }

    // Screen reader live region
    var sr = document.createElement('div');
    sr.id = 'mst-sr-announcer';
    sr.setAttribute('aria-live', 'polite');
    sr.setAttribute('role', 'status');
    document.body.appendChild(sr);
  }

  function announceToSR(message) {
    var sr = document.getElementById('mst-sr-announcer');
    if (sr) {
      sr.textContent = '';
      setTimeout(function () {
        sr.textContent = message;
      }, 50);
    }
  }

  // ==========================================================================
  // Section 7: Back-to-Top Button
  // ==========================================================================
  function createBackToTop() {
    var btn = document.createElement('button');
    btn.id = 'mst-back-to-top';
    btn.textContent = '\u2191';
    btn.setAttribute('aria-label', 'Scroll back to top');
    document.body.appendChild(btn);

    btn.addEventListener('click', function () {
      window.scrollTo({ top: 0, behavior: 'smooth' });
    });

    window.addEventListener(
      'scroll',
      function () {
        btn.classList.toggle('visible', window.scrollY > 300);
      },
      { passive: true },
    );
  }

  // ==========================================================================
  // Section 8: Top Loading Bar & Progress Pulse
  // ==========================================================================
  function createLoadingBar() {
    var bar = document.createElement('div');
    bar.id = 'mst-loading-bar';
    document.body.appendChild(bar);

    $(document).on('shiny:busy', function () {
      bar.style.width = '70%';
      // Pulse any Run button
      document
        .querySelectorAll("[id*='run'], [id*='Run']")
        .forEach(function (b) {
          b.style.animation = 'mst-pulse 1s infinite';
        });
    });

    $(document).on('shiny:idle', function () {
      bar.style.width = '100%';
      setTimeout(function () {
        bar.style.transition = 'none';
        bar.style.width = '0';
        requestAnimationFrame(function () {
          bar.style.transition = 'width 0.3s ease';
        });
      }, 300);
      document
        .querySelectorAll("[id*='run'], [id*='Run']")
        .forEach(function (b) {
          b.style.animation = '';
        });
    });

    // Inject pulse keyframe
    var s = document.createElement('style');
    s.textContent =
      '@keyframes mst-pulse{0%,100%{opacity:1;}50%{opacity:0.5;}}';
    document.head.appendChild(s);
  }

  // ==========================================================================
  // Section 9: Workflow Card Tilt Effect
  // ==========================================================================
  function initCardTilt() {
    document.querySelectorAll("[id^='dash_card_']").forEach(function (card) {
      card.addEventListener('mousemove', function (e) {
        if (card._tiltRAF) return;
        card._tiltRAF = requestAnimationFrame(function () {
          var rect = card.getBoundingClientRect();
          var x = (e.clientX - rect.left) / rect.width - 0.5;
          var y = (e.clientY - rect.top) / rect.height - 0.5;
          card.style.transform =
            'perspective(600px) rotateY(' +
            x * 5 +
            'deg) rotateX(' +
            -y * 5 +
            'deg)';
          card._tiltRAF = null;
        });
      });
      card.addEventListener('mouseleave', function () {
        card.style.transform = 'perspective(600px) rotateY(0deg) rotateX(0deg)';
      });
    });
  }

  // ==========================================================================
  // Section 10: Tab Transitions & Browser Title
  // ==========================================================================
  function initTabTransitions() {
    var baseTitle = document.title;
    $(document).on('shown.bs.tab', '.navbar .nav-link', function (e) {
      var tabName = e.target.textContent.trim();
      document.title = tabName + ' - ' + baseTitle;
      // Fade transition on the active pane
      var href =
        e.target.getAttribute('data-bs-target') ||
        e.target.getAttribute('href');
      if (href) {
        var pane = document.querySelector(href);
        if (pane) {
          pane.style.opacity = '0';
          pane.style.transition = 'opacity 0.25s ease';
          requestAnimationFrame(function () {
            pane.style.opacity = '1';
          });
        }
      }
    });
  }

  // ==========================================================================
  // Section 11: DataTable Enhancements
  // ==========================================================================
  function initTableEnhancements() {
    // Observe new DataTables being added
    var mutationTimer = null;
    var observer = new MutationObserver(function (mutations) {
      if (mutationTimer) return;
      mutationTimer = setTimeout(function () {
        mutationTimer = null;
      }, 150);
      mutations.forEach(function (m) {
        m.addedNodes.forEach(function (node) {
          if (node.nodeType !== 1) return;
          var wrappers =
            node.classList && node.classList.contains('dataTables_wrapper')
              ? [node]
              : node.querySelectorAll
                ? Array.from(node.querySelectorAll('.dataTables_wrapper'))
                : [];
          wrappers.forEach(function (w) {
            // Fade in
            w.style.opacity = '0';
            w.style.transition = 'opacity 0.35s ease';
            requestAnimationFrame(function () {
              w.style.opacity = '1';
            });
            // Copy button
            if (!w.querySelector('.mst-copy-btn')) {
              w.style.position = 'relative';
              var btn = document.createElement('button');
              btn.className = 'mst-copy-btn';
              btn.textContent = 'Copy';
              btn.addEventListener('click', function () {
                var table = w.querySelector('table');
                if (table) {
                  var text = table.innerText;
                  if (navigator.clipboard && navigator.clipboard.writeText) {
                    navigator.clipboard
                      .writeText(text)
                      .then(function () {
                        btn.textContent = 'Copied!';
                        setTimeout(function () {
                          btn.textContent = 'Copy';
                        }, 1500);
                      })
                      .catch(function () {
                        btn.textContent = 'Failed';
                        setTimeout(function () {
                          btn.textContent = 'Copy';
                        }, 1500);
                      });
                  } else {
                    // Fallback for non-secure contexts (HTTP on LAN)
                    var textarea = document.createElement('textarea');
                    textarea.value = text;
                    textarea.style.cssText = 'position:fixed;opacity:0;';
                    document.body.appendChild(textarea);
                    textarea.select();
                    try {
                      document.execCommand('copy');
                      btn.textContent = 'Copied!';
                    } catch (err) {
                      btn.textContent = 'Failed';
                    }
                    document.body.removeChild(textarea);
                    setTimeout(function () {
                      btn.textContent = 'Copy';
                    }, 1500);
                  }
                }
              });
              w.appendChild(btn);
            }
          });
        });
      });
    });
    observer.observe(document.body, { childList: true, subtree: true });

    // Fix DataTables column alignment when tabs become visible.
    // Tables rendered in hidden tabs have zero-width columns until shown.
    document.addEventListener('shown.bs.tab', function () {
      setTimeout(function () {
        if (typeof $ !== 'undefined' && $.fn.dataTable) {
          $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust();
        }
      }, 100);
    });
  }

  // ==========================================================================
  // Section 12: Viewport Reveal (IntersectionObserver)
  // ==========================================================================
  function initViewportReveal() {
    if (!('IntersectionObserver' in window)) return;
    var io = new IntersectionObserver(
      function (entries) {
        entries.forEach(function (entry) {
          if (entry.isIntersecting) {
            entry.target.classList.add('visible');
            io.unobserve(entry.target);
          }
        });
      },
      { threshold: 0.1 },
    );

    // Tag sidebar cards and large content sections
    document.querySelectorAll('.card, .well, .box').forEach(function (el) {
      if (!el.closest('.navbar')) {
        el.classList.add('mst-reveal');
        io.observe(el);
      }
    });
  }

  // ==========================================================================
  // Section 13: Sidebar Input Focus Scroll
  // ==========================================================================
  function initSidebarFocusScroll() {
    document.addEventListener('focusin', function (e) {
      var sidebar = e.target.closest(".sidebar, .well, [class*='sidebar']");
      if (
        sidebar &&
        (e.target.tagName === 'INPUT' ||
          e.target.tagName === 'SELECT' ||
          e.target.tagName === 'TEXTAREA')
      ) {
        e.target.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
      }
    });
  }

  // ==========================================================================
  // Section 14: Konami Code Easter Egg
  // ==========================================================================
  function initKonami() {
    document.addEventListener('keydown', function (e) {
      if (e.keyCode === konamiSeq[konamiPos]) {
        konamiPos++;
        if (konamiPos === konamiSeq.length) {
          konamiPos = 0;
          triggerConfetti();
        }
      } else {
        konamiPos = 0;
      }
    });
  }

  function triggerConfetti() {
    var container = document.createElement('div');
    container.className = 'mst-confetti';
    document.body.appendChild(container);
    var colors = [
      '#ff6b6b',
      '#ffd93d',
      '#6bcb77',
      '#4d96ff',
      '#ff6fff',
      '#ff9f43',
    ];
    for (var i = 0; i < 80; i++) {
      var piece = document.createElement('div');
      piece.className = 'mst-confetti-piece';
      piece.style.left = Math.random() * 100 + '%';
      piece.style.top = '-10px';
      piece.style.background =
        colors[Math.floor(Math.random() * colors.length)];
      piece.style.borderRadius = Math.random() > 0.5 ? '50%' : '0';
      piece.style.transform = 'rotate(' + Math.random() * 360 + 'deg)';
      var dur = 1.5 + Math.random() * 2;
      piece.style.animation =
        'mst-confetti-fall ' + dur + 's ease-out forwards';
      piece.style.animationDelay = Math.random() * 0.5 + 's';
      container.appendChild(piece);
    }
    // Inject confetti keyframe if not present
    if (!document.getElementById('mst-confetti-style')) {
      var s = document.createElement('style');
      s.id = 'mst-confetti-style';
      s.textContent =
        '@keyframes mst-confetti-fall{0%{transform:translateY(0) rotate(0deg);opacity:1;}' +
        '100%{transform:translateY(100vh) rotate(720deg);opacity:0;}}';
      document.head.appendChild(s);
    }
    setTimeout(function () {
      if (container.parentNode) container.parentNode.removeChild(container);
    }, 4000);
    announceToSR('Surprise! Confetti animation triggered.');
  }

  // ==========================================================================
  // Section 15: Keyboard Shortcuts
  // ==========================================================================
  document.addEventListener('keydown', function (e) {
    // Ctrl/Cmd + Shift + D => toggle dark mode
    if ((e.ctrlKey || e.metaKey) && e.shiftKey && e.key === 'D') {
      e.preventDefault();
      var btn = document.getElementById('theme_toggle');
      if (btn) btn.click();
    }
    // Ctrl/Cmd + Shift + 1-7 => navigate tabs
    if (
      (e.ctrlKey || e.metaKey) &&
      e.shiftKey &&
      e.key >= '1' &&
      e.key <= '7'
    ) {
      e.preventDefault();
      var idx = parseInt(e.key, 10) - 1;
      var tabs = document.querySelectorAll('.navbar .nav-link');
      if (tabs[idx]) tabs[idx].click();
    }
  });

  // ==========================================================================
  // Section 16: Smooth Scroll for Anchor Links
  // ==========================================================================
  document.addEventListener('click', function (e) {
    var link = e.target.closest('a[href^="#"]');
    if (link) {
      try {
        var target = document.querySelector(link.getAttribute('href'));
        if (target) {
          e.preventDefault();
          target.scrollIntoView({ behavior: 'smooth', block: 'start' });
        }
      } catch (err) {
        // Invalid selector in href — ignore
      }
    }
  });

  // ==========================================================================
  // Section 17: File Drag-and-Drop Enhancement
  // ==========================================================================
  function initDragDrop() {
    var dropAreas = document.querySelectorAll('.file-upload-area');
    dropAreas.forEach(function (area) {
      area.addEventListener('dragover', function (e) {
        e.preventDefault();
        area.classList.add('drag-over');
      });
      area.addEventListener('dragleave', function () {
        area.classList.remove('drag-over');
      });
      area.addEventListener('drop', function () {
        area.classList.remove('drag-over');
      });
    });
  }

  // ==========================================================================
  // Section 18: Dashboard Card Click Handlers
  // ==========================================================================
  function initCardClicks() {
    Object.keys(cardNavMap).forEach(function (cardId) {
      var card = document.getElementById(cardId);
      if (card) {
        card.style.cursor = 'pointer';
        card.addEventListener('click', function () {
          var tabs = document.querySelectorAll('.navbar .nav-link');
          for (var i = 0; i < tabs.length; i++) {
            if (tabs[i].textContent.trim() === cardNavMap[cardId]) {
              tabs[i].click();
              break;
            }
          }
        });
      }
    });
  }

  // ==========================================================================
  // Section 19: Number Badge Counter Animation
  // ==========================================================================
  function animateCounters() {
    document
      .querySelectorAll(".badge, .workflow-step-number, [class*='step-number']")
      .forEach(function (el) {
        var val = parseInt(el.textContent, 10);
        if (isNaN(val) || val < 1 || val > 20) return;
        el.textContent = '0';
        var count = 0;
        var interval = setInterval(function () {
          count++;
          el.textContent = count;
          if (count >= val) clearInterval(interval);
        }, 80);
      });
  }

  // ==========================================================================
  // Section 20: Main Initialization
  // ==========================================================================
  document.addEventListener('DOMContentLoaded', function () {
    // Guard: ensure jQuery is available (Shiny loads it, but timing may vary)
    if (typeof $ === 'undefined') {
      if (typeof jQuery !== 'undefined') {
        var $ = jQuery;
      } else {
        console.warn(
          'MStargetR: jQuery not found, some features will be disabled',
        );
      }
    }

    injectStyles();
    createA11yElements();

    // Theme init
    var stored = getStoredTheme();
    if (stored) applyTheme(stored);

    // Theme toggle button binding
    var btn = document.getElementById('theme_toggle');
    if (btn) {
      btn.addEventListener('click', function () {
        var current =
          document.documentElement.getAttribute('data-bs-theme') || 'light';
        var next = current === 'dark' ? 'light' : 'dark';
        applyTheme(next);
        setStoredTheme(next);
      });
    }

    // Shiny custom message handlers -- deferred to shiny:connected so
    // they work even when Shiny loads after DOMContentLoaded.
    $(document).on('shiny:connected', function () {
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        try {
          Shiny.addCustomMessageHandler('mst-set-theme', function (data) {
            var theme = typeof data === 'string' ? data : data.theme;
            applyTheme(theme);
            setStoredTheme(theme);
          });
          Shiny.addCustomMessageHandler('mst-notify', function (data) {
            window.MStargetR.notify(data.message, data.type, data.duration);
          });
          Shiny.addCustomMessageHandler(
            'mst-toggle-run-buttons',
            function (data) {
              document.querySelectorAll('.btn-run').forEach(function (btn) {
                if (data.disabled) {
                  btn.setAttribute('disabled', 'disabled');
                  btn.classList.add('disabled');
                } else {
                  btn.removeAttribute('disabled');
                  btn.classList.remove('disabled');
                }
              });
            },
          );
        } catch (e) {
          // Handlers already registered from a previous connection
        }
      }
      var s = getStoredTheme();
      if (s) applyTheme(s);
    });

    // Initialize all features
    animatePageLoad();
    initDragDrop();
    initCardClicks();
    initCardTilt();
    initTabTransitions();
    initTableEnhancements();
    initSidebarFocusScroll();
    initKonami();
    createBackToTop();
    createLoadingBar();
    animateCounters();
    initEmptyStateAutoHide();

    // Delayed viewport reveal (let DOM settle)
    setTimeout(initViewportReveal, 500);
  });

  // ==========================================================================
  // Section: Auto-hide empty state placeholders when outputs render
  // ==========================================================================
  function initEmptyStateAutoHide() {
    function hideRenderedEmptyStates() {
      document.querySelectorAll('.empty-state').forEach(function (el) {
        var parent = el.parentElement;
        if (!parent) return;
        // Check if any sibling Shiny output (plotly or DT) has rendered content
        var outputs = parent.querySelectorAll(
          '.plotly.html-widget, .js-plotly-plot, .datatables.html-widget',
        );
        var hasContent = false;
        outputs.forEach(function (out) {
          if (out.offsetHeight > 0 && out.children.length > 0) {
            hasContent = true;
          }
        });
        el.style.display = hasContent ? 'none' : '';
      });
    }

    // Run on Shiny output changes
    if (window.Shiny) {
      window.Shiny.addCustomMessageHandler =
        window.Shiny.addCustomMessageHandler || function () {};
    }
    // Use MutationObserver on the results page area
    var observer = new MutationObserver(function () {
      hideRenderedEmptyStates();
    });
    // Observe the entire body for output changes (childList + subtree)
    observer.observe(document.body, { childList: true, subtree: true });
    // Initial check
    setTimeout(hideRenderedEmptyStates, 1000);
  }
})();
