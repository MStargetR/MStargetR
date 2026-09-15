// Lightbox for zoomable README figures on the pkgdown site.
// Click a figure link to open it full screen; scroll or pinch to zoom, drag to
// pan, double-click to reset, Esc or click the backdrop to close. On GitHub
// (no JS) the same link just opens the full-resolution image in a new tab.
(function () {
  'use strict';

  var MIN_SCALE = 1;
  var MAX_SCALE = 12;

  function openLightbox(src, alt) {
    var overlay = document.createElement('div');
    overlay.className = 'mst-lightbox';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', alt || 'Enlarged figure');

    var img = document.createElement('img');
    img.src = src;
    img.alt = alt || '';
    img.draggable = false;

    var close = document.createElement('button');
    close.type = 'button';
    close.className = 'mst-lightbox-close';
    close.setAttribute('aria-label', 'Close');
    close.innerHTML = '&times;';

    var hint = document.createElement('div');
    hint.className = 'mst-lightbox-hint';
    hint.textContent =
      'Scroll or pinch to zoom · drag to pan · double-click to reset · Esc to close';

    overlay.appendChild(img);
    overlay.appendChild(close);
    overlay.appendChild(hint);
    document.body.appendChild(overlay);
    document.body.classList.add('mst-lightbox-open');

    var scale = 1;
    var tx = 0;
    var ty = 0;
    var pointers = {};
    var lastPinchDist = null;

    function apply() {
      img.style.transform =
        'translate(' + tx + 'px,' + ty + 'px) scale(' + scale + ')';
      img.style.cursor = scale > 1 ? 'grab' : 'zoom-in';
    }

    // Zoom around a viewport point so the content under the cursor stays put.
    function zoomAt(factor, clientX, clientY) {
      var next = Math.min(MAX_SCALE, Math.max(MIN_SCALE, scale * factor));
      if (next === scale) return;
      var cx = clientX - window.innerWidth / 2;
      var cy = clientY - window.innerHeight / 2;
      tx = cx - (cx - tx) * (next / scale);
      ty = cy - (cy - ty) * (next / scale);
      scale = next;
      if (scale === 1) {
        tx = 0;
        ty = 0;
      }
      apply();
    }

    function reset() {
      scale = 1;
      tx = 0;
      ty = 0;
      apply();
    }

    function destroy() {
      document.removeEventListener('keydown', onKey);
      document.body.classList.remove('mst-lightbox-open');
      overlay.remove();
    }

    function onKey(e) {
      if (e.key === 'Escape') destroy();
      else if (e.key === '+' || e.key === '=')
        zoomAt(1.25, window.innerWidth / 2, window.innerHeight / 2);
      else if (e.key === '-')
        zoomAt(0.8, window.innerWidth / 2, window.innerHeight / 2);
      else if (e.key === '0') reset();
    }

    overlay.addEventListener(
      'wheel',
      function (e) {
        e.preventDefault();
        zoomAt(e.deltaY < 0 ? 1.15 : 1 / 1.15, e.clientX, e.clientY);
      },
      { passive: false },
    );

    img.addEventListener('dblclick', function (e) {
      e.preventDefault();
      if (scale > 1) reset();
      else zoomAt(3, e.clientX, e.clientY);
    });

    img.addEventListener('pointerdown', function (e) {
      img.setPointerCapture(e.pointerId);
      pointers[e.pointerId] = { x: e.clientX, y: e.clientY };
    });

    img.addEventListener('pointermove', function (e) {
      var prev = pointers[e.pointerId];
      if (!prev) return;
      var ids = Object.keys(pointers);
      if (ids.length === 2) {
        pointers[e.pointerId] = { x: e.clientX, y: e.clientY };
        var a = pointers[ids[0]];
        var b = pointers[ids[1]];
        var dist = Math.hypot(a.x - b.x, a.y - b.y);
        if (lastPinchDist)
          zoomAt(dist / lastPinchDist, (a.x + b.x) / 2, (a.y + b.y) / 2);
        lastPinchDist = dist;
        return;
      }
      var dx = e.clientX - prev.x;
      var dy = e.clientY - prev.y;
      pointers[e.pointerId] = { x: e.clientX, y: e.clientY };
      if (scale > 1) {
        tx += dx;
        ty += dy;
        img.style.cursor = 'grabbing';
        img.style.transform =
          'translate(' + tx + 'px,' + ty + 'px) scale(' + scale + ')';
      }
    });

    function endPointer(e) {
      delete pointers[e.pointerId];
      if (Object.keys(pointers).length < 2) lastPinchDist = null;
      if (scale > 1) img.style.cursor = 'grab';
    }
    img.addEventListener('pointerup', endPointer);
    img.addEventListener('pointercancel', endPointer);

    overlay.addEventListener('click', function (e) {
      if (e.target === overlay) destroy();
    });
    close.addEventListener('click', destroy);
    document.addEventListener('keydown', onKey);

    apply();
    close.focus();
  }

  document.addEventListener('click', function (e) {
    var link =
      e.target.closest &&
      e.target.closest(
        'a.zoomable-figure, a[href$="man/figures/architecture.png"]',
      );
    if (!link) return;
    var img = link.querySelector('img');
    if (!img) return;
    if (e.ctrlKey || e.metaKey || e.shiftKey || e.button !== 0) return; // let "open in new tab" work
    e.preventDefault();
    openLightbox(img.currentSrc || img.src, img.alt);
  });
})();
