// showkitr glossary cards: three small behaviours, no dependencies.
// 1. theme buttons switch <html data-theme>
// 2. pointing at an argument highlights its line in the signature
// 3. "Copy" buttons copy the example
(function () {
  var root = document.documentElement;

  var themeBtns = document.querySelectorAll('.seg button');
  themeBtns.forEach(function (btn) {
    btn.addEventListener('click', function () {
      root.setAttribute('data-theme', btn.getAttribute('data-theme'));
      themeBtns.forEach(function (b) { b.setAttribute('aria-pressed', String(b === btn)); });
    });
  });

  document.querySelectorAll('.arg[data-arg]').forEach(function (row) {
    var card = row.closest('.card');
    var set = function (on) {
      var line = card.querySelector('.ln[data-arg="' + CSS.escape(row.getAttribute('data-arg')) + '"]');
      if (line) line.classList.toggle('on', on);
    };
    row.addEventListener('mouseenter', function () { set(true); });
    row.addEventListener('mouseleave', function () { set(false); });
    row.addEventListener('focusin', function () { set(true); });
    row.addEventListener('focusout', function () { set(false); });
  });

  document.querySelectorAll('[data-copy]').forEach(function (btn) {
    btn.addEventListener('click', function () {
      var pre = btn.closest('.ex').querySelector('pre');
      if (!navigator.clipboard) return;
      navigator.clipboard.writeText(pre.innerText.trim()).then(function () {
        var label = btn.querySelector('span');
        var old = label.textContent;
        label.textContent = 'Copied';
        setTimeout(function () { label.textContent = old; }, 1500);
      });
    });
  });
})();
