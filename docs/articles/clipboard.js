document.addEventListener("DOMContentLoaded", function() {
  var icon = '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="#555" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="9" y="9" width="13" height="13" rx="2"/><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"/></svg>';
  var codeBlocks = document.querySelectorAll("pre.r, pre.sourceCode");
  codeBlocks.forEach(function(block) {
    var btn = document.createElement("button");
    btn.className = "copy-btn";
    btn.innerHTML = icon + '<span class="copy-tooltip">Copy to clipboard</span>';
    block.style.position = "relative";
    block.appendChild(btn);
    block.onmouseover = function(){ btn.style.opacity = "1"; };
    block.onmouseout  = function(){ btn.style.opacity = "0"; };
    btn.addEventListener("click", function() {
      navigator.clipboard.writeText(
        block.innerText.replace("Copy to clipboard","").trim()
      );
      btn.innerHTML = icon + '<span class="copy-tooltip" style="visibility:visible;opacity:1;background:#4a9;color:#fff;">Copied!</span>';
      setTimeout(function(){
        btn.innerHTML = icon + '<span class="copy-tooltip">Copy to clipboard</span>';
      }, 1500);
    });
  });
});
