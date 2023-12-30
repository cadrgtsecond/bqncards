const dialog = document.querySelector("#card-dialog");
const cards = document.querySelectorAll("[js-bqn-card]");

for(const c of cards) {
  c.addEventListener("htmx:afterOnLoad", (ev) => {
    if(ev.detail.xhr.status == 200) {
      dialog.showModal();
    }
  });
}
