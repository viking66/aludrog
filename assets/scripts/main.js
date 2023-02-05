const img = document.querySelector("img");

img.onclick = () => {
  const src = img.getAttribute("src");
  if (src == "img/rambutan.jpg") {
    img.setAttribute("src", "img/bam.png");
  } else {
    img.setAttribute("src", "img/rambutan.jpg");
  }
};