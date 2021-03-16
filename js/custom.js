function activeSourceLine() {
    var ic = document.getElementById('active-state-ic').textContent;
    var source = document.querySelector('.listing').children;
    for (var i = 0; i < source.length; i++) {
        const cid = source[i].id;
        if (cid === ic) {
            source[i].classList.add("active");
        } else {
            source[i].classList.remove("active");
        }
    }
}
