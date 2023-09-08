if (window.location.protocol != "file:") {
    fetch("../../../../../../versions.json")
	.then((response) => response.json())
	.then((versions) => {
	    const div = document.getElementsByTagName("div")[0];
	    const select = document.createElement("select");
	    select.setAttribute("id", "version-select");
	    select.setAttribute("onChange", "switch_version()");
	    versions.forEach((version) => {
		const option = document.createElement("option");
		option.value = version;
		option.innerHTML = version;
		select.appendChild(option);
	    });
	    select.value = current_version;
	    div.insertBefore(select, div.firstChild);
	});
}

function switch_version() {
    const new_version = document.getElementById("version-select").value;
    const re = new RegExp("/Macaulay2(-".concat(current_version, ")?/"));
    window.location.href = window.location.href.replace(
	re, "/Macaulay2-".concat(new_version, "/"));
}
