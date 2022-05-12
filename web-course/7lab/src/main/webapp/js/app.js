window.notify = function (message) {
    $.notify(message, {
        position: "right bottom",
        className: "success"
    });
}



ajaxForm = function (data, $error) {
    const success =  function (response) {
        if (response["error"]) {
            $error.text(response["error"]);
        } else {
            location.href = response["redirect"];
        }
    }
    $.ajax({
        type: "POST",
        dataType: "json",
        data,
        success
    })
}

ajax = function (data, success) {
    $.ajax({
        type: "POST",
        dataType: "json",
        data,
        success
    })
}


