function show_results (show) {
    if (show)
        $("#results").show();
    else
        $("#results").hide();
}

function init () {
    jQuery(".numbersOnly").keyup(function() { 
        this.value = this.value.replace(/[^0-9\.]/g, "");
    });

    $("#account_number").numeric();
    $("#new_account_number").numeric();
    $("#new_amount").numeric();
    $("#create_error").hide();

    show_results(false);
}

function post (json, url, callback) {
    var data = JSON.stringify(json);
    $.post(url, {"json" : data}, callback, "json");
}

function update_status (json) {
    var msg = json[0].message;

    if (msg != "ok") {
        $("#msg").text("ERROR: " + msg);
        show_results(false);
        return false;
    }

    $("#msg").text(msg);

    return true;
}

function callback_show (json) {
    if (!update_status(json))
        return;

    $("#results_available").text(json[0].amount_available);
    $("#results_reserved").text(json[0].amount_reserved);
    $("#results_total").text(json[0].amount_available + json[0].amount_reserved);

    if (json[0].transactions)
        $("#transactions").html(json[0].transactions);

    show_results(true);
}

function update_results () {
    var json = {
        "action"         : "search",
        "account_number" : parseInt($("#account_number").val())
    };

    post(json, "/json", callback_show);
}

function update_amounts () {
    var json = {
        "action"         : "amounts",
        "account_number" : parseInt($("#account_number").val())
    };

    post(json, "/json", callback_show);
}

function confirm_transaction(self) {
    var guid = self.parent().parent().parent().attr("id");
    var json = {
        "action" : "confirm",
        "guid"   : guid
    };

    post(json, "/json", function(json) {
        if (json[0].message == "ok") {
            self.parent().text("");
            $("#" + guid + " td:nth-child(3)").text("charged");
            update_amounts();
        }
    });
}

function cancel_transaction(self) {
    var row = self.parent().parent().parent();
    var guid = row.attr("id");
    var json = {
        "action" : "cancel",
        "guid"   : guid
    };

    post(json, "/json", function(json) {
        if (json[0].message == "ok") {
            row.fadeOut(500).children("td").each(function () {
                $(this).children("div").slideUp(600, function() {
                    row.remove();
                });
            });

            update_amounts();
        }
    });
}

function callback_create(json) {
    if (json[0].message != "ok") {
        $("#create_error").text("ERROR: " + json[0].message).show();
    }
    else {
        $("#overlay").remove();
        $("#create_div").css("display", "none");
        $("#create_error").hide();
    }
}

$(document).ready(function() {
    $("#search_form").submit(function() {
        return false;
    });

    $("#delete_account").click(function() {
	var json = {
	    "action"         : "delete",
            "account_number" : parseInt($("#account_number").val())
	};

        show_results(false);
	post(json, "/json", update_status);
    });

    $("#show_account").click(function() {
        update_results();
    });

    $("#create_account").click(function() {
        $("#search_form").before("<div id='overlay'></div>");
        $("#create_div").css("display", "block");
        $("#new_account_number").focus();
    });

    $("#create_form").submit(function() {
        return false;
    });

    $("#new_cancel").click(function() {
        $("#overlay").remove();
        $("#create_div").css("display", "none");
        $("#create_error").hide();
    });

    $("#new_accept").click(function() {
        var json = {
            "action"         : "create",
            "account_number" : parseInt($("#new_account_number").val()),
            "amount"         : parseInt($("#new_amount").val())
        };

        post(json, "/json", callback_create);
    });

    init();
});
