/* *
 * (c) Sergey Prokhorov <me@seriyps.ru>
 *
 */

$(function(){
    "use strict";

    function Output(selector) {
        // API for output area
        var self = this;
        var $el = $(selector);
        this.autoscroll = false;

        function maybeScroll() {
            if (!self.autoscroll) return;
            var b = document.body;
            var scrollPos = $el.offset().top + $el.height() - $(window).height();
            $(window).scrollTop(scrollPos + 30);
        }

        this.reset = function() {
            $el.empty()
        };

        this.addStdout = function(text) {
            var $chunk = $("<span/>");
            $chunk.text(text);
            $el.append($chunk);
            maybeScroll();
        };

        this.addStderr = this.addStdout; // TODO

        this.reportReturnCode = function(code) {
            var $chunk = $("<div/>");
            $chunk
                .text("Return code is " + code)
                .addClass( (code == 0) ? "success" : "error");
            $el.append($chunk);
        };

        this.reportTransportError = function(code, text) {
            var $chunk = $("<div/>");
            $chunk
                .text("Network error " + code + ". " + text)
                .addClass("error");
            $el.append($chunk);
        }

        this.reportTimeout = function() {
            var $chunk = $("<div/>");
            $chunk
                .text("Program execution was interrupted by timeout")
                .addClass("error");
            $el.append($chunk);
        }

        this.reportOutputTooLarge = function() {
            var $chunk = $("<div/>");
            $chunk
                .text("Program output is too large. It was truncated")
                .addClass("error");
            $el.append($chunk);
        }
    }

    function Controls(editor, editorSession, transport, output, pastebin) {
        // UI operations
        var self = this;
        var $evaluate = $("#evaluate"),
            $compile = $("#compile"),
            $emit = $("#emit"),
            $release = $("#release"),
            $keyboard = $("#kb-layout"),
            $autoscroll = $("#autoscroll"),
            $share = $("#share"),
            $sharePanel = $("#share-panel");
        var $buttons = $([$evaluate[0], $compile[0]]);
        this.$buttons = $buttons;
        this.$switches = $([$emit[0], $release[0], $keyboard[0], $autoscroll[0]]);

        output.autoscroll = $autoscroll.prop("checked");

        this.serialize = function() {
            return {
                emit: $emit.val(),
                release: $release.val(),
                keyboard: $keyboard.val(),
                autoscroll: $autoscroll.prop("checked"),
                code: editorSession.getValue()
            }
        }
        this.deserialize = function(obj) {
            var now = self.serialize();
            var changed = ["emit", "release", "keyboard", "autoscroll", "code"].filter(function(k) {
                return (k in obj) && (obj[k] != now[k]);
            });
            changed.forEach(function(k) {
                var v = obj[k];
                switch (k) {
                    case "emit":
                    $emit.val(v).trigger("change")
                    break
                    case "release":
                    $release.val(v).trigger("change")
                    break
                    case "keyboard":
                    $keyboard.val(v).trigger("change")
                    break
                    case "autoscroll":
                    $autoscroll.prop("checked", v).trigger("change")
                    break
                    case "code":
                    editorSession.setValue(v)
                    break
                }
            })
        }

        var progress = false;
        this.evaluate = function() {
            if (progress) {
                alert("In progress");
                return;
            }
            progress = true;
            $buttons.prop("disabled", true);

            transport
                .evaluate(editorSession.getValue(), $release.val())
                .always(function() {
                    progress = false;
                    $buttons.prop("disabled", false);
                })
        };
        $evaluate.on('click', this.evaluate);

        this.compile = function() {
            if (progress) {
                alert("In progress");
                return;
            }
            progress = true;
            $buttons.prop("disabled", true);

            transport
                .compile(editorSession.getValue(), $release.val(), $emit.val())
                .always(function() {
                    progress = false;
                    $buttons.prop("disabled", false);
                })
        };
        $compile.on('click', this.compile);

        $share.on('click', function() {
            $share.prop("disabled", true); // enabled in resetShare to avoid duplicates
            var state = self.serialize();
            pastebin.create(state.code, state.release, state.emit)
                .done(function(_1, _2, jqXHR) {
                    var location = jqXHR.getResponseHeader("location");
                    var id = location.split("/").pop();
                    var origin = document.location.origin;
                    var hash = '#id=' + id;
                    var shareUrl = origin + "/" + hash;
                    document.location.hash = hash;
                    var $a = $("<a/>", {href: shareUrl,
                                    text: shareUrl});
                    $sharePanel.append($a).show();
                }).fail(function(jqXHR, textStatus) {
                    $sharePanel.text("Error #" + jqXHR.status + ". " + textStatus + ". " + jqXHR.responseText)
                    .show();
                })
        });

        function resetShare() {
            $share.prop("disabled", false);
            $sharePanel
                .hide()
                .empty();
            document.location.hash = "";
        }
        editorSession.on("change", resetShare);
        this.$switches.on('change', resetShare);

        $keyboard.on('change', function() {
            editor.setKeyboardHandler($keyboard.val() || null);
        })
        $autoscroll.on('change', function() {
            output.autoscroll = $autoscroll.prop("checked");
        })

        editor.commands.addCommand({
            name: "evaluate",
            exec: this.evaluate,
            bindKey: {win: "Ctrl-Enter", mac: "Ctrl-Enter"}
        });
        editor.commands.addCommand({
            name: "compile",
            exec: this.compile,
            bindKey: {win: "Ctrl-Shift-Enter", mac: "Ctrl-Shift-Enter"}
        });
    }

    function TextTransport(output) {
        // AJAX + plaintext
        var $spinner = $("#transport-spinner");
        var timer = null;

        $(document).ajaxSend(function() {
            clearTimeout(timer);
            timer = setTimeout(function() {$spinner.show()}, 1000);
        }).ajaxComplete(function() {
            clearTimeout(timer);
            $spinner.hide()
        }).ajaxError(function() {
            clearTimeout(timer);
            $spinner.hide()
        })

        // contentType: "application/x-www-form-urlencoded" also accepted
        this.compile = function(code, release, emit) {
            return $.ajax("/api/compile", {
                type: 'POST',
                data : JSON.stringify({
                    code: code,
                    release: release,
                    output_format: emit
                }),
                contentType: "application/json",
                dataType: "text"
            }).done(function(data) {
                output.reset();
                output.addStdout(data);
            }).fail(function(jqXHR, textStatus) {
                output.reset();
                output.reportTransportError(
                    jqXHR.status,
                    textStatus + ".\n" + jqXHR.responseText);
            })
        }
        this.evaluate = function(code, release) {
            output.reset();
            var progressOffset = 0;
            return $.ajax("/api/evaluate", {
                type: 'POST',
                data : JSON.stringify({
                    code: code,
                    release: release
                }),
                contentType: "application/json",
                dataType: "text",
                xhrFields: {
                    onprogress: function(e) {
                        // stream output from chunked long-running response
                        var resp = e.currentTarget.response;
                        if (resp.length == progressOffset) return;
                        output.addStdout(resp.substring(progressOffset));
                        progressOffset = resp.length;
                    }
                }
            }).done(function(data) {
                output.reset();
                output.addStdout(data);
            }).fail(function(jqXHR, textStatus) {
                output.reportTransportError(
                    jqXHR.status,
                    textStatus + ".\n" + jqXHR.responseText);
            })
        }
    }
    function ByteProtocol() {}  // TODO: Accept: octet/stream

    function Pickler(editorSession, control, pastebin) {
        // restore controls and editor state from localstore or URL hash
        var qs = (function() {
            if (!document.location.hash) return {};
            var qs = document.location.hash.toString().substring(1);

            function parseQS(qs) {
                // http://stackoverflow.com/a/2880929/410556
                var match,
                    pl     = /\+/g,  // Regex for replacing addition symbol with a space
                    search = /([^&=]+)=?([^&]*)/g,
                    decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); };

                var urlParams = {};
                while (match = search.exec(qs)) {
                    var v = decode(match[2]);
                    if (v == "true")
                        v = true;
                    else if (v == "false")
                        v = false;
                    urlParams[decode(match[1])] = v;
                }
                return urlParams;
            }
            return parseQS(qs);
        })();
        console.log(qs, "id" in qs);
        if ("id" in qs) {
            pastebin.get(qs.id)
                .done(function(o) {
                    control.deserialize(o);
                }).fail(function(jqXHR) {
                    alert("Can't restore snippet. Error #" + jqXHR.status + ". " + jqXHR.responseText);
                });
            return              // don't try other pickle methods
        }
        (function() {
            // from local storage
            var switches = localStorage.getItem("switches");
            var state = switches ? JSON.parse(switches) : {};
            var code = localStorage.getItem("code");
            if (code !== null) {
                state["code"] = code;
            }
            control.deserialize(state);
        })();

        (function() {
            // from hash
            control.deserialize(qs);

            if ("do_evaluate" in qs) control.evaluate();
            else if ("do_compile" in qs) control.compile();
        })();

        editorSession.on("change", function() {
            localStorage.setItem("code", editorSession.getValue());
        });
        control.$switches.on('change', function() {
            var state = control.serialize()
            localStorage.setItem("code", state["code"]);
            delete state["code"];
            localStorage.setItem("switches", JSON.stringify(state));
        })
    }

    function Pastebin() {
        this.create = function(code, release, emit) {
            return $.ajax("/api/pastebin", {
                type: 'POST',
                data : JSON.stringify({
                    code: code,
                    release: release,
                    emit: emit
                }),
                contentType: "application/json",
                dataType: "text"
            });// .done(function(data, _, jqXHR) {
            //     var location = jqXHR.getResponseHeader("location");
            // }).fail(function(jqXHR, textStatus) {
            //     // output.reset();
            //     // output.reportTransportError(
            //     //     jqXHR.status,
            //     //     textStatus + ".\n" + jqXHR.responseText);
            // })
        };
        this.get = function(id) {
            return $.ajax("/api/pastebin/" + id, {
                type: 'GET',
                dataType: "json"
            });// .done(function(data) {
            // }).fail(function(jqXHR, textStatus) {
            //     // output.reset();
            //     // output.reportTransportError(
            //     //     jqXHR.status,
            //     //     textStatus + ".\n" + jqXHR.responseText);
            // })
        };
    }

    var editor = ace.edit("editor");
    var session = editor.getSession();

    editor.setTheme("ace/theme/github");
    session.setMode("ace/mode/erlang");
    editor.focus();
    // var nLines = session.getLength();
    // editor.gotoLine(nLines, session.getLine(nLines - 1).length);

    var output = new Output("#result");
    var transport = new TextTransport(output);
    var pastebin = new Pastebin();
    var control = new Controls(editor, session, transport, output, pastebin);
    var pickler = new Pickler(session, control, pastebin);
})
