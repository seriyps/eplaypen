/* *
 * (C) Sergey Prokhorov <me@seriyps.ru>
 *
 */


"use strict";
$(function(){

    function Output(selector) {
        var self = this;
        var $el = $(selector);
        this.autoscroll = false;

        function maybeScroll() {
            if (!self.autoscroll) return;
            var b = document.body;
            $(b).scrollTop(b.scrollHeight);
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

    function Controls(editor, editorSession, transport, output) {
        var self = this;
        var $evaluate = $("#evaluate"),
            $compile = $("#compile"),
            $emit = $("#emit"),
            $release = $("#release"),
            $keyboard = $("#kb-layout"),
            $autoscroll = $("#autoscroll");
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

        this.evaluate = function() {
            $buttons.prop("disabled", true);

            transport
                .evaluate(editorSession.getValue(), $release.val())
                .always(function() {
                    $buttons.prop("disabled", false);
                })
        };
        $evaluate.on('click', this.evaluate);

        this.compile = function() {
            $buttons.prop("disabled", true);

            transport
                .compile(editorSession.getValue(), $release.val(), $emit.val())
                .always(function() {
                    $buttons.prop("disabled", false);
                })
        };
        $compile.on('click', this.compile);

        $keyboard.on('change', function() {
            editor.setKeyboardHandler($keyboard.val() || null);
        })
        $autoscroll.on('change', function() {
            output.autoscroll = $autoscroll.prop("checked");
        })

        editor.commands.addCommand({
            name: "evaluate",
            exec: evaluate,
            bindKey: {win: "Ctrl-Enter", mac: "Ctrl-Enter"}
        });
        editor.commands.addCommand({
            name: "compile",
            exec: compile,
            bindKey: {win: "Ctrl-Shift-Enter", mac: "Ctrl-Shift-Enter"}
        });
    }

    function TextTransport(output) {
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
                output.reportTransportError(jqXHR.status, textStatus);
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
                output.reportTransportError(jqXHR.status, textStatus);
            })
        }
    }
    function ByteProtocol() {}  // TODO: Accept: octet/stream

    function Pickler(editorSession, control) {
        // restore

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
            if (!document.location.hash) return;
            var qs = document.location.hash.toString().substring(1);

            function parseQS(qs) {
                // http://stackoverflow.com/a/2880929/410556
                var match,
                    pl     = /\+/g,  // Regex for replacing addition symbol with a space
                    search = /([^&=]+)=?([^&]*)/g,
                    decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); };

                console.log(qs)
                var urlParams = {};
                while (match = search.exec(qs)) {
                    var v = decode(match[2]);
                    if (v == "true")
                        v = true;
                    else if (v == "false")
                        v = false;
                    urlParams[decode(match[1])] = v;
                    console.log(match)
                }
                return urlParams;
            }
            var qsObj = parseQS(qs);
            control.deserialize(qsObj);

            console.log(qsObj);
            if ("do_evaluate" in qsObj) control.evaluate();
            else if ("do_compile" in qsObj) control.compile();
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
    function Pastebin() {}      // TODO

    var editor = ace.edit("editor");
    var session = editor.getSession();

    editor.setTheme("ace/theme/github");
    session.setMode("ace/mode/erlang");
    editor.focus();
    // var nLines = session.getLength();
    // editor.gotoLine(nLines, session.getLine(nLines - 1).length);

    var output = new Output("#result");
    var transport = new TextTransport(output);
    var control = new Controls(editor, session, transport, output);
    var pickler = new Pickler(session, control);
})
