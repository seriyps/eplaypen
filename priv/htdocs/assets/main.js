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

    function Controls(transport, output, pastebin) {
        // UI operations
        var self = this;
        var $evaluate = $("#evaluate"),
            $compile = $("#compile"),
            $emit = $("#emit"),
            $release = $("#release"),
            $keyboard = $("#kb-layout"),
            $autoscroll = $("#autoscroll"),
            $example = $("#example");
        var $share = $("#share"),
            $sharePanel = $("#share-panel"),
            $sharePanelLink = $("#share-panel-link", $sharePanel),
            $sharePanelSocial = $("#share-panel-social", $sharePanel);
        var $buttons = $([$evaluate[0], $compile[0]]);
        this.$switches = $([$emit[0], $release[0], $keyboard[0], $autoscroll[0]]);

        // editor
        this.editor = ace.edit("editor");
        this.editorSession =  this.editor.getSession();

        this.editor.setTheme("ace/theme/github");
        this.editorSession.setMode("ace/mode/erlang");
        this.editor.focus();
        // var nLines = this.EditorSession.getLength();
        // this.editor.gotoLine(nLines, this.editorSession.getLine(nLines - 1).length);

        // serialize
        this.serialize = function() {
            return {
                emit: $emit.val(),
                release: $release.val(),
                keyboard: $keyboard.val(),
                autoscroll: $autoscroll.prop("checked"),
                code: self.editorSession.getValue()
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
                    self.editorSession.setValue(v)
                    break
                }
            })
        }

        // compile/evaluate
        var progress = false;
        this.evaluate = function() {
            if (progress) {
                alert("In progress");
                return;
            }
            progress = true;
            $buttons.prop("disabled", true);

            transport
                .evaluate(self.editorSession.getValue(), $release.val())
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
                .compile(self.editorSession.getValue(), $release.val(), $emit.val())
                .always(function() {
                    progress = false;
                    $buttons.prop("disabled", false);
                })
        };
        $compile.on('click', this.compile);

        // share
        this.activatePaste = function(id, pickled) {
            if (pickled) {      // activated by URL or examples button
                self.lockUrl = true;
                this.deserialize(pickled);
                self.lockUrl = false;
            }                   // else { } -  just created paste
            $share.prop("disabled", true); // enabled in resetShare to avoid duplicates
            var origin = document.location.origin;
            var hash = '#id=' + id;
            var shareUrl = origin + "/" + hash;
            document.location.hash = hash;
            // link to copy
            var $a = $("<a/>",
                       {href: shareUrl,
                        text: shareUrl,
                        target: "_blank"});
            $sharePanelLink.empty().append($a);
            // social
            (function() {
                var title = "Share this snippet";
                var twLink = "https://twitter.com/intent/tweet?" + $.param({
                    hashtags: "Erlang",
                    original_referer: document.location.href,
                    text: "See! I've just created some Erlang snippet",
                    url: shareUrl
                });
                var $tw = $("<a/>",
                            {href: twLink,
                             title: title,
                             target: "_blank",
                             class: "tw-share",
                             html: "<span>&nbsp;</span>"});

                var fbLink = "https://www.facebook.com/sharer/sharer.php?" + $.param({
                    u: shareUrl
                });
                var $fb = $("<a/>",
                            {href: fbLink,
                             title: title,
                             target: "_blank",
                             class: "fb-share",
                             html: "<span>&nbsp;</span>"});

                var gpLink = "https://plus.google.com/share?" + $.param({
                    url: shareUrl
                });
                var $gp = $("<a/>",
                            {href: gpLink,
                             title: title,
                             target: "_blank",
                             class: "gp-share",
                             html: "<span>&nbsp;</span>"});

                $sharePanelSocial.empty().append($tw, $fb, $gp);
            })()
            $sharePanel.show();
        }
        $share.on('click', function() {
            $share.prop("disabled", true); // enabled in resetShare to avoid duplicates
            var state = self.serialize();
            pastebin.create(state.code, state.release, state.emit)
                .done(function(_1, _2, jqXHR) {
                    var location = jqXHR.getResponseHeader("location");
                    var id = location.split("/").pop();
                    self.activatePaste(id, null);
                }).fail(function(jqXHR, textStatus) {
                    alert("Error #" + jqXHR.status + ". " + textStatus + ". " + jqXHR.responseText);
                })
        });
        this.lockUrl = false;
        function resetShare() {
            $share.prop("disabled", false);
            $sharePanel.hide();
            if (!self.lockUrl && document.location.hash) { // see Pickle
                history.pushState(
                    '', document.title,
                    window.location.pathname + window.location.search);
            }
        }
        this.editorSession.on("change", resetShare);
        this.$switches.on('change', resetShare);

        // examples
        $example.on('change', function() {
            $example.prop("disabled", true);
            var id = $example.val();
            pastebin.get(id)
                .done(function(o) {
                    $example.prop("disabled", false);
                    self.activatePaste(id, o);
                }).fail(function(jqXHR, textStatus) {
                    $example.prop("disabled", false);
                    alert("Error #" + jqXHR.status + ". " + textStatus + ". " + jqXHR.responseText);
                })
        })

        // kb layout
        $keyboard.on('change', function() {
            self.editor.setKeyboardHandler($keyboard.val() || null);
        })

        // autoscroll
        output.autoscroll = $autoscroll.prop("checked");
        $autoscroll.on('change', function() {
            output.autoscroll = $autoscroll.prop("checked");
        })

        // shortcuts
        this.editor.commands.addCommand({
            name: "evaluate",
            exec: this.evaluate,
            bindKey: {win: "Ctrl-Enter", mac: "Ctrl-Enter"}
        });
        this.editor.commands.addCommand({
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

    function Pickler(control, pastebin) {
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

        if ("id" in qs) {
            pastebin.get(qs.id)
                .done(function(o) {
                    control.activatePaste(qs.id, o);
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
            // from URL hash
            control.deserialize(qs);

            if ("do_evaluate" in qs) control.evaluate();
            else if ("do_compile" in qs) control.compile();
        })();

        control.editorSession.on("change", function() {
            localStorage.setItem("code", control.editorSession.getValue());
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
            // TODO: cache {id: data}
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

    var output = new Output("#result");
    var transport = new TextTransport(output);
    var pastebin = new Pastebin();
    var control = new Controls(transport, output, pastebin);
    var pickler = new Pickler(control, pastebin);
})
