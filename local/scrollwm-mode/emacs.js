// -*- mode: scrollwm; -*-

emacs = {};

emacs.verbose = true;

GObject = imports.gi.GObject;
emacs.find_property = GObject.Object.find_property;
emacs.list_properties = GObject.Object.list_properties;

const JsParse = imports.misc.jsParse;

let commandHeader = 'const Clutter = imports.gi.Clutter; ' +
                    'const GLib = imports.gi.GLib; ' +
                    'const GObject = imports.gi.GObject; ' +
                    'const Gio = imports.gi.Gio; ' +
                    'const Gtk = imports.gi.Gtk; ' +
                    'const Mainloop = imports.mainloop; ' +
                    'const Meta = imports.gi.Meta; ' +
                    'const Lang = imports.lang;'
;

let _getAutoCompleteGlobalKeywords = () => {
    const keywords = ['true', 'false', 'null', 'new'];
    // Don't add the private properties of window (i.e., ones starting with '_')
    const windowProperties = Object.getOwnPropertyNames(window).filter(function(a){ return a.charAt(0) != '_'; });
    const headerProperties = JsParse.getDeclaredConstants(commandHeader);

    return keywords.concat(windowProperties).concat(headerProperties);
}

emacs.completion_candidates = (text) => {
    let AUTO_COMPLETE_GLOBAL_KEYWORDS = _getAutoCompleteGlobalKeywords();
    let [completions, attrHead] = JsParse.getCompletions(text, commandHeader, AUTO_COMPLETE_GLOBAL_KEYWORDS);

    let path = text.substring(0, text.length - attrHead.length - 1);
    try {
        let obj = eval(path);
        if (obj) {
            print(obj.toString());
            emacs.list_properties.call(obj)
            // list_properties gives names with "-" not "_"
                .forEach((x) => { completions.push(x.name.replace("-", "_")); });

            if (obj.prototype) {
                let [pCompletions, _] = JsParse.getCompletions(text + ".prototype", commandHeader, AUTO_COMPLETE_GLOBAL_KEYWORDS);
                pCompletions.forEach((x) => { completions.push(x); });
            }
        }
    } catch(e) {};

    return completions.filter((x) => {return x.startsWith(attrHead); });
};

