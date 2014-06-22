nemnem = {
  // Objects currently highlighted
  highlit_: null,
  highliteClass_: "hlit",
  
  clearHighlight: function(obj) {
    obj.toggleClass(nemnem.highliteClass_);
  },

  // idname: the pure location (no hash prefix)
  highlightLocalToLocal: function(idname) {
    nemnem.highlightPath('a[name="' + idname + '"],a[href="#' + idname + '"]');
  },

  // href: full href 
  highlightLocalToRemote: function(href) {
    nemnem.highlightPath('a[href="' + href + '"]');
  },

  // Highlight (only) elements matching the given jQuery path.
  highlightPath: function(path) {
     if (nemnem.highlit_ != null) {
      nemnem.highlit_.toggleClass(nemnem.highliteClass_);
    }
    nemnem.highlit_ = $(path);
    nemnem.highlit_.toggleClass(nemnem.highliteClass_);
  },
}
