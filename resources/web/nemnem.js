nemnem = {
  // Objects currently highlighted
  highlit_: null,
  highliteClass_: "hlit",
  
  clearHighlight: function(obj) {
    obj.toggleClass(nemnem.highliteClass_);
  },

  // Highlights references to the given location
  highlight: function(idname) {
    if (nemnem.highlit_ != null) {
      nemnem.highlit_.toggleClass(nemnem.highliteClass_);
    }
    nemnem.highlit_ = $('a[name="' + idname + '"],a[href="#' + idname + '"]');
    nemnem.highlit_.toggleClass(nemnem.highliteClass_);
  }
}
