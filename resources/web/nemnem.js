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
    console.log("idname: " + idname);
    nemnem.highlit_ = $('a[name="' + idname + '"],a[href="' + idname + '"]');
    console.log("highlit: " + nemnem.highlit_);
    nemnem.highlit_.toggleClass(nemnem.highliteClass_);
  }
}
