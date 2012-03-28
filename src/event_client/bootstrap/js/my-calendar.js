var Event = Backbone.Model.extend({
    initialize:function(){
        alert("oh hello");
    },

    defaults: {
        name: "default event",
        description: "default description"
    }

});


