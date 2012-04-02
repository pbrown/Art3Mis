    $(function(){

    var Todo = Backbone.Model.extend({

        defaults: function(){
            return {
                title: "empty todo...",
                order: Todos.nextOrder(),
                done:false
            };
        },

        initialize: function(){
            if(!this.get("title")){
                this.set({"title": this.defaults.title});
            }
        },

        toggle: function(){
            this.save({done: !this.get("done")});
        },

        clear: function(){
            this.destroy();
        }
    });

    var TodoList = Backbone.Collection.extend({
         model: Todo,

        done: function(){
            return this.filter(function(todo){return todo.get('done'); });
        },

        remaining: function(){
            return this.without.apply(this, this.done());
        },

        nextOrder: function(){
            if(!this.length) return 1;
            return this.last().get('order')+1;
        },

        comparator: function(todo){
            return todo.get('order');
        }
    });

    var Todos = new TodoList;


    var TodoView = Backbone.View.extend({

        tagName: "li",

        template: _.template($('#item-template')).html()),

        events: {
            "click .toggle" : "toggleDone",
            "dblclick .view" : "edit",
            "click a.destroy" : "clear",
            "keypress .edit" : "updateOnEnter",
            "blur .edit"      : "close"
        },

        initialize: function(){
            this.model.bind('change', this.render, this);
            this.model.bind('destroy', this.remove, this);
        },

        render: function(){
            this.$el.html(this.template(this.model.toJSON()));
            this.$el.toggleClass('done', this.model.get('done'));
            this.input = this.$('.edit');
            return this;
        },

        edit: function() {
            this.$el.addClass("editing");
            this.input.focus();
        },

        close: function() {
            var value = this.input.val();
            if (!value) this.clear();
            this.model.save({title.value});
            this.$el.removeClass("editing");
        },

        updateOnEnter: function(e){
            if (e.keyCode == 13) this.close();
        },

        clear: function(){
            this.model.clear();
        }


    });


    var AppView = BackBone.View.extend({

        el: $("#todoapp"),

        events: {
            "keypress #new-todo": "createOnEnter",
            "click #clear-completed": "clearCompleted",
            "click #toggle-all": "toggleAllComplete"
        },

        initialize: function(){
            this.input =
        }
    })
});