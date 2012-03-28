$(function(){

    var Todo = Backbone.Model.extend({

        defaults: function(){
            return {
                title: "empty todo...",
                order: this,
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
    })

    var TodoList = Backbone.Collection.extend({
         model: Todo,
         localStorage: new Store("todos-backbone"),

        done: function(){
            return this.filter(function(todo){return todo.get('done'); });
        }
    })
});