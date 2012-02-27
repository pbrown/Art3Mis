-module(records).
-compile(export_all).

-record(robot, {name,
	        type=industrial,
		hobbies,
		details=[]}).

-record(user, {id, name, group, age}).

first_robot()->
	#robot{name="Mechatron",
	       type=handmade,
	       details=["Move a small man inside"]}.


car_factory(CorpName)->
	#robot{name=CorpName, hobbies="building cars"}.


admin_panel(#user{name=Name, group=admin})->
	Name ++ " is allowed!";
admin_panel(#user{name=Name})->
	Name ++ " is not allowed!".

adult_section(U=#user{}) when U#user.age >=18 ->
	allowed;
adult_section(_)->
	forbidden.

