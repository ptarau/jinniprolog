source('hiClass 1','1.0.0','Brad Heathman').
package('prolog/experiments').

/** This RuleSet test the oo behaviors.
 */

%:-[core].

/*---------------------
    Fields
----------------------*/

:-initialization((
  println(initializing),
	static_jbh <== init_value
        )).

hiClass_1 :-                    instance_jbh <= init_value_2.

static_field(Value) :-          static_jbh ==> Value.
instance_field(Value) :-        instance_jbh => Value.


/*---------------------
    Built-In-Test
----------------------*/

bit :-
        sleep(1), nl,
        pp('start bit'),
        listing,
        this_works,
        pp(here1),
        this_works2,
        this_fails,
        pp('All is fine').
bit :-
        pp(something_is_wrong).

this_works :- 
        pp(last_good),
        listing,
        static_jbh ==> _,
        pp(never_here),
        instance_jbh => _,
        pp(this_works).

this_works2 :- 
        static_field(_),
        instance_field(_),
        pp(this_works2).

this_fails :-
        pp(here),
        new(hiClass_1,C),
        C:instance_field(_),    % Works
        C:static_field(_),      % Fails
        pp(this_fails).
