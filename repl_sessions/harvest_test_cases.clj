(ns repl-sessions.harvest-test-cases
  (:require [lambdaisland.harvest :as h]))

;; This repl session is a design document. In literal programming style it
;; explains how the new Harvest will/should work, by example. Many of these
;; examples will not yet work, and some of them may still be up for discussion.
;; It is meant as a guide to contributors to outline the vision of this library
;; should become.

;; Things that currently work will be shown with the regular convention of `=>`
;; followed by the result value. Things that don't yet work are marked with
;; `!!=>`

;; The main thing you immediately notice is different from Facai, is that
;; there's now an argument vector in defactory. You don't have to have
;; arguments, it can just be blank like here.

(h/defactory Tea []
  {:flavor "oolong"})

(h/build Tea)
;; => {:flavor "oolong"}

;; What this hints at is that the body of the factory is now essentially a
;; thunk, i.e. a function. Before we stored the template map (or other type of
;; collection). This had its benefits in that we could inspect and manipulate
;; the template inside the factory directly. Now we will have to first generate
;; a value before we can work with it. But the flexibility you gain is huge, and
;; it provides a much better upgrade path from plain functions to factories.

;; The result is that now you don't need all those lambdas in your factories.

;; Facai:
(f/defactory Thingamajig
  {:id #(rand-int 100)})

;; Harvest:
(f/defactory Thingamajig []
  {:id (rand-int 100)})

;; Calling the factory (remember it now basically acts as a function definition)
;; yields... a factory.
(Tea)
;; => {:harvest.factory/id repl-sessions.harvest-test-cases/Tea,
;;     :harvest.factory/entity? true,
;;     :harvest.factory/state #<Atom@54d21fd5: {}>,
;;     :harvest.factory/template
;;     #function[repl-sessions.harvest-test-cases/fn--27321]}

;; This is going to be a theme in Harvest, you manipulate factories as values,
;; by calling them with specific options/flags, or by using helper functions.
;; The result is always another factory, which eventually you pass to `build` or
;; `buildset`.

(h/defactory User [{:keys [id] :or {id (rand-int 100)}}]
  {:id id
   :flavor "oolong"})

(h/build User)
;; => {:id 32, :flavor "oolong"}

(h/build (User {:id "foo"}))
;; => {:id "foo", :flavor "oolong"}

(h/defactory User [{:keys [id] :or {id (rand-int 100)}}]
  {:id id
   :flavor "oolong"})

;; Note that we only take the final form in `defactory` as the function body.
;; Forms before that are taken as key-value pairs that set properties on
;; the factory.

(h/defactory User [{:keys [id] :or {id (rand-int 100)}}]
  :traits {,,,}
  :entity? false

  {:id id
   :flavor "oolong"})

;; Note that this example probably won't be considered exactly idiomatic, since
;; you can just have the factory use `rand-int`, and possibly override the value
;; later.

(h/defactory User []
  {:id (rand-int 100)
   :flavor "oolong"})

(h/build (h/with User {:id "foo"}))
;; !!=> {:id "foo", :flavor "oolong"}


;; You still have references the same way you had in Facai

(h/defactory Unit []
  (rand-nth ["m" "s" "kg"]))

(h/defactory Amount []
  {:unit Unit
   :amount (rand-int 100)})

(h/build Amount)
;; => {:unit "s", :amount 22}

;; And here you can either use the bare factory name, or call it as a function.
;; Note that the "deferred" concept from Facai is gone, since the factory itself
;; is a thunk/function, the evaluation always happens at the time the factory
;; value is being built.

(h/defactory TeaOrder []
  {:tea (Tea {:flavor "black"})
   :sides [:cookies]})

;; So far we've built a single value, but you can also request the "buildset",
;; which is a map from the factory path to the generated value, and this for
;; all (nested) values generated.
(h/buildset Amount)
;; => {[:lambdaisland.harvest/root
;;      repl-sessions.harvest-test-cases/Amount
;;      :unit
;;      repl-sessions.harvest-test-cases/Unit]
;;     "kg",
;;     [:lambdaisland.harvest/root repl-sessions.harvest-test-cases/Amount]
;;     {:unit "kg", :amount 52}}

;; You'll notice that `build` (and `buildset`) take a single argument, either a
;; factory, or a template.

(h/build {:user User
          :amount Amount})
;; => {:user {:id 51, :flavor "oolong"}, :amount {:unit "s", :amount 82}}

;; In facai build takes a bunch of different options: `:with`, `:traits`,
;; `:rules`, etc. The idea in Harvest is that these become _part of the factory_.

(h/build
 (h/with Tea {:flavor "dayuling"}))
;; !!=> {:flavor "dayuling"}

;; In other words, `(Tea {:with ...})` returns a _new factory_, which has the
;; Tea factory as its parent, but additionally provides overrides.

;; Rules and traits are handled similarly.

(h/defactory Tea []
  :traits
  {:bubble-tea
   {:with
    {:flavor "black"
     :milk? true
     :toppings [:boba]}}}

  {:flavor "green"})

;; Currently factories have a mutable state, you can swap `this` inside the
;; factory to update it. This is meant for cases where you want to make sure you
;; have unique values, e.g. by having an incrementing counter.

;; I'm not 100% sure yet of this, this may change or go away.

(h/defactory User []
  (let [i (:cnt (swap! this update :cnt (fnil inc 0)))]
    {:user/name   "Lilliam Predovic"
     :user/handle (str "lilli" i)
     :user/email  (str "lilli" i "@example.com")}))

(h/build User)
;; => {:user/name "Lilliam Predovic",
;;     :user/handle "lilli1",
;;     :user/email "lilli1@example.com"}

(h/build User)
;; => {:user/name "Lilliam Predovic",
;;     :user/handle "lilli2",
;;     :user/email "lilli2@example.com"}

;; Selections are now also part of the factory itself. Selections are useful
;; when you are generating a large object graph, and then want to continue with
;; specific sub-values.

(h/defactory Article []
  {:article/title "7 Tip-top Things To Try"
   :article/submitter User
   :article/author (h/with User {:user/roles #{"author"}})})

(-> Article
    (h/select {:article [Article]
               :author [:article/author]})
    h/build)
;; => {:article
;;     ({:article/title "7 Tip-top Things To Try",
;;       :article/submitter
;;       {:user/name "Lilliam Predovic",
;;        :user/handle "lilli3",
;;        :user/email "lilli3@example.com"},
;;       :article/author
;;       {:user/name "Lilliam Predovic",
;;        :user/handle "lilli4",
;;        :user/email "lilli4@example.com"}}),
;;     :author
;;     ({:user/name "Lilliam Predovic",
;;       :user/handle "lilli4",
;;       :user/email "lilli4@example.com"})}

;; Which you can then nicely destructure in your test or other code
(let [{:keys [article author]} (-> Article
                                   (h/select {:article [Article]
                                              :author [:article/author]})
                                   h/build)]
  ,,,)

;; This example is fairly trivial, but once you add in persistence you no longer
;; see nested objects in the result, only foreign key references.



;; !!
(let [{:keys [article author]} (-> Article
                                   (h/select {:article [Article]
                                              :author [:article/author]})
                                   (harvest.jdbc/persist jdbc-config) ;; also part of factory
                                   h/build)]
  article)
;; !!=>
;; {:article/title "7 Tip-top Things To Try",
;;  :article/submitter 45
;;  :article/author 46}

;; Regarding persistence, in Facai we consider every value that gets generated
;; by a factory as something that should be inserted/persisted. This means you
;; can't have a factory to e.g. populate a jsonb field.
;;
;; In harvest there's an explicit entity? flag on factories, defaulting to true.

(h/defactory JsonProps []
  :entity? false
  {:prop1 "foo"
   :prop2 "bar"})

(h/defactory FirstClassThing []
  :harvest.jdbc/table "first_class_thing"
  {:props JsonProps})

;; Internally a big change is that rather than trying to do everything the
;; functional way by passing the build context around all through the process,
;; we are now introducing two dynamic vars.

;; Stack based — recursive calls push/pop (with binding), propagates down but
;; not up
(def ^:dynamic *context* nil)

;; Bound to a mutable container during a single build pass, individual factories
;; side effect to add in their buildset.
(def ^:dynamic *buildset* nil)

;; This clearly makes a distinction between two types of bookkeeping we need to
;; do: things that are set for a specific call tree, and popped/unbound again
;; when that call finishes (so a typical usage of dynamic bindings) vs state
;; that gets built up throughout the process.

;; In facai we have a single context map trying to serve both of these uses,
;; which was tricky. Having these as dynamic vars also moves them out of the
;; way, so e.g. in hooks we can pass a simply value, rather than a complete
;; context. The context can still be accessed via the var.

;; Facai:
(f/defactory LineItem
  :after-build
  (fn [ctx]
    (f/update-result
     ctx
     (fn [{:as res :keys [product quantity]}]
       (assoc res :total (* (:price product) quantity)))))

  {:product Product
   :quantity 1})

;; Harvest:
(h/defactory LineItem []
  :after-build
  (fn [{:as line-item :keys [product quantity]}]
    (assoc line-item :total (* (:price product) quantity)))

  {:product Product
   :quantity 1})

;; Having these always available as vars, rather than having to pass them in,
;; also means that you should be able to recursively call build within a
;; factory, which is useful if you want to have certain values be derived from a
;; nested value.

(h/defactory LineItem []
  (let [product (h/build Product)
        qty (rand-int 10)]
    {:product product
     :quantity qty
     :total (* (:price product) qty)}))

;; This does raise some questions, what's the path of the nested factory value
;; in the result for instance. Also in this example if you supply a different
;; value for `:product` with `h/with`, then the total will be off. Whereas when
;; using a hook as above that does work.

;; And this comes to the biggest design question that I have right now.
;; Currently harvest executes the factory body, and then
;; afterwards (potentially) override values. This means the factory has no
;; knowledge of the actual values that will be in the result.

;; This brings us back to the example that takes arguments.

(h/defactory LineItem [{:keys [product qty]
                        :or {product Product
                             qty (rand-int 100)}}]
  {:product product
   :quantity qty
   :total (* (:price Product) qty)})

;; You could imagine us doing some magic here where in the macro we pull out the
;; `:or` and treat is as a factory template, and combine it with any `:with`
;; values.

(h/with LineItem {:product (h/with Product {:price 55})
                  :qty 5})
(h/with LineItem {:product (h/with Product {:price 55})
                  :qty 5})

;; What would happen is:
;; - We know the factory expects `:product` and `:qty`, based on its signature (it has to use destructuring in the defactory as above)
;; - if we have an override (from h/with) we use that, if not we use the value from `:or`
;; - we do process the value either way, so if it's a factory it gets built

;; This brings up another question of symmetry, where h/with is perhaps
;; redundant, and we treat an initial map passed as an argument to a factory as
;; the `:with` map (i.e. containing overrides).

(h/with LineItem {:product (h/with Product {:price 55})
                  :qty 5})
;; Same thing?
(LineItem {:product (h/with Product {:price 55})
           :qty 5})

;; I need to noodle a bit more on this last point, find a balance between
;; convenient and intuitive. We need to make sure we end up with a mental model
;; that is still easy to grok.
