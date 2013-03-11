(ns verje.core
  (:gen-class))

(require '[lanterna.screen :as s])

(def scr (s/get-screen :swing))

(def world-state (ref {
  1 {:renderable {:c "@" :x 10 :y 10 :z 1 :color :red}
     :name-str "david"}
  2 {:renderable {:c "." :x 9 :y 9 :z 1 :color :green}}
}))

(defn find-unused-entity-id
  "Returns the next unused entity ID in the given world state."
  [world-state]
  (+ (apply max (keys world-state)) 1))

(defn make-obj
  "Creates an empty entity with a given ID."
  [world-state entity-id]
  (assoc-in world-state [entity-id] {}))

(defn endow-renderable
  "Endow an entity with the ability to be rendered."
  [world-state entity-id c x y z]
  (assoc-in world-state [entity-id :renderable] {:c c :x x :y y :z z}))

(defn endow-name
  "Endow an entity with a name."
  [world-state entity-id name-str]
  (assoc-in world-state [entity-id :name-str] name-str))

(defn find-renderables-at
  "Returns the entity IDs of any objects at the given position."
  [world-state x y z]
  (for [[entity-id entity] world-state
        :when (and (= (get-in entity [:renderable :x]) x)
                   (= (get-in entity [:renderable :y]) y)
                   (= (get-in entity [:renderable :z]) z))] entity-id))

(defn move-renderable
  "Move a renderable. Two renderables can't occupy the same location in space."
  [world-state entity-id dx dy dz]
  (let [renderable (get-in world-state [entity-id :renderable])
        new-x (+ (:x renderable) dx)
        new-y (+ (:y renderable) dy)
        new-z (+ (:z renderable) dz)]
        (if (empty? (find-renderables-at world-state new-x new-y new-z))
          ((comp #(assoc-in % [entity-id :renderable :x] new-x)
                 #(assoc-in % [entity-id :renderable :y] new-y)
                 #(assoc-in % [entity-id :renderable :z] new-z)) world-state)
          world-state)))

(defn step-create-named-renderable-entity!
  "Steps the world forward and creates a new named renderable entity. Returns the new entity ID."
  [name-str x y z]
  (dosync
    (if (empty? (find-renderables-at @world-state x y z))
      (let [unused-entity-id (find-unused-entity-id @world-state)]
        (alter world-state
          #(-> %
            (make-obj unused-entity-id)
            (endow-name unused-entity-id name-str)
            (endow-renderable unused-entity-id (str (first name-str)) x y z)))
        ; Return the new entity ID.
        unused-entity-id)
      ; If we couldn't make an entity there, return nil.
      nil)))

(defn step-move-renderable-entity!
  "Steps the world by moving a renderable entity. Returns the world."
  [entity-id dx dy dz]
  (dosync
    (alter world-state move-renderable entity-id dx dy dz)))

(defn draw-renderable
    [renderable]
    (s/put-string scr (renderable :x) (renderable :y) (renderable :c) {:fg (renderable :color)}))

(defn clear-screen
  [size]
  (dorun 
    (for [x (range (size 0)) y (range (size 1))]
      (s/put-string scr x y " "))))

(defn draw-scene
  [world]
  (do
    (clear-screen (s/get-size scr))
    (dorun
      (for
        [x (keys world)]
          (draw-renderable (:renderable (world x)))))))

(defn game-loop
  [world]
  (do
    (draw-scene world)
    (s/redraw scr)
    (let
      [key (s/get-key-blocking scr)]
      (cond
        (= key :left) (recur (step-move-renderable-entity! 1 -1 0 0))
        (= key :up) (recur (step-move-renderable-entity! 1 0 -1 0))
        (= key :right) (recur (step-move-renderable-entity! 1 1 0 0))
        (= key :down) (recur (step-move-renderable-entity! 1 0 1 0))
	:else (println "Goodbye!")))))

(defn -main
  "Start verje in client or server mode."
  [& args]
  (do
    (s/start scr)
    (let [world (deref world-state)]
        (game-loop world))
    (s/stop scr)))
