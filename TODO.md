
* Ask for single-returns should have an easier interface (no need for handle there).

        val g1 = replicas.head.ask[get.Type].
          handle[GetResult](_.valueOption).
          send(Get("key1", 2))

* Documentation in textual form of actions.