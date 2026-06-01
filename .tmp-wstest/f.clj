(condp = k
                KeyType/Escape nil
                KeyType/Backspace
                (do (when (seq q) (pop q))
                  (reset! sel 0)
                  (recur))
                KeyType/Enter :done)
