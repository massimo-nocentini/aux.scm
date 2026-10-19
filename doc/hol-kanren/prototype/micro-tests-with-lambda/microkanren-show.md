## μKanren query

```scheme
(μkanren-run (l 4 #f) (fresh° (a d) (append° a d l)))
```

**4** answers.

| # | answer |
| --- | --- |
| 1 | `α` |
| 2 | `(α . β)` |
| 3 | `(α β . γ)` |
| 4 | `(α β γ . δ)` |

```mermaid
flowchart LR
    n0(("l"))
    n1["α"]
    n2(("α"))
    n3["(α . β)"]
    n4(("α"))
    n5(("β"))
    n6["(α β . γ)"]
    n7(("α"))
    n8(("β"))
    n9(("γ"))
    n10["(α β γ . δ)"]
    n11(("α"))
    n12(("β"))
    n13(("γ"))
    n14(("δ"))
    n0 -->|1| n1
    n1 -->|×1| n2
    n0 -->|2| n3
    n3 -->|×1| n4
    n3 -->|×1| n5
    n0 -->|3| n6
    n6 -->|×1| n7
    n6 -->|×1| n8
    n6 -->|×1| n9
    n0 -->|4| n10
    n10 -->|×1| n11
    n10 -->|×1| n12
    n10 -->|×1| n13
    n10 -->|×1| n14
  class n0 query
  class n10,n6,n3,n1 value
  class n14,n13,n12,n11,n9,n8,n7,n5,n4,n2 var
  classDef query fill:#ebfbee,stroke:#2f9e44,stroke-width:2px;
  classDef value fill:#ffffff,stroke:#495057;
  classDef var fill:#fff3bf,stroke:#f08c00,stroke-width:2px;
  classDef atom fill:#e7f5ff,stroke:#1971c2;
  classDef cell fill:#f8f9fa,stroke:#868e96;
  classDef constraint fill:#ffe3e3,stroke:#e03131,stroke-dasharray:4 3;
  classDef answer fill:#ebfbee,stroke:#2f9e44,stroke-width:2px;
```

```mermaid
flowchart LR
  subgraph a1["answer 1"]
    direction LR
    n0(("α"))
  end
  subgraph a2["answer 2"]
    direction LR
    n1[["cons"]]
    n2(("α"))
    n3(("β"))
    n1 -->|car| n2
    n1 -->|cdr| n3
  end
  subgraph a3["answer 3"]
    direction LR
    n4[["cons"]]
    n5(("α"))
    n6[["cons"]]
    n7(("β"))
    n8(("γ"))
    n4 -->|car| n5
    n6 -->|car| n7
    n6 -->|cdr| n8
    n4 -->|cdr| n6
  end
  subgraph a4["answer 4"]
    direction LR
    n9[["cons"]]
    n10(("α"))
    n11[["cons"]]
    n12(("β"))
    n13[["cons"]]
    n14(("γ"))
    n15(("δ"))
    n9 -->|car| n10
    n11 -->|car| n12
    n13 -->|car| n14
    n13 -->|cdr| n15
    n11 -->|cdr| n13
    n9 -->|cdr| n11
  end
  class n15,n14,n12,n10,n8,n7,n5,n3,n2,n0 var
  class n13,n11,n9,n6,n4,n1 cell
  classDef query fill:#ebfbee,stroke:#2f9e44,stroke-width:2px;
  classDef value fill:#ffffff,stroke:#495057;
  classDef var fill:#fff3bf,stroke:#f08c00,stroke-width:2px;
  classDef atom fill:#e7f5ff,stroke:#1971c2;
  classDef cell fill:#f8f9fa,stroke:#868e96;
  classDef constraint fill:#ffe3e3,stroke:#e03131,stroke-dasharray:4 3;
  classDef answer fill:#ebfbee,stroke:#2f9e44,stroke-width:2px;
```

