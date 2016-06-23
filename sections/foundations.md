# Foundations

## System FC

![](images/core_spec.png)

## Lambda cube

![](images/lambda_cube.png)

## Lambda cube

```agda
-- Proof of left multiplicative distributivity of additive inverse in a ring

-‿*-distribˡ : ∀ x y → - x * y ≈ - (x * y)
-‿*-distribˡ x y = begin
  - x * y                        ≈⟨ sym $ proj₂ +-identity _ ⟩
  - x * y + 0#                   ≈⟨ refl ⟨ +-cong ⟩ sym (proj₂ -‿inverse _) ⟩
  - x * y + (x * y + - (x * y))  ≈⟨ sym $ +-assoc _ _ _  ⟩
  - x * y + x * y + - (x * y)    ≈⟨ sym (proj₂ distrib _ _ _) ⟨ +-cong ⟩ refl ⟩
  (- x + x) * y + - (x * y)      ≈⟨ (proj₁ -‿inverse _ ⟨ *-cong ⟩ refl)
                                      ⟨ +-cong ⟩
                                    refl ⟩
  0# * y + - (x * y)             ≈⟨ proj₁ zero _ ⟨ +-cong ⟩ refl ⟩
  0# + - (x * y)                 ≈⟨ proj₁ +-identity _ ⟩
  - (x * y)                      ∎
```

## Lambda cube

Oops
