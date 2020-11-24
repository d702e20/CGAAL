use std::cmp::{min, Ordering};

type Num = usize;

#[derive(Debug, Eq, PartialEq)]
pub struct Weight {
    weight: Num,
    /// Slot 0 is the slot with the most significant weight
    slot: Num,
}

/// The amount of weight to start and end with
const FULL_WEIGHT: Num = Num::MAX;
/// Used as the weight when increasing a slot.
/// Must add upto 0 and have the carry flag set when added to itself.
const SPLIT_WEIGHT: Num = 0b1 << (std::mem::size_of::<Num>() * 8 - 1);
/// Maximal slot value allowed, this need to be adjusted such that the size of the `slots` field in ControllerWeight can fit in main memory, and not use all of it.
/// Remember to consider the maximum amount of physical memory addressable by the processor, and how much can physically be attached.
const MAX_SLOT: Num = 0b1 << 16;
/// The least significant slot the controller will give weight from.
const MAX_TAKE_SLOT: Num = 0b1 << 10;
/// The most significant slot the controller will give weight from.
/// If the controller have less than this amount of weight then it will either fail, or delay.
const MIN_TAKE_SLOT: Num = 0b1 << 14;

impl Weight {
    /// Create weight with initial value for the beginning of a distributed computation.
    /// The distributed computation can be terminated once the controller value become this value again.
    pub fn new_full() -> Self {
        Self {
            weight: FULL_WEIGHT,
            slot: 0,
        }
    }

    /// Does this `Weight` have all the weight, or is it a fraction of the total weight?
    #[allow(clippy::absurd_extreme_comparisons)]
    pub fn is_full(&self) -> bool {
        assert!(
            (self.slot == 0 && self.weight <= FULL_WEIGHT) || self.slot > 0,
            "Total weight have overflowed. Is weight being added out of nowhere?"
        );

        self.weight == FULL_WEIGHT && self.slot == 0
    }

    /// Split weight evenly among two new weights.
    /// Returns Err if self if the minimal weight manual. Instead of splitting further, request more weight from the controller.
    pub fn split(self) -> Result<(Weight, Weight), ()> {
        if self.slot == Num::MAX && self.weight <= 1usize {
            // Can't split the smallest possible weight
            return Err(());
        }

        let weights = match self.weight {
            0 => panic!("Can't split a zero weight. A weight of zero is illegal."),
            1 => (
                Self {
                    weight: SPLIT_WEIGHT,
                    slot: self.slot + 1,
                },
                Self {
                    weight: SPLIT_WEIGHT,
                    slot: self.slot + 1,
                },
            ),
            _ => (
                Self {
                    weight: self.weight / 2,
                    slot: self.slot,
                },
                Self {
                    // Handles odd weight values
                    weight: self.weight / 2 + self.weight % 2,
                    slot: self.slot,
                },
            ),
        };
        Ok(weights)
    }

    /// Adds two weights together.
    /// If the combined weight spans multiple slots the most significant part is returned as the first half of the tuple.
    /// The seconds half of the tuple is None if the combined value fits in a single slot, otherwise the least significant part is returned in the second half of the tuple.
    /// If the second half of the the tuple is Some, that value should be should be returned to the controller.
    pub fn join(self, other: Self) -> (Self, Option<Self>) {
        match self.slot.cmp(&other.slot) {
            Ordering::Less => (other, Some(self)),
            Ordering::Greater => (self, Some(other)),
            Ordering::Equal => {
                let (val, carry) = self.weight.overflowing_add(other.weight);
                if carry {
                    assert_ne!(
                        self.slot, 0,
                        "Total weight have overflowed. Is weight being added out of nowhere?"
                    );

                    let rhs = if val == 0 {
                        None
                    } else {
                        Some(Self {
                            weight: val,
                            slot: self.slot,
                        })
                    };

                    (
                        Self {
                            weight: 1,
                            slot: self.slot - 1,
                        },
                        rhs,
                    )
                } else {
                    (
                        Self {
                            weight: val,
                            slot: self.slot,
                        },
                        None,
                    )
                }
            }
        }
    }
}

pub struct ControllerWeight {
    slots: Vec<Num>,
}

impl ControllerWeight {
    /// Create new ControllerWeight with zero weight present on the controller
    pub fn new() -> Self {
        Self { slots: vec![0] }
    }

    /// Return true if the controller have received all weight, when this happens the controller may terminate the computation.
    pub fn is_full(&self) -> bool {
        // if slot 0 if full, and any of the other slots have a weight of greater than 0 then the total weight have overflowed.

        self.slots[0] == FULL_WEIGHT
    }

    /// Make sure there is allocated storage for `slot`
    fn allocate_slot(&mut self, slot: Num) {
        // NOTE: This functions fails to allocate the last slot if MAX_SLOTS is equal to Num::MAX
        assert_ne!(slot, Num::MAX);

        if self.slots.len() < slot + 1 {
            self.slots.resize(slot + 1, 0);
        }
    }

    pub fn receive_weight(&mut self, weight: Weight) {
        self.allocate_slot(weight.slot);

        let mut slot = weight.slot;
        let (val, mut carry) = weight.weight.overflowing_add(self.slots[weight.slot]);
        self.slots[slot] = val;
        while carry {
            slot -= 1;
            // Destructuring assignments is not possible at the time of writing this,
            // so allocate a temp variable and copy it over to the global one.
            let (val, new_carry) = self.slots[slot].overflowing_add(1);
            self.slots[slot] = val;
            carry = new_carry;

            if carry && slot == 0 {
                panic!("Weight overflow");
            }
        }
    }

    /// Finds the least significant slot that is more significant or equal than `MIN_TAKE_SLOT` and have a non-zero weight
    fn find_least_non_zero(&mut self, slot: Num) -> Option<usize> {
        let mut slot = slot;
        while slot > 0 && self.slots[slot] == 0 {
            slot -= 1;
        }

        if slot == 0 && self.slots[0] == 0 {
            // All slot are zero
            None
        } else {
            assert!(self.slots[slot] > 0);
            Some(slot)
        }
    }

    /// Reduce more significant slots until a `Weight` with weight 1 and slot `MIN_TAKE_SLOT` can be taken.
    /// Returns `None` if there is not atleast `Weight { weight: 1, slot: MIN_TAKE_SLOT }` weight available on the controller.
    fn reduce_weight_to_take(&mut self) -> Option<Weight> {
        let init_slot = match self.find_least_non_zero(MIN_TAKE_SLOT) {
            None => return None,
            Some(slot) => slot,
        };

        if init_slot < MIN_TAKE_SLOT {
            self.slots[init_slot] -= 1;
            for i in (init_slot + 1)..=MIN_TAKE_SLOT {
                self.slots[i] = FULL_WEIGHT - 1;
            }
            Some(Weight {
                weight: 1,
                slot: MIN_TAKE_SLOT,
            })
        } else {
            self.slots[MIN_TAKE_SLOT] -= 1;

            Some(Weight {
                weight: 1,
                slot: MIN_TAKE_SLOT,
            })
        }
    }

    /// Take weight from the controller. This should only be used if a `Weight` have reached the minimum possible value, and needs to be refiled.
    /// Returns Err if the controller have no weight. Consider waiting for the controller to receive more weight from computations in-flight.
    pub fn take_weight(&mut self) -> Result<Weight, ()> {
        self.allocate_slot(MIN_TAKE_SLOT);

        if let Some(weight) = self.reduce_weight_to_take() {
            return Ok(weight);
        }

        // Take the most significant slot between MIN_TAKE_SLOT and MAX_TAKE_SLOT
        for i in (MIN_TAKE_SLOT..=min(MAX_TAKE_SLOT, self.slots.len() - 1)).rev() {
            if self.slots[i] > 0 {
                let weight = Weight {
                    weight: self.slots[i],
                    slot: i,
                };

                self.slots[i] = 0;

                return Ok(weight);
            }
        }

        Err(())
    }
}

#[cfg(test)]
mod test {
    mod weight {
        use super::super::{Num, Weight, FULL_WEIGHT, SPLIT_WEIGHT};

        /// Check the assertion given in the documentation for SPLIT_WEIGHT.
        #[test]
        fn split_weight_must_zero_and_carry() {
            let (val, carry) = SPLIT_WEIGHT.overflowing_add(SPLIT_WEIGHT);
            assert_eq!(val, 0);
            assert_eq!(carry, true);
        }

        #[test]
        fn split_full_weight() {
            let weight = Weight::new_full();

            println!("{:?}", weight);

            let (lhs, rhs) = weight.split().unwrap();
            println!("lhs: {:?}, rhs: {:?}", lhs, rhs);

            let (weight, to_ctrl) = lhs.join(rhs);
            println!("weight: {:?}, to_ctrl: {:?}", weight, to_ctrl);

            assert_eq!(
                weight,
                Weight {
                    weight: FULL_WEIGHT,
                    slot: 0
                }
            );
            assert_eq!(to_ctrl, None);
        }

        #[test]
        fn split_one() {
            let weight = Weight { weight: 1, slot: 0 };

            println!("{:?}", weight);

            let (lhs, rhs) = weight.split().unwrap();
            println!("lhs: {:?}, rhs: {:?}", lhs, rhs);

            let (weight, to_ctrl) = lhs.join(rhs);
            println!("weight: {:?}, to_ctrl: {:?}", weight, to_ctrl);

            assert_eq!(weight, Weight { weight: 1, slot: 0 });
            assert_eq!(to_ctrl, None);
        }

        #[test]
        fn split_max_minus_one() {
            let weight = Weight {
                weight: FULL_WEIGHT - 1,
                slot: 0,
            };

            println!("{:?}", weight);

            let (lhs, rhs) = weight.split().unwrap();
            println!("lhs: {:?}, rhs: {:?}", lhs, rhs);

            let (weight, to_ctrl) = lhs.join(rhs);
            println!("weight: {:?}, to_ctrl: {:?}", weight, to_ctrl);

            assert_eq!(
                weight,
                Weight {
                    weight: FULL_WEIGHT - 1,
                    slot: 0
                }
            );
            assert_eq!(to_ctrl, None);
        }

        /// If the result of joining two weights spans multiple slots, check that the least significant part are returned to the controller
        #[test]
        fn decrease_slot() {
            let lhs = Weight {
                weight: SPLIT_WEIGHT + 1,
                slot: 1,
            };
            let rhs = Weight {
                weight: SPLIT_WEIGHT,
                slot: 1,
            };
            println!("lhs: {:?}, rhs: {:?}", lhs, rhs);

            let (weight, to_ctrl) = lhs.join(rhs);
            println!("weight: {:?}, to_ctrl: {:?}", weight, to_ctrl);

            assert_eq!(weight, Weight { weight: 1, slot: 0 });
            assert_eq!(to_ctrl, Some(Weight { weight: 1, slot: 1 }));
        }

        /// A weight of zero is illegal so check that it yields a panic
        #[test]
        #[should_panic]
        fn split_zero_weight() {
            let weight = Weight { weight: 0, slot: 0 };

            weight.split();
        }

        /// Panic if joined weight overflows the maximal weight that can be represented
        #[test]
        #[should_panic]
        fn underflow_slot() {
            let lhs = Weight {
                weight: Num::MAX,
                slot: 0,
            };
            let rhs = Weight {
                weight: Num::MAX,
                slot: 0,
            };

            lhs.join(rhs);
        }

        /// Split should return an error if it is attempted on the minimum representable weight value
        #[test]
        fn overflow_slot() {
            let weight = Weight {
                weight: 1,
                slot: Num::MAX,
            };

            assert_eq!(weight.split(), Err(()));
        }
    }

    mod controller_weight {
        use super::super::{Num, MAX_SLOT};
        use crate::distterm::{
            ControllerWeight, Weight, FULL_WEIGHT, MAX_TAKE_SLOT, MIN_TAKE_SLOT, SPLIT_WEIGHT,
        };

        /// ControllerWeight::allocate_slot fails to allocate the last slot if MAX_SLOT is Num::MAX
        #[test]
        fn restrict_max_slot() {
            assert!(MAX_SLOT < Num::MAX);
        }

        #[test]
        fn add_weight_allocated_slot() {
            let mut controller = ControllerWeight::new();
            controller.receive_weight(Weight { weight: 1, slot: 0 });

            assert_eq!(controller.slots, vec![1])
        }

        #[test]
        fn add_weight_unallocated_slot() {
            let mut controller = ControllerWeight::new();
            controller.receive_weight(Weight { weight: 1, slot: 5 });

            assert_eq!(controller.slots, vec![0, 0, 0, 0, 0, 1])
        }

        #[test]
        fn add_weight_overflow() {
            let mut controller = ControllerWeight::new();
            controller.receive_weight(Weight {
                weight: FULL_WEIGHT,
                slot: 1,
            });
            controller.receive_weight(Weight {
                weight: FULL_WEIGHT,
                slot: 2,
            });
            controller.receive_weight(Weight {
                weight: FULL_WEIGHT,
                slot: 3,
            });
            controller.receive_weight(Weight {
                weight: FULL_WEIGHT,
                slot: 4,
            });
            controller.receive_weight(Weight {
                weight: FULL_WEIGHT,
                slot: 5,
            });
            controller.receive_weight(Weight { weight: 1, slot: 5 });

            assert_eq!(controller.slots, vec![1, 0, 0, 0, 0, 0])
        }

        #[test]
        fn take_weight() {
            let mut controller = ControllerWeight::new();
            controller.receive_weight(Weight {
                weight: SPLIT_WEIGHT,
                slot: 0,
            });

            let taken_weight = controller.take_weight();
            assert_eq!(
                taken_weight,
                Ok(Weight {
                    weight: 1,
                    slot: MIN_TAKE_SLOT
                })
            );
            let mut expected_slots: Vec<Num> = [FULL_WEIGHT - 1; MIN_TAKE_SLOT + 1].to_vec();
            expected_slots[0] = SPLIT_WEIGHT - 1;
            assert_eq!(controller.slots, expected_slots);
        }

        #[test]
        fn take_weight_maximal() {
            let mut controller = ControllerWeight::new();
            controller.receive_weight(Weight {
                weight: 1,
                slot: MIN_TAKE_SLOT,
            });

            let taken_weight = controller.take_weight();
            assert_eq!(
                taken_weight,
                Ok(Weight {
                    weight: 1,
                    slot: MIN_TAKE_SLOT
                })
            );
            let expected_slots: Vec<Num> = [0 as Num; MIN_TAKE_SLOT + 1].to_vec();
            assert_eq!(controller.slots, expected_slots);
        }

        #[test]
        fn take_weight_minimal() {
            let mut controller = ControllerWeight::new();
            controller.receive_weight(Weight {
                weight: 1,
                slot: MAX_TAKE_SLOT,
            });

            let taken_weight = controller.take_weight();
            assert_eq!(
                taken_weight,
                Ok(Weight {
                    weight: 1,
                    slot: MAX_TAKE_SLOT
                })
            );
            let expected_slots: Vec<Num> = [0 as Num; MAX_TAKE_SLOT + 1].to_vec();
            assert_eq!(controller.slots, expected_slots);
        }

        #[test]
        fn take_weight_unavailable() {
            let mut controller = ControllerWeight::new();

            let taken_weight = controller.take_weight();
            assert_eq!(taken_weight, Err(()));
            // Check that the total weight is zero
            for i in 0..controller.slots.len() {
                assert_eq!(controller.slots[i], 0);
            }
        }
    }
}
