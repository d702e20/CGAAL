# CGAAL

This application can do model checking of alternating-time temporal logic (ATL) on concurrent game structures (CGSs).
It uses an on-the-fly evaluation method as opposed to calculating the fixed point like PRISM does.
With this lazy method we avoid generating the whole graph, and it is therefore an order of magnitude faster in most cases.

CGAAL also uses a custom language to describe CGSs in a lazy manner. We call it LCGS and
its syntax is inspired by PRISM-lang. However, the concepts of synchronization and modules are very different.  

## Requirements
The minimum supported Rust version (MSRV) is 1.65.0.

## Example of how to use

Let's say we want to check if a cowboy can guarantee staying alive in a 3-way Mexican standoff.
The standoff is simulated in rounds and in each round a cowboy can choose to wait, shoot the cowboy to the right, or shoot the cowboy to the left.
If a cowboy is hit by two bullets, he dies.

We describe this scenario using LCGS as seen below

```
const max_health = 2;

player billy = cowboy [target1=clayton, target2=jesse];
player clayton = cowboy [target1=jesse, target2=billy];
player jesse = cowboy [target1=billy, target2=clayton];

template cowboy

    health : [0 .. max_health] init max_health;
    health' = max(health - target1.shoot_left - target2.shoot_right, 0);

    label alive = health > 0;

    [wait] 1;
    [shoot_right] health > 0 && target1.health > 0;
    [shoot_left] health > 0 && target2.health > 0;

endtemplate
```

We can now use ATL logic and query whether there exists a strategy for Billy that guarantees that he survives.
The ATL formula is given below in a file called `billy-can-stay-alive.atl`:

```
<<billy>> G billy.alive
```

We now call CGAAL with the following arguments

```
./cgaal solver -m standoff.lcgs -f billy-can-stay-alive.atl
```

The result turns out to be false. Billy cannot guarantee to stay alive.
