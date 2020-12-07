# atl-checker

This application can do model checking of alternating-time temporal logic (ATL) on concurrent game structures (CGSs).
It uses a on-the-fly evaluation method as opposed to calculating the fixed point like PRISM do.
This lazy method avoids having to generate the whole graph, and is therefore faster in most cases (has yet to be proved).

The ATL-checker also uses a custom language to describe CGSs in a lazy manner. We call it LCGS and
it's syntax is inspired by PRISM-lang, however, the concepts of syncronization and modules are very different.  

## Example of how to use

We want to check if a cowboy can guarantee stay alive in a 3-way Mexican standoff.
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

    [wait] true;
    [shoot_right] health > 0 & target1.health > 0;
    [shoot_left] health > 0 & target2.health > 0;

endtemplate
```

We can now use ATL logic and query whether there exists a strategy for Billy that guarantees that he survives.
The ATL formula is given in json:


```json
{
  "enforce invariant": {
    "players": [0],
    "formula": {
      "proposition": 0
    }
  }
}
```

Here `"players": [0]` refers to Billy, and `"proposition": 0` refers to Billy's label called "alive".

We now call the atl-checker with the following arguments

```
./atl-checker solver -m standoff.lcgs -f billy-stays-alive.json
```

The result turns out to be FALSE. Billy cannot guarantee to stay alive.
