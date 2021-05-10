for f in (find . -iname "*random*")
    echo $f
    mv $f cgs(echo $f | cut -c9-)
end
