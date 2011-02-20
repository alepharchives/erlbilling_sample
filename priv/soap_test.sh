#!/bin/sh

account=1
num=0
time=`date +%s`

while [ 1 ]; do
    current=`date +%s`
    amount=$((1 + $current & 333))
    account=$(($account + 1))
    num=$(($num + 1))

    cat soap_test.xml | \
    sed "s/ACCOUNT/$account/;s/AMOUNT/$amount/" | \
    curl -d @- -H "Content-Type: application/soap+xml; charset=utf-8" \
        http://localhost:8080/soap &>/dev/null

    printf "%i                \r" $num

    if [ $(($current - 10)) -gt $time ]; then
        time=$current
        printf "%i requests in 10 seconds\n" $num
        num=0
    fi
done
