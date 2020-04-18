for i in *.tk; do
  name=$(echo "$i" | sed "s/^\(.*\)\.tk/compile_\1/")
  echo -e "Success\nexamples/$i" > "$name"
done
