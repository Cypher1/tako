for i in *.tk; do
  name=$(echo "$i" | sed "s/.tk//")
  full_name=$(echo "$name" | sed "s/^/compile_/")
  echo -e "Output(goldens/$name.cc)\nexamples/$i" > "$full_name"
done
