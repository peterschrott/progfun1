import week3._

val s1 = Empty.incl(2).incl(3)
val s2 = Empty.union(s1)

println(s2)
s1.incl(1)
println(s2)

val s3 = s1.union(Empty.incl(1).incl(2).incl(6))
println(s3)

if (s3.contains(2)) 1 else false