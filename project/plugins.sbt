//
//resolvers += {
//  val typesafeRepoUrl = new java.net.URL("http://repo.typesafe.com/typesafe/releases")
//  val pattern = Patterns(false, "[organisation]/[module]/[sbtversion]/[revision]/[type]s/[module](-[classifier])-[revision].[ext]")
//  Resolver.url("Typesafe Repository", typesafeRepoUrl)(pattern)
//}
//
//libraryDependencies <<= (libraryDependencies, sbtVersion) { (deps, version) => 
//  deps :+ ("com.typesafe.sbteclipse" %% "sbteclipse" % "2.0.0" extra("sbtversion" -> version))
//}

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.0.0-M3")
