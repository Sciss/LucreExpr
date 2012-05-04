# LucreExpr

## statement

LucreExpr provides an expression tree system on top of LucreSTM's software transactional memory and reactive event system.

LucreExpr is (C)opyright 2011&ndash;2012 by Hanns Holger Rutz. All rights reserved. It is released under the [GNU General Public License](https://raw.github.com/Sciss/LucreExpr/master/licenses/LucreExpr-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

## requirements / installation

LucreSTM builds with sbt 0.11 against Scala 2.9.2. It depends on [LucreSTM](http:/github.com/Sciss/LucreSTM) which should be automatically retrieved from Maven Central.

## linking to LucreExpr

The following dependency is necessary:

    "de.sciss" %% "lucreexpr" % "0.10"

## creating an IntelliJ IDEA project

To develop the sources of LucreExpr, we recommend IntelliJ IDEA. If you haven't globally installed the sbt-idea plugin yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")

Then to create the IDEA project, run the following two commands from the xsbt shell:

    > set ideaProjectName := "LucreExpr"
    > gen-idea
