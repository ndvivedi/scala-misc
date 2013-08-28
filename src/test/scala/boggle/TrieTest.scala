import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object TrieTests extends Properties("Trie") {

  //Generate a Trie and the words in and words NOT in the Trie
  //The suchThat entries are to make sure we get no empty lists
  def genTrie: Gen[(Trie.Trie[Char], List[List[Char]], List[List[Char]])] = 
    for {
      inTrie <- Arbitrary.arbitrary[List[List[Char]]] suchThat (_.length > 0)
      outTrie <- Arbitrary.arbitrary[List[List[Char]]] suchThat (l => (l.length > 0 && l.forall(! inTrie.contains(_))))
    } yield (Trie(inTrie), inTrie, outTrie)

  property("all in") = forAll (genTrie) {
    case (trie, inTrie, _) =>
      inTrie.forall(trie.full(_))
  }

  property("all out") = forAll (genTrie) {
    case (trie, _, outTrie) =>
      outTrie.forall(! trie.full(_))
  }

  property("check that all prefixes are valid") = forAll (genTrie) {
    case (trie, inTrie, _) =>
      inTrie forall { word =>
        val prefixes = (1 to word.length) map { word.take(_) }
        prefixes.forall(trie.prefix(_))
      }
  }

  property("check that prefixes not in trie are not reported") = forAll (genTrie) {
    case (trie, inTrie, outTrie) =>
      val inPrefix = inTrie flatMap { w => (1 until w.length) map { w.take(_) } } toSet
      val outPrefix = outTrie flatMap { w => (1 until w.length) map { w.take(_) } }
      outPrefix filter { ! inPrefix.contains(_) } forall { ! trie.prefix(_) }
  }

}
