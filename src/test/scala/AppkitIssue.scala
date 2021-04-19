import scala.util.Try
import org.bouncycastle.util.encoders.Hex
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.ErgoAddressEncoder.MainnetNetworkPrefix
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.serialization.GroupElementSerializer

import java.util

object Util {
  def decodeBigInt(encoded: String): BigInt = Try(BigInt(encoded, 10)).recover { case ex => BigInt(encoded, 16) }.get

  def createTx(inputBoxes: Array[InputBox], dataInputs: Array[InputBox], outputBoxes: Array[OutBox], fee: Long, changeAddress: String, proveDlogSecrets: Array[String])(
      implicit ctx: BlockchainContext): SignedTransaction = {
    val inputs = new util.ArrayList[InputBox]()

    inputBoxes.foreach(inputs.add)

    val dataInputBoxes = new util.ArrayList[InputBox]()

    dataInputs.foreach(dataInputBoxes.add)

    val txToSign = ctx
      .newTxBuilder()
      .boxesToSpend(inputs)
      .withDataInputs(dataInputBoxes)
      .outputs(outputBoxes: _*)
      .fee(fee)
      .sendChangeTo(getAddressFromString(changeAddress))
      .build()

    val proveDlogSecretsBigInt = proveDlogSecrets.map(decodeBigInt)

    val dlogProver = proveDlogSecretsBigInt.foldLeft(ctx.newProverBuilder()) {
      case (oldProverBuilder, newDlogSecret) =>
        oldProverBuilder.withDLogSecret(newDlogSecret.bigInteger)
    }

    val signedTx = dlogProver.build().sign(txToSign)
    signedTx
  }
  val addressEncoder = new ErgoAddressEncoder(MainnetNetworkPrefix)

  def getAddressFromString(str: String) =
    addressEncoder
      .fromString(str)
      .get
  def encodeHex(bytes: Seq[Byte]): String = Hex.toHexString(bytes.toArray).toLowerCase
  def decodeHex(string: String): Array[Byte] = Hex.decode(string)

  def addressToGroupElement(address: String) = {
    /*
      encoding is as follows:

      group element
      ErgoTree serialized:      0008cd0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
      group element:                  0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
      group element serialized:     070279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798

      address:                     9fSgJ7BmUxBQJ454prQDQ7fQMBkXPLaAmDnimgTtjym6FYPHjAV
     */
    val ergoTree = encodeHex(addressEncoder.fromString(address).get.script.bytes)
    if (ergoTree.size != 72) throw new Exception("A proveDlog ergotree should be 72 chars long")
    val hex =
      if (ergoTree.take(6) != "0008cd") throw new Exception("Invalid address prefix for proveDlog")
      else
        ergoTree.drop(6)
    val groupElement: EcPointType = GroupElementSerializer.parse(decodeHex(hex))
    groupElement

  }
}
class AppkitIssue extends PropSpec with Matchers with ScalaCheckDrivenPropertyChecks with HttpClientTesting {
  import Util._
  val ergoClient1 = createMockedErgoClient(MockData.empty)
  property("Mock Update") {
    ergoClient1.execute { implicit ctx: BlockchainContext =>
      val ballotAddress =
        "7gak11YHej2wnu7YFDxdNtoXZrvc2iysCubCkeAT1rTnuNxKSh35tZCynt1DMjBydS7eS1SJwEM6pruA9fk6MjZFMGpNmUFW61CgEFve5dYcDXBQxf98oh5rnL3LRz6V1QfubNzGt8WDbcijvtCHNSWxmbMmV8Dit4rzHjGjuVp6j2ZjdijmqaDwf7T1qfrW"
      val updateAddress =
        "Su1rWdkh3KvNSBUiKjq2LvsWKJVbFcLQBdHM8WC1KoDL3ZHrY2zGox7DUfnasnmjk8y5LG8nFm3U7NG7fctBuo6WXveKFvgucUYKif2nnW4FZkt9Eudr2YWNN5h8k3MGUouZcPJTKGCJ1gW7UGiYWELC5XmfM5JbF84S4yn55TjjsdNXwatEgHjPgXzSfGo39z1JrTiiNTqRoew26ckbfpsS8D8XZQtg4irLukFUAen95WSzNEx7awcCRWXzn8Y7n9hYE83iKfB7KSBwdM616TpB2Sv3jAdx562pvBWgcHRDDgvL3nKof1w2STQxuWNSv2y2E1M6cy4kcVL8DPu8QAL6v51W7Eq9b938btUJGkDoN4BqjywDLiowVtNvKtFaFbC35M8uEbjLd5zuqTpU3z9ejxDCBv7Er"
      val epochPrepAddress =
        "EfS5abyDe4vKFrJ48K1o6f4psAJSEwhidbnxaSxyVQikRV45JRotq569Vb5mR79xoHYhH1m2MbD8JHPVuSYaA9r7ucXp9tT2aKuuq7qCCtBRaKbXaqNXMhBUDcHjnFbirbsf4xWnVDLQjVTCpsWcj15Faf5VBw73HkxGYxFe9ju44rfSFs4wx653LdN18ptw6sfH5SURR2SfYrRUeqxXpWF3QUsKYryryFobfvdr9k3tkeeMsUFqbGGf2skfWMrchyXxgkWBR6aCZrLpak9zeFwCQHxaeBC8Soj2h53amNgDsQLpLxtjGvc2m3pMbWohZ7QTj2yFsxxUoQBTvbtYNxrn8xo7VyXUury9CZ3CBDPRtgfsDm5d8QA3RmDSZboXd1CAPRSnUJp4R9z5ojzoTUoB7711CyPKgrQCN9KJRbxWB2J8RemUTLmqu3d96epaFzcuafmpvH93BWZTn71FXXB3SwzS51n9bgGTzqhoYLNVviz"

      val updateNFT = "dd26438230986cfe7305ad958451b69e55ad5ac37c8a355bfb08d810edd7a20f"

      val poolNFT = "e0630fa2366d8d0682cc1dbc114ad57f023bc1f2948999079d0cd05fcbc4db01"

      val ballotToken = "b662db51cf2dc39f110a021c2a31c74f0a1a18ffffbf73e8a051a7b8c0f09ebc"
      // now check voting.
      val poolBoxIn: InputBox = ctx
        .newTxBuilder()
        .outBoxBuilder
        .value(6786250000L)
        .tokens(
          new ErgoToken(poolNFT, 1)
        )
        .registers(
          ErgoValue.of(473933649L),
          ErgoValue.of(451338)
        )
        .contract(
          ctx.newContract(getAddressFromString(epochPrepAddress).script)
        )
        .build()
        .convertToInputWith("0ce3903041628de4b2707bc40ca5aaeb6c55d455f716bd935da5f7258211bd8c", 0)

      val updateBoxIn: InputBox = ctx
        .newTxBuilder()
        .outBoxBuilder
        .value(1000000L)
        .tokens(
          new ErgoToken(updateNFT, 1)
        )
        .registers(
          ErgoValue.of(Array[Byte](1))
        )
        .contract(
          ctx.newContract(getAddressFromString(updateAddress).script)
        )
        .build()
        .convertToInputWith("c67bb073e10c0970fde4fdebd3389ccc490f8cfbb33dbc0963178639dce89633", 0)

      // ballot boxes (Inputs)
      // R4 the pub key of voter [GroupElement] (not used here)
      // R5 dummy int due to AOTC non-lazy evaluation (from the line marked ****)
      // R6 the box id of this box [Coll[Byte]]
      // R7 the value voted for [Coll[Byte]]

      // Dummy address to use for voting
      // address "9eiuh5bJtw9oWDVcfJnwTm1EHfK5949MEm5DStc2sD1TLwDSrpx"
      // group element "021ae6ece10590e79fb74f8bdd1305e4ce479aad52b277751cccbf92d4c5bba2bf"
      // private key "37cc5cb5b54f98f92faef749a53b5ce4e9921890d9fb902b4456957d50791bd0"

      val dummyGroupElement = addressToGroupElement("9eiuh5bJtw9oWDVcfJnwTm1EHfK5949MEm5DStc2sD1TLwDSrpx")

      val ballotBoxInCandidate = ctx
        .newTxBuilder()
        .outBoxBuilder
        .value(10000000)
        .tokens(new ErgoToken(ballotToken, 4))
        .contract(ctx.newContract(getAddressFromString(ballotAddress).script))
        .registers(
          ErgoValue.of(dummyGroupElement),
          ErgoValue.of(1),
          ErgoValue.of(updateBoxIn.getId.getBytes),
          ErgoValue.of(decodeHex("138439d3faad36b53970feba95f0d9c0f74eb5b7ba8bfdb99614c8f22727ed26"))
        )
        .build()

      val ballotBoxIn0 = ballotBoxInCandidate.convertToInputWith("cb27ff805834cf7f7568dddce94bf470d92a82f4c189f0eeb25b80fababd9bbe", 0)
      val ballotBoxIn1 = ballotBoxInCandidate.convertToInputWith("cb27ff805834cf7f7568dddce94bf470d92a82f4c189f0eeb25b80fababd9bbe", 1)
      val ballotBoxIn2 = ballotBoxInCandidate.convertToInputWith("cb27ff805834cf7f7568dddce94bf470d92a82f4c189f0eeb25b80fababd9bbe", 2)
      val ballotBoxIn3 = ballotBoxInCandidate.convertToInputWith("cb27ff805834cf7f7568dddce94bf470d92a82f4c189f0eeb25b80fababd9bbe", 3)
      val ballotBoxIn4 = ballotBoxInCandidate.convertToInputWith("cb27ff805834cf7f7568dddce94bf470d92a82f4c189f0eeb25b80fababd9bbe", 4)
      val ballotBoxIn5 = ballotBoxInCandidate.convertToInputWith("cb27ff805834cf7f7568dddce94bf470d92a82f4c189f0eeb25b80fababd9bbe", 5)
      val ballotBoxIn6 = ballotBoxInCandidate.convertToInputWith("cb27ff805834cf7f7568dddce94bf470d92a82f4c189f0eeb25b80fababd9bbe", 6)
      val ballotBoxIn7 = ballotBoxInCandidate.convertToInputWith("cb27ff805834cf7f7568dddce94bf470d92a82f4c189f0eeb25b80fababd9bbe", 7)

      // new update box
      val updateBoxOut = ctx
        .newTxBuilder()
        .outBoxBuilder()
        .value(updateBoxIn.getValue)
        .contract(
          new ErgoTreeContract(getAddressFromString(updateAddress).script)
        )
        .tokens(new ErgoToken(updateNFT, 1L))
        .build()

      // new pool box
      val poolBoxOut = ctx
        .newTxBuilder()
        .outBoxBuilder()
        .value(poolBoxIn.getValue)
        .contract(
          new ErgoTreeContract(getAddressFromString(epochPrepAddress).script)
        )
        .tokens(new ErgoToken(poolNFT, 1L))
        .registers(ErgoValue.of(473933649L), ErgoValue.of(451338))
        .build()
      val ballotBoxOut0 = ctx
        .newTxBuilder()
        .outBoxBuilder()
        .value(10000000L)
        .contract(
          new ErgoTreeContract(getAddressFromString(ballotAddress).script)
        )
        .tokens(new ErgoToken(ballotToken, 4L))
        .registers(ErgoValue.of(dummyGroupElement))
        .build()

      val ballotBoxOut1 = ballotBoxOut0
      val ballotBoxOut2 = ballotBoxOut0
      val ballotBoxOut3 = ballotBoxOut0
      val ballotBoxOut4 = ballotBoxOut0
      val ballotBoxOut5 = ballotBoxOut0
      val ballotBoxOut6 = ballotBoxOut0
      val ballotBoxOut7 = ballotBoxOut0

      // dummy custom input box for funding various transactions
      val changeAddress = "9f5ZKbECVTm25JTRQHDHGM5ehC8tUw5g1fCBQ4aaE792rWBFrjK"
      val dummyTxId = "f9e5ce5aa0d95f5d54a7bc89c46730d9662397067250aa18a0039631c0f5b809"
      val dummyScript = "sigmaProp(true)"
      val dummyFundingBox = ctx
        .newTxBuilder()
        .outBoxBuilder
        .value(1000000000000L)
        .contract(ctx.compileContract(ConstantsBuilder.empty(), dummyScript))
        .build()
        .convertToInputWith(dummyTxId, 111)

      Util.createTx(
        inputBoxes = Array(
          updateBoxIn,
          poolBoxIn,
          ballotBoxIn0,
          ballotBoxIn1,
          ballotBoxIn2,
          ballotBoxIn3,
          ballotBoxIn4,
          ballotBoxIn5,
          ballotBoxIn6,
          ballotBoxIn7,
          dummyFundingBox
        ),
        dataInputs = Array(),
        outputBoxes = Array(
          updateBoxOut,
          poolBoxOut,
          ballotBoxOut0,
          ballotBoxOut1,
          ballotBoxOut2,
          ballotBoxOut3,
          ballotBoxOut4,
          ballotBoxOut5,
          ballotBoxOut6,
          ballotBoxOut7
        ),
        1000000,
        changeAddress,
        proveDlogSecrets = Array[String]()
      )
    }
  }

}
