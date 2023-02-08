package net.soundmining

import net.soundmining.Generative.randomRange
import net.soundmining.modular.ModularInstrument.StaticAudioBusInstrument
import net.soundmining.modular.ModularSynth.{relativePercControl, sineControl, staticAudioBus, staticControl}
import net.soundmining.modular.{ModularSynth, SynthPlayer}
import net.soundmining.synth.Instrument.{EFFECT, TAIL_ACTION}
import net.soundmining.synth.SuperColliderClient.{freeAll, loadDir}
import net.soundmining.synth.Utils.absoluteTimeToMillis
import net.soundmining.synth.{EmptyPatch, Instrument, PatchPlayback, SuperColliderClient, SuperColliderReceiver}

import scala.util.Random

/**
 * Mix noise and pitched tones. Maybe with a MarkovChain between noise and pitch
 * and a second layer of MarkovChain to choose which noise and pitch.
 *
 */
object AmbientMusic6 {
  implicit val client: SuperColliderClient = SuperColliderClient()
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"
  val synthPlayer = SynthPlayer(soundPlays = Map.empty, numberOfOutputBuses = 2, bufferedPlayback = false)
  var patchPlayback: PatchPlayback = PatchPlayback(patch = EmptyPatch, client = client)
  val superColliderReceiver: SuperColliderReceiver = SuperColliderReceiver(patchPlayback)

  implicit val random: Random = new Random()

  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    synthPlayer.init()
    superColliderReceiver.start()
  }


  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.send(freeAll(0))
    client.stop
    this.superColliderReceiver.stop()
  }

  case class Spect(fundamentalNote: String, firstPartialNote: String, scaleSize: Int) {
    val fundamental = Note.noteToHertz(fundamentalNote)
    val firstPartial = Note.noteToHertz(firstPartialNote)
    val fact = Spectrum.makeFact(fundamental, firstPartial)

    val baseSpectrum = Spectrum.makeSpectrum2(fundamental, Spectrum.makeFact(fundamental, firstPartial), 15)

    def makeEqualTemperedScale(size: Int): Seq[Double] = {
      val fundamentalCents = Note.hertzToCents(fundamental)
      val octaveCents = Note.hertzToCents(firstPartial) - fundamentalCents
      val noteCents = octaveCents / scaleSize
      Seq.fill(size)(noteCents)
        .zipWithIndex
        .map {
          case (noteCent, i) => fundamentalCents + (noteCent * i)
        }
        .map(noteCents => Note.centsToHertz(noteCents))
    }

    def makeSpectrums(freqs: Seq[Double]): Seq[Seq[Double]] =
      freqs.map(freq => Spectrum.makeSpectrum2(freq, fact, 15))
  }

  case class OvertoneAmps(enable: Int => Boolean = _ => true, ampValue: Int => Double = i => 1.0 / (i + 1.0)) {
    def amps(): Seq[Double] =
      Seq.fill(15)(1.0)
        .zipWithIndex
        .map {
          case (_, i) => if (enable(i)) ampValue(i) else 0.0
        }
  }

  case class OvertoneRingtimes(ringTime: Int => Double = _ => 1.0) {
    def ringtimes(): Seq[Double] =
      Seq.fill(15)(1.0)
        .zipWithIndex
        .map {
          case (_, i) => ringTime(i)
        }
  }

  case class OvertonePhases(phase: Int => Double = _ => 0.0) {
    def phases(): Seq[Double] =
      Seq.fill(15)(1.0)
        .zipWithIndex
        .map {
          case (_, i) => phase(i)
        }
  }

  case class Incrementor(var value: Int = 0) {
    def curr(): Int = value

    def inc(): Unit = {
      value = value + 1
    }
  }

  object Piece {
    def playCleanEffect(startTime: Double, effectAudioBus: StaticAudioBusInstrument, ampValue: Double, duration: Double, output: Int): Unit = {
      val cleanAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrol(0, 0.1, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val clean = ModularSynth.stereoVolume(effectAudioBus, cleanAmp(ampValue))
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      clean.getOutputBus.staticBus(output)
      val graph = clean.buildGraph(startTime, duration, clean.graph(Seq()))
      client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
    }

    def playShortEffect(startTime: Double, duration: Double, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)
      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrol(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))

      val comb = ModularSynth.stereoComb(effectAudioBus, reverbAmp(5), 0.3, 2.3)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      val reverb = ModularSynth.stereoFreeReverb(comb, reverbAmp(5), mix = 0.7, room = 0.9, damp = 0.9)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))

      effectAudioBus
    }

    def playLongEffect(startTime: Double, duration: Double, cleanRate: Double, cleanOut: Int, effectOut: Int): StaticAudioBusInstrument = {
      val effectAudioBus = staticAudioBus(2)
      effectAudioBus.getOutputBus.dynamicBus(startTime, startTime + duration, 2)
      playCleanEffect(startTime, effectAudioBus, cleanRate, duration, cleanOut)
      val reverbAmp = (amp: Double) => ModularSynth.relativeThreeBlockcontrol(0, 0.05, amp, amp, 0.1, 0, Left(Seq(0, 0, 0)))
      val reverb = ModularSynth.stereoHallReverb(effectAudioBus, reverbAmp(1.0 - cleanRate), rt60 = 7, stereo = 0.6, lowRatio = 0.7, hiRatio = 0.4)
        .addAction(TAIL_ACTION)
        .nodeId(EFFECT)

      reverb.getOutputBus.staticBus(effectOut)
      val graph = reverb.buildGraph(startTime, duration, reverb.graph(Seq()))
      client.send(client.newBundle(absoluteTimeToMillis(startTime), graph))
      effectAudioBus
    }

    def playPiece(start: Double = 0.0, reset: Boolean = true): Unit = {
      if (reset) client.resetClock()

      val effectLen = 60.0 * 4.0
      val effectBus1 = playLongEffect(start, effectLen, 0.6, 0, 0)
      val effectBus2 = playLongEffect(start, effectLen, 0.6, 0, 0)
      val effectBus3 = playLongEffect(start, effectLen, 0.6, 0, 0)
      val effectBus4 = playShortEffect(start, effectLen, 0)

      val ui = UiModelBuilder(Seq("One", "Two", "Three", "Four"))

      val spect = Spect("c2", "fiss3", scaleSize = 6)

      val amp = () => randomRange(0.3, 0.9) * 0.1

      val scale = spect.makeEqualTemperedScale(48)
      val scaleSpects = spect.makeSpectrums(scale)

      val sieve1 = UnionSieve(Seq(SimpleSieve(3, 0), SimpleSieve(4, 2)))
      val sieve2 = UnionSieve(Seq(SimpleSieve(4, 0), SimpleSieve(3, 3)))
      val sieve3 = UnionSieve(Seq(SimpleSieve(2, 1), SimpleSieve(3, 2)))

      val ampFunc1: Int => Double = i => 1 / ((i + 1) * math.Pi)
      val ampFunc2: Int => Double = i => 4.0 / math.pow((i + 1.0) * math.Pi, 2)
      val ampFunc3: Int => Double = i => 2.0 / ((i + 1.0) * math.Pi)

      val sieves = Seq(sieve1, sieve2, sieve3)
      val ampFuncs = Seq(ampFunc1, ampFunc2, ampFunc3, ampFunc2)

      val ringTimes = Seq(0.3, 0.5, 1.0, 0.5).map(ringTime => OvertoneRingtimes(_ => ringTime))
      val firstInc = Incrementor()
      val phases = OvertonePhases()

      def playLongNote(note: Int, currentTime: Double, durationFunc: () => Double, panFunc: () => Double, inc: Incrementor, audioBus: StaticAudioBusInstrument): Unit = {
        var overtoneAmp = OvertoneAmps(i => sieves(inc.curr() % sieves.length).isSieve(i), ampFuncs(inc.curr() % ampFuncs.length))

        synthPlayer()
          .bankOfOsc(scaleSpects(note), overtoneAmp.amps(), phases.phases())
          .monoVolume(sineControl(0, amp()))
          .pan(staticControl(panFunc()))
          .playWithDuration(currentTime, durationFunc(), outputBus = audioBus.getOutputBus.busValue.get, realOutput = false)

        inc.inc()
        overtoneAmp = OvertoneAmps(i => sieves(inc.curr() % sieves.length).isSieve(i), ampFuncs(inc.curr() % ampFuncs.length))
        println(overtoneAmp.amps())

        synthPlayer()
          .bankOfOsc(scaleSpects(note), overtoneAmp.amps(), phases.phases())
          .monoVolume(sineControl(0, amp()))
          .pan(staticControl(panFunc()))
          .playWithDuration(currentTime, durationFunc(), outputBus = audioBus.getOutputBus.busValue.get, realOutput = false)

        synthPlayer()
          .pinkNoise(sineControl(0, amp()))
          .bankOfResonators(scaleSpects(note), overtoneAmp.amps(), ringTimes(inc.curr() % ringTimes.length).ringtimes())
          .highPass(staticControl(scaleSpects(note).head))
          .monoVolume(sineControl(0, amp()))
          .pan(staticControl(panFunc() * -1))
          .playWithDuration(currentTime, durationFunc(), outputBus = audioBus.getOutputBus.busValue.get, realOutput = false)

        inc.inc()
      }

      def playShortNote(note: Int, currentTime: Double, panFunc: () => Double, inc: Incrementor, audioBus: StaticAudioBusInstrument): Unit = {
        var overtoneAmp = OvertoneAmps(i => sieves(inc.curr() % sieves.length).isSieve(i), ampFuncs(inc.curr() % ampFuncs.length))
        val durationFunc = () => 0.1 * randomRange(0.9, 1.1)

        synthPlayer()
          .bankOfOsc(scaleSpects(note), overtoneAmp.amps(), phases.phases())
          .monoVolume(relativePercControl(0.0, amp() * 0.6, 0.001, Left(Seq(4.0, -4.0))))
          .pan(staticControl(panFunc()))
          .playWithDuration(currentTime, durationFunc(), outputBus = audioBus.getOutputBus.busValue.get, realOutput = false)

        inc.inc()
        overtoneAmp = OvertoneAmps(i => sieves(inc.curr() % sieves.length).isSieve(i), ampFuncs(inc.curr() % ampFuncs.length))
        println(overtoneAmp.amps())

        synthPlayer()
          .bankOfOsc(scaleSpects(note), overtoneAmp.amps(), phases.phases())
          .monoVolume(relativePercControl(0.0, amp() * 0.6, 0.001, Left(Seq(4.0, -4.0))))
          .pan(staticControl(panFunc()))
          .playWithDuration(currentTime, durationFunc(), outputBus = audioBus.getOutputBus.busValue.get, realOutput = false)

        synthPlayer()
          .pinkNoise(sineControl(0, amp()))
          .bankOfResonators(scaleSpects(note), overtoneAmp.amps(), ringTimes(inc.curr() % ringTimes.length).ringtimes())
          .highPass(staticControl(scaleSpects(note).head))
          .monoVolume(relativePercControl(0.0, amp() * 0.6, 0.001, Left(Seq(4.0, -4.0))))
          .pan(staticControl(panFunc() * -1))
          .playWithDuration(currentTime, durationFunc(), outputBus = audioBus.getOutputBus.busValue.get, realOutput = false)

        inc.inc()
      }

      val notes4 = Seq(17, 16, 13, 15, 14, 12, 10, 11, 9, 7, 6, 8)

      val fourthInc = Incrementor()
      val pan4 = () => randomRange(-0.99, 0.99)

      val fourthSequencer = Sequencer(notes4.length)
        .nextTimeHandler(_ => 8 * randomRange(0.9, 1.1))
        .startTimeHandler(currentTime => currentTime + (3 * randomRange(0.9, 1.1)))
        .stepHandler((index, currentTime) => {
          val note = notes4(index)
          ui.addUi("Four", currentTime, 0.001, 0.1, note)
          playShortNote(note, currentTime, pan4, fourthInc, effectBus4)
        })
        .build()

      val notes3 = Seq.fill(3)((4, 5)) ++ Seq.fill(5)((10, 11)) ++ Seq.fill(5)((4, 5))
      val duration3 = () => 3 * randomRange(0.9, 1.1)
      val thirdInc = Incrementor()
      val pan3 = () => randomRange(0.50, 0.99)

      val thirdSequencer = Sequencer(notes3.length)
        .nextTimeHandler(_ => 13 * randomRange(0.9, 1.0))
        .startTimeHandler(currentTime => currentTime + (2 * randomRange(0.9, 1.1)))
        .stepHandler((index, currentTime) => {
          val (firstNote, secondNote) = notes3(index)
          val (firstDuration, secondDuration) = (duration3(), duration3())
          val secondStart = currentTime + (5 * randomRange(0.9, 1.1))

          ui.addUi("Three", currentTime, 0.5, firstDuration, firstNote)
          ui.addUi("Three", secondStart, 0.5, secondDuration, secondNote)

          playLongNote(firstNote, currentTime, duration3, pan3, thirdInc, effectBus3)
          playLongNote(secondNote, secondStart, duration3, pan3, thirdInc, effectBus3)
        })
        .spawnSequencerHandler(1, fourthSequencer)
        .build()

      val notes2 = Seq.fill(5)((9, 7)) ++ Seq.fill(5)((1, 3)) ++ Seq.fill(3)((9, 7)) ++ Seq.fill(2)((1, 3))
      val duration2 = () => 5 * randomRange(0.9, 1.1)
      val secondInc = Incrementor()
      val pan2 = () => randomRange(-0.3, 0.30)

      val secondSequencer = Sequencer(notes2.length)
        .nextTimeHandler(_ => 13 * randomRange(0.9, 1.0))
        .startTimeHandler(currentTime => currentTime + (2 * randomRange(0.9, 1.1)))
        .stepHandler((index, currentTime) => {
          val (firstNote, secondNote) = notes2(index)
          val (firstDuration, secondDuration) = (duration2(), duration2())
          val secondStart = currentTime + (5 * randomRange(0.9, 1.1))

          ui.addUi("Two", currentTime, 0.5, firstDuration, firstNote)
          ui.addUi("Two", secondStart, 0.5, secondDuration, secondNote)
          playLongNote(firstNote, currentTime, duration2, pan2, secondInc, effectBus2)
          playLongNote(secondNote, secondStart, duration2, pan2, secondInc, effectBus2)
        })
        .spawnSequencerHandler(1, thirdSequencer)
        .build()

      val notes1 = Seq.fill(5)((6, 8)) ++ Seq.fill(3)((0, 2)) ++ Seq.fill(5)((6, 8)) ++ Seq.fill(3)((0, 2))
      val duration1 = () => 5 * randomRange(0.9, 1.1)
      val pan1 = () => randomRange(-0.99, -0.5)

      Sequencer(notes1.length)
        .nextTimeHandler(_ => 13 * randomRange(0.9, 1.0))
        .stepHandler((index, currentTime) => {
          val (firstNote, secondNote) = notes1(index)
          val (firstDuration, secondDuration) = (duration1(), duration1())
          val secondStart = currentTime + (5 * randomRange(0.9, 1.1))

          ui.addUi("One", currentTime, 0.5, firstDuration, firstNote)
          ui.addUi("One", secondStart, 0.5, secondDuration, secondNote)
          playLongNote(firstNote, currentTime, duration1, pan1, firstInc, effectBus1)
          playLongNote(secondNote, secondStart, duration1, pan1, firstInc, effectBus1)
        })
        .spawnSequencerHandler(1, secondSequencer)
        .build()
        .generateSequence(start)

      PieceCanvas.displayUiModel(ui.uiModel())
    }
  }
}
