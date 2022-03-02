import java.awt
import java.awt.event.KeyListener
import scala.swing._
import java.awt.{Color, Graphics2D}
import scala.swing.event._
import scala.util.Random


case class Vec2(x: Float, y: Float);

object Window {
  val WW = 1000;
  val WH = 1000;

  var B_WW = 25;
  var B_WH = 25;

  var B_W = WW / B_WW
  var B_H = WH / B_WH

  var grid = Array.ofDim[Vec2](B_WW, B_WH)

  var RED = 180
  var GREEN = 180
  var BLUE = 180

  def updateWindow(boxWidth: Int, boxHeight: Int): Unit = {
    B_WW = Math.max(5, boxWidth);
    B_WH = Math.max(5, boxHeight);

    B_W = WW / B_WW
    B_H = WH / B_WH

    grid = Array.ofDim[Vec2](B_WW, B_WH)

    for (x <- 0 until B_WW) {
      for (y <- 0 until B_WH) {
        grid(x)(y) = getRandom2DGradientVector();
      }
    }
  }

  def getRandom2DGradientVector(): Vec2 = {
    // generate floats in (-1, 1)
    val randX = Random.nextFloat() * 2 - 1;
    val randY = Random.nextFloat() * 2 - 1;

    val magnitude = Math.sqrt(randX * randX + randY * randY).toFloat

    Vec2(randX / magnitude, randY / magnitude)
  }

  // offset of v1 from v2 || offset + v1 => v2
  def calculateOffset(v1: Vec2, v2: Vec2): Vec2 = {
    Vec2(v1.x - v2.x, v1.y - v2.y)
  }



  for (x <- 0 until B_WW) {
    for (y <- 0 until B_WH) {
      grid(x)(y) = getRandom2DGradientVector();
    }
  }

  def dot(v1: Vec2, v2: Vec2): Float = {
    v1.x * v2.x + v1.y * v2.y
  }

  def normalize(v: Vec2): Vec2 = {
    val magnitude = Math.sqrt(v.x * v.x + v.y * v.y).toFloat
    if (magnitude == 0) { v }
    else {
      Vec2(v.x / magnitude, v.y / magnitude)
    }
  }

  // sigmoid-like interpolation + clamping function
  def smoothstep(x: Float): Float = {
    if (x <= 0) { 0f }
    else if (x > 1) { 1f }
    else {
      (3 * Math.pow(x, 2) - 2 * Math.pow(x, 3)).toFloat
    }
  }

  def interpolate(a1: Float, a2: Float, w: Float): Float = {
    assert(0 <= w && w <= 1, "Error: invalid interpolation weight domain" + w)
    a1 + (a2 - a1) * smoothstep(w)
  }

  // compute the perlin noise at (x, y)
  def perlin(x: Int, y: Int): Float = {
    val point = Vec2(x, y)

    val cellRow: Int = Math.floor(y.toFloat / B_H).toInt
    val cellCol: Int = Math.floor(x.toFloat / B_W).toInt

    if (cellRow + 1 >= Window.B_WH) return 0
    if (cellCol + 1 >= Window.B_WW) return 0

    val topLeftVec: Vec2 = grid(cellCol)(cellRow)
    val topRightVec: Vec2 = grid(cellCol + 1)(cellRow)
    val bottomLeftVec: Vec2 = grid(cellCol)(cellRow + 1)
    val bottomRightVec: Vec2 = grid(cellCol + 1)(cellRow + 1)

    val topLeftPoint = Vec2(cellCol * B_W, cellRow * B_H)
    val topRightPoint= Vec2((cellCol + 1) * B_W, cellRow * B_H)
    val bottomLeftPoint = Vec2(cellCol * B_W, (cellRow + 1) * B_H)
    val bottomRightPoint = Vec2((cellCol + 1) * B_W, (cellRow + 1) * B_H)

    // normalize these offsets to introduce sharp edges
    val topLeftOffset: Vec2 = calculateOffset(point, topLeftPoint)
    val topRightOffset: Vec2 = calculateOffset(point, topRightPoint)
    val bottomLeftOffset: Vec2 = calculateOffset(point, bottomLeftPoint)
    val bottomRightOffset: Vec2 = calculateOffset(point, bottomRightPoint)

    // dot product will be zero is the candidate point is exactly at the grid corner (offset==0)
    val topLeftDot = dot(topLeftVec, topLeftOffset)
    val topRightDot = dot(topRightVec, topRightOffset)
    val bottomLeftDot = dot(bottomLeftVec, bottomLeftOffset)
    val bottomRightDot = dot(bottomRightVec, bottomRightOffset)

    // interpolation weights (0, 1)
    val weightX = (x - cellCol * B_W).toFloat / B_W
    val weightY = (y - cellRow * B_H).toFloat / B_H

    // interpolate corners
    val interpolateTop = interpolate(topLeftDot, topRightDot, weightX)
    val interpolateBottom = interpolate(bottomLeftDot, bottomRightDot, weightX)

    val interpolateDown = interpolate(interpolateTop, interpolateBottom, weightY)

    interpolateDown
  }
}



class Canvas() extends Component {

  override def paintComponent(g : Graphics2D) {
    // clear the canvas
    g.setColor(Color.white);
    g.fillRect(0,0, Window.WW, Window.WH);

    // redraw the perlin noise
    for (x <- 0 until Window.WW) {
      for (y <- 0 until Window.WH) {
        val noise: Float = Math.cos(Window.perlin(x, y)).toFloat
//          val noise: Float = Window.perlin(x, y)
//        assert(-1 <= noise && noise <= 1, "Error: invalid range for noise" + noise)


        var noiseNormalized = 1f - ((noise + 1f) / 2f)

        val intensity: Float = Window.smoothstep(noiseNormalized)
        val red = Math.round(Window.RED * intensity)
        val green = Math.round(Window.GREEN * intensity)
        val blue = Math.round(Window.BLUE * intensity)

        g.setColor(new Color(red, green, blue))
        g.fillRect(x, y, 4, 4)
      }
    }
  }
}

class Window extends MainFrame {
  val canvas = new Canvas()

  title = "2D Perlin Noise"
  preferredSize = new Dimension(Window.WW, Window.WH)

  contents = new BorderPanel {
    add(canvas, BorderPanel.Position.Center)
  }

  def draw(): Unit = {
    canvas.repaint()
  }

  this.peer.addKeyListener(new KeyListener() {
    override def keyTyped(e: awt.event.KeyEvent): Unit = ()

    override def keyPressed(e: awt.event.KeyEvent): Unit = {
      val key = e.getKeyChar()

      if (key == 'w') { Window.updateWindow(Window.B_WW, Window.B_WH + 1) }
      else if (key == 's') { Window.updateWindow(Window.B_WW, Window.B_WH - 1) }
      else if (key == 'd') { Window.updateWindow(Window.B_WW + 1, Window.B_WH) }
      else if (key == 'a') { Window.updateWindow(Window.B_WW - 1, Window.B_WH) }
      else if (key == '1') { Window.RED = (Window.RED + 1) % 255 }
      else if (key == '2') { Window.GREEN= (Window.GREEN + 1) % 255 }
      else if (key == '3') { Window.BLUE = (Window.BLUE + 1) % 255 }

      Window.updateWindow(Window.B_WW, Window.B_WH)
      draw()
    }

    override def keyReleased(e: awt.event.KeyEvent): Unit = ()
  })
}

object Main {
  def main(args: Array[String]): Unit = {
    val window = new Window()
    window.visible = true;

//    while (true) {
//      window.update()
//      println("")
//    }
/*
Perlin noise steps:
1. defining a grid of random gradient vectors
2. computing the dot product between the gradient vectors and their offsets
3. interpolating between these values
*/
  }
}

// function     - value of the last expression = return value
// val          - cannot be reassigned (const)
// classes      - compared by reference
// case classes - compared by value
// object       - like a singleton of a class
// trait        - set of fields and methods (classes can extend several traits)

