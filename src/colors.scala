import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage
val photo1 = ImageIO.read(new File("image_input.jpg"))
printf("Photo size is %d x %d\n", photo1.getWidth, photo1.getHeight)


def sepia(img: BufferedImage): BufferedImage = {
  val photoOut=new BufferedImage(img.getWidth, img.getHeight,BufferedImage.TYPE_INT_RGB)
  var t=0
  for(i<-0 to img.getWidth-1){
    for(j<-0 to img.getHeight-1){
      try{
        val color=photo1.getRGB(i,j)
        
        var red = (color & 0xff0000) / 65536
        if (red>255){
          red=255
        }
        val green = (color & 0xff00) / 256
        val blue = (color & 0xff)
        val v = (0.299 * red + 0.587 * green + 0.114 * blue).toInt
        var newred = 0.0;
        var newblue = 0.0;
        if(v<62){
          newred = red * 1.1
          newblue = blue * 0.9
         // var newcolor = (1.1*red * 65536) + (green * 256) + 0.9*blue
         // photoOut.setRGB(i,j,newcolor.toInt)
        }
        else if(62<=v&&v<192){
          newred = red * 1.15
          newblue = blue * 0.85
          //var newcolor = (1.15*red * 65536) + (green * 256) + 0.85*blue
          //photoOut.setRGB(i,j,newcolor.toInt)
        }
        else if(192<=v){
          newred = red * 1.08
          newblue = blue * 0.93
         // var newcolor = (1.08*red * 65536) + (green * 256) + 0.93*blue
         // photoOut.setRGB(i,j,newcolor.toInt)
        }
        if (newred > 255) newred = 255.0;
        if (newblue > 255) newblue = 255.0;
        var newcolor = newred.toInt * 65536 + green.toInt * 256 + newblue.toInt
        photoOut.setRGB(i,j,newcolor)
      }
      catch{
        case e:ArrayIndexOutOfBoundsException=>
          //print(i,j,color)
          t=t+1
  			  //println("Not in the image!")
      }
    }
  }
  println(t+"/"+img.getWidth*img.getHeight)
  return photoOut
}

def write(i:BufferedImage){
  ImageIO.write(i, "jpg", new File("image_output.jpg"))
}
write(sepia(photo1))