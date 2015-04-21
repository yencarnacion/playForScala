package controllers

import play.api.mvc.{Action, Controller, Flash}
import play.api.data.Form
import play.api.data.Forms.{mapping, longNumber, nonEmptyText}
import play.api.i18n.Messages

import models.Product

import reflect.runtime.universe._
import reflect.runtime.currentMirror
import scala.collection.immutable.HashMap

object Products extends Controller {

  def convertToMap(p: Product): Map[String, String] = {
      val r = currentMirror.reflect(p)
      val map = r.symbol.typeSignature.members.toStream
        .collect{case s : TermSymbol if !s.isMethod => r.reflectField(s)}
        .map(r => r.symbol.name.toString.trim -> r.get.toString)
        .toMap
      /*val map = collection.immutable.HashMap (
        "ean" -> p.ean.toString,
        "name" -> p.name,
        "description" -> p.description)*/

      map
  }

   def list = Action { implicit request =>

      val products = Product.findAll

      Ok(views.html.products.list(products))
   }

   def show(ean: Long) = Action { implicit request =>
      Product.findByEan(ean).map { product =>
         Ok(views.html.products.details(product))
      }.getOrElse(NotFound)
    }

    private val productForm: Form[Product] = Form(
       mapping(
         "ean" -> longNumber.verifying("validation.ean.duplicate", Product.findByEan(_).isEmpty),
         //"ean" -> longNumber,
         "name" -> nonEmptyText,
         "description" -> nonEmptyText
       )(Product.apply)(Product.unapply)
    )

    def edit(ean: Long) = Action { implicit request =>
      Product.findByEan(ean).map { product =>
        val form = productForm.bind(convertToMap(product))
        Ok(views.html.products.editProduct(form))
      }.getOrElse(NotFound("Product not found"))

    }

   def save = Action { implicit request =>
       val newProductForm = productForm.bindFromRequest()

       newProductForm.fold (
       		hasErrors = { form =>
			                         Redirect(routes.Products.newProduct()).
			                         flashing(Flash(form.data) +
			     	                   ("error" -> Messages("validation.errors")))
			                },
		      success = {newProduct =>
			                         Product.add(newProduct)
			                         val message = Messages("products.new.success", newProduct.name)
			                         Redirect(routes.Products.show(newProduct.ean)).flashing("success" ->message)
			              }
			 )
   }

   def newProduct = Action { implicit request =>
      val form = if(flash.get("error").isDefined)
         productForm.bind(flash.data)
      else
         productForm
      
      Ok(views.html.products.editProduct(form))
   }

}