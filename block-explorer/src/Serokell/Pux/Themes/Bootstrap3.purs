-- | This module provides CSS class names for common Bootstrap 3 classes.

module Serokell.Pux.Themes.Bootstrap3 where

import Prelude             (($), (<>))

import Pux.Html            (Html, Attribute, div, link, script)
import Pux.Html.Attributes (src, rel , type_, href)
import Serokell.Pux.Html   (ClassName (..))

bootstrap :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
bootstrap attr html = bootstrapCss attr $ bootstrapScripts <> html
  where
    bootstrapScripts =
        [ script
            [ src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js" ]
            []
        , script
            [ src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" ]
            []
        ]

bootstrapCss :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
bootstrapCss attr html = div attr $ [ bootstrapStyle ] <> html
  where
    bootstrapStyle =
        link
            [ rel "stylesheet"
            , type_ "text/css"
            , href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            ]
            []

active :: ClassName
active = ClassName "active"

affix :: ClassName
affix = ClassName "affix"

alert :: ClassName
alert = ClassName "alert"

alertDanger :: ClassName
alertDanger = ClassName "alert-danger"

alertDismissable :: ClassName
alertDismissable = ClassName "alert-dismissable"

alertDismissible :: ClassName
alertDismissible = ClassName "alert-dismissible"

alertInfo :: ClassName
alertInfo = ClassName "alert-info"

alertLink :: ClassName
alertLink = ClassName "alert-link"

alertSuccess :: ClassName
alertSuccess = ClassName "alert-success"

alertWarning :: ClassName
alertWarning = ClassName "alert-warning"

arrow :: ClassName
arrow = ClassName "arrow"

badge :: ClassName
badge = ClassName "badge"

bgDanger :: ClassName
bgDanger = ClassName "bg-danger"

bgInfo :: ClassName
bgInfo = ClassName "bg-info"

bgPrimary :: ClassName
bgPrimary = ClassName "bg-primary"

bgSuccess :: ClassName
bgSuccess = ClassName "bg-success"

bgWarning :: ClassName
bgWarning = ClassName "bg-warning"

blockquoteReverse :: ClassName
blockquoteReverse = ClassName "blockquote-reverse"

bottom :: ClassName
bottom = ClassName "bottom"

bottomLeft :: ClassName
bottomLeft = ClassName "bottom-left"

bottomRight :: ClassName
bottomRight = ClassName "bottom-right"

breadcrumb :: ClassName
breadcrumb = ClassName "breadcrumb"

btn :: ClassName
btn = ClassName "btn"

btnBlock :: ClassName
btnBlock = ClassName "btn-block"

btnDanger :: ClassName
btnDanger = ClassName "btn-danger"

btnDefault :: ClassName
btnDefault = ClassName "btn-default"

btnGroup :: ClassName
btnGroup = ClassName "btn-group"

btnGroupJustified :: ClassName
btnGroupJustified = ClassName "btn-group-justified"

btnGroupLg :: ClassName
btnGroupLg = ClassName "btn-group-lg"

btnGroupSm :: ClassName
btnGroupSm = ClassName "btn-group-sm"

btnGroupVertical :: ClassName
btnGroupVertical = ClassName "btn-group-vertical"

btnGroupXs :: ClassName
btnGroupXs = ClassName "btn-group-xs"

btnInfo :: ClassName
btnInfo = ClassName "btn-info"

btnLg :: ClassName
btnLg = ClassName "btn-lg"

btnLink :: ClassName
btnLink = ClassName "btn-link"

btnPrimary :: ClassName
btnPrimary = ClassName "btn-primary"

btnSm :: ClassName
btnSm = ClassName "btn-sm"

btnSuccess :: ClassName
btnSuccess = ClassName "btn-success"

btnToolbar :: ClassName
btnToolbar = ClassName "btn-toolbar"

btnWarning :: ClassName
btnWarning = ClassName "btn-warning"

btnXs :: ClassName
btnXs = ClassName "btn-xs"

caption :: ClassName
caption = ClassName "caption"

caret :: ClassName
caret = ClassName "caret"

carousel :: ClassName
carousel = ClassName "carousel"

carouselCaption :: ClassName
carouselCaption = ClassName "carousel-caption"

carouselControl :: ClassName
carouselControl = ClassName "carousel-control"

carouselIndicators :: ClassName
carouselIndicators = ClassName "carousel-indicators"

carouselInner :: ClassName
carouselInner = ClassName "carousel-inner"

centerBlock :: ClassName
centerBlock = ClassName "center-block"

checkbox :: ClassName
checkbox = ClassName "checkbox"

checkboxInline :: ClassName
checkboxInline = ClassName "checkbox-inline"

clearfix :: ClassName
clearfix = ClassName "clearfix"

close :: ClassName
close = ClassName "close"

colLg1 :: ClassName
colLg1 = ClassName "col-lg-1"

colLg10 :: ClassName
colLg10 = ClassName "col-lg-10"

colLg11 :: ClassName
colLg11 = ClassName "col-lg-11"

colLg12 :: ClassName
colLg12 = ClassName "col-lg-12"

colLg2 :: ClassName
colLg2 = ClassName "col-lg-2"

colLg3 :: ClassName
colLg3 = ClassName "col-lg-3"

colLg4 :: ClassName
colLg4 = ClassName "col-lg-4"

colLg5 :: ClassName
colLg5 = ClassName "col-lg-5"

colLg6 :: ClassName
colLg6 = ClassName "col-lg-6"

colLg7 :: ClassName
colLg7 = ClassName "col-lg-7"

colLg8 :: ClassName
colLg8 = ClassName "col-lg-8"

colLg9 :: ClassName
colLg9 = ClassName "col-lg-9"

colLgOffset0 :: ClassName
colLgOffset0 = ClassName "col-lg-offset-0"

colLgOffset1 :: ClassName
colLgOffset1 = ClassName "col-lg-offset-1"

colLgOffset10 :: ClassName
colLgOffset10 = ClassName "col-lg-offset-10"

colLgOffset11 :: ClassName
colLgOffset11 = ClassName "col-lg-offset-11"

colLgOffset12 :: ClassName
colLgOffset12 = ClassName "col-lg-offset-12"

colLgOffset2 :: ClassName
colLgOffset2 = ClassName "col-lg-offset-2"

colLgOffset3 :: ClassName
colLgOffset3 = ClassName "col-lg-offset-3"

colLgOffset4 :: ClassName
colLgOffset4 = ClassName "col-lg-offset-4"

colLgOffset5 :: ClassName
colLgOffset5 = ClassName "col-lg-offset-5"

colLgOffset6 :: ClassName
colLgOffset6 = ClassName "col-lg-offset-6"

colLgOffset7 :: ClassName
colLgOffset7 = ClassName "col-lg-offset-7"

colLgOffset8 :: ClassName
colLgOffset8 = ClassName "col-lg-offset-8"

colLgOffset9 :: ClassName
colLgOffset9 = ClassName "col-lg-offset-9"

colLgPull0 :: ClassName
colLgPull0 = ClassName "col-lg-pull-0"

colLgPull1 :: ClassName
colLgPull1 = ClassName "col-lg-pull-1"

colLgPull10 :: ClassName
colLgPull10 = ClassName "col-lg-pull-10"

colLgPull11 :: ClassName
colLgPull11 = ClassName "col-lg-pull-11"

colLgPull12 :: ClassName
colLgPull12 = ClassName "col-lg-pull-12"

colLgPull2 :: ClassName
colLgPull2 = ClassName "col-lg-pull-2"

colLgPull3 :: ClassName
colLgPull3 = ClassName "col-lg-pull-3"

colLgPull4 :: ClassName
colLgPull4 = ClassName "col-lg-pull-4"

colLgPull5 :: ClassName
colLgPull5 = ClassName "col-lg-pull-5"

colLgPull6 :: ClassName
colLgPull6 = ClassName "col-lg-pull-6"

colLgPull7 :: ClassName
colLgPull7 = ClassName "col-lg-pull-7"

colLgPull8 :: ClassName
colLgPull8 = ClassName "col-lg-pull-8"

colLgPull9 :: ClassName
colLgPull9 = ClassName "col-lg-pull-9"

colLgPush0 :: ClassName
colLgPush0 = ClassName "col-lg-push-0"

colLgPush1 :: ClassName
colLgPush1 = ClassName "col-lg-push-1"

colLgPush10 :: ClassName
colLgPush10 = ClassName "col-lg-push-10"

colLgPush11 :: ClassName
colLgPush11 = ClassName "col-lg-push-11"

colLgPush12 :: ClassName
colLgPush12 = ClassName "col-lg-push-12"

colLgPush2 :: ClassName
colLgPush2 = ClassName "col-lg-push-2"

colLgPush3 :: ClassName
colLgPush3 = ClassName "col-lg-push-3"

colLgPush4 :: ClassName
colLgPush4 = ClassName "col-lg-push-4"

colLgPush5 :: ClassName
colLgPush5 = ClassName "col-lg-push-5"

colLgPush6 :: ClassName
colLgPush6 = ClassName "col-lg-push-6"

colLgPush7 :: ClassName
colLgPush7 = ClassName "col-lg-push-7"

colLgPush8 :: ClassName
colLgPush8 = ClassName "col-lg-push-8"

colLgPush9 :: ClassName
colLgPush9 = ClassName "col-lg-push-9"

colMd1 :: ClassName
colMd1 = ClassName "col-md-1"

colMd10 :: ClassName
colMd10 = ClassName "col-md-10"

colMd11 :: ClassName
colMd11 = ClassName "col-md-11"

colMd12 :: ClassName
colMd12 = ClassName "col-md-12"

colMd2 :: ClassName
colMd2 = ClassName "col-md-2"

colMd3 :: ClassName
colMd3 = ClassName "col-md-3"

colMd4 :: ClassName
colMd4 = ClassName "col-md-4"

colMd5 :: ClassName
colMd5 = ClassName "col-md-5"

colMd6 :: ClassName
colMd6 = ClassName "col-md-6"

colMd7 :: ClassName
colMd7 = ClassName "col-md-7"

colMd8 :: ClassName
colMd8 = ClassName "col-md-8"

colMd9 :: ClassName
colMd9 = ClassName "col-md-9"

colMdOffset0 :: ClassName
colMdOffset0 = ClassName "col-md-offset-0"

colMdOffset1 :: ClassName
colMdOffset1 = ClassName "col-md-offset-1"

colMdOffset10 :: ClassName
colMdOffset10 = ClassName "col-md-offset-10"

colMdOffset11 :: ClassName
colMdOffset11 = ClassName "col-md-offset-11"

colMdOffset12 :: ClassName
colMdOffset12 = ClassName "col-md-offset-12"

colMdOffset2 :: ClassName
colMdOffset2 = ClassName "col-md-offset-2"

colMdOffset3 :: ClassName
colMdOffset3 = ClassName "col-md-offset-3"

colMdOffset4 :: ClassName
colMdOffset4 = ClassName "col-md-offset-4"

colMdOffset5 :: ClassName
colMdOffset5 = ClassName "col-md-offset-5"

colMdOffset6 :: ClassName
colMdOffset6 = ClassName "col-md-offset-6"

colMdOffset7 :: ClassName
colMdOffset7 = ClassName "col-md-offset-7"

colMdOffset8 :: ClassName
colMdOffset8 = ClassName "col-md-offset-8"

colMdOffset9 :: ClassName
colMdOffset9 = ClassName "col-md-offset-9"

colMdPull0 :: ClassName
colMdPull0 = ClassName "col-md-pull-0"

colMdPull1 :: ClassName
colMdPull1 = ClassName "col-md-pull-1"

colMdPull10 :: ClassName
colMdPull10 = ClassName "col-md-pull-10"

colMdPull11 :: ClassName
colMdPull11 = ClassName "col-md-pull-11"

colMdPull12 :: ClassName
colMdPull12 = ClassName "col-md-pull-12"

colMdPull2 :: ClassName
colMdPull2 = ClassName "col-md-pull-2"

colMdPull3 :: ClassName
colMdPull3 = ClassName "col-md-pull-3"

colMdPull4 :: ClassName
colMdPull4 = ClassName "col-md-pull-4"

colMdPull5 :: ClassName
colMdPull5 = ClassName "col-md-pull-5"

colMdPull6 :: ClassName
colMdPull6 = ClassName "col-md-pull-6"

colMdPull7 :: ClassName
colMdPull7 = ClassName "col-md-pull-7"

colMdPull8 :: ClassName
colMdPull8 = ClassName "col-md-pull-8"

colMdPull9 :: ClassName
colMdPull9 = ClassName "col-md-pull-9"

colMdPush0 :: ClassName
colMdPush0 = ClassName "col-md-push-0"

colMdPush1 :: ClassName
colMdPush1 = ClassName "col-md-push-1"

colMdPush10 :: ClassName
colMdPush10 = ClassName "col-md-push-10"

colMdPush11 :: ClassName
colMdPush11 = ClassName "col-md-push-11"

colMdPush12 :: ClassName
colMdPush12 = ClassName "col-md-push-12"

colMdPush2 :: ClassName
colMdPush2 = ClassName "col-md-push-2"

colMdPush3 :: ClassName
colMdPush3 = ClassName "col-md-push-3"

colMdPush4 :: ClassName
colMdPush4 = ClassName "col-md-push-4"

colMdPush5 :: ClassName
colMdPush5 = ClassName "col-md-push-5"

colMdPush6 :: ClassName
colMdPush6 = ClassName "col-md-push-6"

colMdPush7 :: ClassName
colMdPush7 = ClassName "col-md-push-7"

colMdPush8 :: ClassName
colMdPush8 = ClassName "col-md-push-8"

colMdPush9 :: ClassName
colMdPush9 = ClassName "col-md-push-9"

colSm1 :: ClassName
colSm1 = ClassName "col-sm-1"

colSm10 :: ClassName
colSm10 = ClassName "col-sm-10"

colSm11 :: ClassName
colSm11 = ClassName "col-sm-11"

colSm12 :: ClassName
colSm12 = ClassName "col-sm-12"

colSm2 :: ClassName
colSm2 = ClassName "col-sm-2"

colSm3 :: ClassName
colSm3 = ClassName "col-sm-3"

colSm4 :: ClassName
colSm4 = ClassName "col-sm-4"

colSm5 :: ClassName
colSm5 = ClassName "col-sm-5"

colSm6 :: ClassName
colSm6 = ClassName "col-sm-6"

colSm7 :: ClassName
colSm7 = ClassName "col-sm-7"

colSm8 :: ClassName
colSm8 = ClassName "col-sm-8"

colSm9 :: ClassName
colSm9 = ClassName "col-sm-9"

colSmOffset0 :: ClassName
colSmOffset0 = ClassName "col-sm-offset-0"

colSmOffset1 :: ClassName
colSmOffset1 = ClassName "col-sm-offset-1"

colSmOffset10 :: ClassName
colSmOffset10 = ClassName "col-sm-offset-10"

colSmOffset11 :: ClassName
colSmOffset11 = ClassName "col-sm-offset-11"

colSmOffset12 :: ClassName
colSmOffset12 = ClassName "col-sm-offset-12"

colSmOffset2 :: ClassName
colSmOffset2 = ClassName "col-sm-offset-2"

colSmOffset3 :: ClassName
colSmOffset3 = ClassName "col-sm-offset-3"

colSmOffset4 :: ClassName
colSmOffset4 = ClassName "col-sm-offset-4"

colSmOffset5 :: ClassName
colSmOffset5 = ClassName "col-sm-offset-5"

colSmOffset6 :: ClassName
colSmOffset6 = ClassName "col-sm-offset-6"

colSmOffset7 :: ClassName
colSmOffset7 = ClassName "col-sm-offset-7"

colSmOffset8 :: ClassName
colSmOffset8 = ClassName "col-sm-offset-8"

colSmOffset9 :: ClassName
colSmOffset9 = ClassName "col-sm-offset-9"

colSmPull0 :: ClassName
colSmPull0 = ClassName "col-sm-pull-0"

colSmPull1 :: ClassName
colSmPull1 = ClassName "col-sm-pull-1"

colSmPull10 :: ClassName
colSmPull10 = ClassName "col-sm-pull-10"

colSmPull11 :: ClassName
colSmPull11 = ClassName "col-sm-pull-11"

colSmPull12 :: ClassName
colSmPull12 = ClassName "col-sm-pull-12"

colSmPull2 :: ClassName
colSmPull2 = ClassName "col-sm-pull-2"

colSmPull3 :: ClassName
colSmPull3 = ClassName "col-sm-pull-3"

colSmPull4 :: ClassName
colSmPull4 = ClassName "col-sm-pull-4"

colSmPull5 :: ClassName
colSmPull5 = ClassName "col-sm-pull-5"

colSmPull6 :: ClassName
colSmPull6 = ClassName "col-sm-pull-6"

colSmPull7 :: ClassName
colSmPull7 = ClassName "col-sm-pull-7"

colSmPull8 :: ClassName
colSmPull8 = ClassName "col-sm-pull-8"

colSmPull9 :: ClassName
colSmPull9 = ClassName "col-sm-pull-9"

colSmPush0 :: ClassName
colSmPush0 = ClassName "col-sm-push-0"

colSmPush1 :: ClassName
colSmPush1 = ClassName "col-sm-push-1"

colSmPush10 :: ClassName
colSmPush10 = ClassName "col-sm-push-10"

colSmPush11 :: ClassName
colSmPush11 = ClassName "col-sm-push-11"

colSmPush12 :: ClassName
colSmPush12 = ClassName "col-sm-push-12"

colSmPush2 :: ClassName
colSmPush2 = ClassName "col-sm-push-2"

colSmPush3 :: ClassName
colSmPush3 = ClassName "col-sm-push-3"

colSmPush4 :: ClassName
colSmPush4 = ClassName "col-sm-push-4"

colSmPush5 :: ClassName
colSmPush5 = ClassName "col-sm-push-5"

colSmPush6 :: ClassName
colSmPush6 = ClassName "col-sm-push-6"

colSmPush7 :: ClassName
colSmPush7 = ClassName "col-sm-push-7"

colSmPush8 :: ClassName
colSmPush8 = ClassName "col-sm-push-8"

colSmPush9 :: ClassName
colSmPush9 = ClassName "col-sm-push-9"

colXs1 :: ClassName
colXs1 = ClassName "col-xs-1"

colXs10 :: ClassName
colXs10 = ClassName "col-xs-10"

colXs11 :: ClassName
colXs11 = ClassName "col-xs-11"

colXs12 :: ClassName
colXs12 = ClassName "col-xs-12"

colXs2 :: ClassName
colXs2 = ClassName "col-xs-2"

colXs3 :: ClassName
colXs3 = ClassName "col-xs-3"

colXs4 :: ClassName
colXs4 = ClassName "col-xs-4"

colXs5 :: ClassName
colXs5 = ClassName "col-xs-5"

colXs6 :: ClassName
colXs6 = ClassName "col-xs-6"

colXs7 :: ClassName
colXs7 = ClassName "col-xs-7"

colXs8 :: ClassName
colXs8 = ClassName "col-xs-8"

colXs9 :: ClassName
colXs9 = ClassName "col-xs-9"

colXsOffset0 :: ClassName
colXsOffset0 = ClassName "col-xs-offset-0"

colXsOffset1 :: ClassName
colXsOffset1 = ClassName "col-xs-offset-1"

colXsOffset10 :: ClassName
colXsOffset10 = ClassName "col-xs-offset-10"

colXsOffset11 :: ClassName
colXsOffset11 = ClassName "col-xs-offset-11"

colXsOffset12 :: ClassName
colXsOffset12 = ClassName "col-xs-offset-12"

colXsOffset2 :: ClassName
colXsOffset2 = ClassName "col-xs-offset-2"

colXsOffset3 :: ClassName
colXsOffset3 = ClassName "col-xs-offset-3"

colXsOffset4 :: ClassName
colXsOffset4 = ClassName "col-xs-offset-4"

colXsOffset5 :: ClassName
colXsOffset5 = ClassName "col-xs-offset-5"

colXsOffset6 :: ClassName
colXsOffset6 = ClassName "col-xs-offset-6"

colXsOffset7 :: ClassName
colXsOffset7 = ClassName "col-xs-offset-7"

colXsOffset8 :: ClassName
colXsOffset8 = ClassName "col-xs-offset-8"

colXsOffset9 :: ClassName
colXsOffset9 = ClassName "col-xs-offset-9"

colXsPull0 :: ClassName
colXsPull0 = ClassName "col-xs-pull-0"

colXsPull1 :: ClassName
colXsPull1 = ClassName "col-xs-pull-1"

colXsPull10 :: ClassName
colXsPull10 = ClassName "col-xs-pull-10"

colXsPull11 :: ClassName
colXsPull11 = ClassName "col-xs-pull-11"

colXsPull12 :: ClassName
colXsPull12 = ClassName "col-xs-pull-12"

colXsPull2 :: ClassName
colXsPull2 = ClassName "col-xs-pull-2"

colXsPull3 :: ClassName
colXsPull3 = ClassName "col-xs-pull-3"

colXsPull4 :: ClassName
colXsPull4 = ClassName "col-xs-pull-4"

colXsPull5 :: ClassName
colXsPull5 = ClassName "col-xs-pull-5"

colXsPull6 :: ClassName
colXsPull6 = ClassName "col-xs-pull-6"

colXsPull7 :: ClassName
colXsPull7 = ClassName "col-xs-pull-7"

colXsPull8 :: ClassName
colXsPull8 = ClassName "col-xs-pull-8"

colXsPull9 :: ClassName
colXsPull9 = ClassName "col-xs-pull-9"

colXsPush0 :: ClassName
colXsPush0 = ClassName "col-xs-push-0"

colXsPush1 :: ClassName
colXsPush1 = ClassName "col-xs-push-1"

colXsPush10 :: ClassName
colXsPush10 = ClassName "col-xs-push-10"

colXsPush11 :: ClassName
colXsPush11 = ClassName "col-xs-push-11"

colXsPush12 :: ClassName
colXsPush12 = ClassName "col-xs-push-12"

colXsPush2 :: ClassName
colXsPush2 = ClassName "col-xs-push-2"

colXsPush3 :: ClassName
colXsPush3 = ClassName "col-xs-push-3"

colXsPush4 :: ClassName
colXsPush4 = ClassName "col-xs-push-4"

colXsPush5 :: ClassName
colXsPush5 = ClassName "col-xs-push-5"

colXsPush6 :: ClassName
colXsPush6 = ClassName "col-xs-push-6"

colXsPush7 :: ClassName
colXsPush7 = ClassName "col-xs-push-7"

colXsPush8 :: ClassName
colXsPush8 = ClassName "col-xs-push-8"

colXsPush9 :: ClassName
colXsPush9 = ClassName "col-xs-push-9"

collapse :: ClassName
collapse = ClassName "collapse"

collapsing :: ClassName
collapsing = ClassName "collapsing"

container :: ClassName
container = ClassName "container"

containerFluid :: ClassName
containerFluid = ClassName "container-fluid"

controlLabel :: ClassName
controlLabel = ClassName "control-label"

danger :: ClassName
danger = ClassName "danger"

disabled :: ClassName
disabled = ClassName "disabled"

divider :: ClassName
divider = ClassName "divider"

dlHorizontal :: ClassName
dlHorizontal = ClassName "dl-horizontal"

dropdown :: ClassName
dropdown = ClassName "dropdown"

dropdownBackdrop :: ClassName
dropdownBackdrop = ClassName "dropdown-backdrop"

dropdownHeader :: ClassName
dropdownHeader = ClassName "dropdown-header"

dropdownMenu :: ClassName
dropdownMenu = ClassName "dropdown-menu"

dropdownMenuLeft :: ClassName
dropdownMenuLeft = ClassName "dropdown-menu-left"

dropdownMenuRight :: ClassName
dropdownMenuRight = ClassName "dropdown-menu-right"

dropdownToggle :: ClassName
dropdownToggle = ClassName "dropdown-toggle"

dropup :: ClassName
dropup = ClassName "dropup"

embedResponsive :: ClassName
embedResponsive = ClassName "embed-responsive"

embedResponsive16By9 :: ClassName
embedResponsive16By9 = ClassName "embed-responsive-16by9"

embedResponsive4By3 :: ClassName
embedResponsive4By3 = ClassName "embed-responsive-4by3"

embedResponsiveItem :: ClassName
embedResponsiveItem = ClassName "embed-responsive-item"

eot :: ClassName
eot = ClassName "eot"

fade :: ClassName
fade = ClassName "fade"

focus :: ClassName
focus = ClassName "focus"

formControl :: ClassName
formControl = ClassName "form-control"

formControlFeedback :: ClassName
formControlFeedback = ClassName "form-control-feedback"

formControlStatic :: ClassName
formControlStatic = ClassName "form-control-static"

formGroup :: ClassName
formGroup = ClassName "form-group"

formGroupLg :: ClassName
formGroupLg = ClassName "form-group-lg"

formGroupSm :: ClassName
formGroupSm = ClassName "form-group-sm"

formHorizontal :: ClassName
formHorizontal = ClassName "form-horizontal"

formInline :: ClassName
formInline = ClassName "form-inline"

glyphicon :: ClassName
glyphicon = ClassName "glyphicon"

glyphiconAdjust :: ClassName
glyphiconAdjust = ClassName "glyphicon-adjust"

glyphiconAlert :: ClassName
glyphiconAlert = ClassName "glyphicon-alert"

glyphiconAlignCenter :: ClassName
glyphiconAlignCenter = ClassName "glyphicon-align-center"

glyphiconAlignJustify :: ClassName
glyphiconAlignJustify = ClassName "glyphicon-align-justify"

glyphiconAlignLeft :: ClassName
glyphiconAlignLeft = ClassName "glyphicon-align-left"

glyphiconAlignRight :: ClassName
glyphiconAlignRight = ClassName "glyphicon-align-right"

glyphiconApple :: ClassName
glyphiconApple = ClassName "glyphicon-apple"

glyphiconArrowDown :: ClassName
glyphiconArrowDown = ClassName "glyphicon-arrow-down"

glyphiconArrowLeft :: ClassName
glyphiconArrowLeft = ClassName "glyphicon-arrow-left"

glyphiconArrowRight :: ClassName
glyphiconArrowRight = ClassName "glyphicon-arrow-right"

glyphiconArrowUp :: ClassName
glyphiconArrowUp = ClassName "glyphicon-arrow-up"

glyphiconAsterisk :: ClassName
glyphiconAsterisk = ClassName "glyphicon-asterisk"

glyphiconBabyFormula :: ClassName
glyphiconBabyFormula = ClassName "glyphicon-baby-formula"

glyphiconBackward :: ClassName
glyphiconBackward = ClassName "glyphicon-backward"

glyphiconBanCircle :: ClassName
glyphiconBanCircle = ClassName "glyphicon-ban-circle"

glyphiconBarcode :: ClassName
glyphiconBarcode = ClassName "glyphicon-barcode"

glyphiconBed :: ClassName
glyphiconBed = ClassName "glyphicon-bed"

glyphiconBell :: ClassName
glyphiconBell = ClassName "glyphicon-bell"

glyphiconBishop :: ClassName
glyphiconBishop = ClassName "glyphicon-bishop"

glyphiconBitcoin :: ClassName
glyphiconBitcoin = ClassName "glyphicon-bitcoin"

glyphiconBlackboard :: ClassName
glyphiconBlackboard = ClassName "glyphicon-blackboard"

glyphiconBold :: ClassName
glyphiconBold = ClassName "glyphicon-bold"

glyphiconBook :: ClassName
glyphiconBook = ClassName "glyphicon-book"

glyphiconBookmark :: ClassName
glyphiconBookmark = ClassName "glyphicon-bookmark"

glyphiconBriefcase :: ClassName
glyphiconBriefcase = ClassName "glyphicon-briefcase"

glyphiconBullhorn :: ClassName
glyphiconBullhorn = ClassName "glyphicon-bullhorn"

glyphiconCalendar :: ClassName
glyphiconCalendar = ClassName "glyphicon-calendar"

glyphiconCamera :: ClassName
glyphiconCamera = ClassName "glyphicon-camera"

glyphiconCd :: ClassName
glyphiconCd = ClassName "glyphicon-cd"

glyphiconCertificate :: ClassName
glyphiconCertificate = ClassName "glyphicon-certificate"

glyphiconCheck :: ClassName
glyphiconCheck = ClassName "glyphicon-check"

glyphiconChevronDown :: ClassName
glyphiconChevronDown = ClassName "glyphicon-chevron-down"

glyphiconChevronLeft :: ClassName
glyphiconChevronLeft = ClassName "glyphicon-chevron-left"

glyphiconChevronRight :: ClassName
glyphiconChevronRight = ClassName "glyphicon-chevron-right"

glyphiconChevronUp :: ClassName
glyphiconChevronUp = ClassName "glyphicon-chevron-up"

glyphiconCircleArrowDown :: ClassName
glyphiconCircleArrowDown = ClassName "glyphicon-circle-arrow-down"

glyphiconCircleArrowLeft :: ClassName
glyphiconCircleArrowLeft = ClassName "glyphicon-circle-arrow-left"

glyphiconCircleArrowRight :: ClassName
glyphiconCircleArrowRight = ClassName "glyphicon-circle-arrow-right"

glyphiconCircleArrowUp :: ClassName
glyphiconCircleArrowUp = ClassName "glyphicon-circle-arrow-up"

glyphiconCloud :: ClassName
glyphiconCloud = ClassName "glyphicon-cloud"

glyphiconCloudDownload :: ClassName
glyphiconCloudDownload = ClassName "glyphicon-cloud-download"

glyphiconCloudUpload :: ClassName
glyphiconCloudUpload = ClassName "glyphicon-cloud-upload"

glyphiconCog :: ClassName
glyphiconCog = ClassName "glyphicon-cog"

glyphiconCollapseDown :: ClassName
glyphiconCollapseDown = ClassName "glyphicon-collapse-down"

glyphiconCollapseUp :: ClassName
glyphiconCollapseUp = ClassName "glyphicon-collapse-up"

glyphiconComment :: ClassName
glyphiconComment = ClassName "glyphicon-comment"

glyphiconCompressed :: ClassName
glyphiconCompressed = ClassName "glyphicon-compressed"

glyphiconConsole :: ClassName
glyphiconConsole = ClassName "glyphicon-console"

glyphiconCopy :: ClassName
glyphiconCopy = ClassName "glyphicon-copy"

glyphiconCopyrightMark :: ClassName
glyphiconCopyrightMark = ClassName "glyphicon-copyright-mark"

glyphiconCreditCard :: ClassName
glyphiconCreditCard = ClassName "glyphicon-credit-card"

glyphiconCutlery :: ClassName
glyphiconCutlery = ClassName "glyphicon-cutlery"

glyphiconDashboard :: ClassName
glyphiconDashboard = ClassName "glyphicon-dashboard"

glyphiconDownload :: ClassName
glyphiconDownload = ClassName "glyphicon-download"

glyphiconDownloadAlt :: ClassName
glyphiconDownloadAlt = ClassName "glyphicon-download-alt"

glyphiconDuplicate :: ClassName
glyphiconDuplicate = ClassName "glyphicon-duplicate"

glyphiconEarphone :: ClassName
glyphiconEarphone = ClassName "glyphicon-earphone"

glyphiconEdit :: ClassName
glyphiconEdit = ClassName "glyphicon-edit"

glyphiconEducation :: ClassName
glyphiconEducation = ClassName "glyphicon-education"

glyphiconEject :: ClassName
glyphiconEject = ClassName "glyphicon-eject"

glyphiconEnvelope :: ClassName
glyphiconEnvelope = ClassName "glyphicon-envelope"

glyphiconEqualizer :: ClassName
glyphiconEqualizer = ClassName "glyphicon-equalizer"

glyphiconErase :: ClassName
glyphiconErase = ClassName "glyphicon-erase"

glyphiconEur :: ClassName
glyphiconEur = ClassName "glyphicon-eur"

glyphiconEuro :: ClassName
glyphiconEuro = ClassName "glyphicon-euro"

glyphiconExclamationSign :: ClassName
glyphiconExclamationSign = ClassName "glyphicon-exclamation-sign"

glyphiconExpand :: ClassName
glyphiconExpand = ClassName "glyphicon-expand"

glyphiconExport :: ClassName
glyphiconExport = ClassName "glyphicon-export"

glyphiconEyeClose :: ClassName
glyphiconEyeClose = ClassName "glyphicon-eye-close"

glyphiconEyeOpen :: ClassName
glyphiconEyeOpen = ClassName "glyphicon-eye-open"

glyphiconFacetimeVideo :: ClassName
glyphiconFacetimeVideo = ClassName "glyphicon-facetime-video"

glyphiconFastBackward :: ClassName
glyphiconFastBackward = ClassName "glyphicon-fast-backward"

glyphiconFastForward :: ClassName
glyphiconFastForward = ClassName "glyphicon-fast-forward"

glyphiconFile :: ClassName
glyphiconFile = ClassName "glyphicon-file"

glyphiconFilm :: ClassName
glyphiconFilm = ClassName "glyphicon-film"

glyphiconFilter :: ClassName
glyphiconFilter = ClassName "glyphicon-filter"

glyphiconFire :: ClassName
glyphiconFire = ClassName "glyphicon-fire"

glyphiconFlag :: ClassName
glyphiconFlag = ClassName "glyphicon-flag"

glyphiconFlash :: ClassName
glyphiconFlash = ClassName "glyphicon-flash"

glyphiconFloppyDisk :: ClassName
glyphiconFloppyDisk = ClassName "glyphicon-floppy-disk"

glyphiconFloppyOpen :: ClassName
glyphiconFloppyOpen = ClassName "glyphicon-floppy-open"

glyphiconFloppyRemove :: ClassName
glyphiconFloppyRemove = ClassName "glyphicon-floppy-remove"

glyphiconFloppySave :: ClassName
glyphiconFloppySave = ClassName "glyphicon-floppy-save"

glyphiconFloppySaved :: ClassName
glyphiconFloppySaved = ClassName "glyphicon-floppy-saved"

glyphiconFolderClose :: ClassName
glyphiconFolderClose = ClassName "glyphicon-folder-close"

glyphiconFolderOpen :: ClassName
glyphiconFolderOpen = ClassName "glyphicon-folder-open"

glyphiconFont :: ClassName
glyphiconFont = ClassName "glyphicon-font"

glyphiconForward :: ClassName
glyphiconForward = ClassName "glyphicon-forward"

glyphiconFullscreen :: ClassName
glyphiconFullscreen = ClassName "glyphicon-fullscreen"

glyphiconGbp :: ClassName
glyphiconGbp = ClassName "glyphicon-gbp"

glyphiconGift :: ClassName
glyphiconGift = ClassName "glyphicon-gift"

glyphiconGlass :: ClassName
glyphiconGlass = ClassName "glyphicon-glass"

glyphiconGlobe :: ClassName
glyphiconGlobe = ClassName "glyphicon-globe"

glyphiconGrain :: ClassName
glyphiconGrain = ClassName "glyphicon-grain"

glyphiconHandDown :: ClassName
glyphiconHandDown = ClassName "glyphicon-hand-down"

glyphiconHandLeft :: ClassName
glyphiconHandLeft = ClassName "glyphicon-hand-left"

glyphiconHandRight :: ClassName
glyphiconHandRight = ClassName "glyphicon-hand-right"

glyphiconHandUp :: ClassName
glyphiconHandUp = ClassName "glyphicon-hand-up"

glyphiconHdVideo :: ClassName
glyphiconHdVideo = ClassName "glyphicon-hd-video"

glyphiconHdd :: ClassName
glyphiconHdd = ClassName "glyphicon-hdd"

glyphiconHeader :: ClassName
glyphiconHeader = ClassName "glyphicon-header"

glyphiconHeadphones :: ClassName
glyphiconHeadphones = ClassName "glyphicon-headphones"

glyphiconHeart :: ClassName
glyphiconHeart = ClassName "glyphicon-heart"

glyphiconHeartEmpty :: ClassName
glyphiconHeartEmpty = ClassName "glyphicon-heart-empty"

glyphiconHome :: ClassName
glyphiconHome = ClassName "glyphicon-home"

glyphiconHourglass :: ClassName
glyphiconHourglass = ClassName "glyphicon-hourglass"

glyphiconIceLolly :: ClassName
glyphiconIceLolly = ClassName "glyphicon-ice-lolly"

glyphiconIceLollyTasted :: ClassName
glyphiconIceLollyTasted = ClassName "glyphicon-ice-lolly-tasted"

glyphiconImport :: ClassName
glyphiconImport = ClassName "glyphicon-import"

glyphiconInbox :: ClassName
glyphiconInbox = ClassName "glyphicon-inbox"

glyphiconIndentLeft :: ClassName
glyphiconIndentLeft = ClassName "glyphicon-indent-left"

glyphiconIndentRight :: ClassName
glyphiconIndentRight = ClassName "glyphicon-indent-right"

glyphiconInfoSign :: ClassName
glyphiconInfoSign = ClassName "glyphicon-info-sign"

glyphiconItalic :: ClassName
glyphiconItalic = ClassName "glyphicon-italic"

glyphiconKing :: ClassName
glyphiconKing = ClassName "glyphicon-king"

glyphiconKnight :: ClassName
glyphiconKnight = ClassName "glyphicon-knight"

glyphiconLamp :: ClassName
glyphiconLamp = ClassName "glyphicon-lamp"

glyphiconLeaf :: ClassName
glyphiconLeaf = ClassName "glyphicon-leaf"

glyphiconLevelUp :: ClassName
glyphiconLevelUp = ClassName "glyphicon-level-up"

glyphiconLink :: ClassName
glyphiconLink = ClassName "glyphicon-link"

glyphiconList :: ClassName
glyphiconList = ClassName "glyphicon-list"

glyphiconListAlt :: ClassName
glyphiconListAlt = ClassName "glyphicon-list-alt"

glyphiconLock :: ClassName
glyphiconLock = ClassName "glyphicon-lock"

glyphiconLogIn :: ClassName
glyphiconLogIn = ClassName "glyphicon-log-in"

glyphiconLogOut :: ClassName
glyphiconLogOut = ClassName "glyphicon-log-out"

glyphiconMagnet :: ClassName
glyphiconMagnet = ClassName "glyphicon-magnet"

glyphiconMapMarker :: ClassName
glyphiconMapMarker = ClassName "glyphicon-map-marker"

glyphiconMenuDown :: ClassName
glyphiconMenuDown = ClassName "glyphicon-menu-down"

glyphiconMenuHamburger :: ClassName
glyphiconMenuHamburger = ClassName "glyphicon-menu-hamburger"

glyphiconMenuLeft :: ClassName
glyphiconMenuLeft = ClassName "glyphicon-menu-left"

glyphiconMenuRight :: ClassName
glyphiconMenuRight = ClassName "glyphicon-menu-right"

glyphiconMenuUp :: ClassName
glyphiconMenuUp = ClassName "glyphicon-menu-up"

glyphiconMinus :: ClassName
glyphiconMinus = ClassName "glyphicon-minus"

glyphiconMinusSign :: ClassName
glyphiconMinusSign = ClassName "glyphicon-minus-sign"

glyphiconModalWindow :: ClassName
glyphiconModalWindow = ClassName "glyphicon-modal-window"

glyphiconMove :: ClassName
glyphiconMove = ClassName "glyphicon-move"

glyphiconMusic :: ClassName
glyphiconMusic = ClassName "glyphicon-music"

glyphiconNewWindow :: ClassName
glyphiconNewWindow = ClassName "glyphicon-new-window"

glyphiconObjectAlignBottom :: ClassName
glyphiconObjectAlignBottom = ClassName "glyphicon-object-align-bottom"

glyphiconObjectAlignHorizontal :: ClassName
glyphiconObjectAlignHorizontal = ClassName "glyphicon-object-align-horizontal"

glyphiconObjectAlignLeft :: ClassName
glyphiconObjectAlignLeft = ClassName "glyphicon-object-align-left"

glyphiconObjectAlignRight :: ClassName
glyphiconObjectAlignRight = ClassName "glyphicon-object-align-right"

glyphiconObjectAlignTop :: ClassName
glyphiconObjectAlignTop = ClassName "glyphicon-object-align-top"

glyphiconObjectAlignVertical :: ClassName
glyphiconObjectAlignVertical = ClassName "glyphicon-object-align-vertical"

glyphiconOff :: ClassName
glyphiconOff = ClassName "glyphicon-off"

glyphiconOil :: ClassName
glyphiconOil = ClassName "glyphicon-oil"

glyphiconOk :: ClassName
glyphiconOk = ClassName "glyphicon-ok"

glyphiconOkCircle :: ClassName
glyphiconOkCircle = ClassName "glyphicon-ok-circle"

glyphiconOkSign :: ClassName
glyphiconOkSign = ClassName "glyphicon-ok-sign"

glyphiconOpen :: ClassName
glyphiconOpen = ClassName "glyphicon-open"

glyphiconOpenFile :: ClassName
glyphiconOpenFile = ClassName "glyphicon-open-file"

glyphiconOptionHorizontal :: ClassName
glyphiconOptionHorizontal = ClassName "glyphicon-option-horizontal"

glyphiconOptionVertical :: ClassName
glyphiconOptionVertical = ClassName "glyphicon-option-vertical"

glyphiconPaperclip :: ClassName
glyphiconPaperclip = ClassName "glyphicon-paperclip"

glyphiconPaste :: ClassName
glyphiconPaste = ClassName "glyphicon-paste"

glyphiconPause :: ClassName
glyphiconPause = ClassName "glyphicon-pause"

glyphiconPawn :: ClassName
glyphiconPawn = ClassName "glyphicon-pawn"

glyphiconPencil :: ClassName
glyphiconPencil = ClassName "glyphicon-pencil"

glyphiconPhone :: ClassName
glyphiconPhone = ClassName "glyphicon-phone"

glyphiconPhoneAlt :: ClassName
glyphiconPhoneAlt = ClassName "glyphicon-phone-alt"

glyphiconPicture :: ClassName
glyphiconPicture = ClassName "glyphicon-picture"

glyphiconPiggyBank :: ClassName
glyphiconPiggyBank = ClassName "glyphicon-piggy-bank"

glyphiconPlane :: ClassName
glyphiconPlane = ClassName "glyphicon-plane"

glyphiconPlay :: ClassName
glyphiconPlay = ClassName "glyphicon-play"

glyphiconPlayCircle :: ClassName
glyphiconPlayCircle = ClassName "glyphicon-play-circle"

glyphiconPlus :: ClassName
glyphiconPlus = ClassName "glyphicon-plus"

glyphiconPlusSign :: ClassName
glyphiconPlusSign = ClassName "glyphicon-plus-sign"

glyphiconPrint :: ClassName
glyphiconPrint = ClassName "glyphicon-print"

glyphiconPushpin :: ClassName
glyphiconPushpin = ClassName "glyphicon-pushpin"

glyphiconQrcode :: ClassName
glyphiconQrcode = ClassName "glyphicon-qrcode"

glyphiconQueen :: ClassName
glyphiconQueen = ClassName "glyphicon-queen"

glyphiconQuestionSign :: ClassName
glyphiconQuestionSign = ClassName "glyphicon-question-sign"

glyphiconRandom :: ClassName
glyphiconRandom = ClassName "glyphicon-random"

glyphiconRecord :: ClassName
glyphiconRecord = ClassName "glyphicon-record"

glyphiconRefresh :: ClassName
glyphiconRefresh = ClassName "glyphicon-refresh"

glyphiconRegistrationMark :: ClassName
glyphiconRegistrationMark = ClassName "glyphicon-registration-mark"

glyphiconRemove :: ClassName
glyphiconRemove = ClassName "glyphicon-remove"

glyphiconRemoveCircle :: ClassName
glyphiconRemoveCircle = ClassName "glyphicon-remove-circle"

glyphiconRemoveSign :: ClassName
glyphiconRemoveSign = ClassName "glyphicon-remove-sign"

glyphiconRepeat :: ClassName
glyphiconRepeat = ClassName "glyphicon-repeat"

glyphiconResizeFull :: ClassName
glyphiconResizeFull = ClassName "glyphicon-resize-full"

glyphiconResizeHorizontal :: ClassName
glyphiconResizeHorizontal = ClassName "glyphicon-resize-horizontal"

glyphiconResizeSmall :: ClassName
glyphiconResizeSmall = ClassName "glyphicon-resize-small"

glyphiconResizeVertical :: ClassName
glyphiconResizeVertical = ClassName "glyphicon-resize-vertical"

glyphiconRetweet :: ClassName
glyphiconRetweet = ClassName "glyphicon-retweet"

glyphiconRoad :: ClassName
glyphiconRoad = ClassName "glyphicon-road"

glyphiconRuble :: ClassName
glyphiconRuble = ClassName "glyphicon-ruble"

glyphiconSave :: ClassName
glyphiconSave = ClassName "glyphicon-save"

glyphiconSaveFile :: ClassName
glyphiconSaveFile = ClassName "glyphicon-save-file"

glyphiconSaved :: ClassName
glyphiconSaved = ClassName "glyphicon-saved"

glyphiconScale :: ClassName
glyphiconScale = ClassName "glyphicon-scale"

glyphiconScissors :: ClassName
glyphiconScissors = ClassName "glyphicon-scissors"

glyphiconScreenshot :: ClassName
glyphiconScreenshot = ClassName "glyphicon-screenshot"

glyphiconSdVideo :: ClassName
glyphiconSdVideo = ClassName "glyphicon-sd-video"

glyphiconSearch :: ClassName
glyphiconSearch = ClassName "glyphicon-search"

glyphiconSend :: ClassName
glyphiconSend = ClassName "glyphicon-send"

glyphiconShare :: ClassName
glyphiconShare = ClassName "glyphicon-share"

glyphiconShareAlt :: ClassName
glyphiconShareAlt = ClassName "glyphicon-share-alt"

glyphiconShoppingCart :: ClassName
glyphiconShoppingCart = ClassName "glyphicon-shopping-cart"

glyphiconSignal :: ClassName
glyphiconSignal = ClassName "glyphicon-signal"

glyphiconSort :: ClassName
glyphiconSort = ClassName "glyphicon-sort"

glyphiconSortByAlphabet :: ClassName
glyphiconSortByAlphabet = ClassName "glyphicon-sort-by-alphabet"

glyphiconSortByAlphabetAlt :: ClassName
glyphiconSortByAlphabetAlt = ClassName "glyphicon-sort-by-alphabet-alt"

glyphiconSortByAttributes :: ClassName
glyphiconSortByAttributes = ClassName "glyphicon-sort-by-attributes"

glyphiconSortByAttributesAlt :: ClassName
glyphiconSortByAttributesAlt = ClassName "glyphicon-sort-by-attributes-alt"

glyphiconSortByOrder :: ClassName
glyphiconSortByOrder = ClassName "glyphicon-sort-by-order"

glyphiconSortByOrderAlt :: ClassName
glyphiconSortByOrderAlt = ClassName "glyphicon-sort-by-order-alt"

glyphiconSound5_1 :: ClassName
glyphiconSound5_1 = ClassName "glyphicon-sound-5-1"

glyphiconSound6_1 :: ClassName
glyphiconSound6_1 = ClassName "glyphicon-sound-6-1"

glyphiconSound7_1 :: ClassName
glyphiconSound7_1 = ClassName "glyphicon-sound-7-1"

glyphiconSoundDolby :: ClassName
glyphiconSoundDolby = ClassName "glyphicon-sound-dolby"

glyphiconSoundStereo :: ClassName
glyphiconSoundStereo = ClassName "glyphicon-sound-stereo"

glyphiconStar :: ClassName
glyphiconStar = ClassName "glyphicon-star"

glyphiconStarEmpty :: ClassName
glyphiconStarEmpty = ClassName "glyphicon-star-empty"

glyphiconStats :: ClassName
glyphiconStats = ClassName "glyphicon-stats"

glyphiconStepBackward :: ClassName
glyphiconStepBackward = ClassName "glyphicon-step-backward"

glyphiconStepForward :: ClassName
glyphiconStepForward = ClassName "glyphicon-step-forward"

glyphiconStop :: ClassName
glyphiconStop = ClassName "glyphicon-stop"

glyphiconSubscript :: ClassName
glyphiconSubscript = ClassName "glyphicon-subscript"

glyphiconSubtitles :: ClassName
glyphiconSubtitles = ClassName "glyphicon-subtitles"

glyphiconSunglasses :: ClassName
glyphiconSunglasses = ClassName "glyphicon-sunglasses"

glyphiconSuperscript :: ClassName
glyphiconSuperscript = ClassName "glyphicon-superscript"

glyphiconTag :: ClassName
glyphiconTag = ClassName "glyphicon-tag"

glyphiconTags :: ClassName
glyphiconTags = ClassName "glyphicon-tags"

glyphiconTasks :: ClassName
glyphiconTasks = ClassName "glyphicon-tasks"

glyphiconTent :: ClassName
glyphiconTent = ClassName "glyphicon-tent"

glyphiconTextBackground :: ClassName
glyphiconTextBackground = ClassName "glyphicon-text-background"

glyphiconTextColor :: ClassName
glyphiconTextColor = ClassName "glyphicon-text-color"

glyphiconTextHeight :: ClassName
glyphiconTextHeight = ClassName "glyphicon-text-height"

glyphiconTextSize :: ClassName
glyphiconTextSize = ClassName "glyphicon-text-size"

glyphiconTextWidth :: ClassName
glyphiconTextWidth = ClassName "glyphicon-text-width"

glyphiconTh :: ClassName
glyphiconTh = ClassName "glyphicon-th"

glyphiconThLarge :: ClassName
glyphiconThLarge = ClassName "glyphicon-th-large"

glyphiconThList :: ClassName
glyphiconThList = ClassName "glyphicon-th-list"

glyphiconThumbsDown :: ClassName
glyphiconThumbsDown = ClassName "glyphicon-thumbs-down"

glyphiconThumbsUp :: ClassName
glyphiconThumbsUp = ClassName "glyphicon-thumbs-up"

glyphiconTime :: ClassName
glyphiconTime = ClassName "glyphicon-time"

glyphiconTint :: ClassName
glyphiconTint = ClassName "glyphicon-tint"

glyphiconTower :: ClassName
glyphiconTower = ClassName "glyphicon-tower"

glyphiconTransfer :: ClassName
glyphiconTransfer = ClassName "glyphicon-transfer"

glyphiconTrash :: ClassName
glyphiconTrash = ClassName "glyphicon-trash"

glyphiconTreeConifer :: ClassName
glyphiconTreeConifer = ClassName "glyphicon-tree-conifer"

glyphiconTreeDeciduous :: ClassName
glyphiconTreeDeciduous = ClassName "glyphicon-tree-deciduous"

glyphiconTriangleBottom :: ClassName
glyphiconTriangleBottom = ClassName "glyphicon-triangle-bottom"

glyphiconTriangleLeft :: ClassName
glyphiconTriangleLeft = ClassName "glyphicon-triangle-left"

glyphiconTriangleRight :: ClassName
glyphiconTriangleRight = ClassName "glyphicon-triangle-right"

glyphiconTriangleTop :: ClassName
glyphiconTriangleTop = ClassName "glyphicon-triangle-top"

glyphiconUnchecked :: ClassName
glyphiconUnchecked = ClassName "glyphicon-unchecked"

glyphiconUpload :: ClassName
glyphiconUpload = ClassName "glyphicon-upload"

glyphiconUsd :: ClassName
glyphiconUsd = ClassName "glyphicon-usd"

glyphiconUser :: ClassName
glyphiconUser = ClassName "glyphicon-user"

glyphiconVolumeDown :: ClassName
glyphiconVolumeDown = ClassName "glyphicon-volume-down"

glyphiconVolumeOff :: ClassName
glyphiconVolumeOff = ClassName "glyphicon-volume-off"

glyphiconVolumeUp :: ClassName
glyphiconVolumeUp = ClassName "glyphicon-volume-up"

glyphiconWarningSign :: ClassName
glyphiconWarningSign = ClassName "glyphicon-warning-sign"

glyphiconWrench :: ClassName
glyphiconWrench = ClassName "glyphicon-wrench"

glyphiconYen :: ClassName
glyphiconYen = ClassName "glyphicon-yen"

glyphiconZoomIn :: ClassName
glyphiconZoomIn = ClassName "glyphicon-zoom-in"

glyphiconZoomOut :: ClassName
glyphiconZoomOut = ClassName "glyphicon-zoom-out"

gradient :: ClassName
gradient = ClassName "gradient"

h1 :: ClassName
h1 = ClassName "h1"

h2 :: ClassName
h2 = ClassName "h2"

h3 :: ClassName
h3 = ClassName "h3"

h4 :: ClassName
h4 = ClassName "h4"

h5 :: ClassName
h5 = ClassName "h5"

h6 :: ClassName
h6 = ClassName "h6"

hasError :: ClassName
hasError = ClassName "has-error"

hasFeedback :: ClassName
hasFeedback = ClassName "has-feedback"

hasSuccess :: ClassName
hasSuccess = ClassName "has-success"

hasWarning :: ClassName
hasWarning = ClassName "has-warning"

helpBlock :: ClassName
helpBlock = ClassName "help-block"

hidden :: ClassName
hidden = ClassName "hidden"

hiddenLg :: ClassName
hiddenLg = ClassName "hidden-lg"

hiddenMd :: ClassName
hiddenMd = ClassName "hidden-md"

hiddenPrint :: ClassName
hiddenPrint = ClassName "hidden-print"

hiddenSm :: ClassName
hiddenSm = ClassName "hidden-sm"

hiddenXs :: ClassName
hiddenXs = ClassName "hidden-xs"

hide :: ClassName
hide = ClassName "hide"

iconBar :: ClassName
iconBar = ClassName "icon-bar"

iconNext :: ClassName
iconNext = ClassName "icon-next"

iconPrev :: ClassName
iconPrev = ClassName "icon-prev"

imgCircle :: ClassName
imgCircle = ClassName "img-circle"

imgResponsive :: ClassName
imgResponsive = ClassName "img-responsive"

imgRounded :: ClassName
imgRounded = ClassName "img-rounded"

imgThumbnail :: ClassName
imgThumbnail = ClassName "img-thumbnail"

in_ :: ClassName
in_ = ClassName "in"

info :: ClassName
info = ClassName "info"

initialism :: ClassName
initialism = ClassName "initialism"

inputGroup :: ClassName
inputGroup = ClassName "input-group"

inputGroupAddon :: ClassName
inputGroupAddon = ClassName "input-group-addon"

inputGroupBtn :: ClassName
inputGroupBtn = ClassName "input-group-btn"

inputGroupLg :: ClassName
inputGroupLg = ClassName "input-group-lg"

inputGroupSm :: ClassName
inputGroupSm = ClassName "input-group-sm"

inputLg :: ClassName
inputLg = ClassName "input-lg"

inputSm :: ClassName
inputSm = ClassName "input-sm"

invisible :: ClassName
invisible = ClassName "invisible"

item :: ClassName
item = ClassName "item"

jumbotron :: ClassName
jumbotron = ClassName "jumbotron"

label :: ClassName
label = ClassName "label"

labelDanger :: ClassName
labelDanger = ClassName "label-danger"

labelDefault :: ClassName
labelDefault = ClassName "label-default"

labelInfo :: ClassName
labelInfo = ClassName "label-info"

labelPrimary :: ClassName
labelPrimary = ClassName "label-primary"

labelSuccess :: ClassName
labelSuccess = ClassName "label-success"

labelWarning :: ClassName
labelWarning = ClassName "label-warning"

lead :: ClassName
lead = ClassName "lead"

left :: ClassName
left = ClassName "left"

listGroup :: ClassName
listGroup = ClassName "list-group"

listGroupItem :: ClassName
listGroupItem = ClassName "list-group-item"

listGroupItemDanger :: ClassName
listGroupItemDanger = ClassName "list-group-item-danger"

listGroupItemHeading :: ClassName
listGroupItemHeading = ClassName "list-group-item-heading"

listGroupItemInfo :: ClassName
listGroupItemInfo = ClassName "list-group-item-info"

listGroupItemSuccess :: ClassName
listGroupItemSuccess = ClassName "list-group-item-success"

listGroupItemText :: ClassName
listGroupItemText = ClassName "list-group-item-text"

listGroupItemWarning :: ClassName
listGroupItemWarning = ClassName "list-group-item-warning"

listInline :: ClassName
listInline = ClassName "list-inline"

listUnstyled :: ClassName
listUnstyled = ClassName "list-unstyled"

mark :: ClassName
mark = ClassName "mark"

media :: ClassName
media = ClassName "media"

mediaBody :: ClassName
mediaBody = ClassName "media-body"

mediaBottom :: ClassName
mediaBottom = ClassName "media-bottom"

mediaHeading :: ClassName
mediaHeading = ClassName "media-heading"

mediaLeft :: ClassName
mediaLeft = ClassName "media-left"

mediaList :: ClassName
mediaList = ClassName "media-list"

mediaMiddle :: ClassName
mediaMiddle = ClassName "media-middle"

mediaObject :: ClassName
mediaObject = ClassName "media-object"

mediaRight :: ClassName
mediaRight = ClassName "media-right"

modal :: ClassName
modal = ClassName "modal"

modalBackdrop :: ClassName
modalBackdrop = ClassName "modal-backdrop"

modalBody :: ClassName
modalBody = ClassName "modal-body"

modalContent :: ClassName
modalContent = ClassName "modal-content"

modalDialog :: ClassName
modalDialog = ClassName "modal-dialog"

modalFooter :: ClassName
modalFooter = ClassName "modal-footer"

modalHeader :: ClassName
modalHeader = ClassName "modal-header"

modalLg :: ClassName
modalLg = ClassName "modal-lg"

modalOpen :: ClassName
modalOpen = ClassName "modal-open"

modalScrollbarMeasure :: ClassName
modalScrollbarMeasure = ClassName "modal-scrollbar-measure"

modalSm :: ClassName
modalSm = ClassName "modal-sm"

modalTitle :: ClassName
modalTitle = ClassName "modal-title"

nav :: ClassName
nav = ClassName "nav"

navDivider :: ClassName
navDivider = ClassName "nav-divider"

navJustified :: ClassName
navJustified = ClassName "nav-justified"

navPills :: ClassName
navPills = ClassName "nav-pills"

navStacked :: ClassName
navStacked = ClassName "nav-stacked"

navTabs :: ClassName
navTabs = ClassName "nav-tabs"

navTabsJustified :: ClassName
navTabsJustified = ClassName "nav-tabs-justified"

navbar :: ClassName
navbar = ClassName "navbar"

navbarBrand :: ClassName
navbarBrand = ClassName "navbar-brand"

navbarBtn :: ClassName
navbarBtn = ClassName "navbar-btn"

navbarCollapse :: ClassName
navbarCollapse = ClassName "navbar-collapse"

navbarDefault :: ClassName
navbarDefault = ClassName "navbar-default"

navbarFixedBottom :: ClassName
navbarFixedBottom = ClassName "navbar-fixed-bottom"

navbarFixedTop :: ClassName
navbarFixedTop = ClassName "navbar-fixed-top"

navbarForm :: ClassName
navbarForm = ClassName "navbar-form"

navbarHeader :: ClassName
navbarHeader = ClassName "navbar-header"

navbarInverse :: ClassName
navbarInverse = ClassName "navbar-inverse"

navbarLeft :: ClassName
navbarLeft = ClassName "navbar-left"

navbarLink :: ClassName
navbarLink = ClassName "navbar-link"

navbarNav :: ClassName
navbarNav = ClassName "navbar-nav"

navbarRight :: ClassName
navbarRight = ClassName "navbar-right"

navbarStaticTop :: ClassName
navbarStaticTop = ClassName "navbar-static-top"

navbarText :: ClassName
navbarText = ClassName "navbar-text"

navbarToggle :: ClassName
navbarToggle = ClassName "navbar-toggle"

next :: ClassName
next = ClassName "next"

open :: ClassName
open = ClassName "open"

pageHeader :: ClassName
pageHeader = ClassName "page-header"

pager :: ClassName
pager = ClassName "pager"

pagination :: ClassName
pagination = ClassName "pagination"

paginationLg :: ClassName
paginationLg = ClassName "pagination-lg"

paginationSm :: ClassName
paginationSm = ClassName "pagination-sm"

panel :: ClassName
panel = ClassName "panel"

panelBody :: ClassName
panelBody = ClassName "panel-body"

panelCollapse :: ClassName
panelCollapse = ClassName "panel-collapse"

panelDanger :: ClassName
panelDanger = ClassName "panel-danger"

panelDefault :: ClassName
panelDefault = ClassName "panel-default"

panelFooter :: ClassName
panelFooter = ClassName "panel-footer"

panelGroup :: ClassName
panelGroup = ClassName "panel-group"

panelHeading :: ClassName
panelHeading = ClassName "panel-heading"

panelInfo :: ClassName
panelInfo = ClassName "panel-info"

panelPrimary :: ClassName
panelPrimary = ClassName "panel-primary"

panelSuccess :: ClassName
panelSuccess = ClassName "panel-success"

panelTitle :: ClassName
panelTitle = ClassName "panel-title"

panelWarning :: ClassName
panelWarning = ClassName "panel-warning"

popover :: ClassName
popover = ClassName "popover"

popoverContent :: ClassName
popoverContent = ClassName "popover-content"

popoverTitle :: ClassName
popoverTitle = ClassName "popover-title"

preScrollable :: ClassName
preScrollable = ClassName "pre-scrollable"

prev :: ClassName
prev = ClassName "prev"

previous :: ClassName
previous = ClassName "previous"

progress :: ClassName
progress = ClassName "progress"

progressBar :: ClassName
progressBar = ClassName "progress-bar"

progressBarDanger :: ClassName
progressBarDanger = ClassName "progress-bar-danger"

progressBarInfo :: ClassName
progressBarInfo = ClassName "progress-bar-info"

progressBarStriped :: ClassName
progressBarStriped = ClassName "progress-bar-striped"

progressBarSuccess :: ClassName
progressBarSuccess = ClassName "progress-bar-success"

progressBarWarning :: ClassName
progressBarWarning = ClassName "progress-bar-warning"

progressStriped :: ClassName
progressStriped = ClassName "progress-striped"

pullLeft :: ClassName
pullLeft = ClassName "pull-left"

pullRight :: ClassName
pullRight = ClassName "pull-right"

radio :: ClassName
radio = ClassName "radio"

radioInline :: ClassName
radioInline = ClassName "radio-inline"

right :: ClassName
right = ClassName "right"

row :: ClassName
row = ClassName "row"

show_ :: ClassName
show_ = ClassName "show"

small :: ClassName
small = ClassName "small"

srOnly :: ClassName
srOnly = ClassName "sr-only"

srOnlyFocusable :: ClassName
srOnlyFocusable = ClassName "sr-only-focusable"

success :: ClassName
success = ClassName "success"

svg :: ClassName
svg = ClassName "svg"

tabContent :: ClassName
tabContent = ClassName "tab-content"

tabPane :: ClassName
tabPane = ClassName "tab-pane"

table :: ClassName
table = ClassName "table"

tableBordered :: ClassName
tableBordered = ClassName "table-bordered"

tableCondensed :: ClassName
tableCondensed = ClassName "table-condensed"

tableHover :: ClassName
tableHover = ClassName "table-hover"

tableResponsive :: ClassName
tableResponsive = ClassName "table-responsive"

tableStriped :: ClassName
tableStriped = ClassName "table-striped"

textCapitalize :: ClassName
textCapitalize = ClassName "text-capitalize"

textCenter :: ClassName
textCenter = ClassName "text-center"

textDanger :: ClassName
textDanger = ClassName "text-danger"

textHide :: ClassName
textHide = ClassName "text-hide"

textInfo :: ClassName
textInfo = ClassName "text-info"

textJustify :: ClassName
textJustify = ClassName "text-justify"

textLeft :: ClassName
textLeft = ClassName "text-left"

textLowercase :: ClassName
textLowercase = ClassName "text-lowercase"

textMuted :: ClassName
textMuted = ClassName "text-muted"

textNowrap :: ClassName
textNowrap = ClassName "text-nowrap"

textPrimary :: ClassName
textPrimary = ClassName "text-primary"

textRight :: ClassName
textRight = ClassName "text-right"

textSuccess :: ClassName
textSuccess = ClassName "text-success"

textUppercase :: ClassName
textUppercase = ClassName "text-uppercase"

textWarning :: ClassName
textWarning = ClassName "text-warning"

thumbnail :: ClassName
thumbnail = ClassName "thumbnail"

tooltip :: ClassName
tooltip = ClassName "tooltip"

tooltipArrow :: ClassName
tooltipArrow = ClassName "tooltip-arrow"

tooltipInner :: ClassName
tooltipInner = ClassName "tooltip-inner"

top :: ClassName
top = ClassName "top"

topLeft :: ClassName
topLeft = ClassName "top-left"

topRight :: ClassName
topRight = ClassName "top-right"

ttf :: ClassName
ttf = ClassName "ttf"

visibleLg :: ClassName
visibleLg = ClassName "visible-lg"

visibleLgBlock :: ClassName
visibleLgBlock = ClassName "visible-lg-block"

visibleLgInline :: ClassName
visibleLgInline = ClassName "visible-lg-inline"

visibleLgInlineBlock :: ClassName
visibleLgInlineBlock = ClassName "visible-lg-inline-block"

visibleMd :: ClassName
visibleMd = ClassName "visible-md"

visibleMdBlock :: ClassName
visibleMdBlock = ClassName "visible-md-block"

visibleMdInline :: ClassName
visibleMdInline = ClassName "visible-md-inline"

visibleMdInlineBlock :: ClassName
visibleMdInlineBlock = ClassName "visible-md-inline-block"

visiblePrint :: ClassName
visiblePrint = ClassName "visible-print"

visiblePrintBlock :: ClassName
visiblePrintBlock = ClassName "visible-print-block"

visiblePrintInline :: ClassName
visiblePrintInline = ClassName "visible-print-inline"

visiblePrintInlineBlock :: ClassName
visiblePrintInlineBlock = ClassName "visible-print-inline-block"

visibleSm :: ClassName
visibleSm = ClassName "visible-sm"

visibleSmBlock :: ClassName
visibleSmBlock = ClassName "visible-sm-block"

visibleSmInline :: ClassName
visibleSmInline = ClassName "visible-sm-inline"

visibleSmInlineBlock :: ClassName
visibleSmInlineBlock = ClassName "visible-sm-inline-block"

visibleXs :: ClassName
visibleXs = ClassName "visible-xs"

visibleXsBlock :: ClassName
visibleXsBlock = ClassName "visible-xs-block"

visibleXsInline :: ClassName
visibleXsInline = ClassName "visible-xs-inline"

visibleXsInlineBlock :: ClassName
visibleXsInlineBlock = ClassName "visible-xs-inline-block"

warning :: ClassName
warning = ClassName "warning"

well :: ClassName
well = ClassName "well"

wellLg :: ClassName
wellLg = ClassName "well-lg"

wellSm :: ClassName
wellSm = ClassName "well-sm"

woff :: ClassName
woff = ClassName "woff"

woff2 :: ClassName
woff2 = ClassName "woff2"
