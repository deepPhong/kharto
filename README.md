# kharto

[![forthebadge](https://forthebadge.com/images/badges/powered-by-water.svg)](https://forthebadge.com)
[![forthebadge](http://forthebadge.com/images/badges/built-with-love.svg)](http://forthebadge.com)
[![forthebadge](https://forthebadge.com/images/badges/uses-badges.svg)](https://forthebadge.com)

Lien vers l'application : https://dpnguyen.shinyapps.io/kharto/

`kharto` est une application qui permet de représenter visuellement le recrutement d'un établissement hospitalier par commune. Les données à charger sont mises à disposition sur l'application de cartographie de l'ATIH (suivre les instructions pour les récupérer). 
Pour l'instant, seule l'île-de-France est disponible.

 * Cartes interactives
 * Tableaux de données avec recherche textuelle
 * Génération automatique de rapports html interactifs

<p align="center">
    <img src="https://user-images.githubusercontent.com/20992061/59157430-8d3a6a80-8aaa-11e9-96b8-a091a5fd2e09.gif" width="800">
  </p>

## Instructions pour récupérer les données

  * Se rendre sur [l'onglet FOCUS de l'application de cartographie de l'ATIH](https://cartographie.atih.sante.fr/#pg=3;l=fr;v=map1) et choisir le type de séjour à étudier en cliquant sur la petite carte correspondante.
  Ici, nous nous prendrons l'exemple des *Séjours PMSI MCO au lieu de résidence*. Cliquer sur le bouton `Voir sur la carte`.
  
  <p align="center">
    <img src="https://user-images.githubusercontent.com/20992061/59157125-96750880-8aa5-11e9-9ce7-96e7f7c5e15f.jpg" width="800">
  </p>
  
  * Une fenêtre permettant la recherche de l'établissement d'intérêt apparaît. Cliquer sur l'option *Choisir un territoire* et dans le menu déroulant sélectionner la région où se trouve l'établissement (pour l'instant, seule l'Île-de-France est compatible avec `kharto`).
  
  <p align="center">
    <img src="https://user-images.githubusercontent.com/20992061/59157138-d0dea580-8aa5-11e9-8e66-94cf228d39b2.jpg" width="800">
  </p>
  
  * Sélectionner l'établissement dans le menu déroulant, ou dans l'onglet *Recherche par mot-clé*. Cliquer sur le bouton `Mettre à jour la carte`.
  
  <p align="center">
    <img src="https://user-images.githubusercontent.com/20992061/59156808-ae4a8d80-8aa1-11e9-8ce3-c97a1173c1df.jpg" width="800">
  </p>
  
  * Une fois la carte mise à jour, cliquer sur *Imprimer/Exporter*, puis sur le bouton `Exporter les données`.
  
  <p align="center">
    <img src="https://user-images.githubusercontent.com/20992061/59156809-ae4a8d80-8aa1-11e9-8c6b-a5dee04746b3.jpg" width="800">
  </p>
  
  * Enfin, importer le fichier obtenu dans `kharto`. La suite est plus intuitive :)
  
  ##
