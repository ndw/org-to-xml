<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:o="https://nwalsh.com/ns/org-to-xml"
                xmlns="http://www.w3.org/1999/xhtml"
                exclude-result-prefixes="o xs"
                version="2.0">

  <!-- This is a fairly crude first attempt. It is by no means complete or
       exhaustive yet. -->

  <xsl:output method="html" version="5" encoding="utf-8" indent="yes"
              omit-xml-declaration="yes"/>

  <xsl:strip-space elements="*"/>
  <xsl:preserve-space elements="o:paragraph"/>

  <xsl:template match="o:org-data">
    <html>
      <head>
        <title>
          <xsl:value-of select="o:section[1]/o:keyword[@key='TITLE'][1]"/>
        </title>
        <style>.error { color: red }</style>
      </head>
      <body>
        <main>
          <xsl:if test="o:section[1]/o:keyword[@key='TITLE'][1]">
            <h1>
              <xsl:value-of select="o:section[1]/o:keyword[@key='TITLE'][1]"/>
            </h1>
          </xsl:if>
          <xsl:apply-templates/>
        </main>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="o:org-data/o:section">
    <section>
      <xsl:apply-templates select="o:property-drawer/o:node-property[@key='CUSTOM_ID']"/>
      <xsl:apply-templates/>
    </section>
  </xsl:template>

  <xsl:template match="o:headline">
    <section>
      <xsl:apply-templates select="@CUSTOM_ID"/>
      <xsl:apply-templates/>
    </section>
  </xsl:template>

  <xsl:template match="o:headline/o:title">
    <xsl:element name="h{../@level}">
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="o:headline/o:section">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="o:property-drawer"/>

  <xsl:template match="o:keyword"/>

  <xsl:template match="o:plain-list[@type='unordered']">
    <ul>
      <xsl:apply-templates/>
    </ul>
  </xsl:template>

  <xsl:template match="o:plain-list[@type='ordered']">
    <ol>
      <xsl:apply-templates/>
    </ol>
  </xsl:template>

  <xsl:template match="o:item">
    <li>
      <xsl:apply-templates/>
    </li>
  </xsl:template>

  <xsl:template match="o:paragraph">
    <p>
      <xsl:apply-templates/>
    </p>
  </xsl:template>

  <xsl:template match="o:bold">
    <b><xsl:apply-templates/></b>
  </xsl:template>

  <xsl:template match="o:italic">
    <i><xsl:apply-templates/></i>
  </xsl:template>

  <xsl:template match="o:code">
    <code>
      <xsl:apply-templates/>
    </code>
  </xsl:template>

  <xsl:template match="o:node-property[@key='CUSTOM_ID']">
    <xsl:attribute name="xml:id" select="@value"/>
  </xsl:template>

  <xsl:template match="@CUSTOM_ID">
    <xsl:attribute name="xml:id" select="."/>
  </xsl:template>

  <xsl:template match="o:link[@type='http' or @type='https']">
    <a href="{@raw-link}">
      <xsl:choose>
        <xsl:when test="@format='bracket'">
          <xsl:apply-templates/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@raw-link"/>
        </xsl:otherwise>
      </xsl:choose>
    </a>
  </xsl:template>

  <xsl:template match="o:src-block">
    <pre>
      <code>
        <xsl:apply-templates/>
      </code>
    </pre>
  </xsl:template>

  <xsl:template match="element()">
    <xsl:message>Unhandled: <xsl:value-of select="local-name(.)"/></xsl:message>

    <xsl:variable name="error" as="node()*">
      <xsl:text>&lt;</xsl:text>
      <xsl:value-of select="local-name(.)"/>
      <xsl:text>&gt;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&lt;/</xsl:text>
      <xsl:value-of select="local-name(.)"/>
      <xsl:text>&gt;</xsl:text>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="ancestor::o:paragraph">
        <span class="error"><xsl:sequence select="$error"/></span>
      </xsl:when>
      <xsl:otherwise>
        <div class="error"><xsl:sequence select="$error"/></div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="attribute()|text()|comment()|processing-instruction()">
    <xsl:copy/>
  </xsl:template>

</xsl:stylesheet>
