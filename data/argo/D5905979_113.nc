CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:20Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170920  20220204114422  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               qA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��\�=~�1   @��]`@6&�x����c(1&�x�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    qA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx��B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP�fDQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�RD�%qD�VfD���D���D�D�P�D���D�ϮD�*=D�`�D���D��D� �D�VfDڋ�D��
D��D�` D�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A��
A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�BxQ�B�B�B�B�B�B�B�B���B�B�\)B��\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#��C%��C'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCKǮCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc��Ce�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��qC��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO��DP~�DP��DQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm��DnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�Dy��D�!�D�R�D��
D���D�HD�L�D���D���D�&fD�]D���D��=D�D�R�Dڈ D��3D��D�\)D�D�Ӆ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�E�A�O�A�VA�S�A�Q�A�M�A�JA��A��mA��`A��HA��#AټjAٙ�Aٟ�A١�Aٛ�AٓuAًDAفA�v�A�n�A�bNA�^5A�^5A�\)A�/A�ƨA�p�A�/A�bA��Aֺ^A�|�A�ffA��A�(�A��/A�bA�ȴA�`BA�p�A�9XA��FA�A��A��A��A�7LA�I�A�$�A���A�l�A�I�A��#A���A��A���A��PA��!A���A���A�\)A�\)A�XA���A��A���A�VA��A�K�A�n�A�dZA�I�A�`BA�hsA� �A��PA�5?A�ffA��A�33A�t�A���A���A���A�-A��!A��`A�/A�ƨA�oA�ffA��wA�\)A���A��mA�z�A�"�A��A��yA��A�p�A���A��!A�I�A��A�A�%A���A��\A�/A�PA}
=AzȴAx�Aw�At��Ar^5Apz�An�RAm�
Ai��Ag�Ag%Af9XAd�uAb9XA`M�A]x�AZȴAX�RAV��AVJATVAO�ALffAIl�AF1AD�\ADABȴA@E�A>VA=33A<  A:^5A8ĜA7�A6�DA5|�A4(�A3�A1?}A0ffA/�A-�A,ȴA+�FA*-A(��A(I�A'�hA&Q�A%�A$(�A#oA"$�A �\A;dA�TAK�AjA\)A9XA�
AdZA�/A��A��A7LA�!A-AO�A�DA  AbNAC�A�A�uA�A�FA��A|�A�#Ax�A��A`BA�A�#A
�HA	l�A	XA	A	K�A	VAQ�A��A��Av�A�A��AhsA M�@�
=@�V@�t�@�^5@�/@���@��@�r�@��`@��@�M�@���@@�$�@�9@��
@�K�@���@�M�@�@��m@�!@��#@�z�@�M�@�/@�ƨ@ݙ�@��`@�1@١�@�`B@�I�@�33@Ցh@�Q�@���@�5?@�ff@�?}@Ϯ@�b@� �@θR@˶F@�-@�7L@�^5@���@�@�`B@�&�@ċD@�l�@�v�@�Ĝ@��P@��@�^5@���@��@�A�@�dZ@���@�@��9@�1@���@�p�@��@�b@�\)@�\)@�33@���@�"�@��y@���@�~�@�E�@�@���@���@���@�&�@�ƨ@��/@�V@��D@��@��\@�p�@�`B@��@�(�@�  @�S�@��@��F@�|�@�C�@��R@�=q@���@��`@�1@�1@���@���@�O�@�?}@��@��u@��w@���@�=q@��@�V@��D@��@��F@�t�@�33@�
=@�ȴ@�n�@�M�@�@�`B@���@�I�@��P@��@���@��@���@��R@���@���@���@�n�@��@�@��@��^@���@�V@��u@�bN@� �@�(�@�Q�@�j@�A�@�(�@���@�ƨ@��w@���@���@�|�@�K�@��@��@���@���@�v�@�V@��@��@���@���@�@��-@���@�x�@��`@���@���@���@�z�@�bN@�Q�@�Q�@��@��@�|�@�33@��@���@�E�@��^@�p�@���@�j@���@��D@�r�@�(�@�ƨ@��@��w@���@��@�|�@�dZ@�C�@�K�@�K�@��@��H@���@���@�~�@�5?@���@��^@���@��@�O�@��@�V@��/@��j@���@��D@�r�@�I�@��@���@��;@��F@���@��@�\)@�"�@�o@��@��H@��@���@���@�=q@��@���@���@��@�`B@�/@���@���@�9X@�b@��
@�l�@�K�@�"�@��H@��R@���@�E�@�=q@�E�@�$�@���@�x�@�7L@��@���@���@��@�j@�z�@�z�@�z�@~n�@u�@o�@i�@d*�@^�@X�/@PU2@J��@B.�@:��@3��@-��@(�@!�@tT@�P@?}@��@��@C�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�E�A�O�A�VA�S�A�Q�A�M�A�JA��A��mA��`A��HA��#AټjAٙ�Aٟ�A١�Aٛ�AٓuAًDAفA�v�A�n�A�bNA�^5A�^5A�\)A�/A�ƨA�p�A�/A�bA��Aֺ^A�|�A�ffA��A�(�A��/A�bA�ȴA�`BA�p�A�9XA��FA�A��A��A��A�7LA�I�A�$�A���A�l�A�I�A��#A���A��A���A��PA��!A���A���A�\)A�\)A�XA���A��A���A�VA��A�K�A�n�A�dZA�I�A�`BA�hsA� �A��PA�5?A�ffA��A�33A�t�A���A���A���A�-A��!A��`A�/A�ƨA�oA�ffA��wA�\)A���A��mA�z�A�"�A��A��yA��A�p�A���A��!A�I�A��A�A�%A���A��\A�/A�PA}
=AzȴAx�Aw�At��Ar^5Apz�An�RAm�
Ai��Ag�Ag%Af9XAd�uAb9XA`M�A]x�AZȴAX�RAV��AVJATVAO�ALffAIl�AF1AD�\ADABȴA@E�A>VA=33A<  A:^5A8ĜA7�A6�DA5|�A4(�A3�A1?}A0ffA/�A-�A,ȴA+�FA*-A(��A(I�A'�hA&Q�A%�A$(�A#oA"$�A �\A;dA�TAK�AjA\)A9XA�
AdZA�/A��A��A7LA�!A-AO�A�DA  AbNAC�A�A�uA�A�FA��A|�A�#Ax�A��A`BA�A�#A
�HA	l�A	XA	A	K�A	VAQ�A��A��Av�A�A��AhsA M�@�
=@�V@�t�@�^5@�/@���@��@�r�@��`@��@�M�@���@@�$�@�9@��
@�K�@���@�M�@�@��m@�!@��#@�z�@�M�@�/@�ƨ@ݙ�@��`@�1@١�@�`B@�I�@�33@Ցh@�Q�@���@�5?@�ff@�?}@Ϯ@�b@� �@θR@˶F@�-@�7L@�^5@���@�@�`B@�&�@ċD@�l�@�v�@�Ĝ@��P@��@�^5@���@��@�A�@�dZ@���@�@��9@�1@���@�p�@��@�b@�\)@�\)@�33@���@�"�@��y@���@�~�@�E�@�@���@���@���@�&�@�ƨ@��/@�V@��D@��@��\@�p�@�`B@��@�(�@�  @�S�@��@��F@�|�@�C�@��R@�=q@���@��`@�1@�1@���@���@�O�@�?}@��@��u@��w@���@�=q@��@�V@��D@��@��F@�t�@�33@�
=@�ȴ@�n�@�M�@�@�`B@���@�I�@��P@��@���@��@���@��R@���@���@���@�n�@��@�@��@��^@���@�V@��u@�bN@� �@�(�@�Q�@�j@�A�@�(�@���@�ƨ@��w@���@���@�|�@�K�@��@��@���@���@�v�@�V@��@��@���@���@�@��-@���@�x�@��`@���@���@���@�z�@�bN@�Q�@�Q�@��@��@�|�@�33@��@���@�E�@��^@�p�@���@�j@���@��D@�r�@�(�@�ƨ@��@��w@���@��@�|�@�dZ@�C�@�K�@�K�@��@��H@���@���@�~�@�5?@���@��^@���@��@�O�@��@�V@��/@��j@���@��D@�r�@�I�@��@���@��;@��F@���@��@�\)@�"�@�o@��@��H@��@���@���@�=q@��@���@���@��@�`B@�/@���@���@�9X@�b@��
@�l�@�K�@�"�@��H@��R@���@�E�@�=q@�E�@�$�@���@�x�@�7L@��@���@���@��@�j@�z�@�z�G�O�@~n�@u�@o�@i�@d*�@^�@X�/@PU2@J��@B.�@:��@3��@-��@(�@!�@tT@�P@?}@��@��@C�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
`BB
_;B
`BB
`BB
`BB
`BB
bNB
cTB
dZB
dZB
e`B
gmB
cTB
`BB
dZB
ffB
gmB
ffB
ffB
ffB
ffB
e`B
dZB
dZB
e`B
e`B
`BB
S�B
N�B
VB
aHB
m�B
q�B
{�B
�DB
�=B
�}B
��B
��B
��B<jBp�B+B
=B+BdZBw�B�7B��B�RB�9BÖB��B��B�/B�BB�B�B)�B:^B �B��B
=B1B��B
=B&�B�B\B\B>wB+B
=BB�B,B<jBD�BE�B@�B>wB?}B:^B2-B,BuB�/BƨB�jB�XB�XB�9B�'B�3B�RB�9B��B�hBp�BR�B/B{B
=B
��B
�B
��B
ȴB
�dB
ÖB
�B
�+B
m�B
VB
B�B
0!B
�B
\B	��B	�B	�#B	��B	ĜB	�-B	��B	��B	�uB	�JB	~�B	r�B	_;B	L�B	A�B	49B	0!B	)�B	%B�B�fB��BĜB��B�}B�9B�B��B�B��B��B��B��B��B��B��B�oB�\B�PB�1B�%B�B�B~�B{�By�Bv�Bu�Br�Bm�Bo�Bm�BdZB`BB[#BZBW
BVBVBYB]/B[#B]/B^5B^5BbNB_;BffBgmBaHB\)BcTBdZB_;B_;BffBo�Bz�B~�B~�Bw�Bu�Bq�BiyBe`Bm�BjBx�Bz�By�Bq�BgmBcTB_;BR�BR�BK�BJ�BG�BE�BJ�BI�BL�BVBQ�BL�BF�BJ�BH�BA�BA�BA�B?}B>wBD�BI�BG�BI�BH�BK�BM�BI�BG�BE�BB�BB�BE�BA�BH�BH�BN�BK�BJ�BG�BJ�BN�BL�BJ�BP�BZBT�BL�BH�BG�BB�BI�BM�BO�BP�BR�BP�BP�BR�BP�BP�BQ�BR�B_;Be`BhsBgmBl�Bo�Bq�Bs�Bu�Bw�Bz�B}�B� B�B�DB�PB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�9B�^B�jB�dB�^B�RB�^B�wBBƨB��B��B��B��B�#B�)B�/B�;B�NB�TB�mB�B�B�B��B��B��B��B��B��B	B	B		7B	PB	PB	JB	VB	hB	uB	uB	�B	�B	�B	�B	�B	 �B	!�B	#�B	)�B	/B	2-B	7LB	8RB	9XB	;dB	>wB	A�B	E�B	G�B	J�B	L�B	N�B	Q�B	R�B	S�B	T�B	XB	]/B	aHB	cTB	e`B	iyB	m�B	n�B	n�B	p�B	r�B	t�B	v�B	z�B	}�B	� B	�B	�B	�B	�%B	�+B	�=B	�DB	�VB	�hB	�hB	�oB	�oB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�9B	�LB	�XB	�^B	�^B	�^B	�jB	�wB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�ZB	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
AB
}B
IB
!�B
)B
1�B
8�B
=�B
CaB
H1B
O(B
UMB
Y�B
^�B
b�B
e�B
iB
m�B
t9B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
UcB
T\B
UcB
UcB
UcB
UcB
WoB
XuB
Y{B
Y{B
Z�B
\�B
XuB
UcB
Y{B
[�B
\�B
[�B
[�B
[�B
[�B
Z�B
Y{B
Y{B
Z�B
Z�B
UcB
IB
C�B
K&B
ViB
b�B
f�B
qB
�cB
]B
��B
��B
�B
��B1|Be�B B
�UB BYkBl�B~EB��B�]B�DB��B��B�B�8B�JB�B�B�B/`B�B��B�DB�8B��B�DB�B
�BdBdB3{B B�FB�)B�B!B1oB9�B:�B5�B3}B4�B/dB'4B!BB�>B��B�|B�kB�kB�LB�;B�GB�eB�MB�B�Be�BHB$;B	�B
�aB
�B
�B
�B
��B
��B
��B
�AB
|\B
b�B
K9B
7�B
%ZB
�B
�B	�8B	��B	�dB	�
B	��B	�sB	�B	��B	��B	��B	tEB	g�B	T�B	BB	6�B	)�B	%tB	PB�|B�B��B�6B��B��B��B��B�|B�]B�cB�RB�4B�B�	B�B�B��B��B��B��B}�B{�Bz�BxyBtbBqOBoCBl2Bk,BhBb�BeBb�BY�BU�BP�BO�BLwBKqBKqBN�BR�BP�BR�BS�BS�BW�BT�B[�B\�BV�BQ�BX�BY�BT�BT�B[�BeBpMBtfBtfBm<Bk0BgB^�BZ�Bb�B_�BnBBpNBoHBgB\�BX�BT�BHcBHdBA9B@4B=!B;B@4B?-BB@BKvBG^BB@B<B@5B>(B6�B6�B6�B4�B3�B:B?.B=#B?/B>)BA<BCGB?/B=#B;B8B8B;B6�B>*B>*BDOBA=B@7B=$B@7BDOBBCB@7BF[BO�BJtBBDB>+B=%B8B?1BCJBEVBF\BHiBF\BF\BHiBF\BF\BGcBHiBT�BZ�B]�B\�Bb BeBgBi+Bk8BmDBpVBshButBy�B��B��B��B��B�$B�0B�0B�$B�B�$B�*B�*B�B�B�>B�B�+B�%B�8B�>B�JB�JB�hB��B��B��B��B��B��B��B��B� B�B�1B�CB�\B�mBВBјBҞBԪB׽B��B��B��B�B�B�)B�6B�<B�HB�TB�TB�rB��B��B	�B	�B	�B	�B	�B	�B	�B	
�B	�B	�B	B	"B	/B	5B	AB	fB	$�B	'�B	,�B	-�B	.�B	0�B	3�B	6�B	;	B	=B	@(B	B4B	D@B	GRB	HXB	I^B	JdB	MvB	R�B	V�B	X�B	Z�B	^�B	b�B	c�B	c�B	fB	hB	j B	l-B	pDB	sWB	ucB	woB	xuB	y|B	{�B	|�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�2B	�=B	�CB	�IB	�PB	�PB	�PB	�bB	�nB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�B	�+B	�1B	�6B	�<B	�BB	�IB	�OB	�[B	�aB	�gB	�gB	�mB	�mB	�zB	ЀB	ҌB	ӑB	ԗB	՞B	֤B	תB	תB	ٶB	ٶB	ڼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�G�O�B	�B	��B
�B
�B
$B
hB
'B
-�B
3KB
8�B
=�B
DB
J�B
O?B
TCB
W�B
[B
^fB
c6B
i�B
o0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.01(+/-0.004) in PSS-78.                                                                                                                                                                                        Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144222022020411442220220204114422  AO  ARCAADJP                                                                    20200619170920    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170920  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170920  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114422  IP                  G�O�G�O�G�O�                