CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:12:40Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7T   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    88   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8D   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8H   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8P   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8X   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8`   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8d   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8l   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9l   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  AD   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^x   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  h4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �<   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130103220  20190523124442  1514_5041_007                   2C  D   APEX                            2041                            062805                          846 @�Ի�O��1   @�Ի�O��@6�O�;dZ�c9O�;dZ1   GPS     Primary sampling: averaged [2dbar-bin averaged]                                                                                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @s33@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDw�fDxffDx�fDyff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA�Q�A�G�A�5?A�/A��A���A��TA��#A���A���A�ƨA̼jA̺^A̶FA̲-A̮Ḁ�A̝�A̕�Ȁ\A�p�A�=qA�jA���A�9XAüjA���A�A��#A��
A�l�A��mA��^A�;dA�=qA�z�A��TA�I�A��
A�1'A��A��A��-A���A�`BA��RA�VA�
=A�VA�%A���A��RA��!A���A�C�A���A��#A�K�A���A��DA��A��yA�n�A�(�A��A�1'A��uA��`A��-A�I�A�33A��wA�VA��A�ĜA��A��A��A�ƨA�G�A��A���A��A�"�A�x�A�ZA���A�A�A�A�dZA�G�A�I�A�$�A��A�33A�(�A���A���A��HA��A��A��yA��HA�x�A���A�7LA���A��7A��A���A���A�VA�ĜA�{A���A�\)A}��A{�AzffAyVAxZAw�FAw|�Aw&�Au��At�9As%Aq�Aq�Apz�Am��Ak"�Ai
=Ag33Af$�Ae/Ac�wAb�A` �A^�A]hsA[ƨA[%AY��AX�DAV�`ATQ�AR�`AQt�AP��AO�7AN��AM�AKƨAI33AG�AFz�AE�AD�uAB��AA�TAA�hAAt�AA`BAAC�AA+AAA@$�A=��A;�A;?}A:n�A9l�A8��A8v�A8�A7S�A6��A61A5/A4��A4ZA3C�A1O�A/�A.�A-�TA,�9A++A*�A)�A(��A'x�A&~�A%��A$��A#�wA"�jA"  A �uA {A|�A��A�#Ax�A�jAC�A�A��AK�AZA�A��AJAG�A�/A��A��A�RAdZA
ffA	�;A	"�A��A��A�A�+A��AG�AZAp�A ��@�"�@���@���@�~�@�-@��@�$�@�M�@���@�33@�X@@��@�1'@�
=@ꗍ@�V@�!@���@�X@��@�\@���@޸R@���@�G�@�z�@ۅ@��@ؓu@�C�@�v�@ԛ�@�\)@�n�@ёh@� �@��@�n�@�V@���@��@�-@ɉ7@Ȭ@�33@�=q@ļj@î@��^@� �@��@��@�Ĝ@�(�@���@�~�@�@�Ĝ@�ƨ@��R@�J@���@��@�ƨ@��R@�V@��7@�Z@��@�@���@��-@���@�r�@�ƨ@�\)@���@�ȴ@�n�@���@�x�@��/@�9X@��m@�t�@��y@�$�@��#@�p�@��@�7L@��@��/@�/@��@��R@��R@��T@��@�?}@��9@�ƨ@���@���@��!@�n�@��#@�G�@��@�ȴ@�@�?}@���@�Z@�1@� �@��/@��-@��T@�J@��-@�x�@�G�@��/@���@�r�@�Q�@�1'@�9X@�1'@��P@�dZ@�K�@�"�@��@��@�E�@�x�@��h@�O�@�j@�(�@��m@��P@�+@��T@�?}@���@�A�@�b@�1@��@�A�@��u@��`@���@�hs@�M�@��R@��!@��+@�E�@�?}@�%@���@�E�@�V@�E�@�n�@���@��+@�n�@�E�@�$�@�J@��T@��-@�x�@�O�@�&�@��/@���@�j@�1'@��m@��P@���@��@�C�@��@��@���@�~�@�n�@�V@�-@�@��#@�G�@��`@��u@�Z@�b@�  @���@��P@�\)@�;d@�+@��@�@���@��y@���@��R@�v�@�5?@�$�@�@���@��7@�hs@�G�@��@��@��j@�r�@� �@��@�ƨ@�t�@�;d@��!@�ff@�^5@�M�@��@���@�7L@�j@��;@��F@�l�@��y@�n�@�{@��@��^@�hs@��@���@��@��u@�j@�Q�@�9X@�1'@�1@���@�ƨ@��w@���@�t�@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`BA�Q�A�G�A�5?A�/A��A���A��TA��#A���A���A�ƨA̼jA̺^A̶FA̲-A̮Ḁ�A̝�A̕�Ȁ\A�p�A�=qA�jA���A�9XAüjA���A�A��#A��
A�l�A��mA��^A�;dA�=qA�z�A��TA�I�A��
A�1'A��A��A��-A���A�`BA��RA�VA�
=A�VA�%A���A��RA��!A���A�C�A���A��#A�K�A���A��DA��A��yA�n�A�(�A��A�1'A��uA��`A��-A�I�A�33A��wA�VA��A�ĜA��A��A��A�ƨA�G�A��A���A��A�"�A�x�A�ZA���A�A�A�A�dZA�G�A�I�A�$�A��A�33A�(�A���A���A��HA��A��A��yA��HA�x�A���A�7LA���A��7A��A���A���A�VA�ĜA�{A���A�\)A}��A{�AzffAyVAxZAw�FAw|�Aw&�Au��At�9As%Aq�Aq�Apz�Am��Ak"�Ai
=Ag33Af$�Ae/Ac�wAb�A` �A^�A]hsA[ƨA[%AY��AX�DAV�`ATQ�AR�`AQt�AP��AO�7AN��AM�AKƨAI33AG�AFz�AE�AD�uAB��AA�TAA�hAAt�AA`BAAC�AA+AAA@$�A=��A;�A;?}A:n�A9l�A8��A8v�A8�A7S�A6��A61A5/A4��A4ZA3C�A1O�A/�A.�A-�TA,�9A++A*�A)�A(��A'x�A&~�A%��A$��A#�wA"�jA"  A �uA {A|�A��A�#Ax�A�jAC�A�A��AK�AZA�A��AJAG�A�/A��A��A�RAdZA
ffA	�;A	"�A��A��A�A�+A��AG�AZAp�A ��@�"�@���@���@�~�@�-@��@�$�@�M�@���@�33@�X@@��@�1'@�
=@ꗍ@�V@�!@���@�X@��@�\@���@޸R@���@�G�@�z�@ۅ@��@ؓu@�C�@�v�@ԛ�@�\)@�n�@ёh@� �@��@�n�@�V@���@��@�-@ɉ7@Ȭ@�33@�=q@ļj@î@��^@� �@��@��@�Ĝ@�(�@���@�~�@�@�Ĝ@�ƨ@��R@�J@���@��@�ƨ@��R@�V@��7@�Z@��@�@���@��-@���@�r�@�ƨ@�\)@���@�ȴ@�n�@���@�x�@��/@�9X@��m@�t�@��y@�$�@��#@�p�@��@�7L@��@��/@�/@��@��R@��R@��T@��@�?}@��9@�ƨ@���@���@��!@�n�@��#@�G�@��@�ȴ@�@�?}@���@�Z@�1@� �@��/@��-@��T@�J@��-@�x�@�G�@��/@���@�r�@�Q�@�1'@�9X@�1'@��P@�dZ@�K�@�"�@��@��@�E�@�x�@��h@�O�@�j@�(�@��m@��P@�+@��T@�?}@���@�A�@�b@�1@��@�A�@��u@��`@���@�hs@�M�@��R@��!@��+@�E�@�?}@�%@���@�E�@�V@�E�@�n�@���@��+@�n�@�E�@�$�@�J@��T@��-@�x�@�O�@�&�@��/@���@�j@�1'@��m@��P@���@��@�C�@��@��@���@�~�@�n�@�V@�-@�@��#@�G�@��`@��u@�Z@�b@�  @���@��P@�\)@�;d@�+@��@�@���@��y@���@��R@�v�@�5?@�$�@�@���@��7@�hs@�G�@��@��@��j@�r�@� �@��@�ƨ@�t�@�;d@��!@�ff@�^5@�M�@��@���@�7L@�j@��;@��F@�l�@��y@�n�@�{@��@��^@�hs@��@���@��@��u@�j@�Q�@�9X@�1'@�1@���@�ƨ@��w@���@�t�@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB49B49B6FB6FB6FB8RB:^B;dB;dB;dB<jB>wB?}B@�BB�BB�BC�BC�BC�BC�BB�BB�B?}B8RBE�B=qB�B	7BB��B��B�B�B�B�B�B��B��B��B��B��B��B��B��BBBBBBBBBBBBBBBB  B  B��B��B��B�B�BB��B�qB�LB�B��B�DB�B� B{�Bv�BgmBS�BL�BD�B;dB:^B33B-B)�B"�BuBJB��B�B�fB�B��B�!B�{Bq�BK�B49B2-B$�B
��B
�B
�NB
��B
��B
�B
�B
�yB
�`B
�/B
��B
��B
�jB
�3B
��B
� B
R�B
@�B
,B
�B
�B
�B
�B
�B
{B
hB

=B	��B	��B	�B	�B	�B	��B	��B	�^B	�-B	�!B	��B	��B	��B	�+B	�B	|�B	x�B	q�B	hsB	aHB	Q�B	I�B	A�B	>wB	<jB	49B	.B	&�B	#�B	uB	bB	\B	JB	%B	B	  B	  B��B��B��B��B��B�B�B�B�yB�fB�sB�sB�fB�ZB�TB�HB�#B�B��B��B�qB�9B�B��B��B��B��B��B�uB�bB�VB�JB�=B�+B�B�+B�B}�B~�B}�B� B}�B� B{�Bz�B{�Bp�Bm�Br�Bl�BjBhsBdZBcTBZBXBQ�BM�BK�BK�BG�BE�BB�B=qB:^B8RB7LB5?B2-B2-B1'B1'B0!B.B+B'�B&�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBuBuBuBoB{B{BuB{B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B#�B!�B"�B$�B&�B(�B)�B)�B+B-B0!B0!B2-B49B7LB6FB7LB9XB<jB=qB?}B@�B@�B@�B@�BA�BB�BC�BE�BF�BI�BL�BM�BM�BO�BXBcTBcTBiyBo�By�B� B�B�7B�\B�uB�{B��B��B��B��B��B��B��B�'B�3B�^B�qB��BŢBɺB��B�#B�TB�fB�B�B�B�B��B��B��B��B	B	1B	PB	oB	oB	oB	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	(�B	+B	-B	0!B	0!B	1'B	;dB	=qB	@�B	A�B	C�B	I�B	M�B	Q�B	VB	aHB	hsB	k�B	l�B	p�B	k�B	iyB	n�B	t�B	v�B	w�B	y�B	~�B	�B	�B	�+B	�1B	�7B	�=B	�JB	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�?B	�FB	�RB	�^B	�dB	�dB	�dB	�jB	�jB	�jB	�qB	�wB	��B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�/B	�5B	�5B	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B5?B5?B6FB6FB7LB8RB:^B;dB;dB<jB<jB>wB?}B@�BB�BC�BC�BC�BC�BC�BC�BB�BB�BA�BN�BF�B#�BDB%B  B��B�B�B�B�B��B��B��B��B��B��B��B��BBB%B+BB%BBBBBB%B%BBBBBB��B��B��B��B�TB��B��B�dB�'B��B�PB�+B�B|�B|�Bm�BW
BO�BF�B<jB<jB8RB0!B-B'�B�BhBB��B�B�#BƨB�?B��Bw�BO�B5?B5?B+BB
�B
�fB
�B
��B
�B
�B
�B
�mB
�BB
��B
ĜB
�wB
�LB
��B
�DB
ZB
E�B
1'B
"�B
�B
�B
�B
�B
�B
{B
\B
B	��B	�B	�B	�)B	��B	ŢB	�qB	�9B	�-B	�B	��B	��B	�7B	�B	}�B	z�B	s�B	k�B	ffB	T�B	L�B	C�B	A�B	>wB	6FB	33B	-B	(�B	{B	oB	uB	hB		7B	B	B	  B	  B	  B��B��B��B��B�B�B�B�mB�yB�yB�yB�fB�`B�TB�/B�B�B��BB�LB�!B�B��B��B��B��B��B�uB�bB�VB�VB�=B�+B�DB�B� B�B�B�B� B�B~�B}�B~�Bs�Bp�Bv�Bo�Bl�BiyBgmBgmB\)B[#BT�BO�BM�BO�BJ�BG�BF�B?}B<jB;dB9XB8RB5?B33B2-B1'B1'B0!B0!B.B(�B&�B%�B#�B!�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B �B�B"�B#�B#�B%�B#�B$�B&�B(�B+B+B+B-B/B1'B2-B49B6FB8RB7LB9XB;dB=qB?}B@�BA�BA�BA�BB�BB�BC�BD�BF�BG�BJ�BM�BM�BN�BP�BXBdZBdZBhsBn�Bx�B�B�%B�=B�\B�{B��B��B��B��B��B��B��B��B�-B�9B�dB�wBBƨBɺB��B�B�TB�mB�B��B��B��B��B��B��B��B	B	1B	VB	oB	oB	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	)�B	,B	0!B	1'B	1'B	2-B	;dB	=qB	@�B	A�B	C�B	I�B	M�B	Q�B	T�B	aHB	hsB	k�B	m�B	r�B	l�B	iyB	n�B	t�B	w�B	w�B	y�B	� B	�B	�B	�+B	�1B	�7B	�DB	�PB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�?B	�FB	�LB	�XB	�^B	�dB	�dB	�jB	�jB	�jB	�jB	�qB	�}B	B	ĜB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�HB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�s111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181657232011121816572320111218165723  AO  ARGQ                                                                        20111130103220  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130103220  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165723  IP                  G�O�G�O�G�O�                