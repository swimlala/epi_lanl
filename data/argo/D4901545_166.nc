CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-11-21T18:02:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  AP   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  CH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  zh   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20171121180216  20181025093511  4901545 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4741                            2C  D   NAVIS_A                         0185                            052512                          863 @�7 ���1   @�7����@:G-�c7�;dZ1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��G@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B�]B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B�ǮB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe��Cg�qCjCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\D{�D{\D{�\D|�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aǟ�Aǣ�Aǟ�AǙ�AǙ�AǬAǬAǬAǩ�Aǩ�Aǩ�AǬAǬAǬAǬAǬAǬAǬAǮAǬAǬAǬAǬAǙ�A�x�A�K�A�1'A��A�A��HA�ƨA�ƨAƴ9Aư!AƬAƬAơ�Aƛ�AƉ7A�I�Aũ�A���A� �A��#A�bA�ƨA�oA�A�$�A��A���A��A��A�l�A��A�1'A��A���A�{A�A�-A�VA�A�A���A�jA�dZA�M�A�7LA�XA���A���A��yA�`BA��A�dZA���A��TA���A��A�{A���A�9XA��DA�A���A�C�A�A��uA�n�A��A���A�t�A�r�A�K�A���A�Q�A��#A�33A��A��9A�JA���A���A�bNA���A��A���A�+A�XA���A���A���A�ȴA�XA~~�A|$�AyG�Au�PAr�Aqp�Ao��Al�HAjbNAh��Af�RAc�Ab��AbVA`�A_oA^r�A]��A\�!AZ��AX�jAW33AT�ARjAQ��AQ33AP��AP�DAPv�AN��AL^5AK��AJv�AI��AHE�AG�AG�FAGXAFbNAE33AD=qAC��ABȴAA�AA�FAA�wA@�yA@9XA?S�A?��A>I�A< �A:��A;�-A;�wA:5?A9%A7�mA6��A5K�A4�HA4n�A4$�A3�mA2��A25?A1O�A0�A0ȴA01'A/�;A.��A,Q�A+p�A*E�A)��A(��A'��A%��A$�9A#\)A#+A#?}A"�\A"5?A!��A �A�A�A�/AjA�+A��A��A�A��Az�A�hA�A�/Ar�A�#A��A�#A�A~�A�^A?}AbNA��A��A�DA1A�hAA�A�!A�-A
E�A	�;A	�hA	O�A��A�A|�An�A�;A�AJA�AI�AVA �\A 5?@��@���@���@�7L@�r�@�~�@�X@���@�b@�"�@�r�@�@���@���@�@�j@�!@���@�9X@��@�"�@���@�A�@۝�@���@ٺ^@�bN@�33@��@�r�@ӥ�@��H@���@�V@��m@�;d@͑h@̼j@�bN@��@�Q�@� �@Ɨ�@ź^@��@�A�@�t�@¸R@�I�@�X@��7@��@��`@�z�@�9X@��
@���@�p�@�Ĝ@���@��@�z�@�bN@��F@�`B@���@��`@��j@�1'@��;@�t�@�o@��+@�%@�1'@��
@�K�@��@���@��@�/@��j@�Q�@�K�@���@�$�@�G�@�%@��`@��m@���@���@�&�@�Z@�A�@���@�ȴ@�v�@�5?@�@���@�Q�@�1'@���@���@��#@�`B@���@�Ĝ@��D@�bN@�Z@�Q�@�A�@�b@���@�dZ@��@�^5@��T@��-@�hs@��@�Q�@��@��w@�"�@��y@�n�@��@�p�@��@�r�@�  @���@�o@���@�V@���@�&�@��/@��9@��D@�Q�@���@���@�\)@�+@�ȴ@�ff@�ff@�ff@���@�p�@�7L@��j@�I�@��P@�C�@���@���@���@�v�@�E�@��@�x�@�X@��`@�r�@�Q�@� �@��w@�S�@�33@�
=@�ȴ@�~�@�-@���@���@��h@���@�J@�=q@���@�n�@���@��h@�G�@��j@�bN@~ff@|��@|Z@|1@{�
@{ƨ@{��@{��@{t�@{dZ@{C�@{33@{33@{o@z~�@z�\@z^5@yx�@y&�@x�@w��@wK�@w\)@w;d@v��@u�T@uO�@uV@t�j@tI�@s��@s@r��@r~�@rM�@r=q@r�@q��@q%@p�`@pĜ@p�u@p�u@p��@p�@pr�@pQ�@p �@o�@o�@oK�@n�y@nE�@m��@m��@n@n{@m/@m�h@mV@l��@l�/@l�/@l�/@l��@l�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aǟ�Aǣ�Aǟ�AǙ�AǙ�AǬAǬAǬAǩ�Aǩ�Aǩ�AǬAǬAǬAǬAǬAǬAǬAǮAǬAǬAǬAǬAǙ�A�x�A�K�A�1'A��A�A��HA�ƨA�ƨAƴ9Aư!AƬAƬAơ�Aƛ�AƉ7A�I�Aũ�A���A� �A��#A�bA�ƨA�oA�A�$�A��A���A��A��A�l�A��A�1'A��A���A�{A�A�-A�VA�A�A���A�jA�dZA�M�A�7LA�XA���A���A��yA�`BA��A�dZA���A��TA���A��A�{A���A�9XA��DA�A���A�C�A�A��uA�n�A��A���A�t�A�r�A�K�A���A�Q�A��#A�33A��A��9A�JA���A���A�bNA���A��A���A�+A�XA���A���A���A�ȴA�XA~~�A|$�AyG�Au�PAr�Aqp�Ao��Al�HAjbNAh��Af�RAc�Ab��AbVA`�A_oA^r�A]��A\�!AZ��AX�jAW33AT�ARjAQ��AQ33AP��AP�DAPv�AN��AL^5AK��AJv�AI��AHE�AG�AG�FAGXAFbNAE33AD=qAC��ABȴAA�AA�FAA�wA@�yA@9XA?S�A?��A>I�A< �A:��A;�-A;�wA:5?A9%A7�mA6��A5K�A4�HA4n�A4$�A3�mA2��A25?A1O�A0�A0ȴA01'A/�;A.��A,Q�A+p�A*E�A)��A(��A'��A%��A$�9A#\)A#+A#?}A"�\A"5?A!��A �A�A�A�/AjA�+A��A��A�A��Az�A�hA�A�/Ar�A�#A��A�#A�A~�A�^A?}AbNA��A��A�DA1A�hAA�A�!A�-A
E�A	�;A	�hA	O�A��A�A|�An�A�;A�AJA�AI�AVA �\A 5?@��@���@���@�7L@�r�@�~�@�X@���@�b@�"�@�r�@�@���@���@�@�j@�!@���@�9X@��@�"�@���@�A�@۝�@���@ٺ^@�bN@�33@��@�r�@ӥ�@��H@���@�V@��m@�;d@͑h@̼j@�bN@��@�Q�@� �@Ɨ�@ź^@��@�A�@�t�@¸R@�I�@�X@��7@��@��`@�z�@�9X@��
@���@�p�@�Ĝ@���@��@�z�@�bN@��F@�`B@���@��`@��j@�1'@��;@�t�@�o@��+@�%@�1'@��
@�K�@��@���@��@�/@��j@�Q�@�K�@���@�$�@�G�@�%@��`@��m@���@���@�&�@�Z@�A�@���@�ȴ@�v�@�5?@�@���@�Q�@�1'@���@���@��#@�`B@���@�Ĝ@��D@�bN@�Z@�Q�@�A�@�b@���@�dZ@��@�^5@��T@��-@�hs@��@�Q�@��@��w@�"�@��y@�n�@��@�p�@��@�r�@�  @���@�o@���@�V@���@�&�@��/@��9@��D@�Q�@���@���@�\)@�+@�ȴ@�ff@�ff@�ff@���@�p�@�7L@��j@�I�@��P@�C�@���@���@���@�v�@�E�@��@�x�@�X@��`@�r�@�Q�@� �@��w@�S�@�33@�
=@�ȴ@�~�@�-@���@���@��h@���@�J@�=q@���@�n�@���@��h@�G�@��j@�bN@~ff@|��@|Z@|1@{�
@{ƨ@{��@{��@{t�@{dZ@{C�@{33@{33@{o@z~�@z�\@z^5@yx�@y&�@x�@w��@wK�@w\)@w;d@v��@u�T@uO�@uV@t�j@tI�@s��@s@r��@r~�@rM�@r=q@r�@q��@q%@p�`@pĜ@p�u@p�u@p��@p�@pr�@pQ�@p �@o�@o�@oK�@n�y@nE�@m��@m��@n@n{@m/@m�h@mV@l��@l�/@l�/@l�/@l��@l�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBM�BM�BM�BM�BM�BN�BN�BN�BM�BM�BN�BM�BN�BM�BN�BN�BM�BM�BM�BN�BO�BVBXB^5Be`Bl�Bp�Bs�Bu�Bw�By�By�Bz�Bz�B� B�B�+B�=B�PB�bB�%BS�Bl�B�7B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�7B�B� Bs�BW
BB�B)�B�B�BoB{BhBB��B�B�`B�HB�BB�B��B�FB��BbNBR�BP�BP�BQ�BT�B\)BYBT�BN�BE�B9XB(�B�BuB%B
��B
�yB
��B
��B
�RB
�B
��B
�bB
�B
s�B
jB
YB
H�B
49B
�B
B	��B	�fB	��B	B	�RB	��B	��B	�{B	�hB	�=B	~�B	y�B	s�B	l�B	_;B	F�B	B�B	8RB	&�B	!�B	�B	�B	�B	�B	\B	%B	B��B��B��B�B�B�B�B�mB�`B�`B�;B�/B�5B�`B�BB�)B�)B�B�fB��B��B�B�B�B�fB�TB�#B�B�B�B�B��B��B��B��B��BȴBĜB��B�XB��B��B��B��B�{B�7B{�Bs�BhsBl�By�Bu�Bp�Bl�BgmB[#BYBYBYBbNBp�Br�Br�Bm�BhsBe`BdZBe`BgmBe`B_;B[#BXBVBS�BQ�BN�BL�BI�BE�BC�BA�B;dB6FB33B1'B2-B49B5?B7LB5?B33B0!B.B-B)�B&�B#�B"�B"�B#�B#�B"�B�B�B�B�B�B�B�B�B�B�B�BuBbBVBJBJBPBJBPB\BhBbBhBhBbBhBuBoBhBhBhBhBoBoB{B{B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B$�B$�B$�B$�B#�B$�B-B-B-B-B.B.B0!B2-B33B9XB<jB=qB?}B@�B@�BC�BE�BG�BH�BK�BL�BN�BQ�BR�BQ�BS�BW
BYB\)B_;B_;BaHBbNBcTBdZBhsBk�Bm�Bm�Bn�Bs�By�B|�B� B�B�B�B�%B�%B�+B�1B�DB�JB�VB�hB�{B��B��B��B��B��B��B��B��B��B��B��B�B�'B�?B�LB�dB�}BBǮB��B��B��B��B��B��B��B�B�B�)B�;B�;B�;B�BB�ZB�fB�yB�B�B��B��B��B��B��B	B	B	%B	1B	DB	VB	\B	bB	oB	�B	�B	�B	�B	�B	�B	$�B	&�B	(�B	/B	5?B	8RB	<jB	>wB	>wB	>wB	>wB	@�B	B�B	?}B	?}B	B�B	C�B	D�B	E�B	F�B	H�B	I�B	J�B	J�B	M�B	M�B	O�B	R�B	VB	VB	XB	[#B	^5B	dZB	ffB	ffB	gmB	hsB	jB	m�B	n�B	p�B	r�B	u�B	x�B	y�B	{�B	|�B	|�B	~�B	�B	�%B	�1B	�7B	�JB	�\B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BM�BM�BM�BM�BM�BN�BN�BN�BM�BM�BN�BM�BN�BM�BN�BN�BM�BM�BM�BN�BO�BVBXaB^�BfBl�BqBtBv[Bx8By�Bz)Bz�Bz�B�B�:B�BB�vB��B��B��B\'BprB��B��B��B��B��B��B��B�\B��B�\B�(B�\B�	B�jB�1B�\B�xB�xB�B��B�4B�hB�'B��B�Bz�B\�BJ%B0B \ByB.BBB�B�B��B�B��B��B�xB�ZB��B��B��Bg�BU8BR+BQ�BRBU�B_/BZ�BW�BRBKGB@)B,PB�B�B	�B�B
��B
�iB
��B
��B
��B
��B
��B
�B
v0B
p;B
^yB
N�B
;wB
�B
�B	�B	�dB	�`B	��B	��B	�OB	�,B	�B	�B	�B	�XB	{�B	u�B	p�B	c�B	JVB	G�B	>[B	(�B	"�B	 �B	lB		B	0B	B	'B	�B��B��B��B�YB�B�)B�B��B�1B�dB�UBݱB�4B�rB� B�:BۊB�,B�B�|B�KB�B��B�B�BB��B�zB�0B�\B��B־B֬B�B�qB��B�DB�iBŻB�KB�SB��B��B��B�)B�B�EB~�Bw�BiBlpB{�Bv�Br�Bn�Bk&B[�B[BZcBX�BaBp�Bs@BvHBq.Bk Bf�BeBf�Bi0Bh`Ba�B]3BY|BW�BUBS�BRBN�BL�BF�BD�BD�B>�B8B6TB2B2�B4�B6�B9�B6bB5�B1�B0@B0B+�B*wB'B$RB#�B$�B&�B(�B �B B!�BfBiB�BFB�B�B�BrB�B�B^B=B�B�B�BABgB_B�B�BJBB�B�B{BXB{B4B�B)B$BNB�BBRB�B0B�BLB�B�B�B�BKB6B%B�B+BB:BpB [B#�B%B% B$�B%B$�B(DB-�B-6B-TB-�B.�B.�B0�B3B5oB:�B<�B>AB?�B@�BA�BD�BFRBHUBJ)BL�BM�BPBRHBS/BSbBU�BX2BY�B]>B_mB`$BbxBb�Bc�BeBi�Bl\Bm�Bn"Bo�Bt�BziB}iB�:B�FB�=B�(B�.B�7B�gB��B��B��B�>B�B��B��B��B�~B�B��B�yB�
B�pB�B��B�mB��B��B��B�B��B��B�NBȝB�*B�	B�B�&B�`B�dB�hB�WBٛBܭB�;B�BB��B��B�B�B�"B�B�!B�*B�-B�B�5B�GB	�B	�B	[B	�B	�B	�B	�B	�B	B	�B	�B	�B	B	(B	 JB	%&B	&�B	(SB	.�B	4�B	7�B	<�B	?pB	>�B	>�B	?\B	A6B	DvB	@�B	@B	B�B	C�B	D�B	E�B	F�B	H�B	I�B	J�B	J�B	M�B	M�B	PYB	R�B	V;B	V�B	XWB	[�B	^�B	d�B	f[B	f�B	g�B	iFB	j�B	m�B	n�B	p�B	s-B	v)B	yB	zB	|	B	|�B	}B	XB	�oB	�>B	�IB	�ZB	�IB	�RB	�}B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	��B	��B	��B	�qB	��B	�IB	��B	�B	��B	��B	�3B	�B	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<x<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<|x�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.01 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810231352132018102313521320181023135213  AO  ARCAADJP                                                                    20171121180216    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171121180216  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171121180216  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181023135213  QC  PRES            @�33D|� G�O�                PM  ARSQCTM V1.1                                                                20181023135213  QC  PSAL            @�33D|� G�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181025093511  IP                  G�O�G�O�G�O�                