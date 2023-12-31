CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:10:40Z creation;2022-06-04T19:10:40Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191040  20220610151507  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ا�/hL1   @ا����@/��1&��d ���S�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx��B~��B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�ffB�  B�  B�  B�  Bԙ�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ D�|�D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D��3D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�\@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��BxB~B���B���B�ǮB���B���B���B���B���B���B���B���B���B���B�aGB�.B�aGB���B���B���B���BԔ{B�ǮB���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qCC�qC�qC��C�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC@CA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCvCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D�|{Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��D���D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�<{D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D���D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��ZA���A���A���A���A���A���A��A�"hA�7�A�K)A�e�A�aHA�`�A�b�A�M�A�O�A�Z�A�\�A�_�A�g8A�l�A�l�A�p�A�n/A�k�A�R�A�3�A�MA�'�AɃA�9$A�d&Aʐ�A�?�A�/OA��8A�
	A��DA���Aʫ�A���A��%A�]�A�|�A��A��A��A�_A���A���A�#�A��'A�X�A���A��AA��CA���A��!A��A���A��A��A�oA�%A�4A�x�A��4A��A���A���A��zA�z�A��'A�ZA�@A���A�%�A��HA�n/A�T�A�}VA�|A��A��KA� �A�A�~(A�-�A�"A�A|aAv��AoK�Al��Ak�RAj(Aha�Adw�A_xAZ��AW��ARZANC-AKTaAIt�AG� AE�AEJAEU�AD�gACg8ABX�AAA@�FA>4A<�A9��A7��A7�]A4&A0��A0�?A0�fA0C�A0w2A0L0A/�A/QA-��A,�A,4A+��A+QA*��A*��A*_A(�A&��A&�oA'��A'U2A%�hA%c�A$VA#
=A ��AM�A�AV�A�AXyA��Aw2A2aA4nA \A0UAA A*0A[�A0UA>BA�mA��A��ATaA�BA~A
��A
_�A	��A	��A	7LA`BA�^A%A��A)�A��Ao AG�AqA�A��A�rA#�A$A#�A��A�A�fA�6A�9A-A 8�@�k�@�$t@��@��y@���@��+@�T�@�)_@��@�kQ@�͟@�|�@���@�H@�	@�a�@���@�خ@�Y�@�$t@�ff@�\�@��@���@�/�@��@�L�@��@�p;@���@�@�q�@��M@���@���@�C@��@�A�@�c @�A @�@@�\�@�W?@��@�ں@���@��H@�b@��@��@��@߾w@���@�c @�7@�Q�@ܧ�@ܧ�@�/�@���@�_@݌~@��@�O@�e�@�IR@�K�@�Q�@�=�@��@�3�@ٕ�@ؔF@��r@�v`@�
=@֟�@��@�,�@��K@ԣ@�O@�o�@��@ҏ\@��@�G�@�h
@�Dg@�R�@�u@�x�@�o�@��@�  @�%F@�v�@�Ɇ@ȸR@ȵ�@�c @���@�.I@�y>@�-@���@�8@ğ�@�=�@��@�,=@�e,@��X@�c @�z�@���@�6@��@��}@���@�ȴ@�  @�j�@�a@�J�@���@���@�/�@�i�@���@��x@���@��@�Q@�0U@��T@���@�8�@��@�Q�@���@�T�@�2a@��y@���@�1�@���@�IR@���@��\@�}V@�a|@�$�@��[@�T�@��9@�=q@�1@��}@���@�bN@�e@���@��5@�|�@���@�s@��@��@�Z@�;�@�@���@�]�@�;@���@��@�Q@��]@���@���@�s�@�L�@�q@���@�W�@���@�`B@���@�S�@�9X@��@�خ@��*@��4@�X�@��"@��@�2�@��D@��d@���@�x@�(@��X@���@��j@���@��=@�Y@�v�@�M@�/�@��D@���@�W?@��@��H@�~�@��@��@���@���@�6z@��h@�V�@�	�@��@��@�Y�@��@���@�[�@��g@�a@�S&@�*0@�Ɇ@�ff@�  @��W@��@���@�H�@��@���@�E�@��@��M@��@���@�Ĝ@���@��z@�^5@��@���@��z@�x@��@��u@�Q�@��6@�l�@�7L@��@��|@�a|@���@��0@��n@���@�c�@�U�@��	@�ی@��O@�~(@�C�@��.@�� @���@�l�@�.I@��H@��@��O@���@�j@�Ta@�!@��j@���@�o�@�F�@���@���@���@�c @�E�@�G@���@�U�@��@�v�@�-@��@���@�A�@��@��@��K@���@�tT@�>B@��@��-@���@�H�@��U@��_@���@�GE@��@���@���@���@�n/@�B�@� i@��B@��@�a|@�7�@�G@�ƨ@�s@�K�@�33@�	l@���@�y>@�M�@�0U@��@��9@��@�J#@��M@�͟@��<@�q�@�/�@�@�	@P�@,�@S@~��@~��@~@�@}��@|�U@|��@|]d@|M@{�@{�Q@{�4@{E9@{�@z�X@zv�@y�@y�X@ys�@y;@xĜ@x|�@w�g@w��@wj�@v��@vL0@v�@u��@u��@uS&@t��@t�@t�@s�$@s�@r�]@r��@r�@qT�@q%@p��@pI�@p$@o��@o�@o\)@n�@n�1@n_�@nGE@ne@n$�@m��@m��@m��@m|@m-w@l�/@l�u@l7�@k��@ka@j��@j�+@i��@i��@i�~@ik�@i?}@i�@h�@h�@hz�@g�a@g33@f�@f�b@f�@e��@e�@d��@d:�@cݘ@c��@cl�@c6z@c@bYK@a��@ac@a@`�?@`]d@`M@_�@_��@_H�@^�@^}V@^@]��@]	l@\�4@[�Q@[]�@[�@Z��@Z�\@Z0U@Z
�@Y�@Y�-@YX@Y*0@X�P@Xr�@W�:@Wa@V� @VH�@V
�@V�@Up�@U+@T�@S��@S'�@R�@R�<@Rs�@RW�@R?@R�@Q�@Q�h@Pw�@P@O��@O��@N��@Nv�@Nd�@N3�@N�@Mc�@Lی@Le�@LPH@L?�@K�q@K!-@J� @JYK@J{@I��@Io @IX@IS&@I0�@H��@HbN@H	�@G�@G��@Go@F{�@F;�@FO@Eԕ@E��@Ej@E�@D�v@DK^@C�;@C��@C33@B��@B\�@B5?@Bu@A��@A��@A\�@@�j@@Q�@@?�@?��@?��@>�@>YK@>
�@=ԕ@=@=��@=j@<ی@<��@<h�@;��@;�@:��@:��@:Q@:
�@9��@9�X@9!�@8��@8��@8$@7��@733@6�@6	@6	@5��@5��@5�'@54@4��@4��@4z�@4�@3��@3�*@3C�@2�]@2�@2�+@1��@1p�@1%@0��@0��@0m�@02�@/�@/�@/t�@.�2@.�'@.�\@.Q@.�@-��@-��@-�@-<6@-�@,��@,�`@,�v@,֡@,֡@,�U@,�.@,I�@,@+ݘ@+��@+��@+s@+_p@+�@+@*�'@*��@*#:@)ϫ@)��@)o @)N<@)+�@(�@(c�@(1'@'�$@&�8@&��@&�!@&��@&Z�@&e@& �@%��@%S&@$�@$�U@$��@$e�@$:�@$�@#ݘ@#�k@#\)@#9�@"�"@"�@"��@"8�@!�N@!��@!|@!5�@!	l@ ��@ �@ ��@ ��@ A�@�@�@@ߤ@ȴ@��@kQ@$�@��@�X@c@0�@�@��@�Y@bN@7@�}@��@x@RT@�B@��@kQ@a|@1�@��@|@*0@�@��@Xy@!@�+@�@ݘ@˒@��@��@(@��@u%@n�@d�@GE@�@�9@��@�@�@��@��@��@0�@#�@ی@j@%�@�@�@��@\)@!-@��@4@�@��@��@��@�d@��@��@m]@#�@�f@��@�e@�@��@�I@u�@�@�@�@@iD@;d@�@�@�6@{�@d�@R�@	@�@�H@�n@��@|@e,@Vm@2a@�@�@�$@�@|�@e�@Q�@M@M@>B@�@1@�6@��@o�@8@�@
ں@
��@
�+@
s�@
Q@
�@
J@	�Z@	�z@	rG@	o @	\�@	�@�@S�@9X@@�r@�@��@� @�[@��@1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��ZA���A���A���A���A���A���A��A�"hA�7�A�K)A�e�A�aHA�`�A�b�A�M�A�O�A�Z�A�\�A�_�A�g8A�l�A�l�A�p�A�n/A�k�A�R�A�3�A�MA�'�AɃA�9$A�d&Aʐ�A�?�A�/OA��8A�
	A��DA���Aʫ�A���A��%A�]�A�|�A��A��A��A�_A���A���A�#�A��'A�X�A���A��AA��CA���A��!A��A���A��A��A�oA�%A�4A�x�A��4A��A���A���A��zA�z�A��'A�ZA�@A���A�%�A��HA�n/A�T�A�}VA�|A��A��KA� �A�A�~(A�-�A�"A�A|aAv��AoK�Al��Ak�RAj(Aha�Adw�A_xAZ��AW��ARZANC-AKTaAIt�AG� AE�AEJAEU�AD�gACg8ABX�AAA@�FA>4A<�A9��A7��A7�]A4&A0��A0�?A0�fA0C�A0w2A0L0A/�A/QA-��A,�A,4A+��A+QA*��A*��A*_A(�A&��A&�oA'��A'U2A%�hA%c�A$VA#
=A ��AM�A�AV�A�AXyA��Aw2A2aA4nA \A0UAA A*0A[�A0UA>BA�mA��A��ATaA�BA~A
��A
_�A	��A	��A	7LA`BA�^A%A��A)�A��Ao AG�AqA�A��A�rA#�A$A#�A��A�A�fA�6A�9A-A 8�@�k�@�$t@��@��y@���@��+@�T�@�)_@��@�kQ@�͟@�|�@���@�H@�	@�a�@���@�خ@�Y�@�$t@�ff@�\�@��@���@�/�@��@�L�@��@�p;@���@�@�q�@��M@���@���@�C@��@�A�@�c @�A @�@@�\�@�W?@��@�ں@���@��H@�b@��@��@��@߾w@���@�c @�7@�Q�@ܧ�@ܧ�@�/�@���@�_@݌~@��@�O@�e�@�IR@�K�@�Q�@�=�@��@�3�@ٕ�@ؔF@��r@�v`@�
=@֟�@��@�,�@��K@ԣ@�O@�o�@��@ҏ\@��@�G�@�h
@�Dg@�R�@�u@�x�@�o�@��@�  @�%F@�v�@�Ɇ@ȸR@ȵ�@�c @���@�.I@�y>@�-@���@�8@ğ�@�=�@��@�,=@�e,@��X@�c @�z�@���@�6@��@��}@���@�ȴ@�  @�j�@�a@�J�@���@���@�/�@�i�@���@��x@���@��@�Q@�0U@��T@���@�8�@��@�Q�@���@�T�@�2a@��y@���@�1�@���@�IR@���@��\@�}V@�a|@�$�@��[@�T�@��9@�=q@�1@��}@���@�bN@�e@���@��5@�|�@���@�s@��@��@�Z@�;�@�@���@�]�@�;@���@��@�Q@��]@���@���@�s�@�L�@�q@���@�W�@���@�`B@���@�S�@�9X@��@�خ@��*@��4@�X�@��"@��@�2�@��D@��d@���@�x@�(@��X@���@��j@���@��=@�Y@�v�@�M@�/�@��D@���@�W?@��@��H@�~�@��@��@���@���@�6z@��h@�V�@�	�@��@��@�Y�@��@���@�[�@��g@�a@�S&@�*0@�Ɇ@�ff@�  @��W@��@���@�H�@��@���@�E�@��@��M@��@���@�Ĝ@���@��z@�^5@��@���@��z@�x@��@��u@�Q�@��6@�l�@�7L@��@��|@�a|@���@��0@��n@���@�c�@�U�@��	@�ی@��O@�~(@�C�@��.@�� @���@�l�@�.I@��H@��@��O@���@�j@�Ta@�!@��j@���@�o�@�F�@���@���@���@�c @�E�@�G@���@�U�@��@�v�@�-@��@���@�A�@��@��@��K@���@�tT@�>B@��@��-@���@�H�@��U@��_@���@�GE@��@���@���@���@�n/@�B�@� i@��B@��@�a|@�7�@�G@�ƨ@�s@�K�@�33@�	l@���@�y>@�M�@�0U@��@��9@��@�J#@��M@�͟@��<@�q�@�/�@�@�	@P�@,�@S@~��@~��@~@�@}��@|�U@|��@|]d@|M@{�@{�Q@{�4@{E9@{�@z�X@zv�@y�@y�X@ys�@y;@xĜ@x|�@w�g@w��@wj�@v��@vL0@v�@u��@u��@uS&@t��@t�@t�@s�$@s�@r�]@r��@r�@qT�@q%@p��@pI�@p$@o��@o�@o\)@n�@n�1@n_�@nGE@ne@n$�@m��@m��@m��@m|@m-w@l�/@l�u@l7�@k��@ka@j��@j�+@i��@i��@i�~@ik�@i?}@i�@h�@h�@hz�@g�a@g33@f�@f�b@f�@e��@e�@d��@d:�@cݘ@c��@cl�@c6z@c@bYK@a��@ac@a@`�?@`]d@`M@_�@_��@_H�@^�@^}V@^@]��@]	l@\�4@[�Q@[]�@[�@Z��@Z�\@Z0U@Z
�@Y�@Y�-@YX@Y*0@X�P@Xr�@W�:@Wa@V� @VH�@V
�@V�@Up�@U+@T�@S��@S'�@R�@R�<@Rs�@RW�@R?@R�@Q�@Q�h@Pw�@P@O��@O��@N��@Nv�@Nd�@N3�@N�@Mc�@Lی@Le�@LPH@L?�@K�q@K!-@J� @JYK@J{@I��@Io @IX@IS&@I0�@H��@HbN@H	�@G�@G��@Go@F{�@F;�@FO@Eԕ@E��@Ej@E�@D�v@DK^@C�;@C��@C33@B��@B\�@B5?@Bu@A��@A��@A\�@@�j@@Q�@@?�@?��@?��@>�@>YK@>
�@=ԕ@=@=��@=j@<ی@<��@<h�@;��@;�@:��@:��@:Q@:
�@9��@9�X@9!�@8��@8��@8$@7��@733@6�@6	@6	@5��@5��@5�'@54@4��@4��@4z�@4�@3��@3�*@3C�@2�]@2�@2�+@1��@1p�@1%@0��@0��@0m�@02�@/�@/�@/t�@.�2@.�'@.�\@.Q@.�@-��@-��@-�@-<6@-�@,��@,�`@,�v@,֡@,֡@,�U@,�.@,I�@,@+ݘ@+��@+��@+s@+_p@+�@+@*�'@*��@*#:@)ϫ@)��@)o @)N<@)+�@(�@(c�@(1'@'�$@&�8@&��@&�!@&��@&Z�@&e@& �@%��@%S&@$�@$�U@$��@$e�@$:�@$�@#ݘ@#�k@#\)@#9�@"�"@"�@"��@"8�@!�N@!��@!|@!5�@!	l@ ��@ �@ ��@ ��@ A�@�@�@@ߤ@ȴ@��@kQ@$�@��@�X@c@0�@�@��@�Y@bN@7@�}@��@x@RT@�B@��@kQ@a|@1�@��@|@*0@�@��@Xy@!@�+@�@ݘ@˒@��@��@(@��@u%@n�@d�@GE@�@�9@��@�@�@��@��@��@0�@#�@ی@j@%�@�@�@��@\)@!-@��@4@�@��@��@��@�d@��@��@m]@#�@�f@��@�e@�@��@�I@u�@�@�@�@@iD@;d@�@�@�6@{�@d�@R�@	@�@�H@�n@��@|@e,@Vm@2a@�@�@�$@�@|�@e�@Q�@M@M@>B@�@1@�6@��@o�@8@�@
ں@
��@
�+@
s�@
Q@
�@
J@	�Z@	�z@	rG@	o @	\�@	�@�@S�@9X@@�r@�@��@� @�[@��@1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BC�BC�BC�BCaBB�BC�BC�BC�BIB`BxB��B��B�rB	�B	B	AB	�B		RB	
�B	B	 B	�B	B	�B	!B	 BB	�B	�B	kB	1[B	a�B	��B	�B	�}B
p�B
��B
�B
یB
�B
�6B
�BBhB'�Bi_Bp�B�2B��B�B  B �B
�B#�B/5B2�B0�B4�B2�B/B,�B,qB(�B �BuB��B�}B�"B�<B�B��B�QB�
B�?B��B��B�B��BjBT�B>(B(�B�B
�jB
�'B
�~B
�SB
r-B
[=B
B[B
$&B
NB	�B	��B	�9B	�B	w�B	l�B	`�B	JrB	-wB	1B	�B��B�tB�-BևBˬBĶB�BB�OB�/B�B�B�FB�TBݲBרB�FB�MB��B��BбB��B�B	TB	(sB	2|B	3�B	2aB	6B	72B	;�B	?.B	D�B	F�B	NB	Y�B	O�B	U�B	uB	�B	��B	��B	��B	��B	��B	z�B	r-B	o�B	m�B	kQB	ffB	ZB	K)B	E�B	IB	`�B	g�B	[�B	bhB	`�B	\�B	U�B	R�B	L�B	IlB	I�B	J=B	F�B	GzB	DMB	B�B	P}B	R�B	PHB	IRB	G�B	<�B	9>B	<�B	@�B	A�B	I�B	M�B	NpB	OBB	T,B	a|B	f�B	j0B	r�B	v�B	}VB	|�B	��B	�xB	�bB	�NB	��B	�FB	�[B	�eB	��B	��B	��B	��B	��B	�B	�jB	�pB	��B	�HB	�:B	�B	�mB	�*B	�QB	��B	�B	��B	�*B	��B	��B	�	B	�<B	��B	�[B	��B	�BB	��B	�BB	��B	��B	�B	�mB	�B	��B	ݘB	ܬB	�KB	��B	��B	��B	�uB	�TB	��B	ңB	ӏB	�&B	��B	��B	��B	��B	�B	�EB	ݲB	�vB	�B	ߤB	��B	ܒB	ܒB	��B	�B	��B	�HB	��B	�B	��B	��B	��B	�-B	�-B	��B	�B	ܬB	�)B	�7B	�B	��B	�=B	��B	چB	�$B	�NB	��B	ϫB	�.B	�B	�,B	�YB	�mB	ƎB	�lB	�xB	ݘB	ܒB	��B	چB	��B	�_B	�+B	�B	��B	�$B	�SB	�FB	�B	��B	�B	ϑB	�bB	�.B	��B	̈́B	��B	�	B	�_B	�1B	�pB	�aB	��B	�B	�WB	ܒB	�VB	�B	�|B	�B	�B	�B	��B	�B	�ZB	�tB	�B	�@B	�B	�B	�&B	�ZB	�`B	�2B	�B	��B	��B	�B	�0B	�B	�6B	�QB	�WB	�B	�B	�WB	��B	��B	�B	�B	�!B	�oB	��B	�'B	�B	�-B	�-B	�aB	�aB	�B	��B	�MB	�hB	�hB	�B	��B	�B	�B	�9B	�9B	�nB	��B	�B	�?B	�+B	�B	�fB	��B	��B	�B	�8B	�RB	�lB	�	B	�XB	�B	�*B	�^B	��B	��B	�JB	�JB	�JB	��B	�6B	�PB	�B	�(B	�B	�B	�]B	�]B	�HB	�}B
 4B
B
UB
�B
�B
�B
uB
�B
{B
�B
�B
�B
�B
3B
SB
B
�B
+B
B
�B
B
_B
KB
fB
�B
fB
�B
	lB

#B

�B

�B

=B
	�B

=B

�B
)B
�B
JB
�B
B
6B
PB
�B
pB
pB
BB
vB
�B
�B
HB
:B
�B
�B
�B
�B
�B
uB
�B
�B
B
�B
�B
MB
�B
�B
SB
mB

B

B
YB
�B
�B
�B
�B
B
_B
EB
_B
�B
�B
�B
1B
B
1B
KB
�B
�B
B
QB
kB
�B
�B
]B
�B
�B
IB
�B
5B
!B
VB
;B
�B
 B
�B
�B
 vB
!B
!-B
!�B
!�B
"B
"NB
"�B
"�B
"�B
"�B
"�B
# B
#nB
$@B
$�B
%,B
&B
'RB
($B
(sB
(�B
(�B
)*B
)�B
*B
*�B
*�B
*�B
+�B
+�B
,"B
,=B
,=B
,WB
,�B
,qB
,=B
,=B
-�B
-�B
-�B
./B
.cB
.cB
.cB
.�B
/B
/�B
0!B
0�B
0oB
0oB
0�B
1B
1B
0�B
1vB
1vB
1[B
1�B
2�B
2�B
2�B
33B
3hB
3�B
3�B
4B
49B
3�B
3�B
3hB
3�B
3�B
3�B
3�B
3�B
3�B
4B
4�B
4�B
4nB
4�B
5B
5�B
5�B
6zB
7�B
8�B
9	B
9XB
9�B
9�B
9�B
:*B
:*B
:*B
:�B
:�B
;B
:�B
;B
;JB
;dB
;B
;B
;�B
;�B
<B
<6B
<PB
<�B
<�B
<�B
<�B
="B
=�B
>(B
>B
>BB
>BB
>wB
?B
?B
?B
?.B
?.B
?HB
?HB
?HB
?cB
?�B
?�B
@B
@iB
@�B
@�B
AB
A�B
A�B
BB
B'B
B�B
B�B
B�B
CB
CGB
CaB
C�B
C{B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
G+B
F�B
F�B
F%B
FB
E�B
E�B
F?B
F�B
F�B
F�B
GB
G_B
H�B
H�B
H�B
H�B
I�B
I�B
IlB
I�B
IlB
JrB
JrB
JrB
JrB
JrB
K�B
LJB
L~B
L�B
L�B
MB
MjB
M�B
M�B
M�B
N"B
NVB
NVB
N"B
NB
NpB
NpB
N�B
N�B
OB
O(B
OvB
OvB
OBB
OvB
O�B
O�B
O�B
PHB
P�B
Q B
Q�B
RB
RB
RB
R�B
R�B
SB
R�B
R�B
S�B
S�B
S�B
T,B
TaB
T�B
T�B
T�B
U2B
U2B
UB
T�B
U2B
T�B
UB
UgB
UgB
UgB
U�B
VB
VB
VSB
V�B
W$B
W�B
XEB
X+B
XB
X_B
XyB
X�B
YB
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
Z�B
[WB
[�B
[�B
\B
\B
\CB
\]B
\�B
]B
]/B
]IB
]�B
^B
^B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
_;B
_pB
_�B
_�B
`'B
`vB
`\B
`�B
`�B
`�B
`�B
`�B
aB
`�B
`�B
`�B
a�B
a�B
a�B
a�B
b4B
bNB
bhB
b�B
b�B
b�B
c B
c�B
c�B
c�B
c�B
dB
dB
dtB
d�B
d�B
d�B
d�B
d�B
eB
eFB
e`B
ezB
e�B
e�B
e�B
fB
f2B
fLB
f�B
f�B
f�B
g8B
gmB
g�B
h>B
h>B
h>B
hsB
h�B
i_B
iDB
i_B
iyB
i�B
i�B
i�B
jKB
jeB
jB
j�B
kB
k6B
kB
k6B
kkB
k�B
k�B
k�B
k�B
lqB
lqB
lqB
lqB
l�B
l�B
m)B
m]B
m�B
m�B
n/B
nIB
nIB
ncB
ncB
n}B
n}B
n}B
o5B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p;B
p!B
p;B
p;B
p;B
p;B
p�B
poB
p�B
p�B
q'B
q'B
q[B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
sMB
s�B
shB
s�B
s�B
shB
s�B
s�B
s�B
s�B
tB
tB
tB
tTB
u%B
uZB
uZB
u�B
vB
v+B
v+B
v�B
v�B
v�B
v�B
wB
w2B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
xB
x8B
xlB
x�B
x�B
x�B
y	B
y$B
x�B
x�B
y$B
y>B
y>B
y>B
yrB
zB
z�B
z�B
{B
{B
{B
{JB
z�B
z�B
{B
{dB
{B
{B
{�B
{�B
{�B
|B
|j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BC�BC�BC�BCaBB�BC�BC�BC�BIB`BxB��B��B�rB	�B	B	AB	�B		RB	
�B	B	 B	�B	B	�B	!B	 BB	�B	�B	kB	1[B	a�B	��B	�B	�}B
p�B
��B
�B
یB
�B
�6B
�BBhB'�Bi_Bp�B�2B��B�B  B �B
�B#�B/5B2�B0�B4�B2�B/B,�B,qB(�B �BuB��B�}B�"B�<B�B��B�QB�
B�?B��B��B�B��BjBT�B>(B(�B�B
�jB
�'B
�~B
�SB
r-B
[=B
B[B
$&B
NB	�B	��B	�9B	�B	w�B	l�B	`�B	JrB	-wB	1B	�B��B�tB�-BևBˬBĶB�BB�OB�/B�B�B�FB�TBݲBרB�FB�MB��B��BбB��B�B	TB	(sB	2|B	3�B	2aB	6B	72B	;�B	?.B	D�B	F�B	NB	Y�B	O�B	U�B	uB	�B	��B	��B	��B	��B	��B	z�B	r-B	o�B	m�B	kQB	ffB	ZB	K)B	E�B	IB	`�B	g�B	[�B	bhB	`�B	\�B	U�B	R�B	L�B	IlB	I�B	J=B	F�B	GzB	DMB	B�B	P}B	R�B	PHB	IRB	G�B	<�B	9>B	<�B	@�B	A�B	I�B	M�B	NpB	OBB	T,B	a|B	f�B	j0B	r�B	v�B	}VB	|�B	��B	�xB	�bB	�NB	��B	�FB	�[B	�eB	��B	��B	��B	��B	��B	�B	�jB	�pB	��B	�HB	�:B	�B	�mB	�*B	�QB	��B	�B	��B	�*B	��B	��B	�	B	�<B	��B	�[B	��B	�BB	��B	�BB	��B	��B	�B	�mB	�B	��B	ݘB	ܬB	�KB	��B	��B	��B	�uB	�TB	��B	ңB	ӏB	�&B	��B	��B	��B	��B	�B	�EB	ݲB	�vB	�B	ߤB	��B	ܒB	ܒB	��B	�B	��B	�HB	��B	�B	��B	��B	��B	�-B	�-B	��B	�B	ܬB	�)B	�7B	�B	��B	�=B	��B	چB	�$B	�NB	��B	ϫB	�.B	�B	�,B	�YB	�mB	ƎB	�lB	�xB	ݘB	ܒB	��B	چB	��B	�_B	�+B	�B	��B	�$B	�SB	�FB	�B	��B	�B	ϑB	�bB	�.B	��B	̈́B	��B	�	B	�_B	�1B	�pB	�aB	��B	�B	�WB	ܒB	�VB	�B	�|B	�B	�B	�B	��B	�B	�ZB	�tB	�B	�@B	�B	�B	�&B	�ZB	�`B	�2B	�B	��B	��B	�B	�0B	�B	�6B	�QB	�WB	�B	�B	�WB	��B	��B	�B	�B	�!B	�oB	��B	�'B	�B	�-B	�-B	�aB	�aB	�B	��B	�MB	�hB	�hB	�B	��B	�B	�B	�9B	�9B	�nB	��B	�B	�?B	�+B	�B	�fB	��B	��B	�B	�8B	�RB	�lB	�	B	�XB	�B	�*B	�^B	��B	��B	�JB	�JB	�JB	��B	�6B	�PB	�B	�(B	�B	�B	�]B	�]B	�HB	�}B
 4B
B
UB
�B
�B
�B
uB
�B
{B
�B
�B
�B
�B
3B
SB
B
�B
+B
B
�B
B
_B
KB
fB
�B
fB
�B
	lB

#B

�B

�B

=B
	�B

=B

�B
)B
�B
JB
�B
B
6B
PB
�B
pB
pB
BB
vB
�B
�B
HB
:B
�B
�B
�B
�B
�B
uB
�B
�B
B
�B
�B
MB
�B
�B
SB
mB

B

B
YB
�B
�B
�B
�B
B
_B
EB
_B
�B
�B
�B
1B
B
1B
KB
�B
�B
B
QB
kB
�B
�B
]B
�B
�B
IB
�B
5B
!B
VB
;B
�B
 B
�B
�B
 vB
!B
!-B
!�B
!�B
"B
"NB
"�B
"�B
"�B
"�B
"�B
# B
#nB
$@B
$�B
%,B
&B
'RB
($B
(sB
(�B
(�B
)*B
)�B
*B
*�B
*�B
*�B
+�B
+�B
,"B
,=B
,=B
,WB
,�B
,qB
,=B
,=B
-�B
-�B
-�B
./B
.cB
.cB
.cB
.�B
/B
/�B
0!B
0�B
0oB
0oB
0�B
1B
1B
0�B
1vB
1vB
1[B
1�B
2�B
2�B
2�B
33B
3hB
3�B
3�B
4B
49B
3�B
3�B
3hB
3�B
3�B
3�B
3�B
3�B
3�B
4B
4�B
4�B
4nB
4�B
5B
5�B
5�B
6zB
7�B
8�B
9	B
9XB
9�B
9�B
9�B
:*B
:*B
:*B
:�B
:�B
;B
:�B
;B
;JB
;dB
;B
;B
;�B
;�B
<B
<6B
<PB
<�B
<�B
<�B
<�B
="B
=�B
>(B
>B
>BB
>BB
>wB
?B
?B
?B
?.B
?.B
?HB
?HB
?HB
?cB
?�B
?�B
@B
@iB
@�B
@�B
AB
A�B
A�B
BB
B'B
B�B
B�B
B�B
CB
CGB
CaB
C�B
C{B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
G+B
F�B
F�B
F%B
FB
E�B
E�B
F?B
F�B
F�B
F�B
GB
G_B
H�B
H�B
H�B
H�B
I�B
I�B
IlB
I�B
IlB
JrB
JrB
JrB
JrB
JrB
K�B
LJB
L~B
L�B
L�B
MB
MjB
M�B
M�B
M�B
N"B
NVB
NVB
N"B
NB
NpB
NpB
N�B
N�B
OB
O(B
OvB
OvB
OBB
OvB
O�B
O�B
O�B
PHB
P�B
Q B
Q�B
RB
RB
RB
R�B
R�B
SB
R�B
R�B
S�B
S�B
S�B
T,B
TaB
T�B
T�B
T�B
U2B
U2B
UB
T�B
U2B
T�B
UB
UgB
UgB
UgB
U�B
VB
VB
VSB
V�B
W$B
W�B
XEB
X+B
XB
X_B
XyB
X�B
YB
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
Z�B
[WB
[�B
[�B
\B
\B
\CB
\]B
\�B
]B
]/B
]IB
]�B
^B
^B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
_;B
_pB
_�B
_�B
`'B
`vB
`\B
`�B
`�B
`�B
`�B
`�B
aB
`�B
`�B
`�B
a�B
a�B
a�B
a�B
b4B
bNB
bhB
b�B
b�B
b�B
c B
c�B
c�B
c�B
c�B
dB
dB
dtB
d�B
d�B
d�B
d�B
d�B
eB
eFB
e`B
ezB
e�B
e�B
e�B
fB
f2B
fLB
f�B
f�B
f�B
g8B
gmB
g�B
h>B
h>B
h>B
hsB
h�B
i_B
iDB
i_B
iyB
i�B
i�B
i�B
jKB
jeB
jB
j�B
kB
k6B
kB
k6B
kkB
k�B
k�B
k�B
k�B
lqB
lqB
lqB
lqB
l�B
l�B
m)B
m]B
m�B
m�B
n/B
nIB
nIB
ncB
ncB
n}B
n}B
n}B
o5B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p;B
p!B
p;B
p;B
p;B
p;B
p�B
poB
p�B
p�B
q'B
q'B
q[B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
sMB
s�B
shB
s�B
s�B
shB
s�B
s�B
s�B
s�B
tB
tB
tB
tTB
u%B
uZB
uZB
u�B
vB
v+B
v+B
v�B
v�B
v�B
v�B
wB
w2B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
xB
x8B
xlB
x�B
x�B
x�B
y	B
y$B
x�B
x�B
y$B
y>B
y>B
y>B
yrB
zB
z�B
z�B
{B
{B
{B
{JB
z�B
z�B
{B
{dB
{B
{B
{�B
{�B
{�B
|B
|j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105225  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191040  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191040  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191040                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041048  QCP$                G�O�G�O�G�O�         208F35EJA  ARGQrqcpc3.6                                                                20220605041048  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151507                      G�O�G�O�G�O�                