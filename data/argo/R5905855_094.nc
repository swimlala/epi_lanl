CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:27:17Z creation;2022-06-04T19:27:17Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192717  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ^A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ّ�rX�&1   @ّ��]L;@->��"���d�j~��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  C   C  C  C  C  C
  C33C�C��C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8�C:�C<�C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf�Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BX\)B`\)Bg��Bo��Bw��B��B���B���B�ǮB���B���B���B���B���B���B���B���B�aGB�aGB�ǮB�ǮB���B���B���B���B���B���B���B���B���B���B���B���B�.B��{B�ǮB���B���C�qC�qC�qC�qC	�qC0�CC�>C��C�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-��C/�qC1�qC3�qC5�qC8C:C<C=�qC?��CA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCdCfCg��Ci�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\Dx�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!��D"�D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<��D<�\D=\D=�\D>x�D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DS�DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~��D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�B�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AתeAש�A׫Aת�A׫kA׬A׬�Aר�Aר�AפAע�Aע4Aפ�Aצ�AקRAצLAצAס�AםAה�AבhA�z�A�G�A��A�9�Aԗ�AӴ�AӅA�C�A��A���AғuA��8A��A�W
AοHAͦ�A��A̰�A��Aɟ�A�^5A�e�A�cA�"hA��A�Q�A�+A���A���A�2�A�_A���A�~(A���A�xA��pA�<A��7A��/A��HA�~A��A��A�v+A�B'A�zxA���A�c�A�=�A�S�A�=A�|�A��:A�d�A�A��0A��uA�K�A�V�A�l�A�R�A��2A�z�A��A��qA���A���A�J#A��XA���A�1'A�7�A�5A�.A�.IA��*A�GEA�!�A{�Az�|Ay%FAsOAn��Al�QAj��Aiu�Ac��AZ2�AT��AP�AL�4AJFAGL0ACffAB�AB�|AA�A@Y�A>�cA>?A=��A;�5A9�A8+�A7dZA6��A5XA4l�A3��A1��A/�uA.��A.n�A-��A-�LA,�A*�qA)�gA(s�A&>BA%�LA%/A$�HA$($A#w�A#�A"�]A".IA!�HA ��AXyA|�A8�AP�AQ�A�.A�!A��A�AQ�A�NA|A�A-Ah
A�nA�<A��AB�A˒A_pA�A�A"�A��AzA�A~�A�AoA�A��A�fA��AK^A��Ab�A?�AuAԕA�An/A;A��AO�A@�A��A��A�A�<AtTAoA�mA�@Ag8A%�A��AĜA�nAJ�A�A�Ax�A<6A�A��Au�A��As�A
�A
I�A	�}A	K�A�AaA#�A��A%A�Ae�A�A�BA�uATaAbA��A�}A�DA?�AA�FA��A\�ATaA iA��A�A��A-�AVA�A� AA A ��A H�@��K@�/�@��@���@�!@�m]@��@�+@���@�~@�@���@�&�@� i@��e@�C�@���@��6@���@��@��@��@���@��@�[�@�@���@�$�@��9@��@�/�@��"@�d�@�$@��@�M@�h@�w2@��5@�4@���@��@�0@��@�@�@�	@琗@��@��@�E9@��@�Xy@��>@�c�@��@��K@⭬@�9X@��@��@�K�@�ѷ@�Ov@߱[@�c�@���@�Ov@���@��@ݏ�@�S@�_@�P�@ک�@ڀ�@�-�@�|@��@؜x@׾w@��@��B@�]d@բ�@�ѷ@�C�@�]�@�$t@Ҭ�@ќ�@�-w@�ߤ@�_�@�!@��@϶F@�u�@�!�@ε�@�q�@��@ͫ�@͏�@�33@���@̺�@�v�@�,=@�l�@�n�@�j@�#:@ɻ0@�+�@ȦL@��@Ǟ�@��@��'@�r�@�-@��a@�x@���@ć�@�1@�o @�(@¾�@�oi@�-@�ݘ@��@�U�@�%@��X@��e@��Y@�V@���@�q@���@�!�@���@�zx@�+@�/�@�O@��@��e@���@�=�@���@�5?@��N@���@�33@�Ɇ@��r@�L0@��.@��@�9�@���@�Ĝ@���@�~(@�?@�s�@��@�~(@�Z@��@��d@�K�@��)@���@�B[@��@�7L@��y@��@��@���@�0�@�
=@�p;@�?@��@���@�U�@��@���@��p@��I@���@�?@�{@��r@��:@���@��@��@�A�@�8�@�o@��9@�-@��}@��'@�f�@��@��Y@�B[@��@��m@���@���@��@��9@�z�@�'R@��r@���@��@���@�e�@���@���@��.@�S�@�4@���@���@�Y�@�ی@��u@�z�@�H@��@�A�@��H@�,=@���@��@�<6@��@�͟@��b@�h�@�0U@���@�o�@���@��?@���@�6@��@��[@��	@�X�@�S@���@�I�@���@�X�@�V@��@��s@���@�]d@�4n@� �@�	�@���@���@��@��8@��H@��r@�	@���@�RT@��]@��@�kQ@�#:@��@��@�c�@��@��$@��4@�q�@�V�@�!�@��a@�j�@�/�@��@���@�=q@�	�@���@��@��X@�p�@�Y�@�8@�ں@��@�9X@��@���@�W?@�V@�֡@���@�u%@�:*@�	@���@�F�@��@��<@��A@�#:@��A@��=@�}�@�l�@�K�@��f@�m�@�,=@��@��@�˒@�W?@�$t@���@���@�YK@�6�@���@�n/@�o@���@�N�@�1@��6@���@�b�@�F@�<6@�4�@�&@�+@��@��2@���@�C-@��@W?@~�@~��@~^5@~J@}zx@}:�@}@|ѷ@|b@{X�@{�@z��@z��@z�L@z�1@z�+@zV@y��@yf�@x�p@x�@wn/@v��@u�d@u��@uj@u	l@tɆ@t�Y@tQ�@t  @s�g@s|�@sY@rYK@qX@p��@pu�@pPH@p	�@o�f@o�@n�@n͟@n($@mY�@l��@k��@j��@jl�@i��@ia�@i<6@i<6@i2a@i�@i;@h��@hD�@h@gƨ@g|�@gS@fv�@f8�@f	@ew2@c�]@cv`@c'�@b��@bC�@a��@aj@a=�@aV@`��@`��@`�@_��@_b�@_>�@^��@^3�@]ԕ@]Y�@\�|@\]d@[�A@[�k@[�@Zߤ@Z�\@Z{@Y��@YA @Y	l@X�K@X�D@W��@W{J@WX�@WS@Vl�@U�Z@U�=@Uk�@UN<@T��@TN�@T9X@S��@S�0@S��@S��@SC�@S�@Ru%@R@�@R�@Q�t@QL�@Qq@P�@P �@O�K@O��@O�@Nq�@M��@MT�@M%@L�@L%�@Kƨ@K��@Kb�@J�c@J{�@I�d@IO�@H�	@H�)@H��@Hq@H~@G�}@G�4@GA�@G i@F�,@F�+@Fd�@F�@E�t@E5�@D��@Dl"@DS�@D'R@C�F@C6z@B�'@B��@B1�@A�@A�@@H@@*�@@�@?��@?��@?��@?RT@?)_@>ȴ@>�\@=��@=k�@=5�@<��@<��@<�@<Z@<-�@;�P@;H�@;1�@;C@:ߤ@:��@:+k@9�#@9�=@9?}@8�p@8e�@8x@7��@7X�@6�R@6�r@6!�@5�7@5�@4�E@4�@3��@3�V@3S�@3
=@2=q@1��@1�9@1�t@1k�@1�@0��@0�@0:�@/��@/U�@/;d@/�@.�h@.)�@-��@-��@-a�@-+�@,�	@,�@,�o@,U2@,  @+��@+]�@+J#@+6z@+)_@+$t@+C@*�8@*͟@*��@*3�@)�T@)�n@)o @)m]@)hs@)&�@(�@(��@(/�@(M@(b@(�@'�;@'��@'s@'A�@' i@&��@&�+@&=q@&�@%�9@%�@%@@$�@$y>@#�@#y�@#a@#J#@#=@#,�@#�@#@#�@"�"@"�y@"��@"��@"�x@"ff@!��@!!�@ �)@ ��@ �_@ q@ D�@ 	�@��@��@�{@y�@�@�h@��@Q@.�@�@�o@��@k�@S&@:�@��@�.@�o@Q�@ �@1@�}@��@e�@ߤ@�\@B[@�@�@�@o @=�@!�@�|@��@��@bN@*�@�@�r@��@"�@�y@��@�1@��@�F@}V@�.@�=@��@o @(�@�@�P@��@Ĝ@�O@[�@��@E9@ i@�s@�b@ff@R�@0U@��@�M@p�@:�@��@��@�I@tT@H@6@%�@ݘ@l�@��@�@}V@n�@l�@@�@�@�^@hs@�@�f@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AתeAש�A׫Aת�A׫kA׬A׬�Aר�Aר�AפAע�Aע4Aפ�Aצ�AקRAצLAצAס�AםAה�AבhA�z�A�G�A��A�9�Aԗ�AӴ�AӅA�C�A��A���AғuA��8A��A�W
AοHAͦ�A��A̰�A��Aɟ�A�^5A�e�A�cA�"hA��A�Q�A�+A���A���A�2�A�_A���A�~(A���A�xA��pA�<A��7A��/A��HA�~A��A��A�v+A�B'A�zxA���A�c�A�=�A�S�A�=A�|�A��:A�d�A�A��0A��uA�K�A�V�A�l�A�R�A��2A�z�A��A��qA���A���A�J#A��XA���A�1'A�7�A�5A�.A�.IA��*A�GEA�!�A{�Az�|Ay%FAsOAn��Al�QAj��Aiu�Ac��AZ2�AT��AP�AL�4AJFAGL0ACffAB�AB�|AA�A@Y�A>�cA>?A=��A;�5A9�A8+�A7dZA6��A5XA4l�A3��A1��A/�uA.��A.n�A-��A-�LA,�A*�qA)�gA(s�A&>BA%�LA%/A$�HA$($A#w�A#�A"�]A".IA!�HA ��AXyA|�A8�AP�AQ�A�.A�!A��A�AQ�A�NA|A�A-Ah
A�nA�<A��AB�A˒A_pA�A�A"�A��AzA�A~�A�AoA�A��A�fA��AK^A��Ab�A?�AuAԕA�An/A;A��AO�A@�A��A��A�A�<AtTAoA�mA�@Ag8A%�A��AĜA�nAJ�A�A�Ax�A<6A�A��Au�A��As�A
�A
I�A	�}A	K�A�AaA#�A��A%A�Ae�A�A�BA�uATaAbA��A�}A�DA?�AA�FA��A\�ATaA iA��A�A��A-�AVA�A� AA A ��A H�@��K@�/�@��@���@�!@�m]@��@�+@���@�~@�@���@�&�@� i@��e@�C�@���@��6@���@��@��@��@���@��@�[�@�@���@�$�@��9@��@�/�@��"@�d�@�$@��@�M@�h@�w2@��5@�4@���@��@�0@��@�@�@�	@琗@��@��@�E9@��@�Xy@��>@�c�@��@��K@⭬@�9X@��@��@�K�@�ѷ@�Ov@߱[@�c�@���@�Ov@���@��@ݏ�@�S@�_@�P�@ک�@ڀ�@�-�@�|@��@؜x@׾w@��@��B@�]d@բ�@�ѷ@�C�@�]�@�$t@Ҭ�@ќ�@�-w@�ߤ@�_�@�!@��@϶F@�u�@�!�@ε�@�q�@��@ͫ�@͏�@�33@���@̺�@�v�@�,=@�l�@�n�@�j@�#:@ɻ0@�+�@ȦL@��@Ǟ�@��@��'@�r�@�-@��a@�x@���@ć�@�1@�o @�(@¾�@�oi@�-@�ݘ@��@�U�@�%@��X@��e@��Y@�V@���@�q@���@�!�@���@�zx@�+@�/�@�O@��@��e@���@�=�@���@�5?@��N@���@�33@�Ɇ@��r@�L0@��.@��@�9�@���@�Ĝ@���@�~(@�?@�s�@��@�~(@�Z@��@��d@�K�@��)@���@�B[@��@�7L@��y@��@��@���@�0�@�
=@�p;@�?@��@���@�U�@��@���@��p@��I@���@�?@�{@��r@��:@���@��@��@�A�@�8�@�o@��9@�-@��}@��'@�f�@��@��Y@�B[@��@��m@���@���@��@��9@�z�@�'R@��r@���@��@���@�e�@���@���@��.@�S�@�4@���@���@�Y�@�ی@��u@�z�@�H@��@�A�@��H@�,=@���@��@�<6@��@�͟@��b@�h�@�0U@���@�o�@���@��?@���@�6@��@��[@��	@�X�@�S@���@�I�@���@�X�@�V@��@��s@���@�]d@�4n@� �@�	�@���@���@��@��8@��H@��r@�	@���@�RT@��]@��@�kQ@�#:@��@��@�c�@��@��$@��4@�q�@�V�@�!�@��a@�j�@�/�@��@���@�=q@�	�@���@��@��X@�p�@�Y�@�8@�ں@��@�9X@��@���@�W?@�V@�֡@���@�u%@�:*@�	@���@�F�@��@��<@��A@�#:@��A@��=@�}�@�l�@�K�@��f@�m�@�,=@��@��@�˒@�W?@�$t@���@���@�YK@�6�@���@�n/@�o@���@�N�@�1@��6@���@�b�@�F@�<6@�4�@�&@�+@��@��2@���@�C-@��@W?@~�@~��@~^5@~J@}zx@}:�@}@|ѷ@|b@{X�@{�@z��@z��@z�L@z�1@z�+@zV@y��@yf�@x�p@x�@wn/@v��@u�d@u��@uj@u	l@tɆ@t�Y@tQ�@t  @s�g@s|�@sY@rYK@qX@p��@pu�@pPH@p	�@o�f@o�@n�@n͟@n($@mY�@l��@k��@j��@jl�@i��@ia�@i<6@i<6@i2a@i�@i;@h��@hD�@h@gƨ@g|�@gS@fv�@f8�@f	@ew2@c�]@cv`@c'�@b��@bC�@a��@aj@a=�@aV@`��@`��@`�@_��@_b�@_>�@^��@^3�@]ԕ@]Y�@\�|@\]d@[�A@[�k@[�@Zߤ@Z�\@Z{@Y��@YA @Y	l@X�K@X�D@W��@W{J@WX�@WS@Vl�@U�Z@U�=@Uk�@UN<@T��@TN�@T9X@S��@S�0@S��@S��@SC�@S�@Ru%@R@�@R�@Q�t@QL�@Qq@P�@P �@O�K@O��@O�@Nq�@M��@MT�@M%@L�@L%�@Kƨ@K��@Kb�@J�c@J{�@I�d@IO�@H�	@H�)@H��@Hq@H~@G�}@G�4@GA�@G i@F�,@F�+@Fd�@F�@E�t@E5�@D��@Dl"@DS�@D'R@C�F@C6z@B�'@B��@B1�@A�@A�@@H@@*�@@�@?��@?��@?��@?RT@?)_@>ȴ@>�\@=��@=k�@=5�@<��@<��@<�@<Z@<-�@;�P@;H�@;1�@;C@:ߤ@:��@:+k@9�#@9�=@9?}@8�p@8e�@8x@7��@7X�@6�R@6�r@6!�@5�7@5�@4�E@4�@3��@3�V@3S�@3
=@2=q@1��@1�9@1�t@1k�@1�@0��@0�@0:�@/��@/U�@/;d@/�@.�h@.)�@-��@-��@-a�@-+�@,�	@,�@,�o@,U2@,  @+��@+]�@+J#@+6z@+)_@+$t@+C@*�8@*͟@*��@*3�@)�T@)�n@)o @)m]@)hs@)&�@(�@(��@(/�@(M@(b@(�@'�;@'��@'s@'A�@' i@&��@&�+@&=q@&�@%�9@%�@%@@$�@$y>@#�@#y�@#a@#J#@#=@#,�@#�@#@#�@"�"@"�y@"��@"��@"�x@"ff@!��@!!�@ �)@ ��@ �_@ q@ D�@ 	�@��@��@�{@y�@�@�h@��@Q@.�@�@�o@��@k�@S&@:�@��@�.@�o@Q�@ �@1@�}@��@e�@ߤ@�\@B[@�@�@�@o @=�@!�@�|@��@��@bN@*�@�@�r@��@"�@�y@��@�1@��@�F@}V@�.@�=@��@o @(�@�@�P@��@Ĝ@�O@[�@��@E9@ i@�s@�b@ff@R�@0U@��@�M@p�@:�@��@��@�I@tT@H@6@%�@ݘ@l�@��@�@}V@n�@l�@@�@�@�^@hs@�@�f@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�RB
�RB
�B
�B
��B
��B
�B
�B
�B
�B
�B
�mB
�RB
�mB
�mB
�mB
�B
�B
�B
�B
�B
�B
�$B
��B
�YB
�%B
�B
�B
��B
��B
��B
��B
�fB
��B
��B
�B
�pB
��B
��B
��B
�]B
��B
��B
�EB
��B
��B
�B
�XB
��B�B\B�B7B,qB4BK�BVBa-Bk�BnIB��B�VB��B��B�AB��B�mB�B��B��BרB�?B�+B�B�B�B�sB�"BðB��B��B�B��B_pB4�B;B
��B
�zB
�0B
��B
��B
�{B
tnB
VB
:B
&�B
jB
�B	�?B	�WB	�IB	ªB	��B	�dB	�IB	��B	�nB	t�B	X+B	EB	+�B	�B	 B	�B	 �B	B	&�B	2�B	:�B	8�B	6�B	2|B	2aB	>BB	D�B	K�B	Y�B	gmB	qAB	�4B	�B	��B	��B	��B	��B	�$B	�jB	��B	ڠB	�NB	ңB	ңB	� B	� B	�6B	�B	�eB	�#B	��B	��B	ݲB	��B	��B	��B	��B	�B
�B
�B
BB
�B
�B
+B
FB
B
�B
�B
(B
EB
~B
�B
$B
%�B
*eB
0�B
1B
1�B
1AB
4nB
6�B
8B
:�B
=VB
>�B
CB
@�B
=qB
<�B
="B
>�B
@�B
CB
AUB
@OB
>wB
>(B
?}B
?B
>]B
>B
=�B
=qB
=�B
>BB
>BB
>wB
>�B
>�B
>]B
>�B
>�B
>wB
>wB
>�B
>�B
?�B
@ B
@�B
@�B
>�B
<�B
:�B
;�B
;�B
<6B
:�B
9�B
8B
6`B
5ZB
5tB
5�B
6FB
6FB
5�B
4�B
5%B
4�B
4nB
3�B
2|B
2GB
1AB
1AB
0�B
1�B
1�B
1�B
1[B
1�B
1[B
1AB
0�B
/�B
/5B
-wB
,"B
+�B
+B
*�B
)�B
)_B
(�B
(>B
(
B
&�B
&�B
%FB
$�B
#�B
#B
"B
!�B
!�B
 �B
pB
�B
�B
+B
�B
�B
eB
B
�B
7B
QB
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
{B
�B
gB
FB
 B
<B
pB
�B
�B
B
�B
�B
xB
JB
�B
�B
�B
B
B
�B
�B
DB
�B
�B

�B

	B
B

�B

�B

XB

	B
	lB
�B
fB
	B

�B
)B

�B

�B

�B
	�B
�B
�B
	7B
�B
�B
�B
�B
3B
�B
�B
�B
�B
�B
�B
{B
�B
B
aB
aB
�B
�B
AB
�B
�B
�B
B
�B
B
3B
�B
gB
�B
�B
�B
-B
�B
[B
'B
�B
�B
�B
B
MB
�B
B
3B
�B
�B
�B
�B
AB
�B
-B
�B
�B
�B
{B
�B
AB
�B
 B
 B
 4B	��B	��B
 4B
�B
�B
�B
mB
�B
3B
9B
�B
�B
�B
3B
�B
3B
�B
gB
�B
mB
mB
+B
�B
�B
�B
fB
fB
�B
	�B

	B

rB
	�B
	�B
	�B

#B

XB

�B

�B

�B

�B
)B
^B
B
xB
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
PB
�B
6B
�B
�B
�B
�B
�B
�B
B
B
6B
�B
�B
"B
<B
pB
�B
�B
�B
�B
HB
 B
4B
:B
:B
�B
�B
B
FB
{B
�B
�B
MB
�B
�B
SB
9B
B
B
�B
�B
�B
YB
$B
�B
�B
�B
�B
�B
sB
sB
�B
YB
sB
�B
�B
�B
�B
�B
�B
�B
+B
EB
�B
�B
1B
�B
QB
�B
�B
WB
qB
qB
�B
�B
CB
�B
�B
�B
dB
5B
�B
�B
�B
�B
�B
 BB
 BB
 �B
!B
!|B
!�B
!�B
"B
!�B
"NB
"�B
#B
#:B
# B
#�B
$tB
$tB
$�B
$�B
$�B
$�B
$�B
%,B
%�B
&B
&LB
&�B
&�B
'B
'8B
'RB
'�B
'�B
($B
(>B
(�B
)B
)�B
)�B
*B
*B
*B
*�B
*�B
+B
+B
+QB
,B
,WB
,qB
,�B
,qB
-wB
-]B
-�B
-�B
.cB
.}B
.�B
/OB
/�B
0�B
1B
1AB
1AB
1vB
1�B
2-B
2GB
2GB
2GB
2GB
2GB
2|B
2�B
3MB
3�B
4B
4�B
5B
5B
5ZB
5�B
5�B
5�B
5�B
6FB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7LB
7LB
7�B
8B
8B
8�B
8�B
8�B
9	B
9>B
9XB
9rB
9rB
9�B
9�B
9�B
9�B
:�B
;0B
;B
;�B
;�B
;�B
<PB
<�B
<PB
<6B
<�B
<�B
=qB
>]B
>�B
?B
?.B
?�B
?}B
?�B
?}B
?�B
?�B
@ B
@B
@4B
@�B
@�B
@�B
A B
AB
@�B
AoB
B�B
B�B
CaB
C{B
C�B
DMB
D�B
D�B
EB
D�B
D�B
ESB
FB
FB
F?B
F�B
F�B
F�B
G+B
G_B
GzB
G�B
G�B
H�B
I�B
I�B
J#B
JXB
J�B
J�B
J�B
KB
KxB
KxB
KDB
K^B
K�B
LB
L�B
MB
L�B
MPB
M6B
M6B
MB
MB
MB
M�B
M�B
M�B
N"B
NpB
N�B
N�B
N�B
N�B
NpB
N�B
NpB
NVB
N�B
N�B
O�B
O�B
PB
PHB
P}B
P�B
P�B
P�B
Q B
Q B
Q�B
Q�B
Q�B
Q�B
RB
R:B
RoB
R�B
R�B
R�B
S&B
S[B
SuB
S�B
TB
TaB
T�B
UB
UB
UB
U2B
U�B
U�B
VB
VB
VSB
VSB
W
B
WYB
WYB
WsB
W�B
W�B
W�B
W�B
XB
W�B
X+B
X�B
Y1B
YKB
YB
Y�B
Y�B
Y�B
Y�B
Z�B
ZkB
ZkB
ZkB
Z�B
Z�B
[	B
[WB
[qB
[�B
\B
\]B
\�B
\�B
]B
]~B
]�B
]�B
^B
^jB
^jB
^�B
_B
_VB
_�B
_�B
`�B
aB
`�B
`�B
a|B
bhB
b�B
b�B
b�B
c�B
c�B
c�B
c�B
dtB
d�B
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
fB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
f�B
gRB
gRB
g�B
g�B
g�B
h
B
h>B
hXB
h�B
iB
i*B
h�B
iB
iDB
iyB
i�B
i�B
i�B
i�B
j0B
jeB
jB
j�B
j�B
k6B
kQB
k�B
lB
l"B
l"B
l=B
l=B
l=B
lqB
lqB
lqB
lqB
l�B
l�B
l�B
l�B
l�B
m)B
m�B
nIB
nIB
ncB
ncB
n}B
n}B
n}B
n�B
n�B
n�B
o B
o5B
o�B
o�B
o�B
o�B
o�B
p;B
pUB
p;B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q'B
qAB
q�B
q�B
rGB
rGB
raB
r�B
r�B
sB
s3B
shB
s�B
shB
s�B
tB
tB
tB
tTB
t�B
u%B
utB
utB
utB
utB
utB
vB
v`B
v`B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
w2B
w�B
xRB
x�B
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
y�B
y�B
z*B
z^B
z^B
zDB
z^B
zDB
zxB
z�B
{dB
{dB
|B
|B
{�B
|B
|jB
|�B
}B
}VB
}VB
}V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�RB
�RB
�B
�B
��B
��B
�B
�B
�B
�B
�B
�mB
�RB
�mB
�mB
�mB
�B
�B
�B
�B
�B
�B
�$B
��B
�YB
�%B
�B
�B
��B
��B
��B
��B
�fB
��B
��B
�B
�pB
��B
��B
��B
�]B
��B
��B
�EB
��B
��B
�B
�XB
��B�B\B�B7B,qB4BK�BVBa-Bk�BnIB��B�VB��B��B�AB��B�mB�B��B��BרB�?B�+B�B�B�B�sB�"BðB��B��B�B��B_pB4�B;B
��B
�zB
�0B
��B
��B
�{B
tnB
VB
:B
&�B
jB
�B	�?B	�WB	�IB	ªB	��B	�dB	�IB	��B	�nB	t�B	X+B	EB	+�B	�B	 B	�B	 �B	B	&�B	2�B	:�B	8�B	6�B	2|B	2aB	>BB	D�B	K�B	Y�B	gmB	qAB	�4B	�B	��B	��B	��B	��B	�$B	�jB	��B	ڠB	�NB	ңB	ңB	� B	� B	�6B	�B	�eB	�#B	��B	��B	ݲB	��B	��B	��B	��B	�B
�B
�B
BB
�B
�B
+B
FB
B
�B
�B
(B
EB
~B
�B
$B
%�B
*eB
0�B
1B
1�B
1AB
4nB
6�B
8B
:�B
=VB
>�B
CB
@�B
=qB
<�B
="B
>�B
@�B
CB
AUB
@OB
>wB
>(B
?}B
?B
>]B
>B
=�B
=qB
=�B
>BB
>BB
>wB
>�B
>�B
>]B
>�B
>�B
>wB
>wB
>�B
>�B
?�B
@ B
@�B
@�B
>�B
<�B
:�B
;�B
;�B
<6B
:�B
9�B
8B
6`B
5ZB
5tB
5�B
6FB
6FB
5�B
4�B
5%B
4�B
4nB
3�B
2|B
2GB
1AB
1AB
0�B
1�B
1�B
1�B
1[B
1�B
1[B
1AB
0�B
/�B
/5B
-wB
,"B
+�B
+B
*�B
)�B
)_B
(�B
(>B
(
B
&�B
&�B
%FB
$�B
#�B
#B
"B
!�B
!�B
 �B
pB
�B
�B
+B
�B
�B
eB
B
�B
7B
QB
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
{B
�B
gB
FB
 B
<B
pB
�B
�B
B
�B
�B
xB
JB
�B
�B
�B
B
B
�B
�B
DB
�B
�B

�B

	B
B

�B

�B

XB

	B
	lB
�B
fB
	B

�B
)B

�B

�B

�B
	�B
�B
�B
	7B
�B
�B
�B
�B
3B
�B
�B
�B
�B
�B
�B
{B
�B
B
aB
aB
�B
�B
AB
�B
�B
�B
B
�B
B
3B
�B
gB
�B
�B
�B
-B
�B
[B
'B
�B
�B
�B
B
MB
�B
B
3B
�B
�B
�B
�B
AB
�B
-B
�B
�B
�B
{B
�B
AB
�B
 B
 B
 4B	��B	��B
 4B
�B
�B
�B
mB
�B
3B
9B
�B
�B
�B
3B
�B
3B
�B
gB
�B
mB
mB
+B
�B
�B
�B
fB
fB
�B
	�B

	B

rB
	�B
	�B
	�B

#B

XB

�B

�B

�B

�B
)B
^B
B
xB
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
PB
�B
6B
�B
�B
�B
�B
�B
�B
B
B
6B
�B
�B
"B
<B
pB
�B
�B
�B
�B
HB
 B
4B
:B
:B
�B
�B
B
FB
{B
�B
�B
MB
�B
�B
SB
9B
B
B
�B
�B
�B
YB
$B
�B
�B
�B
�B
�B
sB
sB
�B
YB
sB
�B
�B
�B
�B
�B
�B
�B
+B
EB
�B
�B
1B
�B
QB
�B
�B
WB
qB
qB
�B
�B
CB
�B
�B
�B
dB
5B
�B
�B
�B
�B
�B
 BB
 BB
 �B
!B
!|B
!�B
!�B
"B
!�B
"NB
"�B
#B
#:B
# B
#�B
$tB
$tB
$�B
$�B
$�B
$�B
$�B
%,B
%�B
&B
&LB
&�B
&�B
'B
'8B
'RB
'�B
'�B
($B
(>B
(�B
)B
)�B
)�B
*B
*B
*B
*�B
*�B
+B
+B
+QB
,B
,WB
,qB
,�B
,qB
-wB
-]B
-�B
-�B
.cB
.}B
.�B
/OB
/�B
0�B
1B
1AB
1AB
1vB
1�B
2-B
2GB
2GB
2GB
2GB
2GB
2|B
2�B
3MB
3�B
4B
4�B
5B
5B
5ZB
5�B
5�B
5�B
5�B
6FB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7LB
7LB
7�B
8B
8B
8�B
8�B
8�B
9	B
9>B
9XB
9rB
9rB
9�B
9�B
9�B
9�B
:�B
;0B
;B
;�B
;�B
;�B
<PB
<�B
<PB
<6B
<�B
<�B
=qB
>]B
>�B
?B
?.B
?�B
?}B
?�B
?}B
?�B
?�B
@ B
@B
@4B
@�B
@�B
@�B
A B
AB
@�B
AoB
B�B
B�B
CaB
C{B
C�B
DMB
D�B
D�B
EB
D�B
D�B
ESB
FB
FB
F?B
F�B
F�B
F�B
G+B
G_B
GzB
G�B
G�B
H�B
I�B
I�B
J#B
JXB
J�B
J�B
J�B
KB
KxB
KxB
KDB
K^B
K�B
LB
L�B
MB
L�B
MPB
M6B
M6B
MB
MB
MB
M�B
M�B
M�B
N"B
NpB
N�B
N�B
N�B
N�B
NpB
N�B
NpB
NVB
N�B
N�B
O�B
O�B
PB
PHB
P}B
P�B
P�B
P�B
Q B
Q B
Q�B
Q�B
Q�B
Q�B
RB
R:B
RoB
R�B
R�B
R�B
S&B
S[B
SuB
S�B
TB
TaB
T�B
UB
UB
UB
U2B
U�B
U�B
VB
VB
VSB
VSB
W
B
WYB
WYB
WsB
W�B
W�B
W�B
W�B
XB
W�B
X+B
X�B
Y1B
YKB
YB
Y�B
Y�B
Y�B
Y�B
Z�B
ZkB
ZkB
ZkB
Z�B
Z�B
[	B
[WB
[qB
[�B
\B
\]B
\�B
\�B
]B
]~B
]�B
]�B
^B
^jB
^jB
^�B
_B
_VB
_�B
_�B
`�B
aB
`�B
`�B
a|B
bhB
b�B
b�B
b�B
c�B
c�B
c�B
c�B
dtB
d�B
d�B
d�B
d�B
e,B
eFB
ezB
e�B
e�B
fB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
f�B
gRB
gRB
g�B
g�B
g�B
h
B
h>B
hXB
h�B
iB
i*B
h�B
iB
iDB
iyB
i�B
i�B
i�B
i�B
j0B
jeB
jB
j�B
j�B
k6B
kQB
k�B
lB
l"B
l"B
l=B
l=B
l=B
lqB
lqB
lqB
lqB
l�B
l�B
l�B
l�B
l�B
m)B
m�B
nIB
nIB
ncB
ncB
n}B
n}B
n}B
n�B
n�B
n�B
o B
o5B
o�B
o�B
o�B
o�B
o�B
p;B
pUB
p;B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q'B
qAB
q�B
q�B
rGB
rGB
raB
r�B
r�B
sB
s3B
shB
s�B
shB
s�B
tB
tB
tB
tTB
t�B
u%B
utB
utB
utB
utB
utB
vB
v`B
v`B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
w2B
w�B
xRB
x�B
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
y�B
y�B
z*B
z^B
z^B
zDB
z^B
zDB
zxB
z�B
{dB
{dB
|B
|B
{�B
|B
|jB
|�B
}B
}VB
}VB
}V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105247  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192717  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192717  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192717                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042725  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042725  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                