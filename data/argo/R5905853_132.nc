CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-09-20T06:48:48Z creation;2022-09-20T06:48:49Z conversion to V3.1      
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220920064848  20220920070032  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��\Y�T1   @��֩&N@-��Q��co��S��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B_��Bg��Bp  BxffB�ffB���B�33B���B���B���B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C!�fC#�fC&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@\)@��@��A!p�A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW�]B_�]Bg�]Bo��Bx\)B�aGB�ǮB�.B��{B�ǮB�ǮB���B���B���B���B�.B�ǮB���B���B���B���B���B���B���B�ǮB���B���B���B���B���B���B���B���B���B���B�.B���B���CC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qCC�qC!��C#��C%�qC'�qC)��C+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC`Ca�qCc�qCe�qCg�qCjCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D�{D���D�?�D��D���D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A� �A�%FA��A�oA�GA���A��%A���Aٳ�Aٔ{A�}�A�j�A�aA�U�A�J�A�;dA�,=A�1A��<A��&A��AA��mA���A֑4A���A�E�A͇�A�m]A���A�Z�A�B'A��HA���A�m�AŞ�A�p�A��?A�.A���A��UA���A��A���A�&�A��gA��fA�	lA�?HA�>�A���A��A�(A��_A�՛A�c�A�v�A��A���A��KA���A�-wA��ZA�D3A��2A�~A�%�A���A��A�8�A�FA�6FA��4A�K�A���A�[�A���A�
	A�|�A��A}bAw�bAtO�Ar��Am4Al  Ajg�AhaAe� A_j�A\��AY��AT�=ARo�AP��AO2aAM�AI��AG�AE��AC�AB	lABQAA�cA@^5A>^5A=��A=($A<��A;0�A9N�A8�A6W�A40�A3?�A2��A2
=A0~�A/�PA/i�A-�A*��A)�QA(��A(B�A'�A&G�A%o A$H�A!A��A�_AjA�A�6A5�AJA \AzxA|�AcA1�A��A2�A�AM�A�AkQAq�A_pA}�A
�A�A�qAm]AFtA�#A�!Al�A8�A�,A�$A�AFtA��A��A1'Aa�A�5Au�A��A�EA�A�A��A|�Ae�A�VA|A
��A
Z�A
�A
*0A
J#A	�~A��Au�A0�A�0A�Al"A�A��A�Ar�AN�A!�A�AS&A��A!-ADgA�8A�SAy>AA A �A L�A �@��@��R@��N@�C-@��H@��h@���@�	�@���@��@�z�@�C@��>@�o @� i@�Q@�@�[@�Ov@�1@��@�;@��@��@�.�@��N@�b�@�@�t@���@�?�@��@�=�@�8@罥@�h
@���@�'@��@�_@�@��[@�j@�h@��f@�J�@�X�@�8�@ݤ@@�@���@�oi@�
=@ؔF@�L0@׭C@�s�@�?}@֌@�D�@Ճ{@�oi@�	@�C�@қ�@�c@�@��f@�N<@��@Ϸ@��@λ�@�Ta@�@�g�@̧�@��@�4@�N�@��@ɔ�@�a�@���@��@ʅ�@ɋ�@�}V@�Q�@�)�@��m@�A�@���@Ƶ@Š'@�a�@��@�0�@���@��@�zx@�j@�?}@���@��@ �@�^5@���@���@���@�3�@�~�@��@���@�;d@��k@��@�J�@��m@�Y�@���@�3�@�S&@��@��)@��@@� \@�GE@��}@�O�@�	l@��`@���@�YK@���@�B�@�)_@��f@��\@�,=@��@���@��@�Y�@�%F@���@��?@���@�	@���@�G�@�@�҉@�O@��m@��-@�S�@�Dg@�C�@�F�@�H�@�:�@��9@���@��@��x@�r�@�"h@��3@���@��@���@��@�W?@���@�'R@��	@�"�@��u@��@��C@�s@��@��@��@�\�@�u@��@�4@�ѷ@��j@���@���@���@�v�@�\�@��@�.I@���@��j@�e�@��@��^@�qv@�=�@� \@��@��f@���@��@��z@���@�t�@��@��@��O@�/�@��K@�s�@�&�@���@�z�@��@��q@�dZ@�@@��@�U2@�$@�	�@���@��S@�j@��@�͟@�V�@��@��[@�{J@�]�@�Dg@���@���@��u@�oi@�Q@�$�@���@���@�\)@���@���@���@�h
@��@� �@���@�@��4@�^�@�&@��@�u�@�=q@��z@��	@��@�/�@��}@�(�@��@���@���@�}�@��@�d�@�7@���@��@�IR@���@���@��u@�{�@�`�@�x@���@�zx@���@�Ɇ@�Z�@�@���@���@��M@�N<@�Y@���@���@�V�@�!@���@�X@�.I@��@��@���@�z�@�1'@��A@�ϫ@�@��@�>�@��@���@��p@���@��.@�z�@�V�@�2�@��]@��t@��~@�o�@�J#@��@���@��\@�{�@�W�@�H�@�{@�F@33@~��@~�h@~�x@~��@~GE@}�@}�@}��@}@@|�[@|�D@|h�@|  @{/�@z�'@z��@z��@z��@zJ@yj@x�|@xZ@x1@w�f@vc @u��@u/@t�?@tG@sn/@rȴ@rE�@rE�@r-@q�@p��@p7�@o�}@o��@o$t@n�m@n@�@m�=@mB�@l�U@l��@k�@k�]@k��@kY@j\�@i��@ij@i#�@h��@hu�@g�:@g&@f�]@f��@f�@e�9@eG�@d��@d~(@dH@c�@cJ#@b�F@b:*@b{@a�@aq@`�?@`1'@_��@_�@_/�@_S@^҉@^��@^u%@^�@]��@\��@\Xy@\(�@[خ@[A�@Z҉@Z�@Z��@Y��@Y��@Y��@Y5�@X�p@X'R@W�g@W�@W+@V�@Vl�@U�3@U�@T��@T�@S��@S33@R�!@R�@R�@Q�^@Q`B@Q%@P�9@Py>@PM@O�@Oy�@N�@N�x@NR�@N5?@N4@M�)@M@Mf�@M[W@MX@MJ�@M%F@L�@L�?@L2�@K�g@K�w@K�f@KK�@KF�@J�@J^5@J
�@I��@I(�@H�@H��@H�.@H]d@H!@H  @G��@G\)@GE9@F�y@F��@F_�@E�Z@E��@E�@E(�@D��@D�@Dx@C��@C9�@C�@B��@B�+@BZ�@B	@A�N@A��@AO�@@��@@S�@?�w@?A�@>�m@=��@=�"@=+�@<�|@<��@<*�@;��@;�@:=q@9�@9N<@8�p@8��@8x@7��@7�@6�'@6ff@6{@5�D@5�@5�@5��@5u�@58�@4�@4~(@4H@3��@3�@3�@2�h@2~�@2u%@2q�@2H�@1��@1�'@0�v@0]d@0�@/�
@/��@/�$@/e�@.�8@.�@.��@.3�@-�@-��@-��@-|@-J�@,��@,|�@,�@+��@+��@+o�@+_p@+@*� @*M�@*O@)�@)Q�@(��@(�D@(N�@'��@'��@'�f@'e�@'+@&��@&��@&��@&�A@&@%�@%�7@%x�@%?}@%�@$��@$ی@$�?@$�U@$e�@$@#�W@#�}@#��@#��@#�@#~�@#j�@#a@#H�@"�@"�'@"�@"�\@"�\@"z@"B[@!�Z@!��@!��@ H@ -�@ �@�+@�@�g@�k@�k@x@1�@c @&�@��@T�@�@�/@��@��@z�@g8@Ft@�@s@Z�@]�@_p@O@@O@"�@�B@c @!�@��@�@�d@��@ \@��@1'@�]@��@��@o�@@O@�@�c@�X@��@^5@R�@GE@4@�C@\�@+@�@��@K^@�@��@a@"�@��@��@V@�D@�#@�X@��@p�@`B@�@��@?�@!@�r@��@ݘ@�$@qv@S�@�@�y@҉@�X@�h@��@�\@~�@u%@v�@s�@@�@j@Q�@J�@%F@�v@��@�u@Xy@�@�@�K@�[@��@~�@+@�@
��@
ں@
҉@
�m@
��@
�'@
�@
Q@
e@	�)@	��@	�@	��@	�Z@	�@	�S@	rG@	IR@	*0@	@@��@ѷ@�e@K^@(�@'R@ �@1@  @��@��@ݘ@��@o�@W?@O@8@�@��@�s@�X@��@s�@5?@	@	@�@�n@��@�S@��@zx@s�@c�@B�@q@�f@�51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A� �A�%FA��A�oA�GA���A��%A���Aٳ�Aٔ{A�}�A�j�A�aA�U�A�J�A�;dA�,=A�1A��<A��&A��AA��mA���A֑4A���A�E�A͇�A�m]A���A�Z�A�B'A��HA���A�m�AŞ�A�p�A��?A�.A���A��UA���A��A���A�&�A��gA��fA�	lA�?HA�>�A���A��A�(A��_A�՛A�c�A�v�A��A���A��KA���A�-wA��ZA�D3A��2A�~A�%�A���A��A�8�A�FA�6FA��4A�K�A���A�[�A���A�
	A�|�A��A}bAw�bAtO�Ar��Am4Al  Ajg�AhaAe� A_j�A\��AY��AT�=ARo�AP��AO2aAM�AI��AG�AE��AC�AB	lABQAA�cA@^5A>^5A=��A=($A<��A;0�A9N�A8�A6W�A40�A3?�A2��A2
=A0~�A/�PA/i�A-�A*��A)�QA(��A(B�A'�A&G�A%o A$H�A!A��A�_AjA�A�6A5�AJA \AzxA|�AcA1�A��A2�A�AM�A�AkQAq�A_pA}�A
�A�A�qAm]AFtA�#A�!Al�A8�A�,A�$A�AFtA��A��A1'Aa�A�5Au�A��A�EA�A�A��A|�Ae�A�VA|A
��A
Z�A
�A
*0A
J#A	�~A��Au�A0�A�0A�Al"A�A��A�Ar�AN�A!�A�AS&A��A!-ADgA�8A�SAy>AA A �A L�A �@��@��R@��N@�C-@��H@��h@���@�	�@���@��@�z�@�C@��>@�o @� i@�Q@�@�[@�Ov@�1@��@�;@��@��@�.�@��N@�b�@�@�t@���@�?�@��@�=�@�8@罥@�h
@���@�'@��@�_@�@��[@�j@�h@��f@�J�@�X�@�8�@ݤ@@�@���@�oi@�
=@ؔF@�L0@׭C@�s�@�?}@֌@�D�@Ճ{@�oi@�	@�C�@қ�@�c@�@��f@�N<@��@Ϸ@��@λ�@�Ta@�@�g�@̧�@��@�4@�N�@��@ɔ�@�a�@���@��@ʅ�@ɋ�@�}V@�Q�@�)�@��m@�A�@���@Ƶ@Š'@�a�@��@�0�@���@��@�zx@�j@�?}@���@��@ �@�^5@���@���@���@�3�@�~�@��@���@�;d@��k@��@�J�@��m@�Y�@���@�3�@�S&@��@��)@��@@� \@�GE@��}@�O�@�	l@��`@���@�YK@���@�B�@�)_@��f@��\@�,=@��@���@��@�Y�@�%F@���@��?@���@�	@���@�G�@�@�҉@�O@��m@��-@�S�@�Dg@�C�@�F�@�H�@�:�@��9@���@��@��x@�r�@�"h@��3@���@��@���@��@�W?@���@�'R@��	@�"�@��u@��@��C@�s@��@��@��@�\�@�u@��@�4@�ѷ@��j@���@���@���@�v�@�\�@��@�.I@���@��j@�e�@��@��^@�qv@�=�@� \@��@��f@���@��@��z@���@�t�@��@��@��O@�/�@��K@�s�@�&�@���@�z�@��@��q@�dZ@�@@��@�U2@�$@�	�@���@��S@�j@��@�͟@�V�@��@��[@�{J@�]�@�Dg@���@���@��u@�oi@�Q@�$�@���@���@�\)@���@���@���@�h
@��@� �@���@�@��4@�^�@�&@��@�u�@�=q@��z@��	@��@�/�@��}@�(�@��@���@���@�}�@��@�d�@�7@���@��@�IR@���@���@��u@�{�@�`�@�x@���@�zx@���@�Ɇ@�Z�@�@���@���@��M@�N<@�Y@���@���@�V�@�!@���@�X@�.I@��@��@���@�z�@�1'@��A@�ϫ@�@��@�>�@��@���@��p@���@��.@�z�@�V�@�2�@��]@��t@��~@�o�@�J#@��@���@��\@�{�@�W�@�H�@�{@�F@33@~��@~�h@~�x@~��@~GE@}�@}�@}��@}@@|�[@|�D@|h�@|  @{/�@z�'@z��@z��@z��@zJ@yj@x�|@xZ@x1@w�f@vc @u��@u/@t�?@tG@sn/@rȴ@rE�@rE�@r-@q�@p��@p7�@o�}@o��@o$t@n�m@n@�@m�=@mB�@l�U@l��@k�@k�]@k��@kY@j\�@i��@ij@i#�@h��@hu�@g�:@g&@f�]@f��@f�@e�9@eG�@d��@d~(@dH@c�@cJ#@b�F@b:*@b{@a�@aq@`�?@`1'@_��@_�@_/�@_S@^҉@^��@^u%@^�@]��@\��@\Xy@\(�@[خ@[A�@Z҉@Z�@Z��@Y��@Y��@Y��@Y5�@X�p@X'R@W�g@W�@W+@V�@Vl�@U�3@U�@T��@T�@S��@S33@R�!@R�@R�@Q�^@Q`B@Q%@P�9@Py>@PM@O�@Oy�@N�@N�x@NR�@N5?@N4@M�)@M@Mf�@M[W@MX@MJ�@M%F@L�@L�?@L2�@K�g@K�w@K�f@KK�@KF�@J�@J^5@J
�@I��@I(�@H�@H��@H�.@H]d@H!@H  @G��@G\)@GE9@F�y@F��@F_�@E�Z@E��@E�@E(�@D��@D�@Dx@C��@C9�@C�@B��@B�+@BZ�@B	@A�N@A��@AO�@@��@@S�@?�w@?A�@>�m@=��@=�"@=+�@<�|@<��@<*�@;��@;�@:=q@9�@9N<@8�p@8��@8x@7��@7�@6�'@6ff@6{@5�D@5�@5�@5��@5u�@58�@4�@4~(@4H@3��@3�@3�@2�h@2~�@2u%@2q�@2H�@1��@1�'@0�v@0]d@0�@/�
@/��@/�$@/e�@.�8@.�@.��@.3�@-�@-��@-��@-|@-J�@,��@,|�@,�@+��@+��@+o�@+_p@+@*� @*M�@*O@)�@)Q�@(��@(�D@(N�@'��@'��@'�f@'e�@'+@&��@&��@&��@&�A@&@%�@%�7@%x�@%?}@%�@$��@$ی@$�?@$�U@$e�@$@#�W@#�}@#��@#��@#�@#~�@#j�@#a@#H�@"�@"�'@"�@"�\@"�\@"z@"B[@!�Z@!��@!��@ H@ -�@ �@�+@�@�g@�k@�k@x@1�@c @&�@��@T�@�@�/@��@��@z�@g8@Ft@�@s@Z�@]�@_p@O@@O@"�@�B@c @!�@��@�@�d@��@ \@��@1'@�]@��@��@o�@@O@�@�c@�X@��@^5@R�@GE@4@�C@\�@+@�@��@K^@�@��@a@"�@��@��@V@�D@�#@�X@��@p�@`B@�@��@?�@!@�r@��@ݘ@�$@qv@S�@�@�y@҉@�X@�h@��@�\@~�@u%@v�@s�@@�@j@Q�@J�@%F@�v@��@�u@Xy@�@�@�K@�[@��@~�@+@�@
��@
ں@
҉@
�m@
��@
�'@
�@
Q@
e@	�)@	��@	�@	��@	�Z@	�@	�S@	rG@	IR@	*0@	@@��@ѷ@�e@K^@(�@'R@ �@1@  @��@��@ݘ@��@o�@W?@O@8@�@��@�s@�X@��@s�@5?@	@	@�@�n@��@�S@��@zx@s�@c�@B�@q@�f@�51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�	B
�	B
�>B
�$B
�>B
�>B
��B
��B
�lB
�RB
��B
��B
��B
�`B
�+B
��B
��B
��B
��B
��B
��B
��B
�oB
ѷB
� B
��B
�(B
�BB*�BMjB^Be�Bx�B|�B��B��B�qB.�BR Bs�B.B��B�)B��B�pB��B� B��BuB^jBAoB)B�B�/B��B�mB�vB�#B��B�BB�2B�
B�bB�tB�\B��Be,B1�B�B
�2B
�;B
�lB
��B
��B
n}B
Y�B
M�B
8B
 \B
gB	��B	ٚB	��B	�`B	��B	�^B	�B	raB	b�B	E�B	4�B	'mB	hB	�B�B��B�QB�!BԕBбB̳B�1B�B��B��B�5B��B��B�qB��B��B�B��B�!B�B��B�`B	^B	B	�B	�B	�B	�B	�B	�B		B	'�B	0�B	(>B	
B	mB	�B	1B	�B	"�B	9�B	C�B	C�B	P.B	QhB	R B	V�B	m]B	��B	}B	��B	��B	��B	B	�GB	ªB	�'B	�]B	�rB	��B	��B	�B	�B	�uB	�[B	��B	ǔB	��B	āB	ªB	�{B	�4B	ڠB	خB	յB	�@B	˒B	ƎB	�+B	ɠB	̘B	�~B	��B	�lB	�+B	�zB	��B	˒B	ԯB	ԕB	��B	ΊB	��B	�B	�B	��B	�xB	��B	˒B	�	B	�7B	ȀB	��B	�.B	��B	�B	�B	�HB	�<B	��B	��B	ѝB	ѝB	ңB	�,B	�B	�[B	� B	��B	�SB	�$B	֡B	�mB	��B	�B	�{B	ҽB	ңB	ҽB	��B	�FB	��B	өB	��B	��B	ܒB	��B	רB	��B	�{B	��B	ӏB	��B	�{B	�,B	�B	ңB	�B	ԯB	�uB	��B	��B	ӏB	��B	өB	�2B	ևB	�?B	�$B	��B	�sB	�B	֡B	ּB	�sB	��B	�SB	ּB	��B	��B	ٴB	�WB	�B	��B	�+B	��B	�YB	�sB	רB	՛B	өB	өB	��B	ںB	�)B	��B	��B	�)B	ۦB	�qB	�B	�B	��B	�$B	�+B	��B	��B	ߊB	�"B	�=B	��B	�B	�yB	�B	�_B	�yB	�>B	�B	��B	�B	�>B	�DB	�B	�yB	�B	�WB	�qB	�B	�)B	��B	�oB	��B	�"B	��B	�
B	��B	��B	�eB	�!B	�tB	�<B	��B	�HB
  B
 iB
 �B
 �B
 �B
B
�B
AB
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
-B
GB
�B
3B
�B
SB
�B
�B
�B
�B
�B
YB
YB
�B
�B
�B
�B
�B
	B
	�B
	�B
	�B
	�B
	�B
	RB

=B
�B
B
�B
�B
�B
�B
�B
�B
�B
VB
jB
�B
�B
B
PB
VB
�B
B
\B
�B
HB
HB
}B
 B
 B
4B
�B
�B
�B
�B
�B
�B
�B
�B
&B
�B
�B
@B
uB
,B
aB
�B
�B
�B
�B
2B
gB
�B
�B
�B
B
�B
�B
�B
�B
�B
?B
?B
�B
yB
yB
�B
B
eB
eB
eB
B
B
�B
�B
QB
�B
	B
#B
WB
qB
qB
�B
�B
]B
xB
�B
�B
�B
IB
~B
�B
jB
jB
jB
�B
;B
B
!B
VB
�B
�B
�B
 BB
 �B
 �B
!�B
!bB
!-B
!|B
!�B
"�B
"�B
# B
#B
# B
$&B
$�B
%�B
%�B
%�B
&2B
&�B
'B
&�B
'B
'B
'�B
'�B
'�B
($B
(>B
(�B
(�B
)*B
)DB
)DB
)�B
)�B
*KB
*B
*�B
*�B
+B
*�B
*�B
*�B
+�B
,"B
,qB
,�B
-)B
-�B
-�B
./B
.�B
/�B
0;B
0UB
0UB
0�B
0�B
1B
1B
1[B
1�B
1�B
1�B
1�B
2|B
2�B
3�B
3�B
3�B
3�B
4TB
4�B
4�B
5%B
5ZB
5ZB
5ZB
5tB
5�B
5�B
5�B
6FB
6`B
6zB
6`B
6�B
7�B
7�B
7�B
7�B
7�B
8B
8B
8RB
8�B
8�B
9	B
:B
9�B
:B
:*B
:�B
:�B
;B
;dB
;�B
;B
;�B
<B
<6B
<PB
<6B
<�B
=qB
=�B
=�B
>(B
=�B
>(B
>�B
?.B
@4B
@�B
AB
A�B
A�B
A�B
A�B
B[B
BuB
BAB
B�B
B�B
CaB
DB
DMB
D�B
D3B
ESB
E�B
F�B
G+B
GEB
GEB
G�B
HB
HfB
H�B
IB
I7B
I�B
I�B
I�B
I�B
I�B
J#B
JXB
J�B
KDB
KDB
K�B
K�B
LJB
LJB
LdB
L�B
L�B
MB
M�B
M�B
M�B
NB
NB
NVB
N�B
N�B
O�B
O�B
O�B
PHB
PbB
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
RB
RB
RTB
R�B
R�B
S[B
S�B
S�B
S�B
S�B
S�B
T{B
TaB
T{B
TaB
T�B
T�B
T{B
UB
UMB
U2B
U�B
U�B
U�B
VB
VB
V�B
W$B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
X�B
XyB
X�B
X�B
X�B
YB
YKB
YeB
YB
Y�B
ZB
ZB
ZkB
Z�B
Z�B
[	B
[WB
[WB
[qB
[�B
[�B
[�B
[�B
\CB
\)B
\CB
\B
[�B
\CB
\CB
\xB
\]B
\xB
\�B
]/B
]�B
]�B
]�B
^jB
_VB
`BB
`�B
`�B
aB
abB
a�B
a�B
b�B
b�B
b�B
cB
c B
cTB
c�B
d&B
d@B
d�B
d�B
d�B
d�B
d�B
e,B
eB
eB
eB
ezB
f�B
gB
gRB
gmB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h
B
h>B
h�B
iB
h�B
h�B
iB
i*B
i*B
iyB
i�B
jB
jeB
j�B
j�B
j�B
kQB
kQB
kkB
kkB
k�B
k�B
k�B
l�B
m)B
mCB
mCB
mCB
mCB
mCB
m�B
m�B
m�B
m�B
m�B
nIB
nIB
n}B
ncB
n}B
n�B
n�B
nIB
n}B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oOB
o�B
o�B
p!B
poB
p�B
p�B
q'B
qB
qB
q'B
p�B
q'B
qB
qB
q[B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
t�B
t�B
uB
u%B
utB
u�B
u�B
u�B
w2B
wB
wLB
w�B
w�B
x�B
y	B
y>B
yXB
yrB
y�B
y�B
y�B
z*B
z^B
z�B
{�B
|B
|jB
|�B
|�B
|�B
|�B
}"B
}<B
}qB
}�B
}�B
}�B
~B
~(B
~wB
~�B
~�B
B
B
~�B
~�B
}B
�4B
�OB
��B
��B
�B
�;B
�UB
�UB
��B
��B
��B
��B
��B
��B
��B
��B
�[B
��B
��B
��B
��B
�B
�{B
�aB
�{B
��B
��B
��B
��B
��B
��B
��B
�MB
�gB
�gB
��B
��B
��B
��B
�9B
�mB
��B
��B
��B
��B
��B
�%B
�?B
��B
��B
��B
�+B
�zB
��B
��B
�1B
�fB
��B
��B
�B
�KB
��B
�7B
��B
��B
��B
��B
��B
��B
�DB
�xB
�xB
�DB
��B
��B
�)B
�xB
��B
��B
��B
��B
�0B
�~B
��B
��B
��B
��B
��B
�6B
�B
�6B
�PB
�jB
��B
��B
��B
�VB
�VB
�VB
�VB
�pB
��B
�pB
�pB
�VB
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�	B
�	B
�>B
�$B
�>B
�>B
��B
��B
�lB
�RB
��B
��B
��B
�`B
�+B
��B
��B
��B
��B
��B
��B
��B
�oB
ѷB
� B
��B
�(B
�BB*�BMjB^Be�Bx�B|�B��B��B�qB.�BR Bs�B.B��B�)B��B�pB��B� B��BuB^jBAoB)B�B�/B��B�mB�vB�#B��B�BB�2B�
B�bB�tB�\B��Be,B1�B�B
�2B
�;B
�lB
��B
��B
n}B
Y�B
M�B
8B
 \B
gB	��B	ٚB	��B	�`B	��B	�^B	�B	raB	b�B	E�B	4�B	'mB	hB	�B�B��B�QB�!BԕBбB̳B�1B�B��B��B�5B��B��B�qB��B��B�B��B�!B�B��B�`B	^B	B	�B	�B	�B	�B	�B	�B		B	'�B	0�B	(>B	
B	mB	�B	1B	�B	"�B	9�B	C�B	C�B	P.B	QhB	R B	V�B	m]B	��B	}B	��B	��B	��B	B	�GB	ªB	�'B	�]B	�rB	��B	��B	�B	�B	�uB	�[B	��B	ǔB	��B	āB	ªB	�{B	�4B	ڠB	خB	յB	�@B	˒B	ƎB	�+B	ɠB	̘B	�~B	��B	�lB	�+B	�zB	��B	˒B	ԯB	ԕB	��B	ΊB	��B	�B	�B	��B	�xB	��B	˒B	�	B	�7B	ȀB	��B	�.B	��B	�B	�B	�HB	�<B	��B	��B	ѝB	ѝB	ңB	�,B	�B	�[B	� B	��B	�SB	�$B	֡B	�mB	��B	�B	�{B	ҽB	ңB	ҽB	��B	�FB	��B	өB	��B	��B	ܒB	��B	רB	��B	�{B	��B	ӏB	��B	�{B	�,B	�B	ңB	�B	ԯB	�uB	��B	��B	ӏB	��B	өB	�2B	ևB	�?B	�$B	��B	�sB	�B	֡B	ּB	�sB	��B	�SB	ּB	��B	��B	ٴB	�WB	�B	��B	�+B	��B	�YB	�sB	רB	՛B	өB	өB	��B	ںB	�)B	��B	��B	�)B	ۦB	�qB	�B	�B	��B	�$B	�+B	��B	��B	ߊB	�"B	�=B	��B	�B	�yB	�B	�_B	�yB	�>B	�B	��B	�B	�>B	�DB	�B	�yB	�B	�WB	�qB	�B	�)B	��B	�oB	��B	�"B	��B	�
B	��B	��B	�eB	�!B	�tB	�<B	��B	�HB
  B
 iB
 �B
 �B
 �B
B
�B
AB
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
-B
GB
�B
3B
�B
SB
�B
�B
�B
�B
�B
YB
YB
�B
�B
�B
�B
�B
	B
	�B
	�B
	�B
	�B
	�B
	RB

=B
�B
B
�B
�B
�B
�B
�B
�B
�B
VB
jB
�B
�B
B
PB
VB
�B
B
\B
�B
HB
HB
}B
 B
 B
4B
�B
�B
�B
�B
�B
�B
�B
�B
&B
�B
�B
@B
uB
,B
aB
�B
�B
�B
�B
2B
gB
�B
�B
�B
B
�B
�B
�B
�B
�B
?B
?B
�B
yB
yB
�B
B
eB
eB
eB
B
B
�B
�B
QB
�B
	B
#B
WB
qB
qB
�B
�B
]B
xB
�B
�B
�B
IB
~B
�B
jB
jB
jB
�B
;B
B
!B
VB
�B
�B
�B
 BB
 �B
 �B
!�B
!bB
!-B
!|B
!�B
"�B
"�B
# B
#B
# B
$&B
$�B
%�B
%�B
%�B
&2B
&�B
'B
&�B
'B
'B
'�B
'�B
'�B
($B
(>B
(�B
(�B
)*B
)DB
)DB
)�B
)�B
*KB
*B
*�B
*�B
+B
*�B
*�B
*�B
+�B
,"B
,qB
,�B
-)B
-�B
-�B
./B
.�B
/�B
0;B
0UB
0UB
0�B
0�B
1B
1B
1[B
1�B
1�B
1�B
1�B
2|B
2�B
3�B
3�B
3�B
3�B
4TB
4�B
4�B
5%B
5ZB
5ZB
5ZB
5tB
5�B
5�B
5�B
6FB
6`B
6zB
6`B
6�B
7�B
7�B
7�B
7�B
7�B
8B
8B
8RB
8�B
8�B
9	B
:B
9�B
:B
:*B
:�B
:�B
;B
;dB
;�B
;B
;�B
<B
<6B
<PB
<6B
<�B
=qB
=�B
=�B
>(B
=�B
>(B
>�B
?.B
@4B
@�B
AB
A�B
A�B
A�B
A�B
B[B
BuB
BAB
B�B
B�B
CaB
DB
DMB
D�B
D3B
ESB
E�B
F�B
G+B
GEB
GEB
G�B
HB
HfB
H�B
IB
I7B
I�B
I�B
I�B
I�B
I�B
J#B
JXB
J�B
KDB
KDB
K�B
K�B
LJB
LJB
LdB
L�B
L�B
MB
M�B
M�B
M�B
NB
NB
NVB
N�B
N�B
O�B
O�B
O�B
PHB
PbB
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
RB
RB
RTB
R�B
R�B
S[B
S�B
S�B
S�B
S�B
S�B
T{B
TaB
T{B
TaB
T�B
T�B
T{B
UB
UMB
U2B
U�B
U�B
U�B
VB
VB
V�B
W$B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
X�B
XyB
X�B
X�B
X�B
YB
YKB
YeB
YB
Y�B
ZB
ZB
ZkB
Z�B
Z�B
[	B
[WB
[WB
[qB
[�B
[�B
[�B
[�B
\CB
\)B
\CB
\B
[�B
\CB
\CB
\xB
\]B
\xB
\�B
]/B
]�B
]�B
]�B
^jB
_VB
`BB
`�B
`�B
aB
abB
a�B
a�B
b�B
b�B
b�B
cB
c B
cTB
c�B
d&B
d@B
d�B
d�B
d�B
d�B
d�B
e,B
eB
eB
eB
ezB
f�B
gB
gRB
gmB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h
B
h>B
h�B
iB
h�B
h�B
iB
i*B
i*B
iyB
i�B
jB
jeB
j�B
j�B
j�B
kQB
kQB
kkB
kkB
k�B
k�B
k�B
l�B
m)B
mCB
mCB
mCB
mCB
mCB
m�B
m�B
m�B
m�B
m�B
nIB
nIB
n}B
ncB
n}B
n�B
n�B
nIB
n}B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oOB
o�B
o�B
p!B
poB
p�B
p�B
q'B
qB
qB
q'B
p�B
q'B
qB
qB
q[B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
t�B
t�B
uB
u%B
utB
u�B
u�B
u�B
w2B
wB
wLB
w�B
w�B
x�B
y	B
y>B
yXB
yrB
y�B
y�B
y�B
z*B
z^B
z�B
{�B
|B
|jB
|�B
|�B
|�B
|�B
}"B
}<B
}qB
}�B
}�B
}�B
~B
~(B
~wB
~�B
~�B
B
B
~�B
~�B
}B
�4B
�OB
��B
��B
�B
�;B
�UB
�UB
��B
��B
��B
��B
��B
��B
��B
��B
�[B
��B
��B
��B
��B
�B
�{B
�aB
�{B
��B
��B
��B
��B
��B
��B
��B
�MB
�gB
�gB
��B
��B
��B
��B
�9B
�mB
��B
��B
��B
��B
��B
�%B
�?B
��B
��B
��B
�+B
�zB
��B
��B
�1B
�fB
��B
��B
�B
�KB
��B
�7B
��B
��B
��B
��B
��B
��B
�DB
�xB
�xB
�DB
��B
��B
�)B
�xB
��B
��B
��B
��B
�0B
�~B
��B
��B
��B
��B
��B
�6B
�B
�6B
�PB
�jB
��B
��B
��B
�VB
�VB
�VB
�VB
�pB
��B
�pB
�pB
�VB
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220920064839  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220920064848  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220920064849  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220920064849                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220920154853  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220920154853  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220920070032                      G�O�G�O�G�O�                