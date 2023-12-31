CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-12-24T15:43:10Z creation;2022-12-24T15:43:12Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20221224154310  20221224155833  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��v#�1   @���o��@-|�hr��c��hr�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B���B�  B�  B�33B�33B���B�  B�  B���B���B�  B�  B�  B�  B���B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D˼�D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ Dռ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @%@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B\)B��B'�]B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bx\)B��B�ǮB���B���B�.B�.B��{B���B���B�ǮB�ǮB���B���B���B���B��{B���B���B���B�aGB���B�ǮB���B���B���B���B���B���B�.B���B���B�ǮB���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC8C9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DD�DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˼{D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dռ{D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D��{D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AԌ�AԗYAԚ�AԘ�AԚkAԖ�AԖ�AԞ�AԤ@AԴA���A��A��A��/A��`A���A��A���A��mA���A�ϫA��zAԵAԥ�AԠ'AԞAԜCAԎ�A�j�A�N�A� 'A���AӝIA�s�A���A΄�A͡�A��-A�_pA�_�A�u�A��AA���A�P�A���A���A���A�%�A���A���A���A�W?A�w�A|�fAsFtAqqAmn/Af�AAc�3Ab�;A`_A_�A]��AY��AV{AO,�AK� AI|AG^�AE��AB�A?�)A@$�A?��A>�A=�A=��A=R�A<�A<VmA;�*A;%�A:�3A9p�A5a�A2�-A1K�A06zA/?}A.��A.��A-w�A+��A+`BA*�bA)�A)i�A(��A&��A%5�A$�mA$PHA#dZA"��A"��A"%FA!ԕA!�AA"�A!��A!e,AC�A�A��A	ADgA�?A|�A��A�1A�HA(�A��A��A�AVmAQ�ATaATaAT�A��A�MA��AjA�>A�PA;dA7A��A  A;A�+A3�A�jAS�A��A@OA
��A
��A
��A
�A
IRA
�A	{�A	 �A�\AE9A��A�AcA�A�A�A��AqvA�A��A�A�/A��A��AH�A�XA��Au%A ˒A E�@��)@��@�U2@�e@��F@��0@�]�@�ߤ@�2�@�/@���@�'�@�kQ@�IR@���@�f�@�;�@�X�@��D@���@�b�@��@���@���@�ԕ@�+@�8�@�b�@�Ĝ@脶@��@�m�@�_p@��@�Ft@���@�;@�5�@�#:@���@�\)@���@�}@�r�@��@��@�9X@�L�@�"�@࿱@�g�@��@���@߰�@�`B@ޕ@�Z�@ޏ\@ޅ�@�h
@�b@݁�@�b�@�(@�@��@��@�Z�@��6@���@�GE@֤�@�x@�@�� @��@Վ�@�+@���@��E@Ԏ�@�L0@ӯ�@�=�@Ҥ�@�G@���@�4�@��@Τ�@��@̹�@�Ta@��a@˞�@�@��@�
�@Ɏ�@ȵ�@��m@��c@�v�@��@�Y�@��E@�`�@�O@ð�@À4@�K�@��@�@���@�c�@���@�c @�R�@���@�iD@��H@���@��~@���@��?@��<@��L@�@�+@��@���@�6�@�7@���@�%F@��@��9@���@�YK@�1@��@���@���@��@���@�s@�A�@��@���@��$@�#:@��~@�$t@�Ĝ@���@���@���@�?�@�IR@��@��]@���@�%�@��m@���@�A�@�&@��@�g8@��@��h@�`B@�A @�8�@��@���@��^@�RT@�ѷ@�p;@��@���@���@��{@��?@�Xy@�U2@�-@�1@���@���@�%@��L@�?�@�
�@��N@�]�@�N<@�Dg@�ߤ@�h
@���@�T�@�
=@��y@�d�@��f@�@O@�7L@���@���@�c @�C�@�~@���@���@�o @��@���@�'R@���@�qv@�O�@�=@�S@���@�YK@�  @��m@��j@��@�rG@���@��j@��z@��b@���@�v�@�^5@�D�@�_@��d@��7@��p@�;�@��.@��o@��@@�o@�Ɇ@���@�g8@�!@���@�O�@�#�@��@��@���@��z@�� @�<�@���@���@���@���@�m]@�=�@��|@���@���@�H�@�#:@��@��}@��@�w2@�/�@��@��K@���@���@��@��[@�H�@��@���@�4n@���@��X@���@�s�@�1�@���@���@���@�.�@��Z@��q@�S�@��M@���@�i�@��r@���@�U�@�֡@���@�z�@�I�@�@���@���@���@�/@���@��v@���@�M@�8�@��@�ݘ@�rG@�0�@��@���@���@�H@��@�;d@��@���@�q@�V@�0U@�{@���@���@���@��$@�{J@�6z@��,@��h@�Z�@�?@�8�@��@��D@�˒@�}�@�A @�'�@�	l@��U@�p;@�8�@�A@�@��@��@!-@~q�@}�@}p�@|�@|bN@|1@{��@{�&@{��@{>�@zn�@y}�@xĜ@wƨ@wO@v�@vB[@u&�@t��@tS�@t �@s�&@sg�@sY@s
=@q�.@qa�@p�@pXy@p@o�f@n��@m^�@l��@l(�@k�}@k��@kdZ@kt�@k.I@j�'@j@�@i�@i�M@i@h�D@gƨ@f��@f��@fH�@f@erG@d��@d:�@c�F@cg�@cH�@b�H@b?@b!�@b@a��@ahs@`�|@`PH@_��@_8@^��@^Q@^B[@^:*@^3�@^-@]�.@]|@\�@\��@\~(@\S�@\�@[�@[��@[��@[�{@[9�@[S@Z�]@Z1�@Y��@Yԕ@Y�'@Y&�@X��@X�_@XU2@X@W�P@WdZ@W=@WC@V�c@V��@V��@VM�@U�T@U��@Um]@T�v@T�@TG@SU�@R�@R��@RQ@R.�@Q�@Q�H@Q��@Q��@Q2a@P�9@Pc�@O�*@O�{@OZ�@OP�@OF�@O4�@O$t@O�@Nߤ@N��@N1�@M�z@MY�@L�@L�@L��@LXy@K��@K��@K9�@J��@J�<@J{�@I�N@I:�@H��@HV�@G�r@G�}@G��@Gt�@GS�@G!-@F�]@F�!@F��@F��@FJ@E�@E�@E(�@D�@D��@D(�@C��@C�:@CY@B�@B�}@Bn�@BGE@A�)@A�@@�E@@'R@?S�@>��@>h
@=�D@=�3@=��@=�@=�S@=�@<��@<u�@<~@;�a@;U�@:��@:�+@:E�@9��@9%F@8�@8C-@7�@7Mj@6R�@5\�@5 \@4��@4��@4�D@4/�@3�@36z@3�@3
=@2ߤ@2�x@2d�@2$�@1�-@1c@1Q�@1q@0�[@0�@/9�@.�A@.J�@-��@-�@,q@+��@+�@+��@+j�@+.I@*��@*��@*u@)�@)^�@)#�@(�@(]d@(9X@((�@'�@'��@'��@'dZ@'6z@'S@&��@&��@&��@&H�@&�@& �@%�@%�@%��@%e,@%J�@%/@%�@$��@$Ft@#��@#C�@"�@"��@"Q@!��@!��@!�3@!��@!m]@!\�@!G�@!#�@ ��@ ��@ �@ �D@ Xy@ 7�@ M@ �@�}@��@@�L@�b@�L@��@��@Ta@	@�N@�@��@��@�@c�@J�@�@��@N�@@�W@��@��@�k@�	@�{@s@n/@RT@.I@o@�H@�r@V@&�@J@u@�j@��@�'@��@|@Q�@G�@Dg@7L@#�@�	@�[@�j@�O@�4@�Y@h�@6@ݘ@��@��@��@�	@�P@��@~�@n/@J#@.I@Y@�@��@^5@O@J@�@�S@8�@��@֡@��@M@�@�@�k@g�@+@�@@
=@�@ i@�@�@�M@�@�@�@��@�@~�@c @L0@!�@��@��@��@�D@�T@�j@��@s�@-w@@��@�@q@$@�@��@��@�Q@� @�a@��@~�@v`@t�@v`@qv@\)@=@1�@ i@��@��@�@z@1�@�@��@G�@/@q@+@�@�|@��@��@��@�D@�.@I�@<�@9X@�@�r@��@��@��@��@�q@�k@�P@e�@.I@�@
��@
�@
}V@
s�@
v�@
u%@
s�@
h
@
H�@
=q@
0U@
1�@
1�@
3�@
0U@
&�@
	@
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AԌ�AԗYAԚ�AԘ�AԚkAԖ�AԖ�AԞ�AԤ@AԴA���A��A��A��/A��`A���A��A���A��mA���A�ϫA��zAԵAԥ�AԠ'AԞAԜCAԎ�A�j�A�N�A� 'A���AӝIA�s�A���A΄�A͡�A��-A�_pA�_�A�u�A��AA���A�P�A���A���A���A�%�A���A���A���A�W?A�w�A|�fAsFtAqqAmn/Af�AAc�3Ab�;A`_A_�A]��AY��AV{AO,�AK� AI|AG^�AE��AB�A?�)A@$�A?��A>�A=�A=��A=R�A<�A<VmA;�*A;%�A:�3A9p�A5a�A2�-A1K�A06zA/?}A.��A.��A-w�A+��A+`BA*�bA)�A)i�A(��A&��A%5�A$�mA$PHA#dZA"��A"��A"%FA!ԕA!�AA"�A!��A!e,AC�A�A��A	ADgA�?A|�A��A�1A�HA(�A��A��A�AVmAQ�ATaATaAT�A��A�MA��AjA�>A�PA;dA7A��A  A;A�+A3�A�jAS�A��A@OA
��A
��A
��A
�A
IRA
�A	{�A	 �A�\AE9A��A�AcA�A�A�A��AqvA�A��A�A�/A��A��AH�A�XA��Au%A ˒A E�@��)@��@�U2@�e@��F@��0@�]�@�ߤ@�2�@�/@���@�'�@�kQ@�IR@���@�f�@�;�@�X�@��D@���@�b�@��@���@���@�ԕ@�+@�8�@�b�@�Ĝ@脶@��@�m�@�_p@��@�Ft@���@�;@�5�@�#:@���@�\)@���@�}@�r�@��@��@�9X@�L�@�"�@࿱@�g�@��@���@߰�@�`B@ޕ@�Z�@ޏ\@ޅ�@�h
@�b@݁�@�b�@�(@�@��@��@�Z�@��6@���@�GE@֤�@�x@�@�� @��@Վ�@�+@���@��E@Ԏ�@�L0@ӯ�@�=�@Ҥ�@�G@���@�4�@��@Τ�@��@̹�@�Ta@��a@˞�@�@��@�
�@Ɏ�@ȵ�@��m@��c@�v�@��@�Y�@��E@�`�@�O@ð�@À4@�K�@��@�@���@�c�@���@�c @�R�@���@�iD@��H@���@��~@���@��?@��<@��L@�@�+@��@���@�6�@�7@���@�%F@��@��9@���@�YK@�1@��@���@���@��@���@�s@�A�@��@���@��$@�#:@��~@�$t@�Ĝ@���@���@���@�?�@�IR@��@��]@���@�%�@��m@���@�A�@�&@��@�g8@��@��h@�`B@�A @�8�@��@���@��^@�RT@�ѷ@�p;@��@���@���@��{@��?@�Xy@�U2@�-@�1@���@���@�%@��L@�?�@�
�@��N@�]�@�N<@�Dg@�ߤ@�h
@���@�T�@�
=@��y@�d�@��f@�@O@�7L@���@���@�c @�C�@�~@���@���@�o @��@���@�'R@���@�qv@�O�@�=@�S@���@�YK@�  @��m@��j@��@�rG@���@��j@��z@��b@���@�v�@�^5@�D�@�_@��d@��7@��p@�;�@��.@��o@��@@�o@�Ɇ@���@�g8@�!@���@�O�@�#�@��@��@���@��z@�� @�<�@���@���@���@���@�m]@�=�@��|@���@���@�H�@�#:@��@��}@��@�w2@�/�@��@��K@���@���@��@��[@�H�@��@���@�4n@���@��X@���@�s�@�1�@���@���@���@�.�@��Z@��q@�S�@��M@���@�i�@��r@���@�U�@�֡@���@�z�@�I�@�@���@���@���@�/@���@��v@���@�M@�8�@��@�ݘ@�rG@�0�@��@���@���@�H@��@�;d@��@���@�q@�V@�0U@�{@���@���@���@��$@�{J@�6z@��,@��h@�Z�@�?@�8�@��@��D@�˒@�}�@�A @�'�@�	l@��U@�p;@�8�@�A@�@��@��@!-@~q�@}�@}p�@|�@|bN@|1@{��@{�&@{��@{>�@zn�@y}�@xĜ@wƨ@wO@v�@vB[@u&�@t��@tS�@t �@s�&@sg�@sY@s
=@q�.@qa�@p�@pXy@p@o�f@n��@m^�@l��@l(�@k�}@k��@kdZ@kt�@k.I@j�'@j@�@i�@i�M@i@h�D@gƨ@f��@f��@fH�@f@erG@d��@d:�@c�F@cg�@cH�@b�H@b?@b!�@b@a��@ahs@`�|@`PH@_��@_8@^��@^Q@^B[@^:*@^3�@^-@]�.@]|@\�@\��@\~(@\S�@\�@[�@[��@[��@[�{@[9�@[S@Z�]@Z1�@Y��@Yԕ@Y�'@Y&�@X��@X�_@XU2@X@W�P@WdZ@W=@WC@V�c@V��@V��@VM�@U�T@U��@Um]@T�v@T�@TG@SU�@R�@R��@RQ@R.�@Q�@Q�H@Q��@Q��@Q2a@P�9@Pc�@O�*@O�{@OZ�@OP�@OF�@O4�@O$t@O�@Nߤ@N��@N1�@M�z@MY�@L�@L�@L��@LXy@K��@K��@K9�@J��@J�<@J{�@I�N@I:�@H��@HV�@G�r@G�}@G��@Gt�@GS�@G!-@F�]@F�!@F��@F��@FJ@E�@E�@E(�@D�@D��@D(�@C��@C�:@CY@B�@B�}@Bn�@BGE@A�)@A�@@�E@@'R@?S�@>��@>h
@=�D@=�3@=��@=�@=�S@=�@<��@<u�@<~@;�a@;U�@:��@:�+@:E�@9��@9%F@8�@8C-@7�@7Mj@6R�@5\�@5 \@4��@4��@4�D@4/�@3�@36z@3�@3
=@2ߤ@2�x@2d�@2$�@1�-@1c@1Q�@1q@0�[@0�@/9�@.�A@.J�@-��@-�@,q@+��@+�@+��@+j�@+.I@*��@*��@*u@)�@)^�@)#�@(�@(]d@(9X@((�@'�@'��@'��@'dZ@'6z@'S@&��@&��@&��@&H�@&�@& �@%�@%�@%��@%e,@%J�@%/@%�@$��@$Ft@#��@#C�@"�@"��@"Q@!��@!��@!�3@!��@!m]@!\�@!G�@!#�@ ��@ ��@ �@ �D@ Xy@ 7�@ M@ �@�}@��@@�L@�b@�L@��@��@Ta@	@�N@�@��@��@�@c�@J�@�@��@N�@@�W@��@��@�k@�	@�{@s@n/@RT@.I@o@�H@�r@V@&�@J@u@�j@��@�'@��@|@Q�@G�@Dg@7L@#�@�	@�[@�j@�O@�4@�Y@h�@6@ݘ@��@��@��@�	@�P@��@~�@n/@J#@.I@Y@�@��@^5@O@J@�@�S@8�@��@֡@��@M@�@�@�k@g�@+@�@@
=@�@ i@�@�@�M@�@�@�@��@�@~�@c @L0@!�@��@��@��@�D@�T@�j@��@s�@-w@@��@�@q@$@�@��@��@�Q@� @�a@��@~�@v`@t�@v`@qv@\)@=@1�@ i@��@��@�@z@1�@�@��@G�@/@q@+@�@�|@��@��@��@�D@�.@I�@<�@9X@�@�r@��@��@��@��@�q@�k@�P@e�@.I@�@
��@
�@
}V@
s�@
v�@
u%@
s�@
h
@
H�@
=q@
0U@
1�@
1�@
3�@
0U@
&�@
	@
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�%B��B��B��B�YB��B�B��B��B��B	�B	�B	MB	MB	2B	�B	B	SB	SB	�B	�B	�B	 B	"�B	"�B	#�B	%�B	-�B	6�B	?cB	<6B	8�B	:DB	L�B	mB	u�B	~]B	�B
1B
�B
KB
�B
&B
�B	��B	��B	�HB	ݲB	��B	�B	�[B	�zB	��B	�KB	�aB	xRB	oOB	c B	`BB	W�B	R:B	L~B	CB	:�B	5?B	.�B	-�B	-wB	/�B	2�B	9rB	VB	b�B	gB	l"B	q�B	zDB	.B	�9B	�jB	��B	��B	�dB	x�B	e�B	W�B	L~B	DgB	@�B	A;B	>]B	<�B	CGB	G�B	KB	J�B	J	B	G+B	D�B	C�B	B�B	FtB	L0B	N�B	Q�B	RoB	`�B	l=B	n�B	t�B	lWB	_!B	_VB	\xB	Z�B	_�B	P�B	TB	Y�B	T{B	^�B	t�B	t�B	r-B	m�B	t�B	�B	��B	��B	�_B	�zB	��B	�VB	�<B	��B	�6B	��B	�B	��B	�XB	��B	��B	�MB	�-B	�;B	�[B	��B	�;B	�oB	��B	�mB	�9B	��B	�MB	��B	��B	��B	�QB	��B	��B	��B	�|B	��B	��B	��B	�,B	�B	�2B	��B	�`B	��B	��B	��B	��B	�DB	��B	�iB	��B	�8B	�B	�oB	āB	żB	��B	�B	ƎB	ǮB	��B	�B	ÖB	�;B	��B	�B	��B	�oB	��B	�B	��B	��B	��B	ŢB	�YB	ƨB	�+B	�B	ɺB	͹B	ЗB	ΥB	ՁB	�MB	ԯB	�@B	�$B	�B	��B	�|B	�bB	�HB	�B	��B	�FB	��B	�B	��B	�B	��B	�B	�B	��B	�yB	�*B	�_B	�]B	��B	�)B	�B	��B	�=B	�>B	�B	�$B	��B	��B	�B	� B	��B	�B	�B	�,B	�B	�B	��B	��B	�]B	��B	��B	�ZB	��B	��B	��B	�-B	�OB	��B	�IB	�B	�|B	�!B	�B	�vB	��B	�B	��B	�B	��B	�B	� B	��B	�}B	�B	� B	�/B	��B	�}B	�B	��B	�[B	�AB	�[B	�B	�B	�9B	�nB	�TB	�B	��B	��B	�B	�B	��B	��B	��B	�rB	�B	��B	�2B	�LB	��B	��B	�fB	��B	�`B	��B	��B	��B	��B	�jB	�(B	�B
 OB
�B
�B
�B
�B
�B
�B
B
�B
+B
�B
zB
EB
EB
EB
	B
�B
�B
�B
	7B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B

	B

	B
	�B

XB

XB

�B

�B
DB
^B
�B
DB
)B
^B
JB
0B
B
JB
JB
B
0B
�B
B
�B
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
oB
oB
�B
aB
�B
�B
�B
B

B
�B
�B
�B
�B
EB
+B
EB
B
�B
�B
sB
sB
�B
+B
B
eB
B
�B
7B
kB
kB
kB
�B
�B
�B
�B
�B
#B
qB
�B
�B
�B
�B
�B
]B
)B
]B
xB
/B
IB
B
�B
;B
!B
!B
�B
B
VB
B
B
�B
�B
B
!B
�B
 �B
 �B
!B
!bB
!�B
"�B
#B
$@B
$�B
$�B
$�B
%B
&�B
$�B
$&B
$tB
$�B
$�B
%B
%`B
%�B
%�B
%�B
&�B
'B
'mB
'mB
'�B
'�B
'�B
(sB
)*B
)DB
)�B
*0B
*�B
+�B
,�B
,�B
-B
-)B
-�B
-wB
-�B
-�B
.}B
.�B
.�B
/iB
/�B
/�B
0B
0�B
1AB
1[B
1vB
1AB
1�B
1�B
2aB
3�B
3�B
3�B
3�B
3�B
4B
4B
4�B
5%B
5B
5B
5%B
5tB
5�B
5�B
6B
6+B
6FB
6�B
6�B
6�B
7LB
7�B
7�B
7�B
8B
8�B
9>B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:B
:xB
:�B
:�B
;0B
<B
;�B
;JB
;B
;�B
<�B
<B
<jB
<PB
<�B
<PB
<B
;�B
;�B
<B
<B
<�B
<�B
<jB
<�B
="B
>B
>(B
>�B
?.B
?.B
?}B
?�B
?�B
@�B
A�B
B�B
CB
CB
B�B
CaB
C�B
C�B
DB
D�B
D�B
D�B
D�B
EB
ESB
EmB
E�B
E�B
E�B
FB
FtB
F�B
F�B
F�B
GB
G+B
HKB
IB
I7B
I�B
I�B
IlB
IlB
I�B
K^B
LB
L�B
L�B
L�B
M�B
M�B
NB
N�B
O\B
OBB
O�B
P�B
P�B
P�B
QNB
QhB
Q�B
Q�B
RB
R B
R B
R:B
RTB
R B
R B
R B
R B
R B
RB
RTB
RoB
R�B
R�B
R�B
R�B
S�B
T�B
U2B
UMB
U2B
T�B
T�B
U2B
UMB
UMB
U2B
U�B
VB
VB
W
B
W$B
W$B
WYB
W?B
WYB
WYB
W?B
WYB
W?B
XB
X+B
X+B
X�B
X�B
X�B
Y1B
X�B
X�B
X�B
Y�B
YB
Y�B
ZB
Z7B
ZQB
Y�B
ZB
ZB
Z�B
[�B
[�B
[�B
\B
\B
\B
\B
\�B
\xB
\�B
]B
]/B
]/B
]�B
]�B
]�B
^OB
^5B
^�B
^�B
^�B
^�B
^�B
_VB
_!B
^�B
]�B
]~B
\�B
]dB
]�B
]�B
]�B
^�B
_�B
`vB
`�B
a-B
a�B
a�B
bNB
b�B
cTB
c�B
c�B
d@B
d�B
d�B
e�B
f�B
gB
gB
g8B
gRB
g�B
h>B
h�B
h�B
i*B
i*B
iyB
i�B
i�B
jB
j0B
jKB
jKB
jKB
j�B
k�B
l"B
l"B
l=B
m)B
mwB
m�B
nB
nB
n/B
nIB
n�B
n�B
o5B
o5B
oiB
o�B
o�B
p!B
p;B
p;B
poB
poB
p�B
p�B
p�B
qB
q'B
qAB
q[B
q�B
q�B
q�B
q�B
r-B
r-B
raB
r|B
r|B
r�B
s3B
sMB
tB
tTB
tnB
t�B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v+B
v+B
vFB
v`B
v�B
vzB
vzB
v�B
v�B
w2B
wfB
wfB
w�B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
w�B
xB
w�B
x8B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
zxB
zxB
z�B
{JB
{JB
{�B
{�B
{�B
{�B
|PB
|jB
|�B
|�B
}VB
}VB
}�B
~B
~B
~B
~]B
~]B
~�B
~�B
.B
B
.B
HB
}B
�B
� B
�B
� B
�B
� B
�B
� B
� B
�B
�OB
�iB
��B
��B
�B
� B
�oB
�;B
�oB
��B
��B
��B
��B
�'B
�AB
�[B
��B
��B
��B
�-B
�GB
�{B
��B
��B
�MB
�gB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�9B
�9B
�mB
�9B
�9B
�9B
�9B
�B
�9B
�9B
��B
��B
��B
�B
�%B
��B
��B
��B
��B
��B
�_B
��B
��B
��B
��B
��B
��B
�B
�1B
�1B
�1B
�1B
�1B
�B
�1B
�KB
��B
��B
�B
�7B
�7B
�7B
�7B
�7B
�7B
�lB
��B
��B
�XB
�XB
�=B
�XB
��B
��B
��B
��B
��B
��B
�B
�)B
�)B
��B
�DB
�DB
�)B
�xB
�xB
�DB
�xB
�^B
�xB
��B
��B
��B
��B
��B
��B
��B
��B
�0B
�01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�%B��B��B��B�YB��B�B��B��B��B	�B	�B	MB	MB	2B	�B	B	SB	SB	�B	�B	�B	 B	"�B	"�B	#�B	%�B	-�B	6�B	?cB	<6B	8�B	:DB	L�B	mB	u�B	~]B	�B
1B
�B
KB
�B
&B
�B	��B	��B	�HB	ݲB	��B	�B	�[B	�zB	��B	�KB	�aB	xRB	oOB	c B	`BB	W�B	R:B	L~B	CB	:�B	5?B	.�B	-�B	-wB	/�B	2�B	9rB	VB	b�B	gB	l"B	q�B	zDB	.B	�9B	�jB	��B	��B	�dB	x�B	e�B	W�B	L~B	DgB	@�B	A;B	>]B	<�B	CGB	G�B	KB	J�B	J	B	G+B	D�B	C�B	B�B	FtB	L0B	N�B	Q�B	RoB	`�B	l=B	n�B	t�B	lWB	_!B	_VB	\xB	Z�B	_�B	P�B	TB	Y�B	T{B	^�B	t�B	t�B	r-B	m�B	t�B	�B	��B	��B	�_B	�zB	��B	�VB	�<B	��B	�6B	��B	�B	��B	�XB	��B	��B	�MB	�-B	�;B	�[B	��B	�;B	�oB	��B	�mB	�9B	��B	�MB	��B	��B	��B	�QB	��B	��B	��B	�|B	��B	��B	��B	�,B	�B	�2B	��B	�`B	��B	��B	��B	��B	�DB	��B	�iB	��B	�8B	�B	�oB	āB	żB	��B	�B	ƎB	ǮB	��B	�B	ÖB	�;B	��B	�B	��B	�oB	��B	�B	��B	��B	��B	ŢB	�YB	ƨB	�+B	�B	ɺB	͹B	ЗB	ΥB	ՁB	�MB	ԯB	�@B	�$B	�B	��B	�|B	�bB	�HB	�B	��B	�FB	��B	�B	��B	�B	��B	�B	�B	��B	�yB	�*B	�_B	�]B	��B	�)B	�B	��B	�=B	�>B	�B	�$B	��B	��B	�B	� B	��B	�B	�B	�,B	�B	�B	��B	��B	�]B	��B	��B	�ZB	��B	��B	��B	�-B	�OB	��B	�IB	�B	�|B	�!B	�B	�vB	��B	�B	��B	�B	��B	�B	� B	��B	�}B	�B	� B	�/B	��B	�}B	�B	��B	�[B	�AB	�[B	�B	�B	�9B	�nB	�TB	�B	��B	��B	�B	�B	��B	��B	��B	�rB	�B	��B	�2B	�LB	��B	��B	�fB	��B	�`B	��B	��B	��B	��B	�jB	�(B	�B
 OB
�B
�B
�B
�B
�B
�B
B
�B
+B
�B
zB
EB
EB
EB
	B
�B
�B
�B
	7B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B

	B

	B
	�B

XB

XB

�B

�B
DB
^B
�B
DB
)B
^B
JB
0B
B
JB
JB
B
0B
�B
B
�B
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
�B
oB
oB
�B
aB
�B
�B
�B
B

B
�B
�B
�B
�B
EB
+B
EB
B
�B
�B
sB
sB
�B
+B
B
eB
B
�B
7B
kB
kB
kB
�B
�B
�B
�B
�B
#B
qB
�B
�B
�B
�B
�B
]B
)B
]B
xB
/B
IB
B
�B
;B
!B
!B
�B
B
VB
B
B
�B
�B
B
!B
�B
 �B
 �B
!B
!bB
!�B
"�B
#B
$@B
$�B
$�B
$�B
%B
&�B
$�B
$&B
$tB
$�B
$�B
%B
%`B
%�B
%�B
%�B
&�B
'B
'mB
'mB
'�B
'�B
'�B
(sB
)*B
)DB
)�B
*0B
*�B
+�B
,�B
,�B
-B
-)B
-�B
-wB
-�B
-�B
.}B
.�B
.�B
/iB
/�B
/�B
0B
0�B
1AB
1[B
1vB
1AB
1�B
1�B
2aB
3�B
3�B
3�B
3�B
3�B
4B
4B
4�B
5%B
5B
5B
5%B
5tB
5�B
5�B
6B
6+B
6FB
6�B
6�B
6�B
7LB
7�B
7�B
7�B
8B
8�B
9>B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:B
:xB
:�B
:�B
;0B
<B
;�B
;JB
;B
;�B
<�B
<B
<jB
<PB
<�B
<PB
<B
;�B
;�B
<B
<B
<�B
<�B
<jB
<�B
="B
>B
>(B
>�B
?.B
?.B
?}B
?�B
?�B
@�B
A�B
B�B
CB
CB
B�B
CaB
C�B
C�B
DB
D�B
D�B
D�B
D�B
EB
ESB
EmB
E�B
E�B
E�B
FB
FtB
F�B
F�B
F�B
GB
G+B
HKB
IB
I7B
I�B
I�B
IlB
IlB
I�B
K^B
LB
L�B
L�B
L�B
M�B
M�B
NB
N�B
O\B
OBB
O�B
P�B
P�B
P�B
QNB
QhB
Q�B
Q�B
RB
R B
R B
R:B
RTB
R B
R B
R B
R B
R B
RB
RTB
RoB
R�B
R�B
R�B
R�B
S�B
T�B
U2B
UMB
U2B
T�B
T�B
U2B
UMB
UMB
U2B
U�B
VB
VB
W
B
W$B
W$B
WYB
W?B
WYB
WYB
W?B
WYB
W?B
XB
X+B
X+B
X�B
X�B
X�B
Y1B
X�B
X�B
X�B
Y�B
YB
Y�B
ZB
Z7B
ZQB
Y�B
ZB
ZB
Z�B
[�B
[�B
[�B
\B
\B
\B
\B
\�B
\xB
\�B
]B
]/B
]/B
]�B
]�B
]�B
^OB
^5B
^�B
^�B
^�B
^�B
^�B
_VB
_!B
^�B
]�B
]~B
\�B
]dB
]�B
]�B
]�B
^�B
_�B
`vB
`�B
a-B
a�B
a�B
bNB
b�B
cTB
c�B
c�B
d@B
d�B
d�B
e�B
f�B
gB
gB
g8B
gRB
g�B
h>B
h�B
h�B
i*B
i*B
iyB
i�B
i�B
jB
j0B
jKB
jKB
jKB
j�B
k�B
l"B
l"B
l=B
m)B
mwB
m�B
nB
nB
n/B
nIB
n�B
n�B
o5B
o5B
oiB
o�B
o�B
p!B
p;B
p;B
poB
poB
p�B
p�B
p�B
qB
q'B
qAB
q[B
q�B
q�B
q�B
q�B
r-B
r-B
raB
r|B
r|B
r�B
s3B
sMB
tB
tTB
tnB
t�B
u%B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vB
v+B
v+B
vFB
v`B
v�B
vzB
vzB
v�B
v�B
w2B
wfB
wfB
w�B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
w�B
xB
w�B
x8B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
zxB
zxB
z�B
{JB
{JB
{�B
{�B
{�B
{�B
|PB
|jB
|�B
|�B
}VB
}VB
}�B
~B
~B
~B
~]B
~]B
~�B
~�B
.B
B
.B
HB
}B
�B
� B
�B
� B
�B
� B
�B
� B
� B
�B
�OB
�iB
��B
��B
�B
� B
�oB
�;B
�oB
��B
��B
��B
��B
�'B
�AB
�[B
��B
��B
��B
�-B
�GB
�{B
��B
��B
�MB
�gB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�9B
�9B
�mB
�9B
�9B
�9B
�9B
�B
�9B
�9B
��B
��B
��B
�B
�%B
��B
��B
��B
��B
��B
�_B
��B
��B
��B
��B
��B
��B
�B
�1B
�1B
�1B
�1B
�1B
�B
�1B
�KB
��B
��B
�B
�7B
�7B
�7B
�7B
�7B
�7B
�lB
��B
��B
�XB
�XB
�=B
�XB
��B
��B
��B
��B
��B
��B
�B
�)B
�)B
��B
�DB
�DB
�)B
�xB
�xB
�DB
�xB
�^B
�xB
��B
��B
��B
��B
��B
��B
��B
��B
�0B
�01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221224154154  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221224154310  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221224154311  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221224154312                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221224154312  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221224154312  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221224155833                      G�O�G�O�G�O�                