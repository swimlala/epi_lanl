CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:25:11Z creation;2022-06-04T17:25:11Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604172511  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @������1   @���}�u1@,��
=p��c��G�{1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffBa��Bg��Bp  Bx  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B���B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.33C/�fC2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @,(�@\)@��G@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BX\)Ba�]Bg�]Bo��Bw��B��B��{B���B�ǮB���B���B���B���B���B���B���B�.B�ǮB���B�ǮB���B�.B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B�.B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC,C.0�C/��C1�qC3�qC5�qC7�qC9��C;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCrCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C��C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@��DAx�DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DKx�DK�\DL\DL�\DM\DM�\DN\DN�\DO��DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj��Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D���D�<{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�m�A�\]A�>BA�9$A�5A�5A�4�A�4�A�4�A�5�A�6FA�7�A�7�A�8A�7�A�7�A�7A�6zA�0�A�/�A�-A�"hA�A��A���Aۭ�A��Aڸ�A���A�h�A�b�A��dA�u�Aͭ�A���A��A�C�A�N<A���A�o�A�xA�}�A�w�A���A���A�[WA�1�A���A�CaA�%A�&LA���A��fA�rA�o�A��jA�s�A���A�A�_�A�5�A��zA���A���A�A��#A��A�/OA� �A���A��.A�yrA�A��A�A�A��A�ÖA���A���A��=A�s�A��+A��NA���A�JXA�%FA�Y�A�oiA~�4A{͟Ay6Atg8An�"Af�mAd�CAb��A`'�A]jA\%FAX*0AS�/AP��AM��AL�AJh�AG�AFCAE�'AE��AC�A@�A>XyA<��A:OvA7�1A5ɆA4-wA3�1A2�?A1��A1n/A1sA1�oA08�A-ɆA+��A*�pA)<6A'?A&��A&y>A%�A$��A#�5A#��A#�gA$3�A$A$�A$9XA#�PA!~�A�fAoAS&A��A'�A�WA$A��A��A:�A�hA��AdZA��A	�A{A�6A_�A�jAN<A<6A��A� A�A�PA\�AH�A|�A��A|�A0�A��A?}Ac�A��A�A��AcA
�)A
A	� A	�HA��A�A��A��A�uAXAMjA�A�A��A�AA��A�.A�A!�A�A�DA��A��AOA
=Av�A��AffA�NAtTA�A ��@��m@�@�4@��^@�@��o@��@�g8@��@�Z�@��c@�1@�ϫ@��@�T�@�-w@���@��@��@��+@��@��@�R�@�$@�!�@���@��@�g8@�+@��@�f�@겖@颜@��@�n�@�:*@��@�V@�Z�@�?@�B�@�~@��@�<6@�
=@�YK@��@��@�RT@�bN@ߠ'@�>�@ާ�@��@ݢ�@��B@�9X@۩�@ڸR@�V@��@��@�c@�+�@ف@�a�@���@�8�@���@׀4@��@��E@�a|@�G�@ԔF@��@��@ҋD@��o@н<@��T@�rG@�7L@��c@Ζ�@��@�(@�U2@��@���@˅�@��@ʬ@�Q@ɣn@��v@���@�L�@��@�ߤ@ȵ@ȧ�@�d�@�	�@Ǔ�@�Dg@���@Ƒ @�h�@��@�F@���@Ċr@�1@Ó�@���@¾@�<�@���@�|�@�!-@��@��O@�r�@��@�S@��D@��T@�o @�Y�@�Z�@��@�p;@��^@�|@��v@�7�@���@�O@��P@��"@�҉@��@�o@���@�!-@�&�@��@��j@�]d@�'R@���@��@�2�@��9@���@�*0@��L@�.�@��A@���@�g8@��w@��"@���@�}V@�?�@�PH@�l"@�z@��1@��u@�$@�� @�.I@�3�@��@�?�@�V@�?�@��@��@�9�@�@��@�N�@��@���@�P�@�+�@��@��@���@��@���@��K@��	@�Q@� �@��Z@��
@���@�~�@�;@��!@���@�u%@�s�@��@�Dg@�4�@�0�@�@���@��F@���@�~(@�M�@��@��@���@�u%@�PH@��@���@��@�7�@���@��@�$�@�  @���@��N@�x@��@�ߤ@���@���@���@�YK@�O@��@���@��@��D@�*�@��@��6@��:@�#�@��@�ߤ@���@�2�@���@�)_@���@�PH@�($@��z@�RT@��K@��@��@���@��@���@���@��f@��L@�V@��@���@���@�v`@�k�@�Dg@�5�@��@�ѷ@���@���@��A@�h�@�H@��@��j@��F@���@��$@�L�@��H@�]d@��T@���@��@��P@�rG@�N<@�4@�&@��@��/@��e@�n�@�)�@��)@��&@��@��h@�j@�Vm@�B�@�&�@���@�}V@�0U@�_@��@���@���@�qv@�iD@�H�@�!�@��@���@���@�E�@�@��9@���@�j@�1�@���@��@�	@���@���@�@���@���@�m�@��T@��C@���@��:@�+�@��]@�A�@��.@���@��M@��@�ߤ@�Q�@�e@�u@��@~ȴ@}�)@|�@{�
@z�B@zJ@y�@y@x�.@xU2@x�@wx@v��@v��@vJ�@u�@uhs@u	l@tS�@t!@tx@t	�@s�	@sE9@r�@rxl@r($@q�o@q��@q5�@p�@p6@o��@n��@n\�@m�D@m�@mY�@m�@l�@l!@k�@j��@i��@iw2@i/@i�@h�@hĜ@hq@h�@g�4@gF�@f�@fp;@e��@e=�@dی@doi@d?�@d~@c��@c��@cO@b��@b�A@b@�@b($@a�)@a|@a2a@`Ɇ@`��@`~(@`?�@_�6@_�{@^�M@^{�@^4@]�H@]��@]�@\��@\��@\4n@\  @[��@[+@Z�y@Z��@Zff@Y��@Y��@YY�@Y!�@Xѷ@X��@XFt@X(�@W�a@WJ#@W6z@V�"@V�'@V}V@U��@UO�@U5�@U;@Tq@S�@SiD@S!-@R�8@R��@R�@R�<@Ru%@ROv@R�@Q��@Q+�@Q�@Q@P��@P�5@P`�@O�
@O��@O;d@N҉@NOv@N0U@M�o@M��@M#�@L�/@L�$@Lr�@K��@J�@J3�@I��@I�=@I/@H��@H�u@H9X@G�@@G�@F��@F��@F~�@ES&@D�z@D$@C��@C�F@C��@C�@Cv`@CF�@C@B�y@B�}@B�\@Bq�@B�@A�T@A��@A/@@�@?~�@?C@>�M@>u%@=zx@=%@<ی@<�9@<�Y@<�@;�m@;˒@;�{@;x@;g�@;�@:�+@:�@9��@9�@9��@9o @9G�@9/@90�@9#�@9�@8��@8�@8-�@7l�@7(@6�@6L0@6!�@6@6�@5��@5�T@5�=@5q@4ѷ@4�p@4��@3� @3��@3��@3s@3=@3@2�]@2��@2q�@25?@1�>@1��@1[W@0ی@0�4@0[�@/��@/\)@/C@/�@.��@.v�@.5?@.@-�@-��@-J�@,�@,�$@,�O@,��@,Q�@,�@+خ@+��@+U�@+y�@+O@*��@*��@*�6@*s�@*6�@*�@*4@* �@)��@)�S@)Q�@)4@(��@(q@'�Q@'H�@'dZ@',�@&� @%�N@%Q�@%�@$�@$�$@$��@$��@$�@$|�@$e�@$:�@$@#�@#ƨ@#��@"�y@"��@"i�@"c @"Z�@"!�@!��@!��@!�~@!�@ �O@ l"@ 7�@ @� @�q@��@��@g�@H�@33@ i@��@�b@��@s�@#:@�'@?}@�@�`@�p@�@/�@��@��@Z�@F�@6z@�X@�A@@�@!�@@�j@�S@O�@@��@��@�@N�@!@1@ƨ@�@��@�f@U�@(@�H@�h@�A@@�@�3@p�@A @(�@�@�P@��@PH@%�@�A@��@�a@��@o�@1�@@�y@�m@�x@��@��@s�@)�@_@��@��@5�@5�@Dg@�f@��@y>@D�@M@ݘ@�0@�4@Z�@4�@��@ߤ@��@h
@YK@+k@�.@��@�N@��@�t@�7@[W@��@��@N�@-�@خ@j�@4�@4�@.I@�@
�r@
��@
{�@
n�@
i�@
J�@
#:@
4@	�N@	�@	c@	j@	7L@�K@ѷ@��@m�@Q�@PH111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�m�A�\]A�>BA�9$A�5A�5A�4�A�4�A�4�A�5�A�6FA�7�A�7�A�8A�7�A�7�A�7A�6zA�0�A�/�A�-A�"hA�A��A���Aۭ�A��Aڸ�A���A�h�A�b�A��dA�u�Aͭ�A���A��A�C�A�N<A���A�o�A�xA�}�A�w�A���A���A�[WA�1�A���A�CaA�%A�&LA���A��fA�rA�o�A��jA�s�A���A�A�_�A�5�A��zA���A���A�A��#A��A�/OA� �A���A��.A�yrA�A��A�A�A��A�ÖA���A���A��=A�s�A��+A��NA���A�JXA�%FA�Y�A�oiA~�4A{͟Ay6Atg8An�"Af�mAd�CAb��A`'�A]jA\%FAX*0AS�/AP��AM��AL�AJh�AG�AFCAE�'AE��AC�A@�A>XyA<��A:OvA7�1A5ɆA4-wA3�1A2�?A1��A1n/A1sA1�oA08�A-ɆA+��A*�pA)<6A'?A&��A&y>A%�A$��A#�5A#��A#�gA$3�A$A$�A$9XA#�PA!~�A�fAoAS&A��A'�A�WA$A��A��A:�A�hA��AdZA��A	�A{A�6A_�A�jAN<A<6A��A� A�A�PA\�AH�A|�A��A|�A0�A��A?}Ac�A��A�A��AcA
�)A
A	� A	�HA��A�A��A��A�uAXAMjA�A�A��A�AA��A�.A�A!�A�A�DA��A��AOA
=Av�A��AffA�NAtTA�A ��@��m@�@�4@��^@�@��o@��@�g8@��@�Z�@��c@�1@�ϫ@��@�T�@�-w@���@��@��@��+@��@��@�R�@�$@�!�@���@��@�g8@�+@��@�f�@겖@颜@��@�n�@�:*@��@�V@�Z�@�?@�B�@�~@��@�<6@�
=@�YK@��@��@�RT@�bN@ߠ'@�>�@ާ�@��@ݢ�@��B@�9X@۩�@ڸR@�V@��@��@�c@�+�@ف@�a�@���@�8�@���@׀4@��@��E@�a|@�G�@ԔF@��@��@ҋD@��o@н<@��T@�rG@�7L@��c@Ζ�@��@�(@�U2@��@���@˅�@��@ʬ@�Q@ɣn@��v@���@�L�@��@�ߤ@ȵ@ȧ�@�d�@�	�@Ǔ�@�Dg@���@Ƒ @�h�@��@�F@���@Ċr@�1@Ó�@���@¾@�<�@���@�|�@�!-@��@��O@�r�@��@�S@��D@��T@�o @�Y�@�Z�@��@�p;@��^@�|@��v@�7�@���@�O@��P@��"@�҉@��@�o@���@�!-@�&�@��@��j@�]d@�'R@���@��@�2�@��9@���@�*0@��L@�.�@��A@���@�g8@��w@��"@���@�}V@�?�@�PH@�l"@�z@��1@��u@�$@�� @�.I@�3�@��@�?�@�V@�?�@��@��@�9�@�@��@�N�@��@���@�P�@�+�@��@��@���@��@���@��K@��	@�Q@� �@��Z@��
@���@�~�@�;@��!@���@�u%@�s�@��@�Dg@�4�@�0�@�@���@��F@���@�~(@�M�@��@��@���@�u%@�PH@��@���@��@�7�@���@��@�$�@�  @���@��N@�x@��@�ߤ@���@���@���@�YK@�O@��@���@��@��D@�*�@��@��6@��:@�#�@��@�ߤ@���@�2�@���@�)_@���@�PH@�($@��z@�RT@��K@��@��@���@��@���@���@��f@��L@�V@��@���@���@�v`@�k�@�Dg@�5�@��@�ѷ@���@���@��A@�h�@�H@��@��j@��F@���@��$@�L�@��H@�]d@��T@���@��@��P@�rG@�N<@�4@�&@��@��/@��e@�n�@�)�@��)@��&@��@��h@�j@�Vm@�B�@�&�@���@�}V@�0U@�_@��@���@���@�qv@�iD@�H�@�!�@��@���@���@�E�@�@��9@���@�j@�1�@���@��@�	@���@���@�@���@���@�m�@��T@��C@���@��:@�+�@��]@�A�@��.@���@��M@��@�ߤ@�Q�@�e@�u@��@~ȴ@}�)@|�@{�
@z�B@zJ@y�@y@x�.@xU2@x�@wx@v��@v��@vJ�@u�@uhs@u	l@tS�@t!@tx@t	�@s�	@sE9@r�@rxl@r($@q�o@q��@q5�@p�@p6@o��@n��@n\�@m�D@m�@mY�@m�@l�@l!@k�@j��@i��@iw2@i/@i�@h�@hĜ@hq@h�@g�4@gF�@f�@fp;@e��@e=�@dی@doi@d?�@d~@c��@c��@cO@b��@b�A@b@�@b($@a�)@a|@a2a@`Ɇ@`��@`~(@`?�@_�6@_�{@^�M@^{�@^4@]�H@]��@]�@\��@\��@\4n@\  @[��@[+@Z�y@Z��@Zff@Y��@Y��@YY�@Y!�@Xѷ@X��@XFt@X(�@W�a@WJ#@W6z@V�"@V�'@V}V@U��@UO�@U5�@U;@Tq@S�@SiD@S!-@R�8@R��@R�@R�<@Ru%@ROv@R�@Q��@Q+�@Q�@Q@P��@P�5@P`�@O�
@O��@O;d@N҉@NOv@N0U@M�o@M��@M#�@L�/@L�$@Lr�@K��@J�@J3�@I��@I�=@I/@H��@H�u@H9X@G�@@G�@F��@F��@F~�@ES&@D�z@D$@C��@C�F@C��@C�@Cv`@CF�@C@B�y@B�}@B�\@Bq�@B�@A�T@A��@A/@@�@?~�@?C@>�M@>u%@=zx@=%@<ی@<�9@<�Y@<�@;�m@;˒@;�{@;x@;g�@;�@:�+@:�@9��@9�@9��@9o @9G�@9/@90�@9#�@9�@8��@8�@8-�@7l�@7(@6�@6L0@6!�@6@6�@5��@5�T@5�=@5q@4ѷ@4�p@4��@3� @3��@3��@3s@3=@3@2�]@2��@2q�@25?@1�>@1��@1[W@0ی@0�4@0[�@/��@/\)@/C@/�@.��@.v�@.5?@.@-�@-��@-J�@,�@,�$@,�O@,��@,Q�@,�@+خ@+��@+U�@+y�@+O@*��@*��@*�6@*s�@*6�@*�@*4@* �@)��@)�S@)Q�@)4@(��@(q@'�Q@'H�@'dZ@',�@&� @%�N@%Q�@%�@$�@$�$@$��@$��@$�@$|�@$e�@$:�@$@#�@#ƨ@#��@"�y@"��@"i�@"c @"Z�@"!�@!��@!��@!�~@!�@ �O@ l"@ 7�@ @� @�q@��@��@g�@H�@33@ i@��@�b@��@s�@#:@�'@?}@�@�`@�p@�@/�@��@��@Z�@F�@6z@�X@�A@@�@!�@@�j@�S@O�@@��@��@�@N�@!@1@ƨ@�@��@�f@U�@(@�H@�h@�A@@�@�3@p�@A @(�@�@�P@��@PH@%�@�A@��@�a@��@o�@1�@@�y@�m@�x@��@��@s�@)�@_@��@��@5�@5�@Dg@�f@��@y>@D�@M@ݘ@�0@�4@Z�@4�@��@ߤ@��@h
@YK@+k@�.@��@�N@��@�t@�7@[W@��@��@N�@-�@خ@j�@4�@4�@.I@�@
�r@
��@
{�@
n�@
i�@
J�@
#:@
4@	�N@	�@	c@	j@	7L@�K@ѷ@��@m�@Q�@PH111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	@ B	?B	?.B	?HB	?B	?HB	?.B	?.B	?HB	?HB	?cB	?cB	?HB	?HB	?HB	?HB	?.B	?.B	?B	?B	>�B	>�B	>wB	>BB	=VB	<6B	7�B	3hB	1�B	3B	2�B	6�B	G�B	��B	��B	ȚB	�&B	�&B	��B	�HB
�B
�B
j�B
��B
�BB
��B<�BV�B]BdtByrB��B��B��B�+B� B��B��B�B��B��Bv�BtBd�BZQBO�BAUB4�B(>BBpB�B
�xB
��B
�pB
�aB
~]B
poB
mB
g�B
]�B
Q B
@�B
0;B
)yB
'�B
�B
�B	�rB
;B
B	�B	�JB	_!B	GB	;�B	)�B	�B	�B	KB�tB�eB�B�nB�B�HBԕB�&B� B��BܒBچB��B�~BںB��B�B�MB	 B	vB	�B	2B	�B	.IB	*�B	=B	�B	VB	:B	gB	#�B	AUB	K�B	H�B	NpB	l"B	��B	��B	�ZB	�GB	��B	�OB	��B	��B	��B	��B	��B	�B	�B	��B	��B	�VB	�QB	�$B	�MB	��B	�VB	��B	��B	��B	�`B	�B	�
B	��B	�B	��B	�)B	��B	��B	�iB	��B	�(B	�qB	�.B	�aB	�_B	��B	��B	�JB	ɠB	��B	˒B	�.B	ӏB	ҽB	��B	�@B	ԕB	�9B	��B	��B	��B	�FB	��B	�&B	�}B	�B	�\B	�"B	̈́B	͹B	�<B	�B	��B	��B	��B	�5B	�BB	�pB	�~B	�WB	�eB	��B	�:B	ϫB	�}B	��B	�(B	�uB	ںB	ݘB	�SB	ӏB	�oB	�gB	�B	�B	�B	��B	�fB	�B	�$B	�XB	��B	�0B	��B	�_B	�_B	��B	�IB	�wB	�B	�B	�B	�B	�*B	�B	�B	�=B	�}B	�}B	�iB	��B	��B	��B	�=B	��B	��B	�B	�)B	�)B	�B	�IB	�wB	�"B	�B	�CB	�IB	�]B	�CB	�B	��B	�B	��B	�"B	��B	�'B	��B	�|B	�|B	�-B	��B	�'B	��B	�UB	� B	��B	�OB	�B	�]B	��B	��B	�]B	�]B	�wB	�)B	��B	�]B	�B	�=B	�qB	�B	�wB	�}B	�}B	�B	�B	��B	��B	� B	�B	�iB	�B	��B	��B	�B	�B	�B	��B	�GB	��B	�B	��B	�B	��B	�B	��B	�B	�B	�B	�3B	�ZB	��B	�FB	�FB	�+B	�zB	�tB	��B	��B	�B	�B	��B	��B	�B	��B	��B	�XB	�B	��B	�zB	��B	�RB	��B	��B	�B	�B	�DB	��B	��B	��B	�6B	�B	��B	�]B
�B
�B
�B
UB
�B
�B
�B
�B
 B
  B	��B	��B	��B	�6B	��B	��B
'B
tB
	RB
	�B
	�B
�B
B
B
�B
�B
	�B
	B
�B
	B
	B
	7B
	�B
	�B
	�B
	�B

#B

�B

�B
~B
�B
�B
�B
vB
�B
�B
HB
 B
B
B
B
vB
�B
�B
�B
�B
�B
�B
�B
�B
mB
?B

B
�B
�B

B

B
$B
?B
$B
?B

B
B
�B
�B
sB
�B
�B
�B
�B
EB
EB
�B
�B
�B
�B
eB
�B
�B
7B
QB
�B
�B
�B
�B
�B
qB
�B
�B
)B
�B
�B
/B
B
~B
IB
]B
�B
�B
�B
B
dB
OB
�B
OB
pB
!-B
!�B
!-B
 �B
!HB
!bB
!|B
!�B
!�B
"4B
"�B
# B
#nB
$B
$�B
%FB
%�B
&fB
&�B
&�B
&fB
&�B
%�B
%,B
$@B
$�B
%zB
&�B
'RB
(�B
(�B
(�B
)_B
*0B
*�B
+B
+6B
+�B
,B
,B
,B
,B
,=B
,WB
,�B
-]B
-]B
-�B
-�B
-�B
.B
.IB
.IB
.B
.}B
.�B
.�B
/ B
/B
/OB
/�B
/�B
/�B
/�B
0B
0�B
0�B
1B
1B
1AB
1�B
2B
1�B
2aB
3MB
3�B
3�B
3MB
3�B
3�B
4�B
4�B
5tB
5%B
5?B
5B
6zB
6`B
6zB
6�B
7�B
8B
8RB
7�B
9	B
9	B
9$B
9�B
9�B
:B
:*B
:DB
:xB
:�B
:�B
:�B
;JB
;0B
;JB
;dB
;B
;JB
;�B
;�B
;�B
<6B
<PB
<jB
<�B
="B
="B
=VB
=<B
=�B
>B
>(B
>B
>�B
>�B
>�B
?.B
?}B
@4B
@�B
@�B
AB
AB
AB
A B
AUB
A�B
BB
BB
BAB
B[B
B�B
CB
C{B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
DMB
DgB
DgB
D�B
D�B
D�B
ESB
EmB
EmB
E�B
E�B
FB
FtB
FtB
F�B
F�B
F�B
GzB
G�B
G�B
G�B
G�B
HB
HKB
HfB
H�B
H�B
IB
IB
IRB
IRB
I�B
I�B
I�B
J	B
JrB
J�B
J�B
J�B
J�B
KDB
K�B
LB
K�B
LB
L�B
MB
MPB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
NpB
N<B
N<B
NVB
N"B
N�B
OB
OBB
OvB
O�B
PHB
P.B
PHB
P�B
P�B
Q B
QB
Q4B
Q�B
R:B
R�B
S&B
SuB
S�B
S�B
T,B
TFB
T�B
U2B
U2B
UB
U2B
V9B
V�B
V�B
W$B
W$B
W?B
W$B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
X+B
X+B
X_B
XyB
YB
Y�B
Y�B
ZB
ZB
[WB
[WB
[�B
[�B
[�B
\B
\B
\)B
\CB
\CB
\]B
\�B
]B
]IB
]dB
]�B
]�B
]�B
]�B
^B
^B
^B
^B
^B
^B
^�B
_!B
_VB
_�B
_�B
`B
`B
_�B
`'B
`B
`'B
`�B
`�B
`�B
`�B
a�B
a|B
a|B
a�B
a�B
a�B
a�B
bB
bhB
bhB
b�B
b�B
b�B
cTB
c:B
cnB
c�B
dtB
dtB
d�B
d�B
eB
ezB
ezB
e�B
e�B
e�B
fLB
fLB
ffB
fLB
f�B
f�B
g�B
g�B
h
B
hsB
iB
h�B
h�B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
i�B
j�B
j�B
j�B
j�B
k6B
k�B
l�B
l�B
lqB
lWB
l�B
l�B
mB
mB
m)B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n}B
o�B
o�B
o�B
o�B
o�B
p;B
p;B
p�B
q'B
q�B
q�B
q�B
qvB
q[B
q�B
r�B
r�B
r�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
tB
t9B
t�B
t�B
u?B
utB
u�B
v+B
v�B
v�B
v�B
wB
w�B
w�B
xB
xB
xB
xB
xB
xlB
y>B
y�B
y�B
zB
z*B
z*B
z�B
z�B
z�B
z�B
{0B
{0B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
|�B
|PB
|�B
|�B
}B
}�B
~B
~(B
~BB
~]B
~]B
~�B
~�B
~�B
.B
.B
.B
cB
�B
�B
�B
HB
B
.B
cB
cB
B
HB
cB
�B
�4B
�4B
�OB
�4B
��B
��B
��B
�B
�;B
�;B
�oB
��B
��B
��B
��B
��B
��B
�B
��B
�uB
�[B
��B
�B
�GB
�B
�B
�GB
�B
��B
��B
��B
��B
��B
�B
��B
�B
��B
�MB
�gB
�gB
��B
�B
�B
�mB
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	@ B	?B	?.B	?HB	?B	?HB	?.B	?.B	?HB	?HB	?cB	?cB	?HB	?HB	?HB	?HB	?.B	?.B	?B	?B	>�B	>�B	>wB	>BB	=VB	<6B	7�B	3hB	1�B	3B	2�B	6�B	G�B	��B	��B	ȚB	�&B	�&B	��B	�HB
�B
�B
j�B
��B
�BB
��B<�BV�B]BdtByrB��B��B��B�+B� B��B��B�B��B��Bv�BtBd�BZQBO�BAUB4�B(>BBpB�B
�xB
��B
�pB
�aB
~]B
poB
mB
g�B
]�B
Q B
@�B
0;B
)yB
'�B
�B
�B	�rB
;B
B	�B	�JB	_!B	GB	;�B	)�B	�B	�B	KB�tB�eB�B�nB�B�HBԕB�&B� B��BܒBچB��B�~BںB��B�B�MB	 B	vB	�B	2B	�B	.IB	*�B	=B	�B	VB	:B	gB	#�B	AUB	K�B	H�B	NpB	l"B	��B	��B	�ZB	�GB	��B	�OB	��B	��B	��B	��B	��B	�B	�B	��B	��B	�VB	�QB	�$B	�MB	��B	�VB	��B	��B	��B	�`B	�B	�
B	��B	�B	��B	�)B	��B	��B	�iB	��B	�(B	�qB	�.B	�aB	�_B	��B	��B	�JB	ɠB	��B	˒B	�.B	ӏB	ҽB	��B	�@B	ԕB	�9B	��B	��B	��B	�FB	��B	�&B	�}B	�B	�\B	�"B	̈́B	͹B	�<B	�B	��B	��B	��B	�5B	�BB	�pB	�~B	�WB	�eB	��B	�:B	ϫB	�}B	��B	�(B	�uB	ںB	ݘB	�SB	ӏB	�oB	�gB	�B	�B	�B	��B	�fB	�B	�$B	�XB	��B	�0B	��B	�_B	�_B	��B	�IB	�wB	�B	�B	�B	�B	�*B	�B	�B	�=B	�}B	�}B	�iB	��B	��B	��B	�=B	��B	��B	�B	�)B	�)B	�B	�IB	�wB	�"B	�B	�CB	�IB	�]B	�CB	�B	��B	�B	��B	�"B	��B	�'B	��B	�|B	�|B	�-B	��B	�'B	��B	�UB	� B	��B	�OB	�B	�]B	��B	��B	�]B	�]B	�wB	�)B	��B	�]B	�B	�=B	�qB	�B	�wB	�}B	�}B	�B	�B	��B	��B	� B	�B	�iB	�B	��B	��B	�B	�B	�B	��B	�GB	��B	�B	��B	�B	��B	�B	��B	�B	�B	�B	�3B	�ZB	��B	�FB	�FB	�+B	�zB	�tB	��B	��B	�B	�B	��B	��B	�B	��B	��B	�XB	�B	��B	�zB	��B	�RB	��B	��B	�B	�B	�DB	��B	��B	��B	�6B	�B	��B	�]B
�B
�B
�B
UB
�B
�B
�B
�B
 B
  B	��B	��B	��B	�6B	��B	��B
'B
tB
	RB
	�B
	�B
�B
B
B
�B
�B
	�B
	B
�B
	B
	B
	7B
	�B
	�B
	�B
	�B

#B

�B

�B
~B
�B
�B
�B
vB
�B
�B
HB
 B
B
B
B
vB
�B
�B
�B
�B
�B
�B
�B
�B
mB
?B

B
�B
�B

B

B
$B
?B
$B
?B

B
B
�B
�B
sB
�B
�B
�B
�B
EB
EB
�B
�B
�B
�B
eB
�B
�B
7B
QB
�B
�B
�B
�B
�B
qB
�B
�B
)B
�B
�B
/B
B
~B
IB
]B
�B
�B
�B
B
dB
OB
�B
OB
pB
!-B
!�B
!-B
 �B
!HB
!bB
!|B
!�B
!�B
"4B
"�B
# B
#nB
$B
$�B
%FB
%�B
&fB
&�B
&�B
&fB
&�B
%�B
%,B
$@B
$�B
%zB
&�B
'RB
(�B
(�B
(�B
)_B
*0B
*�B
+B
+6B
+�B
,B
,B
,B
,B
,=B
,WB
,�B
-]B
-]B
-�B
-�B
-�B
.B
.IB
.IB
.B
.}B
.�B
.�B
/ B
/B
/OB
/�B
/�B
/�B
/�B
0B
0�B
0�B
1B
1B
1AB
1�B
2B
1�B
2aB
3MB
3�B
3�B
3MB
3�B
3�B
4�B
4�B
5tB
5%B
5?B
5B
6zB
6`B
6zB
6�B
7�B
8B
8RB
7�B
9	B
9	B
9$B
9�B
9�B
:B
:*B
:DB
:xB
:�B
:�B
:�B
;JB
;0B
;JB
;dB
;B
;JB
;�B
;�B
;�B
<6B
<PB
<jB
<�B
="B
="B
=VB
=<B
=�B
>B
>(B
>B
>�B
>�B
>�B
?.B
?}B
@4B
@�B
@�B
AB
AB
AB
A B
AUB
A�B
BB
BB
BAB
B[B
B�B
CB
C{B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
DMB
DgB
DgB
D�B
D�B
D�B
ESB
EmB
EmB
E�B
E�B
FB
FtB
FtB
F�B
F�B
F�B
GzB
G�B
G�B
G�B
G�B
HB
HKB
HfB
H�B
H�B
IB
IB
IRB
IRB
I�B
I�B
I�B
J	B
JrB
J�B
J�B
J�B
J�B
KDB
K�B
LB
K�B
LB
L�B
MB
MPB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
NpB
N<B
N<B
NVB
N"B
N�B
OB
OBB
OvB
O�B
PHB
P.B
PHB
P�B
P�B
Q B
QB
Q4B
Q�B
R:B
R�B
S&B
SuB
S�B
S�B
T,B
TFB
T�B
U2B
U2B
UB
U2B
V9B
V�B
V�B
W$B
W$B
W?B
W$B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
X+B
X+B
X_B
XyB
YB
Y�B
Y�B
ZB
ZB
[WB
[WB
[�B
[�B
[�B
\B
\B
\)B
\CB
\CB
\]B
\�B
]B
]IB
]dB
]�B
]�B
]�B
]�B
^B
^B
^B
^B
^B
^B
^�B
_!B
_VB
_�B
_�B
`B
`B
_�B
`'B
`B
`'B
`�B
`�B
`�B
`�B
a�B
a|B
a|B
a�B
a�B
a�B
a�B
bB
bhB
bhB
b�B
b�B
b�B
cTB
c:B
cnB
c�B
dtB
dtB
d�B
d�B
eB
ezB
ezB
e�B
e�B
e�B
fLB
fLB
ffB
fLB
f�B
f�B
g�B
g�B
h
B
hsB
iB
h�B
h�B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
i�B
j�B
j�B
j�B
j�B
k6B
k�B
l�B
l�B
lqB
lWB
l�B
l�B
mB
mB
m)B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n}B
o�B
o�B
o�B
o�B
o�B
p;B
p;B
p�B
q'B
q�B
q�B
q�B
qvB
q[B
q�B
r�B
r�B
r�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
tB
t9B
t�B
t�B
u?B
utB
u�B
v+B
v�B
v�B
v�B
wB
w�B
w�B
xB
xB
xB
xB
xB
xlB
y>B
y�B
y�B
zB
z*B
z*B
z�B
z�B
z�B
z�B
{0B
{0B
{�B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
|�B
|PB
|�B
|�B
}B
}�B
~B
~(B
~BB
~]B
~]B
~�B
~�B
~�B
.B
.B
.B
cB
�B
�B
�B
HB
B
.B
cB
cB
B
HB
cB
�B
�4B
�4B
�OB
�4B
��B
��B
��B
�B
�;B
�;B
�oB
��B
��B
��B
��B
��B
��B
�B
��B
�uB
�[B
��B
�B
�GB
�B
�B
�GB
�B
��B
��B
��B
��B
��B
�B
��B
�B
��B
�MB
�gB
�gB
��B
�B
�B
�mB
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104848  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172511  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172511  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172511                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022519  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022519  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                