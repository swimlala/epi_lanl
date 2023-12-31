CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:25:04Z creation;2022-06-04T19:25:05Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192504  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               SA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�v�[�1   @�v�S��@+�bM���d
�\(��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�33@�33A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ffB  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB��B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�ffB�  B���B�  B�ffB�  B�33B�ffB㙚B�  B�  B�  B���B�  B�  C   C  C  C  C  C
�C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0L�C1�fC4  C6  C8�C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT33CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"�fD#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�\@��G@��GAp�A�
A?�
A_�
A�
A��A��A��A��A��A��A�RB \)B��B��B\)B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bp\)Bx\)B�]B�ǮB���B���B���B���B���B���B���B���B���B���B���B���B���B�aGB�.B�aGB���B�ǮB���B�aGB���B�.B�aGB�{B���B���B���B�ǮB���B���B���C�qC�qC�qC�qC
C�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC0J>C1��C3�qC5�qC8C9�qC;��C=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCT0�CU�qCW�qCY�qC[��C]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D"�D"��D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aͪ�AͦLAͨ$AͫkAͫkAͰUAͬ�Aͮ}AͯAͭ�Aͨ�Aͣ�Aͪ�Aͱ�AͰ!AͯAͨ�Aͥ�A�lWA�i�A�OA�7�A�5A�1'A�.�A�)�A�'�A�!�A��A��A��A��A̶�A�~A���A��A�XyA��mAŭCA���AŹ$AŵA��#Aì�A�l"A���A�m]A�DA��A��A���A��mA�xlA���A�]/A��(A�	A���A��OA�_�A�WsA�&LA�8A�N�A��BA�6�A�jA��_A�l�A��EA��$A��A���A��[A�&A�	A�@OA���A��DA�a�A��A��IA���A���A��FA��VA�.IA�<jA�s�A�A���A�cTA��2AZ�Ass�Aj�Ae�wA`�A]�fAX�NAT�\AQ��AOeAK��AH7�AE6zAA��A?dZA?�A>��A=��A<��A;��A9�XA6��A5oA4u�A3��A4cA4��A3�9A3�LA3zxA2�pA3aA3��A3�!A3��A3��A1ZA0g8A0,=A0JA0L0A/ iA,�4A)��A(4nA'�A(�A(��A)K�A)�PA)*0A(ϫA(�A(�A(XA'�EA'�A'H�A'*�A&��A&�wA&n/A&�A%�+A%	A$��A$VmA#��A"��A!�A!��A!=A z�A��AĜA�AO�A�WAA��A�-AiDA  A�'A��Aw�A=qA�HA��AS�A��A��A:�A}VA�XA�|A@�Av�A�.Ah�A=Ab�Ap;AqAMjA��A��A��A-�A�AA� A*0A�;A'�A
��A
"hA	��A	J#A�{AI�A�A7�A��Av�A+A�xAFA6AoA�PAɆA(�A��A��AdZA0�AߤA��A+A ��A ]�@�k�@�Y@���@��@��2@�(�@�`B@��H@���@���@�$�@�V@��h@�[�@��@�a@��,@�($@�%@�_�@��@�@���@�1'@�f@���@�U2@�8@��@�C@���@�/@��@��@�)�@�@�H@��&@���@�o�@�j@鹌@�ی@�s�@�e�@�o@蟾@��@�&�@思@�6@��@�˒@�Z�@��'@�Z�@��)@�Z�@��@�b�@���@��@�4@��_@�q�@�R�@ߙ�@ޞ@��@�S�@��@�%F@څ�@�)�@ٿH@ُ�@�O@��X@�L0@���@�`B@� �@�W?@�;d@�q@�@ԣ�@�Z�@���@Ұ!@�[�@�=@Ї�@�K^@Ϡ�@��@ͶF@���@̈�@�A�@��z@��|@ʀ�@�B[@�b@��9@�o�@��@��'@���@��@��@�N�@��9@Ĺ�@��Z@ä@@��@µ@��z@���@�%F@���@���@�@�@���@��*@��@�q�@���@��@��b@��@�ƨ@�o@��<@��@�"h@��3@��f@�+@��|@��6@�9X@��X@�X@��m@�]d@���@�0�@���@�;�@��@��y@�� @�e@���@�ߤ@���@�xl@��@���@��@�q@�ϫ@��~@�?}@��@��<@�@�t�@�c�@�a@�B�@��1@���@�U2@��@��k@�J�@��@��@���@�tT@�_@���@��~@�o @�Z�@��v@�%�@���@���@�x@��@��X@��z@�I�@�
�@�ݘ@���@�-w@��K@���@��6@�~(@�]d@��@���@�|�@�E9@�\�@��@��H@���@��q@��f@�8�@�(@���@�Ɇ@��+@���@��k@�:�@��@��P@���@���@�1�@��@�a@�"�@���@��B@���@� �@��@���@�d�@�9X@�($@�e@��@��@�o�@�	l@��_@�Z�@��@��n@�k�@�!-@���@��@�]d@�C�@� �@���@���@�g�@�#�@��@��1@�\�@�!@��@��F@��:@�Y@�֡@��\@�7�@��Z@���@�Q�@��@��@�:*@��m@�|�@�6z@��@��e@�R�@��@���@��*@��@@��h@�8�@���@���@��@�rG@�E9@���@��f@��5@��@��U@�E�@���@��7@��@��8@��]@��R@�J�@�$�@��W@���@�9�@��M@��U@��@�h�@�.�@�	@��#@��7@�
=@��@�B[@���@���@�RT@���@�i�@��@��@S@~n�@}��@}�@|A�@{J#@z��@y�d@x�@w=@v�@v��@vkQ@v@u��@u�@tN�@t�@s8@r�@rR�@q��@q	l@pɆ@p�D@p�@o�}@o9�@nxl@m�.@m�H@mY�@l��@l��@lH@l6@l7@k�
@k&@j�m@j}V@jO@ij@h�|@hM@giD@f�X@fa|@f$�@e�X@d�[@dU2@d~@c�6@c�4@c�@b��@b.�@a��@a�~@`ی@`PH@`b@_��@_�@_S�@_"�@^�M@^��@^c @]�@]f�@\��@\c�@\�@[�g@[��@[e�@[&@Z�@Z�L@Z �@X�f@X�j@X�@W��@Wx@WP�@WW?@W�@V�R@V($@U��@U?}@T�_@TPH@S�@S��@S�:@S|�@S4�@Rں@Ra|@Q�D@Q��@Q+�@P��@P��@P~(@P9X@O��@O��@Oy�@OS@N�}@Nxl@N=q@M�@M�7@MN<@L��@L��@L~(@LA�@K�@K��@K|�@K�@J�@J�H@J��@JW�@J�@I��@I%F@H�@H9X@H �@H�@G�]@G�@G�Q@G˒@G�@GiD@GO@G6z@F�y@F�F@Fq�@Fe@E�t@E�7@Em]@EVm@EJ�@E0�@E�@D�P@D֡@D��@D�@D<�@C��@CP�@B�H@B��@B-@A�.@A�@A�@A�@As�@Aa�@A%F@@ѷ@@z�@@M@@(�@@�@?�+@?��@?j�@?/�@>�@>�L@>q�@>Q@>@=�@=�n@=|@=:�@<�K@<��@<U2@<b@;�m@;�@;��@;S�@;�@:�b@:^5@:@�@:e@9�@9�@9L�@8�@8�$@8l"@8*�@7�Q@7�4@76z@7�@6�X@6�r@6p;@6H�@6!�@5�@5�=@5�@5IR@3��@3{J@3Mj@34�@3J#@31�@2�2@2�2@2��@2i�@2J�@25?@2�@1�z@1e,@1@0�?@0U2@0b@/��@//�@.�@.GE@-�o@-��@-a�@-G�@-/@-@,�$@,~(@,6@+�@+�@+o@*�L@)�@)��@)o @)@(Ɇ@(~(@'�m@'��@'W?@')_@'�@&�<@&�x@&p;@&YK@&?@%��@%�X@%0�@$�5@$�E@$|�@$K^@$1'@$�@$�@$�@$@#�0@#|�@#S�@#4�@#.I@#&@#&@"�8@"�m@"�m@"��@":*@!�@!�z@!Y�@!/@!�@!V@!V@!�@!+@ �)@ j@ N�@ <�@  �@�@��@��@W?@�@��@��@�H@�,@��@c @6�@($@�@@��@��@e,@/@ی@�@�@u�@Z@	�@�@�;@��@�0@�P@,�@�,@��@?@!�@��@�9@�h@rG@/@�K@�@��@�/@֡@�j@��@��@|�@(�@�]@��@�@v`@C�@ i@�2@�,@��@��@L0@��@ϫ@�h@}�@7L@�@�j@��@bN@>B@7@�@�@� @�a@�F@��@a@F�@�@�]@�@C�@#:@�@��@�@j@B�@0�@��@~(@%�@�&@��@�@��@��@j�@qv@e�@X�@Z�@/�@��@ߤ@�}@�+@z@_�@+k@u@�#@��@�@L�@2a@*0@@�|@��@Ɇ@�@�o@4n111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aͪ�AͦLAͨ$AͫkAͫkAͰUAͬ�Aͮ}AͯAͭ�Aͨ�Aͣ�Aͪ�Aͱ�AͰ!AͯAͨ�Aͥ�A�lWA�i�A�OA�7�A�5A�1'A�.�A�)�A�'�A�!�A��A��A��A��A̶�A�~A���A��A�XyA��mAŭCA���AŹ$AŵA��#Aì�A�l"A���A�m]A�DA��A��A���A��mA�xlA���A�]/A��(A�	A���A��OA�_�A�WsA�&LA�8A�N�A��BA�6�A�jA��_A�l�A��EA��$A��A���A��[A�&A�	A�@OA���A��DA�a�A��A��IA���A���A��FA��VA�.IA�<jA�s�A�A���A�cTA��2AZ�Ass�Aj�Ae�wA`�A]�fAX�NAT�\AQ��AOeAK��AH7�AE6zAA��A?dZA?�A>��A=��A<��A;��A9�XA6��A5oA4u�A3��A4cA4��A3�9A3�LA3zxA2�pA3aA3��A3�!A3��A3��A1ZA0g8A0,=A0JA0L0A/ iA,�4A)��A(4nA'�A(�A(��A)K�A)�PA)*0A(ϫA(�A(�A(XA'�EA'�A'H�A'*�A&��A&�wA&n/A&�A%�+A%	A$��A$VmA#��A"��A!�A!��A!=A z�A��AĜA�AO�A�WAA��A�-AiDA  A�'A��Aw�A=qA�HA��AS�A��A��A:�A}VA�XA�|A@�Av�A�.Ah�A=Ab�Ap;AqAMjA��A��A��A-�A�AA� A*0A�;A'�A
��A
"hA	��A	J#A�{AI�A�A7�A��Av�A+A�xAFA6AoA�PAɆA(�A��A��AdZA0�AߤA��A+A ��A ]�@�k�@�Y@���@��@��2@�(�@�`B@��H@���@���@�$�@�V@��h@�[�@��@�a@��,@�($@�%@�_�@��@�@���@�1'@�f@���@�U2@�8@��@�C@���@�/@��@��@�)�@�@�H@��&@���@�o�@�j@鹌@�ی@�s�@�e�@�o@蟾@��@�&�@思@�6@��@�˒@�Z�@��'@�Z�@��)@�Z�@��@�b�@���@��@�4@��_@�q�@�R�@ߙ�@ޞ@��@�S�@��@�%F@څ�@�)�@ٿH@ُ�@�O@��X@�L0@���@�`B@� �@�W?@�;d@�q@�@ԣ�@�Z�@���@Ұ!@�[�@�=@Ї�@�K^@Ϡ�@��@ͶF@���@̈�@�A�@��z@��|@ʀ�@�B[@�b@��9@�o�@��@��'@���@��@��@�N�@��9@Ĺ�@��Z@ä@@��@µ@��z@���@�%F@���@���@�@�@���@��*@��@�q�@���@��@��b@��@�ƨ@�o@��<@��@�"h@��3@��f@�+@��|@��6@�9X@��X@�X@��m@�]d@���@�0�@���@�;�@��@��y@�� @�e@���@�ߤ@���@�xl@��@���@��@�q@�ϫ@��~@�?}@��@��<@�@�t�@�c�@�a@�B�@��1@���@�U2@��@��k@�J�@��@��@���@�tT@�_@���@��~@�o @�Z�@��v@�%�@���@���@�x@��@��X@��z@�I�@�
�@�ݘ@���@�-w@��K@���@��6@�~(@�]d@��@���@�|�@�E9@�\�@��@��H@���@��q@��f@�8�@�(@���@�Ɇ@��+@���@��k@�:�@��@��P@���@���@�1�@��@�a@�"�@���@��B@���@� �@��@���@�d�@�9X@�($@�e@��@��@�o�@�	l@��_@�Z�@��@��n@�k�@�!-@���@��@�]d@�C�@� �@���@���@�g�@�#�@��@��1@�\�@�!@��@��F@��:@�Y@�֡@��\@�7�@��Z@���@�Q�@��@��@�:*@��m@�|�@�6z@��@��e@�R�@��@���@��*@��@@��h@�8�@���@���@��@�rG@�E9@���@��f@��5@��@��U@�E�@���@��7@��@��8@��]@��R@�J�@�$�@��W@���@�9�@��M@��U@��@�h�@�.�@�	@��#@��7@�
=@��@�B[@���@���@�RT@���@�i�@��@��@S@~n�@}��@}�@|A�@{J#@z��@y�d@x�@w=@v�@v��@vkQ@v@u��@u�@tN�@t�@s8@r�@rR�@q��@q	l@pɆ@p�D@p�@o�}@o9�@nxl@m�.@m�H@mY�@l��@l��@lH@l6@l7@k�
@k&@j�m@j}V@jO@ij@h�|@hM@giD@f�X@fa|@f$�@e�X@d�[@dU2@d~@c�6@c�4@c�@b��@b.�@a��@a�~@`ی@`PH@`b@_��@_�@_S�@_"�@^�M@^��@^c @]�@]f�@\��@\c�@\�@[�g@[��@[e�@[&@Z�@Z�L@Z �@X�f@X�j@X�@W��@Wx@WP�@WW?@W�@V�R@V($@U��@U?}@T�_@TPH@S�@S��@S�:@S|�@S4�@Rں@Ra|@Q�D@Q��@Q+�@P��@P��@P~(@P9X@O��@O��@Oy�@OS@N�}@Nxl@N=q@M�@M�7@MN<@L��@L��@L~(@LA�@K�@K��@K|�@K�@J�@J�H@J��@JW�@J�@I��@I%F@H�@H9X@H �@H�@G�]@G�@G�Q@G˒@G�@GiD@GO@G6z@F�y@F�F@Fq�@Fe@E�t@E�7@Em]@EVm@EJ�@E0�@E�@D�P@D֡@D��@D�@D<�@C��@CP�@B�H@B��@B-@A�.@A�@A�@A�@As�@Aa�@A%F@@ѷ@@z�@@M@@(�@@�@?�+@?��@?j�@?/�@>�@>�L@>q�@>Q@>@=�@=�n@=|@=:�@<�K@<��@<U2@<b@;�m@;�@;��@;S�@;�@:�b@:^5@:@�@:e@9�@9�@9L�@8�@8�$@8l"@8*�@7�Q@7�4@76z@7�@6�X@6�r@6p;@6H�@6!�@5�@5�=@5�@5IR@3��@3{J@3Mj@34�@3J#@31�@2�2@2�2@2��@2i�@2J�@25?@2�@1�z@1e,@1@0�?@0U2@0b@/��@//�@.�@.GE@-�o@-��@-a�@-G�@-/@-@,�$@,~(@,6@+�@+�@+o@*�L@)�@)��@)o @)@(Ɇ@(~(@'�m@'��@'W?@')_@'�@&�<@&�x@&p;@&YK@&?@%��@%�X@%0�@$�5@$�E@$|�@$K^@$1'@$�@$�@$�@$@#�0@#|�@#S�@#4�@#.I@#&@#&@"�8@"�m@"�m@"��@":*@!�@!�z@!Y�@!/@!�@!V@!V@!�@!+@ �)@ j@ N�@ <�@  �@�@��@��@W?@�@��@��@�H@�,@��@c @6�@($@�@@��@��@e,@/@ی@�@�@u�@Z@	�@�@�;@��@�0@�P@,�@�,@��@?@!�@��@�9@�h@rG@/@�K@�@��@�/@֡@�j@��@��@|�@(�@�]@��@�@v`@C�@ i@�2@�,@��@��@L0@��@ϫ@�h@}�@7L@�@�j@��@bN@>B@7@�@�@� @�a@�F@��@a@F�@�@�]@�@C�@#:@�@��@�@j@B�@0�@��@~(@%�@�&@��@�@��@��@j�@qv@e�@X�@Z�@/�@��@ߤ@�}@�+@z@_�@+k@u@�#@��@�@L�@2a@*0@@�|@��@Ɇ@�@�o@4n111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	�VB	�;B	�B	�!B	��B	�;B	�!B	�;B	�;B	߾B	ߤB	ߤB	�;B	�pB	�VB	�'B	�\B	�nB	�B	�B	�B	�B	�fB	�B	��B	�XB	�B	�B	��B	�B	�]B	� B
�B
*�B
0�B
-B
/OB
9rB
B�B
R�B
d�B
`'B
I�B
M�B
u�B
u%B
��B
��B
�B�Bk6B}<B��B�lB��B��B��B�B�B��B��B˒B�&B��B�|B�bB�_B�wB�B��B�|B�BB�rB��B�>B�B��B��B�Bc�BNVB#:B�B
��B
�B
��B
��B
�B
MB
;�B
4�B
)�B
^B	�_B	�B	q�B	UB	FYB	0B	�B	�B��B�B��B��B��B�]B�dBޞB�B��B��B�hB��B�B��B	�B	/OB	VB	h�B	~�B	�oB	��B	�0B	ʦB	ʌB	ɺB	ǔB	�}B	��B	��B
 iB
�B

�B	�6B	�B	ܒB	�=B	ޞB	�B
	B
SB
yB
#:B
*�B
.cB
/iB
4B
8B
;�B
IB
K�B
L�B
M�B
O�B
OBB
O\B
O(B
N�B
M�B
J�B
GEB
EmB
C�B
B[B
@ B
<�B
;�B
<�B
?�B
CB
B�B
B�B
B[B
C�B
DMB
DgB
EB
D�B
C�B
A�B
@ B
=�B
<jB
9XB
2�B
,�B
&�B
#B
�B
B
�B
�B
�B
uB	�HB	��B	��B	��B	�*B	��B	��B	��B	�*B	��B	��B	��B	�!B	��B	�AB	�aB	�B	�B	�B	��B	�WB	�6B	�B	�qB	�B	�QB	�B	�B	�*B	�*B	�B	�>B	�$B	�B	�/B	�B	��B	�aB	�vB	�B	�B	�B	�B	��B	��B	�UB	��B	��B	�*B	�JB	��B	��B	��B	�wB	�}B	��B
 4B	��B	�B	��B	��B	�XB	�>B	��B	�zB	�tB	��B	�B	��B	�B	�MB	�B	��B	�|B	��B	�B	��B	�TB	�+B	�9B	�AB	��B	��B	�B	�-B	��B	��B	�aB	�-B	�B	��B	�B	��B	�6B	�jB	�B	��B	��B	�B
�B
�B
B
�B
�B
[B
[B
B
�B
;B
 �B
 B
 B
 B
;B
;B
 B
;B
UB
;B
;B
�B
�B
oB
oB
UB
UB
B
 �B
�B
;B
�B
�B
UB
oB
UB
B
AB
AB
[B
�B
AB
uB
[B
'B
�B
B
'B
�B
�B
uB
�B
�B
�B
oB
 �B
 iB
 iB
 4B
 �B
 iB
 �B
 �B
 iB
 iB
 �B
 �B
�B
B
;B
�B
�B
�B
�B
�B
B
�B
uB
uB
�B
�B
�B
�B
aB
�B
�B
�B
�B
B
�B
�B
�B
�B
zB
�B
�B
�B
	�B
	�B
	�B
	�B

=B

XB

�B
�B
^B
�B
�B
�B
�B
B
�B
�B
~B
�B
6B
jB
�B
B
"B
<B
�B
B
�B
�B
BB
BB
BB
�B
�B
}B
�B
.B
�B
}B
�B
}B
 B
�B
�B
�B
NB
NB
�B
�B
oB
:B
oB
�B
�B
�B
 B
TB
�B
�B
�B
�B
B
uB
B
�B
�B
2B
9B
�B
�B
�B

B
�B
?B
?B
�B
�B
�B
�B
�B
EB
�B
B
QB
QB
QB
kB
QB
7B
QB
�B
�B
�B
�B
�B
�B
)B
�B
�B
~B
~B
~B
~B
5B
OB
�B
;B
pB
�B
�B
 B
 \B
 �B
!bB
!bB
!�B
!�B
!�B
"�B
"�B
# B
#�B
#�B
$B
$tB
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'8B
(�B
(�B
)DB
)�B
)�B
)yB
)yB
)�B
*�B
+�B
+6B
,=B
,"B
,WB
,�B
-wB
-CB
-�B
-�B
.}B
.�B
/ B
/5B
/OB
/�B
/�B
/�B
0!B
0�B
1'B
1�B
1�B
2|B
2aB
3B
3MB
3�B
3�B
49B
4TB
4�B
4nB
5B
5%B
4�B
5�B
6�B
6�B
7B
72B
7fB
7�B
7�B
8RB
8�B
8�B
9�B
:B
:^B
:�B
;JB
;dB
;�B
<B
<B
<jB
=B
=VB
=�B
=�B
>B
>]B
>�B
>�B
>�B
>�B
?B
?HB
?cB
?}B
?�B
@ B
@iB
A B
A�B
A�B
A�B
B[B
B�B
CGB
CaB
C�B
C�B
DMB
D�B
D�B
EB
ESB
FtB
FYB
FtB
GEB
G�B
H�B
IB
I�B
J#B
J�B
KxB
KxB
L0B
LB
LJB
L~B
L~B
L�B
L�B
L�B
L�B
L�B
M6B
MjB
MPB
MB
M�B
N"B
N<B
N�B
NpB
N�B
OBB
O\B
O�B
OvB
O�B
PHB
P.B
P.B
P}B
P�B
QNB
Q4B
QB
Q B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
Q�B
R:B
R�B
S@B
S@B
S@B
S&B
S[B
S�B
T,B
S�B
TFB
T�B
T{B
T{B
U2B
VB
VmB
VSB
VmB
V�B
W$B
W$B
W?B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X+B
XyB
XyB
X�B
Y1B
Y1B
YB
YB
YB
YB
Y1B
Y1B
Y1B
YeB
Y1B
Y�B
Y�B
Z7B
ZkB
Z�B
[#B
[=B
[WB
[=B
[qB
[qB
[qB
[�B
\CB
\�B
\�B
\�B
\�B
\�B
]/B
]dB
]�B
]�B
^B
^5B
^jB
^�B
^�B
^�B
_!B
_�B
`B
`BB
`\B
`�B
`�B
`�B
`�B
aB
aHB
a�B
a�B
bB
b4B
b4B
bNB
b�B
c B
c:B
c�B
c�B
c�B
d&B
d&B
d@B
d�B
d�B
d�B
d�B
d�B
eB
eFB
e,B
eB
d�B
d�B
d�B
eB
fB
f�B
f�B
ffB
f�B
f�B
gB
f�B
g8B
gRB
gB
f�B
g�B
g�B
gmB
h
B
h�B
iyB
i�B
j0B
j�B
j�B
j�B
j�B
j�B
kB
k6B
kkB
k�B
kkB
kQB
kkB
kQB
kB
kB
k�B
k�B
l=B
l�B
l�B
m]B
mwB
m�B
nB
nIB
n}B
n}B
nIB
ncB
n�B
oB
o5B
oOB
o�B
o�B
pB
pB
pB
pB
pB
poB
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q[B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r-B
rB
r�B
r�B
sB
sB
s3B
sMB
shB
s�B
s�B
tB
tB
tB
t9B
t9B
tTB
t�B
t�B
t�B
t�B
uB
u%B
u?B
utB
u�B
u�B
vB
v+B
vFB
v`B
v�B
v�B
v�B
v�B
wB
wB
wLB
wfB
w�B
w�B
w�B
w�B
xB
xRB
xlB
x�B
y	B
y	B
y$B
y$B
y$B
y>B
y>B
y>B
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
z�B
z�B
z�B
z�B
{0B
{B
{�B
{�B
{�B
{�B
|6B
|PB
|�B
|�B
|�B
|�B
}B
}B
}B
}"B
}<B
}VB
}�B
}�B
}�B
~(B
~]B
~�B
~�B
~�B
~�B
B
�B
� B
� B
�OB
�B
� B
�B
�OB
�iB
�iB
��B
��B
��B
�B
�;B
�UB
�'B
�'B
�AB
�'B
�B
�B
�AB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�a111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	�VB	�;B	�B	�!B	��B	�;B	�!B	�;B	�;B	߾B	ߤB	ߤB	�;B	�pB	�VB	�'B	�\B	�nB	�B	�B	�B	�B	�fB	�B	��B	�XB	�B	�B	��B	�B	�]B	� B
�B
*�B
0�B
-B
/OB
9rB
B�B
R�B
d�B
`'B
I�B
M�B
u�B
u%B
��B
��B
�B�Bk6B}<B��B�lB��B��B��B�B�B��B��B˒B�&B��B�|B�bB�_B�wB�B��B�|B�BB�rB��B�>B�B��B��B�Bc�BNVB#:B�B
��B
�B
��B
��B
�B
MB
;�B
4�B
)�B
^B	�_B	�B	q�B	UB	FYB	0B	�B	�B��B�B��B��B��B�]B�dBޞB�B��B��B�hB��B�B��B	�B	/OB	VB	h�B	~�B	�oB	��B	�0B	ʦB	ʌB	ɺB	ǔB	�}B	��B	��B
 iB
�B

�B	�6B	�B	ܒB	�=B	ޞB	�B
	B
SB
yB
#:B
*�B
.cB
/iB
4B
8B
;�B
IB
K�B
L�B
M�B
O�B
OBB
O\B
O(B
N�B
M�B
J�B
GEB
EmB
C�B
B[B
@ B
<�B
;�B
<�B
?�B
CB
B�B
B�B
B[B
C�B
DMB
DgB
EB
D�B
C�B
A�B
@ B
=�B
<jB
9XB
2�B
,�B
&�B
#B
�B
B
�B
�B
�B
uB	�HB	��B	��B	��B	�*B	��B	��B	��B	�*B	��B	��B	��B	�!B	��B	�AB	�aB	�B	�B	�B	��B	�WB	�6B	�B	�qB	�B	�QB	�B	�B	�*B	�*B	�B	�>B	�$B	�B	�/B	�B	��B	�aB	�vB	�B	�B	�B	�B	��B	��B	�UB	��B	��B	�*B	�JB	��B	��B	��B	�wB	�}B	��B
 4B	��B	�B	��B	��B	�XB	�>B	��B	�zB	�tB	��B	�B	��B	�B	�MB	�B	��B	�|B	��B	�B	��B	�TB	�+B	�9B	�AB	��B	��B	�B	�-B	��B	��B	�aB	�-B	�B	��B	�B	��B	�6B	�jB	�B	��B	��B	�B
�B
�B
B
�B
�B
[B
[B
B
�B
;B
 �B
 B
 B
 B
;B
;B
 B
;B
UB
;B
;B
�B
�B
oB
oB
UB
UB
B
 �B
�B
;B
�B
�B
UB
oB
UB
B
AB
AB
[B
�B
AB
uB
[B
'B
�B
B
'B
�B
�B
uB
�B
�B
�B
oB
 �B
 iB
 iB
 4B
 �B
 iB
 �B
 �B
 iB
 iB
 �B
 �B
�B
B
;B
�B
�B
�B
�B
�B
B
�B
uB
uB
�B
�B
�B
�B
aB
�B
�B
�B
�B
B
�B
�B
�B
�B
zB
�B
�B
�B
	�B
	�B
	�B
	�B

=B

XB

�B
�B
^B
�B
�B
�B
�B
B
�B
�B
~B
�B
6B
jB
�B
B
"B
<B
�B
B
�B
�B
BB
BB
BB
�B
�B
}B
�B
.B
�B
}B
�B
}B
 B
�B
�B
�B
NB
NB
�B
�B
oB
:B
oB
�B
�B
�B
 B
TB
�B
�B
�B
�B
B
uB
B
�B
�B
2B
9B
�B
�B
�B

B
�B
?B
?B
�B
�B
�B
�B
�B
EB
�B
B
QB
QB
QB
kB
QB
7B
QB
�B
�B
�B
�B
�B
�B
)B
�B
�B
~B
~B
~B
~B
5B
OB
�B
;B
pB
�B
�B
 B
 \B
 �B
!bB
!bB
!�B
!�B
!�B
"�B
"�B
# B
#�B
#�B
$B
$tB
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'8B
(�B
(�B
)DB
)�B
)�B
)yB
)yB
)�B
*�B
+�B
+6B
,=B
,"B
,WB
,�B
-wB
-CB
-�B
-�B
.}B
.�B
/ B
/5B
/OB
/�B
/�B
/�B
0!B
0�B
1'B
1�B
1�B
2|B
2aB
3B
3MB
3�B
3�B
49B
4TB
4�B
4nB
5B
5%B
4�B
5�B
6�B
6�B
7B
72B
7fB
7�B
7�B
8RB
8�B
8�B
9�B
:B
:^B
:�B
;JB
;dB
;�B
<B
<B
<jB
=B
=VB
=�B
=�B
>B
>]B
>�B
>�B
>�B
>�B
?B
?HB
?cB
?}B
?�B
@ B
@iB
A B
A�B
A�B
A�B
B[B
B�B
CGB
CaB
C�B
C�B
DMB
D�B
D�B
EB
ESB
FtB
FYB
FtB
GEB
G�B
H�B
IB
I�B
J#B
J�B
KxB
KxB
L0B
LB
LJB
L~B
L~B
L�B
L�B
L�B
L�B
L�B
M6B
MjB
MPB
MB
M�B
N"B
N<B
N�B
NpB
N�B
OBB
O\B
O�B
OvB
O�B
PHB
P.B
P.B
P}B
P�B
QNB
Q4B
QB
Q B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
Q�B
R:B
R�B
S@B
S@B
S@B
S&B
S[B
S�B
T,B
S�B
TFB
T�B
T{B
T{B
U2B
VB
VmB
VSB
VmB
V�B
W$B
W$B
W?B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X+B
XyB
XyB
X�B
Y1B
Y1B
YB
YB
YB
YB
Y1B
Y1B
Y1B
YeB
Y1B
Y�B
Y�B
Z7B
ZkB
Z�B
[#B
[=B
[WB
[=B
[qB
[qB
[qB
[�B
\CB
\�B
\�B
\�B
\�B
\�B
]/B
]dB
]�B
]�B
^B
^5B
^jB
^�B
^�B
^�B
_!B
_�B
`B
`BB
`\B
`�B
`�B
`�B
`�B
aB
aHB
a�B
a�B
bB
b4B
b4B
bNB
b�B
c B
c:B
c�B
c�B
c�B
d&B
d&B
d@B
d�B
d�B
d�B
d�B
d�B
eB
eFB
e,B
eB
d�B
d�B
d�B
eB
fB
f�B
f�B
ffB
f�B
f�B
gB
f�B
g8B
gRB
gB
f�B
g�B
g�B
gmB
h
B
h�B
iyB
i�B
j0B
j�B
j�B
j�B
j�B
j�B
kB
k6B
kkB
k�B
kkB
kQB
kkB
kQB
kB
kB
k�B
k�B
l=B
l�B
l�B
m]B
mwB
m�B
nB
nIB
n}B
n}B
nIB
ncB
n�B
oB
o5B
oOB
o�B
o�B
pB
pB
pB
pB
pB
poB
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q[B
q�B
q�B
q�B
q�B
q�B
q�B
rB
r-B
rB
r�B
r�B
sB
sB
s3B
sMB
shB
s�B
s�B
tB
tB
tB
t9B
t9B
tTB
t�B
t�B
t�B
t�B
uB
u%B
u?B
utB
u�B
u�B
vB
v+B
vFB
v`B
v�B
v�B
v�B
v�B
wB
wB
wLB
wfB
w�B
w�B
w�B
w�B
xB
xRB
xlB
x�B
y	B
y	B
y$B
y$B
y$B
y>B
y>B
y>B
y�B
y�B
y�B
y�B
y�B
zB
zDB
zxB
z�B
z�B
z�B
z�B
{0B
{B
{�B
{�B
{�B
{�B
|6B
|PB
|�B
|�B
|�B
|�B
}B
}B
}B
}"B
}<B
}VB
}�B
}�B
}�B
~(B
~]B
~�B
~�B
~�B
~�B
B
�B
� B
� B
�OB
�B
� B
�B
�OB
�iB
�iB
��B
��B
��B
�B
�;B
�UB
�'B
�'B
�AB
�'B
�B
�B
�AB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�a111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105245  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192504  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192505  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192505                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042513  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042513  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                