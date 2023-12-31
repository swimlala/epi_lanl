CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:13:39Z creation;2022-06-04T19:13:39Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604191339  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @����
1   @��OՅ�@.,1&�y�dV�u1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B��B  B ffB'��B0  B733B@  BG��BP  BXffB`  Bg��Bp  Bx  B�  B���B�  B�  B�  B�33B�  B�33B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�33B���B�  C   C  C  C  C  C
  C�CffC�3C�fC  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C;�fC=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj33Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\@\)@��@��A�
A?�
A_�
A�
A��A��A��A��AиRA��A��A��B��BB��B \)B'�]B/��B7(�B?��BG�]BO��BX\)B_��Bg�]Bo��Bw��B��B�ǮB���B���B���B�.B���B�.B�ǮB���B���B���B���B���B�ǮB���B���B���B���B���B���B���B���B���B�ǮB���B���B���B�.B�.B�ǮB���B���C�qC�qC�qC�qC	�qCCc�C��C��C�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%��C'�qC)�qC+�qC-�qC/�qC1�qC3�qC6C8C9�qC;��C=��C?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCj0�Ck�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#��D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ��DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dd�Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D��D�B�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�/�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�#�A�!bA� \A�!-A�$tA�(�A�($A�'RA�%A�,�A�-wA�+kA�)�A�+A�-CA�1�A��A���Aݮ}A��A܇�A�jA��Aڡ�AؓuA�8�A�%zA�tAξ�A�ϫA�HKA��A��-A��A�t�A��A���A���A�.A�_AƩ�A�9$A���AĬqA��A�&�A���A���A�r�A��WA�7A���A�=�A��A�+�A���A�y�A�1A���A�;A�2�A�VA�'�A���A�A��A�K^A�n�A��nA���A�"hA��CA��A�M�A���A��{A���A��)A���A�l�A��BA��A���A��HA��A��A��A�EmA�A�A���A���A�kQA}@Aw��As�yAq�*Ao��Am+�Ah�LAf�:AdȴAc;�Aa�A`��A^��A[jAV��AT��ARH�AO�6AN1�ALxAH��AE%�AB˒AAL0A@��A?��A>V�A=�A<$tA:��A8�BA7��A6<6A5�A6)�A4L0A1�AA0�YA.jA-hsA+h
A*I�A*[�A*��A*xA)VmA(��A(e�A&��A$�PA$�\A$-�A#�hA"�mA x�A�"A�A�zA�)Ae,A�zA��A�:AѷA	A�A5?A(�A�<A�4A�A͟A�A3�A�A�mA�A��As�AM�A"hAX�A�A��Au�A�8A��A�aAV�A�A�2A�.A^�AsAqA�Al�A6�A	lA�A�*AhsA�wA��A!�A	��A
A	�sA	��A	|A	_pA	A�}A�,A��A��A�A�rA�HA��A�A��AH�AVA�:A�eA�A�jAK�A ��A �XA Ft@���@���@�7@��@�.I@�#:@���@���@�@O@�͟@��@�l�@���@�c�@���@�6z@�\�@�@���@�`B@�"@���@�M@�j@�F�@���@�9�@��5@�Z@��@�9�@竟@�K^@�*�@�  @��@��@��>@䛦@�0�@��)@�)�@�x�@�ی@��o@���@�\)@ޕ�@�x@�p�@�e,@���@�ȴ@ܕ�@�l"@��@�Ta@ٜ�@�;d@�;@��?@،@��@ה�@�l�@���@�y>@յt@���@�xl@�H�@��}@�Y@���@�O@���@϶F@�@Ό�@�PH@�ݘ@�e,@��@�(�@ˎ"@�/@���@��@���@�l"@�ѷ@��@��z@Ǟ�@�x@�j�@�@O@���@�z�@� �@��6@�H�@���@�_@�L0@Ù�@��@��@@�P�@��|@��@���@��@��@�@���@�8�@��@�Ta@��@���@�Ɇ@���@��r@���@��@���@�h�@��}@�@@�v�@��@��[@�Q�@��@��_@�q@�u@��V@�a�@�(�@� i@��@���@�xl@�-@��@���@�|�@�H�@� \@���@�U2@�1@��z@���@���@���@�s�@�5?@��:@��@�V�@�$@��)@��:@�m]@�0�@��)@�^5@�1�@� �@��@���@���@�{J@�8@�;@��/@���@�^5@���@�U�@���@�4@���@�w2@�V@��s@���@���@�S�@��o@���@�zx@�F�@�*0@�@��,@���@�Z�@�~@��K@���@�p�@��@��o@�b@�ϫ@�o�@�Mj@��@�Ft@� �@���@�Vm@��@�҉@�u�@�x@�x@�+@��@�ی@���@�u%@�E�@�6@�*�@��@���@��@���@�A @��@���@��@���@�h�@�Ta@��D@���@�[W@���@��E@��6@�U2@��@���@�L�@��@��@���@�Z@�+k@���@�1�@��@��@�Ov@�/�@��@�G@��#@��C@���@�O�@��@��@��L@�G@���@��z@�X�@��R@�;�@�  @���@���@�X@��@���@�e�@�8�@��Q@���@�4�@��@���@��.@�r�@�'R@�˒@��V@���@�X�@��M@��@���@�u%@�a|@�E�@�,=@�@���@��;@��@���@�W?@��X@�^5@� �@��
@��{@�F@�2a@� \@��@��U@�~(@�=q@��@��@��a@���@��X@��$@�a@�#�@��s@�Ov@�0U@�u@��X@�~�@�7L@��@�Ĝ@���@�bN@�@خ@� @ƨ@�@@iD@�@~�x@~3�@}ԕ@|�E@{�$@{b�@zc @y�Z@y�@y?}@x��@xy>@xPH@x	�@w�q@wMj@v��@u�@uf�@t�@t_@t@s�
@s{J@r�y@rq�@r($@q�#@q��@q[W@p2�@o�@n�@nh
@m�T@m�"@m(�@l�5@l��@lS�@l-�@k�@k=@j�X@jJ@i�z@i�X@i!�@h��@h:�@g�a@g;d@f�@f\�@f	@ea�@d�@d��@de�@c��@b��@b�\@bV@b$�@a��@aX@a	l@`��@`�o@`1@_b�@^͟@^YK@]��@]&�@\��@[خ@[�:@[Mj@Z�@Z��@ZGE@Z �@Y��@Y��@Yj@Y�@X��@XZ@X!@W��@W�
@W�	@V�@U�@U��@U[W@U7L@U \@T�|@T��@TU2@S��@S��@S6z@R�'@R^5@R&�@Ru@Q��@Q�@Qp�@QVm@Q4@Q \@Q�@P��@P��@P<�@P�@O��@O�g@O˒@O��@OS�@O$t@N��@N��@N�A@N^5@N�@M�@M�@M��@M�~@M�h@M��@M��@M�~@Mu�@MV@L�e@L��@L"h@K��@K�A@K�m@K��@K��@KW?@K�@J҉@Jxl@J+k@J	@I�3@I��@I��@Ia�@I/@H�E@He�@H�@Gt�@GS@F�@F_�@FC�@F	@E�'@E�7@Ea�@E@@D�@Doi@D>B@D�@C�K@CX�@B�@B�+@Bh
@B	@A\�@@��@@��@@Q�@@Q�@@M@@:�@?�@?��@?e�@?�@>��@>�L@>l�@>\�@>	@=ԕ@=S&@<�z@<$@;�}@;��@;33@:�8@:҉@:��@:3�@9�@9S&@9	l@8��@8�@8Xy@7�@7�P@7j�@7;d@6��@6ȴ@6Z�@64@5�H@5��@5Vm@5*0@4��@4'R@3� @3��@3iD@3A�@3@2��@2��@2�R@2ff@2&�@1�>@1�'@1!�@0��@0PH@0@/��@/9�@.�m@.�A@.J�@.)�@. �@-��@-+�@,��@,�@+�Q@+�@@+F�@*�c@*��@*a|@*8�@)��@)�-@)J�@(��@(֡@(��@("h@'�r@'�@'�k@'33@&�c@&�@&W�@%�D@%�H@%Vm@$�K@$�@$]d@$b@#��@#n/@#E9@#�@"�s@"� @")�@!�T@!�X@!e,@!+�@ �K@ �4@ j@ Xy@ /�@�W@�:@E9@$t@ȴ@��@��@R�@�o@��@T�@?}@<6@�@��@�O@`�@x@��@RT@,�@�@�H@��@q�@;�@�@_@��@�-@8�@	l@�@�e@�@oi@>B@��@��@j�@!-@��@ȴ@��@�h@��@��@d�@W�@B[@	@�T@��@IR@�@�K@��@j@2�@�A@��@�0@�@s@J#@$t@�@��@p;@.�@�.@�T@�@��@zx@Q�@�@�4@�.@m�@Z@1'@�@\)@�@�X@�m@��@�b@�F@��@��@kQ@#:@��@�t@��@s�@J�@�v@�z@��@[�@�@�	@y�@g�@A�@$t@
ߤ@
�'@
�@
�b@
{�@
M�@
5?@
	@
_@
 �@	�D@	�d@	��@	�~@	e,@	G�@�	@�$@|�@S�@/�@$@%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�#�A�!bA� \A�!-A�$tA�(�A�($A�'RA�%A�,�A�-wA�+kA�)�A�+A�-CA�1�A��A���Aݮ}A��A܇�A�jA��Aڡ�AؓuA�8�A�%zA�tAξ�A�ϫA�HKA��A��-A��A�t�A��A���A���A�.A�_AƩ�A�9$A���AĬqA��A�&�A���A���A�r�A��WA�7A���A�=�A��A�+�A���A�y�A�1A���A�;A�2�A�VA�'�A���A�A��A�K^A�n�A��nA���A�"hA��CA��A�M�A���A��{A���A��)A���A�l�A��BA��A���A��HA��A��A��A�EmA�A�A���A���A�kQA}@Aw��As�yAq�*Ao��Am+�Ah�LAf�:AdȴAc;�Aa�A`��A^��A[jAV��AT��ARH�AO�6AN1�ALxAH��AE%�AB˒AAL0A@��A?��A>V�A=�A<$tA:��A8�BA7��A6<6A5�A6)�A4L0A1�AA0�YA.jA-hsA+h
A*I�A*[�A*��A*xA)VmA(��A(e�A&��A$�PA$�\A$-�A#�hA"�mA x�A�"A�A�zA�)Ae,A�zA��A�:AѷA	A�A5?A(�A�<A�4A�A͟A�A3�A�A�mA�A��As�AM�A"hAX�A�A��Au�A�8A��A�aAV�A�A�2A�.A^�AsAqA�Al�A6�A	lA�A�*AhsA�wA��A!�A	��A
A	�sA	��A	|A	_pA	A�}A�,A��A��A�A�rA�HA��A�A��AH�AVA�:A�eA�A�jAK�A ��A �XA Ft@���@���@�7@��@�.I@�#:@���@���@�@O@�͟@��@�l�@���@�c�@���@�6z@�\�@�@���@�`B@�"@���@�M@�j@�F�@���@�9�@��5@�Z@��@�9�@竟@�K^@�*�@�  @��@��@��>@䛦@�0�@��)@�)�@�x�@�ی@��o@���@�\)@ޕ�@�x@�p�@�e,@���@�ȴ@ܕ�@�l"@��@�Ta@ٜ�@�;d@�;@��?@،@��@ה�@�l�@���@�y>@յt@���@�xl@�H�@��}@�Y@���@�O@���@϶F@�@Ό�@�PH@�ݘ@�e,@��@�(�@ˎ"@�/@���@��@���@�l"@�ѷ@��@��z@Ǟ�@�x@�j�@�@O@���@�z�@� �@��6@�H�@���@�_@�L0@Ù�@��@��@@�P�@��|@��@���@��@��@�@���@�8�@��@�Ta@��@���@�Ɇ@���@��r@���@��@���@�h�@��}@�@@�v�@��@��[@�Q�@��@��_@�q@�u@��V@�a�@�(�@� i@��@���@�xl@�-@��@���@�|�@�H�@� \@���@�U2@�1@��z@���@���@���@�s�@�5?@��:@��@�V�@�$@��)@��:@�m]@�0�@��)@�^5@�1�@� �@��@���@���@�{J@�8@�;@��/@���@�^5@���@�U�@���@�4@���@�w2@�V@��s@���@���@�S�@��o@���@�zx@�F�@�*0@�@��,@���@�Z�@�~@��K@���@�p�@��@��o@�b@�ϫ@�o�@�Mj@��@�Ft@� �@���@�Vm@��@�҉@�u�@�x@�x@�+@��@�ی@���@�u%@�E�@�6@�*�@��@���@��@���@�A @��@���@��@���@�h�@�Ta@��D@���@�[W@���@��E@��6@�U2@��@���@�L�@��@��@���@�Z@�+k@���@�1�@��@��@�Ov@�/�@��@�G@��#@��C@���@�O�@��@��@��L@�G@���@��z@�X�@��R@�;�@�  @���@���@�X@��@���@�e�@�8�@��Q@���@�4�@��@���@��.@�r�@�'R@�˒@��V@���@�X�@��M@��@���@�u%@�a|@�E�@�,=@�@���@��;@��@���@�W?@��X@�^5@� �@��
@��{@�F@�2a@� \@��@��U@�~(@�=q@��@��@��a@���@��X@��$@�a@�#�@��s@�Ov@�0U@�u@��X@�~�@�7L@��@�Ĝ@���@�bN@�@خ@� @ƨ@�@@iD@�@~�x@~3�@}ԕ@|�E@{�$@{b�@zc @y�Z@y�@y?}@x��@xy>@xPH@x	�@w�q@wMj@v��@u�@uf�@t�@t_@t@s�
@s{J@r�y@rq�@r($@q�#@q��@q[W@p2�@o�@n�@nh
@m�T@m�"@m(�@l�5@l��@lS�@l-�@k�@k=@j�X@jJ@i�z@i�X@i!�@h��@h:�@g�a@g;d@f�@f\�@f	@ea�@d�@d��@de�@c��@b��@b�\@bV@b$�@a��@aX@a	l@`��@`�o@`1@_b�@^͟@^YK@]��@]&�@\��@[خ@[�:@[Mj@Z�@Z��@ZGE@Z �@Y��@Y��@Yj@Y�@X��@XZ@X!@W��@W�
@W�	@V�@U�@U��@U[W@U7L@U \@T�|@T��@TU2@S��@S��@S6z@R�'@R^5@R&�@Ru@Q��@Q�@Qp�@QVm@Q4@Q \@Q�@P��@P��@P<�@P�@O��@O�g@O˒@O��@OS�@O$t@N��@N��@N�A@N^5@N�@M�@M�@M��@M�~@M�h@M��@M��@M�~@Mu�@MV@L�e@L��@L"h@K��@K�A@K�m@K��@K��@KW?@K�@J҉@Jxl@J+k@J	@I�3@I��@I��@Ia�@I/@H�E@He�@H�@Gt�@GS@F�@F_�@FC�@F	@E�'@E�7@Ea�@E@@D�@Doi@D>B@D�@C�K@CX�@B�@B�+@Bh
@B	@A\�@@��@@��@@Q�@@Q�@@M@@:�@?�@?��@?e�@?�@>��@>�L@>l�@>\�@>	@=ԕ@=S&@<�z@<$@;�}@;��@;33@:�8@:҉@:��@:3�@9�@9S&@9	l@8��@8�@8Xy@7�@7�P@7j�@7;d@6��@6ȴ@6Z�@64@5�H@5��@5Vm@5*0@4��@4'R@3� @3��@3iD@3A�@3@2��@2��@2�R@2ff@2&�@1�>@1�'@1!�@0��@0PH@0@/��@/9�@.�m@.�A@.J�@.)�@. �@-��@-+�@,��@,�@+�Q@+�@@+F�@*�c@*��@*a|@*8�@)��@)�-@)J�@(��@(֡@(��@("h@'�r@'�@'�k@'33@&�c@&�@&W�@%�D@%�H@%Vm@$�K@$�@$]d@$b@#��@#n/@#E9@#�@"�s@"� @")�@!�T@!�X@!e,@!+�@ �K@ �4@ j@ Xy@ /�@�W@�:@E9@$t@ȴ@��@��@R�@�o@��@T�@?}@<6@�@��@�O@`�@x@��@RT@,�@�@�H@��@q�@;�@�@_@��@�-@8�@	l@�@�e@�@oi@>B@��@��@j�@!-@��@ȴ@��@�h@��@��@d�@W�@B[@	@�T@��@IR@�@�K@��@j@2�@�A@��@�0@�@s@J#@$t@�@��@p;@.�@�.@�T@�@��@zx@Q�@�@�4@�.@m�@Z@1'@�@\)@�@�X@�m@��@�b@�F@��@��@kQ@#:@��@�t@��@s�@J�@�v@�z@��@[�@�@�	@y�@g�@A�@$t@
ߤ@
�'@
�@
�b@
{�@
M�@
5?@
	@
_@
 �@	�D@	�d@	��@	�~@	e,@	G�@�	@�$@|�@S�@/�@$@%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	V9B	V9B	VSB	V9B	VB	VB	VB	V9B	V9B	VB	U�B	VB	V9B	VSB	VB	U�B	ZB	]~B	b�B	q�B	�[B	�tB	��B	�JB	��B	�kB	�B
B
7B
.B
8RB
G�B
S�B
|�B
��B
��B
յB
�FB�B�B�BaB�BOB!�B(�BZBq�BtBy$B��B�B�B��B��B��B��B�B�iB�GBÖBԯB�B�DB�UB�LB�*B��B�B�wB��B�;B��Bv+Bd�B_;BVSBB�B2GB#B�B
�rB
�B
�fB
��B
�%B
vzB
A�B
/�B
�B
6B	�B	҉B	��B	��B	}�B	r�B	i�B	^�B	KB	:�B	-�B	#�B	dB	9B	�B	{B�B��B�B��B��B�VB�GB�+B��B�B��B��B�B�kB�zB��B�B��B�B�4B��B��B��B�2B�jB�1B�	B�TB�_B�]B��B�#B�BөB��B�B�B��B��B�B��B�	B	�B	^B	
rB	B	B		lB	kB	0�B	2B	9�B	;B	8�B	G�B	R�B	dZB	_�B	^B	h�B	}�B	��B	�@B	��B	��B	��B	�B	��B	��B	�eB	��B	��B	��B	��B	�.B	�B	��B	��B	��B	��B	��B	ɺB	��B	�VB	͟B	͟B	�B	ЗB	��B	�B	�B	��B	ȴB	̘B	�BB	ϑB	�(B	ϫB	��B	��B	��B	��B	ʌB	āB	ϫB	՛B	� B	��B	�B	��B	ϑB	�6B	��B	�=B	�	B	��B	��B	�B	�B	��B	��B	̘B	�dB	��B	͟B	��B	�4B	уB	�:B	�:B	�B	��B	��B	��B	�mB	�uB	�hB	�B	ԕB	өB	�TB	� B	�PB	ʌB	�(B	҉B	�B	�B	��B	��B	��B	��B	��B	�;B	ÖB	�B	��B	��B	өB	�TB	�4B	��B	��B	��B	�pB	̈́B	�(B	�NB	� B	՛B	�
B	��B	�mB	�EB	��B	�CB	�)B	��B	��B	�]B	�]B	�]B	��B	ݘB	ݲB	�5B	��B	ބB	�B	ބB	��B	ߊB	��B	߾B	�'B	ޞB	�B	��B	�B	��B	�!B	߾B	��B	߾B	�B	�B	��B	ߊB	�B	��B	�vB	��B	�\B	�BB	��B	�HB	�B	�&B	�`B	�fB	�LB	�B	�B	��B	�B	�B	�B	�*B	��B	�B	�B	�B	�eB	��B	��B	�"B	�wB	�/B	��B	�CB	�B	�B	�wB	��B	�B	�B	��B	�B	�UB	�'B	�[B	�'B	��B	��B	�B	�AB	�-B	��B	��B	�B	�B	�hB	�B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�?B	�`B	�B	��B	�FB	�LB	�fB	��B	��B	�B	��B	��B	��B	�xB	��B	�dB	�jB	��B	�VB	��B	�(B	��B	��B	��B	��B	��B	�}B	�}B	��B	�VB	��B	��B	�cB	�HB	�cB	��B	��B
 iB
 �B
�B
�B
�B
�B
[B
[B
�B
�B
�B
�B
�B
�B
�B
B
�B
tB
�B
EB
_B
�B
fB
�B
�B
�B
�B
�B

=B
B
xB
)B
DB

�B
)B
�B
dB
B
6B
PB
�B
�B
�B
�B
�B
�B
�B
pB
B
�B
BB
B
�B
.B
B
�B
�B
�B
 B
TB
�B
B
&B
�B
aB
aB
�B
�B
�B
?B
sB
YB
YB
�B
�B
1B
B
�B
=B
�B
�B
)B
�B
�B
�B
B
IB
dB
dB
~B
~B
~B
�B
dB
�B
�B
B
B
B
�B
VB
�B
�B
 vB
 �B
 �B
 �B
 �B
 �B
!B
!-B
!bB
!|B
!�B
!�B
!�B
!�B
#B
#�B
$ZB
$�B
%,B
%�B
%�B
%�B
%�B
&2B
&�B
'B
'B
'mB
'�B
(XB
(>B
($B
(sB
(sB
)B
)�B
*0B
*�B
*�B
+QB
+�B
,"B
,qB
,�B
-)B
-�B
-�B
-�B
-�B
-�B
./B
.}B
.�B
/ B
/B
0!B
0�B
0�B
1vB
1�B
1�B
2aB
2|B
2�B
2|B
2�B
2�B
2�B
3hB
4B
4B
4B
4TB
4�B
4�B
5tB
7fB
7�B
8B
8B
8B
8B
8�B
9�B
9�B
9XB
9rB
9�B
:^B
:xB
:�B
;0B
;dB
;�B
;�B
;�B
<B
<B
<B
<B
<B
<6B
<�B
=<B
=B
=B
=<B
="B
=<B
="B
=�B
>B
>wB
>]B
>BB
>BB
>]B
>�B
>�B
?.B
?HB
?�B
@ B
@iB
@iB
@�B
AUB
A�B
B'B
B'B
B[B
B�B
B�B
B�B
C-B
CGB
CB
CGB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F%B
F?B
F%B
F�B
F�B
GEB
G�B
G�B
HB
H1B
H1B
HfB
H�B
H�B
H�B
H�B
H�B
H�B
IB
IB
IlB
I�B
I�B
I�B
I�B
I�B
J	B
J=B
J=B
J=B
JXB
J�B
KB
KB
K)B
K)B
K^B
K^B
KxB
K�B
K^B
K^B
K�B
L0B
L~B
L�B
L�B
L�B
L�B
L�B
MB
MPB
MjB
M�B
M�B
M�B
N<B
N<B
N"B
NVB
NVB
N�B
N�B
O(B
O�B
O�B
PHB
PbB
PbB
P�B
P�B
P�B
P�B
QB
QNB
Q�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
SB
S�B
S�B
T,B
TaB
TaB
TaB
TaB
T�B
T�B
T�B
UMB
UMB
UgB
U�B
U�B
U�B
U�B
VmB
V�B
WsB
W�B
XB
XEB
X_B
X_B
X_B
X�B
YKB
YB
Y�B
Y�B
ZB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
[�B
\xB
\�B
\�B
\�B
]IB
]/B
]~B
]~B
]dB
]�B
]�B
^B
^B
^OB
^�B
^�B
_VB
_VB
_�B
`'B
`vB
`�B
`�B
`�B
`�B
aHB
a�B
a�B
bhB
b�B
b�B
c:B
cTB
c�B
c�B
c�B
dB
dZB
d�B
d�B
d�B
eB
ezB
ezB
ezB
e�B
fB
f2B
ffB
f�B
gB
gB
gmB
g�B
g�B
h$B
h$B
hXB
h�B
h�B
h�B
i*B
iyB
i�B
i�B
jB
jKB
jB
j�B
kB
k6B
k6B
kQB
kkB
k�B
k�B
k�B
lqB
lqB
l�B
l�B
mB
m]B
m�B
m�B
m�B
m�B
m�B
nB
nIB
n}B
o B
o5B
oOB
oiB
o�B
o�B
o�B
pB
p!B
p;B
p!B
pUB
p�B
p�B
qB
qAB
q[B
q[B
qvB
q�B
rB
r-B
r|B
r�B
r�B
r�B
r�B
r�B
sB
sB
s3B
s3B
sMB
s�B
s�B
tB
t9B
tTB
t�B
t�B
t�B
u%B
u%B
u?B
uZB
utB
u�B
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
w2B
w2B
wLB
w�B
w�B
xB
xB
xB
x8B
x�B
x�B
y>B
y�B
y�B
yrB
y�B
y�B
y�B
y�B
y�B
zB
z*B
z^B
zxB
zxB
z�B
{B
{JB
{0B
{�B
|B
|PB
|PB
|jB
|�B
|�B
}B
}B
}"B
}"B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~BB
~�B
~�B
B
.B
HB
HB
H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	V9B	V9B	VSB	V9B	VB	VB	VB	V9B	V9B	VB	U�B	VB	V9B	VSB	VB	U�B	ZB	]~B	b�B	q�B	�[B	�tB	��B	�JB	��B	�kB	�B
B
7B
.B
8RB
G�B
S�B
|�B
��B
��B
յB
�FB�B�B�BaB�BOB!�B(�BZBq�BtBy$B��B�B�B��B��B��B��B�B�iB�GBÖBԯB�B�DB�UB�LB�*B��B�B�wB��B�;B��Bv+Bd�B_;BVSBB�B2GB#B�B
�rB
�B
�fB
��B
�%B
vzB
A�B
/�B
�B
6B	�B	҉B	��B	��B	}�B	r�B	i�B	^�B	KB	:�B	-�B	#�B	dB	9B	�B	{B�B��B�B��B��B�VB�GB�+B��B�B��B��B�B�kB�zB��B�B��B�B�4B��B��B��B�2B�jB�1B�	B�TB�_B�]B��B�#B�BөB��B�B�B��B��B�B��B�	B	�B	^B	
rB	B	B		lB	kB	0�B	2B	9�B	;B	8�B	G�B	R�B	dZB	_�B	^B	h�B	}�B	��B	�@B	��B	��B	��B	�B	��B	��B	�eB	��B	��B	��B	��B	�.B	�B	��B	��B	��B	��B	��B	ɺB	��B	�VB	͟B	͟B	�B	ЗB	��B	�B	�B	��B	ȴB	̘B	�BB	ϑB	�(B	ϫB	��B	��B	��B	��B	ʌB	āB	ϫB	՛B	� B	��B	�B	��B	ϑB	�6B	��B	�=B	�	B	��B	��B	�B	�B	��B	��B	̘B	�dB	��B	͟B	��B	�4B	уB	�:B	�:B	�B	��B	��B	��B	�mB	�uB	�hB	�B	ԕB	өB	�TB	� B	�PB	ʌB	�(B	҉B	�B	�B	��B	��B	��B	��B	��B	�;B	ÖB	�B	��B	��B	өB	�TB	�4B	��B	��B	��B	�pB	̈́B	�(B	�NB	� B	՛B	�
B	��B	�mB	�EB	��B	�CB	�)B	��B	��B	�]B	�]B	�]B	��B	ݘB	ݲB	�5B	��B	ބB	�B	ބB	��B	ߊB	��B	߾B	�'B	ޞB	�B	��B	�B	��B	�!B	߾B	��B	߾B	�B	�B	��B	ߊB	�B	��B	�vB	��B	�\B	�BB	��B	�HB	�B	�&B	�`B	�fB	�LB	�B	�B	��B	�B	�B	�B	�*B	��B	�B	�B	�B	�eB	��B	��B	�"B	�wB	�/B	��B	�CB	�B	�B	�wB	��B	�B	�B	��B	�B	�UB	�'B	�[B	�'B	��B	��B	�B	�AB	�-B	��B	��B	�B	�B	�hB	�B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�?B	�`B	�B	��B	�FB	�LB	�fB	��B	��B	�B	��B	��B	��B	�xB	��B	�dB	�jB	��B	�VB	��B	�(B	��B	��B	��B	��B	��B	�}B	�}B	��B	�VB	��B	��B	�cB	�HB	�cB	��B	��B
 iB
 �B
�B
�B
�B
�B
[B
[B
�B
�B
�B
�B
�B
�B
�B
B
�B
tB
�B
EB
_B
�B
fB
�B
�B
�B
�B
�B

=B
B
xB
)B
DB

�B
)B
�B
dB
B
6B
PB
�B
�B
�B
�B
�B
�B
�B
pB
B
�B
BB
B
�B
.B
B
�B
�B
�B
 B
TB
�B
B
&B
�B
aB
aB
�B
�B
�B
?B
sB
YB
YB
�B
�B
1B
B
�B
=B
�B
�B
)B
�B
�B
�B
B
IB
dB
dB
~B
~B
~B
�B
dB
�B
�B
B
B
B
�B
VB
�B
�B
 vB
 �B
 �B
 �B
 �B
 �B
!B
!-B
!bB
!|B
!�B
!�B
!�B
!�B
#B
#�B
$ZB
$�B
%,B
%�B
%�B
%�B
%�B
&2B
&�B
'B
'B
'mB
'�B
(XB
(>B
($B
(sB
(sB
)B
)�B
*0B
*�B
*�B
+QB
+�B
,"B
,qB
,�B
-)B
-�B
-�B
-�B
-�B
-�B
./B
.}B
.�B
/ B
/B
0!B
0�B
0�B
1vB
1�B
1�B
2aB
2|B
2�B
2|B
2�B
2�B
2�B
3hB
4B
4B
4B
4TB
4�B
4�B
5tB
7fB
7�B
8B
8B
8B
8B
8�B
9�B
9�B
9XB
9rB
9�B
:^B
:xB
:�B
;0B
;dB
;�B
;�B
;�B
<B
<B
<B
<B
<B
<6B
<�B
=<B
=B
=B
=<B
="B
=<B
="B
=�B
>B
>wB
>]B
>BB
>BB
>]B
>�B
>�B
?.B
?HB
?�B
@ B
@iB
@iB
@�B
AUB
A�B
B'B
B'B
B[B
B�B
B�B
B�B
C-B
CGB
CB
CGB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F%B
F?B
F%B
F�B
F�B
GEB
G�B
G�B
HB
H1B
H1B
HfB
H�B
H�B
H�B
H�B
H�B
H�B
IB
IB
IlB
I�B
I�B
I�B
I�B
I�B
J	B
J=B
J=B
J=B
JXB
J�B
KB
KB
K)B
K)B
K^B
K^B
KxB
K�B
K^B
K^B
K�B
L0B
L~B
L�B
L�B
L�B
L�B
L�B
MB
MPB
MjB
M�B
M�B
M�B
N<B
N<B
N"B
NVB
NVB
N�B
N�B
O(B
O�B
O�B
PHB
PbB
PbB
P�B
P�B
P�B
P�B
QB
QNB
Q�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
SB
S�B
S�B
T,B
TaB
TaB
TaB
TaB
T�B
T�B
T�B
UMB
UMB
UgB
U�B
U�B
U�B
U�B
VmB
V�B
WsB
W�B
XB
XEB
X_B
X_B
X_B
X�B
YKB
YB
Y�B
Y�B
ZB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
[�B
\xB
\�B
\�B
\�B
]IB
]/B
]~B
]~B
]dB
]�B
]�B
^B
^B
^OB
^�B
^�B
_VB
_VB
_�B
`'B
`vB
`�B
`�B
`�B
`�B
aHB
a�B
a�B
bhB
b�B
b�B
c:B
cTB
c�B
c�B
c�B
dB
dZB
d�B
d�B
d�B
eB
ezB
ezB
ezB
e�B
fB
f2B
ffB
f�B
gB
gB
gmB
g�B
g�B
h$B
h$B
hXB
h�B
h�B
h�B
i*B
iyB
i�B
i�B
jB
jKB
jB
j�B
kB
k6B
k6B
kQB
kkB
k�B
k�B
k�B
lqB
lqB
l�B
l�B
mB
m]B
m�B
m�B
m�B
m�B
m�B
nB
nIB
n}B
o B
o5B
oOB
oiB
o�B
o�B
o�B
pB
p!B
p;B
p!B
pUB
p�B
p�B
qB
qAB
q[B
q[B
qvB
q�B
rB
r-B
r|B
r�B
r�B
r�B
r�B
r�B
sB
sB
s3B
s3B
sMB
s�B
s�B
tB
t9B
tTB
t�B
t�B
t�B
u%B
u%B
u?B
uZB
utB
u�B
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
w2B
w2B
wLB
w�B
w�B
xB
xB
xB
x8B
x�B
x�B
y>B
y�B
y�B
yrB
y�B
y�B
y�B
y�B
y�B
zB
z*B
z^B
zxB
zxB
z�B
{B
{JB
{0B
{�B
|B
|PB
|PB
|jB
|�B
|�B
}B
}B
}"B
}"B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~BB
~�B
~�B
B
.B
HB
HB
H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105229  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191339  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191339  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191339                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041347  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041347  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                