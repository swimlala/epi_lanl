CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:39:40Z creation;2022-06-04T17:39:40Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604173940  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               _A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٓoz�1   @ٓ�ۗ@.��t�j�c@�j~��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffBa33Bg��Bp  Bx��B33B���B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C�C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<33C=�fC@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn33Co�fCq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ�fD[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÃ3D��3D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��G@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BX\)Ba(�Bg�]Bo��BxB(�B�ǮB���B���B�aGB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B���B���B���B���B���B�ǮB���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qCCC�qC�qC��C�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC:C<0�C=��C?�qCA��CC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qClCn0�Co��Cq��Cs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW��DX\DX�\DY\DY�\DZ��D[�D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D}�D}��D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�DÂ�D���D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`A�\)A�X�A�5A��)A֪�A�}�A�tTA�kQA�d�A�_A�T�A�P�A�K�A�GA�C�A�A�A�?HA�9XA�7�A�4nA�-CA�(�A��A��A�R�A���AԄ�A�!�A�y>AϵAʋDAƅA��A�l"A���A�T�A��pA�X�A��A�\]A�<�A��)A��tA�/OA��~A���A���A���A���A�ѷA�J�A��A��A��A�3�A���A���A���A�u�A��A��\A�u%A�i�A���A���A���A��A��/A�$A�~�A�{A��CA��HA�kA���A��A���A���A��LA�	�A�<A�J#A�2aA�� A�TaA��vA���A���A��6A�	lA�,�A�Az{As��Ar�rAq��Aj��Ahs�Ad�fA`W?A]��A[XAZ��AYH�AWC�AVںAVK^AT�MARf�AOjAJ�AF�HAF�AD��ACt�AB(A?��A>oA=SA<�'A;�A9�A8��A7x�A3��A1�RA0�A.�fA,	lA+ƨA,�A,VA,�A-�)A.�A-c A+�/A,$�A,�A,.�A+J#A*1�A(�RA'-�A&;dA%;dA#�A#�A"`BA!��A!SA oiA�:AbNA�LA��Az�AE9A�?A�AW?Ae,A}VA֡AO�AU2AN�A7�A��AjA%�A�A��AiDA^5A;dA�A�~A�AG�A{JAA{JAOA��AA��AMA��AV�A
�5A
oiA
GEA
A	��A	��A	H�A	�A��A�	A�*AA�Ab�A�
A�fA �A��Aw�A�VA�>AU�A��A �A a�@��]@�Mj@���@���@�PH@�:�@���@��m@��@���@�;�@�;@���@�,�@�@@���@�V�@�L0@�9X@��@��b@�4@���@��*@��"@��@�h�@�2a@�oi@��@�e�@�C�@�@@�<6@@��d@�F�@첖@��@�$@��@�W�@��@��,@�xl@��@�z@�y>@�bN@�K^@�M@��@㐗@�n@�U�@�+�@���@�t@���@��@�Z�@��Q@�@@��Q@��Z@ݪ�@�}�@���@��@��P@�r�@�	�@ٷ@���@�A�@��@���@�y�@��@�~(@�e@�*0@ԕ�@Ե�@Ԛ�@�z�@��A@�=@��m@ҵ@�ѷ@���@ҹ�@һ�@�Z�@�s@��@Сb@�8�@�l�@�Mj@�<�@�c�@̬�@�R�@��N@�Z�@ɜ@�O@�K�@���@�C�@�e@�-�@ȯO@ȵ@�v�@��d@���@��@��@î@�1�@¤�@���@��H@��@�J�@��@��=@�O�@�@��@�j@���@��@�M@�s@�@��@��@�Ov@��@���@�]�@��|@���@��.@��"@�!�@��m@�\�@��H@�|@�$t@��@��@��0@��	@�_p@��@��c@���@�xl@�/�@��@�{J@���@��o@��r@�\)@�U2@��&@��S@�[W@�@��y@���@�\�@��K@�7L@���@��@�:�@��"@�t�@�dZ@�J�@��@�ں@�oi@�J@���@�k�@���@��9@�v�@�/�@��}@��7@�O@��@�͟@�_@���@�8�@��@��K@��_@�]d@�GE@��@�6z@��H@��@�{�@�]d@��@���@��@�xl@�?�@��]@��C@�*0@��@���@���@�l"@�%�@�{@��@��@��~@�Y@��j@��@�R�@��@�s�@�E9@�'�@��E@��r@�X�@�l�@�%F@��Y@�z@���@�Ft@���@�E9@��@���@��@���@��,@���@��I@�q�@�A�@�~@�zx@�4@�6z@�&�@�@��@�$�@�˒@���@�4@��@�tT@�R�@��@���@�\�@��@���@���@�L0@�u@���@��7@�q@���@���@�~�@�YK@���@���@�P�@�#�@�	l@�҉@��O@�n�@�:*@�3�@�&�@�ݘ@��q@��V@�s�@�B�@�
=@���@��Y@�|�@�bN@�;�@��@�˒@���@�m]@�L�@��@���@�kQ@�YK@�{@��{@��@��`@��9@�d�@�Ov@�C-@��@��^@��h@�w2@�b�@�IR@�.I@��@���@���@�_�@�N�@�(�@��T@��@���@���@��V@�W?@��B@��9@��_@�c @�'R@�@�Q@t�@8@~Ta@|�@|�D@|7�@{�6@{��@{��@{)_@zn�@z	@yDg@x��@x��@x�@x`�@w�+@w�@wv`@vȴ@vH�@u�@u�@u�X@uT�@u?}@u�@t�?@t��@t��@t`�@tM@s�F@sx@s$t@rL0@q�9@qrG@qS&@p��@pc�@o�4@o�@n��@n��@mJ�@l�O@l�@k�@k!-@ji�@i��@i��@i�=@i��@i��@iG�@h��@hc�@g�g@g/�@f�b@fp;@fL0@f-@fJ@e�h@e@dI�@cU�@b}V@b_�@b-@a�#@ak�@a\�@a�@``�@_��@^��@^��@^�@]��@]rG@\��@\Q�@\�@[��@[]�@[>�@[6z@[�@[ i@Z�\@Y��@Y8�@Xoi@W��@W$t@V�b@V�@U�=@U	l@T�$@T1'@S��@S��@S�@R� @R�@Q�@Q}�@Q�@PN�@O��@Ob�@N��@N��@Nȴ@N��@NkQ@M�d@Mp�@Lѷ@L7�@K�K@K��@K�{@KA�@J��@I�.@I�@I�N@I�@H��@Hz�@Hx@G�$@G�@F}V@FR�@E�#@E��@EQ�@E!�@D��@D�@D��@D��@DK^@C�g@Cƨ@C|�@B�H@B�@A��@A��@A�7@AV@@�)@@Ft@?b�@>�@>��@>l�@>H�@=��@<�v@<y>@<4n@;��@;v`@;W?@;4�@:��@:�<@:u%@:)�@9ԕ@9�t@9c�@9-w@8�P@8u�@8<�@7�&@7�P@7Z�@7K�@7>�@7�@6��@6�]@6ȴ@6��@6~�@6h
@6_@5�H@5�h@5zx@5+�@4�e@4C-@4~@4G@3˒@3�@3��@3�V@3E9@3@2ߤ@2�6@2^5@26�@2�@23�@2_@1��@1��@1f�@1@@0M@/�@/ƨ@/�P@/qv@/o�@/X�@/6z@.�R@.kQ@.V@.?@.)�@.�@._@-�o@-�d@-zx@-�@,�$@,�I@,bN@,�@+�6@+��@+�q@+��@+33@+�@*ں@*�x@*h
@*H�@*	@)��@)IR@(�[@(c�@(6@'��@'�k@'Y@&�2@&��@&��@&ff@&#:@&u@%��@%�3@%`B@%�@$�@$`�@#�@@#H�@#S�@"�@"�F@"h
@"Ta@"Q@!�.@![W@!?}@!+�@!@ ��@ ��@ �/@ z�@ A�@ b@�W@��@E9@S@��@Q@�@�@��@L�@	l@�5@ѷ@��@U2@2�@*�@�@˒@��@O@��@ȴ@��@�R@�}@q�@)�@{@_@�@��@��@��@`B@8�@�f@ی@Ĝ@|�@6@�@��@��@��@RT@'�@S@��@~�@�@�#@��@x�@:�@�@�E@�@1'@!@�@��@��@Mj@.I@�@��@�@��@z@i�@Ta@_@�@@��@e,@<6@�5@�@��@j@bN@�@�
@�K@�V@iD@"�@��@�H@��@�@YK@1�@O@�@��@�>@�@@�C@e,@0�@�@ی@�_@w�@h�@U2@M@Ft@%�@1@�@�a@�*@�f@a@)_@'�@&@"�@
�8@
��@
�!@
��@
��@
c @
Ta@
@�@
!�@
@
�@	�.@	�>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`A�\)A�X�A�5A��)A֪�A�}�A�tTA�kQA�d�A�_A�T�A�P�A�K�A�GA�C�A�A�A�?HA�9XA�7�A�4nA�-CA�(�A��A��A�R�A���AԄ�A�!�A�y>AϵAʋDAƅA��A�l"A���A�T�A��pA�X�A��A�\]A�<�A��)A��tA�/OA��~A���A���A���A���A�ѷA�J�A��A��A��A�3�A���A���A���A�u�A��A��\A�u%A�i�A���A���A���A��A��/A�$A�~�A�{A��CA��HA�kA���A��A���A���A��LA�	�A�<A�J#A�2aA�� A�TaA��vA���A���A��6A�	lA�,�A�Az{As��Ar�rAq��Aj��Ahs�Ad�fA`W?A]��A[XAZ��AYH�AWC�AVںAVK^AT�MARf�AOjAJ�AF�HAF�AD��ACt�AB(A?��A>oA=SA<�'A;�A9�A8��A7x�A3��A1�RA0�A.�fA,	lA+ƨA,�A,VA,�A-�)A.�A-c A+�/A,$�A,�A,.�A+J#A*1�A(�RA'-�A&;dA%;dA#�A#�A"`BA!��A!SA oiA�:AbNA�LA��Az�AE9A�?A�AW?Ae,A}VA֡AO�AU2AN�A7�A��AjA%�A�A��AiDA^5A;dA�A�~A�AG�A{JAA{JAOA��AA��AMA��AV�A
�5A
oiA
GEA
A	��A	��A	H�A	�A��A�	A�*AA�Ab�A�
A�fA �A��Aw�A�VA�>AU�A��A �A a�@��]@�Mj@���@���@�PH@�:�@���@��m@��@���@�;�@�;@���@�,�@�@@���@�V�@�L0@�9X@��@��b@�4@���@��*@��"@��@�h�@�2a@�oi@��@�e�@�C�@�@@�<6@@��d@�F�@첖@��@�$@��@�W�@��@��,@�xl@��@�z@�y>@�bN@�K^@�M@��@㐗@�n@�U�@�+�@���@�t@���@��@�Z�@��Q@�@@��Q@��Z@ݪ�@�}�@���@��@��P@�r�@�	�@ٷ@���@�A�@��@���@�y�@��@�~(@�e@�*0@ԕ�@Ե�@Ԛ�@�z�@��A@�=@��m@ҵ@�ѷ@���@ҹ�@һ�@�Z�@�s@��@Сb@�8�@�l�@�Mj@�<�@�c�@̬�@�R�@��N@�Z�@ɜ@�O@�K�@���@�C�@�e@�-�@ȯO@ȵ@�v�@��d@���@��@��@î@�1�@¤�@���@��H@��@�J�@��@��=@�O�@�@��@�j@���@��@�M@�s@�@��@��@�Ov@��@���@�]�@��|@���@��.@��"@�!�@��m@�\�@��H@�|@�$t@��@��@��0@��	@�_p@��@��c@���@�xl@�/�@��@�{J@���@��o@��r@�\)@�U2@��&@��S@�[W@�@��y@���@�\�@��K@�7L@���@��@�:�@��"@�t�@�dZ@�J�@��@�ں@�oi@�J@���@�k�@���@��9@�v�@�/�@��}@��7@�O@��@�͟@�_@���@�8�@��@��K@��_@�]d@�GE@��@�6z@��H@��@�{�@�]d@��@���@��@�xl@�?�@��]@��C@�*0@��@���@���@�l"@�%�@�{@��@��@��~@�Y@��j@��@�R�@��@�s�@�E9@�'�@��E@��r@�X�@�l�@�%F@��Y@�z@���@�Ft@���@�E9@��@���@��@���@��,@���@��I@�q�@�A�@�~@�zx@�4@�6z@�&�@�@��@�$�@�˒@���@�4@��@�tT@�R�@��@���@�\�@��@���@���@�L0@�u@���@��7@�q@���@���@�~�@�YK@���@���@�P�@�#�@�	l@�҉@��O@�n�@�:*@�3�@�&�@�ݘ@��q@��V@�s�@�B�@�
=@���@��Y@�|�@�bN@�;�@��@�˒@���@�m]@�L�@��@���@�kQ@�YK@�{@��{@��@��`@��9@�d�@�Ov@�C-@��@��^@��h@�w2@�b�@�IR@�.I@��@���@���@�_�@�N�@�(�@��T@��@���@���@��V@�W?@��B@��9@��_@�c @�'R@�@�Q@t�@8@~Ta@|�@|�D@|7�@{�6@{��@{��@{)_@zn�@z	@yDg@x��@x��@x�@x`�@w�+@w�@wv`@vȴ@vH�@u�@u�@u�X@uT�@u?}@u�@t�?@t��@t��@t`�@tM@s�F@sx@s$t@rL0@q�9@qrG@qS&@p��@pc�@o�4@o�@n��@n��@mJ�@l�O@l�@k�@k!-@ji�@i��@i��@i�=@i��@i��@iG�@h��@hc�@g�g@g/�@f�b@fp;@fL0@f-@fJ@e�h@e@dI�@cU�@b}V@b_�@b-@a�#@ak�@a\�@a�@``�@_��@^��@^��@^�@]��@]rG@\��@\Q�@\�@[��@[]�@[>�@[6z@[�@[ i@Z�\@Y��@Y8�@Xoi@W��@W$t@V�b@V�@U�=@U	l@T�$@T1'@S��@S��@S�@R� @R�@Q�@Q}�@Q�@PN�@O��@Ob�@N��@N��@Nȴ@N��@NkQ@M�d@Mp�@Lѷ@L7�@K�K@K��@K�{@KA�@J��@I�.@I�@I�N@I�@H��@Hz�@Hx@G�$@G�@F}V@FR�@E�#@E��@EQ�@E!�@D��@D�@D��@D��@DK^@C�g@Cƨ@C|�@B�H@B�@A��@A��@A�7@AV@@�)@@Ft@?b�@>�@>��@>l�@>H�@=��@<�v@<y>@<4n@;��@;v`@;W?@;4�@:��@:�<@:u%@:)�@9ԕ@9�t@9c�@9-w@8�P@8u�@8<�@7�&@7�P@7Z�@7K�@7>�@7�@6��@6�]@6ȴ@6��@6~�@6h
@6_@5�H@5�h@5zx@5+�@4�e@4C-@4~@4G@3˒@3�@3��@3�V@3E9@3@2ߤ@2�6@2^5@26�@2�@23�@2_@1��@1��@1f�@1@@0M@/�@/ƨ@/�P@/qv@/o�@/X�@/6z@.�R@.kQ@.V@.?@.)�@.�@._@-�o@-�d@-zx@-�@,�$@,�I@,bN@,�@+�6@+��@+�q@+��@+33@+�@*ں@*�x@*h
@*H�@*	@)��@)IR@(�[@(c�@(6@'��@'�k@'Y@&�2@&��@&��@&ff@&#:@&u@%��@%�3@%`B@%�@$�@$`�@#�@@#H�@#S�@"�@"�F@"h
@"Ta@"Q@!�.@![W@!?}@!+�@!@ ��@ ��@ �/@ z�@ A�@ b@�W@��@E9@S@��@Q@�@�@��@L�@	l@�5@ѷ@��@U2@2�@*�@�@˒@��@O@��@ȴ@��@�R@�}@q�@)�@{@_@�@��@��@��@`B@8�@�f@ی@Ĝ@|�@6@�@��@��@��@RT@'�@S@��@~�@�@�#@��@x�@:�@�@�E@�@1'@!@�@��@��@Mj@.I@�@��@�@��@z@i�@Ta@_@�@@��@e,@<6@�5@�@��@j@bN@�@�
@�K@�V@iD@"�@��@�H@��@�@YK@1�@O@�@��@�>@�@@�C@e,@0�@�@ی@�_@w�@h�@U2@M@Ft@%�@1@�@�a@�*@�f@a@)_@'�@&@"�@
�8@
��@
�!@
��@
��@
c @
Ta@
@�@
!�@
@
�@	�.@	�>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BT�BT�BTaBSuBR�BQhBQ�BQ�BRTBR�BR�BR�BR�BR�BR�BSBSBS@BS�BS�BS�BTFBTaBU�BYKB^B\�BWYBQhB5�B)B
�rB
��B
̘B
��B
=BGEBV�Bb�Bl�By$B��B�B�B�:B�9B�B�_B��B�&B��B�2B��B 4B�BpB B��B 4B  B;B�BB�BGB��B�$B��B��B��B�}B�B�BרB�hB̘B�?B��B��Bz�Bl�B]/BKxB7fBB iB
چB
��B
��B
l�B
R�B
3�B
fB	ߊB	�8B	��B	�*B	�B	t�B	dtB	Q B	B�B	9>B	2�B	/�B	.IB	1B	F%B	BB	6�B	$�B	�B	�B	�B	�B	{B	�B	$�B	.IB	1vB	1'B	0�B	2B	1�B	-�B	.�B	7�B	5�B	*�B	'�B	5�B	I�B	[�B	{B	�6B	�EB	ބB	�/B	�*B
uB
�B
.B

�B
uB	�LB	�AB	��B	�B	�B	�B	�XB	�B	�`B	�B	��B	ݘB	�qB	�WB	��B	��B	خB	�gB	уB	ΥB	̳B	�=B	ɆB	ȀB	ɆB	�lB	ɠB	��B	��B	�DB	��B	̘B	�jB	��B	�B	�jB	�B	ˬB	��B	ʌB	ʌB	�rB	�xB	��B	��B	�B	�gB	�YB	רB	��B	��B	�~B	�IB	�/B	��B	޸B	ܒB	�xB	�qB	خB	��B	ңB	�@B	��B	ԕB	��B	�~B	�B	��B	߾B	�5B	ٚB	ּB	�2B	ԯB	خB	�B	��B	�.B	ϑB	��B	׍B	��B	�B	�KB	�)B	�B	�B	�'B	��B	�B	�B	�B	�IB	�]B	�oB	�5B	�B	�B	��B	��B	�B	�B	�TB	��B	��B	�?B	�B	�'B	� B	�"B	�_B	�ZB	�B	�B	� B	�NB	�B	��B	�;B	�B	�B	�8B	�DB	�B	�qB	�B	�AB	��B	�B	�nB	�B	��B	�B	��B	��B	�B	�B	��B	�0B	�^B	�dB	�dB	��B	�^B	��B	��B	�+B	��B	�+B	�`B	�FB	��B	��B	�zB	�ZB	�B	��B	��B	��B	�`B	��B	��B	��B	��B	��B	�B	�qB	�jB	�0B	�xB	�0B	��B	��B	��B	�B	�8B	�fB	�B	�-B	��B	��B	�B	�`B	�tB	��B	��B	��B	�wB
 iB	��B	��B	��B	�hB	�hB	�hB	��B	�	B	��B	��B	��B	��B	��B	�	B	�>B	�	B	��B	�B	�	B	�RB	��B	��B	��B	��B	�"B	�HB	�B	�HB	�HB	��B	��B	��B
  B
 4B
 OB
 �B
 B
 B
�B
B
�B
B
B
'B
�B
B
[B
AB
B
�B
�B
'B
oB	��B	�wB	��B	�cB	��B	��B	�wB	��B	�(B	��B	�VB	��B	�0B	��B	��B	�*B	�*B	��B	�B	��B	�xB	�^B	�^B	��B	��B	�B	��B	��B	��B	�B	�JB	�B	�JB	��B	��B	��B	��B	�B	��B	��B	��B	��B
 iB
 �B
 �B
 �B
 �B
AB
�B
aB
�B
aB
-B
�B
AB
�B
�B
GB
�B
�B
gB
gB
�B
�B
MB
�B
B
B
�B
_B
�B
�B
?B
�B
_B
KB
�B
�B

rB
~B
�B
(B
 B
 B
B
 B
 B
 B
�B
�B
�B
sB
�B
xB
B
B
B
�B
�B
�B
jB
�B
�B
�B
 'B
 'B
 \B
 �B
 �B
 �B
!|B
!�B
!�B
"NB
"�B
"�B
# B
#TB
#�B
$@B
$@B
$�B
%B
%B
$�B
$�B
$�B
$�B
%,B
%B
%,B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&fB
&B
&2B
&B
&B
%�B
&2B
&2B
&LB
&fB
&�B
'RB
'RB
'8B
'�B
'�B
'B
'�B
(�B
)*B
)B
)DB
)yB
*�B
*�B
+B
+6B
+�B
+kB
,=B
+�B
,B
,WB
,WB
,�B
-wB
-wB
/B
/�B
/iB
0oB
1'B
1B
0�B
1vB
1�B
1�B
1�B
1�B
1�B
2�B
3MB
33B
3B
33B
3B
3B
33B
3�B
3�B
4�B
4�B
4�B
5B
5?B
5�B
6+B
6�B
7fB
7�B
8B
7�B
8B
8B
8B
88B
8B
8RB
8�B
8�B
8�B
8�B
9>B
9rB
9�B
9�B
:^B
:B
:�B
:�B
;�B
<B
;�B
<B
="B
=<B
=�B
>B
>]B
>�B
?B
?.B
?.B
?cB
?.B
?cB
?�B
?�B
@ B
@�B
@�B
@�B
@�B
@�B
@�B
A;B
A;B
A�B
B�B
B�B
B�B
B�B
C-B
CaB
CGB
CaB
C�B
DB
DMB
DB
D�B
D�B
D�B
EmB
ESB
E�B
FYB
FtB
F�B
F�B
F�B
FtB
F�B
GzB
G�B
H�B
H�B
H�B
IlB
I�B
I�B
J=B
JrB
I�B
I�B
J=B
I�B
IRB
H�B
H�B
HKB
HKB
G�B
G�B
HKB
H�B
IB
I�B
I�B
J=B
I�B
I�B
JrB
J�B
J�B
J�B
J�B
K)B
K�B
K�B
K�B
KxB
K�B
L0B
L~B
L�B
L�B
MB
MPB
M�B
NB
NB
N<B
NVB
N�B
NpB
N�B
N�B
N�B
O\B
OBB
OvB
O�B
P�B
P�B
P�B
Q B
QNB
Q�B
RB
SB
S&B
S�B
S�B
S�B
TFB
T�B
U�B
U�B
U�B
V�B
VmB
V�B
V�B
W
B
W?B
WsB
W�B
W�B
W�B
X�B
X�B
YeB
Y1B
Y�B
ZB
Z7B
ZQB
Z�B
[	B
[	B
[#B
[=B
[qB
[qB
[qB
[�B
\)B
\xB
\]B
\�B
]B
]B
]/B
\�B
]/B
\�B
]/B
]~B
^B
^B
]�B
^B
^B
^jB
_!B
_�B
`B
`BB
`B
_�B
`'B
_�B
_;B
_VB
_�B
_�B
_�B
_�B
_�B
`B
`vB
`�B
`�B
`�B
abB
a�B
a�B
a�B
a�B
bNB
a�B
a�B
a�B
bB
b�B
b�B
bhB
bNB
b�B
cB
cB
c B
c�B
d&B
d�B
d�B
d�B
d�B
e,B
ezB
e�B
fB
f�B
f�B
gB
g�B
g�B
h�B
jB
jB
jB
jeB
j0B
jB
i�B
jeB
k6B
k�B
lqB
l�B
l�B
mB
m)B
mwB
m�B
nB
m�B
n/B
n/B
m�B
m�B
n/B
n}B
n�B
n�B
o B
o5B
oiB
o�B
o�B
o�B
o�B
pB
pUB
poB
p�B
p�B
p�B
qB
q'B
q'B
qAB
q�B
q�B
q�B
r|B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tB
tTB
t9B
t�B
t�B
t�B
u?B
u?B
uZB
u�B
vB
v+B
vFB
vzB
v�B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
xB
xRB
x�B
x�B
x�B
y$B
yrB
y�B
y�B
y�B
zDB
z�B
z�B
{B
{dB
{B
{�B
|B
|6B
|PB
|�B
|�B
|�B
}B
}<B
}<B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
.B
}B
�B
�B
�B
�B
�B
�4B
�4B
�OB
�OB
��B
��B
��B
��B
�B
�;B
�UB
�UB
��B
��B
��B
�B
��B
�B
�AB
�uB
��B
��B
��B
�B
�B
�GB
�-B
�aB
�aB
��B
��B
��B
��B
�B
�MB
�MB
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BT�BT�BTaBSuBR�BQhBQ�BQ�BRTBR�BR�BR�BR�BR�BR�BSBSBS@BS�BS�BS�BTFBTaBU�BYKB^B\�BWYBQhB5�B)B
�rB
��B
̘B
��B
=BGEBV�Bb�Bl�By$B��B�B�B�:B�9B�B�_B��B�&B��B�2B��B 4B�BpB B��B 4B  B;B�BB�BGB��B�$B��B��B��B�}B�B�BרB�hB̘B�?B��B��Bz�Bl�B]/BKxB7fBB iB
چB
��B
��B
l�B
R�B
3�B
fB	ߊB	�8B	��B	�*B	�B	t�B	dtB	Q B	B�B	9>B	2�B	/�B	.IB	1B	F%B	BB	6�B	$�B	�B	�B	�B	�B	{B	�B	$�B	.IB	1vB	1'B	0�B	2B	1�B	-�B	.�B	7�B	5�B	*�B	'�B	5�B	I�B	[�B	{B	�6B	�EB	ބB	�/B	�*B
uB
�B
.B

�B
uB	�LB	�AB	��B	�B	�B	�B	�XB	�B	�`B	�B	��B	ݘB	�qB	�WB	��B	��B	خB	�gB	уB	ΥB	̳B	�=B	ɆB	ȀB	ɆB	�lB	ɠB	��B	��B	�DB	��B	̘B	�jB	��B	�B	�jB	�B	ˬB	��B	ʌB	ʌB	�rB	�xB	��B	��B	�B	�gB	�YB	רB	��B	��B	�~B	�IB	�/B	��B	޸B	ܒB	�xB	�qB	خB	��B	ңB	�@B	��B	ԕB	��B	�~B	�B	��B	߾B	�5B	ٚB	ּB	�2B	ԯB	خB	�B	��B	�.B	ϑB	��B	׍B	��B	�B	�KB	�)B	�B	�B	�'B	��B	�B	�B	�B	�IB	�]B	�oB	�5B	�B	�B	��B	��B	�B	�B	�TB	��B	��B	�?B	�B	�'B	� B	�"B	�_B	�ZB	�B	�B	� B	�NB	�B	��B	�;B	�B	�B	�8B	�DB	�B	�qB	�B	�AB	��B	�B	�nB	�B	��B	�B	��B	��B	�B	�B	��B	�0B	�^B	�dB	�dB	��B	�^B	��B	��B	�+B	��B	�+B	�`B	�FB	��B	��B	�zB	�ZB	�B	��B	��B	��B	�`B	��B	��B	��B	��B	��B	�B	�qB	�jB	�0B	�xB	�0B	��B	��B	��B	�B	�8B	�fB	�B	�-B	��B	��B	�B	�`B	�tB	��B	��B	��B	�wB
 iB	��B	��B	��B	�hB	�hB	�hB	��B	�	B	��B	��B	��B	��B	��B	�	B	�>B	�	B	��B	�B	�	B	�RB	��B	��B	��B	��B	�"B	�HB	�B	�HB	�HB	��B	��B	��B
  B
 4B
 OB
 �B
 B
 B
�B
B
�B
B
B
'B
�B
B
[B
AB
B
�B
�B
'B
oB	��B	�wB	��B	�cB	��B	��B	�wB	��B	�(B	��B	�VB	��B	�0B	��B	��B	�*B	�*B	��B	�B	��B	�xB	�^B	�^B	��B	��B	�B	��B	��B	��B	�B	�JB	�B	�JB	��B	��B	��B	��B	�B	��B	��B	��B	��B
 iB
 �B
 �B
 �B
 �B
AB
�B
aB
�B
aB
-B
�B
AB
�B
�B
GB
�B
�B
gB
gB
�B
�B
MB
�B
B
B
�B
_B
�B
�B
?B
�B
_B
KB
�B
�B

rB
~B
�B
(B
 B
 B
B
 B
 B
 B
�B
�B
�B
sB
�B
xB
B
B
B
�B
�B
�B
jB
�B
�B
�B
 'B
 'B
 \B
 �B
 �B
 �B
!|B
!�B
!�B
"NB
"�B
"�B
# B
#TB
#�B
$@B
$@B
$�B
%B
%B
$�B
$�B
$�B
$�B
%,B
%B
%,B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&fB
&B
&2B
&B
&B
%�B
&2B
&2B
&LB
&fB
&�B
'RB
'RB
'8B
'�B
'�B
'B
'�B
(�B
)*B
)B
)DB
)yB
*�B
*�B
+B
+6B
+�B
+kB
,=B
+�B
,B
,WB
,WB
,�B
-wB
-wB
/B
/�B
/iB
0oB
1'B
1B
0�B
1vB
1�B
1�B
1�B
1�B
1�B
2�B
3MB
33B
3B
33B
3B
3B
33B
3�B
3�B
4�B
4�B
4�B
5B
5?B
5�B
6+B
6�B
7fB
7�B
8B
7�B
8B
8B
8B
88B
8B
8RB
8�B
8�B
8�B
8�B
9>B
9rB
9�B
9�B
:^B
:B
:�B
:�B
;�B
<B
;�B
<B
="B
=<B
=�B
>B
>]B
>�B
?B
?.B
?.B
?cB
?.B
?cB
?�B
?�B
@ B
@�B
@�B
@�B
@�B
@�B
@�B
A;B
A;B
A�B
B�B
B�B
B�B
B�B
C-B
CaB
CGB
CaB
C�B
DB
DMB
DB
D�B
D�B
D�B
EmB
ESB
E�B
FYB
FtB
F�B
F�B
F�B
FtB
F�B
GzB
G�B
H�B
H�B
H�B
IlB
I�B
I�B
J=B
JrB
I�B
I�B
J=B
I�B
IRB
H�B
H�B
HKB
HKB
G�B
G�B
HKB
H�B
IB
I�B
I�B
J=B
I�B
I�B
JrB
J�B
J�B
J�B
J�B
K)B
K�B
K�B
K�B
KxB
K�B
L0B
L~B
L�B
L�B
MB
MPB
M�B
NB
NB
N<B
NVB
N�B
NpB
N�B
N�B
N�B
O\B
OBB
OvB
O�B
P�B
P�B
P�B
Q B
QNB
Q�B
RB
SB
S&B
S�B
S�B
S�B
TFB
T�B
U�B
U�B
U�B
V�B
VmB
V�B
V�B
W
B
W?B
WsB
W�B
W�B
W�B
X�B
X�B
YeB
Y1B
Y�B
ZB
Z7B
ZQB
Z�B
[	B
[	B
[#B
[=B
[qB
[qB
[qB
[�B
\)B
\xB
\]B
\�B
]B
]B
]/B
\�B
]/B
\�B
]/B
]~B
^B
^B
]�B
^B
^B
^jB
_!B
_�B
`B
`BB
`B
_�B
`'B
_�B
_;B
_VB
_�B
_�B
_�B
_�B
_�B
`B
`vB
`�B
`�B
`�B
abB
a�B
a�B
a�B
a�B
bNB
a�B
a�B
a�B
bB
b�B
b�B
bhB
bNB
b�B
cB
cB
c B
c�B
d&B
d�B
d�B
d�B
d�B
e,B
ezB
e�B
fB
f�B
f�B
gB
g�B
g�B
h�B
jB
jB
jB
jeB
j0B
jB
i�B
jeB
k6B
k�B
lqB
l�B
l�B
mB
m)B
mwB
m�B
nB
m�B
n/B
n/B
m�B
m�B
n/B
n}B
n�B
n�B
o B
o5B
oiB
o�B
o�B
o�B
o�B
pB
pUB
poB
p�B
p�B
p�B
qB
q'B
q'B
qAB
q�B
q�B
q�B
r|B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tB
tTB
t9B
t�B
t�B
t�B
u?B
u?B
uZB
u�B
vB
v+B
vFB
vzB
v�B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
xB
xRB
x�B
x�B
x�B
y$B
yrB
y�B
y�B
y�B
zDB
z�B
z�B
{B
{dB
{B
{�B
|B
|6B
|PB
|�B
|�B
|�B
}B
}<B
}<B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
.B
}B
�B
�B
�B
�B
�B
�4B
�4B
�OB
�OB
��B
��B
��B
��B
�B
�;B
�UB
�UB
��B
��B
��B
�B
��B
�B
�AB
�uB
��B
��B
��B
�B
�B
�GB
�-B
�aB
�aB
��B
��B
��B
��B
�B
�MB
�MB
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104923  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173940  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173940  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173940                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023947  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023947  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                