CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:36:08Z creation;2022-06-04T17:36:08Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173608  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               KA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�`�*�1   @�`�&�7�@0+I�^�c�-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�ffB���B�  B�  Bę�B�33B̙�B�  B�ffBי�B�  B�  B���B�  B�  B�  B�  B�33B�ffB�ffC�fC  C�fC  C
  C  C  C  C  C  C  C�C�C�fC  C�fC!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CU�fCW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@x��@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B�.B�aGB���B�aGB�ǮB���B���BĔ{B�.B̔{B���B�aGBה{B���B���B�ǮB���B���B���B���B�.B�aGB�aGC��C�qC��C�qC	�qC�qC�qC�qC�qC�qC�qCCC��C�qC��C!��C#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCRCS�qCU��CW��CY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Dt�Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�<{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��zA�ȀA���A��XA���A�ȀA�ǮA���A��A�ÖA���Aȿ}A�ƨA��
A���AȋxAȋxAȄ�AȂA�}�A�v`A�n�A�oiA�tTAȀiAȌ�Aȗ$AȤAȴAȸA��BA��}A��A��A�A��A��A� �A�YAȻ�A�z�A�6FA�*0A�%A��A�ĜA��A�A���A�(�A�(�A�jKA���A�l�A�v+A���A��IA�m�A���A���A�D�A���A�rA���A���A���A��zA�i�A��nA�� A���A���A��A|@Ay-�Av�As
�Ao�Ai�Ah0UAe)�Ac�FAa��A_�LA^��A\�oAYݘAW�zAV�AU:�AS2aAP��AN!-ALVmAK��AI��AFQ�AC�WAA�EAAVA@(�A>��A=	A;��A;!�A8+kA7�A7��A7&A6�A68A4��A4ȴA4��A4J�A3�A3.�A1��A0$�A/��A.�/A.\)A.(A-�A-��A,�LA+��A,y�A,�zA+��A,2aA,�A+��A+�[A*�dA*J�A)��A)�A)�A)X�A)?�A(�A(=A'��A'g8A&�gA&YKA%�cA%��A$��A$JA#��A#�tA#y>A"��A"��A!��A!�7A!�A!�A �AA ��A Q�A��A�PAx�A+�AjAAB�AX�AzxA��A|�AXA�A�A�yA�dAzxA�AVA�A��A`�A�A�LA��A�A��APHA��A��A�AN�A�EA=�A'RA�Al"A�AD�A=AȴA��ASA�dA}�A(�A�0Ay>A33AA˒A�oAXA!-A�yAaA
�VA	��A	C�A	4A��A��A-wA�A	A��APHA1�A(A�AQ�A�/AU2A)_A1A�AiDA�'AS�A-�A
�A�yA~�A0UA6zA'�A �<A W�A /�@���@�8@�&@��@��@���@�r�@�	@��n@�Z�@��X@�oi@�Mj@���@�Mj@�kQ@�@��@��@���@��3@���@��@�B[@�>B@�M�@��@�n/@�A�@�@�+@�}V@�u@���@�O�@��@���@��8@�%F@��@���@�O@�J#@�8�@���@�ߤ@䍹@䟾@��@�'R@�S�@ᦵ@��P@��@�|�@��N@�=@ގ�@���@�U�@�oi@�tT@�	�@ڏ\@�>B@���@�a@ؽ<@�`�@�B[@��@�x�@�ں@�Ft@��@՞�@�F@ԃ@ӿH@�J�@��@ҟ�@�V�@��@��@�kQ@��@��@ή}@�8�@���@ͮ�@�IR@̛�@��@˜@�8@��B@�r�@�|�@ȣ@��&@ǌ~@���@�~(@�H@��Z@�@�S�@�u@Ê	@��@���@�H�@�$t@��M@��\@�^5@��@���@�[W@�.I@�@��@��)@���@�z�@�($@�s�@�$t@��@��@���@��h@��A@�?�@�$�@��+@���@��@�A�@��]@�K^@��@��D@��@���@�Z�@���@�$t@�K^@�2�@���@�~�@��@��9@�Q�@�2a@��7@�u�@��@��@��@���@��@���@�e@���@��{@��,@�:�@��@�خ@��}@���@�YK@���@�`�@�-@��.@�@�خ@�S�@�҉@��9@��@�%�@��@�@�c�@�+@�Y@���@��c@��@��2@���@��H@��/@��s@�ѷ@��)@��z@�u%@�V�@�$�@�u@��3@�:�@�@���@�H@��K@���@�@��p@���@�=q@��@�	@���@���@���@�X�@�'�@���@�9X@�O@��&@�iD@�l�@���@��p@�ѷ@���@��[@��p@��)@��@���@��@���@�e�@�u@���@�n/@�Ɇ@�z�@�z@�O@��^@��m@��@���@�i�@�Ov@�@��@���@�a�@���@���@�ff@��@���@���@���@�c�@��@�o@���@���@���@���@�kQ@�K^@�%�@���@�'�@�ی@���@�q�@�PH@�-�@��@���@��P@�v`@�k�@�K�@�?}@�Y@��@�_@�,=@�@���@��@���@��:@�4@��@��2@��@���@�m�@�-�@�4@�@��z@��*@�0�@�*�@�X@���@�v�@�I�@� �@�u�@�@O@�5�@�"�@�@@�
=@��"@���@��@���@�!@��+@���@�Dg@�;d@��@���@���@��2@���@���@��@�2�@���@���@�}�@�q@��E@��@�}V@�E�@�0U@��@���@��n@�hs@�F�@�(@���@�K^@�?@��@��@���@��@��@��y@���@��1@�\�@�E�@�1�@��@��@��k@�}�@�qv@�S&@�Y@���@��@�� @��0@��X@���@���@��	@�hs@� \@���@��@��@��@��@>�@~R�@}[W@|y>@|G@{��@{1�@z�@yS&@x�/@x֡@x!@wƨ@wW?@v��@v�@v�'@v�@v��@v��@ue,@t�4@s��@s��@r�c@r^5@r3�@q��@q4@p�E@o�P@o i@nQ@m��@m�@mN<@l��@l�@l��@k�Q@k_p@k�@jW�@jJ@i�j@iϫ@i�t@i��@i��@ij@i<6@iV@i@@i	l@i;@h�@h�@h�[@h�9@hbN@g��@g�F@ga@gS@f��@fJ@eQ�@e	l@d�`@dĜ@d�_@dM@d�@c�F@co@bc @a|@`z�@_�r@_� @_�@@_�k@_�{@_�@^�h@^��@]�@]k�@]j@]L�@]�@\�`@\��@\��@\6@[��@[dZ@Z��@ZH�@Y��@Yhs@X�@Xc�@XS�@X7�@XM@X@W�a@WO@WC@V��@V�@V�+@U�@U��@UX@T�@T��@TI�@S��@Sخ@S��@SW?@R��@RkQ@R �@Q�'@Q*0@P��@P�@P��@Pѷ@P��@PbN@P~@O�m@O�F@O=@N�2@Nz@M�@M?}@M�@L��@L�/@L]d@L"h@L�@K��@K�@K��@Kt�@K9�@J�2@J$�@I/@H-�@G��@F��@FJ�@E�"@EO�@D��@DɆ@D��@D2�@C�Q@C��@C(@B��@B�+@B	@A�^@A�"@AX@A�@@��@@z�@?��@?b�@?�@>҉@>��@>��@>kQ@=�T@=N<@=�@<��@<�@<  @;y�@;e�@;)_@:�H@:��@:�\@:-@9�@9c@8�5@8oi@8h�@8,=@7��@7l�@7H�@7!-@6�@6�@6��@7@7@6��@6�,@6͟@6�'@6�@6��@6��@6{�@6Q@6{@5�@5&�@5V@4��@4��@3�k@3x@2�2@2q�@1ϫ@1zx@1S&@1?}@1<6@1(�@0y>@/�m@/�@@/y�@/!-@.�@.M�@-��@,�$@,��@,7@+��@+U�@*�8@*$�@)�9@)�h@)8�@(Ĝ@(��@(U2@(1@'�@&�y@&W�@&J@%�@%�@%x�@%Q�@%*0@%@%�@$�5@$�@$ѷ@$��@$��@$tT@$V�@$%�@#�@#P�@#+@# i@"�X@"kQ@"{@!�@!��@!e,@!/@ ��@�@,�@��@�s@��@~�@)�@�~@&�@�@�P@֡@ی@��@�_@|�@_@V�@C-@�@�m@��@Y@��@�L@M�@ԕ@?}@�v@�$@�4@�@PH@x@�K@�*@�P@��@��@��@{J@t�@t�@g�@&@{�@��@m]@:�@�[@�@�4@��@��@tT@M@~@��@U�@Y@ i@�@��@�@��@kQ@$�@�T@�'@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��zA�ȀA���A��XA���A�ȀA�ǮA���A��A�ÖA���Aȿ}A�ƨA��
A���AȋxAȋxAȄ�AȂA�}�A�v`A�n�A�oiA�tTAȀiAȌ�Aȗ$AȤAȴAȸA��BA��}A��A��A�A��A��A� �A�YAȻ�A�z�A�6FA�*0A�%A��A�ĜA��A�A���A�(�A�(�A�jKA���A�l�A�v+A���A��IA�m�A���A���A�D�A���A�rA���A���A���A��zA�i�A��nA�� A���A���A��A|@Ay-�Av�As
�Ao�Ai�Ah0UAe)�Ac�FAa��A_�LA^��A\�oAYݘAW�zAV�AU:�AS2aAP��AN!-ALVmAK��AI��AFQ�AC�WAA�EAAVA@(�A>��A=	A;��A;!�A8+kA7�A7��A7&A6�A68A4��A4ȴA4��A4J�A3�A3.�A1��A0$�A/��A.�/A.\)A.(A-�A-��A,�LA+��A,y�A,�zA+��A,2aA,�A+��A+�[A*�dA*J�A)��A)�A)�A)X�A)?�A(�A(=A'��A'g8A&�gA&YKA%�cA%��A$��A$JA#��A#�tA#y>A"��A"��A!��A!�7A!�A!�A �AA ��A Q�A��A�PAx�A+�AjAAB�AX�AzxA��A|�AXA�A�A�yA�dAzxA�AVA�A��A`�A�A�LA��A�A��APHA��A��A�AN�A�EA=�A'RA�Al"A�AD�A=AȴA��ASA�dA}�A(�A�0Ay>A33AA˒A�oAXA!-A�yAaA
�VA	��A	C�A	4A��A��A-wA�A	A��APHA1�A(A�AQ�A�/AU2A)_A1A�AiDA�'AS�A-�A
�A�yA~�A0UA6zA'�A �<A W�A /�@���@�8@�&@��@��@���@�r�@�	@��n@�Z�@��X@�oi@�Mj@���@�Mj@�kQ@�@��@��@���@��3@���@��@�B[@�>B@�M�@��@�n/@�A�@�@�+@�}V@�u@���@�O�@��@���@��8@�%F@��@���@�O@�J#@�8�@���@�ߤ@䍹@䟾@��@�'R@�S�@ᦵ@��P@��@�|�@��N@�=@ގ�@���@�U�@�oi@�tT@�	�@ڏ\@�>B@���@�a@ؽ<@�`�@�B[@��@�x�@�ں@�Ft@��@՞�@�F@ԃ@ӿH@�J�@��@ҟ�@�V�@��@��@�kQ@��@��@ή}@�8�@���@ͮ�@�IR@̛�@��@˜@�8@��B@�r�@�|�@ȣ@��&@ǌ~@���@�~(@�H@��Z@�@�S�@�u@Ê	@��@���@�H�@�$t@��M@��\@�^5@��@���@�[W@�.I@�@��@��)@���@�z�@�($@�s�@�$t@��@��@���@��h@��A@�?�@�$�@��+@���@��@�A�@��]@�K^@��@��D@��@���@�Z�@���@�$t@�K^@�2�@���@�~�@��@��9@�Q�@�2a@��7@�u�@��@��@��@���@��@���@�e@���@��{@��,@�:�@��@�خ@��}@���@�YK@���@�`�@�-@��.@�@�خ@�S�@�҉@��9@��@�%�@��@�@�c�@�+@�Y@���@��c@��@��2@���@��H@��/@��s@�ѷ@��)@��z@�u%@�V�@�$�@�u@��3@�:�@�@���@�H@��K@���@�@��p@���@�=q@��@�	@���@���@���@�X�@�'�@���@�9X@�O@��&@�iD@�l�@���@��p@�ѷ@���@��[@��p@��)@��@���@��@���@�e�@�u@���@�n/@�Ɇ@�z�@�z@�O@��^@��m@��@���@�i�@�Ov@�@��@���@�a�@���@���@�ff@��@���@���@���@�c�@��@�o@���@���@���@���@�kQ@�K^@�%�@���@�'�@�ی@���@�q�@�PH@�-�@��@���@��P@�v`@�k�@�K�@�?}@�Y@��@�_@�,=@�@���@��@���@��:@�4@��@��2@��@���@�m�@�-�@�4@�@��z@��*@�0�@�*�@�X@���@�v�@�I�@� �@�u�@�@O@�5�@�"�@�@@�
=@��"@���@��@���@�!@��+@���@�Dg@�;d@��@���@���@��2@���@���@��@�2�@���@���@�}�@�q@��E@��@�}V@�E�@�0U@��@���@��n@�hs@�F�@�(@���@�K^@�?@��@��@���@��@��@��y@���@��1@�\�@�E�@�1�@��@��@��k@�}�@�qv@�S&@�Y@���@��@�� @��0@��X@���@���@��	@�hs@� \@���@��@��@��@��@>�@~R�@}[W@|y>@|G@{��@{1�@z�@yS&@x�/@x֡@x!@wƨ@wW?@v��@v�@v�'@v�@v��@v��@ue,@t�4@s��@s��@r�c@r^5@r3�@q��@q4@p�E@o�P@o i@nQ@m��@m�@mN<@l��@l�@l��@k�Q@k_p@k�@jW�@jJ@i�j@iϫ@i�t@i��@i��@ij@i<6@iV@i@@i	l@i;@h�@h�@h�[@h�9@hbN@g��@g�F@ga@gS@f��@fJ@eQ�@e	l@d�`@dĜ@d�_@dM@d�@c�F@co@bc @a|@`z�@_�r@_� @_�@@_�k@_�{@_�@^�h@^��@]�@]k�@]j@]L�@]�@\�`@\��@\��@\6@[��@[dZ@Z��@ZH�@Y��@Yhs@X�@Xc�@XS�@X7�@XM@X@W�a@WO@WC@V��@V�@V�+@U�@U��@UX@T�@T��@TI�@S��@Sخ@S��@SW?@R��@RkQ@R �@Q�'@Q*0@P��@P�@P��@Pѷ@P��@PbN@P~@O�m@O�F@O=@N�2@Nz@M�@M?}@M�@L��@L�/@L]d@L"h@L�@K��@K�@K��@Kt�@K9�@J�2@J$�@I/@H-�@G��@F��@FJ�@E�"@EO�@D��@DɆ@D��@D2�@C�Q@C��@C(@B��@B�+@B	@A�^@A�"@AX@A�@@��@@z�@?��@?b�@?�@>҉@>��@>��@>kQ@=�T@=N<@=�@<��@<�@<  @;y�@;e�@;)_@:�H@:��@:�\@:-@9�@9c@8�5@8oi@8h�@8,=@7��@7l�@7H�@7!-@6�@6�@6��@7@7@6��@6�,@6͟@6�'@6�@6��@6��@6{�@6Q@6{@5�@5&�@5V@4��@4��@3�k@3x@2�2@2q�@1ϫ@1zx@1S&@1?}@1<6@1(�@0y>@/�m@/�@@/y�@/!-@.�@.M�@-��@,�$@,��@,7@+��@+U�@*�8@*$�@)�9@)�h@)8�@(Ĝ@(��@(U2@(1@'�@&�y@&W�@&J@%�@%�@%x�@%Q�@%*0@%@%�@$�5@$�@$ѷ@$��@$��@$tT@$V�@$%�@#�@#P�@#+@# i@"�X@"kQ@"{@!�@!��@!e,@!/@ ��@�@,�@��@�s@��@~�@)�@�~@&�@�@�P@֡@ی@��@�_@|�@_@V�@C-@�@�m@��@Y@��@�L@M�@ԕ@?}@�v@�$@�4@�@PH@x@�K@�*@�P@��@��@��@{J@t�@t�@g�@&@{�@��@m]@:�@�[@�@�4@��@��@tT@M@~@��@U�@Y@ i@�@��@�@��@kQ@$�@�T@�'@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Ba�Ba�B`�Ba-BbB`�B`BB`vB_�B_VB_pB^�B_!Bf2B`�BX_BXyBW�BWsBVmBVBVSBW�B[qB_�Bc�BfBh�Bm�Bo Bt9Bw�B�4B�lB�"B�VB��B��B��B�0B��B��B�XB�PB�vB�OB	A B
�B
CB
�B
(�B
%FB
S�B
�TB
��B
��B
�B
�?B
�aB
��B
�RB
��B
��B
�6B
raB
T,B
:�B
)�B
�B
�B	�B	�@B	ĜB	��B	�CB	��B	��B	�B	j�B	a�B	VB	O�B	IB	BB	>BB	;JB	5�B	0�B	.IB	,�B	*�B	+B	(�B	B	#B	�B	�B��B�AB��B��B�cB	�B		�B	�B	�B	�B	A;B	bNB	q�B	s�B	t�B	t�B	z�B	|6B	B	� B	�#B	��B	�zB	�B	�]B	��B	�B	��B	�wB
�B
�B
+6B
-�B
=qB
C-B
C�B
ESB
I�B
J	B
J#B
JXB
L�B
N<B
O�B
O�B
O�B
OBB
O\B
Q B
Q�B
Q�B
Q�B
SB
Q�B
Q�B
Q B
PB
OBB
O�B
OBB
N�B
NVB
M�B
L�B
L0B
K^B
LB
MjB
L�B
M�B
M6B
NB
O�B
K�B
F�B
F�B
F�B
E�B
D�B
D�B
DB
C�B
CaB
BAB
@�B
?B
?�B
@�B
A�B
A B
@�B
?�B
=�B
=VB
A�B
IlB
J	B
O\B
Q�B
RoB
RB
QhB
O�B
M�B
K^B
H�B
F%B
EB
C-B
C�B
DgB
EB
D�B
D�B
C�B
B�B
A B
@B
?}B
>�B
=�B
:�B
6�B
2B
0B
1AB
0!B
/�B
0�B
0B
-�B
-CB
,�B
+�B
+�B
)�B
(�B
'8B
&�B
&LB
%�B
%�B
%,B
$�B
#�B
#�B
#B
%zB
&B
%`B
'mB
(>B
(�B
&�B
%�B
%B
#�B
#�B
$B
$�B
$�B
$@B
#�B
"�B
!�B
�B
B
�B
�B
fB	�B	�LB	��B	�5B	�B	��B	�B	��B	�&B	�B	�mB	�B	�B	�B	��B	�2B	��B	�B	�B	�sB	��B	��B	�0B	��B	�B	�+B	�JB	��B	��B	��B
�B
[B
�B
YB
�B
�B
 B
 �B
{B
�B
�B
�B
tB
B
�B
B
�B
YB
�B
�B
9B
�B
�B
B
tB
%B
%B
�B
�B
B
B
�B
�B
gB
MB
gB
�B
�B
�B
�B
�B
+B
�B
�B
1B
�B
�B
�B
B
�B
�B
B
 iB	�}B	�<B	��B	�>B	�XB	��B	�rB	�B	��B	�$B	�LB	��B	��B	�nB	�B	�vB	�vB	��B	�aB	��B	�B	��B	��B	��B	�B	�B	��B	��B	�aB	��B	�B	�B	�B	�TB	�B	��B	��B	��B	��B	��B	��B	�*B	��B	�XB	��B	�xB	��B	��B	�VB	�B	�BB	�<B	�B	�B	��B	��B	�dB	�B	�B	��B
GB
�B
 B	�wB	�}B
�B
�B
[B
�B
B
�B
B
+B
�B
	�B

#B
DB
�B
�B
�B
�B
�B
B
hB
�B
HB
B
.B
4B
hB
hB
TB
TB
:B
�B
�B
�B
�B
�B
�B
&B
�B
�B
B
[B
[B
�B
�B
�B
FB
�B
:B
:B
 B
bB
�B
�B
�B
bB
�B
HB
�B
bB
 B
}B
�B
�B
B
�B
�B
�B
�B
�B
�B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
?B
YB
B
MB
FB
�B
sB
�B
�B
�B
EB
�B
eB
�B
�B
eB
1B
�B
�B
+B
yB
KB
eB
�B
�B
7B
B
1B
�B
�B
B
�B
�B
KB
�B
1B
B
KB
B
�B
B
B
�B
kB
�B
=B
B
/B
~B
�B
pB
jB
�B
�B
�B
�B
�B
 vB
!|B
!-B
"NB
"hB
"B
"4B
"B
"�B
"hB
$&B
#�B
#�B
"�B
!�B
!HB
!|B
!�B
!�B
!�B
#nB
# B
#�B
#�B
#�B
$&B
$�B
#�B
#�B
%�B
%FB
'B
'�B
(sB
(>B
(�B
)DB
(�B
(�B
($B
(
B
'�B
'�B
(�B
(�B
)B
)�B
)�B
)yB
)�B
*0B
)�B
)�B
*B
*�B
*�B
)�B
+QB
+�B
,WB
,�B
,�B
,qB
,�B
,�B
-�B
-�B
-�B
.B
-�B
.IB
./B
/OB
/5B
/B
/5B
/ B
/OB
0UB
0�B
1vB
1�B
2B
2B
1�B
2B
2B
2B
2B
1vB
2B
2�B
2�B
2�B
3�B
33B
3MB
2�B
4B
4nB
49B
4�B
4�B
6B
6+B
6�B
6zB
6�B
6�B
6�B
6�B
7fB
7�B
7�B
88B
8�B
88B
8B
8�B
7�B
8B
8�B
9rB
:*B
9rB
9XB
8�B
9�B
8�B
9XB
9XB
9>B
9�B
9�B
9�B
:^B
;JB
;�B
;�B
;�B
<6B
=B
=<B
>B
>�B
>�B
>�B
?HB
?B
>wB
?.B
?}B
@ B
?�B
@4B
@B
@�B
@OB
A;B
@�B
A B
AB
@�B
AUB
A;B
A;B
A�B
A�B
A�B
B�B
CB
C-B
B�B
C-B
B�B
B�B
CB
C�B
C�B
C�B
D�B
DgB
D�B
D�B
D�B
EB
D�B
E9B
E�B
FB
F?B
E�B
F�B
F�B
GB
G+B
GB
GEB
G_B
GEB
G�B
G�B
G�B
H1B
HKB
H�B
I7B
I7B
I7B
I�B
I�B
JrB
J�B
KB
J�B
K�B
K�B
LJB
L�B
L~B
MB
M6B
MB
M�B
MjB
M6B
N"B
M�B
M�B
M�B
NVB
N�B
N�B
O\B
O�B
O�B
O�B
PHB
PHB
P�B
P.B
P}B
P�B
P�B
Q B
P�B
Q B
Q4B
Q�B
R�B
R�B
S[B
S�B
TFB
T,B
T�B
T,B
UB
UB
U2B
UgB
U�B
U�B
V9B
VSB
VmB
V�B
W?B
V�B
WYB
W?B
X+B
W�B
W�B
XyB
X+B
XEB
X�B
X�B
YB
Y�B
YKB
Y�B
Z7B
Z�B
ZB
Z�B
[	B
Z�B
Z�B
[qB
[�B
[=B
\]B
]B
\xB
\�B
]~B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]~B
]�B
^B
]�B
^jB
^5B
^OB
]�B
]�B
^�B
^jB
_B
_!B
_;B
_�B
_!B
`�B
`\B
aB
aHB
a�B
a�B
bNB
a�B
b�B
a�B
b�B
cB
c�B
c�B
c�B
c:B
cnB
dtB
d�B
dtB
e`B
ezB
eFB
e�B
f�B
f�B
gRB
gB
g�B
g�B
g�B
h�B
h>B
iyB
i�B
i�B
jeB
jB
i�B
jeB
jeB
i�B
j�B
j�B
k6B
kB
j�B
j�B
kkB
j�B
kB
j�B
k�B
k�B
k�B
k�B
l=B
l�B
lWB
l�B
mB
l�B
m)B
n�B
oOB
o�B
o�B
o�B
oiB
o�B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
p�B
p�B
p�B
p�B
p�B
p�B
qB
p�B
p�B
p�B
q�B
q[B
p�B
p�B
qB
qAB
qAB
q�B
rB
r|B
raB
rB
r-B
raB
r|B
raB
rGB
r-B
raB
r�B
shB
shB
sMB
shB
s�B
s�B
s�B
sMB
s�B
shB
s�B
tTB
tTB
tTB
tTB
t�B
t�B
t�B
t�B
t�B
uB
u%B
u�B
v111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Ba�Ba�B`�Ba-BbB`�B`BB`vB_�B_VB_pB^�B_!Bf2B`�BX_BXyBW�BWsBVmBVBVSBW�B[qB_�Bc�BfBh�Bm�Bo Bt9Bw�B�4B�lB�"B�VB��B��B��B�0B��B��B�XB�PB�vB�OB	A B
�B
CB
�B
(�B
%FB
S�B
�TB
��B
��B
�B
�?B
�aB
��B
�RB
��B
��B
�6B
raB
T,B
:�B
)�B
�B
�B	�B	�@B	ĜB	��B	�CB	��B	��B	�B	j�B	a�B	VB	O�B	IB	BB	>BB	;JB	5�B	0�B	.IB	,�B	*�B	+B	(�B	B	#B	�B	�B��B�AB��B��B�cB	�B		�B	�B	�B	�B	A;B	bNB	q�B	s�B	t�B	t�B	z�B	|6B	B	� B	�#B	��B	�zB	�B	�]B	��B	�B	��B	�wB
�B
�B
+6B
-�B
=qB
C-B
C�B
ESB
I�B
J	B
J#B
JXB
L�B
N<B
O�B
O�B
O�B
OBB
O\B
Q B
Q�B
Q�B
Q�B
SB
Q�B
Q�B
Q B
PB
OBB
O�B
OBB
N�B
NVB
M�B
L�B
L0B
K^B
LB
MjB
L�B
M�B
M6B
NB
O�B
K�B
F�B
F�B
F�B
E�B
D�B
D�B
DB
C�B
CaB
BAB
@�B
?B
?�B
@�B
A�B
A B
@�B
?�B
=�B
=VB
A�B
IlB
J	B
O\B
Q�B
RoB
RB
QhB
O�B
M�B
K^B
H�B
F%B
EB
C-B
C�B
DgB
EB
D�B
D�B
C�B
B�B
A B
@B
?}B
>�B
=�B
:�B
6�B
2B
0B
1AB
0!B
/�B
0�B
0B
-�B
-CB
,�B
+�B
+�B
)�B
(�B
'8B
&�B
&LB
%�B
%�B
%,B
$�B
#�B
#�B
#B
%zB
&B
%`B
'mB
(>B
(�B
&�B
%�B
%B
#�B
#�B
$B
$�B
$�B
$@B
#�B
"�B
!�B
�B
B
�B
�B
fB	�B	�LB	��B	�5B	�B	��B	�B	��B	�&B	�B	�mB	�B	�B	�B	��B	�2B	��B	�B	�B	�sB	��B	��B	�0B	��B	�B	�+B	�JB	��B	��B	��B
�B
[B
�B
YB
�B
�B
 B
 �B
{B
�B
�B
�B
tB
B
�B
B
�B
YB
�B
�B
9B
�B
�B
B
tB
%B
%B
�B
�B
B
B
�B
�B
gB
MB
gB
�B
�B
�B
�B
�B
+B
�B
�B
1B
�B
�B
�B
B
�B
�B
B
 iB	�}B	�<B	��B	�>B	�XB	��B	�rB	�B	��B	�$B	�LB	��B	��B	�nB	�B	�vB	�vB	��B	�aB	��B	�B	��B	��B	��B	�B	�B	��B	��B	�aB	��B	�B	�B	�B	�TB	�B	��B	��B	��B	��B	��B	��B	�*B	��B	�XB	��B	�xB	��B	��B	�VB	�B	�BB	�<B	�B	�B	��B	��B	�dB	�B	�B	��B
GB
�B
 B	�wB	�}B
�B
�B
[B
�B
B
�B
B
+B
�B
	�B

#B
DB
�B
�B
�B
�B
�B
B
hB
�B
HB
B
.B
4B
hB
hB
TB
TB
:B
�B
�B
�B
�B
�B
�B
&B
�B
�B
B
[B
[B
�B
�B
�B
FB
�B
:B
:B
 B
bB
�B
�B
�B
bB
�B
HB
�B
bB
 B
}B
�B
�B
B
�B
�B
�B
�B
�B
�B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
?B
YB
B
MB
FB
�B
sB
�B
�B
�B
EB
�B
eB
�B
�B
eB
1B
�B
�B
+B
yB
KB
eB
�B
�B
7B
B
1B
�B
�B
B
�B
�B
KB
�B
1B
B
KB
B
�B
B
B
�B
kB
�B
=B
B
/B
~B
�B
pB
jB
�B
�B
�B
�B
�B
 vB
!|B
!-B
"NB
"hB
"B
"4B
"B
"�B
"hB
$&B
#�B
#�B
"�B
!�B
!HB
!|B
!�B
!�B
!�B
#nB
# B
#�B
#�B
#�B
$&B
$�B
#�B
#�B
%�B
%FB
'B
'�B
(sB
(>B
(�B
)DB
(�B
(�B
($B
(
B
'�B
'�B
(�B
(�B
)B
)�B
)�B
)yB
)�B
*0B
)�B
)�B
*B
*�B
*�B
)�B
+QB
+�B
,WB
,�B
,�B
,qB
,�B
,�B
-�B
-�B
-�B
.B
-�B
.IB
./B
/OB
/5B
/B
/5B
/ B
/OB
0UB
0�B
1vB
1�B
2B
2B
1�B
2B
2B
2B
2B
1vB
2B
2�B
2�B
2�B
3�B
33B
3MB
2�B
4B
4nB
49B
4�B
4�B
6B
6+B
6�B
6zB
6�B
6�B
6�B
6�B
7fB
7�B
7�B
88B
8�B
88B
8B
8�B
7�B
8B
8�B
9rB
:*B
9rB
9XB
8�B
9�B
8�B
9XB
9XB
9>B
9�B
9�B
9�B
:^B
;JB
;�B
;�B
;�B
<6B
=B
=<B
>B
>�B
>�B
>�B
?HB
?B
>wB
?.B
?}B
@ B
?�B
@4B
@B
@�B
@OB
A;B
@�B
A B
AB
@�B
AUB
A;B
A;B
A�B
A�B
A�B
B�B
CB
C-B
B�B
C-B
B�B
B�B
CB
C�B
C�B
C�B
D�B
DgB
D�B
D�B
D�B
EB
D�B
E9B
E�B
FB
F?B
E�B
F�B
F�B
GB
G+B
GB
GEB
G_B
GEB
G�B
G�B
G�B
H1B
HKB
H�B
I7B
I7B
I7B
I�B
I�B
JrB
J�B
KB
J�B
K�B
K�B
LJB
L�B
L~B
MB
M6B
MB
M�B
MjB
M6B
N"B
M�B
M�B
M�B
NVB
N�B
N�B
O\B
O�B
O�B
O�B
PHB
PHB
P�B
P.B
P}B
P�B
P�B
Q B
P�B
Q B
Q4B
Q�B
R�B
R�B
S[B
S�B
TFB
T,B
T�B
T,B
UB
UB
U2B
UgB
U�B
U�B
V9B
VSB
VmB
V�B
W?B
V�B
WYB
W?B
X+B
W�B
W�B
XyB
X+B
XEB
X�B
X�B
YB
Y�B
YKB
Y�B
Z7B
Z�B
ZB
Z�B
[	B
Z�B
Z�B
[qB
[�B
[=B
\]B
]B
\xB
\�B
]~B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
]~B
]�B
^B
]�B
^jB
^5B
^OB
]�B
]�B
^�B
^jB
_B
_!B
_;B
_�B
_!B
`�B
`\B
aB
aHB
a�B
a�B
bNB
a�B
b�B
a�B
b�B
cB
c�B
c�B
c�B
c:B
cnB
dtB
d�B
dtB
e`B
ezB
eFB
e�B
f�B
f�B
gRB
gB
g�B
g�B
g�B
h�B
h>B
iyB
i�B
i�B
jeB
jB
i�B
jeB
jeB
i�B
j�B
j�B
k6B
kB
j�B
j�B
kkB
j�B
kB
j�B
k�B
k�B
k�B
k�B
l=B
l�B
lWB
l�B
mB
l�B
m)B
n�B
oOB
o�B
o�B
o�B
oiB
o�B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
p�B
p�B
p�B
p�B
p�B
p�B
qB
p�B
p�B
p�B
q�B
q[B
p�B
p�B
qB
qAB
qAB
q�B
rB
r|B
raB
rB
r-B
raB
r|B
raB
rGB
r-B
raB
r�B
shB
shB
sMB
shB
s�B
s�B
s�B
sMB
s�B
shB
s�B
tTB
tTB
tTB
tTB
t�B
t�B
t�B
t�B
t�B
uB
u%B
u�B
v111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104914  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173608  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173608  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173608                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023616  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023616  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                