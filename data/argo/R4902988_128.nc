CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-04-12T12:42:35Z creation;2023-04-12T12:42:36Z conversion to V3.1      
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230412124235  20230412125732  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�#l�l1   @�#�y\�@;�$�/��c�n��1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   AffA@  A`  A�  A���A���A�  A�  A�33A�33A�  B ffB  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B�  B�  B���B�  B�  B�  C   C  C�C  C  C
�C�C�fC  C�fC  C  C  C  C  C�C   C"  C#�fC%�fC(  C*  C,  C.  C/�fC2  C4  C5�fC8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C��C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C��C��3C�  C�  C��3C�  C��C�  C�  C��C�  C�  C��C��C�  C�  C��C�  C�  C��3C�  C�  C�  C��C��C��C��C��C��C��C��C�  C�  C�  C��C��C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C��3D � D  D� D  D�fD  D� D  D� D  D�fD  D� D��Dy�D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  Dy�D��D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D%��D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]fD]�fD^  D^�fD_fD_� D`  D`� Da  Day�Db  Db�fDc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�3D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D���D�<�D�� D�� D�3D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ D�|�D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ Dʼ�D�  D�@ Dˀ D�� D�  D�@ D�|�D̼�D�  D�<�D̀ D��3D�  D�@ D΃3D�� D�  D�@ Dπ D�� D�  D�<�DЀ D�� D�  D�@ D�|�D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D�  D�@ DԀ D��3D�  D�@ DՀ D�� D�  D�@ Dր D��3D�  D�@ D׀ D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D�|�D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@��@��A=pA?�
A_�
A�
A��RA��RA��A��A��A��A��B \)B��B��B��B��B'��B/��B7��B?�]BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B�ǮB���B���B���B���B���B���B���B���B���B�.B���B���B���B���B���B���B�.B�.B���B���B�ǮB���B���B�ǮB���B���B���B���C�qCC�qC�qC
CC��C�qC��C�qC�qC�qC�qC�qCC�qC!�qC#��C%��C'�qC)�qC+�qC-�qC/��C1�qC3�qC5��C7�qC9�qC<C=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg��Ci�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC��C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C��C���C���C��C���C���C��C��C���C���C��C���C���C���C���C���C���C��C��C��C��C��C��C��C��C���C���C���C��C��C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C��C���C���C��C���C���C��C���C���C���C���C��C���C���C���C���C���D \D �\D\D�\D��D�\D\D�\D\D�\D��D�\D\D��Dx�D�\D\D�\D	\D	�\D
��D
�\D\D�\D\D�\D\D�\D\D�\D��D�\D\D�\D\D�\D\D�\D\D�\D\D�\Dx�D�\D\D�\D\D�\D\D�\D\D�\D\D�D��D�\D\D�\Dx�D��D\D�\Dx�D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$��D$�\D%\D%��D&\D&�\D'x�D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+��D,\D-�D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<��D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\��D]�D]��D]�\D^��D_�D_\D_�\D`\D`�\Dax�Da�\Db��Db�\Dcx�Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dh�Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw��Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D��D���D��D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�<{D��D���D��D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�B�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D��{D�<{D��D���D��D�?�D��D���D���D�<{D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D�|{Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��D���D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʼ{D���D�?�D��D˿�D���D�?�D�|{D̼{D���D�<{D��D���D���D�?�D΂�Dο�D���D�?�D��DϿ�D���D�<{D��Dп�D���D�?�D�|{Dѿ�D���D�?�D��Dҿ�D��{D�?�D��Dӿ�D���D�?�D��D���D���D�?�D��Dտ�D���D�?�D��D���D���D�?�D��D׿�D��D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܼ{D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D��D�B�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D���D���D�?�D��D꿮D���D�?�D��D���D���D�?�D�|{D�{D���D�?�D��D���D���D�?�D��DD���D�?�D�|{D￮D���D�?�D��D�{D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�<{D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�<{D�|{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��pA��BA���A��A��A���A�� A���A���A��"A���A��]A��/A���A��mA��)A���A��A��iA�� A��vA��A���A�>�A�e`A�:�A�K^A�7�A��eA�2�A�-wA�;0A���A���A��A�=<A�:*A���A�XA���A�&LA���A�_�A��5A�v�A�ÖA���A��6A�1�A��A��A��7A��A��`A��A�уA��A��.A���A���A��4A�K�A��eA���A��A��A��A�o�A�I�A��fA�y>A�NA��9A�oA��A�}"A�|�A�� A�+kA�� A��aA�{A��A�qA��}A�O�A���A�9�A�h>A�k�A�{�A��%A���A�qA�]�A��TA�;�A��zA�MjA��A��DA�'�A~�A{YKAy��Aw�As��Aqy>Ao�3AoU�Ao�An��An@OAn�Am�qAl�MAh%FAd��A`֡A^�	A]��AZ�5AY�qAYtTAYoiAYTaAV�bAUuAR�AQW�APɆAPD�AOc�AN$tAM]dAM@AL�-ALO�AK�'AJ��AIr�AH�^AGb�AF�oAD��AD��AC��AB�}AB?}AAn�A@��A?�!A>g�A=W�A=7LA=6�A=�A<l�A:!A7�A7{A7
=A6��A6��A6=qA5�A5��A4�KA49�A3��A2��A2ffA2#:A1�hA1�MA1FtA0�A0�^A0��A0kQA09XA/�A/N<A-ȴA,A�A+��A+Z�A+
�A)��A&�A%��A%0UA$HA$�A#��A#OvA#A""�A!��A!J#A 2aA�A�AU2AMA5?A�A	�A�fA�OA�AoiA33A~(AX�AG�A�jA[�A$�A�rA��A�A�WA�VA��A�A��A
��A	�A֡A��Av�A-wA�A�jA��A�YA��A�vA�kA	�Ai�A�sA	l@��.@�q�@���@���@��-@�_@�)�@���@�Z@�@�x@�g�@�7�@�W�@���@��U@���@��H@��@�n@�C�@�u�@��X@�@���@�|@�N�@��@�tT@��@��A@�J�@�4@�ں@܎�@�!@�&�@ژ_@���@٣n@�X@�iD@��@�'R@�B�@���@ђ:@н<@�C-@��o@ϫ�@�f�@��'@̤�@���@�4n@��>@ɉ7@�p;@�|�@ƴ9@ũ�@�~(@Ê	@�@O@���@�@��W@��*@��"@��]@�u%@��@��K@�w2@���@�:�@�?}@�u@��Y@�g�@��1@��+@�o�@��@� �@���@�S&@��9@��@�y�@�e,@�X�@��@�j@�<6@��@��@��[@��@�(�@���@��x@�`�@��@���@��@�YK@�'�@�e�@�.�@�6�@�O@�  @��@��@�\�@�;d@��v@���@�xl@��@��@�ݘ@�~�@��@�R�@��@���@��@�}�@�c�@��@���@���@�y>@�@�@�u@��=@�!�@��j@�$@��N@���@�Y@���@���@��@��e@�|�@�d�@�7�@��@��@��3@��-@���@��f@�]�@�4@��@�:�@��#@�ݘ@���@���@��v@��@�ѷ@���@�Z@�4@��#@���@���@���@���@��P@�F@��@���@�"�@�)_@�*0@�*0@��@��p@�YK@�G@��@��u@��@��@���@�͟@��@�Dg@�;d@�l"@���@���@�G@��=@�2a@���@���@�A�@�~(@�@�|�@�g�@�s�@��@���@��a@�خ@��7@�@���@���@��R@��@�YK@�	@��
@���@�y�@�0�@�o@�	l@�;@��@�M@��@�2�@�j@�@��@�	@��@��@�m@iD@~�m@~� @~GE@~_@}��@}8�@}8�@}!�@|�4@|A�@{��@{�w@{x@{'�@{ i@z��@yԕ@x��@x~@w��@w��@w��@wF�@w�@vȴ@v�1@v$�@u�T@u��@u2a@t��@t��@toi@t6@t  @sU�@r�,@r��@r�h@r}V@q��@q��@qo @qA @p�@o1�@nn�@n	@nu@m�@m��@m��@m�'@l�@l�@lA�@l-�@k��@k\)@j��@j�!@jff@j)�@i�@iO�@i#�@h�@h��@h�Y@hXy@h*�@g��@g!-@e��@e��@dی@d�@c��@b��@b�s@b��@b�X@b��@bl�@a�H@`�	@`bN@`  @_�@_�g@_��@_��@_�$@_�P@_\)@_S@^��@]�t@\�	@\'R@[�@[iD@[o@Z�}@Zxl@Y�@Ys�@YF@Xw�@X*�@Xx@X_@XS�@W˒@W�q@WiD@W�g@WH�@V�R@V1�@U�M@T��@T�@S��@S�@S�V@Sl�@R�8@R�L@RYK@R-@Q�@Qm]@QDg@P��@P��@P~@O�A@O��@O'�@N�c@N�]@N��@N�@N�@N��@Ne@M��@M&�@L�@L*�@L �@L'R@LC-@LM@LV�@L2�@K��@L�@K�r@K�@@J��@JkQ@JJ�@J1�@J�@I��@I�@H[�@H-�@H,=@G��@Gl�@G�@Fں@F�s@F�\@FL0@E��@Em]@E5�@D�K@D�@De�@C�@C�P@C;d@B�L@BZ�@A��@AX@@�@@7�@?خ@?�:@?S@>ں@>�A@>?@=�@=s�@=`B@=4@<�|@<�j@<�@<q@;��@;�0@;�:@;g�@;@:�@:�6@:)�@9�o@9��@9�@9�3@9�-@9�=@9c@9x�@9k�@9^�@9[W@9X@9N<@9G�@95�@9(�@9�@9�@8�	@8��@7&@6�R@6�b@6{�@66�@5��@4ی@4��@4��@3�]@3�*@3s@39�@2��@2��@2�@2�]@2n�@2.�@2u@1�@1�3@1��@1e,@1&�@0��@0�Y@/x@.�@.��@.��@.i�@.Q@-�@-�@-�C@-Vm@,ѷ@,�u@,��@,�o@,h�@,_@,PH@,?�@+��@+�	@*�y@*��@*H�@*�@)�D@)��@)��@)�D@)�@)��@)��@)�@)�h@)zx@)?}@(��@(�@(4n@'��@'ƨ@'��@'x@'/�@&�@&ȴ@&��@&�h@&��@&�6@&�L@&��@&l�@&Q@&+k@%�o@%p�@%IR@%+�@$��@$�@$_@$9X@$M@$�@#�g@#�$@#S�@#>�@#�@"�@"��@"��@"�!@"� @"v�@"\�@"@�@!��@!��@!c�@!B�@!�@ bN@�g@��@Z�@,�@҉@�x@s�@C�@�>@��@|@L�@�@��@Ɇ@�$@��@��@�@��@oi@bN@?�@��@|�@b�@F�@33@�@�@xl@@�@�@��@�'@��@O�@	l@��@��@�@V�@9X@�F@j�@S�@S�@Mj@>�@8@4�@)_@҉@&�@��@�X@��@Y�@�E@��@~(@U2@~@�W@��@��@iD@>�@�M@�B@��@�r@n�@Q@)�@�@�#@�=@p�@Dg@#�@��@�@��@D�@�@��@��@��@\)@�@��@�R@��@3�@@#�@V@�@�@h�@D�@9X@6@9X@<�@6@2�@-�@%�@ �@1@�@خ@��@>�@.I@@
ȴ@
�R@
��@
��@
�1@
�r@
c @
)�@	�j@	�@	�X@	m]@	@֡@�j@��@��@w�@Q�@M@�@ݘ@��@�q@y�@S�@C@�8@��@p;@=q@��@�@�@��@�'@�h@��@p�@p�@O�@+@�5@�@�E@Ɇ@��@�O@�O@�e@�o@w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��pA��BA���A��A��A���A�� A���A���A��"A���A��]A��/A���A��mA��)A���A��A��iA�� A��vA��A���A�>�A�e`A�:�A�K^A�7�A��eA�2�A�-wA�;0A���A���A��A�=<A�:*A���A�XA���A�&LA���A�_�A��5A�v�A�ÖA���A��6A�1�A��A��A��7A��A��`A��A�уA��A��.A���A���A��4A�K�A��eA���A��A��A��A�o�A�I�A��fA�y>A�NA��9A�oA��A�}"A�|�A�� A�+kA�� A��aA�{A��A�qA��}A�O�A���A�9�A�h>A�k�A�{�A��%A���A�qA�]�A��TA�;�A��zA�MjA��A��DA�'�A~�A{YKAy��Aw�As��Aqy>Ao�3AoU�Ao�An��An@OAn�Am�qAl�MAh%FAd��A`֡A^�	A]��AZ�5AY�qAYtTAYoiAYTaAV�bAUuAR�AQW�APɆAPD�AOc�AN$tAM]dAM@AL�-ALO�AK�'AJ��AIr�AH�^AGb�AF�oAD��AD��AC��AB�}AB?}AAn�A@��A?�!A>g�A=W�A=7LA=6�A=�A<l�A:!A7�A7{A7
=A6��A6��A6=qA5�A5��A4�KA49�A3��A2��A2ffA2#:A1�hA1�MA1FtA0�A0�^A0��A0kQA09XA/�A/N<A-ȴA,A�A+��A+Z�A+
�A)��A&�A%��A%0UA$HA$�A#��A#OvA#A""�A!��A!J#A 2aA�A�AU2AMA5?A�A	�A�fA�OA�AoiA33A~(AX�AG�A�jA[�A$�A�rA��A�A�WA�VA��A�A��A
��A	�A֡A��Av�A-wA�A�jA��A�YA��A�vA�kA	�Ai�A�sA	l@��.@�q�@���@���@��-@�_@�)�@���@�Z@�@�x@�g�@�7�@�W�@���@��U@���@��H@��@�n@�C�@�u�@��X@�@���@�|@�N�@��@�tT@��@��A@�J�@�4@�ں@܎�@�!@�&�@ژ_@���@٣n@�X@�iD@��@�'R@�B�@���@ђ:@н<@�C-@��o@ϫ�@�f�@��'@̤�@���@�4n@��>@ɉ7@�p;@�|�@ƴ9@ũ�@�~(@Ê	@�@O@���@�@��W@��*@��"@��]@�u%@��@��K@�w2@���@�:�@�?}@�u@��Y@�g�@��1@��+@�o�@��@� �@���@�S&@��9@��@�y�@�e,@�X�@��@�j@�<6@��@��@��[@��@�(�@���@��x@�`�@��@���@��@�YK@�'�@�e�@�.�@�6�@�O@�  @��@��@�\�@�;d@��v@���@�xl@��@��@�ݘ@�~�@��@�R�@��@���@��@�}�@�c�@��@���@���@�y>@�@�@�u@��=@�!�@��j@�$@��N@���@�Y@���@���@��@��e@�|�@�d�@�7�@��@��@��3@��-@���@��f@�]�@�4@��@�:�@��#@�ݘ@���@���@��v@��@�ѷ@���@�Z@�4@��#@���@���@���@���@��P@�F@��@���@�"�@�)_@�*0@�*0@��@��p@�YK@�G@��@��u@��@��@���@�͟@��@�Dg@�;d@�l"@���@���@�G@��=@�2a@���@���@�A�@�~(@�@�|�@�g�@�s�@��@���@��a@�خ@��7@�@���@���@��R@��@�YK@�	@��
@���@�y�@�0�@�o@�	l@�;@��@�M@��@�2�@�j@�@��@�	@��@��@�m@iD@~�m@~� @~GE@~_@}��@}8�@}8�@}!�@|�4@|A�@{��@{�w@{x@{'�@{ i@z��@yԕ@x��@x~@w��@w��@w��@wF�@w�@vȴ@v�1@v$�@u�T@u��@u2a@t��@t��@toi@t6@t  @sU�@r�,@r��@r�h@r}V@q��@q��@qo @qA @p�@o1�@nn�@n	@nu@m�@m��@m��@m�'@l�@l�@lA�@l-�@k��@k\)@j��@j�!@jff@j)�@i�@iO�@i#�@h�@h��@h�Y@hXy@h*�@g��@g!-@e��@e��@dی@d�@c��@b��@b�s@b��@b�X@b��@bl�@a�H@`�	@`bN@`  @_�@_�g@_��@_��@_�$@_�P@_\)@_S@^��@]�t@\�	@\'R@[�@[iD@[o@Z�}@Zxl@Y�@Ys�@YF@Xw�@X*�@Xx@X_@XS�@W˒@W�q@WiD@W�g@WH�@V�R@V1�@U�M@T��@T�@S��@S�@S�V@Sl�@R�8@R�L@RYK@R-@Q�@Qm]@QDg@P��@P��@P~@O�A@O��@O'�@N�c@N�]@N��@N�@N�@N��@Ne@M��@M&�@L�@L*�@L �@L'R@LC-@LM@LV�@L2�@K��@L�@K�r@K�@@J��@JkQ@JJ�@J1�@J�@I��@I�@H[�@H-�@H,=@G��@Gl�@G�@Fں@F�s@F�\@FL0@E��@Em]@E5�@D�K@D�@De�@C�@C�P@C;d@B�L@BZ�@A��@AX@@�@@7�@?خ@?�:@?S@>ں@>�A@>?@=�@=s�@=`B@=4@<�|@<�j@<�@<q@;��@;�0@;�:@;g�@;@:�@:�6@:)�@9�o@9��@9�@9�3@9�-@9�=@9c@9x�@9k�@9^�@9[W@9X@9N<@9G�@95�@9(�@9�@9�@8�	@8��@7&@6�R@6�b@6{�@66�@5��@4ی@4��@4��@3�]@3�*@3s@39�@2��@2��@2�@2�]@2n�@2.�@2u@1�@1�3@1��@1e,@1&�@0��@0�Y@/x@.�@.��@.��@.i�@.Q@-�@-�@-�C@-Vm@,ѷ@,�u@,��@,�o@,h�@,_@,PH@,?�@+��@+�	@*�y@*��@*H�@*�@)�D@)��@)��@)�D@)�@)��@)��@)�@)�h@)zx@)?}@(��@(�@(4n@'��@'ƨ@'��@'x@'/�@&�@&ȴ@&��@&�h@&��@&�6@&�L@&��@&l�@&Q@&+k@%�o@%p�@%IR@%+�@$��@$�@$_@$9X@$M@$�@#�g@#�$@#S�@#>�@#�@"�@"��@"��@"�!@"� @"v�@"\�@"@�@!��@!��@!c�@!B�@!�@ bN@�g@��@Z�@,�@҉@�x@s�@C�@�>@��@|@L�@�@��@Ɇ@�$@��@��@�@��@oi@bN@?�@��@|�@b�@F�@33@�@�@xl@@�@�@��@�'@��@O�@	l@��@��@�@V�@9X@�F@j�@S�@S�@Mj@>�@8@4�@)_@҉@&�@��@�X@��@Y�@�E@��@~(@U2@~@�W@��@��@iD@>�@�M@�B@��@�r@n�@Q@)�@�@�#@�=@p�@Dg@#�@��@�@��@D�@�@��@��@��@\)@�@��@�R@��@3�@@#�@V@�@�@h�@D�@9X@6@9X@<�@6@2�@-�@%�@ �@1@�@خ@��@>�@.I@@
ȴ@
�R@
��@
��@
�1@
�r@
c @
)�@	�j@	�@	�X@	m]@	@֡@�j@��@��@w�@Q�@M@�@ݘ@��@�q@y�@S�@C@�8@��@p;@=q@��@�@�@��@�'@�h@��@p�@p�@O�@+@�5@�@�E@Ɇ@��@�O@�O@�e@�o@w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  By�By�By�By�By�By�ByXBy�By�By�BzBy�By�By�By�BzBy�By�BzBy�Bx�Bu�B��B�B�nB�*B�MB��BÖB��By�B]�BkQBPHB�B:�Bs�Bu�Bo5B��B�SB�bB�TB�jB�UBh>BJXB)�B�BB��B��B�LB��B�VB�B�`B��B��B��B�+B �B5?BM�BOvBLdBI�BDMB@4B;dB,�B'�BeB�B�_B��B�IB��BԕB�|Bv�B]�B>BB�*B�qBуB��B�$B��B�B��B��Bo�B^5BM�BEmB9�B"�B�B B3B
��B
�B
��B
�?B
��B
�4B
��B
�EB
�B
��B
~BB
{�B
x�B
t�B
mwB
Y1B
?HB
*�B
�B
�B
�B
	B
�B
1B
�B
�B	��B	�nB	�KB	��B	�&B	߾B	ںB	�B	�[B	� B	�bB	�6B	ɆB	��B	�UB	��B	��B	��B	�B	��B	�wB	��B	��B	�LB	��B	�[B	��B	��B	��B	�XB	�fB	��B	��B	��B	�"B	��B	�6B	�#B	�fB	��B	�oB	~B	z�B	wLB	s�B	r�B	p�B	o�B	oB	m�B	lWB	k�B	kB	i�B	h�B	e�B	a-B	\B	XB	U�B	T�B	PHB	F�B	>�B	<�B	9	B	6�B	5�B	1�B	/OB	+�B	($B	&�B	&�B	!�B	�B	�B	�B	
�B	�B	UB�wB�dB�*B��B��B��B��B��B�B�B�3B��B��B��B�B�B��B�B�B�yB�B��B�TB�bB�bB��B��B��B�!B��BܬB��B��B�eB�QBٚB�?B��B�mBԕBԕB�B҉BҽB�&B�SB�{B��B՛BԕB��B�{B�B�BЗB�HBϑB�B��BуBбB҉BѷBуB�4B�.B�NBҽBԯB�[BӏBӏB��B�[B��B� B�hB�FB�{BԕB�B��B�mB֡BּB��BּB�B�BٴB�B�BٴB�B�#B�BބB��B��B�B�&B�tB��B�FB�FB�B�B�RB�>B�B�B� B�B�B��B��B�.B	 �B	AB	GB	�B	EB	�B	fB	
�B	6B	VB	<B	B	B	B	SB	B	�B	/B	!�B	#:B	$�B	%�B	&�B	*�B	/OB	/�B	1B	1�B	4�B	6B	5�B	6zB	6�B	7�B	7�B	9>B	8�B	9�B	9�B	:�B	B�B	E9B	E�B	F�B	HfB	K�B	MB	N�B	OvB	P.B	Q4B	T�B	VSB	VmB	W$B	X�B	Z7B	]B	`vB	c:B	f�B	h�B	i�B	m�B	s�B	t�B	x�B	{�B	}VB	~B	�B	��B	��B	�B	��B	��B	�B	�%B	�+B	��B	��B	�B	��B	��B	�2B	�eB	��B	��B	�qB	��B	��B	�4B	��B	��B	�LB	��B	�mB	��B	�)B	�+B	��B	�*B	�B	��B	� B	��B	�B	ǮB	�B	�tB	��B	��B	�JB	��B	ϫB	ԯB	�9B	�2B	ںB	�B	�)B	��B	�)B	��B	�B	�B	�DB	��B	�eB	�B	��B	�B	�[B	�|B	�FB	�DB	�qB	��B	�HB	�cB	�.B
;B
�B
�B
-B
B
9B
SB
9B
gB
B
SB
_B

�B
�B
�B
�B
QB
	B
#B
=B
5B
!�B
"�B
$@B
%�B
(�B
)�B
)�B
*eB
,�B
.B
/iB
0�B
2aB
3MB
3�B
5B
8B
<jB
>�B
?.B
@B
@�B
A�B
B�B
DB
D�B
F�B
HKB
I�B
KB
K�B
M�B
O�B
Q�B
R�B
SB
SuB
S�B
S�B
T{B
V�B
X�B
YB
ZB
^�B
aHB
c�B
eB
eFB
e�B
f2B
f�B
gB
jB
kQB
lWB
l�B
mB
nB
n�B
oB
o�B
q'B
rGB
r|B
r�B
sB
s�B
s�B
t�B
u�B
u�B
uZB
t�B
t�B
t�B
t�B
zB
|�B
}qB
}�B
}�B
}�B
~�B
}B
��B
�UB
�B
�AB
�AB
��B
��B
��B
��B
�GB
��B
��B
��B
�)B
��B
��B
��B
��B
�VB
��B
�}B
��B
��B
�,B
�gB
��B
�B
��B
�qB
��B
��B
�B
��B
�:B
�&B
��B
��B
��B
��B
�B
��B
��B
��B
�)B
��B
�B
��B
�iB
��B
�oB
��B
�vB
�AB
�B
�B
��B
��B
�?B
�ZB
�ZB
��B
�B
��B
�XB
��B
�B
�JB
��B
��B
��B
�PB
�jB
��B
�qB
��B
�wB
�}B
��B
��B
��B
��B
��B
��B
��B
��B
�'B
��B
�3B
�3B
�B
�B
�+B
�B
�fB
�lB
�XB
ʌB
�xB
̘B
��B
�B
ЗB
�hB
ѝB
�oB
��B
�B
�{B
�B
�2B
�B
�9B
֡B
�
B
��B
�+B
�+B
�_B
خB
��B
�B
�B
�QB
��B
�=B
ۦB
�xB
��B
�B
��B
�5B
�OB
�jB
ޞB
ޞB
�B
�;B
�VB
�pB
ߊB
ߊB
ߤB
��B
��B
�'B
�\B
�\B
�\B
��B
�B
�B
�TB
�nB
�B
�&B
��B
�fB
�fB
�B
�B
�>B
��B
��B
�DB
�B
�B
�B
��B
�0B
�B
��B
�B
�B
�B
�B
��B
�B
�}B
�5B
�B
��B
�B
�!B
��B
�B
�'B
��B
�B
��B
��B
�B
�MB
�3B
�MB
�MB
��B
��B
��B
�B
��B
�2B
�2B
�2B
�B
�2B
�fB
��B
��B
��B
��B
��B
�8B
��B
�rB
�*B
��B
��B
��B
�0B
��B
�B
�jB
��B
��B
��B
��B
��B
��B
�"B
�<B
�qB
��B
��B
�B
�.B
��B  B iB �B �BBUB�BBBuB�BB-B-BGBaB�B�BgB�BBBBYB�BB�B�B�B�B�B	B	�B	�B	�B
#B
�B
�B
�BDBDBDB^BxB�B�B�B~B�B�B�B�B�BjBVB�B�BBBvBvB�BB.BBbB�B�B�B�B:B B B:BTBTBTB�B�BFB�B�B�B�B�BBBmB�B�B
B?B�B�BEB�B�B�B�B1B1B�B�B7B�B�B�B�B�B�BxB�B�B�BIB�BB5BBB�B �B �B �B �B!�B"B"4B"4B"4B"4B"NB"4B"NB"NB"NB"�B"�B"�B#B#�B#�B#�B$B$B$&B$B$ZB$ZB$�B$�B%zB%�B%�B&LB&�B&�B'B'RB'8B'�B'�B(>B(sB(sB)B)DB)�B)�B)�B)�B*eB*eB*�B+6B+�B+QB+�B+�B+�B+�B+�B+�B,"B,qB,�B,�B,�B-)B,�B-]B-]B-wB-�B-�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  By�By�By�By�By�By�ByXBy�By�By�BzBy�By�By�By�BzBy�By�BzBy�Bx�Bu�B��B�B�nB�*B�MB��BÖB��By�B]�BkQBPHB�B:�Bs�Bu�Bo5B��B�SB�bB�TB�jB�UBh>BJXB)�B�BB��B��B�LB��B�VB�B�`B��B��B��B�+B �B5?BM�BOvBLdBI�BDMB@4B;dB,�B'�BeB�B�_B��B�IB��BԕB�|Bv�B]�B>BB�*B�qBуB��B�$B��B�B��B��Bo�B^5BM�BEmB9�B"�B�B B3B
��B
�B
��B
�?B
��B
�4B
��B
�EB
�B
��B
~BB
{�B
x�B
t�B
mwB
Y1B
?HB
*�B
�B
�B
�B
	B
�B
1B
�B
�B	��B	�nB	�KB	��B	�&B	߾B	ںB	�B	�[B	� B	�bB	�6B	ɆB	��B	�UB	��B	��B	��B	�B	��B	�wB	��B	��B	�LB	��B	�[B	��B	��B	��B	�XB	�fB	��B	��B	��B	�"B	��B	�6B	�#B	�fB	��B	�oB	~B	z�B	wLB	s�B	r�B	p�B	o�B	oB	m�B	lWB	k�B	kB	i�B	h�B	e�B	a-B	\B	XB	U�B	T�B	PHB	F�B	>�B	<�B	9	B	6�B	5�B	1�B	/OB	+�B	($B	&�B	&�B	!�B	�B	�B	�B	
�B	�B	UB�wB�dB�*B��B��B��B��B��B�B�B�3B��B��B��B�B�B��B�B�B�yB�B��B�TB�bB�bB��B��B��B�!B��BܬB��B��B�eB�QBٚB�?B��B�mBԕBԕB�B҉BҽB�&B�SB�{B��B՛BԕB��B�{B�B�BЗB�HBϑB�B��BуBбB҉BѷBуB�4B�.B�NBҽBԯB�[BӏBӏB��B�[B��B� B�hB�FB�{BԕB�B��B�mB֡BּB��BּB�B�BٴB�B�BٴB�B�#B�BބB��B��B�B�&B�tB��B�FB�FB�B�B�RB�>B�B�B� B�B�B��B��B�.B	 �B	AB	GB	�B	EB	�B	fB	
�B	6B	VB	<B	B	B	B	SB	B	�B	/B	!�B	#:B	$�B	%�B	&�B	*�B	/OB	/�B	1B	1�B	4�B	6B	5�B	6zB	6�B	7�B	7�B	9>B	8�B	9�B	9�B	:�B	B�B	E9B	E�B	F�B	HfB	K�B	MB	N�B	OvB	P.B	Q4B	T�B	VSB	VmB	W$B	X�B	Z7B	]B	`vB	c:B	f�B	h�B	i�B	m�B	s�B	t�B	x�B	{�B	}VB	~B	�B	��B	��B	�B	��B	��B	�B	�%B	�+B	��B	��B	�B	��B	��B	�2B	�eB	��B	��B	�qB	��B	��B	�4B	��B	��B	�LB	��B	�mB	��B	�)B	�+B	��B	�*B	�B	��B	� B	��B	�B	ǮB	�B	�tB	��B	��B	�JB	��B	ϫB	ԯB	�9B	�2B	ںB	�B	�)B	��B	�)B	��B	�B	�B	�DB	��B	�eB	�B	��B	�B	�[B	�|B	�FB	�DB	�qB	��B	�HB	�cB	�.B
;B
�B
�B
-B
B
9B
SB
9B
gB
B
SB
_B

�B
�B
�B
�B
QB
	B
#B
=B
5B
!�B
"�B
$@B
%�B
(�B
)�B
)�B
*eB
,�B
.B
/iB
0�B
2aB
3MB
3�B
5B
8B
<jB
>�B
?.B
@B
@�B
A�B
B�B
DB
D�B
F�B
HKB
I�B
KB
K�B
M�B
O�B
Q�B
R�B
SB
SuB
S�B
S�B
T{B
V�B
X�B
YB
ZB
^�B
aHB
c�B
eB
eFB
e�B
f2B
f�B
gB
jB
kQB
lWB
l�B
mB
nB
n�B
oB
o�B
q'B
rGB
r|B
r�B
sB
s�B
s�B
t�B
u�B
u�B
uZB
t�B
t�B
t�B
t�B
zB
|�B
}qB
}�B
}�B
}�B
~�B
}B
��B
�UB
�B
�AB
�AB
��B
��B
��B
��B
�GB
��B
��B
��B
�)B
��B
��B
��B
��B
�VB
��B
�}B
��B
��B
�,B
�gB
��B
�B
��B
�qB
��B
��B
�B
��B
�:B
�&B
��B
��B
��B
��B
�B
��B
��B
��B
�)B
��B
�B
��B
�iB
��B
�oB
��B
�vB
�AB
�B
�B
��B
��B
�?B
�ZB
�ZB
��B
�B
��B
�XB
��B
�B
�JB
��B
��B
��B
�PB
�jB
��B
�qB
��B
�wB
�}B
��B
��B
��B
��B
��B
��B
��B
��B
�'B
��B
�3B
�3B
�B
�B
�+B
�B
�fB
�lB
�XB
ʌB
�xB
̘B
��B
�B
ЗB
�hB
ѝB
�oB
��B
�B
�{B
�B
�2B
�B
�9B
֡B
�
B
��B
�+B
�+B
�_B
خB
��B
�B
�B
�QB
��B
�=B
ۦB
�xB
��B
�B
��B
�5B
�OB
�jB
ޞB
ޞB
�B
�;B
�VB
�pB
ߊB
ߊB
ߤB
��B
��B
�'B
�\B
�\B
�\B
��B
�B
�B
�TB
�nB
�B
�&B
��B
�fB
�fB
�B
�B
�>B
��B
��B
�DB
�B
�B
�B
��B
�0B
�B
��B
�B
�B
�B
�B
��B
�B
�}B
�5B
�B
��B
�B
�!B
��B
�B
�'B
��B
�B
��B
��B
�B
�MB
�3B
�MB
�MB
��B
��B
��B
�B
��B
�2B
�2B
�2B
�B
�2B
�fB
��B
��B
��B
��B
��B
�8B
��B
�rB
�*B
��B
��B
��B
�0B
��B
�B
�jB
��B
��B
��B
��B
��B
��B
�"B
�<B
�qB
��B
��B
�B
�.B
��B  B iB �B �BBUB�BBBuB�BB-B-BGBaB�B�BgB�BBBBYB�BB�B�B�B�B�B	B	�B	�B	�B
#B
�B
�B
�BDBDBDB^BxB�B�B�B~B�B�B�B�B�BjBVB�B�BBBvBvB�BB.BBbB�B�B�B�B:B B B:BTBTBTB�B�BFB�B�B�B�B�BBBmB�B�B
B?B�B�BEB�B�B�B�B1B1B�B�B7B�B�B�B�B�B�BxB�B�B�BIB�BB5BBB�B �B �B �B �B!�B"B"4B"4B"4B"4B"NB"4B"NB"NB"NB"�B"�B"�B#B#�B#�B#�B$B$B$&B$B$ZB$ZB$�B$�B%zB%�B%�B&LB&�B&�B'B'RB'8B'�B'�B(>B(sB(sB)B)DB)�B)�B)�B)�B*eB*eB*�B+6B+�B+QB+�B+�B+�B+�B+�B+�B,"B,qB,�B,�B,�B-)B,�B-]B-]B-wB-�B-�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230412124234  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230412124235  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230412124236  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230412124236                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230412124237  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230412124237  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230412125732                      G�O�G�O�G�O�                