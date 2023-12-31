CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-11-04T03:47:15Z creation;2022-11-04T03:47:16Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20221104034715  20221104040811  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               pA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��=����1   @��>c�8�@;`A�7L�c�j~��#1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A��A!��A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�33B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C�C  C�fC�fC  C�fC�fC�fC�fC�fC�fC"  C$�C&�C(  C)�fC,  C.  C/�fC2�C4  C5�fC8  C:  C<  C=�fC@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C��C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� DfD� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5y�D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDHfDH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY�fDZ  DZ� DZ��D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dx  Dx�fDyfDy� Dz  Dz� D{  D{�fD|  D|y�D}  D}� D~  D~� D  D�fD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D�� D���D���D�@ D�� D�� D�3D�C3D��3D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D¼�D���D�<�DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǃ3D��3D�3D�@ Dȃ3D��3D�  D�@ Dɀ D�� D�  D�<�Dʀ D�� D�  D�<�Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D�|�D�� D�  D�@ Dـ Dټ�D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�C3D܀ D�� D�  D�@ D݀ D�� D�  D�@ D�|�D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�3D�C3D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z@��Ap�A!p�A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B�.B���B���B���B���B���B�ǮB���B���B�ǮB���B�.B���B�ǮB���B���B���B���B�.B���B���B���B���B���B���B���B�ǮB���B���B���B���C�qC�qC�qC�qC	�qCC�qC��C��C�qC��C��C��C��C��C��C!�qC$C&C'�qC)��C+�qC-�qC/��C2C3�qC5��C7�qC9�qC;�qC=��C?�qCA�qCDCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc��Ce�qCg�qCi�qCk�qCm�qCpCq�qCs�qCu�qCw�qCy�qC{�qC}�qC��C���C���C���C��C���C���C���C���C���C���C���C���C���C���C��C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\Dx�D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D\D�\D\D�\D\D�D\D�\D\D�D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)��D*\D*�\D+\D,�D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3��D3�\D4\D4�\D5x�D5�\D6��D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG��DH�DH\DH�\DI\DI�\DJ\DJ�\DK��DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DX�DX\DX�\DY��DY�\DZ\DZ��D[x�D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dd�Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Dv�Dv\Dv�\Dw\Dw�\Dx��Dy�Dy\Dy�\Dz\Dz�\D{��D{�\D|x�D|�\D}\D}�\D~\D~�\D��D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�B�D��D��{D��{D�?�D��D���D��D�B�D���D���D��{D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{D�<{D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¼{D��{D�<{D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�Dǂ�D���D��D�?�DȂ�D���D���D�?�D��Dɿ�D���D�<{D��Dʿ�D���D�<{D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D�|{Dؿ�D���D�?�D��Dټ{D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�B�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D�|{D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�B�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�B�D��D迮D���D�?�D��D鿮D��D�B�D��D�{D��{D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�{D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D���D�?�D���D���D���D�?�D�b�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AЙ1AЖ�AЏ�AА�AВ:AГAГ�AГ@AЎ"AЉ7AЊrAЇ�AЇ�A�zDA�x8A�w�A�wfA�v`A�v�A�t�A�sMA�p�A�poA�qvA�qvA�rA�o�A�p;A�o�A�k�A�c A�[#A�RTA�C�A��A�S�A��AAņ�A���A��TA���A��xA��3A��4A�VA�9�A��!A�2-A���A��A��A���A�+6A���A��A���A�7A�iyA�<�A���A��hA�$�A���A��?A�W�A��nA���A�T�A���A��A��A��[A�;�A���A�*0A�H�A���A�Q�A�˒A��/A�m�A���A���A�U2A��dA�IA��A���A�`A�qA��A���A��A�*�A�_;A�~�A��A��JA�M�A�
	A�یA��vA��A�G�A��A�%zA�&�A��]A��A}��A{=�Az�Ax�/Au�|At�At6Ar�Aq�Aqe�AqOAp�zAp.�Ao�zAn�TAn?�Am��Al�VAk��AiJAg�>Af�WAd�/Ad�AcSA`��A_��A_�A]d�A\�OA\K^A[f�AX��AW,=AVC-AUںAUP�AS:�AQ��AP�rAP;�AO��AN-wAL1�AI��AHY�AG�YAFA AD�cADbAC~�AB��AB�"AB��AB�$AAE�A?��A>�A=C�A<H�A:MA9�A9�A9s�A7�A6|�A4��A3��A2�;A0�	A.�{A,�-A+��A*m�A)�PA)h
A(�A(sA(�A'y>A&��A%��A#�A"u%A!�FA �A MjA�A��AN<A��AJ�A�3A��Aa|A��A��A($Ad�A��A>BAL0A�A;A�jA��A�+A1'A��AqA`BA��AZA1�A҉A��Ac�A�A
��A
�A
��A
��A
�A	+kA�.A*�AYA�A�wA�A��A��A��A �3@���@���@�e@��@���@��8@���@�@���@�S@�/�@���@�<6@� i@��-@�\@��z@� i@��Z@�7L@��@���@��@��@�r�@���@��@��@㞄@�,�@�J�@�4@���@�O@ܘ_@�6@�  @�Y�@��A@��@�w�@�Dg@ԕ@�|@��@Ѕ�@�=�@�0�@̭�@�.�@ˆ�@�@@ʲ�@�6@ɱ[@�F�@���@�6�@ǩ�@�c@�7L@Ʃ�@ŋ�@��j@���@��	@��L@���@�� @���@�ȴ@��@�~�@��W@���@��@���@�bN@�R�@��@��@��A@�*0@�~�@���@�$t@���@���@�{@�Dg@�kQ@�J�@��)@��@�[�@�~@��a@�b�@��@���@�u%@�7�@��@��^@���@�o @�_p@�E9@�+@��@�ی@���@��L@��+@�\)@��@���@���@�h�@�w2@�0�@��@��h@���@�@�X�@���@��@��O@�9X@��w@���@���@�t�@�F�@�@��@��o@�V@�@��b@��@�A @�+@�ߤ@�w�@���@���@��R@�&�@��7@��-@��1@�(�@��@��@��7@��v@�l�@�#:@��@�Z�@��P@�h�@���@��@��@��@��}@�~(@�V@�1�@��@���@�2a@��`@��F@���@��_@��@���@��z@���@�W�@�B[@�!�@��Z@���@���@��"@�g�@�F�@�-w@�&@��@��K@���@���@�h
@�:�@��@�s@�N�@�ϫ@���@�{J@�G�@�֡@�y>@�>B@��@���@�c @�V�@�<�@�($@�@��@�@@��@��@t�@~�+@}�@|��@|��@{9�@y�@ye,@y:�@y2a@y@y�@x��@xی@x�@xh�@x7�@w��@w@O@v.�@u�@u�@u�3@u`B@u�@t�@t�p@tĜ@tu�@t/�@t�@s��@s�+@s�m@s�6@s�0@s�@rں@q�=@p��@o=@n��@nu%@nd�@m��@m�3@m��@mk�@l�@lw�@lS�@l6@k�W@k@O@j��@j}V@jC�@j)�@i��@i�@i�@i��@ic�@iF@i�@h�v@hĜ@h�@hQ�@h$@g�}@g�0@gy�@f�@fl�@e��@e��@eY�@d�9@d>B@dM@c�A@c�V@c|�@co@b�x@b~�@b�@ak�@aDg@a0�@`h�@_��@_]�@^�}@^c @^_�@^Ta@]��@]5�@]+@\��@\�@\�@[8@Z�2@Z�B@Z��@Z�@Z}V@Zh
@Z6�@Z �@Y��@Y�h@Yc�@Y�@WC�@V�c@V��@V�R@V�x@V��@V��@V}V@VkQ@VGE@U@Uk�@UL�@U2a@U+�@U�@T��@T�z@TI�@S�k@R��@R�!@RTa@Q�n@Q��@P��@O8@N�c@N�s@N�!@Nn�@NYK@NL0@N
�@M�)@M�3@M�n@Mo @L��@L�@J�@J��@J��@J��@Jc @JO@I�@I��@I��@I��@Is�@IB�@I@H��@HXy@H?�@HG@GU�@G�@F�@F�c@F�@F�@Fߤ@Fߤ@F�\@Fff@F\�@FYK@FTa@F1�@E��@E��@E=�@E%F@EV@D�v@D��@D�.@D|�@Dw�@Dr�@DZ@C��@C=@B�H@B�x@A�^@AVm@A/@@ѷ@@��@@?�@?��@?�q@?��@?t�@?,�@>�@>ں@>��@>��@>=q@>�@=�o@=��@=-w@<�/@<e�@<�@;�6@;��@;g�@;=@:��@9@9S&@9+@8�@8?�@7�@7A�@7!-@7"�@7$t@7(@6�,@6a|@6C�@6�@5�o@5�j@5�X@5zx@5Dg@5�@5�@5�@4��@4֡@4��@4b@36z@2�\@2�@2��@2Ta@2;�@20U@24@1�z@1IR@1%@0��@0�p@0��@0�@0�u@0�.@0��@0�Y@0K^@06@0'R@0�@0�@/��@/�0@/��@/s@/Y@/�@.��@.��@.��@.d�@.5?@.#:@.e@._@.�@-�Z@-��@-�T@-�9@-��@-�7@-0�@,��@,��@,e�@+�@+A�@*�M@*�x@*Ta@)��@)4@(Ɇ@(e�@'�&@'v`@'�@&�8@&�X@&u%@&M�@%�)@%�M@%c�@%IR@%A @%&�@$�P@$�@$I�@#��@#�K@#�[@#�$@#P�@"�X@"J�@":*@"O@!��@!X@!2a@ �@ ��@ V�@ -�@ �@�@�W@�&@�
@�q@�@�h@R�@�@�@k�@5�@q@��@��@�j@��@m�@K^@��@��@�]@�L@ff@@�@1�@�@�@
�@@L�@_@x@�@��@��@|�@|�@~�@��@x@j�@_p@X�@W?@\)@P�@$t@�@��@��@�H@�}@c @R�@Ov@;�@8�@�@O@#:@)�@.�@0U@e@��@�C@��@X@�@�@ѷ@��@h�@D�@?�@C-@9X@�@ݘ@ƨ@��@��@��@��@�	@{J@e�@RT@8@C@��@��@��@��@�@c@j@T�@ \@�p@e�@M@Ft@>B@4n@-�@~@7@�@!@7@�A@�q@�$@��@��@v`@X�@E9@�@xl@$�@_@�D@��@�7@�@��@��@�Y@l"@>B@M@خ@��@��@]�@@O@6z@!-@
��@
�r@
8�@	�T@	�@	q@�E@��@��@�@�j@�U@�@�?@��@�@�.@Z@x@�@�P@l�@]�@8@�A@=q@)�@�T@u�@L�@B�@:�@7L@%F@@��@�/@�.@'R@�W@��@��@�0@�0@�F@��@�f@�f@y�@e�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AЙ1AЖ�AЏ�AА�AВ:AГAГ�AГ@AЎ"AЉ7AЊrAЇ�AЇ�A�zDA�x8A�w�A�wfA�v`A�v�A�t�A�sMA�p�A�poA�qvA�qvA�rA�o�A�p;A�o�A�k�A�c A�[#A�RTA�C�A��A�S�A��AAņ�A���A��TA���A��xA��3A��4A�VA�9�A��!A�2-A���A��A��A���A�+6A���A��A���A�7A�iyA�<�A���A��hA�$�A���A��?A�W�A��nA���A�T�A���A��A��A��[A�;�A���A�*0A�H�A���A�Q�A�˒A��/A�m�A���A���A�U2A��dA�IA��A���A�`A�qA��A���A��A�*�A�_;A�~�A��A��JA�M�A�
	A�یA��vA��A�G�A��A�%zA�&�A��]A��A}��A{=�Az�Ax�/Au�|At�At6Ar�Aq�Aqe�AqOAp�zAp.�Ao�zAn�TAn?�Am��Al�VAk��AiJAg�>Af�WAd�/Ad�AcSA`��A_��A_�A]d�A\�OA\K^A[f�AX��AW,=AVC-AUںAUP�AS:�AQ��AP�rAP;�AO��AN-wAL1�AI��AHY�AG�YAFA AD�cADbAC~�AB��AB�"AB��AB�$AAE�A?��A>�A=C�A<H�A:MA9�A9�A9s�A7�A6|�A4��A3��A2�;A0�	A.�{A,�-A+��A*m�A)�PA)h
A(�A(sA(�A'y>A&��A%��A#�A"u%A!�FA �A MjA�A��AN<A��AJ�A�3A��Aa|A��A��A($Ad�A��A>BAL0A�A;A�jA��A�+A1'A��AqA`BA��AZA1�A҉A��Ac�A�A
��A
�A
��A
��A
�A	+kA�.A*�AYA�A�wA�A��A��A��A �3@���@���@�e@��@���@��8@���@�@���@�S@�/�@���@�<6@� i@��-@�\@��z@� i@��Z@�7L@��@���@��@��@�r�@���@��@��@㞄@�,�@�J�@�4@���@�O@ܘ_@�6@�  @�Y�@��A@��@�w�@�Dg@ԕ@�|@��@Ѕ�@�=�@�0�@̭�@�.�@ˆ�@�@@ʲ�@�6@ɱ[@�F�@���@�6�@ǩ�@�c@�7L@Ʃ�@ŋ�@��j@���@��	@��L@���@�� @���@�ȴ@��@�~�@��W@���@��@���@�bN@�R�@��@��@��A@�*0@�~�@���@�$t@���@���@�{@�Dg@�kQ@�J�@��)@��@�[�@�~@��a@�b�@��@���@�u%@�7�@��@��^@���@�o @�_p@�E9@�+@��@�ی@���@��L@��+@�\)@��@���@���@�h�@�w2@�0�@��@��h@���@�@�X�@���@��@��O@�9X@��w@���@���@�t�@�F�@�@��@��o@�V@�@��b@��@�A @�+@�ߤ@�w�@���@���@��R@�&�@��7@��-@��1@�(�@��@��@��7@��v@�l�@�#:@��@�Z�@��P@�h�@���@��@��@��@��}@�~(@�V@�1�@��@���@�2a@��`@��F@���@��_@��@���@��z@���@�W�@�B[@�!�@��Z@���@���@��"@�g�@�F�@�-w@�&@��@��K@���@���@�h
@�:�@��@�s@�N�@�ϫ@���@�{J@�G�@�֡@�y>@�>B@��@���@�c @�V�@�<�@�($@�@��@�@@��@��@t�@~�+@}�@|��@|��@{9�@y�@ye,@y:�@y2a@y@y�@x��@xی@x�@xh�@x7�@w��@w@O@v.�@u�@u�@u�3@u`B@u�@t�@t�p@tĜ@tu�@t/�@t�@s��@s�+@s�m@s�6@s�0@s�@rں@q�=@p��@o=@n��@nu%@nd�@m��@m�3@m��@mk�@l�@lw�@lS�@l6@k�W@k@O@j��@j}V@jC�@j)�@i��@i�@i�@i��@ic�@iF@i�@h�v@hĜ@h�@hQ�@h$@g�}@g�0@gy�@f�@fl�@e��@e��@eY�@d�9@d>B@dM@c�A@c�V@c|�@co@b�x@b~�@b�@ak�@aDg@a0�@`h�@_��@_]�@^�}@^c @^_�@^Ta@]��@]5�@]+@\��@\�@\�@[8@Z�2@Z�B@Z��@Z�@Z}V@Zh
@Z6�@Z �@Y��@Y�h@Yc�@Y�@WC�@V�c@V��@V�R@V�x@V��@V��@V}V@VkQ@VGE@U@Uk�@UL�@U2a@U+�@U�@T��@T�z@TI�@S�k@R��@R�!@RTa@Q�n@Q��@P��@O8@N�c@N�s@N�!@Nn�@NYK@NL0@N
�@M�)@M�3@M�n@Mo @L��@L�@J�@J��@J��@J��@Jc @JO@I�@I��@I��@I��@Is�@IB�@I@H��@HXy@H?�@HG@GU�@G�@F�@F�c@F�@F�@Fߤ@Fߤ@F�\@Fff@F\�@FYK@FTa@F1�@E��@E��@E=�@E%F@EV@D�v@D��@D�.@D|�@Dw�@Dr�@DZ@C��@C=@B�H@B�x@A�^@AVm@A/@@ѷ@@��@@?�@?��@?�q@?��@?t�@?,�@>�@>ں@>��@>��@>=q@>�@=�o@=��@=-w@<�/@<e�@<�@;�6@;��@;g�@;=@:��@9@9S&@9+@8�@8?�@7�@7A�@7!-@7"�@7$t@7(@6�,@6a|@6C�@6�@5�o@5�j@5�X@5zx@5Dg@5�@5�@5�@4��@4֡@4��@4b@36z@2�\@2�@2��@2Ta@2;�@20U@24@1�z@1IR@1%@0��@0�p@0��@0�@0�u@0�.@0��@0�Y@0K^@06@0'R@0�@0�@/��@/�0@/��@/s@/Y@/�@.��@.��@.��@.d�@.5?@.#:@.e@._@.�@-�Z@-��@-�T@-�9@-��@-�7@-0�@,��@,��@,e�@+�@+A�@*�M@*�x@*Ta@)��@)4@(Ɇ@(e�@'�&@'v`@'�@&�8@&�X@&u%@&M�@%�)@%�M@%c�@%IR@%A @%&�@$�P@$�@$I�@#��@#�K@#�[@#�$@#P�@"�X@"J�@":*@"O@!��@!X@!2a@ �@ ��@ V�@ -�@ �@�@�W@�&@�
@�q@�@�h@R�@�@�@k�@5�@q@��@��@�j@��@m�@K^@��@��@�]@�L@ff@@�@1�@�@�@
�@@L�@_@x@�@��@��@|�@|�@~�@��@x@j�@_p@X�@W?@\)@P�@$t@�@��@��@�H@�}@c @R�@Ov@;�@8�@�@O@#:@)�@.�@0U@e@��@�C@��@X@�@�@ѷ@��@h�@D�@?�@C-@9X@�@ݘ@ƨ@��@��@��@��@�	@{J@e�@RT@8@C@��@��@��@��@�@c@j@T�@ \@�p@e�@M@Ft@>B@4n@-�@~@7@�@!@7@�A@�q@�$@��@��@v`@X�@E9@�@xl@$�@_@�D@��@�7@�@��@��@�Y@l"@>B@M@خ@��@��@]�@@O@6z@!-@
��@
�r@
8�@	�T@	�@	q@�E@��@��@�@�j@�U@�@�?@��@�@�.@Z@x@�@�P@l�@]�@8@�A@=q@)�@�T@u�@L�@B�@:�@7L@%F@@��@�/@�.@'R@�W@��@��@�0@�0@�F@��@�f@�f@y�@e�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B�~B�~B�JB�0B�JB��B��B��B�B��B��B��B��B��B��B�JB�0B�~B��B�0B��B��B�B��B��Bp�BZ�B?�B�B��B�@B�B��B��B.BxBq�BnBjBiyBd�B_�B^�BX�BVBQ�BO�BN�BI�BF�BE�BC�B;0B*eBB
BB	�BUB��B��B�
B�B� BǮB��B�KB��B�5B��B��Bs�Bi_BR�B;�B4B*�B'�B vB�BBhB�B��B�9B��B�}B��B�\B��B��B��Bx�Bt�Bo�Bh�B]dBG_B2GB)�B�B	�B
��B
��B
�2B
�VB
��B
�TB
��B
�1B
ƎB
ðB
��B
��B
�$B
��B
��B
�B
��B
��B
��B
��B
~�B
v`B
q[B
i�B
^�B
]dB
S�B
NB
K)B
E�B
;�B
0;B
*B
&2B
"�B
B
�B
�B
�B
B	��B	��B	�DB	�B	�IB	��B	�:B	�VB	̳B	�)B	�rB	�	B	�B	��B	�jB	�B	�iB	�B	��B	�VB	�B	�xB	�B	�jB	��B	�AB	�B	z�B	qAB	e�B	a�B	\]B	ZQB	XyB	V9B	T�B	S�B	Q�B	O�B	M�B	I�B	E�B	CGB	@4B	>BB	<�B	;dB	:*B	8B	5�B	4�B	0!B	.�B	*0B	%�B	#�B	�B	�B	�B	�B	sB	sB	�B	SB	�B	,B	�B	�B	�B	�B		�B	�B	�B	�B	�B	�B	EB	�B	?B	�B	�B	�B	 4B	 �B��B�rB�XB�8B��B�B��B��B�tB�tB��B�TB�B�B�[B��B�;B��B�B��B��B�B�cB�/B��B��B�/B��B��B��B�B�wB�]B�]B�qB��B�IB� B��B��B��B�!B��B�oB��B�B�B�B��B��B�B��B�+B��B�B�}B�}B��B	 �B	 �B	;B	AB	�B	B	�B	MB	B	9B	�B	YB	�B	B	�B	�B	�B	�B	�B	7B	IB	]B	#�B	#B	#�B	%,B	%`B	%�B	%,B	$�B	'�B	0�B	2�B	4�B	72B	:�B	;B	;�B	=VB	@�B	CB	HB	H�B	I�B	JrB	KxB	L�B	M�B	O�B	QNB	Q�B	R�B	S�B	T�B	T�B	U�B	U�B	VB	V�B	W$B	WYB	WsB	WsB	Z�B	\CB	]/B	`\B	_�B	`�B	b�B	b�B	cnB	c�B	ezB	g�B	m�B	o�B	pB	p�B	qvB	q�B	q�B	q�B	q�B	r-B	r-B	r�B	s3B	s�B	t9B	zB	|PB	B	� B	��B	�uB	�gB	�B	�DB	��B	�4B	�OB	�tB	�2B	��B	��B	�6B	��B	�3B	�tB	��B	��B	�(B	ðB	�RB	�vB	�TB	�&B	�,B	յB	��B	��B	�B	ݘB	�B	�8B	�$B	�>B	��B	��B	��B	��B	��B	��B	�B	�9B	��B	��B	�^B	��B	�wB
 �B
�B
-B
�B
tB
+B
KB
	�B

rB

#B

rB
.B
B
�B
�B
�B
B
�B
�B
"�B
&�B
($B
)*B
)�B
*B
+6B
+�B
,B
+�B
,"B
,�B
0UB
1B
0;B
0UB
2GB
5B
6`B
72B
7�B
8B
8lB
8�B
9rB
:�B
;dB
<6B
>B
?�B
DMB
EB
EmB
E�B
GzB
H�B
IRB
I�B
I�B
J�B
K�B
LJB
L~B
L�B
L�B
M6B
MjB
M�B
O�B
S[B
WYB
[�B
]IB
^�B
_B
a-B
a�B
b4B
c�B
gmB
g�B
h�B
i*B
j�B
m�B
p�B
q�B
raB
r�B
s3B
s�B
t9B
tnB
t�B
uB
utB
vB
vFB
wLB
w�B
x�B
{�B
}qB
�B
�UB
�B
�B
�SB
�SB
�B
�B
��B
�B
�lB
��B
��B
�PB
��B
�B
�hB
��B
��B
�aB
��B
�
B
��B
��B
�B
��B
��B
��B
��B
�VB
��B
�B
�:B
��B
�@B
��B
��B
�,B
�,B
��B
�fB
�B
�$B
��B
�DB
��B
�UB
�;B
�B
�[B
�vB
��B
��B
��B
�B
��B
��B
�?B
��B
�+B
��B
�B
��B
�>B
�B
��B
�(B
�cB
� B
�B
B
ƎB
ƨB
��B
�_B
��B
�B
�B
��B
�B
�RB
ɆB
��B
��B
��B
��B
��B
�B
�HB
��B
�hB
��B
�B
�oB
ңB
�B
ӏB
��B
�B
՛B
��B
�SB
��B
ؓB
ؓB
��B
��B
��B
��B
��B
ٴB
�B
�B
�B
�B
�7B
��B
ۦB
�]B
ܬB
��B
�B
ݘB
��B
��B
�B
�B
�B
�B
�vB
��B
�bB
�TB
�B
�&B
��B
�,B
��B
�LB
�B
�B
�RB
�
B
�XB
�B
��B
�*B
��B
��B
�B
�B
�B
��B
��B
��B
��B
�}B
�B
��B
�OB
�B
�-B
�B
�B
�B
�B
��B
��B
��B
��B
�FB
��B
�fB
��B
��B
��B
�B
�lB
��B
�	B
�$B
�>B
�>B
�>B
�rB
��B
��B
�6B
�B
��B
��B
�<B
�qB
��B
��B
�(B
��B
�HB
�cB
��B
��B
��B B B B B �B �B �B �BB BoB�B�BuB�B�B-BGB{B�B�B�BBBBB3B3BMB�BB�B�BB�B�B1B�B	7B
=B
�B)B�BJBBjB�B�B<B�B�B�B�B�B�BBHB�BNB�BB B B�B[BB�BFB�B2B�B�B�B�B�B�B$B
B$B$B?B_B�BBeB�BkB�B�B�B#B#BWB�BqB�B~B�B�BOBjB�B�B�B�B!B!B!-B!-B!bB!�B!�B"B"B!�B!�B"4B"4B"NB"NB"NB"NB"NB"�B"�B"�B"�B#B#TB#�B#�B#�B#�B#�B$&B$B$&B$B#�B#�B$@B$�B$�B$�B%,B%zB%zB%�B&LB&fB&�B&�B&�B&�B'B'8B'RB'�B'�B'�B'�B'�B'�B'�B(
B(
B(>B(�B(�B)B*B*KB*eB*eB*�B*�B+6B+�B+�B,"B,"B,=B,"B,WB,WB,=B,WB,=B,�B-B-B-B-B-CB-CB-]B-�B.}B/ B/B/B/5B/�B0�B0�B0�B1B1'B1vB1�B1�B2-B2GB2�B2�B2�B2�B2�B3�B3�B4nB4�B5�B5�B5�B5�B6B6B5�B6B5�B6B6B6FB6zB6�B7fB7fB7�B7�B7�B8�B9>B9XB9�B:�B:�B:�B:�B:�B:�B:�B:�B;B;�B<B<�B<�B<�B<�B<�B<�B<�B="B=B=<B=�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B��B��B��B��B��B��B��B��B��B�~B�~B�JB�0B�JB��B��B��B�B��B��B��B��B��B��B�JB�0B�~B��B�0B��B��B�B��B��Bp�BZ�B?�B�B��B�@B�B��B��B.BxBq�BnBjBiyBd�B_�B^�BX�BVBQ�BO�BN�BI�BF�BE�BC�B;0B*eBB
BB	�BUB��B��B�
B�B� BǮB��B�KB��B�5B��B��Bs�Bi_BR�B;�B4B*�B'�B vB�BBhB�B��B�9B��B�}B��B�\B��B��B��Bx�Bt�Bo�Bh�B]dBG_B2GB)�B�B	�B
��B
��B
�2B
�VB
��B
�TB
��B
�1B
ƎB
ðB
��B
��B
�$B
��B
��B
�B
��B
��B
��B
��B
~�B
v`B
q[B
i�B
^�B
]dB
S�B
NB
K)B
E�B
;�B
0;B
*B
&2B
"�B
B
�B
�B
�B
B	��B	��B	�DB	�B	�IB	��B	�:B	�VB	̳B	�)B	�rB	�	B	�B	��B	�jB	�B	�iB	�B	��B	�VB	�B	�xB	�B	�jB	��B	�AB	�B	z�B	qAB	e�B	a�B	\]B	ZQB	XyB	V9B	T�B	S�B	Q�B	O�B	M�B	I�B	E�B	CGB	@4B	>BB	<�B	;dB	:*B	8B	5�B	4�B	0!B	.�B	*0B	%�B	#�B	�B	�B	�B	�B	sB	sB	�B	SB	�B	,B	�B	�B	�B	�B		�B	�B	�B	�B	�B	�B	EB	�B	?B	�B	�B	�B	 4B	 �B��B�rB�XB�8B��B�B��B��B�tB�tB��B�TB�B�B�[B��B�;B��B�B��B��B�B�cB�/B��B��B�/B��B��B��B�B�wB�]B�]B�qB��B�IB� B��B��B��B�!B��B�oB��B�B�B�B��B��B�B��B�+B��B�B�}B�}B��B	 �B	 �B	;B	AB	�B	B	�B	MB	B	9B	�B	YB	�B	B	�B	�B	�B	�B	�B	7B	IB	]B	#�B	#B	#�B	%,B	%`B	%�B	%,B	$�B	'�B	0�B	2�B	4�B	72B	:�B	;B	;�B	=VB	@�B	CB	HB	H�B	I�B	JrB	KxB	L�B	M�B	O�B	QNB	Q�B	R�B	S�B	T�B	T�B	U�B	U�B	VB	V�B	W$B	WYB	WsB	WsB	Z�B	\CB	]/B	`\B	_�B	`�B	b�B	b�B	cnB	c�B	ezB	g�B	m�B	o�B	pB	p�B	qvB	q�B	q�B	q�B	q�B	r-B	r-B	r�B	s3B	s�B	t9B	zB	|PB	B	� B	��B	�uB	�gB	�B	�DB	��B	�4B	�OB	�tB	�2B	��B	��B	�6B	��B	�3B	�tB	��B	��B	�(B	ðB	�RB	�vB	�TB	�&B	�,B	յB	��B	��B	�B	ݘB	�B	�8B	�$B	�>B	��B	��B	��B	��B	��B	��B	�B	�9B	��B	��B	�^B	��B	�wB
 �B
�B
-B
�B
tB
+B
KB
	�B

rB

#B

rB
.B
B
�B
�B
�B
B
�B
�B
"�B
&�B
($B
)*B
)�B
*B
+6B
+�B
,B
+�B
,"B
,�B
0UB
1B
0;B
0UB
2GB
5B
6`B
72B
7�B
8B
8lB
8�B
9rB
:�B
;dB
<6B
>B
?�B
DMB
EB
EmB
E�B
GzB
H�B
IRB
I�B
I�B
J�B
K�B
LJB
L~B
L�B
L�B
M6B
MjB
M�B
O�B
S[B
WYB
[�B
]IB
^�B
_B
a-B
a�B
b4B
c�B
gmB
g�B
h�B
i*B
j�B
m�B
p�B
q�B
raB
r�B
s3B
s�B
t9B
tnB
t�B
uB
utB
vB
vFB
wLB
w�B
x�B
{�B
}qB
�B
�UB
�B
�B
�SB
�SB
�B
�B
��B
�B
�lB
��B
��B
�PB
��B
�B
�hB
��B
��B
�aB
��B
�
B
��B
��B
�B
��B
��B
��B
��B
�VB
��B
�B
�:B
��B
�@B
��B
��B
�,B
�,B
��B
�fB
�B
�$B
��B
�DB
��B
�UB
�;B
�B
�[B
�vB
��B
��B
��B
�B
��B
��B
�?B
��B
�+B
��B
�B
��B
�>B
�B
��B
�(B
�cB
� B
�B
B
ƎB
ƨB
��B
�_B
��B
�B
�B
��B
�B
�RB
ɆB
��B
��B
��B
��B
��B
�B
�HB
��B
�hB
��B
�B
�oB
ңB
�B
ӏB
��B
�B
՛B
��B
�SB
��B
ؓB
ؓB
��B
��B
��B
��B
��B
ٴB
�B
�B
�B
�B
�7B
��B
ۦB
�]B
ܬB
��B
�B
ݘB
��B
��B
�B
�B
�B
�B
�vB
��B
�bB
�TB
�B
�&B
��B
�,B
��B
�LB
�B
�B
�RB
�
B
�XB
�B
��B
�*B
��B
��B
�B
�B
�B
��B
��B
��B
��B
�}B
�B
��B
�OB
�B
�-B
�B
�B
�B
�B
��B
��B
��B
��B
�FB
��B
�fB
��B
��B
��B
�B
�lB
��B
�	B
�$B
�>B
�>B
�>B
�rB
��B
��B
�6B
�B
��B
��B
�<B
�qB
��B
��B
�(B
��B
�HB
�cB
��B
��B
��B B B B B �B �B �B �BB BoB�B�BuB�B�B-BGB{B�B�B�BBBBB3B3BMB�BB�B�BB�B�B1B�B	7B
=B
�B)B�BJBBjB�B�B<B�B�B�B�B�B�BBHB�BNB�BB B B�B[BB�BFB�B2B�B�B�B�B�B�B$B
B$B$B?B_B�BBeB�BkB�B�B�B#B#BWB�BqB�B~B�B�BOBjB�B�B�B�B!B!B!-B!-B!bB!�B!�B"B"B!�B!�B"4B"4B"NB"NB"NB"NB"NB"�B"�B"�B"�B#B#TB#�B#�B#�B#�B#�B$&B$B$&B$B#�B#�B$@B$�B$�B$�B%,B%zB%zB%�B&LB&fB&�B&�B&�B&�B'B'8B'RB'�B'�B'�B'�B'�B'�B'�B(
B(
B(>B(�B(�B)B*B*KB*eB*eB*�B*�B+6B+�B+�B,"B,"B,=B,"B,WB,WB,=B,WB,=B,�B-B-B-B-B-CB-CB-]B-�B.}B/ B/B/B/5B/�B0�B0�B0�B1B1'B1vB1�B1�B2-B2GB2�B2�B2�B2�B2�B3�B3�B4nB4�B5�B5�B5�B5�B6B6B5�B6B5�B6B6B6FB6zB6�B7fB7fB7�B7�B7�B8�B9>B9XB9�B:�B:�B:�B:�B:�B:�B:�B:�B;B;�B<B<�B<�B<�B<�B<�B<�B<�B="B=B=<B=�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221104034701  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221104034715  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221104034715  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221104034716                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221104124722  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221104124722  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221104040811                      G�O�G�O�G�O�                