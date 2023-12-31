CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-14T21:35:16Z creation;2016-09-14T21:35:18Z conversion to V3.1;2019-12-19T08:30:23Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160914213516  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               %A   JA  I2_0576_037                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @���� 1   @���O���@;$��*0�dm��Q�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~y�D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @8��@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�ǮB���B�ǮB���C�qC�qC�qC��C	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK��CM�qCP
CQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*x�D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=��D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB��DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~x�D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�<{D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�<{D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D��{D�<{D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�FD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�~�A�t�A�p�A�t�A�p�A�r�A�l�A�jA�jA�hsA�hsA�dZA�ZA�;dA���Aч+A�"�A��A���A���A�;dA��A�S�A��TA��A�-A��
A��A�ĜA�^5A���A��
A�E�A��A���A��A�VA�p�A��A��uA���A�~�A�XA�bA���A�p�A���A�p�A��A��A��A�K�A��uA���A�hsA��A��#A� �A�G�A��A��A��TA�A�|�A�M�A��A�{A���A�ffA�VA�7LA��jA���A�jA�"�A���A��A��`A�O�A�p�A���A�dZA�=qA��jA��A�(�A~�!A{VAz{Ay?}Ay%Ax�yAw
=Av-Au�At��At5?As��AsG�Ar��Aqp�Ao�Ak��Ag��Af�AeAdA�Ab�\Aa��Aa|�Aa%A`ffA_&�A^��A]��A]�A[�-A[hsA[33AZ�+AY��AXz�AW�AW�AV�\AV$�AUoATbNAS�ARv�AQ�TAQl�AP�AO33AN�+AN=qAM��AM�AL  AKXAK�AJ~�AI�AH{AG�AFz�AEx�AD=qAC��AB�HAA�wA@�\A=�A=��A=;dA<^5A;�A:�A8�A7XA7oA6Q�A4��A4A�A3��A3+A2A�A1��A0�A0r�A01A/A/�A-�A-C�A,bA+%A*�HA*ȴA*��A)C�A(�A(ĜA'�A&�HA&ĜA&ĜA&ĜA&��A&n�A$�A#t�A#+A"A�A �A��A�A��A|�A�jA1AK�A�A(�A\)A�A��AZA�;A|�A�\A�A�A1'A|�AhsAK�A�A��AQ�A�An�A�TA;dA
�yA
$�A	��AȴA^5A-A�wA�A\)A��A�+A1'A�TA��A\)A�HA��A ��@�\)@��@�ƨ@�~�@�G�@��w@��+@��@���@�@�=q@�X@��@���@�-@�u@�\)@��@��;@�hs@�r�@��;@�\@���@��@��y@�M�@�1'@��@�`B@�r�@�dZ@�?}@� �@�
=@җ�@�{@�x�@У�@��;@ͺ^@˶F@ʗ�@Ɂ@�j@�S�@Ȭ@�v�@�x�@�j@Ý�@�V@��@�%@���@�V@��F@�v�@��h@��@�dZ@��^@���@�|�@��@��!@���@�Q�@��@�I�@�1'@�(�@�"�@���@�S�@���@�Q�@���@�I�@� �@���@���@���@�&�@��@�b@��!@��T@�x�@��#@�x�@��@���@��^@���@�1'@��;@�|�@��H@�^5@�^5@�v�@�r�@��9@��@�x�@��@�hs@�O�@��@�G�@�?}@�%@�z�@�I�@�A�@�1'@�(�@�  @��m@��
@��
@��@�\)@�33@��@�
=@��@���@���@�V@���@��-@�p�@�X@���@�j@��D@���@��@��u@�bN@�I�@�b@�o@�5?@�J@��#@�X@�%@��j@�9X@�ƨ@��P@��@���@��\@�5?@��^@���@��@�`B@��@�j@�dZ@���@���@�p�@�/@���@�1'@�\)@���@��@��#@��#@�@��^@�G�@�V@�%@��@���@�r�@�bN@�Q�@�Z@�9X@��
@�K�@�+@��@��@��H@�@�
=@��@�5?@���@�Q�@�z�@��@��j@��9@���@�%@�`B@�O�@��@���@���@�Q�@\)@~�@~�+@~5?@|�D@{S�@{ƨ@{�F@z�!@w�;@t�@tZ@tj@t��@t��@t��@sƨ@rn�@q�@q��@qx�@qx�@q�7@q�@r�\@s"�@s�@s�@sS�@r�H@r�!@r~�@r-@rM�@rM�@r-@r~�@r��@r^5@rM�@r=q@q��@q�@q��@q��@qx�@qhs@q%@p�u@pb@o��@o�P@o|�@o\)@n��@n�y@n�@nff@nV@nV@nV@nV@nV@nV@nV@nV@nV@nV@nE�@n$�@m��@m`B@mV@l�D@lI�@l9X@l(�@l(�@l(�@l�@k�
@k��@k�@kt�@kC�@k@jM�@i��@ihs@h��@h��@hr�@h1'@h �@h  @g�;@g�@gK�@fv�@f{@e@ep�@e�@d�D@d9X@d�@ct�@b�!@b~�@b=q@a��@a�#@a�^@ax�@aG�@a7L@a�@`��@`�9@`b@_��@_\)@_;d@_
=@^�R@^�+@^V@^ff@^ff@^ff@^E�@]�T@]`B@\�j@\I�@[�m@[�F@[��@[dZ@Z�H@Y�#@Yx�@Y7L@X�u@X  @W|�@W\)@V��@Vȴ@VE�@Up�@U?}@T�@Tj@TI�@T(�@S�F@S��@SS�@R��@R-@Q��@Q��@QG�@P��@PA�@P  @O�@O��@OK�@N��@Nv�@NV@NE�@N$�@N$�@M�T@M�@L��@K��@Kt�@K"�@J�!@Jn�@J�@I��@I�7@Ix�@I%@H�9@HbN@G��@G|�@G\)@G\)@GK�@G;d@G+@F�y@FV@F@E�T@E�h@Ep�@D�@D(�@C�m@C��@C"�@B�H@B��@B�!@B�\@BM�@B=q@B-@A��@Ax�@A&�@@��@@Ĝ@@�9@@�@@Q�@@ �@?�;@?��@?;d@>�y@>ff@>@=�h@=p�@=`B@=?}@=V@<�/@<��@<�j@<z�@;�m@;ƨ@;��@;�@;t�@;C�@;o@:��@:�!@:��@:�\@:~�@:n�@:^5@:M�@:J@9�^@9hs@9X@9&�@8��@8�u@81'@8  @7�w@7\)@7+@6��@6ȴ@6��@6{@5`B@5O�@5?}@5V@4�j@4z�@4Z@4(�@3�
@3��@3o@2M�@1�@1�^@1��@1x�@17L@17L@0��@0�u@0Q�@0b@/��@/�w@/��@/|�@/�@.�y@.ȴ@.V@.{@-�@.@-�T@-@-��@-�@,��@,�@,�/@,�j@,Z@+�m@+�
@+�F@+��@+C�@*��@*�\@*^5@*=q@)�@)�^@)%@(��@(��@(�@(�@(�@(�@(Q�@(  @'��@'�P@'\)@';d@&�y@&�R@&V@&$�@&@%�@%@%p�@$�/@$�j@$Z@#��@#�@#C�@#C�@#33@#@"�H@"��@"�\@"~�@"^5@"-@!��@!�#@!�^@!��@!hs@!X@!7L@ ��@ ��@ Q�@ A�@ A�@ 1'@ b@�@�;@�w@|�@\)@K�@;d@+@
=@�R@ff@V@E�@$�@@p�@/@�/@�j@z�@(�@��@�F@t�@�@-@�#@hs@&�@%@��@��@�@ �@�@��@��@�w@�w@�@\)@ȴ@�+@v�@V@$�@@��@O�@��@�D@�@��@ƨ@��@t�@S�@o@��@�\@^5@M�@�@�@�^@�7@x�@hs@G�@7L@&�@�@��@��@�9@��@�@�@\)@��@�R@v�@$�@�-@p�@p�@`B@`B@/@��@�j@�D@z�@I�@(�@�@�@��@�m@ƨ@S�@33@"�@@
�!@
n�@
M�@
�@	��@	7L@	7L@	�@Ĝ@�@1'@ �@b@  @�;@\)@+@�@�@
=@��@ȴ@�R@��@v�@V@V@E�@E�@5?@5?@$�@{@�T@��@�@`B@O�@�@��@�/@�@Z@9X@�@�@1@ƨ@�F@��@�@C�@o@�H@��@��@~�@^5@^5@M�@�@J@�#@�^@��@7L@ ��@ �`@ ��@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�~�A�t�A�p�A�t�A�p�A�r�A�l�A�jA�jA�hsA�hsA�dZA�ZA�;dA���Aч+A�"�A��A���A���A�;dA��A�S�A��TA��A�-A��
A��A�ĜA�^5A���A��
A�E�A��A���A��A�VA�p�A��A��uA���A�~�A�XA�bA���A�p�A���A�p�A��A��A��A�K�A��uA���A�hsA��A��#A� �A�G�A��A��A��TA�A�|�A�M�A��A�{A���A�ffA�VA�7LA��jA���A�jA�"�A���A��A��`A�O�A�p�A���A�dZA�=qA��jA��A�(�A~�!A{VAz{Ay?}Ay%Ax�yAw
=Av-Au�At��At5?As��AsG�Ar��Aqp�Ao�Ak��Ag��Af�AeAdA�Ab�\Aa��Aa|�Aa%A`ffA_&�A^��A]��A]�A[�-A[hsA[33AZ�+AY��AXz�AW�AW�AV�\AV$�AUoATbNAS�ARv�AQ�TAQl�AP�AO33AN�+AN=qAM��AM�AL  AKXAK�AJ~�AI�AH{AG�AFz�AEx�AD=qAC��AB�HAA�wA@�\A=�A=��A=;dA<^5A;�A:�A8�A7XA7oA6Q�A4��A4A�A3��A3+A2A�A1��A0�A0r�A01A/A/�A-�A-C�A,bA+%A*�HA*ȴA*��A)C�A(�A(ĜA'�A&�HA&ĜA&ĜA&ĜA&��A&n�A$�A#t�A#+A"A�A �A��A�A��A|�A�jA1AK�A�A(�A\)A�A��AZA�;A|�A�\A�A�A1'A|�AhsAK�A�A��AQ�A�An�A�TA;dA
�yA
$�A	��AȴA^5A-A�wA�A\)A��A�+A1'A�TA��A\)A�HA��A ��@�\)@��@�ƨ@�~�@�G�@��w@��+@��@���@�@�=q@�X@��@���@�-@�u@�\)@��@��;@�hs@�r�@��;@�\@���@��@��y@�M�@�1'@��@�`B@�r�@�dZ@�?}@� �@�
=@җ�@�{@�x�@У�@��;@ͺ^@˶F@ʗ�@Ɂ@�j@�S�@Ȭ@�v�@�x�@�j@Ý�@�V@��@�%@���@�V@��F@�v�@��h@��@�dZ@��^@���@�|�@��@��!@���@�Q�@��@�I�@�1'@�(�@�"�@���@�S�@���@�Q�@���@�I�@� �@���@���@���@�&�@��@�b@��!@��T@�x�@��#@�x�@��@���@��^@���@�1'@��;@�|�@��H@�^5@�^5@�v�@�r�@��9@��@�x�@��@�hs@�O�@��@�G�@�?}@�%@�z�@�I�@�A�@�1'@�(�@�  @��m@��
@��
@��@�\)@�33@��@�
=@��@���@���@�V@���@��-@�p�@�X@���@�j@��D@���@��@��u@�bN@�I�@�b@�o@�5?@�J@��#@�X@�%@��j@�9X@�ƨ@��P@��@���@��\@�5?@��^@���@��@�`B@��@�j@�dZ@���@���@�p�@�/@���@�1'@�\)@���@��@��#@��#@�@��^@�G�@�V@�%@��@���@�r�@�bN@�Q�@�Z@�9X@��
@�K�@�+@��@��@��H@�@�
=@��@�5?@���@�Q�@�z�@��@��j@��9@���@�%@�`B@�O�@��@���@���@�Q�@\)@~�@~�+@~5?@|�D@{S�@{ƨ@{�F@z�!@w�;@t�@tZ@tj@t��@t��@t��@sƨ@rn�@q�@q��@qx�@qx�@q�7@q�@r�\@s"�@s�@s�@sS�@r�H@r�!@r~�@r-@rM�@rM�@r-@r~�@r��@r^5@rM�@r=q@q��@q�@q��@q��@qx�@qhs@q%@p�u@pb@o��@o�P@o|�@o\)@n��@n�y@n�@nff@nV@nV@nV@nV@nV@nV@nV@nV@nV@nV@nE�@n$�@m��@m`B@mV@l�D@lI�@l9X@l(�@l(�@l(�@l�@k�
@k��@k�@kt�@kC�@k@jM�@i��@ihs@h��@h��@hr�@h1'@h �@h  @g�;@g�@gK�@fv�@f{@e@ep�@e�@d�D@d9X@d�@ct�@b�!@b~�@b=q@a��@a�#@a�^@ax�@aG�@a7L@a�@`��@`�9@`b@_��@_\)@_;d@_
=@^�R@^�+@^V@^ff@^ff@^ff@^E�@]�T@]`B@\�j@\I�@[�m@[�F@[��@[dZ@Z�H@Y�#@Yx�@Y7L@X�u@X  @W|�@W\)@V��@Vȴ@VE�@Up�@U?}@T�@Tj@TI�@T(�@S�F@S��@SS�@R��@R-@Q��@Q��@QG�@P��@PA�@P  @O�@O��@OK�@N��@Nv�@NV@NE�@N$�@N$�@M�T@M�@L��@K��@Kt�@K"�@J�!@Jn�@J�@I��@I�7@Ix�@I%@H�9@HbN@G��@G|�@G\)@G\)@GK�@G;d@G+@F�y@FV@F@E�T@E�h@Ep�@D�@D(�@C�m@C��@C"�@B�H@B��@B�!@B�\@BM�@B=q@B-@A��@Ax�@A&�@@��@@Ĝ@@�9@@�@@Q�@@ �@?�;@?��@?;d@>�y@>ff@>@=�h@=p�@=`B@=?}@=V@<�/@<��@<�j@<z�@;�m@;ƨ@;��@;�@;t�@;C�@;o@:��@:�!@:��@:�\@:~�@:n�@:^5@:M�@:J@9�^@9hs@9X@9&�@8��@8�u@81'@8  @7�w@7\)@7+@6��@6ȴ@6��@6{@5`B@5O�@5?}@5V@4�j@4z�@4Z@4(�@3�
@3��@3o@2M�@1�@1�^@1��@1x�@17L@17L@0��@0�u@0Q�@0b@/��@/�w@/��@/|�@/�@.�y@.ȴ@.V@.{@-�@.@-�T@-@-��@-�@,��@,�@,�/@,�j@,Z@+�m@+�
@+�F@+��@+C�@*��@*�\@*^5@*=q@)�@)�^@)%@(��@(��@(�@(�@(�@(�@(Q�@(  @'��@'�P@'\)@';d@&�y@&�R@&V@&$�@&@%�@%@%p�@$�/@$�j@$Z@#��@#�@#C�@#C�@#33@#@"�H@"��@"�\@"~�@"^5@"-@!��@!�#@!�^@!��@!hs@!X@!7L@ ��@ ��@ Q�@ A�@ A�@ 1'@ b@�@�;@�w@|�@\)@K�@;d@+@
=@�R@ff@V@E�@$�@@p�@/@�/@�j@z�@(�@��@�F@t�@�@-@�#@hs@&�@%@��@��@�@ �@�@��@��@�w@�w@�@\)@ȴ@�+@v�@V@$�@@��@O�@��@�D@�@��@ƨ@��@t�@S�@o@��@�\@^5@M�@�@�@�^@�7@x�@hs@G�@7L@&�@�@��@��@�9@��@�@�@\)@��@�R@v�@$�@�-@p�@p�@`B@`B@/@��@�j@�D@z�@I�@(�@�@�@��@�m@ƨ@S�@33@"�@@
�!@
n�@
M�@
�@	��@	7L@	7L@	�@Ĝ@�@1'@ �@b@  @�;@\)@+@�@�@
=@��@ȴ@�R@��@v�@V@V@E�@E�@5?@5?@$�@{@�T@��@�@`B@O�@�@��@�/@�@Z@9X@�@�@1@ƨ@�F@��@�@C�@o@�H@��@��@~�@^5@^5@M�@�@J@�#@�^@��@7L@ ��@ �`@ ��@ ��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BcTBcTBcTBcTBcTBcTBbNBbNBbNBbNBcTBcTBcTBffBm�Bz�B� B� B�B~�By�Bw�Bu�Bm�BM�B@�B2-B'�B!�B�B\B+BB��B��B�B�HB�/B�qB��B��B�1B�B�bB�JB�Bz�Bw�Bl�B\)BO�BG�B5?B�B\BB��B�B�fB��B�XB��B��B�oB�VB�DB}�Bt�Bp�B`BBE�B,B&�B!�B�B
=B
��B
�B
�TB
�B
��B
ȴB
ƨB
�wB
�FB
��B
��B
�B
y�B
q�B
n�B
l�B
hsB
_;B
ZB
T�B
O�B
H�B
D�B
B�B
33B
$�B
%B	�/B	ĜB	�dB	�FB	��B	��B	��B	��B	��B	�\B	�=B	�%B	�B	x�B	v�B	u�B	p�B	m�B	iyB	dZB	aHB	]/B	ZB	W
B	Q�B	M�B	F�B	B�B	@�B	=qB	;dB	8RB	6FB	33B	/B	(�B	!�B	�B	�B	�B	{B	bB	VB	JB		7B	B	  B��B��B�B�B�B�B�`B�/B�B��B��B��BǮBȴBȴBȴBǮBĜBÖB��B�}B�jB�jB�RB�?B�?B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�JB�7B�1B�B� B}�By�By�Bx�Bu�Bt�Br�Bq�Bp�BiyBffBcTB`BB_;B\)B\)B[#B[#BYBXBW
BR�BP�BL�BK�BH�BG�BE�BC�BB�BA�BA�B=qB;dB:^B9XB8RB7LB6FB5?B33B.B.B+B'�B%�B#�B"�B!�B �B �B�B�B�B�B�B�B�B�B�B{BoBoBhBbB\BPBJBPBVBDB	7B1B	7B
=B
=B
=B
=BDBDB
=B	7BDBDBPBDBDBVB�B�B{BoBoBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B(�BA�BA�B@�BA�BC�BB�B@�B?}B:^B2-B1'B2-B?}BR�BdZBffBhsBiyBn�Bp�Bo�Bm�B]/BVBS�BVBYBZB[#B\)B_;BdZBjBn�B�B�%B�7B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�RBÖBBÖBŢBȴBɺB��B��B��B��B�
B�B�B�B�)B�/B�5B�HB�NB�NB�ZB�`B�`B�mB�yB�yB�B�B�B��B��B��B��B��B��B��B��B	B	B		7B	PB	�B	�B	�B	�B	 �B	!�B	"�B	#�B	'�B	-B	.B	.B	/B	1'B	49B	5?B	8RB	:^B	=qB	>wB	?}B	A�B	B�B	?}B	A�B	B�B	C�B	E�B	F�B	G�B	J�B	N�B	P�B	S�B	XB	YB	\)B	bNB	dZB	ffB	hsB	hsB	hsB	jB	jB	jB	iyB	ffB	e`B	ffB	hsB	l�B	n�B	p�B	p�B	s�B	t�B	v�B	w�B	z�B	~�B	�B	�%B	�7B	�=B	�JB	�PB	�PB	�JB	�PB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�3B	�9B	�9B	�?B	�RB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�^B	�^B	�dB	�dB	�dB	�wB	�}B	��B	��B	��B	B	B	B	B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�HB	�NB	�NB	�ZB	�`B	�`B	�`B	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
JB
JB
VB
VB
bB
hB
hB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BcnBcTBcTBcTBcTBcTBbNBbNBbNBbhBcnBc�Bc�BgBn}B{�B�iB�B��B�iB{B{B~�ByXBW�BGEB7B+�B%zBOBNB�BB�}B�0B�B��B�TB��B�nB��B�B�B��B��B�aB|�Bz�BoB^jBR�BK�B9$B!�BhB�B��B�AB�KB�NB��B��B��B�&B��B�B.Bv+Bs�Bd&BHKB,�B'�B#:B�BB
�zB
�"B
�FB
ڠB
ΊB
�lB
��B
�OB
��B
�sB
��B
�SB
z�B
r-B
oOB
n}B
iyB
`'B
Z�B
U�B
P�B
IRB
E�B
D�B
6+B
)*B

XB	�pB	�%B	��B	�B	��B	�tB	�vB	��B	��B	�.B	�DB	�_B	�{B	yXB	wLB	v�B	q�B	o5B	j0B	e,B	bhB	^B	[WB	W�B	R�B	OvB	GzB	C{B	BB	>�B	<6B	8�B	72B	4B	0UB	)�B	"hB	 �B	VB	�B	�B	�B	�B	�B	
=B	SB	�B��B��B�6B�UB�B��B��B�VBںBңB�B�PBȴBɆBɺB��BȚBňB�3B� B�B��B��B�rB��B�`B��B�cB��B�wB�B��B�XB��B�B��B�B�LB��B��B�zB��B�;B��B��B��B�B��B�-B�B~�Bz�Bz�By�BvFBuZBs�Br�BsMBj�Bg�BdtBa-B`B\]B\xB[�B[�BY�BYKBX_BS�BQ�BM�BL�BI�BH�BF?BDBC{BB�BC-B>(B;�B:�B9�B8�B7�B7LB6�B4�B/�B/�B,B(�B&�B$�B#�B"NB!bB!�B!-BpBB�B�B�BB�B
B�B&B�BTBNBB"BB�BBB0B	�B	B
�B
�B
�B
�B
�B�B�BB
�B~BB"B�B�B"B�BeBMB&B[BB�B�B�B�ByB?BEB�B�BkBkB�B�BqB�B(�BA�BA�B@�BB�BEBC�BAoBA;B<B2�B1[B1�B?BS&Bd�Bf�BiBj�BoOBq�BrGBp;B^�BV�BT�BV�BY�BZkB[�B\�B_�BdZBj0Bm�B��B�B�B�vB��B��B��B��B��B�B�B��B��B��B��B�B�B��B��B�B�2B�B�
B�B�KB�kB��B��B�)B�;B��B��B��B��BÖBŢB��B��B�B�B�HBѝB׍B�KB�eB�kB�xB�~BޞB�B�B�B�B�B�B�B�B�B�B��B�AB�tB�LB�rB�$B�DB�6B��B��B	�B	�B		lB	�B	�B	�B	�B	�B	 �B	!�B	#B	$&B	(
B	-)B	./B	.IB	/iB	1�B	4TB	5tB	8RB	:^B	=�B	>�B	?�B	BAB	C{B	?�B	AoB	BuB	C�B	E�B	F�B	GzB	J�B	OB	Q4B	TFB	XEB	YeB	\�B	b�B	d�B	f�B	iB	h�B	hsB	j�B	kB	k�B	jKB	f�B	e`B	ffB	h�B	l�B	o B	q'B	p�B	s�B	t�B	v�B	w�B	z�B	~�B	��B	�B	�RB	�XB	�~B	�jB	�jB	�dB	�PB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	�0B	�]B	�iB	�'B	�AB	�GB	�hB	�TB	�TB	��B	�RB	�RB	�XB	�XB	�rB	�XB	�^B	�xB	�^B	�xB	��B	�B	��B	��B	��B	��B	��B	��B	B	B	B	ªB	��B	ðB	ĶB	ĶB	żB	��B	�B	��B	��B	��B	��B	��B	��B	� B	�B	� B	� B	� B	�@B	�9B	�$B	�?B	�_B	�1B	�WB	�CB	�xB	�dB	�IB	�jB	�OB	�bB	�B	�NB	�B	�zB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�$B	�B	��B	�0B	�B	�"B	�]B	�B	�.B
 4B
B
 B
;B
AB
-B
GB
MB
9B
%B
YB
_B
fB
	RB
	RB
	RB

rB

rB
^B
^B
JB
~B
dB
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%B
%�B
&B
'B
($B
($B
)B
)B
)B
)*B
)B
)B
)B
)B
*KB
*B
+B
+B
+B
+B
,"B
,"B
,"B
,"B
-B
-B
-B
-)B
-)B
-)B
./B
./B
./B
./B
/5B
/OB
0;B
0;B
0;B
1AB
1[B
1[B
2GB
2GB
2aB
3�B
3hB
3MB
3MB
4TB
4nB
5ZB
5ZB
5ZB
6`B
6zB
6�B
6`B
6zB
6zB
6`B
6`B
7fB
8lB
9rB
9rB
9rB
9rB
9�B
9rB
9rB
9rB
:xB
:xB
;B
;B
<jB
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
MB
L�B
L�B
MB
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
PB
O�B
O�B
Q B
Q B
RB
Q�B
RB
R B
R:B
SB
S&B
SB
TB
TB
T,B
TB
UB
UB
VB
VB
VB
VB
V9B
V9B
VSB
W
B
W$B
W$B
W?B
W$B
W?B
X+B
XEB
YKB
YKB
ZQB
Z7B
Z7B
Z7B
Z7B
Z7B
[=B
[#B
\CB
\CB
\CB
\CB
]dB
]dB
]IB
]/B
]IB
^5B
^5B
^5B
_VB
_;B
_�B
`�B
`\B
aHB
abB
bhB
cnB
cnB
c�B
c�B
cnB
cnB
dZB
dZB
dZB
dtB
dtB
ezB
ezB
ezB
ezB
e`B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
gmB
gmB
g�B
g�B
h�B
hsB
hsB
iyB
i�B
i�B
i�B
jB
jB
jB
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<<j<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609190033022016091900330220160919003302201806221213582018062212135820180622121358201804050406372018040504063720180405040637  JA  ARFMdecpA19c                                                                20160915063505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160914213516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160914213516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160914213516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160914213517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160914213517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160914213517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160914213517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160914213518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160914213518                      G�O�G�O�G�O�                JA  ARUP                                                                        20160914223002                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160915153504  CV  JULD            G�O�G�O�F�W�                JM  ARCAJMQC2.0                                                                 20160918153302  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160918153302  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190637  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031358  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                