CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-09T21:35:45Z creation;2018-02-09T21:36:03Z conversion to V3.1;2019-12-19T07:49:43Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180209213545  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_208                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�K1���1   @�K2����@:������d]+��a1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�C3Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @8��@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A�RA��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B�ǮB���B���B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�ǮB���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D7�D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB��DC\DC�\DD\DD�\DE��DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D��{D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�B�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�<{D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�F11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Q�A�O�A�O�A�O�A�M�A�Q�A�S�A�VA�S�A�S�A�VA�VA�XA�ZA�XA�XA�VA�S�A�S�A�VA�VA�ZA�\)A�^5A�^5A�^5A�dZA�bNA�bNA�bNA�`BA�\)A�XA�XA�ZA�^5A�XA�S�A�O�A�M�A�E�A�+A��A��A�ƨA��A�9XA���A��yA��RA�dZA�;dA�JA�ĜA���A�-A�v�A��DA�
=A�x�A�M�A��^A���A�jA��A�A���A�p�A���A�A�A���A�%A�dZA��mA��jA���A�;dA�p�A���A�33A���A�VA���A��uA��A�ƨA��A��FA�S�A�VA��
A�l�A���A���A�A�A��#A���A�\)A�  Al�A}�
A|��Ay��Ax�DAw�#Av�RAuVAp��AnI�AmO�Al�jAj��AgAedZAd��AdffAd-AdbAaƨA_+A]�A]�A]K�A]oA\�`A[C�AZbAYC�AVr�ATn�AQ�AP�DAPz�APjAO��AL��AK��AK/AJv�AHĜAG��AF��AEK�AD�/AD�DAChsAA��A@��A@v�A?��A?�A?33A>��A>A=K�A<ȴA<M�A;�;A;C�A:ffA9ƨA9?}A8��A8�A8Q�A8{A7��A7K�A6�RA4�uA2VA1�mA1��A1|�A0�A0I�A/;dA-ƨA,bNA+�
A+x�A*��A)��A(��A(A�A'x�A%�FA$��A$��A$�A$��A$��A$�uA$�DA$z�A$A�A#�mA"JA�A��A7LA=qA��A�jA��An�A�A�/Av�A��Ax�A�jA9XAA�A�#A�A��A�A�A�A��A��A�A��A
�\A	AĜA�;A33A�`AĜA�A�uA�+An�AQ�A1'Al�A��AĜAr�A1A&�A�A�DA=qA��@�l�@�/@�1'@�t�@��@��#@���@�+@��`@�u@�r�@�A�@�w@�R@��@�V@�1@���@�V@�V@��@�7@��#@��@ߍP@�
=@�?}@�@ڧ�@�x�@؃@��@�t�@���@�V@�x�@���@��@�o@�G�@Ѓ@�  @ϥ�@�
=@��H@͡�@̬@�1'@�b@�t�@��@ʟ�@��@ȋD@�ƨ@�@Ə\@�n�@�E�@�-@�$�@��@��@�J@őh@�|�@��@��u@���@�l�@��@�`B@�I�@���@���@���@���@�"�@���@��T@���@��j@��w@�@�&�@�ƨ@�n�@���@�O�@�z�@��;@�\)@�o@�$�@��D@�33@�ff@�X@�Q�@���@�K�@���@���@���@�Q�@���@���@���@��^@�`B@���@�A�@�1'@�1@��P@�"�@���@��/@��j@�z�@�I�@�  @���@�\)@�K�@�C�@�;d@�;d@�33@���@�5?@�?}@��u@�A�@���@���@�
=@�V@��@��@��h@���@�Z@���@��@���@�5?@���@���@��@��`@�z�@�Z@�9X@��@��@��F@�t�@�|�@���@���@���@��@��@�K�@���@��@�^5@�J@�G�@�O�@�&�@��@�Ĝ@��9@��@���@�Z@�I�@�I�@�(�@��@�ƨ@��@�C�@�33@�o@���@���@��@���@��T@�x�@�x�@�x�@�`B@�7L@���@��@�K�@�+@�C�@�S�@�\)@�S�@�"�@���@�@�
=@��\@�v�@�V@��7@�%@���@��@�9X@��@�1@�@~��@}`B@}p�@|�@{��@z��@zn�@z=q@y�@y��@y�^@y��@y�7@yX@y&�@xĜ@x�u@xr�@xA�@w�@w�@w�P@w|�@wK�@w�@v��@v�y@v��@vE�@v{@u�T@u�@t��@t��@tz�@tj@t1@sƨ@s��@s��@s�@r�H@q��@p�`@pĜ@p�@pQ�@pQ�@p1'@p1'@p1'@p1'@p1'@p1'@o�@o�;@o�P@o;d@n�@mp�@l�j@l�@k�F@kC�@j�H@j~�@j�@i�@i��@i��@iX@i&�@h��@h�9@h��@h��@h��@h�9@hr�@hb@g�;@g��@f��@fv�@e��@d��@d��@d�D@d�@e�@d�j@dZ@dj@c�F@c"�@b�\@b-@bJ@b�@a�#@ahs@`�`@`��@`�@`�@`�@`�@`A�@_+@]�@]p�@]�@\j@\9X@\Z@\9X@[dZ@[o@Y��@Y��@Y�7@X�`@XQ�@W�@W;d@V��@Vv�@V{@U�-@U`B@U`B@U`B@U�@U�@T�/@TZ@S�@R�!@Rn�@R^5@R=q@R-@RJ@Q�@Q��@Q�#@Q�^@Qhs@P��@PQ�@P1'@Pb@O��@O
=@N��@N�+@Nv�@Nff@N$�@N$�@M��@M�@M�@L�j@Lz�@LZ@K��@K�F@K��@K��@K�@KdZ@K"�@J~�@J�@I�#@I��@Ihs@IG�@I�@HĜ@H�u@HQ�@H1'@G|�@G;d@G;d@G
=@Fȴ@F�+@E�@E/@D��@Dz�@DI�@D(�@D1@C�m@Cƨ@C��@C��@CdZ@B�@B�!@B�\@B-@BJ@BJ@A��@A�#@A��@A�7@AG�@A�@@��@@1'@?��@?\)@>��@>�@>v�@>{@=p�@<j@<1@;�
@;��@;C�@;o@:�@:~�@9��@9X@9%@8b@7�@7�w@7��@7|�@7K�@6��@6ȴ@6�R@6��@6ff@6V@5�@5��@5`B@5/@4�@4�j@4��@4z�@4j@3�m@3�@3t�@3t�@3t�@3t�@3C�@3o@3@3@2��@2��@2M�@1��@1�7@1x�@1&�@0�`@0��@0�9@0�u@0Q�@0 �@0b@/�w@/\)@/;d@/;d@/;d@/�@/
=@.��@.5?@.$�@-��@-��@-O�@,��@,��@,�@,��@,��@,�D@,�D@,�D@,z�@,I�@,9X@,1@+�
@+�F@+��@+��@+dZ@+33@*�H@*�!@*n�@*=q@)�#@)hs@)&�@(��@(�@(A�@(b@(  @'�@'�;@'��@'�w@'�@'�P@'\)@'+@'�@&��@&ȴ@&��@&$�@%�T@%�-@%�h@%p�@%�@$�@$�j@$�j@$z�@$I�@$�@#�m@#ƨ@#��@#�@#dZ@#"�@"�H@"��@"~�@"^5@"�@!hs@!7L@ ��@ ��@ �9@ �u@ Q�@  �@�w@\)@
=@�R@v�@V@5?@��@/@�@I�@9X@9X@1@�m@ƨ@��@t�@dZ@S�@��@�!@�\@^5@M�@M�@J@�#@�^@�^@�7@hs@7L@�9@A�@1'@b@�@�@|�@+@��@�y@E�@�@�@��@O�@��@��@�j@�D@j@Z@1@�m@�
@�
@�F@dZ@"�@��@�!@n�@�@�#@��@x�@hs@G�@%@Ĝ@�u@�@A�@1'@ �@b@  @�@�;@�@�P@K�@�y@v�@$�@@�@�-@�h@p�@O�@/@�@V@�@�/@Z@�@�
@33@
�!@
n�@
=q@
-@
�@
�@	�@	�^@	�7@	hs@	G�@	%@�`@��@�9@�u@�u@�@bN@Q�@�;@�P@|�@|�@|�@\)@
=@�y@ȴ@�R@��@v�@E�@5?@�T@@@�-@�h@`B@�@��@��@Z@9X@9X@9X@(�@1@1@�m@��@��@S�@o@��@�!@�\@~�@n�@^5@M�@=q@-@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�Q�A�O�A�O�A�O�A�M�A�Q�A�S�A�VA�S�A�S�A�VA�VA�XA�ZA�XA�XA�VA�S�A�S�A�VA�VA�ZA�\)A�^5A�^5A�^5A�dZA�bNA�bNA�bNA�`BA�\)A�XA�XA�ZA�^5A�XA�S�A�O�A�M�A�E�A�+A��A��A�ƨA��A�9XA���A��yA��RA�dZA�;dA�JA�ĜA���A�-A�v�A��DA�
=A�x�A�M�A��^A���A�jA��A�A���A�p�A���A�A�A���A�%A�dZA��mA��jA���A�;dA�p�A���A�33A���A�VA���A��uA��A�ƨA��A��FA�S�A�VA��
A�l�A���A���A�A�A��#A���A�\)A�  Al�A}�
A|��Ay��Ax�DAw�#Av�RAuVAp��AnI�AmO�Al�jAj��AgAedZAd��AdffAd-AdbAaƨA_+A]�A]�A]K�A]oA\�`A[C�AZbAYC�AVr�ATn�AQ�AP�DAPz�APjAO��AL��AK��AK/AJv�AHĜAG��AF��AEK�AD�/AD�DAChsAA��A@��A@v�A?��A?�A?33A>��A>A=K�A<ȴA<M�A;�;A;C�A:ffA9ƨA9?}A8��A8�A8Q�A8{A7��A7K�A6�RA4�uA2VA1�mA1��A1|�A0�A0I�A/;dA-ƨA,bNA+�
A+x�A*��A)��A(��A(A�A'x�A%�FA$��A$��A$�A$��A$��A$�uA$�DA$z�A$A�A#�mA"JA�A��A7LA=qA��A�jA��An�A�A�/Av�A��Ax�A�jA9XAA�A�#A�A��A�A�A�A��A��A�A��A
�\A	AĜA�;A33A�`AĜA�A�uA�+An�AQ�A1'Al�A��AĜAr�A1A&�A�A�DA=qA��@�l�@�/@�1'@�t�@��@��#@���@�+@��`@�u@�r�@�A�@�w@�R@��@�V@�1@���@�V@�V@��@�7@��#@��@ߍP@�
=@�?}@�@ڧ�@�x�@؃@��@�t�@���@�V@�x�@���@��@�o@�G�@Ѓ@�  @ϥ�@�
=@��H@͡�@̬@�1'@�b@�t�@��@ʟ�@��@ȋD@�ƨ@�@Ə\@�n�@�E�@�-@�$�@��@��@�J@őh@�|�@��@��u@���@�l�@��@�`B@�I�@���@���@���@���@�"�@���@��T@���@��j@��w@�@�&�@�ƨ@�n�@���@�O�@�z�@��;@�\)@�o@�$�@��D@�33@�ff@�X@�Q�@���@�K�@���@���@���@�Q�@���@���@���@��^@�`B@���@�A�@�1'@�1@��P@�"�@���@��/@��j@�z�@�I�@�  @���@�\)@�K�@�C�@�;d@�;d@�33@���@�5?@�?}@��u@�A�@���@���@�
=@�V@��@��@��h@���@�Z@���@��@���@�5?@���@���@��@��`@�z�@�Z@�9X@��@��@��F@�t�@�|�@���@���@���@��@��@�K�@���@��@�^5@�J@�G�@�O�@�&�@��@�Ĝ@��9@��@���@�Z@�I�@�I�@�(�@��@�ƨ@��@�C�@�33@�o@���@���@��@���@��T@�x�@�x�@�x�@�`B@�7L@���@��@�K�@�+@�C�@�S�@�\)@�S�@�"�@���@�@�
=@��\@�v�@�V@��7@�%@���@��@�9X@��@�1@�@~��@}`B@}p�@|�@{��@z��@zn�@z=q@y�@y��@y�^@y��@y�7@yX@y&�@xĜ@x�u@xr�@xA�@w�@w�@w�P@w|�@wK�@w�@v��@v�y@v��@vE�@v{@u�T@u�@t��@t��@tz�@tj@t1@sƨ@s��@s��@s�@r�H@q��@p�`@pĜ@p�@pQ�@pQ�@p1'@p1'@p1'@p1'@p1'@p1'@o�@o�;@o�P@o;d@n�@mp�@l�j@l�@k�F@kC�@j�H@j~�@j�@i�@i��@i��@iX@i&�@h��@h�9@h��@h��@h��@h�9@hr�@hb@g�;@g��@f��@fv�@e��@d��@d��@d�D@d�@e�@d�j@dZ@dj@c�F@c"�@b�\@b-@bJ@b�@a�#@ahs@`�`@`��@`�@`�@`�@`�@`A�@_+@]�@]p�@]�@\j@\9X@\Z@\9X@[dZ@[o@Y��@Y��@Y�7@X�`@XQ�@W�@W;d@V��@Vv�@V{@U�-@U`B@U`B@U`B@U�@U�@T�/@TZ@S�@R�!@Rn�@R^5@R=q@R-@RJ@Q�@Q��@Q�#@Q�^@Qhs@P��@PQ�@P1'@Pb@O��@O
=@N��@N�+@Nv�@Nff@N$�@N$�@M��@M�@M�@L�j@Lz�@LZ@K��@K�F@K��@K��@K�@KdZ@K"�@J~�@J�@I�#@I��@Ihs@IG�@I�@HĜ@H�u@HQ�@H1'@G|�@G;d@G;d@G
=@Fȴ@F�+@E�@E/@D��@Dz�@DI�@D(�@D1@C�m@Cƨ@C��@C��@CdZ@B�@B�!@B�\@B-@BJ@BJ@A��@A�#@A��@A�7@AG�@A�@@��@@1'@?��@?\)@>��@>�@>v�@>{@=p�@<j@<1@;�
@;��@;C�@;o@:�@:~�@9��@9X@9%@8b@7�@7�w@7��@7|�@7K�@6��@6ȴ@6�R@6��@6ff@6V@5�@5��@5`B@5/@4�@4�j@4��@4z�@4j@3�m@3�@3t�@3t�@3t�@3t�@3C�@3o@3@3@2��@2��@2M�@1��@1�7@1x�@1&�@0�`@0��@0�9@0�u@0Q�@0 �@0b@/�w@/\)@/;d@/;d@/;d@/�@/
=@.��@.5?@.$�@-��@-��@-O�@,��@,��@,�@,��@,��@,�D@,�D@,�D@,z�@,I�@,9X@,1@+�
@+�F@+��@+��@+dZ@+33@*�H@*�!@*n�@*=q@)�#@)hs@)&�@(��@(�@(A�@(b@(  @'�@'�;@'��@'�w@'�@'�P@'\)@'+@'�@&��@&ȴ@&��@&$�@%�T@%�-@%�h@%p�@%�@$�@$�j@$�j@$z�@$I�@$�@#�m@#ƨ@#��@#�@#dZ@#"�@"�H@"��@"~�@"^5@"�@!hs@!7L@ ��@ ��@ �9@ �u@ Q�@  �@�w@\)@
=@�R@v�@V@5?@��@/@�@I�@9X@9X@1@�m@ƨ@��@t�@dZ@S�@��@�!@�\@^5@M�@M�@J@�#@�^@�^@�7@hs@7L@�9@A�@1'@b@�@�@|�@+@��@�y@E�@�@�@��@O�@��@��@�j@�D@j@Z@1@�m@�
@�
@�F@dZ@"�@��@�!@n�@�@�#@��@x�@hs@G�@%@Ĝ@�u@�@A�@1'@ �@b@  @�@�;@�@�P@K�@�y@v�@$�@@�@�-@�h@p�@O�@/@�@V@�@�/@Z@�@�
@33@
�!@
n�@
=q@
-@
�@
�@	�@	�^@	�7@	hs@	G�@	%@�`@��@�9@�u@�u@�@bN@Q�@�;@�P@|�@|�@|�@\)@
=@�y@ȴ@�R@��@v�@E�@5?@�T@@@�-@�h@`B@�@��@��@Z@9X@9X@9X@(�@1@1@�m@��@��@S�@o@��@�!@�\@~�@n�@^5@M�@=q@-@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BDBDBDBJBJBJBJBDBDBJBDBDBDBDBDBDBDBDBDBDBJBDBDBDBJBJBDBDBDBDB
=B
=B
=B
=B
=B	7B1B1B+B%BBB��B��B�B�B�B�B�B�NB�;B�BĜB��BT�B[#B]/BZBM�B49B
=BB  B�B�)B��BɺB�wB�FB�B��B�7BgmB7LB;dB33B�B�BB
�fB
��B
�B
ȴB
ȴB
ƨB
�LB
��B
��B
��B
��B
��B
�{B
�uB
�VB
�1B
�1B
�B
z�B
t�B
dZB
S�B
<jB
>wB
@�B
1'B
 �B
B	�B
B	��B	�TB	ɺB	�wB	��B	��B	��B	ÖB	��B	�hB	��B	��B	��B	��B	��B	� B	o�B	o�B	L�B	C�B	@�B	E�B	YB	P�B	>wB	�B	,B	0!B	'�B	�B	�B	hB	JB	oB	{B	B��B	B	%B	  B	B	B��B�B�B�B�B�B�sB�ZB�TB�`B�fB�`B�TB�NB�;B�B��B�qB�BȴBɺBǮB�}B�LB�B��B��B��B�B��B��B��B��B�hB�B�PB��B��B��B��B��B��B�{B�JB� Bk�B^5B{�By�Bk�Bm�BcTBT�BffBcTBbNBhsBaHBe`B_;BbNBffBgmBdZB[#BK�BN�B\)BVBK�B<jB49B�B%�B;dB9XB5?B>wBB�BF�BF�BF�BF�BE�BC�B@�B9XB9XB;dB9XB5?B-B49B2-B.B&�BuB�B%�B%�B!�B�B  B	7B�B'�B)�B'�B"�B�B �B�B�B�B�B�BVBuB+B�B�B�BoB+B�B�B�B�B�B�B!�B�B�B�B�B�B!�B$�B$�B!�B �B�B�B�B �B�B�B!�B�B�B�B �B"�B'�B(�B)�B,B,B+B&�B �B�B�B"�B)�B-B+B�B'�B'�B+B/B33B9XB:^B7LB:^B6FB49B0!B8RB5?B;dBC�BH�BF�BH�BJ�BJ�BD�BA�BF�BN�BP�BR�B]/B[#BZB\)BaHBe`BgmBgmBo�Bw�Bx�Bx�B{�B�B�B~�B~�Bz�B�1B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�B��B�B�!B�'B�RB�XB�jB��BB��BǮB��B��B��B��B��B��B�/B�NB�`B�mB�sB�mB�`B�`B�mB�sB�B�B��B	  B	B	B	1B		7B	
=B	
=B	hB	�B	�B	�B	�B	�B	�B	!�B	!�B	#�B	"�B	�B	&�B	'�B	'�B	+B	.B	-B	-B	)�B	+B	+B	.B	33B	49B	49B	5?B	49B	49B	8RB	;dB	;dB	@�B	?}B	;dB	=qB	E�B	G�B	I�B	L�B	M�B	L�B	K�B	I�B	S�B	P�B	M�B	P�B	VB	XB	YB	\)B	\)B	\)B	\)B	\)B	]/B	_;B	bNB	cTB	cTB	dZB	ffB	hsB	iyB	iyB	jB	k�B	l�B	l�B	l�B	o�B	p�B	p�B	p�B	s�B	u�B	u�B	t�B	u�B	x�B	y�B	y�B	w�B	x�B	� B	�+B	�1B	�=B	�JB	�JB	�VB	�\B	�\B	�bB	�bB	�\B	�bB	�VB	�VB	�PB	�=B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�9B	�FB	�?B	�9B	�9B	�FB	�LB	�XB	�qB	�}B	��B	�}B	��B	ÖB	B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	��B	�B	�)B	�5B	�5B	�HB	�TB	�NB	�;B	�NB	�;B	�NB	�NB	�HB	�NB	�TB	�`B	�fB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�sB	�sB	�yB	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
+B
1B
	7B

=B
DB
DB
DB
JB
JB
JB
DB
PB
VB
VB
PB
VB
JB
PB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
"�B
!�B
%�B
%�B
&�B
&�B
%�B
%�B
&�B
'�B
'�B
&�B
&�B
&�B
&�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
(�B
)�B
-B
-B
-B
-B
,B
,B
-B
-B
,B
,B
+B
+B
-B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
/B
/B
1'B
2-B
2-B
1'B
1'B
0!B
/B
2-B
1'B
2-B
2-B
33B
49B
5?B
6FB
6FB
6FB
6FB
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
6FB
6FB
6FB
6FB
6FB
7LB
6FB
6FB
8RB
8RB
:^B
:^B
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
=qB
>wB
?}B
@�B
@�B
?}B
@�B
@�B
A�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
A�B
D�B
D�B
E�B
F�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
F�B
G�B
I�B
G�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
J�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
O�B
O�B
O�B
O�B
N�B
M�B
N�B
Q�B
Q�B
P�B
P�B
P�B
P�B
Q�B
Q�B
O�B
R�B
S�B
R�B
R�B
S�B
T�B
VB
VB
VB
W
B
VB
W
B
XB
XB
W
B
VB
VB
W
B
XB
XB
XB
YB
YB
ZB
ZB
ZB
YB
YB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
\)B
[#B
ZB
ZB
[#B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
_;B
`BB
^5B
`BB
bNB
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
ffB
ffB
ffB
ffB
gmB
gmB
ffB
ffB
e`B
ffB
hsB
hsB
hsB
hsB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
jB
iyB
jB
k�B
k�B
jB
jB
jB
k�B
jB
jB
l�B
m�B
m�B
l�B
l�B
m�B
l�B
l�B
m�B
l�B
l�B
m�B
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
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BDBDBDBJBJBJBJBDBDBJBDBDBDBDBDBDBDBDBDBDBJBDBDBDBJBJBDBDBDBDB
=B
=B
=B
=B
=B	RBKB1BEB?BGB;B�VB�DB�hB�AB�B��B�B� B��B�sB�fG�O�G�O�B^�B_!B[qBOvB6�B�B�BoB�B�BּB�)B�4B�fB��B�dB�DBkB;�B=<B4�B!�B�BaB
��B
�
B
ںB
�B
��B
ǮB
�$B
��B
��B
��B
��B
��B
��B
�FB
�BB
�B
��B
��B
{�B
u�B
fLB
U�B
?cB
?�B
A�B
2�B
#�B
�B	��B
B	�B	��B	�jB	� B	ϑB	�[B	ΊB	�G�O�B	�aB	��B	�8B	�TB	�OB	��G�O�B	q�B	q�G�O�B	F?B	CGB	GEB	YeB	RoG�O�G�O�B	-�B	0�B	)DB	�B	�B	&B	�B	&B	2B	�B��B	�B	�B	 �B	uB	�B��B��B�B�[B�UB�IB�_B�zB�&B��B��B��B��B��B��B��B�4B�B��B�B�#B�B�iB�lB��B��B�hB��B��B�B��B��B��B��B�SB�<B��B��B��B��B��B��B��B�B��G�O�B`�B|6Bz�Bl�Bn�Bd�BWYBg8Bd�Bc:BiBb4BfB`BBc Bf�Bg�Bd�B\BM�BO�B\�BV�BMB>]B6zB BB(XB<jB:�B6zB?cBB�BF�BF�BF�BF�BE�BC�BAB:xB:B;�B9�B6+B.IB4�B2�B.�B($BB�B&�B&�B"�BeG�O�B
�BqB($B*0B(>B#TB�B!bB�B~B]BOB�B�B�B	lB?BkB]B�B�B�BYBQB)B5B BB"hBpB \B~B�B�B"NB%FB%FB"NB!-B�BWB!B!B;B5B"4BjB�BCB!HB#:B($B)*B*0B,"B,"B+6B'8B!|BB�B#�B*B-wB+�B!B(�B(�B+�B0!B3�B9�B:�B7�B:�B7B5?B1[B9	B6`B<jBD3BIBGEBI7BK)BK)BE�BB�BG�BO�BQ�BS�B]~B[�BZ�B\�Ba�Be�Bh$Bh>Bp!BxBy>ByXB|PB�-B�UB}B}B|B��B��B��B��B��B��B��B��B��B��B��B�B��B�9B�EB�'B�&B�2B�FB�fB�mB�]B�;B�wB��B��B��B��B��B��B��B��B��B�B�B��B�B�B�B� B�2B�/B�NB�`B�mB�sB�B�B�B�B��B��B�B��B	 B	AB	9B	KB		RB	
XB	
�B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	$B	# B	 BB	'B	($B	(>B	+6B	.B	-CB	-]B	*B	+�B	+kB	./B	3MB	49B	4TB	5ZB	4TB	4nB	8RB	;B	;�B	@�B	?�B	<B	=�B	E�B	G�B	I�B	MB	M�B	MB	LB	JXB	S�B	Q4B	NVB	Q4B	VB	X+B	Y1B	\CB	\CB	\CB	\CB	\CB	]IB	_pB	bhB	cnB	cnB	d�B	f�B	h�B	i�B	i�B	j�B	k�B	l�B	l�B	l�B	o�B	p�B	p�B	p�B	s�B	u�B	u�B	t�B	u�B	y	B	y�B	zB	x8B	y>B	�OB	�EB	�KB	�rB	�dB	�~B	�VB	�\B	�\B	�bB	�}B	�vB	�}G�O�B	��B	��G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�"B	�B	�5B	�-B	�hB	�nB	�`B	�tB	��B	��B	��B	��B	��B	�qB	�cB	��G�O�B	��B	ðB	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�$B	�+B	�B	�KB	�KG�O�G�O�B	֡B	�]B	�jB	�jB	�|B	�TB	�G�O�B	�G�O�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�qB	�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	��B	�B	��B	�B	�"B	�(B	�B
 B
 B
;B
'B
3B
B
3B
3B
MB
MB
_B
KB
	RB

XB
DB
^B
^B
dB
dB
dB
xB
jB
VB
pB
PB
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �G�O�B
�B
�B
#B
"B
%�B
%�B
'B
'B
%�B
%�B
'B
'�B
'�B
'B
'B
'B
'B
)*B
)B
)B
*0B
*B
)�B
*B
)DB
*B
-B
-B
-B
-B
,"B
,"B
,�B
-B
,"B
,"B
+6B
+B
-)B
./B
.B
./B
/OB
/5B
/5B
/5B
/5B
0;B
/5B
/OB
1'B
2-B
2-B
1AB
1AB
0UB
/OB
2B
1AB
2aB
2aB
3MB
4TB
5tB
6`B
6FB
6FB
6FB
5?B
5ZB
5ZB
5ZB
6zB
6`B
7fB
7LB
7LB
6zB
6zB
6`B
6`B
6`B
7fB
6�B
6zB
8lB
8lB
:xB
:xB
<�B
=qB
=qB
=qB
=qB
=qB
=qB
=�B
=�B
=�B
>wB
>�B
>�B
>�B
=�B
>wB
?�B
@�B
@�B
?�B
@�B
@�B
A�B
@�B
A�B
A�B
B�B
B�B
B�B
C{B
B�B
B�B
B�B
C�B
C�B
C�B
B�B
A�B
D�B
D�B
E�B
F�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�G�O�B
G�B
I�G�O�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
J�B
M�B
M�B
M�B
N�B
N�B
M�B
N�B
O�B
O�B
O�B
O�B
OB
NB
N�B
Q�B
RB
Q B
P�B
Q B
Q B
R B
R G�O�B
SB
S�B
SB
SB
TB
UB
VB
V9B
V9B
V�B
VB
W
B
XB
XB
W$B
VB
VB
W$B
X+B
X+B
X+B
Y1B
Y1B
Z7B
Z7B
Z7B
Y1B
YKB
Z7B
[#B
[WB
\)B
\B
\)B
\)B
\)B
\)B
[=B
\]B
[=B
ZQB
ZQB
[=B
]/B
]IB
]IB
^OB
^5B
_VB
_VB
_;B
_;B
_pB
_VB
^jB
_VB
`\G�O�B
`vB
b�B
cnB
dZB
dtB
dZB
dtB
dtB
ezB
ezB
ezB
e�B
f�B
f�B
f�B
f�B
gRB
g�B
f�B
ffB
e�B
f�B
hXB
hsB
hsB
h�B
g�B
h�B
iyB
iyB
iyB
i�B
i�B
jB
i�B
j�B
k�B
k�B
j�B
j�B
j�B
k�B
j�B
j�B
l�B
m�B
m�B
l�B
l�B
m�B
l�B
l�B
m�B
l�B
l�B
m�B
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
r�11111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111141111114114111114411111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114114111111111111111111111111111111411111111111111114411111114141111111111111114111111111111111111111111111111111111111111111111111111141111111111111111111111114111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411411111111111111111111111111111111411111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802140033222018021400332220180214003322201806221237312018062212373120180622123731201804050434162018040504341620180405043416  JA  ARFMdecpA19c                                                                20180210063530  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180209213545  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180209213550  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180209213550  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180209213553  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180209213553  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180209213555  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180209213555  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180209213601  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180209213603                      G�O�G�O�G�O�                JA  ARUP                                                                        20180209221002                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180210153157  CV  JULD            G�O�G�O�F�Y�                JM  ARSQJMQC2.0                                                                 20180213000000  CF  PSAL_ADJUSTED_QCB�  D� G�O�                JM  ARCAJMQC2.0                                                                 20180213153322  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180213153322  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193416  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033731  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                