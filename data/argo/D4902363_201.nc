CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-20T00:35:17Z creation;2018-01-20T00:35:21Z conversion to V3.1;2019-12-19T07:51:22Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180120003517  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_201                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�E�N� 1   @�E�[��@;*�0��dU{J#9�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9y�D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�<�Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D҃3D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D�|�D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�L�D�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��H@��Ap�A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7�\B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D5�D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9x�D9��D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DPx�DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�<{D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D҂�Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D�|{D׿�D��{D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�L{D�b�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�
=A�
=A�
=A�
=A�
=A�
=A�JA�VA�VA�bA�bA�bA�oA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A�VA���A��;A��-A��uA�S�A��;A��FA�ffA�K�A�5?A��A��hA��mA��DA�A�A��HA�{A�33A�&�A���A���A��A���A�&�A�A��mA�hsA���A��A���A�$�A���A�\)A���A�ƨA�^5A�+A�%A�v�A��TA��TA�p�A��;A�`BA���A�hsA���A�A~A{��AydZAw?}Au�-AtE�As�Ar=qAp��AnbNAj�AgS�Ae�-Ae��Ae&�Acp�Aa�A`�A`��A`ȴA`ȴA`ĜA`��A`r�A`�A`  A_x�A^�!A^��A^ffA]�A[��A[��A[��A[p�AZ�HAY�
AY�AXM�AW�-AWS�AW�AV�AUdZATbAR��ARJAQ�FAQ��AP�jAOx�AO�AO%ANȴAN^5AM�mAM�ALjAK;dAJVAI��AIS�AI�AIVAH�`AH��AG�AF��AD�AC�#AC33AB��ABA�AA��A@��A?�hA=K�A;�FA;A:��A:bA9��A9"�A7�A6�A41A2��A1C�A0ĜA0�uA0�+A/��A.��A-��A-S�A,��A,��A+��A*��A* �A)��A)+A(��A(ZA(�A'�^A&�A%"�A$jA#��A#33A!�;A!C�A �uA 1'AC�A��A�A��AO�A�AffA?}A��A^5AA��A�`A?}A��A=qA"�A5?AdZAVA�AoA=qA��A?}A
�DA	��A��AE�A�
A"�A�RA=qA�7A�A�HAz�A�AA�!AffA-A�TA�FA��Ap�A�A V@�
=@���@�9X@��@���@���@�S�@��^@��@�+@��
@�S�@��@��@��-@���@�`B@�@��@���@��@��D@�~�@�7L@�Z@�ȴ@ٙ�@���@ׅ@�S�@��y@�hs@�(�@ҟ�@љ�@��
@Ͳ-@�X@̬@�A�@ˮ@�~�@�@ɡ�@�hs@Ȭ@��@�=q@���@Å@�@��T@�7L@��m@���@�-@�7L@�ff@��D@�ƨ@�t�@�\)@�o@���@��@���@�@��+@�J@�p�@���@�j@��R@���@�n�@�J@�@��@�(�@�  @��R@�v�@���@���@�x�@�?}@�&�@�%@��D@�(�@�C�@���@��y@��!@��H@�"�@��
@�33@�ȴ@�v�@��h@���@���@���@���@���@��@� �@��@�b@�1@��;@��@�5?@���@���@��D@���@�l�@��@�{@���@�p�@�Ĝ@�b@�S�@��@���@���@��-@��h@��@�x�@�O�@��@�Ĝ@��u@�b@�+@��!@�$�@��-@���@��T@���@���@��7@�`B@�O�@�?}@�&�@���@��`@�Ĝ@��9@���@�j@�|�@��!@�=q@�`B@�Ĝ@��u@�r�@�Q�@�(�@� �@�b@�1@�  @���@��@��@�1@��m@��F@�S�@�+@��y@���@�V@�5?@�{@���@�@��-@��h@�%@��/@��/@���@���@�Q�@�  @��m@��;@���@��y@��\@�@�@�7L@���@�9X@;d@~�R@~ff@}��@}/@|�@|�@|�D@|z�@|Z@{�
@{C�@zn�@y�#@yX@x�`@xĜ@x��@xbN@w��@v�@vv�@u�-@t�@t�D@tz�@tz�@tz�@tz�@tI�@s�@s@r~�@r-@q�#@q7L@pQ�@pb@o�;@o�@o��@o�P@o|�@ol�@o\)@o+@n��@n��@n5?@n{@n@m�h@m/@m/@m/@m/@m/@m`B@mp�@mp�@mp�@m�@mp�@m�@l�@l�@l(�@k�m@k��@kƨ@ko@j��@j��@j�!@j�\@j^5@i��@ix�@hĜ@hbN@g��@g+@g�@f��@f�@f�+@f{@e�-@eO�@e/@d��@d1@c�@cS�@b��@b�\@bn�@b=q@a�^@aX@`Ĝ@`�u@`�@`A�@`b@_�;@_��@_K�@^��@^ȴ@^��@^{@]@]`B@]O�@]V@\�@\�/@\��@\��@\j@\9X@\1@[��@[ƨ@[C�@Z��@Zn�@Z=q@Y�^@Y�7@Y�@XĜ@XQ�@Xb@W�@Wl�@W;d@V��@V@U�h@U?}@T��@Tj@TI�@T(�@T1@S�m@Sƨ@S�F@S��@So@R��@R��@R��@R�!@R��@R��@R��@R�\@R�\@RM�@R-@Q�#@Q�#@Q��@Q��@Qx�@P��@Pb@O�@O�;@O;d@N�y@N�R@N��@N�+@Nv�@Nv�@NV@M@M�h@MV@L�D@LZ@K��@K��@K"�@J��@J-@I�^@I�^@I�^@I��@Ihs@H�`@H1'@G�@G�@G�P@G|�@GK�@Fȴ@FE�@E�@EV@D��@Dz�@DI�@C�
@CC�@C@B�\@A��@A7L@@�@@Q�@@1'@?��@?+@>v�@>$�@>{@>@=�T@=`B@<�@;�@;C�@;o@:��@:~�@:=q@9��@9�^@97L@9�@8�`@8Ĝ@8�9@8��@8�u@8�u@8 �@7�P@7l�@7K�@7;d@7�@7
=@6��@6�y@6ȴ@6ff@6{@5�@5@5V@4�j@4�D@4�D@4�D@4��@4�D@4(�@3��@333@3o@3o@3o@2�@2=q@2J@1�^@1��@1X@1G�@1G�@1&�@0�`@0��@0�9@0�9@0r�@01'@/��@/l�@/;d@.��@.ȴ@.E�@-�@-�@-��@-@-�@-/@,�@,��@,j@,(�@+��@+�
@+�F@+��@+��@+dZ@+C�@+"�@*�@*�!@*��@*��@*�\@*=q@)�^@)hs@)G�@)�@(��@(�u@(bN@( �@(  @'�@'�;@'�w@'|�@';d@&��@&ff@&@%��@%/@$��@$�D@$(�@#�m@#��@#t�@#C�@#33@#@"��@"��@"�\@"~�@"^5@"=q@"J@!�#@!��@!��@!G�@!&�@!%@ ��@ ��@ �@ bN@ A�@  �@�;@��@��@|�@�@�@�R@��@�+@V@$�@�-@�@/@��@�@�@�/@��@��@�@I�@��@ƨ@�F@C�@��@~�@-@J@J@��@�@�#@��@X@%@�9@r�@Q�@A�@  @�;@�;@��@��@l�@+@�@�@�@ȴ@��@�+@�+@v�@V@�@�h@/@�j@�j@�@�@��@9X@�@1@��@�m@�m@ƨ@��@t�@C�@�@��@n�@=q@=q@=q@-@�@�#@��@G�@&�@��@��@�9@�9@�9@��@�@  @�w@�@;d@
=@
=@��@��@v�@E�@5?@5?@5?@{@@�@�T@p�@/@V@��@��@�@�/@�/@�/@�j@z�@9X@�@�m@ƨ@�@"�@@
�H@
��@
�!@
�\@
M�@
=q@
J@	�@	x�@	%@��@��@�`@�`@�`@��@�@Q�@bN@bN@Q�@A�@b@��@��@��@�P@|�@|�@l�@l�@|�@l�@l�@�@ȴ@v�@{@�@�@�@��@@�h@`B@?}@�@�@�/@�j@�D@z�@j@I�@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�
=A�
=A�
=A�
=A�
=A�
=A�JA�VA�VA�bA�bA�bA�oA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A�VA���A��;A��-A��uA�S�A��;A��FA�ffA�K�A�5?A��A��hA��mA��DA�A�A��HA�{A�33A�&�A���A���A��A���A�&�A�A��mA�hsA���A��A���A�$�A���A�\)A���A�ƨA�^5A�+A�%A�v�A��TA��TA�p�A��;A�`BA���A�hsA���A�A~A{��AydZAw?}Au�-AtE�As�Ar=qAp��AnbNAj�AgS�Ae�-Ae��Ae&�Acp�Aa�A`�A`��A`ȴA`ȴA`ĜA`��A`r�A`�A`  A_x�A^�!A^��A^ffA]�A[��A[��A[��A[p�AZ�HAY�
AY�AXM�AW�-AWS�AW�AV�AUdZATbAR��ARJAQ�FAQ��AP�jAOx�AO�AO%ANȴAN^5AM�mAM�ALjAK;dAJVAI��AIS�AI�AIVAH�`AH��AG�AF��AD�AC�#AC33AB��ABA�AA��A@��A?�hA=K�A;�FA;A:��A:bA9��A9"�A7�A6�A41A2��A1C�A0ĜA0�uA0�+A/��A.��A-��A-S�A,��A,��A+��A*��A* �A)��A)+A(��A(ZA(�A'�^A&�A%"�A$jA#��A#33A!�;A!C�A �uA 1'AC�A��A�A��AO�A�AffA?}A��A^5AA��A�`A?}A��A=qA"�A5?AdZAVA�AoA=qA��A?}A
�DA	��A��AE�A�
A"�A�RA=qA�7A�A�HAz�A�AA�!AffA-A�TA�FA��Ap�A�A V@�
=@���@�9X@��@���@���@�S�@��^@��@�+@��
@�S�@��@��@��-@���@�`B@�@��@���@��@��D@�~�@�7L@�Z@�ȴ@ٙ�@���@ׅ@�S�@��y@�hs@�(�@ҟ�@љ�@��
@Ͳ-@�X@̬@�A�@ˮ@�~�@�@ɡ�@�hs@Ȭ@��@�=q@���@Å@�@��T@�7L@��m@���@�-@�7L@�ff@��D@�ƨ@�t�@�\)@�o@���@��@���@�@��+@�J@�p�@���@�j@��R@���@�n�@�J@�@��@�(�@�  @��R@�v�@���@���@�x�@�?}@�&�@�%@��D@�(�@�C�@���@��y@��!@��H@�"�@��
@�33@�ȴ@�v�@��h@���@���@���@���@���@��@� �@��@�b@�1@��;@��@�5?@���@���@��D@���@�l�@��@�{@���@�p�@�Ĝ@�b@�S�@��@���@���@��-@��h@��@�x�@�O�@��@�Ĝ@��u@�b@�+@��!@�$�@��-@���@��T@���@���@��7@�`B@�O�@�?}@�&�@���@��`@�Ĝ@��9@���@�j@�|�@��!@�=q@�`B@�Ĝ@��u@�r�@�Q�@�(�@� �@�b@�1@�  @���@��@��@�1@��m@��F@�S�@�+@��y@���@�V@�5?@�{@���@�@��-@��h@�%@��/@��/@���@���@�Q�@�  @��m@��;@���@��y@��\@�@�@�7L@���@�9X@;d@~�R@~ff@}��@}/@|�@|�@|�D@|z�@|Z@{�
@{C�@zn�@y�#@yX@x�`@xĜ@x��@xbN@w��@v�@vv�@u�-@t�@t�D@tz�@tz�@tz�@tz�@tI�@s�@s@r~�@r-@q�#@q7L@pQ�@pb@o�;@o�@o��@o�P@o|�@ol�@o\)@o+@n��@n��@n5?@n{@n@m�h@m/@m/@m/@m/@m/@m`B@mp�@mp�@mp�@m�@mp�@m�@l�@l�@l(�@k�m@k��@kƨ@ko@j��@j��@j�!@j�\@j^5@i��@ix�@hĜ@hbN@g��@g+@g�@f��@f�@f�+@f{@e�-@eO�@e/@d��@d1@c�@cS�@b��@b�\@bn�@b=q@a�^@aX@`Ĝ@`�u@`�@`A�@`b@_�;@_��@_K�@^��@^ȴ@^��@^{@]@]`B@]O�@]V@\�@\�/@\��@\��@\j@\9X@\1@[��@[ƨ@[C�@Z��@Zn�@Z=q@Y�^@Y�7@Y�@XĜ@XQ�@Xb@W�@Wl�@W;d@V��@V@U�h@U?}@T��@Tj@TI�@T(�@T1@S�m@Sƨ@S�F@S��@So@R��@R��@R��@R�!@R��@R��@R��@R�\@R�\@RM�@R-@Q�#@Q�#@Q��@Q��@Qx�@P��@Pb@O�@O�;@O;d@N�y@N�R@N��@N�+@Nv�@Nv�@NV@M@M�h@MV@L�D@LZ@K��@K��@K"�@J��@J-@I�^@I�^@I�^@I��@Ihs@H�`@H1'@G�@G�@G�P@G|�@GK�@Fȴ@FE�@E�@EV@D��@Dz�@DI�@C�
@CC�@C@B�\@A��@A7L@@�@@Q�@@1'@?��@?+@>v�@>$�@>{@>@=�T@=`B@<�@;�@;C�@;o@:��@:~�@:=q@9��@9�^@97L@9�@8�`@8Ĝ@8�9@8��@8�u@8�u@8 �@7�P@7l�@7K�@7;d@7�@7
=@6��@6�y@6ȴ@6ff@6{@5�@5@5V@4�j@4�D@4�D@4�D@4��@4�D@4(�@3��@333@3o@3o@3o@2�@2=q@2J@1�^@1��@1X@1G�@1G�@1&�@0�`@0��@0�9@0�9@0r�@01'@/��@/l�@/;d@.��@.ȴ@.E�@-�@-�@-��@-@-�@-/@,�@,��@,j@,(�@+��@+�
@+�F@+��@+��@+dZ@+C�@+"�@*�@*�!@*��@*��@*�\@*=q@)�^@)hs@)G�@)�@(��@(�u@(bN@( �@(  @'�@'�;@'�w@'|�@';d@&��@&ff@&@%��@%/@$��@$�D@$(�@#�m@#��@#t�@#C�@#33@#@"��@"��@"�\@"~�@"^5@"=q@"J@!�#@!��@!��@!G�@!&�@!%@ ��@ ��@ �@ bN@ A�@  �@�;@��@��@|�@�@�@�R@��@�+@V@$�@�-@�@/@��@�@�@�/@��@��@�@I�@��@ƨ@�F@C�@��@~�@-@J@J@��@�@�#@��@X@%@�9@r�@Q�@A�@  @�;@�;@��@��@l�@+@�@�@�@ȴ@��@�+@�+@v�@V@�@�h@/@�j@�j@�@�@��@9X@�@1@��@�m@�m@ƨ@��@t�@C�@�@��@n�@=q@=q@=q@-@�@�#@��@G�@&�@��@��@�9@�9@�9@��@�@  @�w@�@;d@
=@
=@��@��@v�@E�@5?@5?@5?@{@@�@�T@p�@/@V@��@��@�@�/@�/@�/@�j@z�@9X@�@�m@ƨ@�@"�@@
�H@
��@
�!@
�\@
M�@
=q@
J@	�@	x�@	%@��@��@�`@�`@�`@��@�@Q�@bN@bN@Q�@A�@b@��@��@��@�P@|�@|�@l�@l�@|�@l�@l�@�@ȴ@v�@{@�@�@�@��@@�h@`B@?}@�@�@�/@�j@�D@z�@j@I�@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B"�B"�B"�B"�B"�B"�B"�B#�B"�B"�B"�B#�B"�B"�B#�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B!�B!�B �B�B�B�B�B�B\BhBPBPBDBB��B�B�yB�;BǮB��B[#B$�B��B�)B��BaHB�B�B�B
�B
��B
�B
��B
�B
�B
�sB
�5B
�B
�
B
�B
��B
��B
ĜB
�-B
�-B
�B
��B
��B
��B
�{B
�1B
w�B
e`B
YB
K�B
G�B
=qB
9XB
/B
 �B

=B	�B	�NB	�5B	�mB	�BB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	ǮB	ÖB	�jB	��B	�jB	�FB	�B	�9B	�3B	�B	��B	��B	��B	��B	��B	��B	�{B	�DB	�%B	~�B	x�B	w�B	y�B	v�B	o�B	hsB	k�B	l�B	jB	e`B	aHB	[#B	VB	O�B	K�B	K�B	K�B	K�B	K�B	H�B	D�B	;dB	49B	)�B	)�B	(�B	(�B	$�B	�B	�B	bB	  B	  B	  B	  B��B��B��B�sB�)B��B��B��B��B��B��BǮB�}B�}B��B�}B�qB�?B�B�!B�B�B��B��B��B��B��B�hB�uB�oB�hB�=B�7B�7B�B� Bt�Bu�By�Bw�Bv�Bs�Bk�BdZB^5BiyBffB`BBXB^5BZBR�BQ�BP�BR�BH�BG�BC�BF�BG�BB�B<jB@�B<jB?}B;dB=qB=qB9XB:^B>wB:^B2-B6FB9XB9XB9XB8RB8RB7LB5?B0!B(�B#�B%�B#�B�B$�B'�B%�B"�B �B �B�B-B.B-B%�B�B �B�B �B�B�B�B�B�B�B�B�B�B$�B$�B!�B�B�B�B�B�B�B+B(�B(�B&�B#�B%�B)�B)�B'�B!�B)�B&�B(�B,B/B0!B-B/B33B.B$�B-B9XB=qBE�BD�BC�BL�BL�BH�BT�BW
BT�BL�BG�BC�BM�BM�BM�BO�BM�BR�BYBR�BXBW
BYBYB]/B_;B_;B]/B_;B`BBffBjBl�Bp�B{�B�+B�DB�hB�oB�JB�JB�=B�DB��B��B��B�B�B�B�B��B��B��B��B�?B�LB�FB�jB�}B�qBÖBÖBÖBÖBƨBɺB��B��B��B��B�B�
B�B�
B�B�B�
B�B�B�HB�TB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	%B		7B		7B	
=B	DB	JB	VB	bB	hB	�B	�B	#�B	&�B	&�B	-B	/B	0!B	5?B	49B	49B	49B	;dB	=qB	=qB	<jB	;dB	@�B	D�B	D�B	B�B	@�B	E�B	D�B	F�B	F�B	G�B	F�B	L�B	VB	[#B	^5B	aHB	ffB	gmB	hsB	hsB	hsB	gmB	iyB	jB	m�B	o�B	q�B	s�B	s�B	r�B	r�B	r�B	x�B	y�B	}�B	�B	�%B	�+B	�1B	�1B	�1B	�1B	�DB	�PB	�\B	�\B	�VB	�\B	�oB	�uB	�uB	�{B	�{B	�{B	��B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�!B	�'B	�?B	�FB	�RB	�qB	�}B	�}B	�}B	�}B	�wB	�}B	��B	��B	B	ĜB	ȴB	ȴB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�BB	�HB	�HB	�BB	�HB	�NB	�ZB	�ZB	�`B	�fB	�fB	�mB	�fB	�fB	�mB	�mB	�mB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
1B
+B
1B
1B
	7B

=B
VB
VB
\B
\B
VB
VB
\B
hB
oB
uB
uB
oB
oB
uB
uB
uB
oB
uB
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
$�B
'�B
)�B
,B
-B
-B
-B
,B
,B
.B
0!B
0!B
0!B
/B
.B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
33B
33B
2-B
49B
5?B
5?B
5?B
49B
5?B
7LB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
J�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
P�B
P�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
R�B
S�B
T�B
T�B
T�B
S�B
S�B
S�B
S�B
T�B
W
B
W
B
W
B
VB
VB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
ZB
[#B
ZB
ZB
YB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
[#B
ZB
\)B
]/B
\)B
]/B
^5B
^5B
]/B
_;B
_;B
_;B
`BB
`BB
_;B
_;B
`BB
_;B
^5B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
dZB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
hsB
hsB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
hsB
hsB
gmB
iyB
hsB
iyB
jB
jB
jB
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
l�B
l�B
l�B
m�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B"�B"�B"�B"�B"�B"�B"�B#�B"�B"�B"�B#�B"�B"�B#�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B"�B!�B!�B �B�B�BB�BBbB�B�B�B�B�B��B�B�eB�BB��B��Bc�B+�B�JBߊB�Bj0B!�B�B�B
�$B
��B
��B
��B
��B
�B
�B
�B
��B
��B
ؓB
՛B
�0B
�%B
�nB
�MB
�WB
�B
��B
�5B
�B
�XB
z^B
h>B
[�B
NVB
I�B
?B
:xB
0�B
#B
�B	�-B	�B	��B	��B	�HB	�B	̳B	̘B	�B	�B	��B	� B	�B	�B	�7B	��B	�MB	�<B	��B	�B	��B	��B	�nB	��B	��B	��B	�B	��B	��B	�QB	�B	��B	��B	�EB	��B	zDB	x�B	zDB	wLB	p�B	i�B	k�B	l�B	j�B	e�B	a�B	\CB	W$B	QNB	L�B	L�B	L0B	LB	K�B	IB	E9B	<�B	5�B	,"B	+6B	)�B	)�B	%�B	 �B	�B	oB	�B	�B	 �B	 �B��B��B��B�eBޞB�HBϑB�~B�}B�4B�\B��B��B��B�;B�B�B��B�;B�B��B��B��B�yB�sB��B�/B�uB�{B��B�TB��B�#B�#B�B�oBv�BwLBz�Bx�Bw�Bt�BmCBfLB`BjBg8Ba|BZB^�B[WBT{BS@BRBS�BJ#BIBD�BGzBH�BC�B=�BAUB=qB@4B<jB>BB>BB:DB;0B>�B;B3MB7B9�B9�B9�B8�B8�B7�B5�B0�B*B%B&�B%B�B%�B(�B&�B#�B!�B!�B�B-]B.cB-�B'B~B!�B!B"4B)B!B�B�B�B�B�B �B�B%B%,B"hB�B�B�B�BB!B+kB)yB)_B'�B$�B&LB*eB*eB(�B#B*�B'�B)�B,�B/�B0�B.B0B3�B/5B&�B.IB9�B=�BE�BEBD�BM6BMPBI�BUgBW�BU�BNpBI�BD�BNBN<BN<BPHBN�BS[BYeBS�BXEBWsBYKBYeB]dB_pB_pB]�B_�B`�Bf�Bj�Bl�Bp�B{�B��B��B��B��B�B�B��B�~B�EB��B��B�=B�"B�"B�=B�KB��B��B��B�tB��B��B��B��B�B��B��B�B�3B�+B�#B�"B�VB� B�B�9B�$B�9B�YB�QB�eB׍B֡BڠB�B�B��B��B��B��B��B�B��B��B��B�B�B�B�B�B�$B�tB�`B�PB��B�VB	 4B	 B	'B	AB	3B	?B		7B		RB	
XB	^B	dB	VB	�B	�B	�B	�B	$B	'B	'8B	-)B	/OB	0UB	5ZB	4TB	4nB	4�B	;B	=qB	=�B	<�B	;�B	@�B	D�B	D�B	B�B	AB	E�B	EB	F�B	GB	G�B	G+B	M6B	V9B	[WB	^jB	a�B	f�B	g�B	h�B	h�B	h�B	g�B	i�B	j�B	m�B	o�B	q�B	s�B	s�B	r�B	r�B	sB	y	B	zDB	~BB	�-B	�%B	�+B	�1B	�KB	�fB	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�/B	�;B	�AB	�AB	�UB	�AB	�?B	�`B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�B	�B	�9B	�9B	�_B	�KB	�=B	�CB	�CB	�IB	�IB	�OB	�jB	�\B	�bB	�bB	�\B	�bB	�hB	�tB	�tB	�zB	�B	�B	�mB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�	B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	��B	�B	�6B	�B	�B	�B	�B
  B	�B	�B	�<B	�B	�(B	�.B
 B
;B
UB
;B
AB
GB
9B
+B
1B
EB
KB
�B
	lB

rB
VB
pB
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
#�B
$B
#�B
#�B
#�B
$�B
$�B
$�B
%B
%B
%�B
%�B
%,B
(
B
*B
,"B
-B
-)B
-CB
,=B
,=B
.IB
0;B
0!B
0!B
/5B
.IB
1AB
1AB
2GB
2-B
33B
33B
3MB
3MB
3MB
4TB
49B
3MB
3MB
2aB
4TB
5tB
5tB
5ZB
4nB
5ZB
7LB
6`B
6`B
6`B
6`B
7fB
7fB
8lB
8lB
8lB
9rB
9�B
9rB
:^B
9rB
:xB
:�B
:xB
:xB
;dB
;dB
;B
;�B
;�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
?}B
?}B
?�B
?�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
E�B
F�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
J�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
MB
L�B
M�B
M�B
NB
N"B
N�B
N�B
O�B
P�B
P�B
P�B
O�B
PB
PB
O�B
Q B
P�B
RB
RB
RB
SB
TB
SB
SB
SB
SB
TB
S�B
S�B
SB
TB
T�B
UB
T�B
TB
T,B
TFB
T,B
T�B
W
B
W
B
W$B
V9B
V9B
XB
XB
XB
XB
XB
X+B
X+B
X+B
X+B
X+B
X+B
Y1B
Z7B
[=B
ZB
ZQB
Y1B
ZB
Z7B
Z7B
[=B
[#B
\CB
\CB
\)B
\)B
\)B
[WB
ZQB
\CB
]IB
\xB
]IB
^5B
^OB
]dB
_VB
_VB
_;B
`BB
`BB
_VB
_VB
`BB
_VB
^�B
_VB
`\B
aHB
abB
aHB
aHB
aHB
abB
`\B
`\B
`\B
abB
aHB
aHB
abB
abB
bhB
cnB
cnB
dZB
dtB
cnB
dZB
dZB
dtB
d�B
d�B
gmB
gmB
gmB
gmB
gmB
g�B
f�B
g�B
hsB
h�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
hsB
i_B
i�B
iyB
iyB
hsB
h�B
g�B
iyB
h�B
i�B
jB
jB
jB
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801240035052018012400350520180124003505201806221236322018062212363220180622123632201804050433052018040504330520180405043305  JA  ARFMdecpA19c                                                                20180120093515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180120003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180120003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180120003520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180120003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180120003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180120003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180120003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180120003521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180120003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20180120005449                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180120153411  CV  JULD            G�O�G�O�F�/�                JM  ARCAJMQC2.0                                                                 20180123153505  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180123153505  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193305  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033632  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                