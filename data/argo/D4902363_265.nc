CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-31T00:35:24Z creation;2018-07-31T00:35:28Z conversion to V3.1;2019-12-19T07:36:14Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180731003524  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              	A   JA  I2_0576_265                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�u�$h� 1   @�u���b�@9�:��S�dZ
�L/�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@���A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCG�fCJ  CL  CN  CP  CR  CT  CV�CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�  D�@ D؀ D�� D�  D�<�Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@�z�@��A�
AAp�A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE��CG��CI�qCK�qCM�qCO�qCQ�qCS�qCV
CX
CY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\Dx�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ��D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�<{D�|{D׿�D���D�?�D��Dؿ�D���D�<{D��Dٿ�D��{D�<{D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D��{D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D�l{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AжFAд9AХ�AЍPAЅAЇ+AЅAЃAЁAЃA�z�A�jA��A���AǴ9A���A���A�7LA��A���A��A��A�O�A�O�A��mA���A�bA�?}A�$�A��jA�7LA��A��A�G�A��A��A�bA�|�A��A���A��A�A�A��+A�?}A�1A��TA��wA�=qA��TA�bNA��A�ffA���A�v�A�l�A�x�A��A�A�ĜA�7LA��A�A���A��7A�C�A�ffA�x�A���A�K�A���A��A��/A�dZA�{A�p�A��\A��DA���A��A��DA��A��9A��A��
A���A�Q�A�9XA�A�
A}oAz�Ax�`Av  As��ArffAq��Ap�9ApbNAo��Ao"�AnbAmt�AkG�Ai�
Ag�^AfbNAe��Adv�Abv�AaƨA`�RA_/A^��A^9XA]�hA\�\AZ�yAZ��AZbNAZ-AXVAW&�AV��AUƨATv�ARr�AQG�AO?}AM&�AL�RAKG�AJ�!AJbNAJ  AI|�AH��AHjAGK�AF�RAFQ�AEoAC��AC�AB��AB�jAA�wA@ĜA?��A?&�A>��A=��A<��A<�\A;�hA:��A:��A:ffA:$�A9S�A8��A8z�A7�hA6��A5�wA4�uA3�A3+A2�A1ƨA/�TA.Q�A-|�A,��A,�!A,ZA,5?A+�mA+��A+7LA*ȴA*�RA)�A(�A(5?A'A&��A&v�A&E�A&�A& �A&bA%��A$��A#�A#l�A"��A!�-A!`BA v�A�;A�#A�FAK�A��AE�A$�A�AoA��A7LA�AA(�AK�A&�A�A�RAA�AS�A�RA�-AM�A�A1'A�Al�A�uA��A%A
JA	G�A��A�TAS�A�\A��AVAVA?}A|�A �u@���@��!@�J@���@���@�&�@��P@�{@��/@�5?@�F@�5?@�&�@�1@�|�@��y@�@��@�G�@�@�z�@�w@�ff@���@��
@޸R@�ff@�J@�@�/@ۮ@�{@���@�hs@�C�@��@љ�@�V@�Z@��m@�\)@Ώ\@�1@��@���@�9X@�ȴ@ŉ7@�1'@�\)@�^5@��7@�hs@��@�{@���@�hs@�G�@�&�@���@�9X@�33@���@�5?@���@� �@��
@��@��-@��D@�l�@�K�@�\)@�dZ@�t�@�t�@�l�@���@��/@���@�;d@�V@��@��@�X@�V@�z�@��\@��^@���@���@�`B@��/@�I�@�1@�l�@�;d@�^5@��h@��@��;@�dZ@�ȴ@�ff@�5?@���@�O�@��@�j@��F@�K�@�
=@���@�{@��#@��@���@��u@�Q�@�b@�dZ@���@���@�E�@���@�p�@�V@��j@�r�@�\)@��@���@�^5@���@�x�@�%@� �@�1@��
@�dZ@��@�V@��-@��`@�Q�@�1'@�1@��@��@���@���@��@�V@��9@�1@��w@�K�@�@���@���@�E�@��@��7@�X@�/@�V@��@��D@�b@���@�\)@�"�@��H@��!@��+@�ff@�=q@�{@��#@��-@���@�G�@���@�A�@�9X@�1'@��;@��P@�dZ@�C�@�o@��@���@�ff@�=q@�J@���@��^@���@�x�@��`@�r�@�bN@�Z@�9X@��@�  @�w@l�@�@�@~��@~�R@~$�@}�T@}@}p�@|�/@|Z@|9X@{��@{ƨ@{�F@{��@{dZ@z�H@z�\@z=q@zJ@y��@yX@y�@xr�@xb@w�;@w��@w��@w�;@w��@w�@w|�@w;d@v�@v5?@u�T@u��@u`B@u�@t�@tZ@t(�@sƨ@s��@s@r��@r~�@r^5@r�@q�@qx�@q�@p��@pr�@p1'@o��@o;d@n�R@m�T@l�@l�@lj@l1@kdZ@kt�@kS�@kS�@kC�@k"�@j�@j�!@j��@jn�@jM�@i�#@i�@h�u@hbN@hb@g�;@g��@g�@g�P@g\)@g+@f�y@fV@e@e�@d��@dZ@dI�@d1@cƨ@ct�@c"�@b�@b��@bM�@b-@b-@a��@a��@a7L@a�@`��@`bN@_��@_+@^ff@]�@]�-@]��@]�@\��@\�/@\��@\�j@\�@\(�@[�F@[��@["�@Z�@Z�H@Z��@Z~�@Z-@Y�@Y�@Y��@YX@Y7L@X�`@XbN@W�;@W\)@WK�@W+@V��@Vv�@V5?@V{@U�@U�h@U/@T�j@T�D@T9X@S��@S�m@S�F@St�@S33@R�H@R=q@RJ@Q��@Q�^@Q��@QG�@Q%@P��@P��@P �@O�w@O\)@O+@N�y@Nȴ@Nȴ@N��@N$�@M�@M�T@M@M�-@M`B@MV@L��@LI�@Kƨ@KS�@Ko@K@J��@Jn�@JM�@I��@I�#@I�7@Ix�@IG�@I7L@I�@H�`@H�9@H�@HQ�@G�;@GK�@Fȴ@F5?@F{@F@E�@E@E��@EV@Dj@D(�@C�m@C�@CdZ@CS�@Co@B��@B~�@BJ@A��@A7L@@�9@@A�@?��@>�R@>5?@=��@=�h@=?}@<�/@<(�@;�F@;C�@:��@:��@:^5@:=q@:�@9��@9x�@97L@9&�@9%@9%@8��@8��@7��@7�P@7\)@7;d@7+@7
=@6ȴ@6$�@5O�@5�@4��@4��@4�@4�D@4j@4I�@41@3��@2�@2��@2~�@2^5@1�^@1�7@1x�@1&�@0�9@01'@/�@/\)@/;d@.�y@.$�@-�@-/@,�@,�j@,�D@,I�@,�@+�m@+t�@+@*�!@*n�@*�@)�^@)x�@)&�@(��@(Ĝ@(bN@(Q�@'��@'
=@&�@&��@&5?@%�T@%�h@%p�@%?}@$��@$��@$z�@$9X@#�m@#��@#�@#S�@#"�@#o@#o@"�@"��@"~�@"M�@"�@!�#@!�^@!x�@!�@!%@ �`@ Ĝ@ r�@ bN@ 1'@  �@�@�w@��@l�@+@��@�@��@�+@ff@V@5?@�T@�@O�@�j@�@��@z�@j@Z@I�@9X@(�@��@�F@�@dZ@S�@33@"�@@@@�H@�\@^5@-@-@�@�@�^@��@�7@�7@x�@x�@G�@Ĝ@�9@��@�u@Q�@�;@�w@�P@;d@��@v�@E�@5?@{@�@�-@�@`B@/@�@��@�/@�@��@j@�@��@�
@�F@dZ@C�@@�H@��@=q@�@�@�7@7L@&�@��@��@Ĝ@�@  @��@|�@�@
=@
=@�@ȴ@��@$�@�T@@�-@��@��@��@�h@/@�@z�@I�@�@�
@��@S�@C�@"�@
�H@
�H@
��@
��@
M�@
=q@
=q@
�@	��@	�@	�@	�#@	�#@	�^@	��@	hs@	7L@	7L@	7L@	�@��@r�@��@�P@l�@;d@�@��@�y@ȴ@�R@��@��@��@��@��@�+@v�@ff@V@$�@�@��@@�h@`B@/@�/@��@�@�D@j@9X@�m@�m@�
@�F@��@��@S�@�H@��@�!@�!@��@~�@~�@n�@M�@=q@-@��@�#@�^@��@��@��@�7@hs@G�@�@ �`@ �9@ �@ Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AжFAд9AХ�AЍPAЅAЇ+AЅAЃAЁAЃA�z�A�jA��A���AǴ9A���A���A�7LA��A���A��A��A�O�A�O�A��mA���A�bA�?}A�$�A��jA�7LA��A��A�G�A��A��A�bA�|�A��A���A��A�A�A��+A�?}A�1A��TA��wA�=qA��TA�bNA��A�ffA���A�v�A�l�A�x�A��A�A�ĜA�7LA��A�A���A��7A�C�A�ffA�x�A���A�K�A���A��A��/A�dZA�{A�p�A��\A��DA���A��A��DA��A��9A��A��
A���A�Q�A�9XA�A�
A}oAz�Ax�`Av  As��ArffAq��Ap�9ApbNAo��Ao"�AnbAmt�AkG�Ai�
Ag�^AfbNAe��Adv�Abv�AaƨA`�RA_/A^��A^9XA]�hA\�\AZ�yAZ��AZbNAZ-AXVAW&�AV��AUƨATv�ARr�AQG�AO?}AM&�AL�RAKG�AJ�!AJbNAJ  AI|�AH��AHjAGK�AF�RAFQ�AEoAC��AC�AB��AB�jAA�wA@ĜA?��A?&�A>��A=��A<��A<�\A;�hA:��A:��A:ffA:$�A9S�A8��A8z�A7�hA6��A5�wA4�uA3�A3+A2�A1ƨA/�TA.Q�A-|�A,��A,�!A,ZA,5?A+�mA+��A+7LA*ȴA*�RA)�A(�A(5?A'A&��A&v�A&E�A&�A& �A&bA%��A$��A#�A#l�A"��A!�-A!`BA v�A�;A�#A�FAK�A��AE�A$�A�AoA��A7LA�AA(�AK�A&�A�A�RAA�AS�A�RA�-AM�A�A1'A�Al�A�uA��A%A
JA	G�A��A�TAS�A�\A��AVAVA?}A|�A �u@���@��!@�J@���@���@�&�@��P@�{@��/@�5?@�F@�5?@�&�@�1@�|�@��y@�@��@�G�@�@�z�@�w@�ff@���@��
@޸R@�ff@�J@�@�/@ۮ@�{@���@�hs@�C�@��@љ�@�V@�Z@��m@�\)@Ώ\@�1@��@���@�9X@�ȴ@ŉ7@�1'@�\)@�^5@��7@�hs@��@�{@���@�hs@�G�@�&�@���@�9X@�33@���@�5?@���@� �@��
@��@��-@��D@�l�@�K�@�\)@�dZ@�t�@�t�@�l�@���@��/@���@�;d@�V@��@��@�X@�V@�z�@��\@��^@���@���@�`B@��/@�I�@�1@�l�@�;d@�^5@��h@��@��;@�dZ@�ȴ@�ff@�5?@���@�O�@��@�j@��F@�K�@�
=@���@�{@��#@��@���@��u@�Q�@�b@�dZ@���@���@�E�@���@�p�@�V@��j@�r�@�\)@��@���@�^5@���@�x�@�%@� �@�1@��
@�dZ@��@�V@��-@��`@�Q�@�1'@�1@��@��@���@���@��@�V@��9@�1@��w@�K�@�@���@���@�E�@��@��7@�X@�/@�V@��@��D@�b@���@�\)@�"�@��H@��!@��+@�ff@�=q@�{@��#@��-@���@�G�@���@�A�@�9X@�1'@��;@��P@�dZ@�C�@�o@��@���@�ff@�=q@�J@���@��^@���@�x�@��`@�r�@�bN@�Z@�9X@��@�  @�w@l�@�@�@~��@~�R@~$�@}�T@}@}p�@|�/@|Z@|9X@{��@{ƨ@{�F@{��@{dZ@z�H@z�\@z=q@zJ@y��@yX@y�@xr�@xb@w�;@w��@w��@w�;@w��@w�@w|�@w;d@v�@v5?@u�T@u��@u`B@u�@t�@tZ@t(�@sƨ@s��@s@r��@r~�@r^5@r�@q�@qx�@q�@p��@pr�@p1'@o��@o;d@n�R@m�T@l�@l�@lj@l1@kdZ@kt�@kS�@kS�@kC�@k"�@j�@j�!@j��@jn�@jM�@i�#@i�@h�u@hbN@hb@g�;@g��@g�@g�P@g\)@g+@f�y@fV@e@e�@d��@dZ@dI�@d1@cƨ@ct�@c"�@b�@b��@bM�@b-@b-@a��@a��@a7L@a�@`��@`bN@_��@_+@^ff@]�@]�-@]��@]�@\��@\�/@\��@\�j@\�@\(�@[�F@[��@["�@Z�@Z�H@Z��@Z~�@Z-@Y�@Y�@Y��@YX@Y7L@X�`@XbN@W�;@W\)@WK�@W+@V��@Vv�@V5?@V{@U�@U�h@U/@T�j@T�D@T9X@S��@S�m@S�F@St�@S33@R�H@R=q@RJ@Q��@Q�^@Q��@QG�@Q%@P��@P��@P �@O�w@O\)@O+@N�y@Nȴ@Nȴ@N��@N$�@M�@M�T@M@M�-@M`B@MV@L��@LI�@Kƨ@KS�@Ko@K@J��@Jn�@JM�@I��@I�#@I�7@Ix�@IG�@I7L@I�@H�`@H�9@H�@HQ�@G�;@GK�@Fȴ@F5?@F{@F@E�@E@E��@EV@Dj@D(�@C�m@C�@CdZ@CS�@Co@B��@B~�@BJ@A��@A7L@@�9@@A�@?��@>�R@>5?@=��@=�h@=?}@<�/@<(�@;�F@;C�@:��@:��@:^5@:=q@:�@9��@9x�@97L@9&�@9%@9%@8��@8��@7��@7�P@7\)@7;d@7+@7
=@6ȴ@6$�@5O�@5�@4��@4��@4�@4�D@4j@4I�@41@3��@2�@2��@2~�@2^5@1�^@1�7@1x�@1&�@0�9@01'@/�@/\)@/;d@.�y@.$�@-�@-/@,�@,�j@,�D@,I�@,�@+�m@+t�@+@*�!@*n�@*�@)�^@)x�@)&�@(��@(Ĝ@(bN@(Q�@'��@'
=@&�@&��@&5?@%�T@%�h@%p�@%?}@$��@$��@$z�@$9X@#�m@#��@#�@#S�@#"�@#o@#o@"�@"��@"~�@"M�@"�@!�#@!�^@!x�@!�@!%@ �`@ Ĝ@ r�@ bN@ 1'@  �@�@�w@��@l�@+@��@�@��@�+@ff@V@5?@�T@�@O�@�j@�@��@z�@j@Z@I�@9X@(�@��@�F@�@dZ@S�@33@"�@@@@�H@�\@^5@-@-@�@�@�^@��@�7@�7@x�@x�@G�@Ĝ@�9@��@�u@Q�@�;@�w@�P@;d@��@v�@E�@5?@{@�@�-@�@`B@/@�@��@�/@�@��@j@�@��@�
@�F@dZ@C�@@�H@��@=q@�@�@�7@7L@&�@��@��@Ĝ@�@  @��@|�@�@
=@
=@�@ȴ@��@$�@�T@@�-@��@��@��@�h@/@�@z�@I�@�@�
@��@S�@C�@"�@
�H@
�H@
��@
��@
M�@
=q@
=q@
�@	��@	�@	�@	�#@	�#@	�^@	��@	hs@	7L@	7L@	7L@	�@��@r�@��@�P@l�@;d@�@��@�y@ȴ@�R@��@��@��@��@��@�+@v�@ff@V@$�@�@��@@�h@`B@/@�/@��@�@�D@j@9X@�m@�m@�
@�F@��@��@S�@�H@��@�!@�!@��@~�@~�@n�@M�@=q@-@��@�#@�^@��@��@��@�7@hs@G�@�@ �`@ �9@ �@ Q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BbB\BhB{B�B�B�B�B�B{BhBB��B��B@�B!�BhsBz�Bm�B��B�B�bB��B�!B�-B�B��B��B��B��B��B��B��B��B��B�{B�%Bx�B|�Bv�B~�Bs�Bs�Bt�Bq�Bm�BbNBXBB�B.B/B$�B#�BuBB1B%B��B�B�)B��B�dB��B~�B}�Br�BffBgmB`BBVBI�BE�B=qB1'B �B\BB
�B
�BB
�qB
ĜB
�qB
��B
�DB
�oB
�\B
�B
k�B
S�B
>wB
0!B
!�B
oB
oB
oB
1B

=B
B	��B	�B	�B	�
B	ȴB	�wB	�jB	�dB	�LB	��B	�'B	�B	��B	��B	��B	��B	��B	�VB	��B	�oB	�PB	~�B	s�B	y�B	o�B	gmB	XB	T�B	E�B	9XB	?}B	5?B	1'B	49B	33B	/B	-B	'�B	!�B	�B	�B	{B	\B	bB	VB	DB	B��B��B��B��B�B�B�B�fB�ZB�fB�ZB�HB�B��B�
B��BĜBĜB�qB�jB�qB�dB�3B��B��B��B��B�B�B�B��B��B��B��B��B��B�oB��B�bB��B��B��B��B��B��B�\B�+B�%B�7B�%By�B�B{�B~�B�B�B|�Bx�By�By�Bu�Bl�BdZBhsBaHB`BB`BB`BBe`BdZBaHB]/BR�BR�BM�BD�BC�BN�BO�BH�B@�B2-B9XB;dBB�B@�B@�B=qB;dB:^B8RB33B,B%�B&�B,B,B-B#�B'�B$�B"�B �B �B�BoB�B�B�B �B �B�BoBuB�B�B�B{B{B�B�B�B�B�B�BoBhBuBDB�B#�B(�B+B+B+B)�B$�B�B�B%�B(�B$�B(�B)�B+B+B(�B0!B+B/B8RB;dB;dB:^B8RB49B5?B;dB<jB9XB=qBD�BA�B?}B?}BB�BJ�BM�BM�BM�BL�BK�BG�BD�BK�BP�BR�BYBZBW
BXBVBP�B]/BgmBiyBhsBhsBiyBm�Bm�Bo�Bl�Bk�Bn�Br�Br�Bq�Bs�Bv�Bv�Bu�Bw�By�By�B~�B�B�B�B�7B�=B�7B�PB�bB�bB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�'B�'B�-B�9B�RB�^B��BȴBȴBƨBɺB��B��B�B�B�)B�/B�BB�NB�fB�yB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	%B	+B	1B		7B	
=B	DB	JB	PB	JB	VB	hB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	%�B	%�B	'�B	+B	+B	,B	,B	2-B	8RB	9XB	9XB	:^B	<jB	=qB	=qB	@�B	C�B	C�B	D�B	E�B	J�B	L�B	L�B	L�B	O�B	S�B	T�B	W
B	YB	YB	ZB	ZB	_;B	aHB	bNB	cTB	e`B	gmB	gmB	k�B	l�B	n�B	n�B	n�B	n�B	n�B	n�B	o�B	p�B	q�B	t�B	v�B	x�B	y�B	z�B	{�B	~�B	� B	�B	�B	�1B	�7B	�DB	�JB	�VB	�VB	�hB	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�3B	�9B	�RB	�dB	�jB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	��B	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�B	�B	�#B	�/B	�5B	�5B	�5B	�BB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�fB	�sB	�mB	�sB	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
	7B
1B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
JB
DB
JB
PB
VB
hB
hB
hB
hB
hB
bB
hB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
%�B
$�B
'�B
(�B
(�B
)�B
(�B
(�B
'�B
(�B
,B
-B
-B
-B
-B
-B
-B
-B
,B
-B
/B
/B
0!B
/B
1'B
1'B
0!B
0!B
1'B
1'B
33B
49B
33B
2-B
49B
5?B
6FB
7LB
8RB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
<jB
<jB
?}B
?}B
>wB
?}B
@�B
A�B
A�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
G�B
G�B
H�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
J�B
J�B
J�B
K�B
K�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
O�B
O�B
O�B
P�B
Q�B
Q�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
Q�B
Q�B
S�B
S�B
S�B
R�B
R�B
T�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
YB
XB
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
[#B
\)B
]/B
]/B
^5B
^5B
]/B
]/B
_;B
_;B
_;B
aHB
aHB
aHB
aHB
aHB
`BB
aHB
cTB
cTB
cTB
cTB
cTB
bNB
aHB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
iyB
hsB
hsB
gmB
gmB
ffB
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
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BbBvB�B{B�B�B�B�B�B�BoB�B��B��BH�B-�Bm�B�4Bs�B��B�_B��B��B�B��B��B��B��B��B�B��B��B�fB�zB��B��B��B|PBcBy$B�Bu%BtTBu%BrBn/Bc�BYBE�B0�B0�B&�B$�B�BB�B�B��B�|B�BāB��B��B��B� Bt�Bh>BhsBa�BX+BK�BF�B>wB2�B#B�B+B
�3B
�nB
��B
�mB
��B
�ZB
��B
�B
��B
�{B
m�B
W?B
AUB
2�B
%B
B
�B
uB
	RB

�B
�B	�B	�B	��B	ٚB	��B	��B	��B	��B	��B	�0B	�-B	�wB	��B	��B	��B	��B	��B	�B	��B	�B	�B	� B	u?B	z�B	p�B	iDB	Z�B	V�B	H1B	;�B	@4B	6�B	1�B	4�B	3�B	/�B	-�B	(�B	# B	�B	�B	B	�B	B	B	�B	aB�6B�8B��B��B��B�wB�6B�B�,B��B��B��B�#B��B׍B�B�B��B��B��B�BB�jB��B�RB��B��B��B�qB��B�QB�yB�sB�zB�nB�:B��B��B�yB��B�
B��B��B��B��B��B�.B��B�EB��B��B{�B��B}B�B�3B�oB}�By�Bz^Bz*Bv`Bm�BfBiyBcBa�BabBaHBe�Bd�Ba�B^BTaBTBOvBF�BE�BO\BPbBI�BB'B4�B;JB<�BC�BA�BA�B>]B<�B;dB9�B4nB-�B(
B(>B-B,�B-�B%`B(�B%�B$B!�B!�BQBBBdB~B!HB!bB�BFB�BBB1BgBgB?B1BBB/BQB�B�B{B�B�B$�B)_B+�B+�B+�B*�B%�BpBB&�B)�B%�B)�B*�B+�B+�B)�B0�B,WB0B8�B;�B;�B:�B8�B4�B5�B;�B=B:DB>BEBB[B@iB@OBCGBJ�BM�BM�BM�BMBLBH�BE�BL~BQ�BS�BYKBZkBW�BXyBV�BR:B]�Bg�Bi�Bh�Bh�Bi�Bm�BnBo�BmCBl=BoiBs3BsBr-BtBv�BwBvFBx8BzDBzxBHB�[B�{B��B�lB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�dB�-B�B�2B�`B�0B�_B��B�[B�vB��B��B��B��B��B��B��B��B�B�=B�0B˒B�SB�B�xBݲB��B�B�B��B�B��B��B��B��B��B��B�B�	B�B�6B	 OB	[B	GB	YB	_B	fB		lB	
rB	xB	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#B	%�B	&B	(>B	+B	+QB	,=B	,qB	2|B	8RB	9�B	9rB	:xB	<jB	=�B	=�B	@�B	C�B	C�B	D�B	E�B	J�B	L�B	MB	MB	PB	TB	UB	W
B	YKB	Y1B	Z7B	ZkB	_VB	abB	bhB	c�B	ezB	g�B	g�B	k�B	l�B	n�B	n�B	n�B	n�B	n�B	n�B	o�B	p�B	rB	t�B	v�B	x�B	y�B	z�B	|B	B	�OB	�-B	�MB	�KB	�RB	�^B	�dB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�=B	�B	�/B	�5B	�5B	�;B	�GB	�GB	�MB	�hB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�,B	�2B	�$B	�$B	�?B	�YB	�eB	�KB	�WB	�IB	�jB	�OB	�jB	�\B	�HB	�bB	�bB	�|B	�B	�nB	�B	�ZB	�zB	�zB	�B	�zB	�B	�sB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�"B	�B	�B
 B
 B
B
 B
 4B
;B
B
-B
B
'B
AB
AB
3B
MB
SB
_B
EB
EB
EB
	RB
1B
	RB

rB
DB
^B
^B
^B
xB
^B
dB
dB
xB
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
!�B
!�B
#�B
#�B
$�B
$�B
%B
%�B
%�B
&�B
'B
'B
'B
&B
%B
(
B
)B
)*B
)�B
)B
)*B
(>B
)DB
,"B
-CB
-)B
-CB
-)B
-)B
-)B
-)B
,=B
-CB
/OB
/5B
0;B
/OB
1AB
1AB
0oB
0UB
1AB
1[B
3MB
49B
3hB
2|B
4nB
5ZB
6`B
7fB
8lB
7fB
8�B
8�B
8�B
8�B
9rB
:xB
:xB
:�B
;B
;B
<�B
<�B
<�B
=�B
<�B
<�B
?�B
?�B
>�B
?�B
@�B
A�B
A�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
G�B
G�B
H�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
J�B
J�B
J�B
K�B
K�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
OB
M�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
O�B
O�B
O�B
QB
Q�B
RB
Q B
R B
RB
SB
R�B
R�B
R�B
RB
R B
S�B
S�B
TB
SB
S&B
UB
U2B
U2B
UB
UMB
W$B
W
B
W$B
W?B
W$B
W$B
X+B
X+B
XB
X+B
X+B
X+B
YB
X+B
X+B
Y1B
Y1B
Y1B
Z7B
Z7B
Z7B
[=B
[=B
[=B
\CB
\CB
[WB
\]B
]/B
]IB
^OB
^OB
]IB
]dB
_pB
_pB
_VB
aHB
aHB
abB
abB
aHB
`vB
abB
cTB
cTB
c:B
cTB
cTB
bhB
a|B
bhB
b�B
cnB
cnB
cnB
d�B
d�B
e`B
ezB
e`B
ffB
f�B
ffB
f�B
gmB
gmB
g�B
g�B
hsB
hsB
hsB
hsB
g�B
h�B
h�B
h�B
iyB
hsB
h�B
g�B
g�B
f�B
jB
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
lqB
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
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<B�8<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808040033392018080400333920180804003339201808040200162018080402001620180804020016201808050023102018080500231020180805002310  JA  ARFMdecpA19c                                                                20180731093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180731003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180731003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180731003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180731003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180731003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180731003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180731003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180731003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180731003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20180731005503                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180731153613  CV  JULD            G�O�G�O�Fï�                JM  ARCAJMQC2.0                                                                 20180803153339  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180803153339  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180803170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180804152310  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                