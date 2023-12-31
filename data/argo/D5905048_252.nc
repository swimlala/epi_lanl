CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-19T00:35:14Z creation;2018-06-19T00:35:19Z conversion to V3.1;2019-12-19T07:35:34Z update;     
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
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180619003514  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_252                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�kvOW�1   @�kw�l @4@��4n�dQ��!�.1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�C3Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�{@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B@\)BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�B�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�<{D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�oAӾwAӋDA�|�A�x�A�hsA�O�A�-A�`BA�G�AΕ�AͶFA�r�A̡�A�A��AǾwA���A�jA�C�A�ZA�%AìA��A�bNA��9A��A�/A��^A���A� �A���A�hsA�p�A��A�=qA��A���A�n�A�1A��A���A�^5A��
A�hsA�O�A���A���A�1A�\)A���A���A��/A��A�|�A�=qA�A�A�9XA��\A�|�A���A�O�A��DA���A�~�A�=qA���A�r�A���A��`A��A���A�A�A��A�%A��\A�K�A�$�A��-A�JA�oA�K�A�
=A��
A��!A��A��7A���A��A���A�  A��-A��-A��A�"�A�|�A�
=Al�A{|�Aw�Au
=Ar^5Ao�-An��Am`BAlĜAk�AiXAf1Ad��Ad(�Aa�FA`bA^ffAZ��AX �AV��AUoAS�AR�!AQG�AN��AL�AK�AK/AJ�DAI��AG�7AD��AC\)AB�9A@�A?�-A>1A<9XA;�A:9XA8��A7�A61'A5G�A4�A4��A3��A1�A/A.�+A.-A-�wA,�/A,1'A+�A+�A*��A*1'A'��A'��A'�A&�+A&-A%x�A#"�A!�;A!"�A �yA �DA  �A�A��An�A�A+A�-A�uA�A��AA^5AȴAI�A��A7LA��A  AS�A��A�+A�hAbNA��A$�AdZA
�+A
1'A	�A	�A	�A	�A	
=A��A�TAhsA��AZA��A�+A1'A  A�7A~�A9XA��A ��A ��@�|�@�?}@�A�@�@��@�9X@��P@�t�@���@�/@��@�33@�@��m@�J@�7L@�P@��/@��
@��@�K�@��@�E�@�-@㕁@���@���@߅@�ȴ@�ȴ@���@ܛ�@�/@��@���@��@�ƨ@���@��#@�x�@�(�@Ώ\@���@�Ĝ@��;@�@ɡ�@ȓu@���@�t�@Ə\@�  @�
=@�^5@�E�@�J@�=q@���@�(�@��@�ff@�=q@�-@�J@��@�(�@�t�@�S�@�33@�"�@��@�~�@�x�@��@�b@�dZ@��R@�@��@�A�@�"�@�V@���@��h@�O�@��@���@���@��@��@��
@�;d@���@�~�@��@��@�7L@��`@��D@�b@���@��@�K�@�@���@�n�@�E�@�-@���@���@�G�@�V@���@���@��@�Q�@�I�@���@���@�;d@��@��@��@���@��R@���@�v�@�@�X@���@�Ĝ@�9X@��
@���@��P@�t�@�K�@��y@���@��+@�5?@��@��#@��T@��^@�hs@�?}@�&�@���@��u@��m@���@�dZ@��H@��+@�ff@�n�@��@��7@��7@�X@��@�j@�9X@�9X@��
@�1@�I�@�Q�@�1@��
@�S�@�~�@�{@�@�@�`B@��@��@��@���@�\)@�;d@��@�ȴ@�@���@��-@���@��@��@�r�@�Q�@� �@���@���@�|�@�S�@�o@��H@��\@�=q@�$�@���@���@�`B@�/@�G�@��^@���@�Z@��;@��F@�t�@�;d@�C�@�K�@�dZ@�"�@��y@���@�^5@�5?@���@���@�x�@�`B@�&�@��`@���@��`@��/@�z�@�Q�@�Z@�1'@�1@��m@��
@���@�t�@�\)@�;d@�"�@��H@��R@��R@�~�@�M�@�=q@�=q@�V@�5?@���@�G�@��@��@�/@�7L@��`@��D@�(�@�b@�1'@�A�@�(�@��m@��@��\@��R@��@��R@�5?@��-@�X@��@��@�&�@���@���@��D@��u@�Z@��F@�dZ@�S�@�;d@�o@��H@��!@���@���@���@�M�@�E�@��@��#@��7@�O�@�&�@���@��@���@��`@�Z@�(�@�1'@��@�9X@�@\)@�;@�@~�@~��@~E�@}�h@}p�@|��@|1@{�
@{�m@|z�@{ƨ@{33@{33@z�!@zn�@zM�@z�@zJ@yX@xĜ@xQ�@xb@w�@v��@vv�@vV@v5?@u�@u/@t��@t�@t(�@sƨ@s�F@so@r�\@q�@qX@p�9@o�@o�w@o;d@o
=@nȴ@n5?@m��@mp�@mV@l�@l�@k��@k33@j��@jn�@j=q@i��@iG�@hĜ@h1'@g�w@g�P@g�@e�T@e�@dz�@c��@co@bJ@a�7@a�@`Q�@_�w@_;d@_�@^V@]�-@]`B@]V@\�/@\��@\�j@\�j@\��@\�D@\j@\�@[�m@["�@Z-@Y�^@Yx�@YG�@Y7L@Y%@XĜ@Xr�@X �@W�;@W��@W+@V��@V5?@V5?@V{@U��@U��@U`B@U?}@UV@T9X@S�F@S33@R��@R��@RM�@RJ@Q�@Q�^@QG�@Q�@P��@PA�@O��@N�y@N��@NE�@N5?@N{@M�T@M@M�h@MO�@MV@L�/@Lz�@L(�@K��@K@J�H@J�\@JM�@I��@I7L@H��@H��@H�@HA�@H  @G�P@GK�@F�y@F�+@Fff@FE�@F$�@E��@E��@E�h@E�@EO�@E�@D�@D�@D�@C�m@CC�@B�@B��@B�\@B�@A��@A�#@A�^@AX@@��@@Ĝ@@��@@�u@@�u@@�@@bN@@  @?��@?K�@?
=@>�+@>@=��@=�-@=��@=�h@=`B@=O�@=/@<��@<�/@<��@<��@<z�@<(�@;�
@;��@;��@;S�@:�@:�H@:�!@:^5@:-@:�@9�^@9�7@9�@8��@8 �@7�w@7K�@6�@6��@6��@6�+@5�@5��@5V@4z�@4(�@3�m@3�F@3��@3dZ@3o@2�\@2�@2J@1��@1��@1X@1�@1�@0��@0��@0��@0r�@0r�@0A�@0  @/��@/�P@/+@.ȴ@.��@.E�@-��@-��@-�@-?}@,�j@,�D@,Z@,Z@,I�@,(�@+�@+C�@+@*�!@*��@*~�@*^5@)�#@)�^@)��@)hs@)&�@)%@(��@(b@'�@'��@'�@'\)@&��@&�R@&V@&$�@&{@%�T@%@%�-@%��@%�h@%p�@%?}@%V@$��@$�@$�j@$�D@$(�@#�
@#t�@#33@#@"�!@"~�@"M�@"J@!��@!x�@!X@!�@ ��@ Q�@  �@�;@��@�@|�@;d@+@��@�y@�y@ȴ@�+@ff@V@$�@�@��@��@p�@O�@?}@/@�@�@��@z�@Z@(�@�m@ƨ@�F@�@dZ@33@@�H@��@��@^5@�@J@��@��@��@X@X@G�@�@�`@��@��@r�@bN@1'@�;@l�@��@��@5?@@��@��@�@`B@O�@V@�/@��@j@9X@��@��@�@t�@dZ@C�@@�H@�!@n�@M�@-@�@��@��@x�@X@&�@��@��@��@�9@�@bN@ �@  @�;@�w@��@|�@\)@;d@�@��@�y@�@��@ff@E�@5?@{@�T@�-@��@�@p�@`B@O�@/@��@�/@��@�j@�@�D@j@�@�m@�
@�F@��@�@S�@C�@C�@33@@
��@
~�@
n�@
M�@
=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�oAӾwAӋDA�|�A�x�A�hsA�O�A�-A�`BA�G�AΕ�AͶFA�r�A̡�A�A��AǾwA���A�jA�C�A�ZA�%AìA��A�bNA��9A��A�/A��^A���A� �A���A�hsA�p�A��A�=qA��A���A�n�A�1A��A���A�^5A��
A�hsA�O�A���A���A�1A�\)A���A���A��/A��A�|�A�=qA�A�A�9XA��\A�|�A���A�O�A��DA���A�~�A�=qA���A�r�A���A��`A��A���A�A�A��A�%A��\A�K�A�$�A��-A�JA�oA�K�A�
=A��
A��!A��A��7A���A��A���A�  A��-A��-A��A�"�A�|�A�
=Al�A{|�Aw�Au
=Ar^5Ao�-An��Am`BAlĜAk�AiXAf1Ad��Ad(�Aa�FA`bA^ffAZ��AX �AV��AUoAS�AR�!AQG�AN��AL�AK�AK/AJ�DAI��AG�7AD��AC\)AB�9A@�A?�-A>1A<9XA;�A:9XA8��A7�A61'A5G�A4�A4��A3��A1�A/A.�+A.-A-�wA,�/A,1'A+�A+�A*��A*1'A'��A'��A'�A&�+A&-A%x�A#"�A!�;A!"�A �yA �DA  �A�A��An�A�A+A�-A�uA�A��AA^5AȴAI�A��A7LA��A  AS�A��A�+A�hAbNA��A$�AdZA
�+A
1'A	�A	�A	�A	�A	
=A��A�TAhsA��AZA��A�+A1'A  A�7A~�A9XA��A ��A ��@�|�@�?}@�A�@�@��@�9X@��P@�t�@���@�/@��@�33@�@��m@�J@�7L@�P@��/@��
@��@�K�@��@�E�@�-@㕁@���@���@߅@�ȴ@�ȴ@���@ܛ�@�/@��@���@��@�ƨ@���@��#@�x�@�(�@Ώ\@���@�Ĝ@��;@�@ɡ�@ȓu@���@�t�@Ə\@�  @�
=@�^5@�E�@�J@�=q@���@�(�@��@�ff@�=q@�-@�J@��@�(�@�t�@�S�@�33@�"�@��@�~�@�x�@��@�b@�dZ@��R@�@��@�A�@�"�@�V@���@��h@�O�@��@���@���@��@��@��
@�;d@���@�~�@��@��@�7L@��`@��D@�b@���@��@�K�@�@���@�n�@�E�@�-@���@���@�G�@�V@���@���@��@�Q�@�I�@���@���@�;d@��@��@��@���@��R@���@�v�@�@�X@���@�Ĝ@�9X@��
@���@��P@�t�@�K�@��y@���@��+@�5?@��@��#@��T@��^@�hs@�?}@�&�@���@��u@��m@���@�dZ@��H@��+@�ff@�n�@��@��7@��7@�X@��@�j@�9X@�9X@��
@�1@�I�@�Q�@�1@��
@�S�@�~�@�{@�@�@�`B@��@��@��@���@�\)@�;d@��@�ȴ@�@���@��-@���@��@��@�r�@�Q�@� �@���@���@�|�@�S�@�o@��H@��\@�=q@�$�@���@���@�`B@�/@�G�@��^@���@�Z@��;@��F@�t�@�;d@�C�@�K�@�dZ@�"�@��y@���@�^5@�5?@���@���@�x�@�`B@�&�@��`@���@��`@��/@�z�@�Q�@�Z@�1'@�1@��m@��
@���@�t�@�\)@�;d@�"�@��H@��R@��R@�~�@�M�@�=q@�=q@�V@�5?@���@�G�@��@��@�/@�7L@��`@��D@�(�@�b@�1'@�A�@�(�@��m@��@��\@��R@��@��R@�5?@��-@�X@��@��@�&�@���@���@��D@��u@�Z@��F@�dZ@�S�@�;d@�o@��H@��!@���@���@���@�M�@�E�@��@��#@��7@�O�@�&�@���@��@���G�O�@�Z@�(�@�1'G�O�G�O�@�@\)G�O�G�O�@~�@~��G�O�@}�hG�O�@|��@|1@{�
@{�mG�O�@{ƨ@{33@{33@z�!@zn�@zM�@z�G�O�@yX@xĜ@xQ�@xb@w�@v��@vv�@vV@v5?G�O�@u/@t��@t�@t(�@sƨ@s�F@so@r�\@q�@qX@p�9@o�@o�w@o;d@o
=@nȴ@n5?@m��@mp�@mV@l�@l�@k��@k33@j��@jn�@j=q@i��@iG�@hĜ@h1'@g�w@g�P@g�@e�T@e�@dz�@c��@co@bJ@a�7@a�@`Q�@_�w@_;d@_�@^V@]�-@]`B@]V@\�/@\��@\�j@\�j@\��@\�D@\j@\�@[�m@["�@Z-@Y�^@Yx�@YG�@Y7L@Y%@XĜ@Xr�@X �@W�;@W��@W+@V��@V5?@V5?@V{@U��@U��@U`B@U?}G�O�@T9X@S�F@S33@R��@R��@RM�@RJ@Q�@Q�^@QG�@Q�@P��@PA�@O��@N�y@N��@NE�@N5?@N{@M�T@M@M�h@MO�@MV@L�/@Lz�@L(�@K��@K@J�H@J�\@JM�@I��@I7L@H��@H��@H�@HA�@H  @G�P@GK�@F�y@F�+@Fff@FE�@F$�@E��@E��@E�h@E�@EO�@E�@D�@D�@D�@C�m@CC�@B�@B��@B�\@B�@A��@A�#@A�^@AX@@��@@Ĝ@@��@@�u@@�u@@�@@bN@@  @?��@?K�@?
=@>�+@>@=��@=�-@=��@=�h@=`B@=O�@=/@<��@<�/@<��@<��@<z�@<(�@;�
@;��@;��@;S�@:�@:�H@:�!@:^5@:-@:�@9�^@9�7@9�@8��@8 �@7�w@7K�@6�@6��@6��G�O�@5�@5��@5V@4z�@4(�@3�m@3�F@3��@3dZ@3o@2�\@2�@2J@1��@1��@1X@1�@1�@0��@0��@0��@0r�@0r�@0A�@0  @/��@/�P@/+@.ȴ@.��@.E�@-��@-��@-�@-?}@,�j@,�D@,Z@,Z@,I�@,(�@+�@+C�@+@*�!@*��@*~�@*^5@)�#@)�^@)��@)hs@)&�@)%@(��@(b@'�@'��@'�@'\)@&��@&�R@&V@&$�@&{@%�T@%@%�-@%��@%�h@%p�@%?}@%V@$��@$�@$�j@$�D@$(�@#�
@#t�@#33@#@"�!@"~�@"M�@"J@!��@!x�@!X@!�@ ��@ Q�@  �@�;@��@�@|�@;d@+@��@�y@�y@ȴ@�+@ff@V@$�@�@��@��@p�@O�@?}@/@�@�@��@z�@Z@(�@�m@ƨ@�F@�@dZ@33@@�H@��@��@^5@�@J@��@��@��@X@X@G�@�@�`@��@��@r�@bN@1'@�;@l�@��@��@5?@@��@��@�@`B@O�@V@�/@��@j@9X@��@��@�@t�@dZ@C�@@�H@�!@n�@M�@-@�@��@��@x�@X@&�@��@��@��@�9@�@bN@ �@  @�;@�w@��@|�@\)@;d@�@��@�y@�@��@ff@E�@5?@{@�T@�-@��@�@p�@`B@O�@/@��@�/@��@�j@�@�D@j@�@�m@�
@�F@��@�@S�@C�@C�@33@@
��@
~�@
n�@
M�@
=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411144114411414111141111111411111111141111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
+B
-B
1'B
1'B
0!B
,B
�B
+B	��B

=B
VB
I�B
ZB
�7B
�BJBdZB�+B�\B�LB�B�#B�/B�;B�B�BBhBuB�BM�BXBQ�B[#Bt�Bw�Bv�Bk�B[#Bl�Bm�Bn�Bq�Bu�B�B�B�Bv�Bw�Bw�BiyBm�Bp�Bx�B{�Bw�BffB_;B]/BM�B49B6FB@�B:^B-B�B��B��B�5B�`B��BĜB��B��B�JBk�B`BBp�BgmBK�B>wB+BoB
�sB
�sB
�B
��B
�wB
�3B
�=B
�B
w�B
iyB
e`B
R�B
G�B
:^B
�B	�B	�B	��B	ƨB	�LB	ĜB	�LB	�?B	��B	�PB	y�B	� B	y�B	`BB	S�B	H�B	!�B	%�B	%�B	�B	�B	\B	B�B�B��B	B��B�B�NB��B�B�BB��B��BȴB�dBÖB�XB�B�3B�B�B�B��B��B�VB|�B��B��B��B�bB�hB�\B�oB�VB�DBu�B�VB�DB�%B�Bx�Be`Bo�Bw�B� Bz�By�Bt�Bo�Bw�Bq�BffB\)BdZBgmBdZBQ�BXBT�BhsBhsBk�BiyBgmBgmBffBhsB^5BYBR�BcTBdZBbNBhsBhsBl�BhsBq�Bq�Bk�BffBjBjBhsBgmBcTBn�Bn�Bk�BcTBhsBdZB]/BdZB]/B\)BdZBcTBdZB`BBe`BjBcTB_;BbNBgmBaHBaHBbNBgmBdZB_;Bo�Bw�Bu�Bs�Bs�Bq�BjBjBr�Bp�Bu�B{�Bs�Bm�BffBgmBx�By�Bv�Bw�B{�B~�Bz�By�B�B�B�%B�+B�1B�JB�{B��B�uB�JB��B��B��B�B�B��B�'B�B�-B�XB�dB�jB�^B�FB�qBĜBŢBƨBŢBŢBBƨB��B��B��B��B�B�/B�BB�yB�B��B��B��B��B��B	  B	B	%B	B	1B	JB	PB	hB	�B	�B	�B	�B	�B	 �B	 �B	!�B	$�B	%�B	)�B	-B	1'B	49B	6FB	:^B	<jB	=qB	=qB	?}B	A�B	@�B	A�B	C�B	G�B	J�B	M�B	O�B	P�B	P�B	P�B	P�B	R�B	[#B	]/B	]/B	bNB	gmB	iyB	jB	jB	k�B	o�B	o�B	r�B	t�B	x�B	{�B	{�B	|�B	�B	�B	�B	�B	�B	�1B	�DB	�=B	�VB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�3B	�!B	�3B	�'B	�dB	�jB	�wB	��B	��B	�}B	�}B	ÖB	��B	ɺB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�)B	�5B	�B	�B	�#B	�5B	�5B	�;B	�NB	�ZB	�`B	�TB	�TB	�TB	�`B	�fB	�`B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
1B
	7B
1B
%B
B
B

=B
DB
	7B
+B
B
B
+B
	7B
DB

=B

=B
	7B

=B
	7B
+B
	7B
DB
DB

=B
DB
JB
PB
VB
VB
PB
PB
PB
JB
JB
JB
PB
VB
\B
bB
bB
VB
bB
uB
{B
uB
bB
�B
�B
�B
{B
{B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
 �B
!�B
 �B
 �B
!�B
 �B
 �B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
%�B
$�B
%�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
+B
)�B
(�B
)�B
)�B
+B
+B
)�B
'�B
)�B
+B
+B
+B
+B
-B
.B
-B
.B
/B
0!B
/B
.B
0!B
1'B
2-B
2-B
33B
33B
33B
33B
33B
2-B
2-B
1'B
1'B
33B
5?B
6FB
6FB
6FB
5?B
6FB
6FB
6FB
6FB
7LB
6FB
7LB
9XB
8RB
8RB
8RB
8RB
9XB
8RB
6FB
8RB
9XB
9XB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
:^B
:^B
:^B
<jB
<jB
>wB
>wB
>wB
>wB
?}B
>wB
>wB
>wB
>wB
=qB
>wB
=qB
@�B
@�B
@�B
?}B
?}B
@�B
A�B
A�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
C�B
B�B
C�B
C�B
D�B
E�B
F�B
E�B
F�B
F�B
F�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
F�B
F�B
F�B
F�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
O�B
Q�B
P�B
N�B
O�B
P�B
P�B
R�B
R�B
S�B
S�B
R�B
R�B
R�B
S�B
T�B
T�B
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
YB
YB
YB
XB
ZB
ZB
[#B
[#B
ZB
YB
ZB
[#B
[#B
\)B
\)B
\)B
[#B
\)B
]/B
\)B
]/B
]/B
\)B
\)B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
`BB
`BB
aHB
`BB
bNB
bNB
bNB
cTB
bNB
cTB
cTB
cTB
dZB
cTB
cTB
dZB
dZB
e`B
ffB
ffB
e`B
ffB
ffB
ffB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
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
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
l�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
w�B
w�B
w�B
x�B
y�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
x�B
x�B
y�B
z�B
z�B
z�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
+�B
-wB
1AB
1[B
0�B
,�B
 \B

�B	�.B
�B
�B
J�B
\]B
��B
�|B Be�B��B��B��B֡B�BޞB��B�B�tB�B�BB�BN�BX�BTB\�Bu%Bx8BwfBmB^Bm�Bo Bo�Br�Bv�B�[B��B��Bx�ByXByXBlBoiBr|By�B|�Bx�Bh�BaB_BP�B8RB8�BB'B;�B.�B�B[B��B��B�mB�{B�zB�kB��B�\BpUBb�Bq'BiBO\B@�B-]B�B
�CB
�B
�}B
�B
� B
�FB
��B
��B
z�B
k�B
gB
U2B
IlB
<B
VB	�[B	�dB	�hB	��B	�B	ŢB	�>B	�`B	�B	��B	}qB	�UB	{dB	c B	VSB	K)B	&fB	(�B	'�B	�B	#B	4B	?B�B��B�VB	�B	 4B�nB�,B�:BڠB�|B�hB�\B��B��BĶB�0B�'B��B��B�"B��B��B�jB��B�iB�/B�IB�QB��B�TB�HB�&B�(B�0BxlB��B�B�B��Bz^Bh>Bq'Bx�B�iB{Bz�Bu�Bp�Bx8Br|Bg�B^5Be�BhXBezBT�BY�BV�Bi*Bi_Bl=BjeBhXBhXBg8Bi*B_�BZ�BUBd&BezBcTBh�Bh�Bl�Bi*Bq�Bq�BlWBgmBk6BkQBiyBh�Bd�BoBoBlWBd�BiBe,B^jBeB^�B]�BeBd@Be,BaHBe�Bj�Bd&B`BBc Bh>Bb�Bb�Bc�Bh>Be�B`�Bp;BxBv+Bt9Bt9Br|Bk�Bk�BshBq�Bv`B|Bt�Bn�Bh�Bh�ByrBzxBw�Bx�B|�B}B{�Bz�B��B��B��B��B�7B�B��B�B�aB�B�OB�@B�B�CB�CB�B��B��B��B�rB��B��B��B�2B��BĶB��B��B��B�B�aB�EB�DB�VB�}BԯBٴB��B�-B��B��B�B�B�B�<B�.B	 iB	9B	YB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	"B	%B	&2B	*0B	-CB	1[B	4�B	6�B	:�B	<�B	=�B	=�B	?�B	A�B	@�B	A�B	C�B	G�B	J�B	M�B	O�B	Q B	Q B	QB	QNB	SuB	[WB	]dB	]�B	b�B	g�B	i�B	j�B	j�B	k�B	o�B	o�B	r�B	uB	x�B	{�B	|B	}"B	� B	�-B	�GB	��B	��B	�fB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	�B	��B	��B	�)B	�WB	�IB	�]B	��B	�iB	�GB	�MB	��B	��B	��B	�B	��B	��B	��B	��B	��B	� B	��B	��B	��B	�+B	��B	��B	��B	�B	�B	�B	�(B	� B	�B	� B	�:B	�@B	�B	�B	�2B	�9B	�1B	�CB	�B	ٴB	�B	�qB	�jB	�jB	�pB	�NB	�ZB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	��B	�"B	�B	�B
  B
  B	�.B	�qB	�HB
;B
B
-B
-B
UB
uB
[B
9B
B
	B
fB
tB
�B
SB

#B
DB
	lB
�B
�B
mB
EB
	7B
DB

rB

XB
	lB

rB
	lB
�B
	lB
^B
^B

�B
xB
dB
PB
pB
pB
�B
jB
�B
~B
~B
~B
jB
pB
vB
}B
�G�O�B
�B
uB
�G�O�G�O�B
�B
�G�O�G�O�B
�B
�G�O�B
�G�O�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
B
�B
�B
 �B
 �B
 �G�O�B
 �B
!�B
 �B
 �B
!�B
!B
 �B
!�B
!�B
# B
$B
$�B
%B
$�B
%�B
%B
&B
'B
'B
($B
($B
)DB
*0B
*0B
*B
+B
*0B
)DB
*0B
*0B
+6B
+B
*KB
(XB
*KB
+6B
+QB
+QB
+kB
-CB
.cB
-]B
.IB
/iB
0;B
/iB
.cB
0;B
1'B
2GB
2-B
33B
33B
3MB
3MB
3MB
2aB
2GB
1vB
1vB
3hB
5ZB
6`B
6`B
6`B
5tB
6`B
6FB
6`B
6`B
7�B
6zB
7fB
9XB
8lB
8lB
8lB
8lB
9rB
8�G�O�B
8�B
9�B
9�B
;B
;B
;B
;�B
;B
;�B
;B
;B
:�B
:�B
:�B
<�B
<�B
>wB
>�B
>�B
>�B
?�B
>�B
>�B
>�B
>�B
=�B
>�B
=�B
@�B
@�B
@�B
?�B
?�B
@�B
A�B
A�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
C�B
B�B
C�B
C�B
D�B
E�B
F�B
E�B
F�B
F�B
F�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
F�B
F�B
F�B
F�B
F�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
LB
LB
K�B
MB
MB
NB
O�B
Q�B
QG�O�B
PB
QB
QB
SB
SB
TB
TB
SB
S&B
S&B
TB
UB
UB
VB
VB
VB
W
B
W$B
W$B
W?B
W?B
W$B
W$B
W$B
W$B
W$B
W?B
W$B
X+B
X+B
X_B
Y1B
Y1B
Y1B
XEB
Z7B
Z7B
[#B
[=B
Z7B
YKB
Z7B
[=B
[=B
\)B
\CB
\CB
[WB
\CB
]/B
\CB
]IB
]/B
\]B
\]B
^OB
^5B
^OB
^OB
^5B
^OB
^jB
_pB
`BB
_VB
`BB
`BB
`'B
`BB
`\B
`\B
`\B
aHB
abB
abB
`\B
`vB
abB
`vB
b�B
bhB
bhB
c�B
bhB
c�B
cnB
cnB
dtB
cnB
c�B
dtB
dtB
e�B
ffB
f�B
ezB
f�B
f�B
f�B
gmB
gmB
g�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
iyB
iyB
iyB
h�B
h�B
h�B
i�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
j�B
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
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
l�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
w�B
w�B
w�B
x�B
y�B
y	B
x�B
y	B
x�B
y�B
y�B
y�B
x�B
x�B
y�B
z�B
z�B
z�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411144114411414111141111111411111111141111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;oG�O�G�O�;o;oG�O�G�O�;o;oG�O�;oG�O�;o;o;o;oG�O�;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�<#�
<#�
G�O�G�O�<#�
<#�
G�O�<#�
G�O�<#�
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
G�O�<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806230038062018062300380620180623003806201806230200222018062302002220180623020022201806240028052018062400280520180624002805  JA  ARFMdecpA19c                                                                20180619093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180619003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180619003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180619003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180619003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180619003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180619003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180619003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180619003518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180619003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20180619005555                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180619153904  CV  JULD            G�O�G�O�F�[�                JM  ARSQJMQC2.0                                                                 20180620000000  CF  PSAL_ADJUSTED_QCCF  D�� G�O�                JM  ARSQJMQC2.0                                                                 20180620000000  CF  TEMP_ADJUSTED_QCCF  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180622153806  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180622153806  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622170022  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180623152805  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                