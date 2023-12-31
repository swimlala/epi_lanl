CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-15T00:35:20Z creation;2018-03-15T00:35:24Z conversion to V3.1;2019-12-19T07:43:10Z update;     
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20180315003520  20200116221514  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_220                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�Sv,/ 1   @�Sv��� @4K/�V���d@쿱[W1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  @���AffA>ffA`  A�  A�33A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@��@�z�A=qA>=qA_�
A�
A��A��A��A��A��A��A�RA��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D"�D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D�v111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ƨA���A��`A��A��A���A���A���A���A���A��A���A���A���A���A���A���A��A��mA���A��RA��-A��9A�ĜA���A��/A��mA��HA��
A��#A��#A��
A��#A��A��#A��A���A���A��A��A��A���A�ĜA���A�K�A���A��uA�K�A�%A��-A�ffA�I�A�9XA��A���A�XA�ffA��HA�Q�A�JA�VA�$�A�v�A��uA��9A�&�A���A�dZA�-A��hA�XA�VA�hsA���A���A�z�A��RA�ȴA�A�A���A��jA�VA���A���A�r�A��
A��PA��!A��#A�S�A��PA���A�;dA�
=A�?}A��PA���A��uA��A��FA�M�A��A��\A���A�C�A��A�&�A�ĜA�hsA��7A~A�A|�AzbNAwC�At�+ArM�ApI�Ao|�An~�Am�;Al�9Ak�Ai�TAgVAf1Ae��Ad��AcO�A^�AZbAY�AY�AY�;AYO�AXffAW&�AV{ATI�AS�AP�AM33AJ��AJ1'AI`BAH�AG�AGXAE"�AC�PAB��ABM�AA��AA%A@1'A?G�A>��A>9XA=��A:�A9�PA9�A9t�A9\)A8��A6  A4�A3��A2��A2bNA2JA0�A.�DA-�A,�DA,  A+�hA*��A'l�A&ȴA%XA#��A#\)A"�A"�DA ȴAG�AJA��Al�A�A�A  A�wAp�A�RA��A{A��AjA�AG�A�9A
�A	`BAQ�A1'A-A�#Al�A$�A`BAbA��Ap�AA  �@�C�@�p�@�{@�l�@�@�Q�@�!@��#@�C�@���@�%@�9X@�S�@�bN@�J@噚@��@�I�@�ƨ@�@��#@�V@�
=@�I�@�S�@��@ف@�Z@���@�dZ@֗�@��/@�Z@ӝ�@�M�@�`B@�Z@϶F@Ͳ-@̋D@��@˅@ʇ+@�`B@���@��@ǅ@��H@�p�@�Ĝ@�1@�K�@�5?@�&�@�I�@�K�@���@���@��#@��j@�(�@�l�@�ȴ@�E�@���@���@�
=@�ff@�{@�/@���@��@�Q�@�b@��
@�l�@��+@�5?@�{@�7L@��@��;@�|�@�l�@�K�@��H@�^5@��#@�X@���@�r�@���@�1'@�|�@�C�@��\@�-@�M�@�V@�M�@��h@��@�Z@��D@���@��@� �@���@��w@��P@�dZ@�\)@�\)@�\)@�S�@�33@��H@�v�@�E�@��T@�p�@�/@��`@�b@�t�@�C�@�;d@�33@�"�@��@��@�$�@�-@���@�33@���@�l�@��P@��@�o@��@�ff@�{@��@��^@��-@�x�@�`B@��@��@�j@��@�r�@�r�@�Q�@�I�@�
=@�J@�p�@��@��@��9@���@���@�A�@�  @��
@��w@���@���@�|�@��F@��@��m@��w@�K�@�
=@��@��@�K�@��y@�n�@���@�O�@�?}@�7L@�&�@��@�/@���@���@�V@��@�?}@�G�@�/@��@�A�@� �@��
@��F@�ƨ@��w@�|�@�"�@�
=@��R@���@���@��\@�^5@���@�Ĝ@��D@��u@��u@��@�r�@�Z@�1'@�b@���@�t�@��@��!@�~�@�^5@�=q@�5?@��@��^@���@���@��@�p�@��@��j@�j@�1'@��@���@��m@��
@��@��P@�\)@��H@���@��@��T@��T@���@��h@��7@�x�@��@��9@��@���@��u@�j@��@��
@���@�;d@�"�@�"�@�o@���@��\@�n�@�M�@�-@�@���@�`B@�?}@�V@�%@���@���@���@��j@���@��u@��D@�A�@��@�dZ@�o@��R@��\@���@���@��h@��7@�hs@�G�@�&�@�V@���@���@��j@���@��@�I�@�1@��P@�;d@�;d@�S�@�K�@���@��!@�~�@�n�@�v�@�ff@�ff@�v�@�^5@�-@���@��#@��^@���@���@��h@��@�p�@�`B@�`B@�O�@�?}@�7L@�%@���@�z�@�1'@��@��@��@�@l�@
=@~ȴ@~v�@}��@}�-@}�h@|��@{�m@{t�@{@z^5@y��@y��@yG�@x�`@x�9@x�@xbN@xQ�@xQ�@xQ�@xA�@x1'@xb@w��@w\)@w
=@v�+@u�T@u�-@u��@uO�@t��@t��@t9X@s��@s��@s�
@sdZ@r�!@r^5@r-@q�@qx�@p�`@p�u@p1'@o\)@n�@mp�@mV@l�@l�j@l�@l�D@l�@kƨ@kdZ@k"�@j��@j��@j~�@i��@hĜ@h1'@gK�@fȴ@fV@e�@d9X@c@b~�@a�^@a�@`��@`�u@`r�@`  @_K�@^��@^V@]�-@]/@\j@[�F@[dZ@[C�@[33@["�@["�@["�@Z��@Z=q@Y��@Y��@YG�@XĜ@W��@V�+@V@U�@UO�@T��@T�j@T��@T1@S��@S��@SdZ@S@R-@Q�7@Q�@Q%@P�9@O�P@O;d@N��@N��@NE�@M�@M��@M�@L�D@K�m@KdZ@K"�@K@J�@J�H@J��@J�!@J�!@J��@J�\@J�@Ix�@H�`@H��@Hr�@H  @G�;@G��@G�w@G��@G�w@G�P@G|�@Gl�@G�@Fȴ@Fff@F$�@E�T@E@E�h@E�@E`B@E?}@E/@D��@Dz�@C��@Cƨ@C��@C��@C��@C�@C33@B�\@B-@A�^@AX@A7L@A&�@@�9@@Q�@@1'@?�@?l�@>ȴ@>5?@=�@=�@<�@<��@<j@<Z@<j@<9X@<(�@;�F@;dZ@;C�@;o@:�H@:�\@9�@9x�@9hs@9G�@97L@9&�@9&�@9%@8��@8�9@8��@8��@8��@8��@8r�@8Q�@8 �@7\)@6ȴ@5�-@5�@4�@4j@49X@4�@41@3�
@3ƨ@3�F@3��@3t�@3t�@3dZ@333@3"�@3"�@3o@2�H@2~�@2M�@2=q@1�@1�^@1x�@1&�@0�u@0bN@0A�@0  @/�@/��@/|�@/|�@/K�@.�y@.��@-�T@-/@-V@,�@,�j@,z�@,9X@,(�@,�@,1@+ƨ@+��@+S�@+"�@+"�@*�H@*��@*~�@*n�@*n�@*^5@*M�@*-@)�^@)X@)%@(��@(�u@(bN@(b@'�w@'��@'|�@'K�@';d@'+@&ȴ@&V@&E�@&5?@&{@&{@&@%��@%p�@%/@%V@$��@$�@$Z@#��@#t�@#C�@#33@#o@"��@"��@"��@"�!@"�!@"��@"�\@"n�@"=q@"�@"J@!��@!��@!�7@!7L@!&�@ ��@ �9@ r�@ Q�@�;@��@�P@�P@��@�P@�P@K�@;d@�@�@��@�+@V@E�@5?@$�@��@��@�h@�h@�@`B@?}@V@�@z�@�m@dZ@S�@S�@C�@"�@o@��@��@~�@^5@=q@�@�@��@��@X@7L@�`@�@Q�@1'@  @�@�;@�w@|�@;d@+@
=@�@ff@{@�@�T@��@p�@�@�j@��@j@�@�m@�F@��@�@33@��@n�@^5@��@X@7L@&�@�@%@��@Ĝ@��@A�@  @�@�;@�@��@l�@l�@\)@K�@+@
=@
=@��@ȴ@��@��@�+@ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ƨA���A��`A��A��A���A���A���A���A���A��A���A���A���A���A���A���A��A��mA���A��RA��-A��9A�ĜA���A��/A��mA��HA��
A��#A��#A��
A��#A��A��#A��A���A���A��A��A��A���A�ĜA���A�K�A���A��uA�K�A�%A��-A�ffA�I�A�9XA��A���A�XA�ffA��HA�Q�A�JA�VA�$�A�v�A��uA��9A�&�A���A�dZA�-A��hA�XA�VA�hsA���A���A�z�A��RA�ȴA�A�A���A��jA�VA���A���A�r�A��
A��PA��!A��#A�S�A��PA���A�;dA�
=A�?}A��PA���A��uA��A��FA�M�A��A��\A���A�C�A��A�&�A�ĜA�hsA��7A~A�A|�AzbNAwC�At�+ArM�ApI�Ao|�An~�Am�;Al�9Ak�Ai�TAgVAf1Ae��Ad��AcO�A^�AZbAY�AY�AY�;AYO�AXffAW&�AV{ATI�AS�AP�AM33AJ��AJ1'AI`BAH�AG�AGXAE"�AC�PAB��ABM�AA��AA%A@1'A?G�A>��A>9XA=��A:�A9�PA9�A9t�A9\)A8��A6  A4�A3��A2��A2bNA2JA0�A.�DA-�A,�DA,  A+�hA*��A'l�A&ȴA%XA#��A#\)A"�A"�DA ȴAG�AJA��Al�A�A�A  A�wAp�A�RA��A{A��AjA�AG�A�9A
�A	`BAQ�A1'A-A�#Al�A$�A`BAbA��Ap�AA  �@�C�@�p�@�{@�l�@�@�Q�@�!@��#@�C�@���@�%@�9X@�S�@�bN@�J@噚@��@�I�@�ƨ@�@��#@�V@�
=@�I�@�S�@��@ف@�Z@���@�dZ@֗�@��/@�Z@ӝ�@�M�@�`B@�Z@϶F@Ͳ-@̋D@��@˅@ʇ+@�`B@���@��@ǅ@��H@�p�@�Ĝ@�1@�K�@�5?@�&�@�I�@�K�@���@���@��#@��j@�(�@�l�@�ȴ@�E�@���@���@�
=@�ff@�{@�/@���@��@�Q�@�b@��
@�l�@��+@�5?@�{@�7L@��@��;@�|�@�l�@�K�@��H@�^5@��#@�X@���@�r�@���@�1'@�|�@�C�@��\@�-@�M�@�V@�M�@��h@��@�Z@��D@���@��@� �@���@��w@��P@�dZ@�\)@�\)@�\)@�S�@�33@��H@�v�@�E�@��T@�p�@�/@��`@�b@�t�@�C�@�;d@�33@�"�@��@��@�$�@�-@���@�33@���@�l�@��P@��@�o@��@�ff@�{@��@��^@��-@�x�@�`B@��@��@�j@��@�r�@�r�@�Q�@�I�@�
=@�J@�p�@��@��@��9@���@���@�A�@�  @��
@��w@���@���@�|�@��F@��@��m@��w@�K�@�
=@��@��@�K�@��y@�n�@���@�O�@�?}@�7L@�&�@��@�/@���@���@�V@��@�?}@�G�@�/@��@�A�@� �@��
@��F@�ƨ@��w@�|�@�"�@�
=@��R@���@���@��\@�^5@���@�Ĝ@��D@��u@��u@��@�r�@�Z@�1'@�b@���@�t�@��@��!@�~�@�^5@�=q@�5?@��@��^@���@���@��@�p�@��@��j@�j@�1'@��@���@��m@��
@��@��P@�\)@��H@���@��@��T@��T@���@��h@��7@�x�@��@��9@��@���@��u@�j@��@��
@���@�;d@�"�@�"�@�o@���@��\@�n�@�M�@�-@�@���@�`B@�?}@�V@�%@���@���@���@��j@���@��u@��D@�A�@��@�dZ@�o@��R@��\@���@���@��h@��7@�hs@�G�@�&�@�V@���@���@��j@���@��@�I�@�1@��P@�;d@�;d@�S�@�K�@���@��!@�~�@�n�@�v�@�ff@�ff@�v�@�^5@�-@���@��#@��^@���@���@��h@��@�p�@�`B@�`B@�O�@�?}@�7L@�%@���@�z�@�1'@��@��@��@�@l�@
=@~ȴ@~v�@}��@}�-@}�h@|��@{�m@{t�@{@z^5@y��@y��@yG�@x�`@x�9@x�@xbN@xQ�@xQ�@xQ�@xA�@x1'@xb@w��@w\)@w
=@v�+@u�T@u�-@u��@uO�@t��@t��@t9X@s��@s��@s�
@sdZ@r�!@r^5@r-@q�@qx�@p�`@p�u@p1'@o\)@n�@mp�@mV@l�@l�j@l�@l�D@l�@kƨ@kdZ@k"�@j��@j��@j~�@i��@hĜ@h1'@gK�@fȴ@fV@e�@d9X@c@b~�@a�^@a�@`��@`�u@`r�@`  @_K�@^��@^V@]�-@]/@\j@[�F@[dZ@[C�@[33@["�@["�@["�@Z��@Z=q@Y��@Y��@YG�@XĜ@W��@V�+@V@U�@UO�@T��@T�j@T��@T1@S��@S��@SdZ@S@R-@Q�7@Q�@Q%@P�9@O�P@O;d@N��@N��@NE�@M�@M��@M�@L�D@K�m@KdZ@K"�@K@J�@J�H@J��@J�!@J�!@J��@J�\@J�@Ix�@H�`@H��@Hr�@H  @G�;@G��@G�w@G��@G�w@G�P@G|�@Gl�@G�@Fȴ@Fff@F$�@E�T@E@E�h@E�@E`B@E?}@E/@D��@Dz�@C��@Cƨ@C��@C��@C��@C�@C33@B�\@B-@A�^@AX@A7L@A&�@@�9@@Q�@@1'@?�@?l�@>ȴ@>5?@=�@=�@<�@<��@<j@<Z@<j@<9X@<(�@;�F@;dZ@;C�@;o@:�H@:�\@9�@9x�@9hs@9G�@97L@9&�@9&�@9%@8��@8�9@8��@8��@8��@8��@8r�@8Q�@8 �@7\)@6ȴ@5�-@5�@4�@4j@49X@4�@41@3�
@3ƨ@3�F@3��@3t�@3t�@3dZ@333@3"�@3"�@3o@2�H@2~�@2M�@2=q@1�@1�^@1x�@1&�@0�u@0bN@0A�@0  @/�@/��@/|�@/|�@/K�@.�y@.��@-�T@-/@-V@,�@,�j@,z�@,9X@,(�@,�@,1@+ƨ@+��@+S�@+"�@+"�@*�H@*��@*~�@*n�@*n�@*^5@*M�@*-@)�^@)X@)%@(��@(�u@(bN@(b@'�w@'��@'|�@'K�@';d@'+@&ȴ@&V@&E�@&5?@&{@&{@&@%��@%p�@%/@%V@$��@$�@$Z@#��@#t�@#C�@#33@#o@"��@"��@"��@"�!@"�!@"��@"�\@"n�@"=q@"�@"J@!��@!��@!�7@!7L@!&�@ ��@ �9@ r�@ Q�@�;@��@�P@�P@��@�P@�P@K�@;d@�@�@��@�+@V@E�@5?@$�@��@��@�h@�h@�@`B@?}@V@�@z�@�m@dZ@S�@S�@C�@"�@o@��@��@~�@^5@=q@�@�@��@��@X@7L@�`@�@Q�@1'@  @�@�;@�w@|�@;d@+@
=@�@ff@{@�@�T@��@p�@�@�j@��@j@�@�m@�F@��@�@33@��@n�@^5@��@X@7L@&�@�@%@��@Ĝ@��@A�@  @�@�;@�@��@l�@l�@\)@K�@+@
=@
=@��@ȴ@��@��@�+@ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�=B
��B
�9B
��B
�)B
�HB
�TB
�mB
�B
��BJB
=B	7B1BPB\B\B�B/BJ�Bm�B{�B�B�oB��B��B��B�B�B�!B�'B�-B�9B�?B�?B�FB�FB�RB�RB�RB�LB�FB�?B�dB��B�mBBhB �B5?BG�BJ�BF�BH�BH�B<jB>wBT�BW
BG�B2-B �B.B	7B��BȴB�`B�B��B�B�fB�NB�}B�yB�B�/B��B��B�3B��B��B��B��B�bB�Bl�BcTBS�B<jB!�BVBB
��B
�B
�B
��B
�B
��B
ƨB
�wB
�B
��B
�=B
k�B
E�B
>wB
J�B
B�B
0!B
�B
�B	��B	�yB	�B	��B	ǮB	��B	��B	�}B	�!B	��B	��B	�=B	��B	��B	�1B	r�B	O�B	0!B	k�B	jB	ffB	\)B	Q�B	G�B	=qB	,B	#�B	1B�B��B	DB	
=B	B	  B��B�ZB�BB�`B�sB�HB�/B��B��B��B��BÖB��B��BȴBǮB��B�LB��B��B��B��B��B��B��B�1B�JB��B��B��B�DBq�B�\B�1B�+B�{B�hB�PBz�Bm�BjBZBO�BVBk�BffBp�Bn�BgmB_;BT�BK�BW
B`BBe`BaHBR�BS�BYBffBffB_;BXBI�B7LBO�B^5B_;B\)BT�BXBR�BH�BK�BYBR�BW
BXBO�BVBYBW
BS�BI�BM�B_;B_;B\)B^5B\)BXBXBR�BO�B_;BffB`BBdZBk�Bl�BjBffBp�Bo�Bl�Bo�Bp�Bt�Bm�Bz�B�B�B�B�B�1B�+B�7B�7B�%B�PB�bB�bB�hB�oB��B��B��B��B��B��B�B�B�!B�?B�?B�RB�FBŢB��B��B�B�B�B�#B�/B�/B�/B�fB�sB�`B�yB�B��B��B��B��B��B	B	B	+B	\B	�B	�B	�B	�B	�B	!�B	(�B	,B	+B	&�B	'�B	2-B	6FB	9XB	8RB	:^B	@�B	A�B	C�B	F�B	I�B	J�B	J�B	J�B	J�B	K�B	N�B	R�B	S�B	VB	YB	YB	W
B	\)B	dZB	k�B	n�B	q�B	r�B	q�B	o�B	w�B	|�B	�B	�B	�B	�%B	�%B	�B	�DB	�DB	�VB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�9B	�9B	�3B	�-B	�9B	�^B	�qB	�qB	�jB	�dB	�RB	�RB	�jB	�qB	�qB	�wB	�}B	�}B	B	ÖB	ĜB	ǮB	ǮB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	��B	��B	�
B	�
B	�B	�B	��B	��B	��B	��B	�
B	�
B	�B	�)B	�5B	�5B	�BB	�BB	�5B	�HB	�TB	�TB	�TB	�ZB	�ZB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
%B
%B
%B
%B
+B
%B
1B

=B
	7B
JB
JB
JB
DB
JB
JB
PB
JB

=B
1B
DB
DB
JB
JB

=B
\B
hB
hB
bB
bB
bB
hB
bB
bB
hB
bB
bB
\B
VB
VB
\B
{B
{B
uB
bB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
!�B
!�B
!�B
!�B
 �B
�B
�B
 �B
 �B
!�B
#�B
$�B
#�B
#�B
#�B
#�B
%�B
%�B
$�B
#�B
"�B
$�B
%�B
$�B
#�B
#�B
$�B
$�B
"�B
#�B
!�B
'�B
)�B
)�B
+B
)�B
(�B
)�B
)�B
+B
)�B
+B
)�B
'�B
&�B
'�B
'�B
(�B
(�B
%�B
(�B
'�B
,B
,B
-B
/B
1'B
0!B
/B
.B
/B
1'B
/B
0!B
0!B
1'B
49B
5?B
6FB
6FB
6FB
5?B
49B
49B
5?B
5?B
49B
2-B
1'B
2-B
6FB
7LB
8RB
8RB
8RB
9XB
8RB
8RB
:^B
9XB
8RB
7LB
9XB
:^B
<jB
;dB
9XB
=qB
=qB
=qB
=qB
>wB
=qB
>wB
<jB
<jB
>wB
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
@�B
>wB
>wB
?}B
B�B
A�B
A�B
C�B
D�B
D�B
D�B
C�B
C�B
C�B
C�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
C�B
E�B
F�B
G�B
G�B
H�B
G�B
F�B
E�B
G�B
H�B
H�B
J�B
J�B
I�B
I�B
J�B
H�B
J�B
H�B
H�B
I�B
K�B
L�B
M�B
M�B
N�B
N�B
M�B
M�B
L�B
M�B
N�B
N�B
N�B
M�B
L�B
N�B
P�B
P�B
P�B
P�B
Q�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
O�B
O�B
M�B
M�B
L�B
P�B
R�B
R�B
T�B
T�B
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
VB
VB
VB
T�B
VB
W
B
T�B
VB
VB
VB
T�B
W
B
XB
XB
XB
YB
YB
YB
YB
XB
XB
XB
XB
\)B
[#B
[#B
[#B
[#B
]/B
]/B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
`BB
_;B
_;B
bNB
aHB
bNB
bNB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
aHB
aHB
bNB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
e`B
ffB
ffB
ffB
e`B
ffB
hsB
hsB
hsB
hsB
hsB
gmB
hsB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
hsB
hsB
iyB
jB
jB
iyB
iyB
iyB
iyB
iyB
gmB
gmB
iyB
k�B
k�B
k�B
k�B
k�B
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
k�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
n�B
p�B
p�B
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
p�B
q�B
r�B
p�B
r�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
w�B
w�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
�B
̳B
�B
�HB
�TB
�mB
�B
��B0B
=B	7B1BPB\B�B�B/OBJ�Bm]B{�B��B�TB�B��B��B�B�B�!B�'B�-B�9B�?B�?B�FB�FB�RB�RB�RB�fB�zB��B�6BбB�>B�BB!|B5�BG�BKBGzBI�BI�B>�BAoBVBX+BJXB7fB&�B2|B\BؓB͟B�B�'B��B�[B��B��B��B�0B�/B�B�BοB��B��B��B�hB�
B��B��BoiBe�BVmB@4B&BTB�B
��B
��B
�B
�\B
ٚB
��B
ǮB
��B
��B
�WB
��B
o�B
K)B
AUB
K�B
C�B
2�B
�B
B
[B	�)B	�=B	ևB	�	B	͹B	��B	��B	��B	��B	��B	�B	��B	�?B	��B	utB	U2B	5ZB	kB	j�B	f�B	]B	S@B	IRB	?B	.IB	%�B	B�B	oB	0B	DB	3B	B�*B�B�4B�fB�B�NB�B�9B�BѷB��B��B��B� B��B��B�'B��B��B�KB�mB�B��B��B�eB�)B�B�qB�yB�mB�Bu?B�bB�#B�B�B�:B�VB}<Bo�Bl�B]IBR�BXEBl=Bg�BqBoOBh�BaBWYBN�BX�BabBfBb�BUgBU�BZ7Bf�Bf�B_�BY1BK�B:�BQNB^�B_�B\�BVSBX�BT�BJ�BM�BZBTFBXEBX�BQ�BV�BY�BW�BT�BK�BOBB_�B_�B\�B^�B\�BX�BX�BT{BQ�B_�Bf�BabBe,Bk�BmBk6Bg�BqBpUBm�BpoBqvButBo B{�B�uB��B��B��B��B��B��B��B�EB��B� B�B�:B�@B�7B�dB�:B�8B��B��B��B��B��B��B��B�	B��B�B�"B�jB�9B�_B�QB�qB�~BݘB��B�B��B�2B�B�!B�B�B�B�dB�]B	oB	�B	�B	�B	�B	B	
B	�B	;B	!�B	(�B	,B	+QB	'�B	(�B	2GB	6FB	9rB	8�B	:xB	@�B	A�B	C�B	F�B	I�B	J�B	J�B	J�B	J�B	LB	O(B	S&B	TFB	VSB	YKB	YeB	W�B	\�B	dtB	k�B	n�B	q�B	r�B	q�B	p!B	w�B	|�B	��B	��B	�AB	�?B	�?B	��B	�xB	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	��B	�QB	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�"B	�B	�)B	�B	�B	�TB	�hB	�|B	�TB	�*B	��B	��B	��B	��B	��B	��B	�jB	�qB	��B	�wB	�}B	��B	B	ÖB	ĜB	ǔB	��B	��B	�B	�B	�B	��B	��B	��B	��B	�(B	�B	��B	�B	��B	��B	��B	�B	�_B	�JB	�&B	�
B	�
B	�B	�B	�B	�2B	�2B	�FB	�?B	�YB	�QB	�]B	�OB	�OB	�\B	�\B	ޞB	�bB	�nB	�nB	�nB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�$B	��B	�(B	�B	�<B	�<B	�(B
 4B	�.B
;B
B
-B
AB
?B
?B
?B
?B
zB
tB
fB

XB
	lB
JB
dB
dB
^B
~B
dB
jB
dB

�B
�B
xB
�B
�B
~B

�B
vB
�B
�B
bB
�B
}B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
!�B
!�B
!�B
!�B
 �B
�B
�B
 �B
 �B
"B
$B
$�B
#�B
#�B
$B
$B
%�B
%�B
$�B
$B
# B
$�B
%�B
$�B
$B
#�B
%B
%B
# B
$B
"NB
(
B
)�B
)�B
+B
*B
)*B
*B
*0B
+B
*0B
+B
*0B
(>B
'8B
($B
(>B
)*B
)DB
&LB
)_B
(XB
,=B
,=B
-]B
/OB
1'B
0;B
/OB
.cB
/OB
1[B
/OB
0UB
0oB
1[B
4TB
5tB
6FB
6FB
6FB
5ZB
4nB
4nB
5ZB
5ZB
4�B
2|B
1�B
2�B
6zB
7�B
8�B
8�B
8lB
9rB
8�B
8�B
:xB
9�B
8�B
7�B
9�B
:�B
<�B
;�B
9�B
=�B
=�B
=�B
=�B
>�B
=�B
>�B
<�B
<�B
>�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
@�B
>�B
>�B
?�B
B�B
A�B
A�B
C�B
D�B
D�B
D�B
C�B
C�B
C�B
C�B
B�B
B�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
C�B
E�B
F�B
G�B
G�B
H�B
G�B
F�B
E�B
G�B
H�B
H�B
J�B
J�B
I�B
I�B
J�B
H�B
J�B
IB
H�B
I�B
K�B
L�B
NB
M�B
N�B
N�B
NB
NB
MB
M�B
N�B
N�B
N�B
NB
MB
N�B
P�B
P�B
P�B
P�B
RB
QB
Q B
R B
Q�B
Q�B
Q�B
Q�B
Q B
O�B
O�B
N"B
N"B
M6B
QB
SB
S&B
U2B
U2B
UB
UB
VB
VB
VB
VB
U�B
VB
VB
V�B
VB
VB
VB
U2B
VB
W
B
UB
VB
VB
VB
U2B
W$B
XEB
X+B
XEB
YB
Y1B
X�B
Y1B
XEB
XEB
XyB
XyB
\B
[=B
[=B
[=B
[=B
]IB
]/B
\CB
\CB
\CB
\CB
]IB
]/B
]dB
]IB
^OB
^5B
^5B
^5B
]IB
]IB
\]B
]IB
]IB
^OB
^OB
^jB
^jB
_VB
`\B
`\B
`\B
`\B
`\B
_pB
_pB
bNB
abB
bNB
bNB
abB
abB
abB
abB
b�B
bhB
bhB
abB
a|B
b�B
cnB
dZB
dtB
dtB
ezB
e`B
e`B
ezB
e`B
e`B
dtB
dtB
ezB
ezB
ezB
e�B
ezB
ezB
f�B
ezB
f�B
f�B
f�B
e�B
f�B
hsB
hsB
hsB
hsB
hXB
g�B
hsB
g�B
g�B
g�B
hsB
h�B
iyB
iyB
h�B
h�B
iyB
jB
jB
i�B
i�B
i�B
i�B
i�B
g�B
g�B
i�B
kkB
k�B
k�B
k�B
k�B
j�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
k�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
n�B
n�B
p�B
p�B
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
p�B
q�B
r�B
p�B
r�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
w�B
w�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803190036282018031900362820180319003628201806221327382018062213273820180622132738201804050731332018040507313320180405073133  JA  ARFMdecpA19c                                                                20180315093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180315003520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180315003523  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180315003523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180315003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180315003524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180315003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180315003524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180315003524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180315003524                      G�O�G�O�G�O�                JA  ARUP                                                                        20180315005543                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180315153404  CV  JULD            G�O�G�O�F�                JM  ARGQJMQC2.0                                                                 20180315153404  CV  JULD_LOCATION   G�O�G�O�F�                JM  ARGQJMQC2.0                                                                 20180315153404  CV  LATITUDE        G�O�G�O�A�n�                JM  ARGQJMQC2.0                                                                 20180315153404  CV  LONGITUDE       G�O�G�O��"�                JM  ARCAJMQC2.0                                                                 20180318153628  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180318153628  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404223133  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042738  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221514                      G�O�G�O�G�O�                