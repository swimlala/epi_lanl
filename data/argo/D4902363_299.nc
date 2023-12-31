CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-10T00:36:14Z creation;2018-11-10T00:36:19Z conversion to V3.1;2019-12-19T07:28:25Z update;     
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
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181110003614  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              +A   JA  I2_0576_299                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؏rc-" 1   @؏sO���@8�ߤ?��d0��4m�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�C3Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�<�Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@,(�@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B�\B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B�.B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6��D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D�|{D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�B�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�<{D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D�{D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D�	H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AΉ7A΍PAΏ\A΋DAΏ\AΏ\AΑhAΓuAΕ�AΕ�AΓuAΓuAΓuAΗ�AΗ�AΙ�AΙ�AΝ�AΡ�AΡ�AΣ�AΣ�AΥ�AΥ�AΥ�AΥ�AΡ�AΉ7A�r�A̲-Aʰ!A�n�A�$�A�
=A�;dA�n�A��A�5?A���A���A���A�;dA��\A�$�A��A�JA�/A���A��A�&�A�`BA�Q�A�p�A��A��A�hsA��A��A�p�A�^5A���A���A�9XA���A���A�ffA�/A�+A���A���A���A��jA�JA�-A��A���A��A��7A�l�A�O�A���A���A�M�A�"�A��uA�Q�A� �A�=qA���A`BA}��A{�-A{33Az�`Ay��Ax�Ax-Awx�Av�yAt�HAr��Aq�7Ap��Ao��Ao�Anv�Al��Al-Ak�7Ak7LAi��Ag+Aep�Ae;dAeoAd-Ab�RAaK�A`A�A_��A_+A^��A]�hA\n�A[��A[;dAZ��AZ�AX��AW�PAU�AT��AS�AR�!AQl�AP�yAP�jAP�APJAOp�AOVALA�AHAE��AD�AD�9AD(�ACC�AB�HAB�uAA��AA�A@ĜA@jA?�A??}A>M�A=��A=��A=�-A=;dA<��A<ȴA;��A:1A9XA8��A8I�A7�A7�A5�;A57LA4��A2��A0ĜA/�hA/dZA/oA.�jA.�A,ĜA*�A*JA(v�A'x�A&�A&  A$��A#�A#
=A!��A!p�A �jA n�A -A��A��A�A��A�
AhsA�RAv�AA�A��A�A��AbA��A�hA7LAĜA�wA�`Ar�A��AC�AffA33A��A|�A=qA�wA\)A
�A	�A	�7A	�A=qA��A-AVA��A��A��At�A�9AVA�;A 1'@�%@��@���@��@�j@�dZ@��\@�7L@�@��@��@�z�@�ƨ@�=q@�9@�P@�O�@�w@�@��#@��/@��y@���@�$�@� �@�
=@ՙ�@Դ9@�ȴ@�$�@���@ёh@�V@�r�@�ƨ@��@�j@�K�@�ȴ@�@�ƨ@��#@ř�@�X@�z�@Å@�E�@�&�@�bN@�@�-@���@���@�`B@�G�@���@��@�@�hs@�/@���@��m@�S�@�E�@�{@��@�@���@��@�j@�|�@�33@�n�@��7@��D@�
=@��@�ȴ@�J@�/@���@��;@�v�@�/@��@��
@���@�o@���@��@��D@�bN@�A�@� �@�  @��;@��w@��@��@��@���@���@�|�@�S�@�"�@���@���@�E�@���@�X@�S�@�"�@��@�o@��H@��!@�V@���@�O�@���@�r�@�(�@���@��;@��F@���@�M�@���@��@�O�@�G�@�7L@��9@��P@���@��\@��+@�^5@�-@��@��-@��7@�X@��@���@���@��9@���@��@��@��P@��R@�5?@�@���@�G�@�Ĝ@��m@�@���@�5?@���@��h@�X@��@�r�@�1'@��@� �@��u@��D@�r�@�I�@�1@��@��R@���@�ff@�{@���@�x�@�V@��`@�Ĝ@��@� �@�1@��@��;@��F@��@��@���@���@��@���@�"�@�E�@���@�7L@�V@��/@��9@��9@��9@���@��@�Q�@�I�@l�@}�@}V@|��@|�D@|I�@|�@|�/@|��@}V@|�@|�j@|�D@|(�@|1@{t�@z��@z-@y�#@y��@y��@y�7@y&�@x�`@xA�@w\)@vV@v@u�-@uV@t��@tj@tZ@t9X@t(�@s�F@r��@r-@q�^@q�7@q%@p�`@p�`@p��@p�u@pb@o��@oK�@o+@n�y@n5?@m@m@m@m�-@m�@l�@k�F@k@j~�@j=q@i�@i�7@i�@hQ�@g�@g�P@gl�@g;d@f�R@fv�@fff@fff@fff@fV@fV@fE�@fE�@f$�@e�T@e�-@e��@e�h@e�@ep�@ep�@e`B@e/@d��@d��@dj@c�
@c�F@c��@cS�@b��@bJ@a��@`�9@`�@`r�@` �@_��@_
=@^��@^ff@^V@^E�@^5?@^{@]@]p�@]?}@\�/@\�D@\j@[�F@[@Z��@Y��@X��@X��@X �@W�;@W�;@W�@W|�@WK�@W+@V�y@V�R@V��@Vv�@Vff@V$�@U@T9X@S�@SS�@So@R�H@R��@R��@R^5@Q�@Q��@Q�@PĜ@PbN@O�;@O�w@O|�@O\)@O;d@O;d@O;d@O;d@O;d@O+@O�@N��@NV@NE�@N5?@M�@Mp�@Kƨ@K��@K�
@L(�@L9X@L(�@L�@K��@J�H@J�\@J=q@J-@JJ@I��@H�`@H�u@H1'@GK�@F5?@EO�@Dj@C��@Ct�@C"�@Co@C@B�H@B�H@B�H@B��@A�@A��@AX@@��@@�9@@Q�@@ �@@  @?�@?�@?�;@?�;@?��@?�@?��@?�P@?�P@?�P@?\)@?�@>�@>�R@>��@>�+@>5?@>$�@>$�@>$�@>$�@>$�@>5?@>$�@>5?@>$�@>$�@>@=�@=/@<�@<�/@<�j@<��@<��@<Z@;S�@:�H@:~�@:�\@:n�@9�#@9��@9x�@9X@8��@81'@8b@6�y@5��@5@5��@5�h@5�@5p�@5?}@5�@4��@4��@4�j@4�@4��@4Z@4Z@4Z@49X@4(�@41@3�
@3dZ@3o@2~�@1�#@0�`@0�u@0bN@/�@/�w@/�P@/l�@/+@.ȴ@.��@.v�@.v�@.v�@.ff@-�-@-/@,��@,�j@,��@,I�@+S�@+@+o@+@*�\@)�@)x�@)X@)G�@)7L@)%@(��@(��@(�`@(�9@(�u@(�@(r�@(Q�@(Q�@(1'@'��@';d@&��@&��@&v�@%�@%@%�-@%��@%��@%�@%O�@%?}@$��@$I�@$1@#��@#�m@#�
@#�
@#�
@#ƨ@#ƨ@#�F@#�F@#��@#�@#33@#o@"�H@"�!@!��@!X@ �`@ b@   @�@�;@��@��@��@��@�w@��@K�@+@+@�@��@�+@v�@ff@{@@@��@�@O�@V@�@Z@9X@�m@ƨ@�F@�F@�F@��@dZ@"�@�@��@��@��@~�@^5@M�@M�@=q@-@-@�@�#@�^@��@��@�7@�7@G�@��@��@1'@  @�;@�;@�w@��@�@�P@l�@\)@+@+@+@�@�@�@�@�@
=@�y@�@V@{@@@@O�@��@��@�/@��@�@z�@9X@��@�F@t�@t�@33@@��@��@~�@M�@=q@-@�@J@J@�#@�7@hs@G�@7L@7L@7L@7L@7L@7L@&�@&�@��@��@�9@��@Q�@1'@��@l�@;d@��@��@�y@�R@��@v�@V@5?@{@��@�-@�h@�@p�@`B@?}@�@��@�/@j@�m@��@t�@dZ@C�@
�@
�\@
n�@
^5@
M�@
=q@
-@
�@
J@	��@	�@	�@	�#@	��@	�^@	��@	x�@	7L@��@�@r�@A�@�@;d@�@ȴ@��@V@5?@{@�@@��@`B@/@V@�j@z�@(�@�m@�F@t�@C�@o@�@�@�@�H@��@�!@�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AΉ7A΍PAΏ\A΋DAΏ\AΏ\AΑhAΓuAΕ�AΕ�AΓuAΓuAΓuAΗ�AΗ�AΙ�AΙ�AΝ�AΡ�AΡ�AΣ�AΣ�AΥ�AΥ�AΥ�AΥ�AΡ�AΉ7A�r�A̲-Aʰ!A�n�A�$�A�
=A�;dA�n�A��A�5?A���A���A���A�;dA��\A�$�A��A�JA�/A���A��A�&�A�`BA�Q�A�p�A��A��A�hsA��A��A�p�A�^5A���A���A�9XA���A���A�ffA�/A�+A���A���A���A��jA�JA�-A��A���A��A��7A�l�A�O�A���A���A�M�A�"�A��uA�Q�A� �A�=qA���A`BA}��A{�-A{33Az�`Ay��Ax�Ax-Awx�Av�yAt�HAr��Aq�7Ap��Ao��Ao�Anv�Al��Al-Ak�7Ak7LAi��Ag+Aep�Ae;dAeoAd-Ab�RAaK�A`A�A_��A_+A^��A]�hA\n�A[��A[;dAZ��AZ�AX��AW�PAU�AT��AS�AR�!AQl�AP�yAP�jAP�APJAOp�AOVALA�AHAE��AD�AD�9AD(�ACC�AB�HAB�uAA��AA�A@ĜA@jA?�A??}A>M�A=��A=��A=�-A=;dA<��A<ȴA;��A:1A9XA8��A8I�A7�A7�A5�;A57LA4��A2��A0ĜA/�hA/dZA/oA.�jA.�A,ĜA*�A*JA(v�A'x�A&�A&  A$��A#�A#
=A!��A!p�A �jA n�A -A��A��A�A��A�
AhsA�RAv�AA�A��A�A��AbA��A�hA7LAĜA�wA�`Ar�A��AC�AffA33A��A|�A=qA�wA\)A
�A	�A	�7A	�A=qA��A-AVA��A��A��At�A�9AVA�;A 1'@�%@��@���@��@�j@�dZ@��\@�7L@�@��@��@�z�@�ƨ@�=q@�9@�P@�O�@�w@�@��#@��/@��y@���@�$�@� �@�
=@ՙ�@Դ9@�ȴ@�$�@���@ёh@�V@�r�@�ƨ@��@�j@�K�@�ȴ@�@�ƨ@��#@ř�@�X@�z�@Å@�E�@�&�@�bN@�@�-@���@���@�`B@�G�@���@��@�@�hs@�/@���@��m@�S�@�E�@�{@��@�@���@��@�j@�|�@�33@�n�@��7@��D@�
=@��@�ȴ@�J@�/@���@��;@�v�@�/@��@��
@���@�o@���@��@��D@�bN@�A�@� �@�  @��;@��w@��@��@��@���@���@�|�@�S�@�"�@���@���@�E�@���@�X@�S�@�"�@��@�o@��H@��!@�V@���@�O�@���@�r�@�(�@���@��;@��F@���@�M�@���@��@�O�@�G�@�7L@��9@��P@���@��\@��+@�^5@�-@��@��-@��7@�X@��@���@���@��9@���@��@��@��P@��R@�5?@�@���@�G�@�Ĝ@��m@�@���@�5?@���@��h@�X@��@�r�@�1'@��@� �@��u@��D@�r�@�I�@�1@��@��R@���@�ff@�{@���@�x�@�V@��`@�Ĝ@��@� �@�1@��@��;@��F@��@��@���@���@��@���@�"�@�E�@���@�7L@�V@��/@��9@��9@��9@���@��@�Q�@�I�@l�@}�@}V@|��@|�D@|I�@|�@|�/@|��@}V@|�@|�j@|�D@|(�@|1@{t�@z��@z-@y�#@y��@y��@y�7@y&�@x�`@xA�@w\)@vV@v@u�-@uV@t��@tj@tZ@t9X@t(�@s�F@r��@r-@q�^@q�7@q%@p�`@p�`@p��@p�u@pb@o��@oK�@o+@n�y@n5?@m@m@m@m�-@m�@l�@k�F@k@j~�@j=q@i�@i�7@i�@hQ�@g�@g�P@gl�@g;d@f�R@fv�@fff@fff@fff@fV@fV@fE�@fE�@f$�@e�T@e�-@e��@e�h@e�@ep�@ep�@e`B@e/@d��@d��@dj@c�
@c�F@c��@cS�@b��@bJ@a��@`�9@`�@`r�@` �@_��@_
=@^��@^ff@^V@^E�@^5?@^{@]@]p�@]?}@\�/@\�D@\j@[�F@[@Z��@Y��@X��@X��@X �@W�;@W�;@W�@W|�@WK�@W+@V�y@V�R@V��@Vv�@Vff@V$�@U@T9X@S�@SS�@So@R�H@R��@R��@R^5@Q�@Q��@Q�@PĜ@PbN@O�;@O�w@O|�@O\)@O;d@O;d@O;d@O;d@O;d@O+@O�@N��@NV@NE�@N5?@M�@Mp�@Kƨ@K��@K�
@L(�@L9X@L(�@L�@K��@J�H@J�\@J=q@J-@JJ@I��@H�`@H�u@H1'@GK�@F5?@EO�@Dj@C��@Ct�@C"�@Co@C@B�H@B�H@B�H@B��@A�@A��@AX@@��@@�9@@Q�@@ �@@  @?�@?�@?�;@?�;@?��@?�@?��@?�P@?�P@?�P@?\)@?�@>�@>�R@>��@>�+@>5?@>$�@>$�@>$�@>$�@>$�@>5?@>$�@>5?@>$�@>$�@>@=�@=/@<�@<�/@<�j@<��@<��@<Z@;S�@:�H@:~�@:�\@:n�@9�#@9��@9x�@9X@8��@81'@8b@6�y@5��@5@5��@5�h@5�@5p�@5?}@5�@4��@4��@4�j@4�@4��@4Z@4Z@4Z@49X@4(�@41@3�
@3dZ@3o@2~�@1�#@0�`@0�u@0bN@/�@/�w@/�P@/l�@/+@.ȴ@.��@.v�@.v�@.v�@.ff@-�-@-/@,��@,�j@,��@,I�@+S�@+@+o@+@*�\@)�@)x�@)X@)G�@)7L@)%@(��@(��@(�`@(�9@(�u@(�@(r�@(Q�@(Q�@(1'@'��@';d@&��@&��@&v�@%�@%@%�-@%��@%��@%�@%O�@%?}@$��@$I�@$1@#��@#�m@#�
@#�
@#�
@#ƨ@#ƨ@#�F@#�F@#��@#�@#33@#o@"�H@"�!@!��@!X@ �`@ b@   @�@�;@��@��@��@��@�w@��@K�@+@+@�@��@�+@v�@ff@{@@@��@�@O�@V@�@Z@9X@�m@ƨ@�F@�F@�F@��@dZ@"�@�@��@��@��@~�@^5@M�@M�@=q@-@-@�@�#@�^@��@��@�7@�7@G�@��@��@1'@  @�;@�;@�w@��@�@�P@l�@\)@+@+@+@�@�@�@�@�@
=@�y@�@V@{@@@@O�@��@��@�/@��@�@z�@9X@��@�F@t�@t�@33@@��@��@~�@M�@=q@-@�@J@J@�#@�7@hs@G�@7L@7L@7L@7L@7L@7L@&�@&�@��@��@�9@��@Q�@1'@��@l�@;d@��@��@�y@�R@��@v�@V@5?@{@��@�-@�h@�@p�@`B@?}@�@��@�/@j@�m@��@t�@dZ@C�@
�@
�\@
n�@
^5@
M�@
=q@
-@
�@
J@	��@	�@	�@	�#@	��@	�^@	��@	x�@	7L@��@�@r�@A�@�@;d@�@ȴ@��@V@5?@{@�@@��@`B@/@V@�j@z�@(�@�m@�F@t�@C�@o@�@�@�@�H@��@�!@�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B9XB8RB7LB8RB9XB8RB9XB9XB9XB9XB7LB7LB7LB8RB7LB7LB8RB8RB8RB8RB8RB9XB8RB8RB8RB8RB8RB7LB+BjB��B�B�qB�B��B{�BaHBcTBy�Br�Bu�Br�BI�BN�B_;BG�B.B/B+BoB�B%B\BJB��B�B�B�NB�jBŢB�jB��B�B�B{�BhsBe`BdZBe`BS�BB�B8RB,B�B
=BB
��BB
��B
�B
�B
��B
�?B
�'B
�B
��B
�\B
s�B
p�B
l�B
aHB
ffB
bNB
W
B
P�B
L�B
F�B
?}B
.B
�B
�B
�B
�B
VB
PB
  B
B
B	��B	�B	�
B	��B	�
B	��B	ǮB	��B	�}B	�dB	�dB	�dB	�FB	�B	��B	��B	��B	��B	��B	�VB	�%B	~�B	}�B	v�B	s�B	k�B	p�B	r�B	p�B	jB	cTB	\)B	D�B	�B	�B	&�B	,B	&�B	�B	�B	�B	�B	\B	hB	\B	DB	+B	B	B		7B	1B	B��B��B�B�HB�B�mB�ZB�BB�BŢBÖB��B�B��B�B�dB�RB�3B�B��B�JB�uB�%B�PB�1B�B� By�B}�Bw�B� B|�B~�B|�Bu�Bp�BgmBgmBdZBk�BgmBl�Bk�Be`B`BBe`BbNBaHBe`B_;BZBP�BO�BS�BP�BF�BA�B6FB%�B49B<jBD�BF�BC�B>wB@�B@�B6FB33B49B33B-B:^B;dB7LB.B,B(�B�BoB#�B!�B+B+B%�B#�B!�B�B"�B"�B �B �B�B�B�BbB{BB�B�B�B�BuB�B$�B!�B$�B"�B)�B.B.B,B(�B'�B!�B'�B-B1'B.B#�B)�B:^B:^B5?B33B49B5?B:^B8RB>wBF�BF�BF�BF�BB�B=qB8RBJ�BJ�BI�BE�BH�BH�BP�BQ�BQ�BP�BL�BL�BK�BP�BO�BP�BR�BVBdZBdZB_;B_;B^5BiyBiyBl�Bq�BiyB`BB]/BdZB|�B~�B~�B� B�B�B�B�B�B�B�B�B�B�B�B�B�B� B� B� B|�Bw�B�oB�{B�{B�oB�hB�hB�bB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B�3B�jB�}B�wB�}B�}B��BBÖBĜBƨBȴBɺBɺBȴBǮBǮBǮB��B��B�B�/B�5B�;B�BB�yB�B�B�B��B��B��B	  B	1B	
=B	oB	{B	�B	�B	�B	�B	%�B	-B	,B	-B	/B	0!B	1'B	49B	5?B	5?B	6FB	9XB	:^B	;dB	;dB	=qB	>wB	>wB	>wB	=qB	<jB	9XB	>wB	M�B	W
B	]/B	^5B	_;B	aHB	aHB	aHB	aHB	aHB	aHB	`BB	ffB	jB	n�B	q�B	t�B	{�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�7B	�VB	�hB	�uB	�uB	�uB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�9B	�3B	�3B	�3B	�?B	�FB	�RB	�RB	�LB	�dB	�}B	�wB	�wB	�qB	�jB	�dB	��B	ŢB	ǮB	ǮB	ǮB	ȴB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�
B	�
B	�B	�B	�5B	�;B	�5B	�5B	�;B	�NB	�`B	�`B	�fB	�fB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�ZB	�ZB	�fB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
B
B
JB
bB
hB
bB
bB
\B
PB
VB
oB
oB
{B
�B
�B
uB
{B
�B
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
#�B
#�B
"�B
!�B
!�B
"�B
#�B
#�B
$�B
#�B
"�B
 �B
#�B
%�B
'�B
'�B
'�B
(�B
(�B
'�B
%�B
$�B
%�B
"�B
"�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
/B
/B
.B
.B
.B
.B
-B
-B
,B
,B
+B
/B
0!B
/B
0!B
1'B
1'B
0!B
0!B
2-B
2-B
2-B
2-B
1'B
0!B
1'B
33B
33B
49B
2-B
2-B
6FB
9XB
9XB
9XB
:^B
<jB
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
A�B
@�B
@�B
B�B
B�B
B�B
B�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
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
I�B
H�B
H�B
I�B
I�B
I�B
G�B
H�B
I�B
G�B
K�B
K�B
K�B
L�B
M�B
M�B
N�B
N�B
M�B
N�B
P�B
Q�B
P�B
O�B
Q�B
Q�B
Q�B
P�B
Q�B
S�B
R�B
R�B
R�B
R�B
R�B
R�B
T�B
S�B
T�B
VB
VB
VB
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
XB
W
B
XB
YB
YB
YB
YB
XB
W
B
XB
YB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
\)B
\)B
[#B
\)B
]/B
]/B
\)B
[#B
]/B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
^5B
_;B
`BB
_;B
_;B
_;B
aHB
aHB
aHB
aHB
bNB
bNB
bNB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
cTB
cTB
cTB
bNB
cTB
cTB
cTB
dZB
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
hsB
hsB
iyB
iyB
iyB
iyB
hsB
hsB
hsB
gmB
gmB
jB
k�B
k�B
k�B
jB
jB
l�B
m�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
k�B
k�B
m�B
n�B
m�B
m�B
m�B
n�B
q�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
q�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B9>B8RB7LB8RB9XB8RB9XB9XB9XB9XB7LB7LB7LB8RB7LB7LB8RB8RB8RB8RB8RB9XB8RB8RB8lB8�B8�B8�B.�BncB�B�'B��B��B�!B��Bh�Bh�B}�BvBw�Bu�BP�BS@Ba�BLB2�B2|B./B�B vB
	B�BpB��B�B�B��B��BǔB��B��B��B�{B~�Bk6BgmBeFBf2BVBESB:*B.IBjB�B%B
��B{B
��B
�[B
�B
ĜB
��B
�aB
��B
��B
��B
wLB
r�B
n�B
cTB
gB
b�B
XyB
Q�B
M�B
G�B
@�B
0oB
�B
;B
~B
�B
�B
VB
�B
�B
�B	��B	�B	�B	οB	�?B	уB	�B	�[B	� B	��B	�6B	�B	�2B	�iB	�>B	��B	��B	��B	��B	�.B	��B	��B	HB	x8B	u?B	mB	qAB	r�B	q'B	k6B	dZB	]�B	H�B	#nB	7B	'�B	,qB	'�B	�B	;B	IB	B	bB	�B	�B	B	1B	9B	�B		7B	fB	�B��B�B�CB�B�QB�XB��B��B�
BǮBĶB��B��B�B�qB��B��B��B��B��B��B��B�1B��B�RB�?B�oB{BHByXB��B}�B}B}qBv�Bq�Bh�Bh�Be�Bl=BhXBl�BlBfLBa-Be�Bc Ba�Be�B_�BZ�BRTBQ BT�BQ�BG�BB�B8lB(�B5�B=�BESBG_BD�B?�BA;BAoB7�B4�B5�B4�B.}B:�B;�B7�B/B,�B*B�BaB$�B#B+kB+�B&�B$�B"�B�B#�B#�B!�B!|B�B�B�B B�B�B�B�B�B�B�B�B%�B"�B%�B$B*B.cB.cB,�B)�B(�B#:B(�B-�B1�B.�B%zB+B:�B:�B6B4B5%B6+B:�B9XB>�BF�BF�BGBF�BCB>wB9�BJ�BK)BJ#BFtBI7BI�BQBR BR:BQ4BM�BMPBL~BQ4BP�BQ�BS�BW$BdtBd�B_�B_�B_;Bi�BjBm�BrBj�Ba�B_Be�B|�B.B.B�4B�;B�;B�;B�AB�-B�3B�3B�-B�-B�AB�AB�;B�AB�OB�iB�iB}�By>B�TB��B��B��B��B��B��B��B��B��B��B�B��B�B�WB�-B�2B�RB�"B�)B�=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�1B�fB�&B�[B�1BݘB޸B��B��B��B��B� B��B��B�B�`B	 4B	KB	
XB	:B	�B	�B	�B	�B	SB	%�B	-CB	,WB	-wB	/OB	0�B	1vB	4TB	5tB	5�B	6�B	9rB	:xB	;B	;B	=�B	>�B	>�B	>wB	=�B	<�B	9�B	?.B	N<B	WYB	]IB	^jB	_VB	aHB	abB	abB	abB	a|B	a|B	`�B	f�B	j�B	n�B	q�B	t�B	{�B	~�B	�B	�B	�'B	�GB	�3B	�SB	�?B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�*B	�RB	�6B	�CB	�5B	�UB	�3B	�9B	�MB	�hB	�hB	�tB	�FB	�lB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	�B	�B	�$B	�$B	�9B	�1B	�1B	�_B	�YB	�YB	�QB	�kB	�jB	�VB	�jB	�jB	�pB	�B	�zB	�zB	�B	�B	�zB	�zB	�zB	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�;B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�(B	�.B
 4B
'B
-B
3B
B
B
B
B
B
3B
3B
GB
9B
?B
YB
gB
{B
�B
JB
HB
NB
bB
}B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$B
#�B
"�B
!�B
!�B
"�B
#�B
#�B
$�B
#�B
#B
!HB
$&B
%�B
(
B
(
B
(>B
)B
)B
(
B
&B
%B
&2B
#TB
#:B
)�B
*B
)�B
)�B
*B
*B
*B
+B
+B
+B
,B
,"B
-)B
/B
/ B
.IB
./B
./B
./B
-]B
-CB
,WB
,=B
+QB
/5B
0;B
/OB
0;B
1[B
1[B
0;B
0;B
2GB
2GB
2-B
2-B
1AB
0UB
1[B
3hB
3MB
4TB
2|B
2|B
6`B
9rB
9rB
9�B
:�B
<�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
A�B
@�B
@�B
B�B
B�B
B�B
B�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
D�B
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
I�B
H�B
H�B
I�B
I�B
I�B
HB
H�B
I�B
G�B
K�B
K�B
K�B
L�B
M�B
M�B
N�B
N�B
M�B
N�B
Q B
Q�B
Q B
PB
RB
Q�B
RB
P�B
RB
S�B
SB
SB
SB
SB
SB
SB
T�B
TB
UB
VB
VB
VB
UB
UB
UB
VB
VB
W
B
W$B
W
B
W$B
XB
XB
XB
XB
XB
X+B
W$B
X+B
YB
YB
Y1B
YB
X+B
W?B
X+B
YKB
Z7B
[#B
[#B
[=B
\)B
\)B
\CB
\CB
\)B
\]B
]/B
]/B
]/B
]IB
]/B
]/B
]/B
]/B
\)B
\CB
[WB
\CB
]IB
]/B
\CB
[WB
]/B
^5B
^OB
^OB
^jB
^OB
]IB
^jB
^OB
_VB
`BB
_VB
_VB
_VB
aHB
abB
abB
aHB
bhB
bNB
bNB
aHB
abB
abB
bNB
bhB
cTB
c:B
cnB
cTB
cTB
cTB
cTB
c:B
bhB
c�B
cnB
cnB
bhB
cnB
c�B
cnB
dtB
e�B
ffB
f�B
ffB
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
iyB
iyB
iyB
i�B
h�B
h�B
h�B
g�B
g�B
jB
k�B
k�B
k�B
j�B
j�B
l�B
m�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
k�B
k�B
m�B
n�B
m�B
m�B
m�B
n�B
q�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
q�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811140034592018111400345920181114003459201811140200162018111402001620181114020016201811150018542018111500185420181115001854  JA  ARFMdecpA19c                                                                20181110093612  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181110003614  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181110003618  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181110003618  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181110003619  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181110003619  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181110003619  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181110003619  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181110003619  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181110003619                      G�O�G�O�G�O�                JA  ARUP                                                                        20181110005537                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181110153446  CV  JULD            G�O�G�O�F�{�                JM  ARCAJMQC2.0                                                                 20181113153459  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181113153459  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181113170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181114151854  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                