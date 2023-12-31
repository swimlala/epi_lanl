CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-12-04T00:36:24Z creation;2018-12-04T00:36:29Z conversion to V3.1;2019-12-19T07:26:34Z update;     
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181204003624  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              3A   JA  I2_0576_307                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @ؕvOC� 1   @ؕwO���@9&�+J�d.|����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B�.B���B���B�ǮB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\Dx�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DH�DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�B�DՂ�Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D��D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D���D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aź^Aź^Aź^AŸRAź^Aź^Aź^Aź^AŸRAź^Aź^AžwA���A���A���A���A���A�A�A�ĜA�A�ĜA�A�ĜA�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ȴA�ȴA�ƨAŶFA��AþwA��
A�bNA���A��A���A�~�A��A���A���A���A���A���A�z�A���A�oA��A��uA�33A�O�A���A��FA�XA�oA��A�+A���A��uA�l�A�E�A�(�A��A�/A���A��;A�M�A���A�1'A��A��uA�7LA�dZA��A�|�A���A�G�A��A�&�A�ƨA�ffA�ZA��#A�VA�E�A�+A�ȴA���A��A�7LA��`A���A�A��A���A�/A��HA�VA���A�bNA�bA�|�A�A}��A}�A|��Az�+Ax��AwAv��Au�At~�ArAp�Ao;dAm�;Am�Al=qAk%AiS�Af��AeK�Ac|�A_��A^JA]p�A\��A[K�AY�
AX�DAWhsAW+AV�`AVjAUS�AT=qAS;dAR��AR�+AQƨAP�HAPJANZAL�AK��AKS�AJ�AJ  AH�`AH$�AG&�AFVAE�AEAC�AB�yAB�AAA?�^A?+A>ZA=�7A<��A:Q�A9/A8�A81'A7��A7l�A6�DA5�
A4�uA3��A3+A2M�A1hsA0 �A/��A.ffA-�A-K�A,��A+hsA*�A)��A(�A'��A&��A$jA#7LA"{A ��A   A�A�+A��A�A1A��A�RA=qA�A��A�AJA�AdZA�RAbNA�
A&�A��A�hA��A�A�9AbA��A�A	��A	p�A �AM�AXA�!A  A/A �A�FA;d@�@��@���@��H@��@��`@��;@��H@��#@��@�(�@��T@��
@��@��@�@홚@�O�@�z�@�~�@��T@��@�7@�hs@�@�dZ@��@�@�A�@�n�@�p�@���@� �@�;d@݉7@��
@���@ى7@؃@��m@��@�n�@���@ղ-@��@�1@�
=@��@Ь@��;@ϕ�@�dZ@�"�@��#@̋D@˥�@�t�@��y@�5?@��#@��@�K�@��H@Ɵ�@��T@�p�@��@��m@å�@ÍP@�\)@���@�j@�|�@�33@���@�Q�@�|�@��y@��@��T@�@�X@���@�1@��@��T@�Ĝ@��@��@�Z@�A�@�  @��@�o@���@���@���@��@�ff@��@�{@�J@��T@��7@�/@���@��u@�bN@� �@��#@��\@�`B@�Ĝ@�j@�1'@��@���@�C�@��@��+@��h@�7L@��`@��@�ȴ@�{@��-@�x�@���@���@�j@�1'@��@���@�@��h@�G�@��@��/@�I�@��P@��@�M�@��@���@�Z@��@�ȴ@��h@�p�@�hs@�X@�/@�V@��@�Ĝ@��j@�Ĝ@�r�@���@��y@��+@��\@��\@��\@��+@�n�@�ff@�$�@�p�@�7L@�V@��`@�Q�@��F@���@�5?@���@���@�z�@���@��H@�ȴ@�ȴ@���@��R@��R@���@�ȴ@��@�@�o@�o@�;d@�S�@�K�@�C�@�@���@��R@��!@�~�@�n�@�n�@�^5@�^5@�=q@�@��T@��h@�?}@��@�A�@��;@�;d@���@�+@�S�@�t�@�;d@��@���@��y@��y@��H@�ȴ@���@��\@�E�@�J@�@�@���@���@��#@���@�x�@�G�@�/@��u@�Q�@�A�@�(�@�w@\)@+@~��@~�@~ff@}�@}��@}?}@}�@}�@}O�@|�j@|j@{��@{��@{33@z��@y�@x��@x�u@xr�@xA�@w;d@vȴ@vV@v@u/@t��@t(�@st�@r�!@rJ@qG�@p �@o|�@n�@n�+@m�T@m/@lz�@l�@k�F@j�@iG�@hĜ@h�@hQ�@h  @g��@gK�@g
=@f�@f�R@f$�@e?}@d��@c��@cC�@c"�@b�@b��@b^5@a��@a�#@a��@a&�@`�`@`�@`Q�@`A�@`1'@`b@`b@`  @_�;@_�@_��@_|�@_�P@_�P@_�P@_|�@_K�@_�@_K�@^��@]�@]/@\��@\z�@\j@\Z@[��@[��@[S�@[33@["�@[@Z�!@Z=q@Y��@Y�@X�@W��@W\)@V��@Vv�@VV@VE�@V$�@V@U��@U��@U/@T��@T�/@TI�@S��@R��@R^5@Q�@Q�^@QX@Q�@P��@P��@PĜ@P�@PbN@PbN@PbN@PQ�@PA�@Pb@O��@Ol�@OK�@O�@N��@N�+@N$�@Mp�@L��@L�j@L��@Lz�@LI�@K�m@K��@KC�@K"�@J�@J�@J��@J=q@J�@I��@IG�@H�@G�P@G\)@G;d@F��@F�@F�@F�R@F��@Fff@E�-@EO�@E�@D�j@D(�@C�F@C"�@B�\@BM�@A�^@AG�@A7L@@�`@?�w@>�@=�@=`B@<�@<��@;ƨ@;�@;S�@;33@;@:�@:�H@:��@:�!@:��@:~�@:^5@:=q@:-@9��@9�@9�@9�@9��@9�@9hs@8��@8bN@81'@7��@7�@7��@7��@7�P@7�P@7�P@7�P@7|�@7l�@7\)@7K�@7�@6�y@6�R@6{@5`B@5/@5V@4�/@4�@4z�@49X@4�@3�m@3��@333@3"�@3@2��@2~�@2�@1��@1��@1�7@17L@0��@0��@0�@0�@0 �@/��@/\)@/;d@/
=@.��@.�@.5?@-�T@-`B@,�/@,j@,Z@,(�@+�@+S�@+S�@+C�@+33@+o@+o@*�@*��@*��@*��@*M�@)�@)��@)7L@(��@(��@(�9@(�@'�;@'��@';d@'+@'�@&�R@&E�@%�@%��@%��@%�@$Z@$1@#�m@#�
@#�@"~�@"-@!�#@!�^@!�^@!�^@!��@!G�@!&�@!%@ Ĝ@ ��@ 1'@�w@l�@��@ff@V@V@V@E�@5?@5?@5?@$�@{@�@�@�-@`B@��@�j@��@�D@j@�m@dZ@33@�@��@�!@~�@J@hs@�@��@�9@��@�u@�@r�@1'@  @�w@��@l�@�@��@ȴ@�+@V@E�@E�@5?@5?@@@�@p�@p�@`B@/@V@�/@�/@�@��@j@�@�
@�
@��@�@dZ@C�@"�@o@�@��@��@�\@^5@��@�^@��@�7@hs@X@G�@%@��@bN@ �@b@  @�P@;d@+@�@�@
=@��@ȴ@�R@�+@v�@ff@ff@��@��@p�@?}@/@�@��@�@��@�@z�@z�@Z@9X@1@��@ƨ@��@C�@
��@
~�@
n�@
M�@
�@	�@	��@	��@	�7@	�7@	hs@	&�@	�@��@��@��@��@�9@�u@Q�@1'@�;@�P@|�@\)@+@+@��@ȴ@ȴ@ȴ@ȴ@�R@��@v�@E�@E�@5?@$�@�@@��@�@��@�j@�@�@��@z�@Z@Z@I�@�@�@1@1@��@��@�m@�
@�@"�@o@@�@�@�!@n�@M�@=q@J@��@hs@G�@&�@%@ ��@ ��@ ��@ ��@ ��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aź^Aź^Aź^AŸRAź^Aź^Aź^Aź^AŸRAź^Aź^AžwA���A���A���A���A���A�A�A�ĜA�A�ĜA�A�ĜA�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ȴA�ȴA�ƨAŶFA��AþwA��
A�bNA���A��A���A�~�A��A���A���A���A���A���A�z�A���A�oA��A��uA�33A�O�A���A��FA�XA�oA��A�+A���A��uA�l�A�E�A�(�A��A�/A���A��;A�M�A���A�1'A��A��uA�7LA�dZA��A�|�A���A�G�A��A�&�A�ƨA�ffA�ZA��#A�VA�E�A�+A�ȴA���A��A�7LA��`A���A�A��A���A�/A��HA�VA���A�bNA�bA�|�A�A}��A}�A|��Az�+Ax��AwAv��Au�At~�ArAp�Ao;dAm�;Am�Al=qAk%AiS�Af��AeK�Ac|�A_��A^JA]p�A\��A[K�AY�
AX�DAWhsAW+AV�`AVjAUS�AT=qAS;dAR��AR�+AQƨAP�HAPJANZAL�AK��AKS�AJ�AJ  AH�`AH$�AG&�AFVAE�AEAC�AB�yAB�AAA?�^A?+A>ZA=�7A<��A:Q�A9/A8�A81'A7��A7l�A6�DA5�
A4�uA3��A3+A2M�A1hsA0 �A/��A.ffA-�A-K�A,��A+hsA*�A)��A(�A'��A&��A$jA#7LA"{A ��A   A�A�+A��A�A1A��A�RA=qA�A��A�AJA�AdZA�RAbNA�
A&�A��A�hA��A�A�9AbA��A�A	��A	p�A �AM�AXA�!A  A/A �A�FA;d@�@��@���@��H@��@��`@��;@��H@��#@��@�(�@��T@��
@��@��@�@홚@�O�@�z�@�~�@��T@��@�7@�hs@�@�dZ@��@�@�A�@�n�@�p�@���@� �@�;d@݉7@��
@���@ى7@؃@��m@��@�n�@���@ղ-@��@�1@�
=@��@Ь@��;@ϕ�@�dZ@�"�@��#@̋D@˥�@�t�@��y@�5?@��#@��@�K�@��H@Ɵ�@��T@�p�@��@��m@å�@ÍP@�\)@���@�j@�|�@�33@���@�Q�@�|�@��y@��@��T@�@�X@���@�1@��@��T@�Ĝ@��@��@�Z@�A�@�  @��@�o@���@���@���@��@�ff@��@�{@�J@��T@��7@�/@���@��u@�bN@� �@��#@��\@�`B@�Ĝ@�j@�1'@��@���@�C�@��@��+@��h@�7L@��`@��@�ȴ@�{@��-@�x�@���@���@�j@�1'@��@���@�@��h@�G�@��@��/@�I�@��P@��@�M�@��@���@�Z@��@�ȴ@��h@�p�@�hs@�X@�/@�V@��@�Ĝ@��j@�Ĝ@�r�@���@��y@��+@��\@��\@��\@��+@�n�@�ff@�$�@�p�@�7L@�V@��`@�Q�@��F@���@�5?@���@���@�z�@���@��H@�ȴ@�ȴ@���@��R@��R@���@�ȴ@��@�@�o@�o@�;d@�S�@�K�@�C�@�@���@��R@��!@�~�@�n�@�n�@�^5@�^5@�=q@�@��T@��h@�?}@��@�A�@��;@�;d@���@�+@�S�@�t�@�;d@��@���@��y@��y@��H@�ȴ@���@��\@�E�@�J@�@�@���@���@��#@���@�x�@�G�@�/@��u@�Q�@�A�@�(�@�w@\)@+@~��@~�@~ff@}�@}��@}?}@}�@}�@}O�@|�j@|j@{��@{��@{33@z��@y�@x��@x�u@xr�@xA�@w;d@vȴ@vV@v@u/@t��@t(�@st�@r�!@rJ@qG�@p �@o|�@n�@n�+@m�T@m/@lz�@l�@k�F@j�@iG�@hĜ@h�@hQ�@h  @g��@gK�@g
=@f�@f�R@f$�@e?}@d��@c��@cC�@c"�@b�@b��@b^5@a��@a�#@a��@a&�@`�`@`�@`Q�@`A�@`1'@`b@`b@`  @_�;@_�@_��@_|�@_�P@_�P@_�P@_|�@_K�@_�@_K�@^��@]�@]/@\��@\z�@\j@\Z@[��@[��@[S�@[33@["�@[@Z�!@Z=q@Y��@Y�@X�@W��@W\)@V��@Vv�@VV@VE�@V$�@V@U��@U��@U/@T��@T�/@TI�@S��@R��@R^5@Q�@Q�^@QX@Q�@P��@P��@PĜ@P�@PbN@PbN@PbN@PQ�@PA�@Pb@O��@Ol�@OK�@O�@N��@N�+@N$�@Mp�@L��@L�j@L��@Lz�@LI�@K�m@K��@KC�@K"�@J�@J�@J��@J=q@J�@I��@IG�@H�@G�P@G\)@G;d@F��@F�@F�@F�R@F��@Fff@E�-@EO�@E�@D�j@D(�@C�F@C"�@B�\@BM�@A�^@AG�@A7L@@�`@?�w@>�@=�@=`B@<�@<��@;ƨ@;�@;S�@;33@;@:�@:�H@:��@:�!@:��@:~�@:^5@:=q@:-@9��@9�@9�@9�@9��@9�@9hs@8��@8bN@81'@7��@7�@7��@7��@7�P@7�P@7�P@7�P@7|�@7l�@7\)@7K�@7�@6�y@6�R@6{@5`B@5/@5V@4�/@4�@4z�@49X@4�@3�m@3��@333@3"�@3@2��@2~�@2�@1��@1��@1�7@17L@0��@0��@0�@0�@0 �@/��@/\)@/;d@/
=@.��@.�@.5?@-�T@-`B@,�/@,j@,Z@,(�@+�@+S�@+S�@+C�@+33@+o@+o@*�@*��@*��@*��@*M�@)�@)��@)7L@(��@(��@(�9@(�@'�;@'��@';d@'+@'�@&�R@&E�@%�@%��@%��@%�@$Z@$1@#�m@#�
@#�@"~�@"-@!�#@!�^@!�^@!�^@!��@!G�@!&�@!%@ Ĝ@ ��@ 1'@�w@l�@��@ff@V@V@V@E�@5?@5?@5?@$�@{@�@�@�-@`B@��@�j@��@�D@j@�m@dZ@33@�@��@�!@~�@J@hs@�@��@�9@��@�u@�@r�@1'@  @�w@��@l�@�@��@ȴ@�+@V@E�@E�@5?@5?@@@�@p�@p�@`B@/@V@�/@�/@�@��@j@�@�
@�
@��@�@dZ@C�@"�@o@�@��@��@�\@^5@��@�^@��@�7@hs@X@G�@%@��@bN@ �@b@  @�P@;d@+@�@�@
=@��@ȴ@�R@�+@v�@ff@ff@��@��@p�@?}@/@�@��@�@��@�@z�@z�@Z@9X@1@��@ƨ@��@C�@
��@
~�@
n�@
M�@
�@	�@	��@	��@	�7@	�7@	hs@	&�@	�@��@��@��@��@�9@�u@Q�@1'@�;@�P@|�@\)@+@+@��@ȴ@ȴ@ȴ@ȴ@�R@��@v�@E�@E�@5?@$�@�@@��@�@��@�j@�@�@��@z�@Z@Z@I�@�@�@1@1@��@��@�m@�
@�@"�@o@@�@�@�!@n�@M�@=q@J@��@hs@G�@&�@%@ ��@ ��@ ��@ ��@ ��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	7B	7B	7B	7B	7B
=B
=B
=B
=B
=B
=B
=B	7B	7B	7B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B	7B+BB�BhBQ�B:^BA�Bn�B`BBT�BQ�BM�B=qB$�B0!B49BP�BE�B=qBI�BD�B7LB�B-BoB�ZB�ZB�B�TBB�^B�#B�BǮB��B��BB�9B��B��B�BffB{�BffB\)B=qB �B�B-B'�B$�B�BBB
�B
�mB
��B
�)B
�/B
ǮB
�qB
��B
�qB
�B
��B
��B
�bB
�oB
�=B
z�B
�%B
~�B
o�B
[#B
^5B
_;B
[#B
C�B
2-B
:^B
1'B
%�B
oB	��B
B	��B	��B	�B	�B	�)B	ǮB	��B	�B	��B	jB	�B	�VB	�%B	q�B	ffB	jB	gmB	v�B	q�B	hsB	YB	VB	R�B	^5B	YB	N�B	D�B	;dB	-B	�B	)�B	49B	'�B	$�B	�B	�B	oB	\B	
=B	%B��B��B�B�B�5B�mB�;B��B��B�RBŢB��B��B��B��BĜB�qB�?B�qB�RB�B�B��B�B��B��B��B��B�uB�bB�hB�+B}�Bv�BgmBhsBgmBhsBgmBiyB_;BW
BF�B@�BR�B]/B[#BQ�BQ�BVBN�B<jB7LBE�BJ�BE�B=qB=qB0!B33B,B49B7LB;dB.B#�B,B�BhB'�B/B)�B(�B"�B,B&�BuB#�B&�B)�B,B+B+B+B,B(�B+B�B�B-B.B6FB6FB0!B-B�B1'B6FB5?B1'B(�B#�B'�B"�B!�B�B(�B-B+B#�B!�B�B'�B'�B(�B1'B.B0!B33B2-B.B'�B%�B(�B(�B2-B8RB7LB33B(�B'�B1'B9XB49B33B49B.B+B9XB;dB6FB:^B8RB6FB=qB=qB7LB+B-B49B<jB0!B33B>wBD�BC�BL�BM�BI�BE�BG�BH�BA�BK�B^5B^5B^5B^5B\)B[#BXB[#BZBZBT�BK�BgmBl�Bl�BjBhsBhsBjBgmBgmB_;BO�BG�Bq�B}�B�B�B�B�B�B�B�B�B�1B�=B�B�1B�hB��B��B��B��B��B��B��B��B��B�B�B�'B�!B�B�B�'B�-B�FB�B�LB�^B�?B�}B�B�B�
B�B�
B�B�B�#B�#B�B�B�)B�yB��B��B��B��B��B��B��B��B	B	%B	%B	B	
=B	\B	�B	�B	!�B	%�B	&�B	'�B	5?B	7LB	8RB	8RB	:^B	<jB	<jB	>wB	?}B	@�B	A�B	B�B	D�B	B�B	A�B	B�B	H�B	M�B	M�B	L�B	N�B	P�B	P�B	P�B	N�B	M�B	O�B	O�B	P�B	O�B	VB	XB	XB	[#B	iyB	iyB	iyB	hsB	m�B	o�B	s�B	v�B	v�B	v�B	v�B	u�B	u�B	v�B	y�B	z�B	z�B	z�B	z�B	z�B	}�B	~�B	� B	~�B	�7B	�PB	�JB	�DB	�PB	�bB	�bB	�bB	�bB	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�?B	�FB	�FB	�dB	�qB	��B	��B	B	ĜB	ƨB	ǮB	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�)B	�;B	�;B	�;B	�5B	�5B	�NB	�NB	�5B	�NB	�NB	�`B	�`B	�fB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�mB	�mB	�fB	�fB	�fB	�mB	�TB	�TB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
B
B
B
B
B
%B
+B
+B
+B
+B
%B
%B
%B
1B
1B
+B
	7B
1B
+B

=B
DB
JB
JB
DB

=B
JB
PB
\B
\B
bB
\B
\B
bB
VB
VB
VB
VB
{B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
!�B
%�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
,B
-B
-B
-B
,B
)�B
)�B
.B
/B
/B
0!B
1'B
1'B
1'B
2-B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
/B
/B
33B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
7LB
6FB
6FB
6FB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
:^B
:^B
<jB
=qB
=qB
=qB
<jB
:^B
<jB
;dB
<jB
<jB
>wB
=qB
<jB
?}B
@�B
@�B
?}B
?}B
@�B
?}B
?}B
@�B
?}B
>wB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
?}B
A�B
B�B
D�B
C�B
B�B
B�B
D�B
E�B
D�B
C�B
C�B
F�B
G�B
G�B
F�B
C�B
H�B
I�B
J�B
K�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
O�B
P�B
Q�B
R�B
R�B
Q�B
P�B
Q�B
S�B
S�B
S�B
T�B
S�B
R�B
Q�B
S�B
T�B
T�B
VB
W
B
W
B
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
XB
XB
XB
YB
ZB
ZB
ZB
ZB
YB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
^5B
_;B
`BB
aHB
`BB
`BB
`BB
_;B
_;B
`BB
aHB
bNB
bNB
`BB
bNB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
bNB
dZB
e`B
ffB
ffB
gmB
ffB
gmB
ffB
gmB
gmB
hsB
gmB
ffB
gmB
gmB
gmB
gmB
ffB
ffB
hsB
jB
jB
jB
jB
jB
k�B
l�B
l�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
l�B
m�B
o�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
o�B
o�B
p�B
o�B
p�B
r�B
s�B
s�B
s�B
r�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
s�B
s�B
s�B
r�B
r�B
u�B
u�B
u�B
t�B
t�B
s�B
u�B
u�B
t�B
t�B
u�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	7B	7B	7B	7B	7B
=B
=B
=B
=B
=B
=B
=B	7B	7B	7B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
XB	lB�BmB�BBU�BA�BF�Bp�Bc�BXEBUBP�BA�B)*B4TB6�BR BG�B?�BJ�BE�B9XB�B.IBB�*B�B�/B�`BƨB��B�#BևB�B�>B��B�B��B��B�)B��BiyB}"BhsB]�B@�B$�B�B.IB)*B%�B B�BaB
��B
�_B
�MB
��B
��B
�RB
��B
�[B
�(B
��B
�8B
�B
� B
�@B
��B
|jB
��B
�B
q'B
]IB
_�B
`'B
[�B
F?B
4B
;B
2GB
'mB
{B	��B
�B	��B	�RB	��B	��B	��B	�	B	�B	��B	�B	oB	��B	�BB	�EB	s�B	hXB	lB	h�B	v�B	r-B	iDB	Z�B	WYB	T,B	^�B	Y�B	O�B	E�B	<�B	/5B	�B	+B	4�B	)B	%�B	B	�B	�B	}B	^B	B��B��B��B�B��B�$B�\B�SB�bB�JB�BϑB՛BΥB�TB��B��B��B�BB�rB�oB�WB��B��B�HB��B��B��B��B��B��B��B�Bx�BjBjBiBi�Bh�BjeB`�BXyBI7BB�BTB]�B[�BSBR�BV�BO�B>(B9$BFtBKDBFtB>�B>wB1�B4�B-�B5ZB8RB;�B/OB%`B-CB �B�B)*B0!B+B*0B$@B,�B(
B�B$�B(
B*�B,�B+�B+�B+�B,�B)�B+�B \B�B-�B.�B6zB6�B0�B-�B;B1vB6zB5tB1vB)�B$�B(�B#�B"�B B)�B-�B+�B$�B#B�B(�B(�B)�B1�B.�B0�B3�B2�B.�B(�B&�B)�B)�B2�B8�B7�B3�B)�B(�B1�B9�B4�B3�B4�B/ B,B9�B;�B6�B:�B8�B6�B=�B=�B7�B,=B.B4�B<�B1'B49B?BEBD3BMBNBJ#BF?BH1BIRBB�BL~B^OB^jB^jB^jB\xB[�BX�B[�BZ�BZ�BU�BMjBgmBl�Bl�Bj�Bh�Bh�Bj�Bg�Bg�B_�BQ�BI�BrGB~]B�aB�mB�mB�mB�mB�mB��B��B��B��B��B�B��B��B��B�	B�B��B�B�/B�xB�FB�QB�OB�[B�oB��B��B��B��B��B�!B��B��B�FB�OB��B�+B�?B�9B�?B�EB�KB�=B�=B�yB֡BܬB��B��B��B�B�B�B�B�0B�jB	MB	YB	�B	�B	
�B	.B		B	;B	"hB	&LB	'�B	(�B	5ZB	7LB	8�B	8lB	:^B	<jB	<jB	>wB	?}B	@�B	A�B	B�B	D�B	B�B	A�B	B�B	H�B	M�B	M�B	MB	N�B	P�B	Q B	Q B	OB	NB	O�B	P.B	QNB	P}B	V9B	X_B	XyB	[WB	i_B	iyB	iyB	h�B	m�B	o�B	s�B	v�B	v�B	v�B	v�B	u�B	u�B	v�B	y�B	z�B	z�B	{B	{B	{B	~B	.B	�4B	}B	�lB	�jB	�~B	��B	��B	�}B	�}B	��B	��B	��B	�uB	��B	�sB	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�:B	�DB	�B	�KB	�RB	�QB	�CB	�cB	�wB	�UB	�aB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�B	��B	� B	�B	�:B	� B	�,B	�B	�B	�B	�@B	�TB	�MB	�sB	�CB	�VB	�VB	�VB	�OB	�jB	�NB	�B	ބB	�hB	�B	�zB	�zB	�fB	�`B	�fB	�B	�B	�B	�B	�B	�sB	�mB	�mB	�B	�B	�B	�mB	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	�B	��B	�B	��B	��B	��B	�B	��B	�B	�	B	��B	�B	�B	�B	�B	�B	�0B	�B	�B	�B	�*B	�*B	�B	�.B
;B
 4B
 B
-B
-B
B
3B
?B
+B
+B
EB
EB
?B
?B
YB
KB
KB
_B
	RB
fB
zB

XB
^B
dB
dB
DB

rB
dB
�B
vB
�B
bB
�B
�B
}B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	B
B
 �B
!�B
#B
"B
%�B
(
B
(
B
(
B
(�B
(�B
)B
)B
)B
)B
)B
*B
*B
*B
,B
-B
-B
-)B
,"B
*B
*0B
./B
/5B
/5B
0;B
1'B
1AB
1'B
2-B
1'B
1'B
1'B
1'B
1'B
1AB
1AB
1AB
1AB
/OB
/OB
3hB
4TB
4TB
4TB
4TB
4TB
4TB
4TB
4TB
5ZB
7LB
6`B
6zB
6`B
7fB
8RB
9rB
9rB
9rB
9�B
:xB
;B
;B
:�B
:�B
<�B
=�B
=�B
=�B
<�B
:�B
<�B
;�B
<�B
<�B
>wB
=�B
<�B
?�B
@�B
@�B
?�B
?�B
@�B
?�B
?�B
@�B
?�B
>�B
>�B
?�B
?�B
@�B
A�B
A�B
A�B
?�B
A�B
B�B
D�B
C�B
B�B
B�B
D�B
E�B
D�B
C�B
C�B
F�B
G�B
G�B
F�B
DB
H�B
I�B
J�B
K�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
J�B
J�B
K�B
K�B
LB
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
Q�B
Q B
O�B
Q B
RB
SB
SB
RB
QB
R:B
TB
S�B
TB
UB
TB
S&B
R B
T,B
UB
UB
VB
W
B
W
B
VB
V9B
VB
VB
W
B
W$B
W$B
X+B
X+B
X+B
Y1B
Z7B
ZB
ZB
ZB
Y1B
Y1B
Z7B
[#B
[#B
[WB
[=B
[=B
[=B
\)B
[=B
[=B
[WB
[WB
\CB
]/B
]IB
]IB
]IB
]dB
^5B
^5B
^OB
^OB
^OB
_;B
^jB
^jB
_VB
`\B
aHB
`\B
`BB
`vB
_VB
_pB
`\B
abB
bNB
b�B
`vB
bhB
dZB
dtB
dZB
dZB
dtB
dtB
dZB
dZB
e`B
e`B
dtB
b�B
dtB
ezB
f�B
f�B
gmB
f�B
gmB
f�B
g�B
g�B
hsB
g�B
f�B
g�B
g�B
g�B
g�B
f�B
f�B
h�B
jB
j�B
j�B
j�B
j�B
k�B
l�B
lqB
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
l�B
l�B
m�B
l�B
m�B
o�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
o�B
p�B
p�B
p�B
o�B
o�B
p�B
o�B
p�B
r�B
s�B
s�B
s�B
r�B
s�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
s�B
s�B
s�B
r�B
r�B
u�B
u�B
u�B
t�B
t�B
s�B
u�B
u�B
t�B
t�B
u�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812080031392018120800313920181208003139201812080200162018120802001620181208020016201812090022492018120900224920181209002249  JA  ARFMdecpA19c                                                                20181204093622  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181204003624  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181204003628  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181204003628  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181204003629  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181204003629  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181204003629  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181204003629  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181204003629  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181204003629                      G�O�G�O�G�O�                JA  ARUP                                                                        20181204005713                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181204153740  CV  JULD            G�O�G�O�Fī�                JM  ARCAJMQC2.0                                                                 20181207153139  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181207153139  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181207170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181208152249  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                