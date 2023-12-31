CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  r   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-05-21T11:00:34Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  G8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  Xt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  [�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  wx   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ӌ   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    Ӑ   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    Ӕ   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    Ә   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  Ӝ   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210521110034  20210521110034  5906585 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8354                            2B  A   NAVIS_A                         1272                            170425                          863 @�vZ�V��1   @�v[I���@5Q���l��d��+J1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�<�Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ Dܐ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @?\)@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/��C1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D��D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;x�D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�<{D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�<{D��Dڿ�D���D�?�D��Dۿ�D���D�?�D܏�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�~�A�~�A�~�A�~�A�~�A�|�A�x�A�^5A�$�AξwAΧ�AΑhA�jA�K�A� �A��/A�K�A���A���A˓uA�C�A�oA���A���A�ZA�jA�oAƕ�A�|�Aô9A�{AA��A��#A��A��+A�z�A�;dA��A���A�+A�C�A��A�1'A��A��FA��A���A�v�A�\)A��A��A���A��DA�O�A���A�r�A��wA�^5A�hsA��jA��A�x�A��7A�ĜA�jA�A�ĜA��A���A�"�A��RA�ZA��/A���A��RA���A�ZA��/A���A�jA�\)A�p�A��9A��A�?}A��wA���A��-A�7LA�(�A�"�A�A��/A��\A�ƨA��A�(�A���A�7LA�1'A�JA�VA���A���A��+A���A���A�
=A�ffA��!A���A���A���A��;A�+A|��Az��Awt�Av��Au��At��AsoAq�TAqoAn~�Ak�Ah�+Ahv�Agt�Af^5Ad��AdZAc�;Ac�AbbA`�`A]�mA\Q�A[�hAYoAU�mAT�AS�7AR�HAR�AP�/AO��AN(�AKp�AI�PAHVAF��AF-AEhsAD�/ADA�AC�hAB�jAA�wAA%A@��A?�7A?7LA>r�A=�A=�;A=��A=A<~�A;�^A:=qA9�A8ĜA8~�A7\)A6  A3A2�A21A0�A/��A.�A-�A,��A+�mA+"�A*�A($�A&�A&{A$��A$^5A#�FA"�\A"bA �/An�A�A��A��AbNA�;A�hA��AO�A�A��A�\A  A�yAr�AI�A1'A  AG�Ap�A�+A�AQ�A5?A�Al�A�9A$�A/AƨA
�RA	Az�A��AA��A"�Az�A�AE�A�A��A�;A�TA��AK�A �A   @�+@��R@�-@�G�@��@�  @��R@�^5@�D@�^5@�h@�?}@�j@�1@�ƨ@��@�^5@�z�@�@߶F@��T@܃@�I�@���@�1@�\)@�
=@֧�@Ցh@щ7@�ff@�|�@ʸR@�{@��#@�-@���@�o@϶F@���@�E�@�O�@��
@�dZ@��@ʸR@ʇ+@�E�@�/@ȃ@�1'@�@�&�@ě�@�Z@���@��;@ÍP@�@��@�7L@��@�K�@��R@���@���@�/@�9X@�33@�{@���@��@��9@�bN@�A�@� �@�1@���@�@��R@��@��-@��@�%@��9@��;@��!@�-@��-@�j@��P@�S�@�@���@��#@���@���@���@�1'@��H@��@��9@��/@��@���@�/@�?}@�7L@�?}@�@���@��y@��@���@���@��y@��@���@�ȴ@��T@�Ĝ@���@���@���@���@���@���@��/@��/@��/@��/@��@��@�I�@���@�+@�o@��y@���@��\@��+@���@���@�o@�ff@��@���@���@��@���@���@�  @�@�o@���@��@���@���@�$�@�J@��@�{@�J@��T@�@���@���@��7@�x�@�hs@�7L@�/@�&�@���@��@��/@���@���@��@��u@��u@��u@��@�j@�Q�@�1'@��m@�+@��y@�ȴ@���@�n�@��@�`B@�7L@�V@���@��/@���@��@���@��@�Q�@�A�@��@���@��;@��
@���@��@�t�@�;d@�5?@��#@��-@�?}@��@�Ĝ@�Q�@��
@��R@�^5@�X@�j@���@��H@�M�@�?}@�9X@�|�@��y@�v�@�n�@�5?@�J@��h@�`B@���@��@��;@��F@���@���@��@�"�@��@��R@�^5@�@��T@���@��h@��@�7L@���@�Ĝ@��9@���@���@��D@��D@��D@�Q�@�(�@��@��;@�ƨ@��F@�|�@�dZ@�dZ@�dZ@�dZ@�l�@�dZ@�\)@�C�@�C�@�dZ@�;d@���@�ff@�M�@�-@��^@�p�@��@���@��@���@�j@�I�@� �@� �@�b@�  @�P@~�R@~$�@}�h@}�@|�/@|�j@|��@|�D@|z�@|Z@|(�@{��@{ƨ@{��@{t�@{@z�\@y��@x��@x��@x��@x��@xĜ@x�9@x�9@x��@x��@xQ�@w�;@w;d@v�R@vv�@v$�@v@u�@u��@uO�@u�@t��@sƨ@s33@rn�@rJ@q��@q��@q�7@qG�@q�@p��@pQ�@o;d@n$�@m�h@mO�@l��@l�@k�F@k33@j-@i�7@h��@hQ�@hb@h  @g�@g�;@g�w@gl�@f�R@e�T@d��@c��@b-@a�^@a��@ahs@a&�@a�@`��@`�9@`�u@`Q�@_��@_|�@_K�@_
=@^�y@^�@^�R@^ff@^V@^$�@^@]�T@]@]@]p�@\�j@[��@[o@Z��@Z��@ZM�@Y�#@Y��@Yx�@Yhs@Yhs@Yx�@Yhs@Yhs@X��@X1'@V$�@U�@T��@T�j@Tz�@T9X@T(�@T(�@T�@T1@T1@T1@T1@S��@S�
@R�H@Q�@Qx�@Q7L@Q%@P�9@P��@P�u@P�u@Pr�@PQ�@O�@O\)@Ol�@O�@M�@M`B@L��@LI�@L(�@Kƨ@K��@K�@Kt�@K33@J�@J~�@J�@I��@I��@Ihs@I7L@I�@H��@H��@H��@H��@Hr�@G�@G�@G�;@G��@G�w@G�@G�P@G|�@Gl�@G;d@F��@F��@F�+@FE�@E�-@E�@E�@D9X@D1@Cƨ@CdZ@C@B�\@B=q@A��@Ahs@Ahs@AG�@@Ĝ@@A�@?�P@?�@>��@>ff@>V@>V@>E�@>E�@>5?@=�T@=/@<��@<I�@;�m@;�@;C�@:�@9�@8 �@8  @8  @7�;@7�;@7�w@7��@7��@7�w@7�@7��@7�@7�@7�@7��@7\)@7K�@7;d@6��@6V@5@5O�@4�/@4�j@4��@4z�@4(�@41@3�m@3�F@3��@3�@3C�@2��@1�@01'@/
=@.�+@-�h@,9X@,I�@,j@,9X@,�@+ƨ@+33@+@*�H@*��@)�#@)�@(��@(�u@( �@( �@( �@(b@(  @'�@'�@'\)@'+@&�@&�R@&�R@&$�@$�/@$�@#��@#S�@#C�@#33@#"�@"�\@!�@!x�@!�@ r�@�@l�@+@�@��@ff@@p�@?}@�@V@�@��@1@�F@��@t�@S�@S�@C�@��@�\@�\@�\@�\@n�@^5@=q@�@��@�^@��@��@��@x�@hs@&�@��@��@Q�@1'@�@�w@��@\)@+@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�~�A�~�A�~�A�~�A�~�A�|�A�x�A�^5A�$�AξwAΧ�AΑhA�jA�K�A� �A��/A�K�A���A���A˓uA�C�A�oA���A���A�ZA�jA�oAƕ�A�|�Aô9A�{AA��A��#A��A��+A�z�A�;dA��A���A�+A�C�A��A�1'A��A��FA��A���A�v�A�\)A��A��A���A��DA�O�A���A�r�A��wA�^5A�hsA��jA��A�x�A��7A�ĜA�jA�A�ĜA��A���A�"�A��RA�ZA��/A���A��RA���A�ZA��/A���A�jA�\)A�p�A��9A��A�?}A��wA���A��-A�7LA�(�A�"�A�A��/A��\A�ƨA��A�(�A���A�7LA�1'A�JA�VA���A���A��+A���A���A�
=A�ffA��!A���A���A���A��;A�+A|��Az��Awt�Av��Au��At��AsoAq�TAqoAn~�Ak�Ah�+Ahv�Agt�Af^5Ad��AdZAc�;Ac�AbbA`�`A]�mA\Q�A[�hAYoAU�mAT�AS�7AR�HAR�AP�/AO��AN(�AKp�AI�PAHVAF��AF-AEhsAD�/ADA�AC�hAB�jAA�wAA%A@��A?�7A?7LA>r�A=�A=�;A=��A=A<~�A;�^A:=qA9�A8ĜA8~�A7\)A6  A3A2�A21A0�A/��A.�A-�A,��A+�mA+"�A*�A($�A&�A&{A$��A$^5A#�FA"�\A"bA �/An�A�A��A��AbNA�;A�hA��AO�A�A��A�\A  A�yAr�AI�A1'A  AG�Ap�A�+A�AQ�A5?A�Al�A�9A$�A/AƨA
�RA	Az�A��AA��A"�Az�A�AE�A�A��A�;A�TA��AK�A �A   @�+@��R@�-@�G�@��@�  @��R@�^5@�D@�^5@�h@�?}@�j@�1@�ƨ@��@�^5@�z�@�@߶F@��T@܃@�I�@���@�1@�\)@�
=@֧�@Ցh@щ7@�ff@�|�@ʸR@�{@��#@�-@���@�o@϶F@���@�E�@�O�@��
@�dZ@��@ʸR@ʇ+@�E�@�/@ȃ@�1'@�@�&�@ě�@�Z@���@��;@ÍP@�@��@�7L@��@�K�@��R@���@���@�/@�9X@�33@�{@���@��@��9@�bN@�A�@� �@�1@���@�@��R@��@��-@��@�%@��9@��;@��!@�-@��-@�j@��P@�S�@�@���@��#@���@���@���@�1'@��H@��@��9@��/@��@���@�/@�?}@�7L@�?}@�@���@��y@��@���@���@��y@��@���@�ȴ@��T@�Ĝ@���@���@���@���@���@���@��/@��/@��/@��/@��@��@�I�@���@�+@�o@��y@���@��\@��+@���@���@�o@�ff@��@���@���@��@���@���@�  @�@�o@���@��@���@���@�$�@�J@��@�{@�J@��T@�@���@���@��7@�x�@�hs@�7L@�/@�&�@���@��@��/@���@���@��@��u@��u@��u@��@�j@�Q�@�1'@��m@�+@��y@�ȴ@���@�n�@��@�`B@�7L@�V@���@��/@���@��@���@��@�Q�@�A�@��@���@��;@��
@���@��@�t�@�;d@�5?@��#@��-@�?}@��@�Ĝ@�Q�@��
@��R@�^5@�X@�j@���@��H@�M�@�?}@�9X@�|�@��y@�v�@�n�@�5?@�J@��h@�`B@���@��@��;@��F@���@���@��@�"�@��@��R@�^5@�@��T@���@��h@��@�7L@���@�Ĝ@��9@���@���@��D@��D@��D@�Q�@�(�@��@��;@�ƨ@��F@�|�@�dZ@�dZ@�dZ@�dZ@�l�@�dZ@�\)@�C�@�C�@�dZ@�;d@���@�ff@�M�@�-@��^@�p�@��@���@��@���@�j@�I�@� �@� �@�b@�  @�P@~�R@~$�@}�h@}�@|�/@|�j@|��@|�D@|z�@|Z@|(�@{��@{ƨ@{��@{t�@{@z�\@y��@x��@x��@x��@x��@xĜ@x�9@x�9@x��@x��@xQ�@w�;@w;d@v�R@vv�@v$�@v@u�@u��@uO�@u�@t��@sƨ@s33@rn�@rJ@q��@q��@q�7@qG�@q�@p��@pQ�@o;d@n$�@m�h@mO�@l��@l�@k�F@k33@j-@i�7@h��@hQ�@hb@h  @g�@g�;@g�w@gl�@f�R@e�T@d��@c��@b-@a�^@a��@ahs@a&�@a�@`��@`�9@`�u@`Q�@_��@_|�@_K�@_
=@^�y@^�@^�R@^ff@^V@^$�@^@]�T@]@]@]p�@\�j@[��@[o@Z��@Z��@ZM�@Y�#@Y��@Yx�@Yhs@Yhs@Yx�@Yhs@Yhs@X��@X1'@V$�@U�@T��@T�j@Tz�@T9X@T(�@T(�@T�@T1@T1@T1@T1@S��@S�
@R�H@Q�@Qx�@Q7L@Q%@P�9@P��@P�u@P�u@Pr�@PQ�@O�@O\)@Ol�@O�@M�@M`B@L��@LI�@L(�@Kƨ@K��@K�@Kt�@K33@J�@J~�@J�@I��@I��@Ihs@I7L@I�@H��@H��@H��@H��@Hr�@G�@G�@G�;@G��@G�w@G�@G�P@G|�@Gl�@G;d@F��@F��@F�+@FE�@E�-@E�@E�@D9X@D1@Cƨ@CdZ@C@B�\@B=q@A��@Ahs@Ahs@AG�@@Ĝ@@A�@?�P@?�@>��@>ff@>V@>V@>E�@>E�@>5?@=�T@=/@<��@<I�@;�m@;�@;C�@:�@9�@8 �@8  @8  @7�;@7�;@7�w@7��@7��@7�w@7�@7��@7�@7�@7�@7��@7\)@7K�@7;d@6��@6V@5@5O�@4�/@4�j@4��@4z�@4(�@41@3�m@3�F@3��@3�@3C�@2��@1�@01'@/
=@.�+@-�h@,9X@,I�@,j@,9X@,�@+ƨ@+33@+@*�H@*��@)�#@)�@(��@(�u@( �@( �@( �@(b@(  @'�@'�@'\)@'+@&�@&�R@&�R@&$�@$�/@$�@#��@#S�@#C�@#33@#"�@"�\@!�@!x�@!�@ r�@�@l�@+@�@��@ff@@p�@?}@�@V@�@��@1@�F@��@t�@S�@S�@C�@��@�\@�\@�\@�\@n�@^5@=q@�@��@�^@��@��@��@x�@hs@&�@��@��@Q�@1'@�@�w@��@\)@+@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
=qB
<jB
<jB
<jB
=qB
=qB
A�B
P�B
iyB
��B
��B
�B
�-B
�9B
�'B
�B
�B+B,B7LB?}BF�BT�Br�B�VB��B��B�3B��B��BhB!�B5?B9XB?}BA�BbNBx�B~�B� B�=B��B�bB� B{�B�Bt�B{�B�uB�oB�+B�B{�Br�Bn�BgmBaHB_;BYBP�BD�B@�B>wB>wB=qB<jBE�BP�BM�BT�BI�BP�BZBR�B:^B#�B�BB��B��B�B�fB�5B��B��B�^B�B��B��B��B��B��B��B�{B�hB�7Bw�Bk�B_;BYB\)B]/BW
B>wB-B�BPB
��B
��B
�B
�#B
�RB
��B
�bB
�B
dZB
N�B
J�B
8RB
2-B
/B
'�B
�B
uB
\B
B	��B	�5B	�)B	�B	��B	��B	ǮB	ÖB	��B	�qB	�?B	��B	�{B	�VB	�B	iyB	aHB	YB	R�B	N�B	F�B	>wB	6FB	(�B	�B	�B	\B	PB	
=B	1B	B	B��B��B��B��B��B��B�B�B�B�B�B�B�B�mB�ZB�NB�NB�BB�)B�
B��B��B��B��BƨBŢBÖB��B��B�wB�dB�XB�RB�jBBŢBƨBƨBȴBƨBǮBǮBȴB��B��B��B�B�)B�5B�5B�BB�NB�fB�NB�BB�;B�/B�/B�B�
B��B��B��B��B��BɺBȴB��B�B��B��B��B��B�BB�sB�B�B�fB�`B�B�sB�B�B��B�B�B�B�B�B�B�mB�TB�5B�NB�B�B�mB�NB�BB�;B�fB�`B�BB��B��B��B��BȴBB�}B�jB�FB�9B�3B�-B�B��B��B�PB�PB�PB�\B��B�B��B��B��B��B��B��B��B��B��B��BɺB��B��B��B��B�B�
B�B�B�B�B�)B�)B�5B�BB�mB�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B		7B		7B	
=B	
=B	DB	DB	JB	bB	�B	�B	�B	)�B	0!B	5?B	8RB	?}B	@�B	@�B	@�B	>wB	<jB	-B	&�B	0!B	33B	49B	8RB	<jB	>wB	>wB	?}B	G�B	O�B	S�B	T�B	T�B	T�B	VB	VB	VB	W
B	bNB	o�B	q�B	r�B	s�B	s�B	s�B	t�B	t�B	t�B	t�B	t�B	u�B	x�B	y�B	w�B	v�B	v�B	v�B	u�B	u�B	v�B	w�B	z�B	|�B	�B	�B	�B	�B	�B	�+B	�=B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�'B	�'B	�!B	�'B	�3B	�9B	�?B	�FB	�LB	�RB	�RB	�LB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�jB	�wB	��B	ÖB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�;B	�5B	�5B	�;B	�HB	�HB	�BB	�5B	�)B	�B	�B	�B	�B	�B	�/B	�5B	�BB	�BB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
+B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
PB
JB
JB
PB
PB
PB
VB
VB
\B
bB
oB
oB
oB
oB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
#�B
$�B
#�B
#�B
#�B
$�B
$�B
#�B
$�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
.B
.B
/B
/B
/B
/B
0!B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
33B
49B
49B
49B
49B
5?B
49B
49B
49B
5?B
49B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
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
;dB
;dB
<jB
<jB
<jB
<jB
<jB
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
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
C�B
D�B
D�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
H�B
H�B
H�B
I�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
N�B
O�B
O�B
P�B
R�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
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
XB
XB
YB
YB
ZB
YB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
=qB
<jB
<jB
<jB
=qB
=qB
A�B
P�B
iyB
��B
��B
�B
�-B
�9B
�'B
�B
�B+B,B7LB?}BF�BT�Br�B�VB��B��B�3B��B��BhB!�B5?B9XB?}BA�BbNBx�B~�B� B�=B��B�bB� B{�B�Bt�B{�B�uB�oB�+B�B{�Br�Bn�BgmBaHB_;BYBP�BD�B@�B>wB>wB=qB<jBE�BP�BM�BT�BI�BP�BZBR�B:^B#�B�BB��B��B�B�fB�5B��B��B�^B�B��B��B��B��B��B��B�{B�hB�7Bw�Bk�B_;BYB\)B]/BW
B>wB-B�BPB
��B
��B
�B
�#B
�RB
��B
�bB
�B
dZB
N�B
J�B
8RB
2-B
/B
'�B
�B
uB
\B
B	��B	�5B	�)B	�B	��B	��B	ǮB	ÖB	��B	�qB	�?B	��B	�{B	�VB	�B	iyB	aHB	YB	R�B	N�B	F�B	>wB	6FB	(�B	�B	�B	\B	PB	
=B	1B	B	B��B��B��B��B��B��B�B�B�B�B�B�B�B�mB�ZB�NB�NB�BB�)B�
B��B��B��B��BƨBŢBÖB��B��B�wB�dB�XB�RB�jBBŢBƨBƨBȴBƨBǮBǮBȴB��B��B��B�B�)B�5B�5B�BB�NB�fB�NB�BB�;B�/B�/B�B�
B��B��B��B��B��BɺBȴB��B�B��B��B��B��B�BB�sB�B�B�fB�`B�B�sB�B�B��B�B�B�B�B�B�B�mB�TB�5B�NB�B�B�mB�NB�BB�;B�fB�`B�BB��B��B��B��BȴBB�}B�jB�FB�9B�3B�-B�B��B��B�PB�PB�PB�\B��B�B��B��B��B��B��B��B��B��B��B��BɺB��B��B��B��B�B�
B�B�B�B�B�)B�)B�5B�BB�mB�B��B��B��B��B��B��B��B��B	  B	B	B	B	B	B		7B		7B	
=B	
=B	DB	DB	JB	bB	�B	�B	�B	)�B	0!B	5?B	8RB	?}B	@�B	@�B	@�B	>wB	<jB	-B	&�B	0!B	33B	49B	8RB	<jB	>wB	>wB	?}B	G�B	O�B	S�B	T�B	T�B	T�B	VB	VB	VB	W
B	bNB	o�B	q�B	r�B	s�B	s�B	s�B	t�B	t�B	t�B	t�B	t�B	u�B	x�B	y�B	w�B	v�B	v�B	v�B	u�B	u�B	v�B	w�B	z�B	|�B	�B	�B	�B	�B	�B	�+B	�=B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�'B	�'B	�!B	�'B	�3B	�9B	�?B	�FB	�LB	�RB	�RB	�LB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�jB	�wB	��B	ÖB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�;B	�5B	�5B	�;B	�HB	�HB	�BB	�5B	�)B	�B	�B	�B	�B	�B	�/B	�5B	�BB	�BB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
+B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
PB
JB
JB
PB
PB
PB
VB
VB
\B
bB
oB
oB
oB
oB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
#�B
$�B
#�B
#�B
#�B
$�B
$�B
#�B
$�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
.B
.B
/B
/B
/B
/B
0!B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
33B
49B
49B
49B
49B
5?B
49B
49B
49B
5?B
49B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
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
;dB
;dB
<jB
<jB
<jB
<jB
<jB
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
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
C�B
D�B
D�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
H�B
H�B
H�B
I�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
N�B
O�B
O�B
P�B
R�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
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
XB
XB
YB
YB
ZB
YB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210521110034                              AO  ARCAADJP                                                                    20210521110034    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210521110034  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210521110034  QCF$                G�O�G�O�G�O�0               