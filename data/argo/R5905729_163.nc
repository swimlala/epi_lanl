CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-10-13T09:01:13Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  `   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ox   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ۈ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۸   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޸   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20221013090113  20221013090113  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @���t�1   @��ֲ@��@*�l�C���dx�`A�71   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH��BP  BX  B_��Bh  BpffBx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�z@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B0\)B7��B?��BHBO��BW��B_�]Bg��Bp\)Bw��B�]B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw��Cy��C{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'x�D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[x�D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�/�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�5?A�5?A�7LA�5?A�5?A�$�A�%A�FA�1A���A��DA��A�z�A�`BA�XA�O�A�9XA� �A�  A��A���Aߟ�A�XAް!A݇+A�~�A��A�G�Aԛ�A���A��
A�/A�r�A�I�Aǉ7A�x�A�;dA�\)AþwA�"�A���A�ZA�A�jA���A�9XA��7A��TA�ĜA��A�v�A��DA�~�A�JA���A���A�z�A�1'A��A�/A��A���A�\)A��wA���A�%A�oA�I�A��#A��A��A�jA��PA��-A��!A�A�A|Av��As&�Al�`Aj��Ag
=Abv�A\�/AX�9AS�mAQ��AO�PAN{AJE�AI�AG%ACK�A?��A;A:��A8��A4��A3`BA2�HA2�A0�A0(�A/�mA/�wA.v�A-�7A*r�A'33A&A%;dA$�\A%\)A';dA'
=A&E�A&��A(Q�A*  A*VA)�A)S�A&~�A%�A%oA$ffA$1A#�PA#O�A#VA"�`A"��A"ZA!�PA ��A!��A ��A+A��A��A�!A�^At�A33A��AJA��Ax�AA��AO�A33AAQ�AbNAbA��A;dAx�A��A��A/A�DAA�-A�`A��A^5AM�A �A�^Al�A"�A��Av�A�;A��A\)A�`A�A�A^5AM�A�7A�`A�\A�^A��A
�yA	��A��AffAA�A�A��Av�A-A�7A�`A�AoA��A�7AE�AVA�;A  A�AI�A{A�;A�PA��AbA�
AƨA��AdZA/A �A �!A VA {@���@�o@���@��@��D@���@�ƨ@�@�ff@�x�@��@�z�@�(�@��P@���@�ff@���@��`@�Q�@�F@��@��@�J@���@��@�X@�@�F@��H@�ff@��-@���@���@��@�l�@�
=@�\@���@�@�&�@���@�b@�t�@�@���@�\@�$�@�`B@�9X@�dZ@���@�M�@�-@��@�9@�I�@�  @�dZ@ޟ�@���@�`B@���@܃@�9X@���@�K�@���@�~�@�J@٩�@�X@�%@ؓu@�9X@�  @���@�ȴ@պ^@�7L@���@�j@�ƨ@�dZ@��@҇+@�=q@�@���@У�@�9X@�ƨ@���@Ώ\@�=q@��@��@�@͡�@�x�@�&�@̛�@��@���@�~�@�ff@��@ɡ�@��@ǶF@�@���@���@�@Ł@�O�@��@��/@�Z@�j@��
@�\)@���@\@�5?@�J@��7@�?}@��u@�t�@�@��y@�E�@���@�p�@��/@��D@�Q�@�1@��m@��w@�\)@��!@�=q@�@���@�hs@�&�@�I�@��
@���@�dZ@��@�n�@�`B@�&�@�%@���@�(�@���@�"�@��@���@�ff@���@��7@�?}@��@��@���@�1@��
@�l�@���@��R@�$�@�@��@��#@�%@�j@�b@��w@��@��@��P@��P@�dZ@��@�hs@��`@���@��9@��@� �@��;@���@�33@�ff@�-@��@�@���@��7@�G�@���@��/@��j@�Z@�ƨ@��@��@�+@�V@��7@�G�@�%@��u@��@�r�@�j@�A�@� �@��@�+@���@��\@�V@�=q@�$�@���@��T@��^@�X@�bN@�  @��m@�ƨ@�l�@�"�@��\@���@��7@�&�@���@��@���@���@��u@�z�@�z�@�A�@��@��m@���@�l�@�dZ@�\)@��@�@��@��@���@�ff@�5?@�@���@��@��
@�|�@�dZ@�o@�ȴ@���@��\@�v�@�^5@�5?@�J@��T@��^@��h@�X@�?}@�&�@���@��@��m@��w@�dZ@�+@�@��R@��+@�^5@��@�@��h@�p�@�`B@�&�@��@��@�Z@�1@���@�\)@��H@���@�ff@�^5@�=q@��@��^@���@��@�V@��@�r�@�  @�K�@���@�5?@�@�p�@�V@���@���@��@��@��;@���@�dZ@�+@���@��!@�V@�-@��@���@��h@�hs@�&�@�%@��j@�r�@�1'@�b@�@~��@~$�@}��@}@}p�@|��@|��@|(�@{�@z��@zM�@zJ@yx�@y�@xĜ@xr�@w�@wl�@v�y@vv�@v@u��@u@u��@t��@t�D@tZ@t1@s�F@s�@sS�@so@r�!@r�\@rn�@rJ@q�^@qx�@q7L@p�`@p��@pA�@o�w@n��@m�T@mp�@mV@l�@l(�@kƨ@k��@k��@k�@kt�@kS�@k"�@j~�@j-@j�@i�@i��@i�7@iX@i7L@h�`@h�@h1'@g�w@f�@f5?@e��@d�/@d�D@d9X@d9X@c�m@c��@c��@c�@c�@ct�@cS�@b��@a�@a��@a��@aX@`�9@`�@`r�@`r�@`bN@`A�@`  @_��@_;d@^ȴ@^E�@]�-@]?}@]V@\�/@\�j@\z�@\9X@[�F@Z�@Z��@Zn�@Y�@Y��@Y7L@Y�@X�u@X �@W�@V�y@V�@Vv�@V@U@U�@S��@Sƨ@S��@S"�@R��@R^5@RM�@Q�^@P�`@P1'@O�;@O\)@N��@Nv�@N{@M�T@M�-@M`B@L��@L1@K�
@K�@KS�@K"�@K@J��@J�@I��@I��@Ihs@H��@Hr�@G�;@G+@F�R@F��@Fv�@E��@E/@D�/@D��@D�D@D9X@C��@C"�@B��@Bn�@B�@A��@A��@A&�@@��@@Ĝ@@�9@@�u@@�u@@�u@@A�@@b@?�@?�;@?�P@?K�@?�@?
=@>�y@>V@>@=@=?}@<��@<��@<1@;�@;dZ@;dZ@;S�@;C�@:�H@:=q@9�#@9G�@8��@8�u@8Q�@8b@7�w@7|�@7�@6��@6ff@6@5�h@5/@4I�@3��@3dZ@2��@2�@1��@1��@1x�@1hs@17L@1�@1�@1�@1%@0�9@01'@0  @0  @/��@/�P@/
=@.v�@.5?@.E�@.V@.V@.{@.@.@-��@-�@,��@,I�@,(�@,1@+ƨ@+�@+t�@+C�@*�@*�!@*~�@*M�@*=q@*-@*J@)�#@)�^@)�7@)x�@)x�@)G�@(��@(r�@(b@'�w@'\)@';d@'+@&�@&�+@&ff@&ff@&V@&V@&{@%�@%@%�-@%p�@$��@$9X@#�
@#�F@#�@#C�@"�@"�!@"�\@"n�@"^5@"-@"J@!�#@!��@!x�@!hs@!�@ ��@ ��@ �9@ �u@ �@ r�@ bN@ bN@ 1'@   @�;@�;@��@�w@��@|�@K�@+@��@�y@ȴ@��@ff@ff@V@E�@5?@5?@{@@@@�@��@p�@/@��@j@ƨ@dZ@C�@o@@��@��@~�@�@�7@�@��@�9@bN@1'@ �@�@�;@��@�P@K�@
=@�+@5?@�@�T@�T@�T@�T@��@�h@p�@�@��@�@��@I�@�m@dZ@S�@"�@��@��@�\@^5@=q@=q@�@��@hs@G�@7L@7L@7L@&�@�@��@Ĝ@r�@Q�@1'@ �@  @�;@�@K�@+@�@v�@ff@ff@$�@{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1'A�5?A�5?A�7LA�5?A�5?A�$�A�%A�FA�1A���A��DA��A�z�A�`BA�XA�O�A�9XA� �A�  A��A���Aߟ�A�XAް!A݇+A�~�A��A�G�Aԛ�A���A��
A�/A�r�A�I�Aǉ7A�x�A�;dA�\)AþwA�"�A���A�ZA�A�jA���A�9XA��7A��TA�ĜA��A�v�A��DA�~�A�JA���A���A�z�A�1'A��A�/A��A���A�\)A��wA���A�%A�oA�I�A��#A��A��A�jA��PA��-A��!A�A�A|Av��As&�Al�`Aj��Ag
=Abv�A\�/AX�9AS�mAQ��AO�PAN{AJE�AI�AG%ACK�A?��A;A:��A8��A4��A3`BA2�HA2�A0�A0(�A/�mA/�wA.v�A-�7A*r�A'33A&A%;dA$�\A%\)A';dA'
=A&E�A&��A(Q�A*  A*VA)�A)S�A&~�A%�A%oA$ffA$1A#�PA#O�A#VA"�`A"��A"ZA!�PA ��A!��A ��A+A��A��A�!A�^At�A33A��AJA��Ax�AA��AO�A33AAQ�AbNAbA��A;dAx�A��A��A/A�DAA�-A�`A��A^5AM�A �A�^Al�A"�A��Av�A�;A��A\)A�`A�A�A^5AM�A�7A�`A�\A�^A��A
�yA	��A��AffAA�A�A��Av�A-A�7A�`A�AoA��A�7AE�AVA�;A  A�AI�A{A�;A�PA��AbA�
AƨA��AdZA/A �A �!A VA {@���@�o@���@��@��D@���@�ƨ@�@�ff@�x�@��@�z�@�(�@��P@���@�ff@���@��`@�Q�@�F@��@��@�J@���@��@�X@�@�F@��H@�ff@��-@���@���@��@�l�@�
=@�\@���@�@�&�@���@�b@�t�@�@���@�\@�$�@�`B@�9X@�dZ@���@�M�@�-@��@�9@�I�@�  @�dZ@ޟ�@���@�`B@���@܃@�9X@���@�K�@���@�~�@�J@٩�@�X@�%@ؓu@�9X@�  @���@�ȴ@պ^@�7L@���@�j@�ƨ@�dZ@��@҇+@�=q@�@���@У�@�9X@�ƨ@���@Ώ\@�=q@��@��@�@͡�@�x�@�&�@̛�@��@���@�~�@�ff@��@ɡ�@��@ǶF@�@���@���@�@Ł@�O�@��@��/@�Z@�j@��
@�\)@���@\@�5?@�J@��7@�?}@��u@�t�@�@��y@�E�@���@�p�@��/@��D@�Q�@�1@��m@��w@�\)@��!@�=q@�@���@�hs@�&�@�I�@��
@���@�dZ@��@�n�@�`B@�&�@�%@���@�(�@���@�"�@��@���@�ff@���@��7@�?}@��@��@���@�1@��
@�l�@���@��R@�$�@�@��@��#@�%@�j@�b@��w@��@��@��P@��P@�dZ@��@�hs@��`@���@��9@��@� �@��;@���@�33@�ff@�-@��@�@���@��7@�G�@���@��/@��j@�Z@�ƨ@��@��@�+@�V@��7@�G�@�%@��u@��@�r�@�j@�A�@� �@��@�+@���@��\@�V@�=q@�$�@���@��T@��^@�X@�bN@�  @��m@�ƨ@�l�@�"�@��\@���@��7@�&�@���@��@���@���@��u@�z�@�z�@�A�@��@��m@���@�l�@�dZ@�\)@��@�@��@��@���@�ff@�5?@�@���@��@��
@�|�@�dZ@�o@�ȴ@���@��\@�v�@�^5@�5?@�J@��T@��^@��h@�X@�?}@�&�@���@��@��m@��w@�dZ@�+@�@��R@��+@�^5@��@�@��h@�p�@�`B@�&�@��@��@�Z@�1@���@�\)@��H@���@�ff@�^5@�=q@��@��^@���@��@�V@��@�r�@�  @�K�@���@�5?@�@�p�@�V@���@���@��@��@��;@���@�dZ@�+@���@��!@�V@�-@��@���@��h@�hs@�&�@�%@��j@�r�@�1'@�b@�@~��@~$�@}��@}@}p�@|��@|��@|(�@{�@z��@zM�@zJ@yx�@y�@xĜ@xr�@w�@wl�@v�y@vv�@v@u��@u@u��@t��@t�D@tZ@t1@s�F@s�@sS�@so@r�!@r�\@rn�@rJ@q�^@qx�@q7L@p�`@p��@pA�@o�w@n��@m�T@mp�@mV@l�@l(�@kƨ@k��@k��@k�@kt�@kS�@k"�@j~�@j-@j�@i�@i��@i�7@iX@i7L@h�`@h�@h1'@g�w@f�@f5?@e��@d�/@d�D@d9X@d9X@c�m@c��@c��@c�@c�@ct�@cS�@b��@a�@a��@a��@aX@`�9@`�@`r�@`r�@`bN@`A�@`  @_��@_;d@^ȴ@^E�@]�-@]?}@]V@\�/@\�j@\z�@\9X@[�F@Z�@Z��@Zn�@Y�@Y��@Y7L@Y�@X�u@X �@W�@V�y@V�@Vv�@V@U@U�@S��@Sƨ@S��@S"�@R��@R^5@RM�@Q�^@P�`@P1'@O�;@O\)@N��@Nv�@N{@M�T@M�-@M`B@L��@L1@K�
@K�@KS�@K"�@K@J��@J�@I��@I��@Ihs@H��@Hr�@G�;@G+@F�R@F��@Fv�@E��@E/@D�/@D��@D�D@D9X@C��@C"�@B��@Bn�@B�@A��@A��@A&�@@��@@Ĝ@@�9@@�u@@�u@@�u@@A�@@b@?�@?�;@?�P@?K�@?�@?
=@>�y@>V@>@=@=?}@<��@<��@<1@;�@;dZ@;dZ@;S�@;C�@:�H@:=q@9�#@9G�@8��@8�u@8Q�@8b@7�w@7|�@7�@6��@6ff@6@5�h@5/@4I�@3��@3dZ@2��@2�@1��@1��@1x�@1hs@17L@1�@1�@1�@1%@0�9@01'@0  @0  @/��@/�P@/
=@.v�@.5?@.E�@.V@.V@.{@.@.@-��@-�@,��@,I�@,(�@,1@+ƨ@+�@+t�@+C�@*�@*�!@*~�@*M�@*=q@*-@*J@)�#@)�^@)�7@)x�@)x�@)G�@(��@(r�@(b@'�w@'\)@';d@'+@&�@&�+@&ff@&ff@&V@&V@&{@%�@%@%�-@%p�@$��@$9X@#�
@#�F@#�@#C�@"�@"�!@"�\@"n�@"^5@"-@"J@!�#@!��@!x�@!hs@!�@ ��@ ��@ �9@ �u@ �@ r�@ bN@ bN@ 1'@   @�;@�;@��@�w@��@|�@K�@+@��@�y@ȴ@��@ff@ff@V@E�@5?@5?@{@@@@�@��@p�@/@��@j@ƨ@dZ@C�@o@@��@��@~�@�@�7@�@��@�9@bN@1'@ �@�@�;@��@�P@K�@
=@�+@5?@�@�T@�T@�T@�T@��@�h@p�@�@��@�@��@I�@�m@dZ@S�@"�@��@��@�\@^5@=q@=q@�@��@hs@G�@7L@7L@7L@&�@�@��@Ĝ@r�@Q�@1'@ �@  @�;@�@K�@+@�@v�@ff@ff@$�@{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�XB	�XB	�XB	�RB	�LB	�?B	�3B	�!B	�!B	�jB	ǮB	ȴB	ǮB	ƨB	ƨB	ŢB	ÖB	��B	��B	��B	��B	��B	B	ƨB
+B
W
B
S�B
XB
�7B
�LB
�HBXB�\B�
B��BJB&�B?}BL�BR�BJ�B;dBM�B`BBM�BVB|�B�=B�B�VB� BhsBQ�B-B��BŢB��B��B�NB�BB��B��B�RB�hB[#B�B�B  B
�
B
ĜB
��B
u�B
|�B
:^B
�B	�ZB	�qB	��B	�oB	p�B	{�B	YB	>wB	�B	hB��B	B	
=B	+B�B��B�B��B��B��B�ZB�/BŢB�HB�B�B�fB��B	\B	�B	�B	{B�B�HB��B	%B	oB	7LB	O�B	q�B	r�B	�uB	��B	B	��B	��B	�B	��B	ŢB	��B	ɺB	��B	�B	�#B	�;B	�TB	�B	��B	��B	��B
+B
B	��B	��B	��B	��B
B

=B

=B
	7B

=B
oB
�B
�B
#�B
!�B
&�B
&�B
#�B
)�B
.B
.B
0!B
8RB
A�B
B�B
=qB
8RB
8RB
9XB
6FB
;dB
B�B
F�B
E�B
D�B
E�B
D�B
C�B
A�B
?}B
?}B
?}B
:^B
9XB
8RB
6FB
49B
.B
'�B
'�B
�B
�B
+B
B
  B
B	��B	��B
B
B
  B	��B	��B	��B	�B	�yB	�HB	�B	��B
\B
!�B
'�B
)�B
-B
+B
)�B
$�B
#�B
�B
!�B
%�B
&�B
%�B
&�B
%�B
&�B
$�B
#�B
#�B
!�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
hB
oB
uB
{B
uB
hB
PB
JB
VB
VB
\B
VB
VB
\B
\B
VB
PB

=B

=B
JB
VB
hB
oB
bB
\B
\B
\B
VB
VB
VB
\B
VB
VB
VB
JB
	7B
1B
DB
PB
DB
DB
JB
DB
JB
JB

=B
1B
DB
DB

=B
	7B
JB
PB
VB
VB
VB
VB
PB
DB
	7B
	7B
1B
\B
hB
\B
PB

=B
+B

=B
+B
JB
PB
JB
DB
DB
JB
VB
hB
bB
VB
VB
bB
bB
\B
PB
JB
	7B
B
	7B
DB
	7B
	7B
DB
DB
VB
hB
uB
{B
{B
uB
{B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
�B
�B
"�B
"�B
#�B
%�B
%�B
$�B
#�B
!�B
�B
%�B
)�B
)�B
(�B
'�B
'�B
'�B
'�B
%�B
)�B
,B
,B
+B
)�B
+B
+B
,B
+B
)�B
(�B
,B
+B
(�B
&�B
'�B
,B
-B
,B
/B
/B
/B
.B
-B
+B
-B
.B
/B
/B
0!B
0!B
/B
/B
.B
-B
+B
/B
1'B
0!B
.B
.B
.B
-B
/B
0!B
1'B
33B
49B
49B
49B
33B
33B
2-B
2-B
2-B
2-B
33B
49B
49B
33B
33B
49B
33B
2-B
2-B
1'B
0!B
.B
2-B
2-B
49B
6FB
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
5?B
7LB
9XB
:^B
9XB
:^B
;dB
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
;dB
;dB
<jB
<jB
<jB
;dB
<jB
<jB
>wB
?}B
?}B
?}B
>wB
?}B
?}B
?}B
>wB
?}B
=qB
=qB
<jB
>wB
@�B
@�B
B�B
B�B
D�B
D�B
D�B
C�B
E�B
E�B
E�B
E�B
F�B
E�B
F�B
G�B
H�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
J�B
I�B
J�B
L�B
M�B
L�B
L�B
L�B
K�B
K�B
K�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
P�B
P�B
O�B
N�B
O�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
R�B
R�B
S�B
R�B
Q�B
P�B
Q�B
S�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
VB
T�B
S�B
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
VB
VB
VB
W
B
VB
W
B
W
B
XB
W
B
W
B
XB
YB
YB
YB
XB
W
B
W
B
YB
YB
XB
W
B
ZB
ZB
[#B
[#B
[#B
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
[#B
[#B
[#B
ZB
[#B
\)B
]/B
\)B
\)B
\)B
]/B
\)B
]/B
\)B
^5B
^5B
^5B
^5B
_;B
^5B
]/B
`BB
`BB
`BB
`BB
aHB
bNB
`BB
_;B
`BB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
bNB
bNB
aHB
dZB
cTB
dZB
dZB
dZB
dZB
cTB
e`B
e`B
e`B
dZB
dZB
e`B
e`B
ffB
gmB
ffB
e`B
gmB
gmB
hsB
hsB
hsB
gmB
gmB
hsB
hsB
iyB
iyB
hsB
iyB
jB
k�B
k�B
k�B
k�B
k�B
jB
jB
k�B
k�B
jB
jB
k�B
k�B
k�B
jB
k�B
l�B
k�B
l�B
m�B
l�B
m�B
o�B
o�B
o�B
n�B
m�B
m�B
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
q�B
q�B
q�B
q�B
p�B
r�B
s�B
s�B
s�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
t�B
v�B
v�B
v�B
u�B
u�B
v�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
x�B
y�B
z�B
{�B
{�B
{�B
{�B
|�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
|�B
}�B
}�B
~�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
}�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�%B
�%B
�+B
�7B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�1B
�1B
�7B
�1B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�=B
�DB
�DB
�DB
�JB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�JB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�\B
�bB
�hB
�hB
�hB
�oB
�o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�XB	�XB	�XB	�RB	�LB	�?B	�3B	�!B	�!B	�jB	ǮB	ȴB	ǮB	ƨB	ƨB	ŢB	ÖB	��B	��B	��B	��B	��B	B	ƨB
+B
W
B
S�B
XB
�7B
�LB
�HBXB�\B�
B��BJB&�B?}BL�BR�BJ�B;dBM�B`BBM�BVB|�B�=B�B�VB� BhsBQ�B-B��BŢB��B��B�NB�BB��B��B�RB�hB[#B�B�B  B
�
B
ĜB
��B
u�B
|�B
:^B
�B	�ZB	�qB	��B	�oB	p�B	{�B	YB	>wB	�B	hB��B	B	
=B	+B�B��B�B��B��B��B�ZB�/BŢB�HB�B�B�fB��B	\B	�B	�B	{B�B�HB��B	%B	oB	7LB	O�B	q�B	r�B	�uB	��B	B	��B	��B	�B	��B	ŢB	��B	ɺB	��B	�B	�#B	�;B	�TB	�B	��B	��B	��B
+B
B	��B	��B	��B	��B
B

=B

=B
	7B

=B
oB
�B
�B
#�B
!�B
&�B
&�B
#�B
)�B
.B
.B
0!B
8RB
A�B
B�B
=qB
8RB
8RB
9XB
6FB
;dB
B�B
F�B
E�B
D�B
E�B
D�B
C�B
A�B
?}B
?}B
?}B
:^B
9XB
8RB
6FB
49B
.B
'�B
'�B
�B
�B
+B
B
  B
B	��B	��B
B
B
  B	��B	��B	��B	�B	�yB	�HB	�B	��B
\B
!�B
'�B
)�B
-B
+B
)�B
$�B
#�B
�B
!�B
%�B
&�B
%�B
&�B
%�B
&�B
$�B
#�B
#�B
!�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
hB
oB
uB
{B
uB
hB
PB
JB
VB
VB
\B
VB
VB
\B
\B
VB
PB

=B

=B
JB
VB
hB
oB
bB
\B
\B
\B
VB
VB
VB
\B
VB
VB
VB
JB
	7B
1B
DB
PB
DB
DB
JB
DB
JB
JB

=B
1B
DB
DB

=B
	7B
JB
PB
VB
VB
VB
VB
PB
DB
	7B
	7B
1B
\B
hB
\B
PB

=B
+B

=B
+B
JB
PB
JB
DB
DB
JB
VB
hB
bB
VB
VB
bB
bB
\B
PB
JB
	7B
B
	7B
DB
	7B
	7B
DB
DB
VB
hB
uB
{B
{B
uB
{B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
!�B
�B
�B
"�B
"�B
#�B
%�B
%�B
$�B
#�B
!�B
�B
%�B
)�B
)�B
(�B
'�B
'�B
'�B
'�B
%�B
)�B
,B
,B
+B
)�B
+B
+B
,B
+B
)�B
(�B
,B
+B
(�B
&�B
'�B
,B
-B
,B
/B
/B
/B
.B
-B
+B
-B
.B
/B
/B
0!B
0!B
/B
/B
.B
-B
+B
/B
1'B
0!B
.B
.B
.B
-B
/B
0!B
1'B
33B
49B
49B
49B
33B
33B
2-B
2-B
2-B
2-B
33B
49B
49B
33B
33B
49B
33B
2-B
2-B
1'B
0!B
.B
2-B
2-B
49B
6FB
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
5?B
7LB
9XB
:^B
9XB
:^B
;dB
:^B
;dB
;dB
;dB
;dB
<jB
<jB
=qB
;dB
;dB
<jB
<jB
<jB
;dB
<jB
<jB
>wB
?}B
?}B
?}B
>wB
?}B
?}B
?}B
>wB
?}B
=qB
=qB
<jB
>wB
@�B
@�B
B�B
B�B
D�B
D�B
D�B
C�B
E�B
E�B
E�B
E�B
F�B
E�B
F�B
G�B
H�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
J�B
I�B
J�B
L�B
M�B
L�B
L�B
L�B
K�B
K�B
K�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
P�B
P�B
O�B
N�B
O�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
R�B
R�B
S�B
R�B
Q�B
P�B
Q�B
S�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
VB
T�B
S�B
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
VB
VB
VB
W
B
VB
W
B
W
B
XB
W
B
W
B
XB
YB
YB
YB
XB
W
B
W
B
YB
YB
XB
W
B
ZB
ZB
[#B
[#B
[#B
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
[#B
[#B
[#B
ZB
[#B
\)B
]/B
\)B
\)B
\)B
]/B
\)B
]/B
\)B
^5B
^5B
^5B
^5B
_;B
^5B
]/B
`BB
`BB
`BB
`BB
aHB
bNB
`BB
_;B
`BB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
bNB
bNB
aHB
dZB
cTB
dZB
dZB
dZB
dZB
cTB
e`B
e`B
e`B
dZB
dZB
e`B
e`B
ffB
gmB
ffB
e`B
gmB
gmB
hsB
hsB
hsB
gmB
gmB
hsB
hsB
iyB
iyB
hsB
iyB
jB
k�B
k�B
k�B
k�B
k�B
jB
jB
k�B
k�B
jB
jB
k�B
k�B
k�B
jB
k�B
l�B
k�B
l�B
m�B
l�B
m�B
o�B
o�B
o�B
n�B
m�B
m�B
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
q�B
q�B
q�B
q�B
p�B
r�B
s�B
s�B
s�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
t�B
v�B
v�B
v�B
u�B
u�B
v�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
x�B
y�B
z�B
{�B
{�B
{�B
{�B
|�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
|�B
}�B
}�B
~�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
}�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�%B
�%B
�+B
�7B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�1B
�1B
�7B
�1B
�7B
�=B
�DB
�DB
�DB
�DB
�DB
�=B
�DB
�DB
�DB
�JB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�JB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�\B
�bB
�hB
�hB
�hB
�oB
�o1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221013090113                              AO  ARCAADJP                                                                    20221013090113    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221013090113  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221013090113  QCF$                G�O�G�O�G�O�4000            