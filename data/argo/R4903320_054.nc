CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-10-02T12:00:50Z creation      
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
resolution        =���   axis      Z        |  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  `(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ̐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20211002120050  20211002120050  4903320 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               6A   AO  8282                            2B  A   NAVIS_A                         1161                            170425                          863 @ٗݺ��1   @ٗ�W:�@9O�;dZ�c�Q��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         6A   A   A   @���@�  A   AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�3D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@��@��A=pA>=pA_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bx\)B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC4C5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D\D�\D\D�\D	x�D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DL�DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D��D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D���D���D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A� �A�33A�9XA�O�A�O�A�K�A�G�A�E�A�+A�$�A��A�%A��TA���A�ĜA�A�A϶FAϑhAκ^A���A���A�{A�JA��-A��DA��!A��FA��A�&�A��uA���A�{A��A�"�A���A��A���A��A�VA��mA��A�A�&�A��TA��A��A��A�~�A��TA��hA���A�+A�%A���A�l�A�?}A�~�A��A�ĜA�x�A�{A��A�Q�A�A���A�ZA�jA��yA�XA��^A���A��-A��PA�"�A�A��`A���A�VA��A�~�A��#A�K�A�ȴA�n�A�~�A���A��7A��A��hA�VA��#A���A�-A�K�A���A��PA��
A���A���A�v�A�ZA�O�A�O�A���A�x�A��RA�p�A��+AS�A}�A{VAu�#Aqt�An��AlZAkdZAj��Ah��AdjAb�RAaK�A_A^�DA\��A[x�AY�^AW�;AU�^ATVAS/AR�AQ�-AO�#AM��AL�/AL$�AJ��AI33AHffAG%AE��ADVAChsABz�AA�^AA7LA@��A?�A>ffA<�!A:�A8��A8�A7��A6$�A5l�A4�A4�uA3A2��A1�#A0��A0bNA0-A0A/��A/��A.�A,��A+x�A+oA)�wA(r�A'�;A'��A&��A%|�A$�RA$E�A#A"�A!�;A!33A v�A�A�yAffAJA�FAl�AjA/AVA�PA~�AJA��A?}AjA�hA��A�A;dA��A�yA9XAC�AffAVA9XA�hA�A33A
��A
=qA	�A�DA��AJA��AA�-Az�AS�A ffA {@�E�@�?}@�Z@���@�K�@�7L@��m@�M�@���@�K�@���@�5?@�+@�`B@��@�E�@���@�D@�  @�^5@��@��@�z�@��@�{@ݲ-@��/@���@��y@�-@٩�@؛�@�dZ@ְ!@�~�@��#@�p�@�V@�r�@�33@ҏ\@��@ύP@�;d@��@��@��@�@�ȴ@�=q@���@�@�1'@�"�@�ȴ@�@ȃ@ȼj@ȣ�@ǶF@���@�Q�@�33@���@�v�@�X@�Z@���@���@�O�@�33@���@���@�(�@���@���@�;d@�n�@�5?@�Ĝ@��@�n�@��;@���@���@��@�1'@�9X@��@�
=@�-@�X@�V@���@���@�@�5?@��#@�p�@�;d@�bN@���@�;d@���@��+@��+@��+@�V@��^@�`B@�hs@�r�@�K�@��^@��@��@�b@�Z@���@�M�@���@��@�@�n�@�o@�33@�l�@�  @���@��`@���@�z�@�A�@��;@���@�K�@��@���@��7@��@�p�@�O�@�`B@��m@�\)@�"�@�n�@��@��@���@�7L@��@�bN@� �@�A�@�r�@�  @�ƨ@�1@��P@��
@�  @��@�(�@�  @��
@��
@���@�"�@��+@�M�@��@�X@��D@�b@���@�ƨ@��@��y@��\@�V@��T@�`B@��@���@��@���@���@�9X@��m@���@���@��@�\)@�C�@��@��!@��+@�^5@�{@��-@�x�@�`B@�X@��@���@��@�Z@�1@��m@��;@��
@��w@��P@�l�@�;d@�"�@���@���@�ff@�V@�n�@�v�@�ff@�=q@�$�@��@���@��-@�/@���@���@��@�Z@�9X@� �@�  @�ƨ@���@�dZ@�C�@��@�@��@��@���@���@���@�M�@�5?@�J@��T@���@��-@�hs@�G�@�7L@���@���@���@�z�@�I�@�1'@� �@�  @�P@;d@~��@~@}�@}/@}V@|�@|�D@{ƨ@{"�@zn�@y�7@yG�@x��@w��@wK�@v��@v��@vff@vE�@v$�@u��@u�@u/@uV@t��@s��@s"�@s@r��@r�\@r=q@q�#@q�@o�@nȴ@m�-@m�@m?}@l��@l�@k��@k��@ko@jn�@i�#@i��@h��@hA�@g�@g\)@g�@f��@f�R@f$�@e�h@eO�@d��@d�@d�D@d(�@c��@c�F@cdZ@c33@c33@c"�@co@b��@b~�@bJ@a��@a��@a��@`bN@_��@_\)@_+@_+@_+@_
=@^�R@^�+@^5?@^5?@^@]�T@]��@]@]��@]?}@\z�@\9X@[��@[��@[C�@Z�H@Z�!@Z�\@Z~�@Z=q@Y�@Y�^@Y��@Yx�@YX@Y�@X��@X��@X��@X�`@X��@X�9@Xb@W��@W�P@WK�@W
=@V�@V��@U�@U�h@U�@TZ@T(�@S��@S�m@S��@St�@SS�@S33@S33@R��@R^5@R^5@R^5@RM�@R�@Q��@Q��@QG�@P�`@PĜ@P�u@PA�@Pb@O�w@O|�@O�@N�R@NV@N{@M��@Mp�@M/@MV@MV@L�/@Lz�@LI�@L1@Kƨ@KdZ@K"�@J�@J��@J�\@JM�@I�#@Ihs@H��@H��@H�u@Hr�@HA�@H  @G�@F��@F�@F��@Fv�@FV@FE�@E�@EV@Dz�@C��@C��@CS�@CS�@CC�@B�H@B��@Bn�@B=q@A��@A�7@@��@@bN@@ �@?�@?l�@?\)@?\)@?+@>��@>ȴ@>$�@=�T@=@=��@=p�@=�@<�/@<�D@<�D@<9X@;�m@;t�@;33@:�@:��@:M�@:-@:�@9�@9x�@8bN@8b@7l�@6��@6��@6$�@5��@5�@4�/@4�@4�D@4j@4Z@4(�@3ƨ@3dZ@3o@2�H@2�\@2M�@2-@1��@17L@1%@0��@0�u@/�;@/�@/|�@/\)@/\)@/\)@/K�@/;d@/
=@.ȴ@.�+@.E�@.@-@-`B@-V@,��@,��@,z�@,9X@,�@+��@+ƨ@+�F@+��@+dZ@+33@*�@*�!@*n�@)��@)�@)��@)�^@)�^@)�7@)X@)&�@)%@(�9@(r�@(Q�@(A�@( �@'�@'�;@'��@'�@'|�@'K�@'
=@&ȴ@&��@&v�@&$�@%�h@%p�@%p�@%`B@%?}@%�@$��@$�@$�@$�/@$�@$�D@$z�@$Z@$9X@#�m@#t�@#33@#o@"�@"�H@"�!@"M�@"-@"J@!�#@!��@!�7@!X@!�@ Ĝ@ Q�@ b@�@�w@�P@l�@+@
=@��@�y@��@5?@@@�h@`B@O�@O�@�/@z�@I�@(�@�@1@�
@�@S�@"�@��@��@��@n�@M�@-@�#@&�@Ĝ@bN@b@�@�w@�P@l�@;d@�@��@�@�R@v�@ff@5?@{@��@�h@�@�@z�@9X@�@��@�
@�F@�@"�@@��@��@~�@-@�^@hs@&�@��@�9@r�@A�@ �@�;@l�@+@�@��@�R@E�@@��@�-@��@`B@/@�@V@�/@�j@�D@z�@��@�
@�
@�
@�
@�
@��@C�@33@o@
��@
=q@	��@	��@	x�@	&�@��@��@Ĝ@�9@r�@A�@�@��@��@|�@|�@\)@\)@K�@+@�@��@�y@ȴ@�R@��@�+@ff@V@V@V@V@V@V@$�@@�-@O�@�@V@�/@�D@j@Z@(�@1@1@�
@�@S�@dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A� �A�33A�9XA�O�A�O�A�K�A�G�A�E�A�+A�$�A��A�%A��TA���A�ĜA�A�A϶FAϑhAκ^A���A���A�{A�JA��-A��DA��!A��FA��A�&�A��uA���A�{A��A�"�A���A��A���A��A�VA��mA��A�A�&�A��TA��A��A��A�~�A��TA��hA���A�+A�%A���A�l�A�?}A�~�A��A�ĜA�x�A�{A��A�Q�A�A���A�ZA�jA��yA�XA��^A���A��-A��PA�"�A�A��`A���A�VA��A�~�A��#A�K�A�ȴA�n�A�~�A���A��7A��A��hA�VA��#A���A�-A�K�A���A��PA��
A���A���A�v�A�ZA�O�A�O�A���A�x�A��RA�p�A��+AS�A}�A{VAu�#Aqt�An��AlZAkdZAj��Ah��AdjAb�RAaK�A_A^�DA\��A[x�AY�^AW�;AU�^ATVAS/AR�AQ�-AO�#AM��AL�/AL$�AJ��AI33AHffAG%AE��ADVAChsABz�AA�^AA7LA@��A?�A>ffA<�!A:�A8��A8�A7��A6$�A5l�A4�A4�uA3A2��A1�#A0��A0bNA0-A0A/��A/��A.�A,��A+x�A+oA)�wA(r�A'�;A'��A&��A%|�A$�RA$E�A#A"�A!�;A!33A v�A�A�yAffAJA�FAl�AjA/AVA�PA~�AJA��A?}AjA�hA��A�A;dA��A�yA9XAC�AffAVA9XA�hA�A33A
��A
=qA	�A�DA��AJA��AA�-Az�AS�A ffA {@�E�@�?}@�Z@���@�K�@�7L@��m@�M�@���@�K�@���@�5?@�+@�`B@��@�E�@���@�D@�  @�^5@��@��@�z�@��@�{@ݲ-@��/@���@��y@�-@٩�@؛�@�dZ@ְ!@�~�@��#@�p�@�V@�r�@�33@ҏ\@��@ύP@�;d@��@��@��@�@�ȴ@�=q@���@�@�1'@�"�@�ȴ@�@ȃ@ȼj@ȣ�@ǶF@���@�Q�@�33@���@�v�@�X@�Z@���@���@�O�@�33@���@���@�(�@���@���@�;d@�n�@�5?@�Ĝ@��@�n�@��;@���@���@��@�1'@�9X@��@�
=@�-@�X@�V@���@���@�@�5?@��#@�p�@�;d@�bN@���@�;d@���@��+@��+@��+@�V@��^@�`B@�hs@�r�@�K�@��^@��@��@�b@�Z@���@�M�@���@��@�@�n�@�o@�33@�l�@�  @���@��`@���@�z�@�A�@��;@���@�K�@��@���@��7@��@�p�@�O�@�`B@��m@�\)@�"�@�n�@��@��@���@�7L@��@�bN@� �@�A�@�r�@�  @�ƨ@�1@��P@��
@�  @��@�(�@�  @��
@��
@���@�"�@��+@�M�@��@�X@��D@�b@���@�ƨ@��@��y@��\@�V@��T@�`B@��@���@��@���@���@�9X@��m@���@���@��@�\)@�C�@��@��!@��+@�^5@�{@��-@�x�@�`B@�X@��@���@��@�Z@�1@��m@��;@��
@��w@��P@�l�@�;d@�"�@���@���@�ff@�V@�n�@�v�@�ff@�=q@�$�@��@���@��-@�/@���@���@��@�Z@�9X@� �@�  @�ƨ@���@�dZ@�C�@��@�@��@��@���@���@���@�M�@�5?@�J@��T@���@��-@�hs@�G�@�7L@���@���@���@�z�@�I�@�1'@� �@�  @�P@;d@~��@~@}�@}/@}V@|�@|�D@{ƨ@{"�@zn�@y�7@yG�@x��@w��@wK�@v��@v��@vff@vE�@v$�@u��@u�@u/@uV@t��@s��@s"�@s@r��@r�\@r=q@q�#@q�@o�@nȴ@m�-@m�@m?}@l��@l�@k��@k��@ko@jn�@i�#@i��@h��@hA�@g�@g\)@g�@f��@f�R@f$�@e�h@eO�@d��@d�@d�D@d(�@c��@c�F@cdZ@c33@c33@c"�@co@b��@b~�@bJ@a��@a��@a��@`bN@_��@_\)@_+@_+@_+@_
=@^�R@^�+@^5?@^5?@^@]�T@]��@]@]��@]?}@\z�@\9X@[��@[��@[C�@Z�H@Z�!@Z�\@Z~�@Z=q@Y�@Y�^@Y��@Yx�@YX@Y�@X��@X��@X��@X�`@X��@X�9@Xb@W��@W�P@WK�@W
=@V�@V��@U�@U�h@U�@TZ@T(�@S��@S�m@S��@St�@SS�@S33@S33@R��@R^5@R^5@R^5@RM�@R�@Q��@Q��@QG�@P�`@PĜ@P�u@PA�@Pb@O�w@O|�@O�@N�R@NV@N{@M��@Mp�@M/@MV@MV@L�/@Lz�@LI�@L1@Kƨ@KdZ@K"�@J�@J��@J�\@JM�@I�#@Ihs@H��@H��@H�u@Hr�@HA�@H  @G�@F��@F�@F��@Fv�@FV@FE�@E�@EV@Dz�@C��@C��@CS�@CS�@CC�@B�H@B��@Bn�@B=q@A��@A�7@@��@@bN@@ �@?�@?l�@?\)@?\)@?+@>��@>ȴ@>$�@=�T@=@=��@=p�@=�@<�/@<�D@<�D@<9X@;�m@;t�@;33@:�@:��@:M�@:-@:�@9�@9x�@8bN@8b@7l�@6��@6��@6$�@5��@5�@4�/@4�@4�D@4j@4Z@4(�@3ƨ@3dZ@3o@2�H@2�\@2M�@2-@1��@17L@1%@0��@0�u@/�;@/�@/|�@/\)@/\)@/\)@/K�@/;d@/
=@.ȴ@.�+@.E�@.@-@-`B@-V@,��@,��@,z�@,9X@,�@+��@+ƨ@+�F@+��@+dZ@+33@*�@*�!@*n�@)��@)�@)��@)�^@)�^@)�7@)X@)&�@)%@(�9@(r�@(Q�@(A�@( �@'�@'�;@'��@'�@'|�@'K�@'
=@&ȴ@&��@&v�@&$�@%�h@%p�@%p�@%`B@%?}@%�@$��@$�@$�@$�/@$�@$�D@$z�@$Z@$9X@#�m@#t�@#33@#o@"�@"�H@"�!@"M�@"-@"J@!�#@!��@!�7@!X@!�@ Ĝ@ Q�@ b@�@�w@�P@l�@+@
=@��@�y@��@5?@@@�h@`B@O�@O�@�/@z�@I�@(�@�@1@�
@�@S�@"�@��@��@��@n�@M�@-@�#@&�@Ĝ@bN@b@�@�w@�P@l�@;d@�@��@�@�R@v�@ff@5?@{@��@�h@�@�@z�@9X@�@��@�
@�F@�@"�@@��@��@~�@-@�^@hs@&�@��@�9@r�@A�@ �@�;@l�@+@�@��@�R@E�@@��@�-@��@`B@/@�@V@�/@�j@�D@z�@��@�
@�
@�
@�
@�
@��@C�@33@o@
��@
=q@	��@	��@	x�@	&�@��@��@Ĝ@�9@r�@A�@�@��@��@|�@|�@\)@\)@K�@+@�@��@�y@ȴ@�R@��@�+@ff@V@V@V@V@V@V@$�@@�-@O�@�@V@�/@�D@j@Z@(�@1@1@�
@�@S�@dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BL�BM�BL�BL�BN�BO�BN�BM�BN�BZB^5BdZBhsBjBn�Bn�Bo�Bn�Bo�Bo�Bo�Bq�B~�Bv�Bn�BiyBgmBdZB^5B^5B[#BaHBcTBdZBffBe`BgmBl�Bl�Bp�Br�Bu�Bw�Bz�By�By�Bz�Bz�By�Bw�Bt�Bq�Bo�BjBiyBffBffBaHB\)BVBH�BF�BB�B=qB2-B-B+B$�B�BPBB��B�B�B�sB�NB�BB�5B�B��B��B�9B�B��B��B�\B�Br�BcTBQ�B?}B:^B'�B{B
=B
��B
�B
�ZB
�B
��B
��B
��B
�qB
�B
��B
�bB
{�B
n�B
hsB
[#B
6FB
#�B
oB	��B	�B	�qB	�-B	��B	��B	��B	�B	u�B	k�B	aHB	YB	O�B	E�B	<jB	0!B	#�B	�B	uB	VB		7B��B�B�B�sB�BB�B��B��B��B��BŢBÖB��B�}B�qB�LB�-B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�hB�DB�+B�B�B{�By�Bw�Bu�Bt�Bp�Bm�Bl�BjBffBe`BcTBaHB^5B\)B[#BYBXBS�BN�BH�BG�BC�BA�B@�B>wB>wB=qB:^B9XB9XB5?B33B1'B1'B1'B1'B0!B/B,B)�B'�B%�B#�B$�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBuBuBuB�B{B�B�B�B�B�B�B �B%�B(�B$�B%�B,B.B1'B33B33B33B33B2-B2-B2-B1'B0!B0!B.B.B-B,B+B-B/B0!B7LB=qB=qB;dB:^B8RB7LB7LB9XBA�B?}BM�BR�BS�BQ�BP�BS�BS�BS�BVBZB`BBcTBbNB^5B^5B^5BbNB_;BZBVBVB]/B^5BhsBq�B~�B�=B�uB�PB��B��B��B��B��B��B��B��B�B�!B�B�'B�3B�B��B��B��B��B��B�B�B�!B�'B�?B�LB�RB�FB�?B�?B�dBÖBɺB��B��B��B��B��B��B�;B�ZB�yB�B��B��B	B	B	B	1B		7B	
=B	PB	PB	VB	VB	VB	bB	{B	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	%�B	&�B	(�B	/B	.B	.B	33B	33B	7LB	9XB	;dB	=qB	@�B	B�B	C�B	E�B	G�B	K�B	L�B	N�B	O�B	N�B	O�B	O�B	O�B	P�B	Q�B	R�B	S�B	S�B	S�B	S�B	W
B	ZB	ZB	[#B	^5B	`BB	`BB	aHB	bNB	bNB	dZB	gmB	jB	jB	k�B	l�B	n�B	q�B	q�B	r�B	u�B	u�B	v�B	x�B	{�B	|�B	}�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�=B	�DB	�JB	�VB	�VB	�\B	�bB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�^B	�jB	�qB	�qB	�qB	�wB	�}B	�}B	��B	B	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�HB	�HB	�NB	�TB	�`B	�`B	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
JB
PB
PB
PB
VB
VB
\B
bB
hB
hB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
+B
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
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
:^B
:^B
;dB
;dB
;dB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
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
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
T�B
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
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
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
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
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
s�B
s�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BL�BM�BL�BL�BN�BO�BN�BM�BN�BZB^5BdZBhsBjBn�Bn�Bo�Bn�Bo�Bo�Bo�Bq�B~�Bv�Bn�BiyBgmBdZB^5B^5B[#BaHBcTBdZBffBe`BgmBl�Bl�Bp�Br�Bu�Bw�Bz�By�By�Bz�Bz�By�Bw�Bt�Bq�Bo�BjBiyBffBffBaHB\)BVBH�BF�BB�B=qB2-B-B+B$�B�BPBB��B�B�B�sB�NB�BB�5B�B��B��B�9B�B��B��B�\B�Br�BcTBQ�B?}B:^B'�B{B
=B
��B
�B
�ZB
�B
��B
��B
��B
�qB
�B
��B
�bB
{�B
n�B
hsB
[#B
6FB
#�B
oB	��B	�B	�qB	�-B	��B	��B	��B	�B	u�B	k�B	aHB	YB	O�B	E�B	<jB	0!B	#�B	�B	uB	VB		7B��B�B�B�sB�BB�B��B��B��B��BŢBÖB��B�}B�qB�LB�-B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�hB�DB�+B�B�B{�By�Bw�Bu�Bt�Bp�Bm�Bl�BjBffBe`BcTBaHB^5B\)B[#BYBXBS�BN�BH�BG�BC�BA�B@�B>wB>wB=qB:^B9XB9XB5?B33B1'B1'B1'B1'B0!B/B,B)�B'�B%�B#�B$�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{BuBuBuBuB�B{B�B�B�B�B�B�B �B%�B(�B$�B%�B,B.B1'B33B33B33B33B2-B2-B2-B1'B0!B0!B.B.B-B,B+B-B/B0!B7LB=qB=qB;dB:^B8RB7LB7LB9XBA�B?}BM�BR�BS�BQ�BP�BS�BS�BS�BVBZB`BBcTBbNB^5B^5B^5BbNB_;BZBVBVB]/B^5BhsBq�B~�B�=B�uB�PB��B��B��B��B��B��B��B��B�B�!B�B�'B�3B�B��B��B��B��B��B�B�B�!B�'B�?B�LB�RB�FB�?B�?B�dBÖBɺB��B��B��B��B��B��B�;B�ZB�yB�B��B��B	B	B	B	1B		7B	
=B	PB	PB	VB	VB	VB	bB	{B	�B	�B	�B	�B	�B	 �B	!�B	#�B	%�B	%�B	&�B	(�B	/B	.B	.B	33B	33B	7LB	9XB	;dB	=qB	@�B	B�B	C�B	E�B	G�B	K�B	L�B	N�B	O�B	N�B	O�B	O�B	O�B	P�B	Q�B	R�B	S�B	S�B	S�B	S�B	W
B	ZB	ZB	[#B	^5B	`BB	`BB	aHB	bNB	bNB	dZB	gmB	jB	jB	k�B	l�B	n�B	q�B	q�B	r�B	u�B	u�B	v�B	x�B	{�B	|�B	}�B	�B	�B	�%B	�+B	�1B	�1B	�7B	�=B	�DB	�JB	�VB	�VB	�\B	�bB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�3B	�9B	�?B	�?B	�FB	�LB	�RB	�RB	�^B	�jB	�qB	�qB	�qB	�wB	�}B	�}B	��B	B	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�HB	�HB	�NB	�TB	�`B	�`B	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
JB
PB
PB
PB
VB
VB
\B
bB
hB
hB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
+B
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
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
:^B
:^B
;dB
;dB
;dB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
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
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
T�B
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
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
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
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
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
s�B
s�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.01 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211002120050                              AO  ARCAADJP                                                                    20211002120050    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211002120050  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211002120050  QCF$                G�O�G�O�G�O�0               