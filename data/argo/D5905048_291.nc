CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-14T00:35:16Z creation;2018-10-14T00:35:22Z conversion to V3.1;2019-12-19T07:26:35Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181014003516  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              #A   JA  I2_0577_291                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؈���G�1   @؈���7 @4k'�/��d\����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D��3D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ��CS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]x�D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��D���D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A��HA��/A��/A��/A��/A��/A��/A��;A��;A��;A��TA��TAּjA�S�A���A�M�A�C�A�C�A�"�A��/Aԡ�AԁA�-A���A��HA�/A�jA�=qA��AƲ-A�I�AÑhA¡�A���A�t�A�?}A�A�A�/A�S�A��mA�I�A��;A��A��A�=qA���A�1'A���A�33A��A�|�A���A�ȴA��+A�?}A��A��wA�z�A���A��jA�+A�S�A��PA��A�(�A��HA���A���A�ffA��hA���A�?}A��TA���A��A��A�`BA���A���A�JA��A��\A��mA��FA��DA�ffA��A�ĜA��hA��-A��wA���A��\A�E�A�;dA���A�1'A�ffA�I�A�v�A�33A|��Aw`BAuC�At�DAs�wAr�!Aq�wAq�AoƨAm/AkAk��Aj��AhffAf��Ae"�AcAb�A_33AZ�yAYK�AXffATv�AR�AQ��AQ%AMK�AK�AIC�AF�DAD��ACdZAAƨA>v�A=XA<�/A9��A7hsA5�mA3�
A3�PA3?}A2��A2�A1�^A1
=A0�jA0��A/��A,ffA+�^A+|�A*��A)�FA)�A(�9A'�A'33A$�yA$bA#�FA!�A �A   At�A�7A33A33A  A�RA��AA�\A�AffAA`BAȴAbA�jA
A�A�A��Ar�A��A�-A�DA`BA�AĜA��AO�A 1'@��@�n�@�Ĝ@��;@���@���@��;@��!@��T@�hs@��`@�Q�@�9X@�@�X@�9X@�=q@��@�ff@�X@�~�@�hs@���@�r�@�M�@��m@�ff@�r�@��@� �@�M�@�@ް!@�p�@ܬ@��@��#@���@��@�b@׾w@��#@��@�  @҇+@�/@�9X@�  @�;d@�=q@�&�@��@ˮ@���@�=q@ɑh@���@�;d@���@�;d@ř�@�Q�@�Ĝ@��H@��@�r�@�Z@�I�@�9X@�1@��@���@�~�@���@��9@��w@���@�dZ@�{@���@���@��
@���@�-@�hs@���@�1@�C�@�E�@�@��h@��`@�9X@��w@�dZ@��\@�-@���@��@��h@��@��`@��@��/@���@���@��m@�l�@�S�@��@�o@�M�@��h@�?}@�hs@���@�x�@�/@��j@�r�@�(�@��F@�t�@�33@��@���@���@��\@�V@�$�@��@���@��@���@��/@��9@�I�@���@��m@��
@��P@��@��R@�$�@��@�@�x�@�%@���@���@��@�j@���@��m@�ƨ@���@��P@�dZ@�33@�o@�@��@�ȴ@�~�@�V@�5?@��@���@��h@���@���@��h@��7@�hs@�`B@�O�@�/@�%@��@���@��9@��j@��u@�r�@�I�@�  @��@�l�@�S�@�;d@��y@���@�M�@���@�@��@�G�@��@���@���@��@�I�@�1@��;@��F@�t�@�33@���@���@���@�M�@�5?@��T@���@�`B@�p�@�`B@�V@�Ĝ@��9@�bN@�(�@�  @���@�C�@�@���@���@�n�@�^5@�$�@�@�X@�?}@�/@�%@��/@�I�@�  @���@��w@�
=@���@��@���@�~�@�n�@�V@�@�@�G�@���@�I�@�b@��
@��@��P@�\)@�;d@��y@��R@���@�E�@��#@��@��@���@���@�Z@� �@��;@���@��@�S�@�;d@�"�@�@�
=@��@���@��\@�n�@�V@��@��#@���@�@��-@��@�/@�V@���@��9@�z�@�r�@�Z@�b@���@��;@��w@�|�@�;d@��@��y@�ȴ@��!@���@�^5@�5?@�-@�-@��@��-@���@���@�hs@��@���@��j@��@�j@�9X@��@�b@�  @l�@~�R@~$�@}�@}@}��@}V@|z�@|I�@|(�@{��@{�F@{dZ@{"�@z�@z��@z~�@y�@y��@y7L@x��@x�@xb@w�;@xb@w�@w�@w\)@v�@vv�@v5?@u��@u@u�@uO�@t�@t�@sC�@r��@rn�@rM�@r=q@q�^@qX@qG�@q%@p �@o��@o\)@o
=@n��@m�T@m�@m`B@m�@l�D@lZ@lZ@k��@k�m@kƨ@k�F@k33@j�@k"�@kC�@k33@k33@j�!@jn�@j=q@iX@h��@h��@h�u@hQ�@g�w@fȴ@fV@e�@ep�@d��@d�j@d�D@dI�@d9X@c��@b��@b^5@b�@bJ@a��@`��@`r�@`1'@`b@`b@_�@_��@_�@_|�@_K�@_;d@^��@^�R@^E�@^{@]�-@]�h@]O�@\��@\�D@[��@[33@[@Z�!@Z�\@Z^5@Y�#@Y��@X��@XA�@X �@W�@W�w@W��@W�P@WK�@V��@V��@VV@V{@U�h@Up�@T��@T��@T�@T��@Tz�@T9X@T�@S��@S��@R�@R�!@R��@R~�@R�@Q��@Qhs@QG�@Q�@P�`@P��@PQ�@P1'@Pb@O��@O��@O�@N��@N��@Nff@N5?@N$�@M�T@Mp�@L��@L��@Lj@K�m@K�@KdZ@KC�@K@J��@J^5@JM�@J-@I��@Ihs@IX@IG�@I%@H�`@H�`@H��@HA�@G�;@G�P@G
=@F��@F{@E�@E�T@E�T@E��@EV@D�/@D�j@D�j@D�@D�D@DZ@C��@Ct�@C@B�H@B��@B��@B~�@B-@BJ@A��@A�@A�7@A&�@@��@@bN@@A�@@ �@?�@?\)@>�@>V@>{@=�@=�T@=�@<��@<��@<��@<I�@;�m@;��@;"�@:��@:~�@:M�@:J@9�^@9�7@9X@9%@8�9@8A�@81'@8b@7�;@7��@7;d@6��@6��@6ff@6V@6$�@5�@5@5��@5p�@5`B@5/@4�@4�j@4�D@4j@4j@49X@4�@3��@3�
@3�
@3ƨ@3��@2�H@2�\@2~�@2n�@2M�@1�@1��@1��@1G�@0��@0�@0�u@0A�@0 �@0b@/�;@/��@/�P@/l�@/+@/
=@.�R@.V@.5?@.{@-�h@-O�@-�@,�@,�@,z�@,j@,I�@,(�@+�F@+S�@+C�@+@*~�@*=q@)��@)��@)��@)x�@)G�@)�@(�`@(r�@(1'@'�@'�w@'|�@'K�@&�y@&�y@&�@&��@&��@%�@%�-@%�@$��@$��@$�D@$j@$(�@#��@#�@#S�@#33@"�!@"=q@!��@!x�@!G�@ ��@ �@ 1'@   @�;@�@;d@�y@�R@��@E�@�T@@�h@�@`B@�@��@�/@j@(�@�@1@�m@�
@��@t�@o@@�H@�H@�H@��@��@��@�!@n�@=q@-@�@�^@X@%@Ĝ@�u@bN@1'@�@�P@\)@+@
=@��@�@��@$�@@�T@�@?}@�@�@�D@z�@z�@z�@j@9X@1@�F@t�@S�@33@@�!@�\@�\@^5@=q@��@��@��@�7@X@G�@&�@�@��@��@��@bN@A�@�;@l�@��@�R@��@��@v�@V@{@�@@�h@�@V@�/@�j@�@z�@j@I�@(�@�
@��@t�@dZ@C�@"�@
�@
�H@
�H@
�H@
��@
^5@
=q@
J@	��@	�#@	��@	G�@	7L@	7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A��HA��/A��/A��/A��/A��/A��/A��;A��;A��;A��TA��TAּjA�S�A���A�M�A�C�A�C�A�"�A��/Aԡ�AԁA�-A���A��HA�/A�jA�=qA��AƲ-A�I�AÑhA¡�A���A�t�A�?}A�A�A�/A�S�A��mA�I�A��;A��A��A�=qA���A�1'A���A�33A��A�|�A���A�ȴA��+A�?}A��A��wA�z�A���A��jA�+A�S�A��PA��A�(�A��HA���A���A�ffA��hA���A�?}A��TA���A��A��A�`BA���A���A�JA��A��\A��mA��FA��DA�ffA��A�ĜA��hA��-A��wA���A��\A�E�A�;dA���A�1'A�ffA�I�A�v�A�33A|��Aw`BAuC�At�DAs�wAr�!Aq�wAq�AoƨAm/AkAk��Aj��AhffAf��Ae"�AcAb�A_33AZ�yAYK�AXffATv�AR�AQ��AQ%AMK�AK�AIC�AF�DAD��ACdZAAƨA>v�A=XA<�/A9��A7hsA5�mA3�
A3�PA3?}A2��A2�A1�^A1
=A0�jA0��A/��A,ffA+�^A+|�A*��A)�FA)�A(�9A'�A'33A$�yA$bA#�FA!�A �A   At�A�7A33A33A  A�RA��AA�\A�AffAA`BAȴAbA�jA
A�A�A��Ar�A��A�-A�DA`BA�AĜA��AO�A 1'@��@�n�@�Ĝ@��;@���@���@��;@��!@��T@�hs@��`@�Q�@�9X@�@�X@�9X@�=q@��@�ff@�X@�~�@�hs@���@�r�@�M�@��m@�ff@�r�@��@� �@�M�@�@ް!@�p�@ܬ@��@��#@���@��@�b@׾w@��#@��@�  @҇+@�/@�9X@�  @�;d@�=q@�&�@��@ˮ@���@�=q@ɑh@���@�;d@���@�;d@ř�@�Q�@�Ĝ@��H@��@�r�@�Z@�I�@�9X@�1@��@���@�~�@���@��9@��w@���@�dZ@�{@���@���@��
@���@�-@�hs@���@�1@�C�@�E�@�@��h@��`@�9X@��w@�dZ@��\@�-@���@��@��h@��@��`@��@��/@���@���@��m@�l�@�S�@��@�o@�M�@��h@�?}@�hs@���@�x�@�/@��j@�r�@�(�@��F@�t�@�33@��@���@���@��\@�V@�$�@��@���@��@���@��/@��9@�I�@���@��m@��
@��P@��@��R@�$�@��@�@�x�@�%@���@���@��@�j@���@��m@�ƨ@���@��P@�dZ@�33@�o@�@��@�ȴ@�~�@�V@�5?@��@���@��h@���@���@��h@��7@�hs@�`B@�O�@�/@�%@��@���@��9@��j@��u@�r�@�I�@�  @��@�l�@�S�@�;d@��y@���@�M�@���@�@��@�G�@��@���@���@��@�I�@�1@��;@��F@�t�@�33@���@���@���@�M�@�5?@��T@���@�`B@�p�@�`B@�V@�Ĝ@��9@�bN@�(�@�  @���@�C�@�@���@���@�n�@�^5@�$�@�@�X@�?}@�/@�%@��/@�I�@�  @���@��w@�
=@���@��@���@�~�@�n�@�V@�@�@�G�@���@�I�@�b@��
@��@��P@�\)@�;d@��y@��R@���@�E�@��#@��@��@���@���@�Z@� �@��;@���@��@�S�@�;d@�"�@�@�
=@��@���@��\@�n�@�V@��@��#@���@�@��-@��@�/@�V@���@��9@�z�@�r�@�Z@�b@���@��;@��w@�|�@�;d@��@��y@�ȴ@��!@���@�^5@�5?@�-@�-@��@��-@���@���@�hs@��@���@��j@��@�j@�9X@��@�b@�  @l�@~�R@~$�@}�@}@}��@}V@|z�@|I�@|(�@{��@{�F@{dZ@{"�@z�@z��@z~�@y�@y��@y7L@x��@x�@xb@w�;@xb@w�@w�@w\)@v�@vv�@v5?@u��@u@u�@uO�@t�@t�@sC�@r��@rn�@rM�@r=q@q�^@qX@qG�@q%@p �@o��@o\)@o
=@n��@m�T@m�@m`B@m�@l�D@lZ@lZ@k��@k�m@kƨ@k�F@k33@j�@k"�@kC�@k33@k33@j�!@jn�@j=q@iX@h��@h��@h�u@hQ�@g�w@fȴ@fV@e�@ep�@d��@d�j@d�D@dI�@d9X@c��@b��@b^5@b�@bJ@a��@`��@`r�@`1'@`b@`b@_�@_��@_�@_|�@_K�@_;d@^��@^�R@^E�@^{@]�-@]�h@]O�@\��@\�D@[��@[33@[@Z�!@Z�\@Z^5@Y�#@Y��@X��@XA�@X �@W�@W�w@W��@W�P@WK�@V��@V��@VV@V{@U�h@Up�@T��@T��@T�@T��@Tz�@T9X@T�@S��@S��@R�@R�!@R��@R~�@R�@Q��@Qhs@QG�@Q�@P�`@P��@PQ�@P1'@Pb@O��@O��@O�@N��@N��@Nff@N5?@N$�@M�T@Mp�@L��@L��@Lj@K�m@K�@KdZ@KC�@K@J��@J^5@JM�@J-@I��@Ihs@IX@IG�@I%@H�`@H�`@H��@HA�@G�;@G�P@G
=@F��@F{@E�@E�T@E�T@E��@EV@D�/@D�j@D�j@D�@D�D@DZ@C��@Ct�@C@B�H@B��@B��@B~�@B-@BJ@A��@A�@A�7@A&�@@��@@bN@@A�@@ �@?�@?\)@>�@>V@>{@=�@=�T@=�@<��@<��@<��@<I�@;�m@;��@;"�@:��@:~�@:M�@:J@9�^@9�7@9X@9%@8�9@8A�@81'@8b@7�;@7��@7;d@6��@6��@6ff@6V@6$�@5�@5@5��@5p�@5`B@5/@4�@4�j@4�D@4j@4j@49X@4�@3��@3�
@3�
@3ƨ@3��@2�H@2�\@2~�@2n�@2M�@1�@1��@1��@1G�@0��@0�@0�u@0A�@0 �@0b@/�;@/��@/�P@/l�@/+@/
=@.�R@.V@.5?@.{@-�h@-O�@-�@,�@,�@,z�@,j@,I�@,(�@+�F@+S�@+C�@+@*~�@*=q@)��@)��@)��@)x�@)G�@)�@(�`@(r�@(1'@'�@'�w@'|�@'K�@&�y@&�y@&�@&��@&��@%�@%�-@%�@$��@$��@$�D@$j@$(�@#��@#�@#S�@#33@"�!@"=q@!��@!x�@!G�@ ��@ �@ 1'@   @�;@�@;d@�y@�R@��@E�@�T@@�h@�@`B@�@��@�/@j@(�@�@1@�m@�
@��@t�@o@@�H@�H@�H@��@��@��@�!@n�@=q@-@�@�^@X@%@Ĝ@�u@bN@1'@�@�P@\)@+@
=@��@�@��@$�@@�T@�@?}@�@�@�D@z�@z�@z�@j@9X@1@�F@t�@S�@33@@�!@�\@�\@^5@=q@��@��@��@�7@X@G�@&�@�@��@��@��@bN@A�@�;@l�@��@�R@��@��@v�@V@{@�@@�h@�@V@�/@�j@�@z�@j@I�@(�@�
@��@t�@dZ@C�@"�@
�@
�H@
�H@
�H@
��@
^5@
=q@
J@	��@	�#@	��@	G�@	7L@	7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B%�B"�B'�B6FB<jBR�BQ�BM�BJ�BL�BN�BI�BE�B9XB"�B �B�
B��BDB	7B49B1'B%�B1BVB@�B)�B6FBo�Bq�Bs�Bz�Bl�Bu�B{�Bo�B\)B|�Bs�B�B�1B�\B�JB�=B�JB�DB�%Bt�BiyBm�BM�B\)BaHBK�B<jB7LBA�B6FB.B�BbB�B�B��B�#B�
B��BŢB�qB�B�!B��B��B��B� Bl�B\)B1'B
�B
�3B
��B
�B
�+B
�=B
�=B
z�B
aHB
.B
oB
PB	�B	�dB	�`B	�mB	�TB	�B	��B	ȴB	�?B	�{B	�hB	��B	�bB	p�B	\)B	ZB	K�B	>wB	�B	B	�B	#�B��B	DB	�B	JB�B�BB�BB��BƨBȴB�}B��B�FB�LB�hB�oB��B�bB�B�B��B��B��B��B��B��B�1BgmB�=B�oB�=B�B�1B�7B�By�BgmBq�Bz�BdZBjBm�BjBQ�BE�BR�BW
BT�BS�BC�BJ�B@�BXBW
BN�BJ�B@�B-B�B&�B?}B9XB$�B#�B$�B'�B5?B2-B"�B-B"�B+B)�B#�B+B(�B)�B%�B-B49B7LB7LB7LB:^B@�B+B49B;dB9XBD�BN�BiyB�1B��B�hB�+B�B�7B�B�+B|�B�B�7B�1B�\B�PB�hB�+B�=B��B��B��B�\B�VB�oB�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�-B��B�B�wB�'B�B��BŢBĜBÖB��B��B�}BǮBƨBȴB��B�
B��B��B��B�)B�NB�;B�B�HB�yB�sB�B�B��B��B��B��B	B	%B	+B	VB	uB	�B	�B	�B	�B	"�B	#�B	"�B	%�B	"�B	'�B	.B	1'B	49B	49B	8RB	<jB	B�B	G�B	@�B	B�B	C�B	D�B	E�B	D�B	F�B	H�B	H�B	K�B	L�B	N�B	N�B	R�B	T�B	W
B	YB	^5B	bNB	bNB	dZB	ffB	k�B	l�B	jB	k�B	n�B	p�B	w�B	x�B	x�B	x�B	}�B	~�B	�B	�B	�B	�%B	�+B	�7B	�DB	�DB	�JB	�VB	�\B	�bB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�?B	�RB	�RB	�RB	�RB	�^B	�dB	�dB	�jB	�wB	B	B	B	ŢB	ÖB	ĜB	ƨB	ɺB	��B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	��B	�B	�#B	�B	�
B	�;B	�BB	�;B	�5B	�BB	�BB	�5B	�;B	�5B	�/B	�TB	�ZB	�`B	�mB	�sB	�sB	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
+B
+B
%B
B
+B
+B
B
B
%B
1B
+B
1B
1B
	7B

=B
	7B
%B
+B
1B
DB
DB
DB
	7B

=B
VB
\B
\B
\B
bB
bB
hB
oB
hB
hB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
 �B
!�B
!�B
!�B
 �B
"�B
%�B
(�B
%�B
'�B
%�B
&�B
&�B
#�B
%�B
'�B
'�B
%�B
#�B
"�B
%�B
%�B
%�B
&�B
'�B
+B
)�B
+B
(�B
'�B
,B
-B
.B
,B
+B
.B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
0!B
1'B
0!B
1'B
0!B
/B
0!B
.B
/B
2-B
1'B
2-B
2-B
0!B
1'B
1'B
1'B
5?B
5?B
5?B
5?B
5?B
5?B
49B
49B
5?B
5?B
5?B
7LB
6FB
7LB
8RB
8RB
8RB
7LB
8RB
7LB
6FB
6FB
8RB
;dB
:^B
9XB
9XB
;dB
<jB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
;dB
;dB
<jB
>wB
>wB
>wB
>wB
=qB
;dB
;dB
=qB
>wB
<jB
=qB
>wB
>wB
>wB
>wB
@�B
A�B
@�B
@�B
?}B
A�B
B�B
A�B
B�B
B�B
A�B
@�B
@�B
@�B
?}B
@�B
?}B
B�B
C�B
B�B
A�B
@�B
C�B
C�B
C�B
C�B
B�B
A�B
@�B
@�B
A�B
C�B
D�B
C�B
B�B
B�B
D�B
D�B
D�B
B�B
B�B
B�B
D�B
D�B
D�B
C�B
A�B
B�B
B�B
D�B
D�B
D�B
C�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H�B
H�B
G�B
I�B
I�B
H�B
I�B
I�B
L�B
K�B
K�B
J�B
J�B
K�B
L�B
M�B
N�B
N�B
O�B
O�B
P�B
P�B
Q�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
R�B
Q�B
T�B
VB
W
B
VB
T�B
VB
VB
T�B
S�B
W
B
YB
W
B
YB
YB
YB
YB
YB
YB
YB
YB
XB
YB
YB
YB
XB
YB
ZB
ZB
ZB
[#B
[#B
[#B
ZB
ZB
[#B
\)B
[#B
ZB
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
aHB
aHB
`BB
`BB
^5B
aHB
aHB
`BB
aHB
bNB
bNB
aHB
aHB
cTB
cTB
bNB
aHB
bNB
cTB
bNB
dZB
dZB
cTB
e`B
ffB
ffB
e`B
e`B
e`B
gmB
gmB
ffB
ffB
hsB
hsB
iyB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
jB
jB
iyB
k�B
k�B
l�B
l�B
l�B
l�B
k�B
k�B
jB
jB
k�B
jB
jB
jB
jB
k�B
l�B
l�B
l�B
l�B
k�B
m�B
m�B
n�B
n�B
m�B
m�B
l�B
n�B
n�B
m�B
n�B
o�B
o�B
o�B
q�B
q�B
q�B
p�B
p�B
p�B
o�B
p�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
s�B
u�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
w�B
v�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
|�B
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B%�B#nB(�B72B=<BR�BRBN<BK^BMPBOBBJ�BF�B;�B&�B$B�HB�BpB�B5�B3hB(�BPB&BB�B.cB9�Bp!Br�Bt�B{�Bn�Bv�B|�Bq�B_!B~(BvB�GB�7B��B��B��B�B��B�Bv�Bk�Bo�BQ�B]�Bb�BN<B?HB9�BB�B7�B0BB�B�B�IB�{B�/B��BѷB�zB�HB�5B�[B�2B��B��B��Bn�B^�B4�B
�+B
��B
�4B
��B
�B
�~B
��B
|�B
c�B
3MB

B
NB	ܬB	��B	�B	�XB	�tB	�kB	�"B	ɺB	�2B	�YB	�B	�B	��B	s�B	^�B	[�B	M�B	@�B	"4B	B	 �B	%�B	-B	jB	�B	"B��B� B��B� B�BʦB��B��B��B��B�gB�2B�yB��B��B��B��B��B�zB��B�BB�WB�#Bj�B��B��B�^B�{B�B��B�GB{JBi�Br�B{�Bf�BlBn�Bk�BT�BH�BUgBX�BV�BU�BE�BL�BB�BX�BW�BO�BK�BA�B/OB"�B(sB?�B:B&�B%zB&fB)_B5�B2�B$@B-�B$ZB+�B+B%B+�B)�B*�B'B-�B4�B7�B7�B7�B:�B@�B-B5?B<�B:�BE�BO(BhsB�tB��B�:B��B��B�=B�tB�KBcB�aB�XB��B�.B�B� B��B�DB��B��B�B��B�(B�&B�pB�[B�1B�B�dB�eB�~B�vB�,B�0B��B��B��B��B�KB�GB�B��B�]B��B�IB��BżBĶB��B��B�B�B��B�EBɆB�pB�$B�aB��B��BܒB�BߤB�1B��B��B�B�"B�IB�B�6B�dB�jB	�B	�B	�B	�B	�B	�B	�B	�B	)B	"�B	$B	#B	&B	#nB	(>B	.IB	1[B	4nB	4�B	8�B	<�B	BuB	GzB	A B	B�B	C�B	D�B	E�B	EB	F�B	H�B	H�B	K�B	MB	N�B	OB	S&B	U2B	WYB	YB	^jB	bNB	b�B	d�B	f�B	k�B	l�B	j�B	k�B	n�B	qB	xB	y	B	y$B	y>B	~B	.B	� B	�'B	�UB	�?B	�EB	�RB	�xB	�^B	�~B	�pB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�2B	�8B	�0B	�"B	�)B	�QB	�CB	�IB	�iB	�UB	�|B	�nB	�tB	�tB	��B	�lB	��B	�lB	�xB	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	ɺB	��B	�B	��B	��B	�0B	�B	�B	�"B	�(B	�4B	� B	�&B	�&B	�B	�FB	�@B	�FB	�+B	�1B	�YB	�?B	�aB	�EB	�WB	�QB	׍B	�;B	�\B	�VB	�jB	�\B	�\B	ބB	ߊB	ޞB	ݲB	�B	�B	�B	�B	�sB	��B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�.B	�.B	�.B	�.B
 B
'B
'B
-B
-B
GB
9B
EB
EB
YB
SB
+B
EB
SB
SB
YB
KB
_B
KB
KB
	RB

=B
	7B
YB
zB
fB
^B
^B
^B
	lB

rB
pB
vB
�B
�B
}B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
 �B
�B
�B
!�B
"�B
 �B
!�B
!�B
!�B
 �B
"�B
%�B
(�B
%�B
(
B
&B
'B
'B
$B
&B
(
B
(
B
&B
$&B
# B
&B
&B
&B
'8B
(
B
+B
*B
+B
)*B
(>B
,"B
-)B
./B
,=B
+QB
./B
0;B
1AB
1AB
1AB
1AB
1AB
1AB
1AB
1AB
0;B
0;B
0UB
1AB
0UB
1AB
0;B
/OB
0;B
.}B
/OB
2GB
1[B
2GB
2GB
0UB
1AB
1[B
1[B
5ZB
5ZB
5ZB
5ZB
5ZB
5tB
4nB
4nB
5ZB
5ZB
5tB
7fB
6zB
7fB
8lB
8�B
8lB
7�B
8lB
7�B
6zB
6�B
8lB
;dB
:xB
9�B
9�B
;B
<�B
;B
;B
;B
;B
<�B
<�B
<�B
;B
;�B
<�B
>wB
>�B
>wB
>wB
=�B
;�B
;�B
=�B
>�B
<�B
=�B
>�B
>�B
>�B
>�B
@�B
A�B
@�B
@�B
?�B
A�B
B�B
A�B
B�B
B�B
A�B
@�B
@�B
@�B
?�B
@�B
?�B
B�B
C�B
B�B
A�B
@�B
C�B
C�B
C�B
C�B
B�B
A�B
@�B
@�B
A�B
C�B
D�B
C�B
B�B
B�B
D�B
D�B
D�B
B�B
B�B
B�B
D�B
D�B
D�B
C�B
A�B
B�B
B�B
D�B
D�B
D�B
C�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H�B
H�B
G�B
I�B
I�B
H�B
I�B
I�B
L�B
K�B
K�B
J�B
J�B
K�B
MB
M�B
N�B
N�B
O�B
PB
Q B
QB
Q�B
Q B
Q B
R B
R B
R B
SB
SB
SB
TB
T,B
S�B
T,B
SB
R B
UB
VB
W$B
VB
UB
VB
VB
U2B
T,B
W$B
Y1B
W?B
YB
Y1B
Y1B
Y1B
Y1B
Y1B
Y1B
Y1B
XEB
Y1B
Y1B
Y1B
XEB
Y1B
Z7B
Z7B
Z7B
[=B
[=B
[=B
Z7B
ZQB
[=B
\CB
[=B
ZQB
\CB
]IB
]dB
^OB
^OB
^OB
^OB
^OB
]dB
]IB
^jB
_VB
_VB
_VB
_VB
aHB
abB
`\B
`\B
^�B
abB
abB
`vB
a|B
bhB
bhB
abB
a�B
cnB
cnB
bhB
a|B
b�B
cnB
b�B
dtB
d�B
c�B
ezB
f�B
f�B
e`B
e�B
ezB
g�B
g�B
f�B
f�B
hsB
h�B
iyB
h�B
h�B
hsB
h�B
h�B
i�B
j�B
jB
j�B
j�B
j�B
j�B
i�B
k�B
k�B
l�B
l�B
l�B
l�B
k�B
k�B
j�B
j�B
k�B
j�B
j�B
jB
j�B
k�B
l�B
l�B
l�B
l�B
k�B
m�B
m�B
n�B
n�B
m�B
m�B
l�B
n�B
n�B
m�B
n�B
o�B
o�B
o�B
q�B
q�B
q�B
p�B
p�B
p�B
o�B
p�B
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
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
s�B
u�B
v�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
w�B
v�B
w�B
x�B
x�B
x�B
x�B
y	B
x�B
xB
x�B
x�B
y�B
zB
y�B
y�B
z�B
z�B
z�B
y�B
y�B
z�B
z�B
z�B
{B
z�B
z�B
|�B
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810180038142018101800381420181018003814201810180200262018101802002620181018020026201810190037032018101900370320181019003703  JA  ARFMdecpA19c                                                                20181014093514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181014003516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181014003520  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181014003520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181014003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181014003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181014003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181014003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181014003521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181014003522                      G�O�G�O�G�O�                JA  ARUP                                                                        20181014005605                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181014153725  CV  JULD            G�O�G�O�F�E�                JM  ARCAJMQC2.0                                                                 20181017153814  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181017153814  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181017170026  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181018153703  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                