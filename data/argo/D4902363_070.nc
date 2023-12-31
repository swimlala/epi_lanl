CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-23T00:35:47Z creation;2016-12-23T00:35:50Z conversion to V3.1;2019-12-19T08:22:32Z update;     
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
resolution        =���   axis      Z        X  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oT   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20161223003547  20200115111517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               FA   JA  I2_0576_070                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��j���1   @��    @:�L/�{J�d�V�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� DzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�3D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@��H@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BX\)B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7��D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Ds�Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dz�Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D���D�?�D���D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D��D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D��{D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D�{D��{D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�~�A��A��A��+A��7A��+A��+A��A�Q�A�JA�v�A�oA��
A���A�G�A�&�A���A��A���A���A���A���A��+A�;dA���A��`A��A��-A��7A�n�A�^5A�E�A�/A�A�ĜA��^A��!A���A�oA���A��9A���A�A��/A�O�A�XA�&�A�{A�O�A��`A���A���A�C�A��A���A� �A��-A��A��A�K�A�JA��-A�JA���A���A��^A�;dA�VA�=qA��mA��A�JA���A��A�O�A���A��/A���A��jA�r�A���A�p�A��9A���A�l�A�(�A���A���A�v�A�(�A�hsA��A��TA��A��jA�A�/A���A�mA�A~�A}%A{�^Az�yAz-Ay��AxȴAv�AsC�Arv�Aq��Aql�Ap�Ap�jAp1Ao\)Anz�Am|�Al  Aj=qAhVAgAfbNAe�PAe
=AdbNAc33Ab^5Aa�
A`�/A_�
A^^5A]�A]/A\��A[ƨAZ=qAX�HAV��AUATjATA�AT{AS�AR��AQ��AOS�AN�DAN{AM�7ALQ�AK
=AH�uAF��AEG�ADbAC�AC?}AB1'AA�A@�9A?��A>��A=��A=p�A=+A<�A<�RA<�A<E�A;��A;�A9A8�A6��A4�A4��A4r�A4M�A4 �A3��A3C�A2��A2$�A0�`A0r�A/��A/hsA.n�A,�DA+\)A*��A*JA)��A(��A(A'�PA'VA&�A&JA%
=A#�;A"�RA!�A �/AVAO�A�A(�A�-AoA1'A��A�uA�DA��A�AAC�AȴA$�A��A��A9XA33A	�#A	hsA	%A��A�wAAVAXA��A��A9XA33AbA`BA �A {@�ȴ@��-@�&�@���@�33@��@�
=@�V@��
@�M�@��@���@�+@��@���@�D@��y@�u@�S�@��@�V@�`B@�$�@�b@ە�@�l�@�
=@�J@�`B@�z�@���@ӶF@�@ѩ�@��@�bN@��@�~�@̃@ʇ+@���@�r�@�ƨ@�K�@Ƨ�@�G�@ě�@�bN@�1'@��@�ƨ@þw@Å@��@�A�@�M�@�x�@��@�
=@���@�~�@�^5@�E�@�{@���@��h@�&�@��@���@��7@�hs@��`@��P@��@���@���@��@��w@�t�@�"�@��+@��@�@�`B@���@�I�@��m@���@�+@��#@�x�@�%@��j@�bN@��w@��\@�M�@���@�I�@�1@���@���@��@���@��@�I�@�  @�l�@��@�^5@�J@�p�@��@��@��@���@���@��\@�V@��#@��@��@��/@���@��u@�z�@�bN@�(�@�  @�t�@�+@��@��@�M�@�p�@���@���@���@���@���@���@� �@�  @���@��;@���@���@�t�@�S�@�33@��@��@���@��-@��@��D@�r�@�bN@���@�o@��H@��H@�ȴ@���@���@���@��\@�~�@�n�@�V@�E�@�$�@�X@���@��@���@���@�K�@�;d@��@���@���@�~�@�M�@�5?@���@��@��R@��@���@���@��#@���@���@�@���@�O�@��@��@�I�@�ƨ@�@�^5@�@��-@�p�@�O�@�/@�/@�V@���@�Ĝ@��@�9X@�@��@�;@�;@��@�P@��@�P@\)@K�@K�@;d@;d@~�y@~��@~��@~�+@~v�@~ff@~5?@~{@~{@~{@~{@~{@~@~@~{@~@~@}�@}�-@}�h@}O�@}�@|��@{ƨ@{�@{t�@z�H@z=q@z�@y�@y�^@y��@yx�@y7L@x��@x�u@xA�@xbN@x�u@xĜ@x�u@w�w@w�w@w�P@w+@vE�@u`B@tz�@s�m@r�H@r=q@q��@p�`@p�@p  @o�P@o+@o
=@n��@nff@m��@l�@l��@lI�@k��@kC�@j��@jn�@jJ@i��@i�#@i�7@ihs@i�@i%@h�`@hbN@g|�@fȴ@f5?@e/@d(�@cƨ@b�@bM�@b-@a�^@a�@`Ĝ@`��@`�u@`  @_�@_|�@_;d@_;d@_l�@_|�@_|�@_l�@_�@]�@\(�@[�F@[t�@[o@Z��@Y��@Y�@XA�@W�w@V��@V{@U�@V@U@U`B@U�@T�D@T(�@S�m@S��@SS�@R�!@Q�^@Qhs@Qx�@QX@Q&�@Q7L@Q7L@Q&�@Q&�@Q7L@Q�@P��@P��@PQ�@P �@P  @O�@O�@O�w@O��@O|�@O+@N��@N��@N5?@M�T@M�@Mp�@M�@L�j@LZ@L�@Kƨ@KdZ@J��@J�\@Jn�@J�@I��@I�@I�@Ihs@H�`@HĜ@H�9@H��@H�u@H�u@H�u@H�@H�@Hr�@HA�@H �@Hb@G�@H  @G�;@G�;@G��@G�w@Gl�@F�@Fff@F{@E�T@E��@E@E�-@E�-@E��@E`B@D�/@C�m@Cƨ@C�F@C��@Ct�@CS�@CC�@CC�@C33@C"�@B�@B�\@B�@A�#@A�7@@��@@ �@?�P@>��@>ȴ@>��@>��@>ff@=�T@=�@=/@<9X@;C�@;@:�@:�@:�H@:��@:��@:�!@:^5@:�@:J@9��@9x�@97L@8��@8��@8�9@8��@8��@8��@8�u@7��@7;d@7�@6�R@6v�@6V@6E�@6E�@65?@5�@5p�@4�D@4Z@4I�@49X@4(�@3��@3ƨ@3��@3��@3��@3��@3�@3t�@3dZ@3C�@3@2�@2�!@2^5@2-@1��@0��@0�`@0Ĝ@0Ĝ@0Ĝ@0��@0�u@0�@0r�@0Q�@01'@01'@0b@/�w@/l�@.�y@.V@-�@-��@-�@-O�@,I�@+dZ@+33@+33@+C�@+C�@+33@+o@*�@*��@*��@*M�@)��@)�#@)�#@)��@)�@(��@(b@'�@(  @'�@'��@'+@&�R@&��@&��@&v�@&E�@&@%�-@%�@$�@$j@$(�@#�m@"�@"~�@"-@!��@!G�@!&�@!%@ ��@ �`@ ��@ ��@ Ĝ@ �9@ ��@ �u@ bN@ A�@  �@�;@\)@��@v�@ff@E�@5?@�@�h@p�@p�@`B@��@j@��@ƨ@�@C�@@�@�H@�\@-@�@��@��@�@�u@A�@ �@�;@�w@�@�@�@�@�@�@�P@\)@ff@��@��@�@?}@�@V@�@�/@��@�@(�@��@�m@�m@�
@ƨ@t�@S�@C�@C�@C�@33@@�!@-@&�@�`@��@Ĝ@Ĝ@�9@�@bN@bN@b@�w@|�@K�@+@�@�y@��@v�@E�@E�@�T@��@�@�@�@O�@/@��@��@�j@�@�D@Z@��@�@�@�@C�@"�@
�@
��@
��@
��@
��@
��@
n�@	�#@	G�@	�@	%@��@��@�`@��@Ĝ@�9@�u@Q�@1'@�@�P@+@�@�R@��@��@ff@ff@ff@ff@V@$�@�@O�@�@��@�/@��@��@��@9X@�@1@��@�m@ƨ@ƨ@��@t�@S�@C�@C�@33@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�~�A��A��A��+A��7A��+A��+A��A�Q�A�JA�v�A�oA��
A���A�G�A�&�A���A��A���A���A���A���A��+A�;dA���A��`A��A��-A��7A�n�A�^5A�E�A�/A�A�ĜA��^A��!A���A�oA���A��9A���A�A��/A�O�A�XA�&�A�{A�O�A��`A���A���A�C�A��A���A� �A��-A��A��A�K�A�JA��-A�JA���A���A��^A�;dA�VA�=qA��mA��A�JA���A��A�O�A���A��/A���A��jA�r�A���A�p�A��9A���A�l�A�(�A���A���A�v�A�(�A�hsA��A��TA��A��jA�A�/A���A�mA�A~�A}%A{�^Az�yAz-Ay��AxȴAv�AsC�Arv�Aq��Aql�Ap�Ap�jAp1Ao\)Anz�Am|�Al  Aj=qAhVAgAfbNAe�PAe
=AdbNAc33Ab^5Aa�
A`�/A_�
A^^5A]�A]/A\��A[ƨAZ=qAX�HAV��AUATjATA�AT{AS�AR��AQ��AOS�AN�DAN{AM�7ALQ�AK
=AH�uAF��AEG�ADbAC�AC?}AB1'AA�A@�9A?��A>��A=��A=p�A=+A<�A<�RA<�A<E�A;��A;�A9A8�A6��A4�A4��A4r�A4M�A4 �A3��A3C�A2��A2$�A0�`A0r�A/��A/hsA.n�A,�DA+\)A*��A*JA)��A(��A(A'�PA'VA&�A&JA%
=A#�;A"�RA!�A �/AVAO�A�A(�A�-AoA1'A��A�uA�DA��A�AAC�AȴA$�A��A��A9XA33A	�#A	hsA	%A��A�wAAVAXA��A��A9XA33AbA`BA �A {@�ȴ@��-@�&�@���@�33@��@�
=@�V@��
@�M�@��@���@�+@��@���@�D@��y@�u@�S�@��@�V@�`B@�$�@�b@ە�@�l�@�
=@�J@�`B@�z�@���@ӶF@�@ѩ�@��@�bN@��@�~�@̃@ʇ+@���@�r�@�ƨ@�K�@Ƨ�@�G�@ě�@�bN@�1'@��@�ƨ@þw@Å@��@�A�@�M�@�x�@��@�
=@���@�~�@�^5@�E�@�{@���@��h@�&�@��@���@��7@�hs@��`@��P@��@���@���@��@��w@�t�@�"�@��+@��@�@�`B@���@�I�@��m@���@�+@��#@�x�@�%@��j@�bN@��w@��\@�M�@���@�I�@�1@���@���@��@���@��@�I�@�  @�l�@��@�^5@�J@�p�@��@��@��@���@���@��\@�V@��#@��@��@��/@���@��u@�z�@�bN@�(�@�  @�t�@�+@��@��@�M�@�p�@���@���@���@���@���@���@� �@�  @���@��;@���@���@�t�@�S�@�33@��@��@���@��-@��@��D@�r�@�bN@���@�o@��H@��H@�ȴ@���@���@���@��\@�~�@�n�@�V@�E�@�$�@�X@���@��@���@���@�K�@�;d@��@���@���@�~�@�M�@�5?@���@��@��R@��@���@���@��#@���@���@�@���@�O�@��@��@�I�@�ƨ@�@�^5@�@��-@�p�@�O�@�/@�/@�V@���@�Ĝ@��@�9X@�@��@�;@�;@��@�P@��@�P@\)@K�@K�@;d@;d@~�y@~��@~��@~�+@~v�@~ff@~5?@~{@~{@~{@~{@~{@~@~@~{@~@~@}�@}�-@}�h@}O�@}�@|��@{ƨ@{�@{t�@z�H@z=q@z�@y�@y�^@y��@yx�@y7L@x��@x�u@xA�@xbN@x�u@xĜ@x�u@w�w@w�w@w�P@w+@vE�@u`B@tz�@s�m@r�H@r=q@q��@p�`@p�@p  @o�P@o+@o
=@n��@nff@m��@l�@l��@lI�@k��@kC�@j��@jn�@jJ@i��@i�#@i�7@ihs@i�@i%@h�`@hbN@g|�@fȴ@f5?@e/@d(�@cƨ@b�@bM�@b-@a�^@a�@`Ĝ@`��@`�u@`  @_�@_|�@_;d@_;d@_l�@_|�@_|�@_l�@_�@]�@\(�@[�F@[t�@[o@Z��@Y��@Y�@XA�@W�w@V��@V{@U�@V@U@U`B@U�@T�D@T(�@S�m@S��@SS�@R�!@Q�^@Qhs@Qx�@QX@Q&�@Q7L@Q7L@Q&�@Q&�@Q7L@Q�@P��@P��@PQ�@P �@P  @O�@O�@O�w@O��@O|�@O+@N��@N��@N5?@M�T@M�@Mp�@M�@L�j@LZ@L�@Kƨ@KdZ@J��@J�\@Jn�@J�@I��@I�@I�@Ihs@H�`@HĜ@H�9@H��@H�u@H�u@H�u@H�@H�@Hr�@HA�@H �@Hb@G�@H  @G�;@G�;@G��@G�w@Gl�@F�@Fff@F{@E�T@E��@E@E�-@E�-@E��@E`B@D�/@C�m@Cƨ@C�F@C��@Ct�@CS�@CC�@CC�@C33@C"�@B�@B�\@B�@A�#@A�7@@��@@ �@?�P@>��@>ȴ@>��@>��@>ff@=�T@=�@=/@<9X@;C�@;@:�@:�@:�H@:��@:��@:�!@:^5@:�@:J@9��@9x�@97L@8��@8��@8�9@8��@8��@8��@8�u@7��@7;d@7�@6�R@6v�@6V@6E�@6E�@65?@5�@5p�@4�D@4Z@4I�@49X@4(�@3��@3ƨ@3��@3��@3��@3��@3�@3t�@3dZ@3C�@3@2�@2�!@2^5@2-@1��@0��@0�`@0Ĝ@0Ĝ@0Ĝ@0��@0�u@0�@0r�@0Q�@01'@01'@0b@/�w@/l�@.�y@.V@-�@-��@-�@-O�@,I�@+dZ@+33@+33@+C�@+C�@+33@+o@*�@*��@*��@*M�@)��@)�#@)�#@)��@)�@(��@(b@'�@(  @'�@'��@'+@&�R@&��@&��@&v�@&E�@&@%�-@%�@$�@$j@$(�@#�m@"�@"~�@"-@!��@!G�@!&�@!%@ ��@ �`@ ��@ ��@ Ĝ@ �9@ ��@ �u@ bN@ A�@  �@�;@\)@��@v�@ff@E�@5?@�@�h@p�@p�@`B@��@j@��@ƨ@�@C�@@�@�H@�\@-@�@��@��@�@�u@A�@ �@�;@�w@�@�@�@�@�@�@�P@\)@ff@��@��@�@?}@�@V@�@�/@��@�@(�@��@�m@�m@�
@ƨ@t�@S�@C�@C�@C�@33@@�!@-@&�@�`@��@Ĝ@Ĝ@�9@�@bN@bN@b@�w@|�@K�@+@�@�y@��@v�@E�@E�@�T@��@�@�@�@O�@/@��@��@�j@�@�D@Z@��@�@�@�@C�@"�@
�@
��@
��@
��@
��@
��@
n�@	�#@	G�@	�@	%@��@��@�`@��@Ĝ@�9@�u@Q�@1'@�@�P@+@�@�R@��@��@ff@ff@ff@ff@V@$�@�@O�@�@��@�/@��@��@��@9X@�@1@��@�m@ƨ@ƨ@��@t�@S�@C�@C�@33@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BT�BT�BT�BT�BT�BT�BVBT�BT�BT�BQ�BO�BH�BF�BF�BF�BI�BN�BVB[#B\)B]/B]/B]/B^5B]/B^5B_;BaHBcTBe`BffBffBgmBgmBhsBhsBgmBhsBiyBm�Bl�BjB_;BR�BO�BF�B"�BB�B�sB�TB��BB�B��Bn�BgmBYB@�B#�B	7B�B�B�NB��BĜB�FB��B�%Bl�BZBW
BR�BO�BL�BG�BD�BC�BC�BA�B<jB49B%�B�BbB
=B+B%BBB
��B
�B
�TB
��B
��B
�^B
�-B
��B
��B
��B
�oB
�PB
�B
y�B
s�B
n�B
k�B
e`B
^5B
K�B
B�B
>wB
;dB
7LB
5?B
1'B
,B
&�B
 �B
�B
hB
B	��B	��B	�B	�B	�yB	�TB	�)B	�B	��B	��B	ÖB	�jB	�^B	�FB	�'B	��B	��B	�uB	�+B	�B	�B	�B	~�B	x�B	r�B	hsB	bNB	_;B	]/B	XB	Q�B	E�B	=qB	6FB	.B	,B	(�B	$�B	!�B	�B	�B	uB	\B	PB	DB	
=B		7B		7B	+B	B	B��B��B�B�fB�`B�ZB�TB�TB�HB�;B�/B�#B�B��B��B��B��BÖB�qB�dB�XB�FB�9B�'B�B�B��B��B��B��B��B��B�uB�bB�1B�%B�B�B�B~�B|�Bz�Bt�Bn�Be`Be`BdZBcTB`BB_;B[#BVBR�BO�BM�BK�BJ�BI�BF�BE�BD�BA�BA�B@�B?}B;dB;dB9XB8RB7LB6FB49B5?B49B0!B0!B.B/B.B-B-B-B,B,B,B-B-B,B+B,B,B-B,B-B,B-B,B-B-B+B)�B)�B,B,B-B-B.B/B2-B33B49B6FB7LB9XB<jB=qB=qB=qB>wB>wB=qB=qBA�BB�BF�BE�BH�BL�BM�BM�BM�BN�BN�BO�BO�BP�BVB[#B[#B[#B\)BaHBcTBcTBdZBffBgmBgmBhsBk�Bk�Bk�Bl�Bm�Bo�Bp�Bp�Br�Bv�Bw�Bx�By�Bz�B~�B�B�B�1B�PB�PB�\B�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�'B�'B�'B�-B�-B�9B�?B�?B�LB�XBBȴB��B��B��B��B��B�B�
B�B�B�B�B�B�B�B�#B�)B�BB�`B�sB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	+B		7B	
=B	DB	JB	JB	PB	VB	\B	hB	hB	oB	\B	VB	\B	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	'�B	)�B	-B	/B	1'B	2-B	2-B	49B	7LB	:^B	?}B	C�B	G�B	J�B	J�B	K�B	M�B	M�B	N�B	P�B	R�B	R�B	S�B	T�B	T�B	YB	]/B	]/B	^5B	_;B	dZB	p�B	v�B	w�B	x�B	x�B	x�B	y�B	y�B	z�B	z�B	{�B	|�B	|�B	}�B	~�B	� B	�B	�B	�%B	�%B	�1B	�=B	�=B	�JB	�PB	�PB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�9B	�FB	�RB	�^B	�^B	�jB	�wB	��B	ĜB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�)B	�;B	�BB	�NB	�TB	�TB	�`B	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
JB
PB
PB
VB
VB
VB
\B
bB
bB
hB
hB
oB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
#�B
#�B
$�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
+B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
/B
/B
/B
0!B
0!B
0!B
/B
/B
1'B
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
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
:^B
:^B
;dB
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
;dB
;dB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
B�B
B�B
B�B
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
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
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
J�B
J�B
L�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
P�B
P�B
P�B
P�B
P�B
Q�B
S�B
S�B
T�B
T�B
T�B
T�B
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
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
^5B
^5B
^5B
_;B
_;B
_;B
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
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BT�BT�BT�BT�BT�BT�BVBUBU2BU�BR�BQ BI�BGEBGEBGEBJ#BO\BV�B[=B\CB]/B]IB]~B^�B]�B^jB_pBa�Bc�Be�Bf�Bf�Bg�Bg�Bh�Bh�Bg�Bh�BjBn/Bm]Bl�B`�BTBR�BL�B'B�B��B�B��B��B�KB�'B��BqBkB]�BC�B'�B�B�B�B�FB�BǮB��B��B��Bo5B[WBX+BTBQ4BNVBHKBD�BC�BDBB�B>�B6�B'�BjBNB
�B�B�B�B'B
��B
��B
�B
бB
��B
�6B
�B
�yB
�-B
��B
�uB
��B
��B
z�B
t�B
o�B
mB
h$B
a�B
L�B
CaB
>�B
<B
7�B
6+B
2-B
-CB
(sB
"�B
�B
�B
�B	��B	��B	�|B	�B	��B	�ZB	�B	�kB	�aB	�pB	ĜB	�B	�JB	��B	�B	��B	�'B	�MB	��B	�gB	�uB	��B	�iB	zxB	uB	iyB	c B	`BB	^�B	ZB	T�B	G�B	?HB	7�B	.�B	,�B	*0B	%�B	# B	B	�B	FB	B	�B	�B	
�B		�B		�B	�B	YB	�B��B��B��B��B�B�B��B��B�B�B�OBܒBּB��B��B�vB�B�B�]B�jB�B�LB�ZB��B��B��B��B�sB��B�HB�/B�B��B�TB�B��B��B�B�GB�4B~�B}�BxBpUBf2Bf2Be,BdZBa�Ba�B\�BW�BT{BP�BN�BL�BK�BJ�BG�BF�BE9BBABBuBBB@�B<jB<PB:xB9XB8B7B5�B8B5�B1AB1B/�B0;B/ B-�B-�B-�B,�B-B-]B.}B-�B,�B+�B-)B.B.IB,�B-]B,�B-�B,�B./B/B+�B*�B*�B,�B,�B-�B.IB/�B0oB3B3�B4�B6�B7�B:DB<�B=�B=�B=�B>�B>�B=�B>�BB�BC�BG_BFtBI�BM6BNBNBNBOBO(BP.BP}BRBW$B[qB[qB[�B]/BbBc�Bc�Bd�BgBg�Bg�Bh�Bk�Bk�Bk�BmBnBo�Bp�BqABs�BwBx8By$Bz*B{B�B�mB��B�B��B��B��B�.B�@B�B�B�B�#B�)B�5B�!B�BB�4B�@B�zB�sB�B�*B�KB�eB�WB�]B�IB�UB�;B�AB�[B�[B�aB��B�nB�tB��B��B�B��B��B��B��B��B�B�(B�B�$B�+B�+B�EB�EB�EB�KB�QB�WBܒB��B��B��B�B��B�B�'B��B��B��B��B��B��B��B��B��B��B��B�B��B�qB	�B	_B		RB	
�B	^B	dB	~B	�B	�B	�B	�B	 B	�B	B	�B	�B	uB	�B	�B	�B	�B	�B	�B	�B	�B	/B	5B	!bB	%`B	(>B	*KB	-CB	/5B	1[B	2GB	2aB	4nB	7fB	:�B	?�B	C�B	G�B	J�B	J�B	K�B	M�B	M�B	N�B	Q B	R�B	S&B	TB	UB	UB	Y1B	]/B	]IB	^OB	_VB	d�B	p�B	v�B	w�B	x�B	x�B	x�B	y�B	y�B	z�B	z�B	|B	}B	}B	~B	B	�4B	�[B	�9B	�YB	�YB	�fB	�XB	�XB	�dB	�jB	�jB	��B	��B	��B	��B	�gB	��B	��B	��B	��B	��B	�B	�B	�LB	�>B	�_B	�WB	�wB	�UB	�vB	�|B	��B	�nB	��B	��B	�xB	��B	��B	��B	��B	ĶB	��B	��B	�B	��B	��B	��B	��B	� B	� B	�B	�B	�$B	�+B	�eB	چB	ܒB	ߊB	�B	�B	�B	�B	�B	�`B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�}B	�B	�B	�B	�B	��B	�GB	�?B	��B	��B	�B	�B	�B	�$B	�*B	�*B	�JB	�"B	�B	��B	�B	�.B	�.B
 4B
;B
'B
'B
GB
aB
mB
?B
EB
EB
EB
+B
+B
B
+B
EB
EB
EB
KB
1B
	RB

XB

=B

=B

XB

XB

XB
^B
^B
�B
�B
jB
jB
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
 �B
 �B
!�B
"B
$B
$B
%B
'B
'B
'B
&�B
'8B
($B
($B
)_B
+QB
,"B
,"B
,B
,"B
,B
,"B
,"B
-)B
-)B
-)B
-CB
./B
./B
/5B
/5B
/B
0;B
0!B
0B
/OB
/iB
1[B
1AB
1AB
2GB
2aB
33B
33B
3hB
3MB
3hB
4�B
5ZB
5?B
5ZB
5ZB
5ZB
6`B
6`B
6FB
6+B
6FB
6FB
6FB
6`B
7fB
7fB
7fB
7fB
8lB
8lB
9�B
:�B
:xB
;dB
:^B
:^B
:�B
;dB
;dB
;dB
;B
;B
;dB
;B
;B
;�B
<�B
<�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
C�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
B�B
B�B
B�B
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
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
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
J�B
J�B
L�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
NB
M�B
NB
NB
OB
O�B
PB
O�B
P�B
P�B
P�B
Q�B
P�B
Q B
P�B
Q B
Q4B
R:B
T,B
TB
U2B
UB
U2B
T�B
UB
T�B
UB
UB
V9B
VB
VB
VB
VB
VB
VB
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
W$B
W?B
W$B
X_B
Z7B
Z7B
ZB
ZB
ZQB
Z7B
Z7B
Z7B
Z7B
Z7B
ZQB
Z7B
Z7B
Z7B
Z7B
[WB
[=B
[=B
[=B
[=B
\CB
\CB
]/B
]/B
]/B
]/B
]IB
^OB
^5B
^OB
^OB
^OB
^jB
^B
^5B
_;B
_VB
_pB
_VB
`\B
`BB
`'B
`\B
`\B
`\B
`vB
a|B
abB
bNB
bhB
bNB
bNB
b4B
bNB
bhB
bhB
bhB
cnB
cnB
c�B
dtB
d�B
ezB
e`B
e`B
ezB
e`B
e`B
e`B
ezB
ezB
e�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
hsB
hsB
hsB
h�B
hsB
h�B
i�B
i�B
iyB
iyB
i�B
j�B
j�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612270034312016122700343120161227003431201806221218402018062212184020180622121840201804050411482018040504114820180405041148  JA  ARFMdecpA19c                                                                20161223093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161223003547  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161223003548  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161223003548  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161223003549  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161223003549  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161223003549  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161223003549  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161223003549  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161223003550                      G�O�G�O�G�O�                JA  ARUP                                                                        20161223022508                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161223153304  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161226153431  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161226153431  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191148  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031840  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111517                      G�O�G�O�G�O�                