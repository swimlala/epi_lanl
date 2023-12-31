CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-11T00:35:16Z creation;2016-12-11T00:35:19Z conversion to V3.1;2019-12-19T08:23:26Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161211003516  20200115111516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               BA   JA  I2_0576_066                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @���j1�1   @�ඦ�À@:���7���d��m\��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD�fD  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D̼�D�  D�C3D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D��3D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC��C�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D��D�D��D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du��Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D�|{D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̼{D���D�B�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�DՂ�D���D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�B�D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aď\AēuAēuAēuAēuAĕ�Aĕ�AēuAēuAĕ�Aė�Aę�Aě�Ağ�Aġ�Aģ�Aĥ�Aĥ�Aĩ�AĬAĬAĮAĬAĬAĮAİ!AĴ9Aİ!AĬAĬAħ�Aġ�A�x�A�l�A�hsA�ffA�ffA��yA���A���A�XA�9XA��;A�ƨA���A��A��wA�z�A�ZA��jA��+A���A�G�A� �A��FA���A���A�A�A�bNA�Q�A���A��A�%A���A� �A�  A��wA���A�-A��A�
=A�ȴA��+A�(�A� �A��^A���A�?}A���A�/A���A�ƨA���A��A�v�A�ZA��A�9XA�bNA�{A���A���A�VA�AXA33A
=A~ĜA}�A{�#Az=qAx�AxI�Ax(�Aw�^Av�AtAr�`Ar~�AqhsApVAo��An1Ak��Aj{AiAhM�Ag�Af�!Ae�TAd�DAb�DA`�`A_p�A^=qA]��A]7LA\-AZ �AY\)AXVAW
=AUp�AT�ATA�AS��AS�PAS7LARĜAR(�AQ33APE�AO�hAO
=AN��ANjAM�FAL��AL1'AK��AK��AJ��AJ-AIƨAH�yAGAD�/AC�
AB�A@bNA?hsA>v�A=/A<��A<{A;p�A;O�A;�A8�A7��A5�
A4bNA3�TA3A2Q�A1G�A0��A/�#A/�PA.�\A-�A-��A-VA,JA+l�A+VA*��A*A�A)p�A)C�A(�`A(9XA({A'A&�\A$ĜA#�A#�^A#XA"�`A"$�A!7LA ��A 5?A�mA�hAG�A��A�!A�hAv�A(�A\)A��A��AO�A�HAv�A5?A{A�-AXAȴA�A
=A�AbNAE�AK�AI�A��A��AJAt�AAz�A(�A�A(�AhsA
A	XA	Av�A|�A�A��A��A{A�A%Az�AAA��A ��@��@���@�=q@��@��^@���@��@���@�l�@��@�D@�Q�@�@���@�@� �@�"�@�j@�R@��T@�h@�h@�A�@�~�@��`@�@���@���@�/@��/@�|�@�"�@�=q@ܼj@�X@���@�J@Ь@ϝ�@�/@�{@Ȭ@Ǖ�@Ƨ�@�~�@��@���@ă@ÍP@��@�M�@�?}@� �@��\@��@��@��@��H@��@�b@�@��`@��@�@��R@�J@��7@��@��@�|�@��@�-@��7@�7L@��m@�X@�G�@�@���@�I�@��;@��F@��@�C�@�@���@�bN@�  @��@�\)@��@��@�M�@���@�X@��@���@���@���@� �@�o@�n�@��@��@��h@��u@�ƨ@�\)@�@��H@���@�=q@��@���@��h@�`B@�%@��@���@��u@�9X@��@�33@�o@��@�ff@�{@���@���@�x�@�/@��9@�bN@� �@��;@���@��P@�\)@�C�@�33@���@���@�$�@�@���@��-@���@�G�@���@���@��@�9X@�b@��@���@��P@�\)@�;d@�
=@��\@�{@���@���@�ƨ@�dZ@��@��H@��\@�{@�p�@��/@��/@���@���@��j@��@��@��D@�Q�@��;@��@���@�t�@�dZ@�"�@���@��!@�v�@�v�@�n�@�E�@�5?@�@��h@�O�@�7L@�p�@��7@�`B@��@�;@~5?@}�@|�j@|Z@{��@{t�@{S�@{@z�!@zn�@z=q@y��@y��@y��@yhs@yG�@y�@x��@x�9@x1'@w�@w\)@w�@v�@v�R@v��@v��@v��@vV@u�T@u��@up�@u`B@uO�@u?}@u?}@t�@t��@tI�@t1@s�m@s�F@s�@sC�@s"�@s@r��@r~�@rM�@q�#@qX@p��@p �@oK�@n��@nȴ@nE�@n5?@m�T@m@m�h@m`B@m/@mO�@mO�@lz�@k�m@k"�@k@j��@j�!@j�!@j��@j��@j��@j�@j�H@jn�@i��@i�@h�`@h��@hbN@h1'@h  @g��@g�@fV@e`B@e/@d��@d�j@dj@dj@d�@c��@cC�@co@b^5@a��@a�#@a�#@a�#@a��@a�#@a��@a�@`�`@`��@`�9@`�@`r�@`1'@_�@_��@_;d@^ȴ@^v�@^$�@^{@]�@]��@]��@]p�@]V@\�@\��@\�@\Z@\�@\1@[�
@[�F@[��@[�@[S�@[C�@["�@Z�@Zn�@Z^5@Z�@Y��@Y�^@Y��@Y��@Y��@Yx�@Y7L@X��@XA�@W��@W+@V�y@V�R@V��@Vff@VE�@V$�@U�@U��@U`B@T��@T�@Tz�@TI�@S��@Sƨ@S�@S@R�@R��@R=q@Q��@Q7L@PĜ@P�@Pb@O�w@O��@O\)@Nv�@NE�@N{@N@M�T@M@M�@M�@L�j@LI�@L(�@L1@K�F@Kt�@KC�@K"�@J��@J�\@J~�@JM�@JJ@I�@I��@I&�@H�@H  @G��@G|�@G\)@G;d@F�@F�+@F{@E�h@E�@E?}@Dj@DZ@DI�@D�@C�
@Ct�@B�@B-@A�@A�#@A��@A��@A�7@Ahs@A7L@A&�@@Ĝ@@bN@@A�@@b@?�@>ȴ@=@=V@<�j@<z�@<j@<�@;�
@;�F@;��@;C�@;"�@;"�@;"�@;"�@;@:�H@:��@:��@:~�@:^5@:J@9��@9�^@9��@9��@9%@8r�@8 �@7�@7l�@6��@6v�@6$�@5��@5O�@5?}@5/@4Z@3�F@3S�@2��@2n�@2=q@2-@2�@2J@1��@1��@1G�@1&�@1�@1%@0�`@0�9@0bN@/�@/�w@/K�@.ȴ@.E�@-�@-�T@-�-@-O�@-?}@-/@-�@,��@,�/@,�D@,9X@,1@+�F@+S�@*�\@*�@)�#@)��@)�7@)hs@)X@)G�@)7L@)&�@(��@(r�@( �@'�@';d@'�@&��@&�@&�R@&��@&��@&�+@&v�@&V@&$�@%��@%��@%@%�@$�/@$��@$�D@$j@$9X@#��@#�
@#�F@#��@#��@#�@#C�@#"�@#o@"��@"�!@"~�@"-@"�@"�@"J@!��@!��@!��@!�@!�^@!7L@!&�@!%@!%@ ��@ ��@ Ĝ@ �u@ Q�@�;@�@\)@
=@�R@v�@ff@$�@��@@�-@��@p�@`B@O�@O�@/@�@V@��@�/@��@I�@1@�
@ƨ@�@"�@�H@M�@��@��@��@�7@x�@G�@%@��@�9@bN@ �@�;@�w@K�@��@�R@�+@E�@{@��@@�@V@�j@��@�D@�D@�D@��@�D@Z@9X@�m@��@�@C�@@�!@-@�@��@G�@��@��@�@r�@A�@�@�P@\)@
=@�R@��@�+@V@$�@@@�-@�-@��@��@��@�h@�h@�@`B@?}@�@��@�D@j@I�@9X@1@��@t�@dZ@dZ@C�@"�@o@o@
�H@
�\@
M�@
=q@
-@	��@	��@	��@	��@	x�@	%@��@Ĝ@�9@�9@�@bN@b@��@�P@�P@�P@�P@�P@��@��@�P@\)@
=@��@v�@ff@V@E�@E�@$�@�-@p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aď\AēuAēuAēuAēuAĕ�Aĕ�AēuAēuAĕ�Aė�Aę�Aě�Ağ�Aġ�Aģ�Aĥ�Aĥ�Aĩ�AĬAĬAĮAĬAĬAĮAİ!AĴ9Aİ!AĬAĬAħ�Aġ�A�x�A�l�A�hsA�ffA�ffA��yA���A���A�XA�9XA��;A�ƨA���A��A��wA�z�A�ZA��jA��+A���A�G�A� �A��FA���A���A�A�A�bNA�Q�A���A��A�%A���A� �A�  A��wA���A�-A��A�
=A�ȴA��+A�(�A� �A��^A���A�?}A���A�/A���A�ƨA���A��A�v�A�ZA��A�9XA�bNA�{A���A���A�VA�AXA33A
=A~ĜA}�A{�#Az=qAx�AxI�Ax(�Aw�^Av�AtAr�`Ar~�AqhsApVAo��An1Ak��Aj{AiAhM�Ag�Af�!Ae�TAd�DAb�DA`�`A_p�A^=qA]��A]7LA\-AZ �AY\)AXVAW
=AUp�AT�ATA�AS��AS�PAS7LARĜAR(�AQ33APE�AO�hAO
=AN��ANjAM�FAL��AL1'AK��AK��AJ��AJ-AIƨAH�yAGAD�/AC�
AB�A@bNA?hsA>v�A=/A<��A<{A;p�A;O�A;�A8�A7��A5�
A4bNA3�TA3A2Q�A1G�A0��A/�#A/�PA.�\A-�A-��A-VA,JA+l�A+VA*��A*A�A)p�A)C�A(�`A(9XA({A'A&�\A$ĜA#�A#�^A#XA"�`A"$�A!7LA ��A 5?A�mA�hAG�A��A�!A�hAv�A(�A\)A��A��AO�A�HAv�A5?A{A�-AXAȴA�A
=A�AbNAE�AK�AI�A��A��AJAt�AAz�A(�A�A(�AhsA
A	XA	Av�A|�A�A��A��A{A�A%Az�AAA��A ��@��@���@�=q@��@��^@���@��@���@�l�@��@�D@�Q�@�@���@�@� �@�"�@�j@�R@��T@�h@�h@�A�@�~�@��`@�@���@���@�/@��/@�|�@�"�@�=q@ܼj@�X@���@�J@Ь@ϝ�@�/@�{@Ȭ@Ǖ�@Ƨ�@�~�@��@���@ă@ÍP@��@�M�@�?}@� �@��\@��@��@��@��H@��@�b@�@��`@��@�@��R@�J@��7@��@��@�|�@��@�-@��7@�7L@��m@�X@�G�@�@���@�I�@��;@��F@��@�C�@�@���@�bN@�  @��@�\)@��@��@�M�@���@�X@��@���@���@���@� �@�o@�n�@��@��@��h@��u@�ƨ@�\)@�@��H@���@�=q@��@���@��h@�`B@�%@��@���@��u@�9X@��@�33@�o@��@�ff@�{@���@���@�x�@�/@��9@�bN@� �@��;@���@��P@�\)@�C�@�33@���@���@�$�@�@���@��-@���@�G�@���@���@��@�9X@�b@��@���@��P@�\)@�;d@�
=@��\@�{@���@���@�ƨ@�dZ@��@��H@��\@�{@�p�@��/@��/@���@���@��j@��@��@��D@�Q�@��;@��@���@�t�@�dZ@�"�@���@��!@�v�@�v�@�n�@�E�@�5?@�@��h@�O�@�7L@�p�@��7@�`B@��@�;@~5?@}�@|�j@|Z@{��@{t�@{S�@{@z�!@zn�@z=q@y��@y��@y��@yhs@yG�@y�@x��@x�9@x1'@w�@w\)@w�@v�@v�R@v��@v��@v��@vV@u�T@u��@up�@u`B@uO�@u?}@u?}@t�@t��@tI�@t1@s�m@s�F@s�@sC�@s"�@s@r��@r~�@rM�@q�#@qX@p��@p �@oK�@n��@nȴ@nE�@n5?@m�T@m@m�h@m`B@m/@mO�@mO�@lz�@k�m@k"�@k@j��@j�!@j�!@j��@j��@j��@j�@j�H@jn�@i��@i�@h�`@h��@hbN@h1'@h  @g��@g�@fV@e`B@e/@d��@d�j@dj@dj@d�@c��@cC�@co@b^5@a��@a�#@a�#@a�#@a��@a�#@a��@a�@`�`@`��@`�9@`�@`r�@`1'@_�@_��@_;d@^ȴ@^v�@^$�@^{@]�@]��@]��@]p�@]V@\�@\��@\�@\Z@\�@\1@[�
@[�F@[��@[�@[S�@[C�@["�@Z�@Zn�@Z^5@Z�@Y��@Y�^@Y��@Y��@Y��@Yx�@Y7L@X��@XA�@W��@W+@V�y@V�R@V��@Vff@VE�@V$�@U�@U��@U`B@T��@T�@Tz�@TI�@S��@Sƨ@S�@S@R�@R��@R=q@Q��@Q7L@PĜ@P�@Pb@O�w@O��@O\)@Nv�@NE�@N{@N@M�T@M@M�@M�@L�j@LI�@L(�@L1@K�F@Kt�@KC�@K"�@J��@J�\@J~�@JM�@JJ@I�@I��@I&�@H�@H  @G��@G|�@G\)@G;d@F�@F�+@F{@E�h@E�@E?}@Dj@DZ@DI�@D�@C�
@Ct�@B�@B-@A�@A�#@A��@A��@A�7@Ahs@A7L@A&�@@Ĝ@@bN@@A�@@b@?�@>ȴ@=@=V@<�j@<z�@<j@<�@;�
@;�F@;��@;C�@;"�@;"�@;"�@;"�@;@:�H@:��@:��@:~�@:^5@:J@9��@9�^@9��@9��@9%@8r�@8 �@7�@7l�@6��@6v�@6$�@5��@5O�@5?}@5/@4Z@3�F@3S�@2��@2n�@2=q@2-@2�@2J@1��@1��@1G�@1&�@1�@1%@0�`@0�9@0bN@/�@/�w@/K�@.ȴ@.E�@-�@-�T@-�-@-O�@-?}@-/@-�@,��@,�/@,�D@,9X@,1@+�F@+S�@*�\@*�@)�#@)��@)�7@)hs@)X@)G�@)7L@)&�@(��@(r�@( �@'�@';d@'�@&��@&�@&�R@&��@&��@&�+@&v�@&V@&$�@%��@%��@%@%�@$�/@$��@$�D@$j@$9X@#��@#�
@#�F@#��@#��@#�@#C�@#"�@#o@"��@"�!@"~�@"-@"�@"�@"J@!��@!��@!��@!�@!�^@!7L@!&�@!%@!%@ ��@ ��@ Ĝ@ �u@ Q�@�;@�@\)@
=@�R@v�@ff@$�@��@@�-@��@p�@`B@O�@O�@/@�@V@��@�/@��@I�@1@�
@ƨ@�@"�@�H@M�@��@��@��@�7@x�@G�@%@��@�9@bN@ �@�;@�w@K�@��@�R@�+@E�@{@��@@�@V@�j@��@�D@�D@�D@��@�D@Z@9X@�m@��@�@C�@@�!@-@�@��@G�@��@��@�@r�@A�@�@�P@\)@
=@�R@��@�+@V@$�@@@�-@�-@��@��@��@�h@�h@�@`B@?}@�@��@�D@j@I�@9X@1@��@t�@dZ@dZ@C�@"�@o@o@
�H@
�\@
M�@
=q@
-@	��@	��@	��@	��@	x�@	%@��@Ĝ@�9@�9@�@bN@b@��@�P@�P@�P@�P@�P@��@��@�P@\)@
=@��@v�@ff@V@E�@E�@$�@�-@p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B0!B0!B0!B1'B1'B1'B1'B1'B2-B2-B49B49B49B5?B6FB6FB7LB6FB8RB9XB9XB9XB9XB8RB:^B=qBB�BB�BB�BB�BB�BB�BA�B?}B>wB<jB;dB(�BbBDBB�B�sB�TB�NB�
B��BȴBÖB�LB�?B�-B��B�DBhsBK�B?}B7LB2-B'�B�B�BhBJB
=B  B��B��B��B�B�mB�fB�ZB�HB�BȴB�B��Bt�BffB_;BW
BO�BK�BB�B'�BuB%B
��B
�B
ÖB
�dB
�B
��B
��B
��B
��B
��B
��B
�DB
~�B
t�B
o�B
m�B
jB
e`B
YB
L�B
I�B
F�B
=qB
9XB
0!B
�B
oB

=B
B
B	��B	�B	�B	�;B	��B	��B	��B	�qB	�^B	�3B	��B	��B	��B	�hB	�=B	�%B	�B	�B	� B	~�B	|�B	x�B	t�B	o�B	k�B	hsB	ffB	dZB	bNB	^5B	ZB	W
B	T�B	Q�B	M�B	K�B	G�B	;dB	33B	-B	%�B	�B	{B	bB	
=B	%B	B	B��B��B��B�B�fB�HB�5B�/B�B��B��B��B��B��BƨBŢBĜB��B�wB�jB�dB�^B�LB�FB�?B�3B�'B�!B�B��B��B��B��B��B��B��B�{B�oB�hB�\B�VB�PB�DB�=B�B�B� B|�Bx�Bv�Bt�Bs�Bq�Bp�Bn�Bl�BiyBe`BbNBaHB`BB_;B`BB]/B\)BZBYBXBW
BS�BR�BQ�BO�BN�BN�BL�BK�BJ�BJ�BF�BF�BC�BA�B=qB<jB:^B9XB8RB8RB7LB6FB5?B49B6FB7LB8RB:^B7LB33B+B+B(�B(�B'�B&�B$�B#�B%�B#�B'�B-B5?B49B0!B-B0!B0!B0!B33B5?B7LB:^B7LB6FB2-B,B!�B�B�B�B{B�B{B�B�B�B�B�B"�B'�B.B-B)�B)�B,B,B+B'�B%�B$�B'�B)�B/B1'B33B6FB8RB:^B:^B:^B:^B:^B9XB9XB:^BA�BG�BK�BP�BN�BO�BO�BO�BP�BYBZB\)B\)B_;B_;BaHBcTBcTBiyBk�Bm�Bp�Br�Bs�Bu�By�By�By�By�Bz�B}�B�B�B�B�B�%B�7B�PB�bB�oB��B��B��B��B��B��B��B��B�B�B�!B�-B�?B�FB�LB�RB�dB�qB�}B��BÖBŢBƨBǮBǮBɺB��B��B��B��B��B��B�B�B�/B�;B�BB�HB�NB�TB�`B�fB�mB�sB�yB�B�yB�yB�yB�B�B�B�B��B	B	DB	JB	JB	PB	bB	oB	oB	{B	�B	�B	�B	�B	 �B	!�B	$�B	'�B	)�B	-B	1'B	49B	5?B	6FB	8RB	:^B	:^B	>wB	D�B	L�B	O�B	P�B	O�B	N�B	N�B	O�B	P�B	R�B	VB	W
B	YB	[#B	]/B	_;B	cTB	e`B	ffB	k�B	k�B	k�B	k�B	m�B	o�B	q�B	s�B	t�B	u�B	u�B	u�B	v�B	v�B	w�B	z�B	{�B	}�B	� B	� B	�B	�B	�B	�B	�B	�+B	�+B	�=B	�JB	�VB	�\B	�bB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�?B	�FB	�LB	�RB	�XB	�XB	�jB	��B	ÖB	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
	7B
	7B

=B

=B
JB
JB
JB
PB
PB
PB
VB
VB
\B
bB
bB
bB
hB
oB
oB
oB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
.B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
33B
33B
33B
49B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
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
H�B
H�B
H�B
H�B
H�B
J�B
J�B
J�B
J�B
J�B
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
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
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
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
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
ffB
ffB
ffB
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
iyB
iyB
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
k�B
k�B
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
n�B
n�B
n�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B0!B0!B0!B1'B1'B1'B1'B1'B2-B2-B49B49B49B5?B6FB6FB7LB6FB8RB9XB9XB9XB9XB8RB:^B=qBB�BB�BB�BB�BB�BB�BA�B?�B?�B@ BBB-)BaB�B�B��B�B��B��B�1B��B�xB�tB��B��B�+B�RB��Bl�BM�B@�B9�B4�B*�B!�B_B�B�BJB �B��B�B��B��B�$B�RB��B�&B��B�6B��B��Bv�Bh$Ba-BX�BQ�BN�BF�B*�B�B�B
��B
�B
�mB
�"B
��B
�8B
�B
�&B
��B
�'B
��B
�B
�OB
utB
pB
ncB
l"B
h>B
ZQB
M�B
KB
G�B
>�B
;�B
2�B
pB
�B
)B
�B
�B	�6B	��B	��B	�HB	ԯB	�B	�[B	�wB	�B	�ZB	�8B	�-B	�CB	�&B	�B	��B	��B	�uB	��B	�B	}�B	zB	u�B	p�B	l=B	iB	f�B	eFB	c:B	_!B	Z�B	W�B	VB	R�B	N�B	MPB	J#B	=�B	4�B	/OB	'�B		B	�B	�B	B	�B	�B	�B	  B��B��B�B�
B��B��B��B�kB��B��BΊB�B˒B�_BƎB��B�UB��B��B�6B�JB��B��B�B��B�B��B�B��B�HB�pB��B��B��B�9B�2B��B��B��B��B�"B��B�xB��B�'B��B~(By�BwfBuZBtBrBqABoOBmwBj�Bf�Bb�Ba�B`�B`�Ba�B^5B]IB[=BY�BX�BW�BT�BTFBS�BQ4BP}BO�BM�BL�BLJBLdBG�BG�BD�BB[B>BB=<B;B9�B8�B9�B8�B6�B5�B4�B6�B7�B9>B<PB9XB4�B+�B+kB)�B)�B(�B(
B%�B%zB'8B$tB(>B-wB6+B5tB1'B-�B0�B1B1'B3�B6+B7�B;JB8�B8�B4TB.�B"�B�BkBeB�BmBB�BBdB5B vB#TB(�B.�B.B+B*�B,�B,�B+�B(�B'mB&2B(�B*�B/�B1�B3�B6�B8�B:�B:�B:�B:�B:�B9�B:xB;�BA�BG�BL0BQ�BOBBPBP.BPbBQ�BY�BZ�B\xB\]B_�B_�Ba�Bc�Bc�Bi�Bl=Bn}Bq�BshBtnBvzBz^BzDBz*Bz^B{B~�B�UB�[B�MB��B�tB��B��B��B��B��B��B��B��B�NB�@B�>B�0B�QB�wB��B�|B�tB�zB��B��B��B��B��B��BðB��B��B��B�B�#B�B� B�B� B�&B�,B�SB�QB�dBߊB�vB�|B�B�B�B�B�B��B��B��B�KB�0B��B��B��B� B�B�dB	aB	DB	dB	dB	jB	}B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	%B	(
B	*0B	-)B	1AB	4TB	5ZB	6zB	8�B	:�B	:xB	>]B	D�B	MB	PbB	Q�B	P}B	OBB	OB	PB	QB	S@B	VB	WYB	YKB	[WB	]IB	_VB	cnB	ezB	f�B	k�B	k�B	k�B	k�B	m�B	o�B	q�B	s�B	t�B	u�B	u�B	u�B	v�B	v�B	xB	z�B	|B	~B	� B	�4B	� B	�;B	�-B	�3B	�9B	�EB	�EB	�XB	�dB	��B	�vB	�}B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�B	�B	�B	�B	�B	��B	�B	�]B	�[B	�nB	�ZB	�`B	�fB	�RB	�XB	�rB	�jB	��B	ðB	��B	��B	��B	��B	��B	��B	��B	� B	� B	� B	�@B	�FB	�$B	�+B	�B	�+B	�1B	�kB	�WB	�IB	�OB	�jB	�\B	�\B	�BB	�HB	�HB	�HB	�bB	�B	�NB	�nB	�nB	�B	�nB	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�.B
 B
 4B
 B
'B
'B
-B
3B
GB
MB
MB
SB
YB
EB
_B
	RB
	RB

�B

�B
dB
dB
dB
jB
jB
jB
�B
�B
�B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#B
#�B
#�B
$B
%B
%,B
&LB
'B
)B
)B
)B
)B
*B
*B
*B
*B
+B
+B
+B
+B
+B
+6B
,"B
,B
,"B
,"B
,"B
-)B
-B
-CB
-)B
-CB
./B
/5B
/OB
0;B
0UB
0UB
1[B
2aB
2GB
3MB
3MB
3�B
4nB
5tB
5ZB
6`B
6`B
7LB
7LB
7fB
7fB
7�B
8lB
8lB
8RB
8lB
8lB
8lB
9rB
9�B
:xB
:�B
;�B
<�B
<�B
=�B
=�B
=�B
>]B
>wB
>�B
>�B
>�B
?�B
?}B
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
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
H�B
H�B
H�B
H�B
H�B
J�B
J�B
J�B
J�B
J�B
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
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
O(B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
QB
Q B
Q B
RB
RB
SB
SB
SB
SB
S�B
S�B
TB
TB
S�B
S�B
S�B
TB
T�B
T�B
U2B
UB
UB
UB
VB
VB
VB
VB
WYB
W$B
W?B
XEB
X+B
Y1B
Y1B
Y1B
Y1B
Y1B
Z7B
Z7B
Z7B
Z7B
[=B
[=B
[WB
\CB
\CB
\]B
\]B
]IB
]IB
]IB
]IB
]dB
^OB
^OB
_;B
_;B
_;B
_;B
_VB
_VB
_VB
_VB
`\B
`\B
`\B
`\B
`vB
a|B
abB
abB
bhB
bhB
bhB
cnB
cnB
cnB
c�B
cTB
dtB
cnB
dtB
e`B
ezB
ezB
ezB
ezB
ezB
e`B
e`B
e`B
ezB
ffB
ffB
ffB
fLB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
hsB
i�B
i�B
i�B
iyB
i�B
i�B
j�B
jB
j�B
j�B
j�B
jB
j�B
k�B
k�B
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
n�B
n�B
n�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612150034362016121500343620161215003436201806221218052018062212180520180622121805201804050411112018040504111120180405041111  JA  ARFMdecpA19c                                                                20161211093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161211003516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161211003516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161211003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161211003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161211003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161211003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161211003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161211003518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161211003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20161211013248                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161211153155  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161214153436  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161214153436  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191111  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031805  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111516                      G�O�G�O�G�O�                