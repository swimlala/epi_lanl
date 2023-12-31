CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-04T00:35:17Z creation;2017-10-04T00:35:21Z conversion to V3.1;2019-12-19T08:00:07Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20171004003517  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_165                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�*��-�1   @�*�I���@:�4m��9�d�_o� 1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  A   A   A@  A`  A�  A�  A�  A���A�33A�  A�  A�  B   B��B  B  B   B(ffB0ffB8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D��3D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��A�
A?�
A_�
A�
A��A��A��RA��A��A��A��A��B�\B��B��B��B(\)B0\)B7��B?��BG��BO��BW�\B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D��D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D��{D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��D���D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�DՂ�Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�IHD�b�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��#A��#A��/A��/A��;A��/A��/A��;A��;A��HA��HA��HA��TA��`A��`A��/A��#A��#A��HA��;A��#A��#A��#Aײ-A�1A�1A�x�A���A�/A���A�=qA��FA�K�A��/A�oA�r�A�t�A�7LA�-A���A��uA��A�I�A�r�A�~�A�+A��A�ZA�p�A��A�I�A���A��uA�`BA�G�A��A�33A�%A���A�x�A�oA���A�?}A��A��wA��\A��-A�-A�33A��A��PA��wA��`A��A�$�A���A��+A��A���A�jA��jA���A��A��A��`A���A�r�A���A��A��TA��jA��AoA~M�A}�A|��A{��A{/Az��Azn�AzJAy�AyhsAx�RAw�Aw"�Avz�Av�AuG�As
=Aq�Aq�Ap��ApffApVApE�Ao�AnjAmp�Al�AlAj  Ail�Ah��Ah1Ag��Af�yAf�Ae|�Ae
=Ad�DAc�-Acp�Acl�Ac�AbbAa?}A`�uA_ƨA_x�A^��A^ �A]�-A\�A[\)AZ��AZ-AY+AW|�AV��AVA�AV1'AU�AU�AS�-AS�AR��AR^5AQ/AP�+AOANbAM��AM?}AM�AL��AL�yAL9XAI�TAH�yAH�+AF�AE��AE�hAEx�AE7LADr�AC�7AC
=AA�;AA`BA@�jA?��A=XA:��A:I�A9p�A8�DA6�uA5dZA5VA4�A3XA3�A2�+A1O�A/�A.ZA-C�A,ȴA+�A+t�A*�A*ȴA*^5A)�A(��A(~�A'��A&�9A&VA&  A%�A%p�A%7LA$z�A#�PA#C�A"ffA!�A E�A�A��A�;A$�A��A?}AM�A�#Ax�AS�A�!A�AXA��A�A�/A�jA�DA�#A��A  AƨA&�A9XA��AƨA
�RA	x�Av�A^5A�A��AȴAM�A�hA/AȴA��@���@�^5@���@�?}@���@�C�@�o@���@�K�@��@�@�u@��@�-@�%@��@�K�@��@��@�;d@�p�@�I�@���@���@�ƨ@�-@��@��T@��#@���@�@ݙ�@܃@�5?@׮@֗�@���@ӶF@�;d@ҧ�@�E�@�p�@�j@϶F@�t�@Η�@�V@� �@˾w@��H@�V@�@ɉ7@�A�@ǍP@�S�@��@�V@ũ�@�`B@��@Ĵ9@�z�@��;@Õ�@Å@�t�@�S�@�@�?}@���@�Q�@���@�|�@�p�@�r�@�ƨ@�ȴ@�@�G�@��@�5?@��h@�p�@�p�@���@�b@���@�@�p�@�z�@��
@���@�\)@��H@��+@�{@��@��@�"�@��@��\@��@�?}@�z�@��
@�K�@�v�@��h@���@�bN@��w@���@�~�@�ff@�J@���@���@�`B@���@���@�1@�ȴ@�-@�G�@�Ĝ@�b@�S�@��!@�=q@�hs@�1'@�(�@��@��!@�v�@��!@��!@���@��9@�t�@�t�@�K�@��!@���@���@���@���@�~�@�v�@�v�@�ff@���@��9@�j@�1@�t�@��@�ff@��T@�p�@��@��D@�  @��@�t�@��H@���@��+@���@�ff@��@��@���@��@��@�1'@���@��@�V@�V@�J@���@�{@��@��7@��`@�r�@�(�@��F@�dZ@���@���@��R@��!@�~�@���@�O�@�bN@� �@�P@K�@;d@K�@~ȴ@}�@|z�@|z�@|I�@{33@z�@z�H@z��@z��@z�\@zJ@x�u@xb@w��@w
=@v��@vv�@v@u�T@u�-@u?}@t��@t(�@sƨ@s33@r��@q�@q��@qX@p��@p1'@o|�@n�+@m�@m��@m�-@m�@m�@mp�@mp�@m�@lz�@kC�@jn�@ihs@i7L@i&�@h��@h�u@hb@g�;@g|�@f�+@f{@f@e��@e�@eO�@d��@d�@d��@dj@d(�@c�
@c�@c�@cdZ@b�@b��@b�\@b=q@a��@a��@a7L@`��@`r�@`1'@_�P@_
=@^��@^�y@^ȴ@^ȴ@^ȴ@^ȴ@^��@^��@^��@^ff@^$�@]�h@]/@\�@\�j@\��@\Z@[�
@[33@["�@[@[@Z�H@Z^5@Y�@XĜ@X�@XbN@XA�@W�@X �@W�;@Wl�@W
=@V�+@U�@U�-@UO�@T�@T��@Tj@Tj@T�j@T��@T�D@T1@S�
@S�
@S�
@S��@SdZ@R�@R��@RJ@Q7L@P��@P�@P�@Pr�@P �@N��@O�@N�y@N�R@N5?@M��@M�h@M�@Mp�@MO�@M?}@M?}@MO�@M?}@L�j@L�@Lj@LZ@LI�@L9X@L9X@L1@K��@K�
@Kƨ@K�F@KdZ@Ko@J�H@J~�@J=q@J-@I��@IX@HĜ@Hr�@Hb@G�P@F��@F�+@E�-@E`B@EO�@E�@E?}@D�/@D�@Cƨ@C�F@CC�@Co@B�H@BJ@A�7@AX@A&�@@��@@1'@@b@@  @?��@?�w@?|�@?�@>�@>ȴ@>��@>E�@=@<�/@<Z@;�m@;�F@;��@;t�@;33@:��@:M�@9��@9X@8��@8��@8��@8��@8A�@8b@7��@7|�@7K�@7
=@6��@6$�@5�T@5�-@5p�@5?}@5/@5V@4�/@4�D@4z�@4I�@3��@3��@333@3@2��@2n�@2^5@2=q@1��@1�^@1��@1��@1hs@1G�@17L@1%@0�`@0��@0bN@0b@/�;@/�@.�y@.�R@.��@.�+@.V@.{@.@-��@-�-@-`B@-V@,�@,�D@,�@,1@+�
@+�
@+�
@+�F@+��@+dZ@+o@*�@*�H@*��@*��@*�\@*-@)�@)��@)��@)�7@)G�@)&�@)&�@)&�@)�@)�@(��@(��@(��@(b@'�w@'�@'��@'�P@'l�@';d@&��@&��@&5?@&$�@&{@%�T@%��@%��@%�-@%��@%`B@%/@%V@$�/@$��@$��@$Z@$9X@#��@#��@#S�@#"�@#o@#o@"�H@"��@"��@"M�@"J@!�@!��@!x�@!G�@ Ĝ@ Q�@��@|�@;d@��@��@v�@$�@�@��@/@�j@�D@z�@j@Z@�@��@�m@ƨ@��@S�@�@��@M�@�7@��@��@��@�@A�@ �@b@b@�@��@�@��@|�@l�@��@v�@@�T@��@�-@�-@��@�h@p�@/@�@��@�D@�D@�D@j@��@�
@�@��@�!@n�@-@-@�@��@��@��@x�@hs@X@&�@%@��@�9@r�@bN@Q�@ �@�@�@�@�@��@�w@��@�P@l�@l�@\)@K�@+@�@
=@��@�y@��@��@��@��@E�@$�@��@�h@`B@O�@/@��@�@�@��@�D@(�@1@�m@ƨ@t�@C�@C�@33@o@
�H@
�!@
n�@
-@
�@	��@	�^@	�7@	7L@	�@��@�@�P@
=@ȴ@��@��@ff@{@��@?}@�/@�j@�@��@z�@I�@(�@��@ƨ@ƨ@ƨ@�F@��@��@t�@o@@��@�\@M�@-@��@�#@hs@7L@&�@�@ ��@ Ĝ@ �9@ ��@ r�@ r�@ r�@ r�@ bN@ A�@   ?���?�;d?���?��?��R?�v�?�V?��?��h?�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��#A��#A��/A��/A��;A��/A��/A��;A��;A��HA��HA��HA��TA��`A��`A��/A��#A��#A��HA��;A��#A��#A��#Aײ-A�1A�1A�x�A���A�/A���A�=qA��FA�K�A��/A�oA�r�A�t�A�7LA�-A���A��uA��A�I�A�r�A�~�A�+A��A�ZA�p�A��A�I�A���A��uA�`BA�G�A��A�33A�%A���A�x�A�oA���A�?}A��A��wA��\A��-A�-A�33A��A��PA��wA��`A��A�$�A���A��+A��A���A�jA��jA���A��A��A��`A���A�r�A���A��A��TA��jA��AoA~M�A}�A|��A{��A{/Az��Azn�AzJAy�AyhsAx�RAw�Aw"�Avz�Av�AuG�As
=Aq�Aq�Ap��ApffApVApE�Ao�AnjAmp�Al�AlAj  Ail�Ah��Ah1Ag��Af�yAf�Ae|�Ae
=Ad�DAc�-Acp�Acl�Ac�AbbAa?}A`�uA_ƨA_x�A^��A^ �A]�-A\�A[\)AZ��AZ-AY+AW|�AV��AVA�AV1'AU�AU�AS�-AS�AR��AR^5AQ/AP�+AOANbAM��AM?}AM�AL��AL�yAL9XAI�TAH�yAH�+AF�AE��AE�hAEx�AE7LADr�AC�7AC
=AA�;AA`BA@�jA?��A=XA:��A:I�A9p�A8�DA6�uA5dZA5VA4�A3XA3�A2�+A1O�A/�A.ZA-C�A,ȴA+�A+t�A*�A*ȴA*^5A)�A(��A(~�A'��A&�9A&VA&  A%�A%p�A%7LA$z�A#�PA#C�A"ffA!�A E�A�A��A�;A$�A��A?}AM�A�#Ax�AS�A�!A�AXA��A�A�/A�jA�DA�#A��A  AƨA&�A9XA��AƨA
�RA	x�Av�A^5A�A��AȴAM�A�hA/AȴA��@���@�^5@���@�?}@���@�C�@�o@���@�K�@��@�@�u@��@�-@�%@��@�K�@��@��@�;d@�p�@�I�@���@���@�ƨ@�-@��@��T@��#@���@�@ݙ�@܃@�5?@׮@֗�@���@ӶF@�;d@ҧ�@�E�@�p�@�j@϶F@�t�@Η�@�V@� �@˾w@��H@�V@�@ɉ7@�A�@ǍP@�S�@��@�V@ũ�@�`B@��@Ĵ9@�z�@��;@Õ�@Å@�t�@�S�@�@�?}@���@�Q�@���@�|�@�p�@�r�@�ƨ@�ȴ@�@�G�@��@�5?@��h@�p�@�p�@���@�b@���@�@�p�@�z�@��
@���@�\)@��H@��+@�{@��@��@�"�@��@��\@��@�?}@�z�@��
@�K�@�v�@��h@���@�bN@��w@���@�~�@�ff@�J@���@���@�`B@���@���@�1@�ȴ@�-@�G�@�Ĝ@�b@�S�@��!@�=q@�hs@�1'@�(�@��@��!@�v�@��!@��!@���@��9@�t�@�t�@�K�@��!@���@���@���@���@�~�@�v�@�v�@�ff@���@��9@�j@�1@�t�@��@�ff@��T@�p�@��@��D@�  @��@�t�@��H@���@��+@���@�ff@��@��@���@��@��@�1'@���@��@�V@�V@�J@���@�{@��@��7@��`@�r�@�(�@��F@�dZ@���@���@��R@��!@�~�@���@�O�@�bN@� �@�P@K�@;d@K�@~ȴ@}�@|z�@|z�@|I�@{33@z�@z�H@z��@z��@z�\@zJ@x�u@xb@w��@w
=@v��@vv�@v@u�T@u�-@u?}@t��@t(�@sƨ@s33@r��@q�@q��@qX@p��@p1'@o|�@n�+@m�@m��@m�-@m�@m�@mp�@mp�@m�@lz�@kC�@jn�@ihs@i7L@i&�@h��@h�u@hb@g�;@g|�@f�+@f{@f@e��@e�@eO�@d��@d�@d��@dj@d(�@c�
@c�@c�@cdZ@b�@b��@b�\@b=q@a��@a��@a7L@`��@`r�@`1'@_�P@_
=@^��@^�y@^ȴ@^ȴ@^ȴ@^ȴ@^��@^��@^��@^ff@^$�@]�h@]/@\�@\�j@\��@\Z@[�
@[33@["�@[@[@Z�H@Z^5@Y�@XĜ@X�@XbN@XA�@W�@X �@W�;@Wl�@W
=@V�+@U�@U�-@UO�@T�@T��@Tj@Tj@T�j@T��@T�D@T1@S�
@S�
@S�
@S��@SdZ@R�@R��@RJ@Q7L@P��@P�@P�@Pr�@P �@N��@O�@N�y@N�R@N5?@M��@M�h@M�@Mp�@MO�@M?}@M?}@MO�@M?}@L�j@L�@Lj@LZ@LI�@L9X@L9X@L1@K��@K�
@Kƨ@K�F@KdZ@Ko@J�H@J~�@J=q@J-@I��@IX@HĜ@Hr�@Hb@G�P@F��@F�+@E�-@E`B@EO�@E�@E?}@D�/@D�@Cƨ@C�F@CC�@Co@B�H@BJ@A�7@AX@A&�@@��@@1'@@b@@  @?��@?�w@?|�@?�@>�@>ȴ@>��@>E�@=@<�/@<Z@;�m@;�F@;��@;t�@;33@:��@:M�@9��@9X@8��@8��@8��@8��@8A�@8b@7��@7|�@7K�@7
=@6��@6$�@5�T@5�-@5p�@5?}@5/@5V@4�/@4�D@4z�@4I�@3��@3��@333@3@2��@2n�@2^5@2=q@1��@1�^@1��@1��@1hs@1G�@17L@1%@0�`@0��@0bN@0b@/�;@/�@.�y@.�R@.��@.�+@.V@.{@.@-��@-�-@-`B@-V@,�@,�D@,�@,1@+�
@+�
@+�
@+�F@+��@+dZ@+o@*�@*�H@*��@*��@*�\@*-@)�@)��@)��@)�7@)G�@)&�@)&�@)&�@)�@)�@(��@(��@(��@(b@'�w@'�@'��@'�P@'l�@';d@&��@&��@&5?@&$�@&{@%�T@%��@%��@%�-@%��@%`B@%/@%V@$�/@$��@$��@$Z@$9X@#��@#��@#S�@#"�@#o@#o@"�H@"��@"��@"M�@"J@!�@!��@!x�@!G�@ Ĝ@ Q�@��@|�@;d@��@��@v�@$�@�@��@/@�j@�D@z�@j@Z@�@��@�m@ƨ@��@S�@�@��@M�@�7@��@��@��@�@A�@ �@b@b@�@��@�@��@|�@l�@��@v�@@�T@��@�-@�-@��@�h@p�@/@�@��@�D@�D@�D@j@��@�
@�@��@�!@n�@-@-@�@��@��@��@x�@hs@X@&�@%@��@�9@r�@bN@Q�@ �@�@�@�@�@��@�w@��@�P@l�@l�@\)@K�@+@�@
=@��@�y@��@��@��@��@E�@$�@��@�h@`B@O�@/@��@�@�@��@�D@(�@1@�m@ƨ@t�@C�@C�@33@o@
�H@
�!@
n�@
-@
�@	��@	�^@	�7@	7L@	�@��@�@�P@
=@ȴ@��@��@ff@{@��@?}@�/@�j@�@��@z�@I�@(�@��@ƨ@ƨ@ƨ@�F@��@��@t�@o@@��@�\@M�@-@��@�#@hs@7L@&�@�@ ��@ Ĝ@ �9@ ��@ r�@ r�@ r�@ r�@ bN@ A�@   ?���?�;d?���?��?��R?�v�?�V?��?��h?�p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BH�BH�BI�BH�BH�BH�BG�BD�B7LB��B�?B��B�dB�'B��B�=Bq�BW
B[#B`BB^5B^5BT�BL�BaHB^5BT�BJ�BQ�BR�BD�B=qB)�B#�B/B2-B,B�B{BbBVBhBVB1BB��B�#B�^B��B}�Bp�BYBJ�BB�B8RB �B%B
��B
�B
�B
�`B
�5B
��B
��B
ƨB
�qB
��B
��B
�}B
�jB
�FB
�B
��B
��B
��B
��B
�oB
�\B
�JB
�B
�B
|�B
{�B
x�B
v�B
s�B
q�B
l�B
hsB
cTB
^5B
ZB
S�B
E�B
@�B
>wB
;dB
;dB
9XB
7LB
1'B
'�B
!�B
�B
�B
DB
	7B
+B
B	��B	��B	��B	�B	�B	�B	�yB	�yB	�yB	�`B	�/B	�B	�B	��B	��B	��B	ƨB	B	�qB	�?B	�3B	�B	��B	��B	��B	��B	��B	��B	�oB	�7B	�=B	�%B	�B	{�B	w�B	u�B	k�B	n�B	m�B	l�B	jB	gmB	_;B	O�B	J�B	K�B	A�B	<jB	=qB	=qB	9XB	49B	.B	,B	%�B	$�B	�B	uB	+B��B��B��B�B�yB�ZB�yB�TB�HB�HB�)B��B��BƨBĜBƨBBB�}B��B�qB�RB�FB�LB�-B�B�'B�B�B�B�B��B��B��B��B�uB�{B�uB�hB�JB�B�B�%B�B�B~�B� Bw�Bp�BgmBq�Bo�BjBp�Bn�BiyBe`Be`BiyBffBbNBbNBVBQ�BN�BL�BQ�BM�BH�B@�BE�BH�BI�BF�B?}B49B9XB;dB:^B7LB:^B9XB49B'�B,B%�B1'B2-B.B0!B1'B2-B/B,B)�B,B,B.B(�B"�B)�B1'B33B33B33B1'B/B+B%�B$�B.B,B0!B49B49B49B33B1'B49B6FB49B2-B5?B7LB6FB8RB:^B9XB7LB;dB>wB>wB=qB=qB@�B@�BB�BB�BA�BC�BE�BD�BC�B@�B?}BF�BE�BE�BF�B@�BE�BI�BH�BK�BK�BJ�BJ�BT�BXBXBVBS�BS�B[#B\)B]/B`BBdZBe`Be`BffBgmBe`BgmBk�Bp�Bp�Bn�Bo�Bp�Bt�Bv�Bv�Bx�B}�B~�B� B�B�1B�1B�1B�7B�7B�7B�1B�7B�%B�B�VB�\B�{B�{B��B��B��B��B��B��B��B��B�B�B�!B�B��B�B�RB�RB�LB�qB�qB�qB�qB�wB��B��B�}B�qB�qB��B�}B�wBÖBƨBɺB��B��B��B�
B�)B�BB�BB�fB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	
=B	hB	�B	�B	�B	�B	�B	 �B	 �B	�B	�B	"�B	#�B	)�B	)�B	-B	.B	/B	0!B	1'B	33B	8RB	8RB	6FB	<jB	=qB	=qB	=qB	=qB	<jB	<jB	C�B	D�B	E�B	G�B	I�B	J�B	N�B	O�B	O�B	Q�B	S�B	VB	VB	W
B	W
B	[#B	[#B	\)B	^5B	aHB	cTB	gmB	jB	k�B	l�B	m�B	m�B	m�B	l�B	iyB	m�B	s�B	w�B	}�B	~�B	}�B	}�B	~�B	�B	�B	�B	�%B	�7B	�7B	�7B	�=B	�=B	�DB	�PB	�PB	�PB	�PB	�VB	�bB	�bB	�bB	�hB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�FB	�FB	�?B	�?B	�?B	�?B	�LB	�XB	�XB	�^B	�^B	�jB	�wB	�qB	�wB	�wB	�}B	��B	ÖB	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�
B	�#B	�;B	�;B	�5B	�;B	�NB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�`B	�mB	�mB	�sB	�sB	�sB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
1B

=B

=B

=B

=B
DB
DB
JB
PB
PB
PB
PB
VB
\B
\B
bB
hB
bB
hB
hB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
$�B
$�B
$�B
&�B
&�B
'�B
'�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
,B
-B
-B
-B
.B
/B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
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
=qB
=qB
>wB
?}B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
K�B
K�B
K�B
L�B
L�B
L�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
L�B
L�B
M�B
L�B
L�B
N�B
N�B
O�B
P�B
P�B
O�B
O�B
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
S�B
R�B
S�B
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
XB
YB
YB
XB
XB
YB
ZB
[#B
[#B
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
^5B
^5B
_;B
`BB
`BB
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
aHB
aHB
cTB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
iyB
iyB
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
k�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
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
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BH�BH�BI�BH�BH�BH�BHKBF�B="BdBðB�uB�B��B��B�\ButB\)B_BcnB`'B_VBWsBO�Ba�B_�BW$BN�BTaBU�BGEB?}B.B&�B0�B33B-�B#B�BB�B�B�B	7B{B�B�vB�}B�|B��Br�B\xBMBD�B;0B%B
rB
�lB
�B
�kB
�fB
��B
�TB
��B
�1B
��B
��B
��B
��B
��B
�fB
��B
�
B
�zB
�|B
�#B
��B
�HB
�B
�?B
�B
}�B
|jB
yXB
wLB
t9B
r-B
mwB
iyB
dZB
_B
Z�B
UgB
G�B
A�B
?cB
;�B
;�B
9�B
7�B
2-B
)DB
"�B
�B
�B
jB

	B
�B
�B	��B	��B	��B	�B	�GB	�OB	�eB	��B	�B	��B	ބB	�B	��B	��B	�hB	�~B	��B	�aB	��B	��B	�B	�!B	�kB	��B	��B	�B	��B	�1B	��B	��B	��B	��B	��B	}VB	x�B	v�B	m]B	oB	nB	l�B	j�B	g�B	`�B	RoB	LB	L�B	CaB	=qB	=�B	=�B	9�B	5ZB	/5B	,�B	'RB	%�B	�B	MB	
#B��B��B�$B�3B�B��B�0B�B�4B��B�IBյBοBȚB��B�zBÖB�GB�4B��B�BB�rB�2B�B�hB�5B��B��B��B�wB��B��B��B�|B�B�2B��B��B��B��B�%B��B��B�-B��B�B��By$Br�Bi�Br�Bp�Bk�BqBoBj�Bf�BffBi�BgmBc�Bc�BX_BS�BP}BNBRoBO(BJXBB�BFtBI�BJrBG�BAB6�B:DB;�B:�B8RB:�B9�B5%B*0B-�B($B1�B2�B/OB0�B1�B2�B/�B-CB+kB-)B,�B.�B*B$�B*�B1[B3MB3MB3MB1[B/�B,"B'�B&�B/ B-CB0�B4�B4�B4�B3�B1�B4�B6�B5B3hB5�B7�B6�B8�B:�B9�B88B;�B>�B>�B=�B=�B@�BABB�BB�BBBC�BE�BD�BC�BA;B@iBF�BF%BF%BGEBA�BFYBJrBI�BLdBL~BK�BK�BUgBXEBXEBV�BT�BT�B[�B\�B]�B`�Bd�Be�Be�Bf�Bg�Bf2Bh>Bl"Bp�BqBoBpUBq[Bu?BwLBw�By�B~wB}B��B��B�fB�fB��B�lB�lB��B��B��B��B�B��B�B��B�B�+B�)B�5B�pB��B�$B��B�KB�CB�5B�UB��B�6B��B�RB��B��B�qB��B��B��B��B��B��B��B�B�(B��B��B��B��B�B�#B�<B�4B�uB�sB�xB�B�B�B�B�B��B��B��B�B�-B�?B�B�RB�RB�*B�"B�"B�6B	�B	SB	�B	
�B	�B	�B	�B	�B	�B	�B	 �B	 �B	 B	 BB	#:B	$tB	*0B	*KB	-)B	./B	/5B	0oB	1�B	3�B	8RB	8�B	6�B	<�B	=�B	=�B	=qB	=�B	<�B	<�B	C�B	D�B	E�B	G�B	I�B	J�B	N�B	O�B	PB	R B	TFB	V9B	VSB	W?B	WYB	[=B	[WB	\�B	^�B	a�B	c�B	g�B	j�B	k�B	l�B	m�B	m�B	m�B	l�B	i�B	nB	tB	xB	~B	B	~B	~(B	.B	�'B	�GB	�{B	�YB	�RB	�RB	�lB	�XB	�rB	�^B	�jB	��B	�jB	�jB	�pB	�bB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�$B	�>B	�*B	��B	�OB	�OB	�aB	��B	�FB	�`B	�tB	�ZB	��B	��B	�fB	�rB	�rB	�xB	�xB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	żB	��B	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�&B	�&B	�,B	�@B	�,B	�?B	�B	�+B	�EB	�sB	�#B	�VB	�pB	�jB	�pB	�hB	�ZB	�`B	�zB	�fB	�fB	�fB	�B	�B	�B	�B	�B	�B	�sB	�yB	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�(B
 B
 B
 B
 B	�.B	�HB
 4B
'B
-B
3B
3B
MB
MB
SB
YB
zB
KB

=B

=B

XB

rB
DB
xB
dB
jB
jB
�B
�B
pB
vB
vB
}B
hB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
!�B
"�B
"�B
"�B
#B
#�B
#�B
#�B
$B
#�B
$�B
%B
%B
&�B
'B
'�B
(
B
'B
(
B
(
B
)B
*0B
)�B
*B
*0B
*B
*0B
+B
,"B
-)B
-)B
-CB
./B
/B
0!B
1'B
2-B
2GB
2GB
2GB
2aB
3MB
4TB
49B
4TB
4TB
4TB
4TB
4TB
4TB
4TB
5?B
5ZB
6FB
6FB
6`B
6`B
6`B
6`B
7fB
7fB
8lB
8lB
8lB
8lB
8lB
8�B
9�B
:xB
;dB
;dB
;B
;B
;B
;B
<jB
<�B
=�B
=�B
=�B
=�B
>�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
H�B
I�B
K�B
K�B
K�B
L�B
L�B
L�B
K�B
K�B
K�B
MB
L�B
M�B
M�B
L�B
MB
M�B
MB
MB
N�B
N�B
O�B
P�B
P�B
O�B
O�B
Q B
Q B
P�B
Q B
Q B
RB
RB
RB
RB
R�B
S�B
SB
TB
T�B
T�B
T�B
U2B
T�B
UB
T�B
VB
VB
VB
VB
VB
VB
VB
V�B
W
B
W�B
X�B
Y1B
XEB
X+B
Y1B
Z7B
[=B
[=B
\)B
\CB
\CB
]IB
]IB
]/B
]dB
]IB
^OB
^OB
^OB
^OB
_VB
`BB
`BB
_VB
_VB
`\B
`\B
`BB
aHB
abB
abB
abB
abB
bhB
bhB
a|B
a�B
c�B
ezB
ffB
f�B
f�B
f�B
f�B
f�B
g�B
i�B
iyB
i�B
i�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
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
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�1111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<we�<g�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710101049522017101010495220171010104952201806221231382018062212313820180622123138201804050427022018040504270220180405042702  JA  ARFMdecpA19c                                                                20171004093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171004003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171004003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171004003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171004003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171004003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171004003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171004003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171004003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171004003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20171004005532                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20171010014850  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20171010014828  CV  JULD            G�O�G�O�F�W�                JM  ARCAJMQC2.0                                                                 20171010014952  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171010014952  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20171011000000  CF  PSAL_ADJUSTED_QCBW��D}� G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192702  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033138  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                