CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-17T00:35:18Z creation;2018-10-17T00:35:23Z conversion to V3.1;2019-12-19T07:30:15Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20181017003518  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              #A   JA  I2_0576_291                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؉u΁� 1   @؉v�[�@9RM����d8��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|fD|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\)@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCp
Cq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`��Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D|�D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��{D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D��{D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D���D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�61111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A� �A��A��A��A��A�oA�bA�bA�bA�bA�oA�oA�oA��yA�7LA˃A�ZAʥ�Aɏ\A�VA�ffAć+A�I�AuA��A��uA���A�$�A�z�A��A��mA���A���A�(�A�|�A��hA��jA���A��A�1'A�l�A���A�%A���A�ffA�A�bNA�ȴA���A�A���A�G�A�&�A�~�A�7LA�  A���A�XA��;A��A�G�A��^A�\)A���A�I�A���A�A���A�oA�t�A��A�ƨA��hA�
=A��A�`BA�ȴA��uA�C�A�v�A��TA�\)A��yA�ȴA���A��A���A�A�A��mA���A�A��^A���A�~�A+A|�yA|  A{�TAy��AvbAu/At��At��As�Asl�Ar�HAq�Ap��Apr�ApJAop�Am��Al�Ak��Ak;dAj��AhĜAh=qAgƨAfȴAd��Ac;dAaVA`��A_��A^ �A\�RA\VA[A[p�A["�AZ9XAY�AX��AWAVv�AV$�AU�#AUdZAT-AS��ARI�AQ`BAP(�AN�/ANbNAN{AM��AM�AL��AL�\AJ~�AG33AFA�AEAD��AB�AAoA@�+A@�A>��A=��A=�A<ȴA<�!A<��A<E�A;�A9�;A8ZA6�+A4{A2^5A1�A0{A/XA.Q�A-��A-�A-`BA-%A,�yA,ȴA,JA+�wA*��A'�A&jA%�wA%p�A%�A#x�A"��A"E�A ��A bNA �A�PAS�A��A�hAbA�A�A��A\)A�A-A�A�uAt�AZA�7A�`A�jA�DA��A%A1'Al�AVA�hAȴA��AbA
��A	��A	
=Ar�A�AS�A��A��A9XA�TA��A7LA/A%AffA�A1'Ax�A?}A�A ��A ��A ��A �D@�~�@��@�A�@�C�@�`B@��y@�O�@�b@���@�b@�x�@�
=@�C�@柾@�V@��T@�hs@��@��m@㕁@�K�@��#@�j@���@܋D@�hs@�o@�`B@�l�@���@���@�n�@��@�&�@̓u@�(�@˾w@�|�@�^5@ɑh@��@ǶF@Ų-@�Ĝ@�bN@�b@���@�|�@�S�@���@�~�@�@�1@��@��#@�l�@�+@�@���@�v�@�=q@�@��@��9@�-@�/@��j@�(�@��@���@���@���@��P@�l�@�C�@��@��@�V@�@�/@��u@�b@�1@���@���@��@�ƨ@��@�;d@���@�-@��#@���@�p�@��`@�1@�\)@��y@�V@�O�@��`@��9@��@�+@�v�@���@���@��^@��@�G�@�%@�r�@�ƨ@�o@�-@�G�@���@��w@�@���@��P@��@�t�@�t�@�l�@�l�@�dZ@��@���@�J@�@�Z@��P@�\)@�;d@�+@�
=@��y@���@�$�@��@��T@��h@�O�@��`@��
@�;d@��@�ȴ@�v�@�M�@��@���@�/@��@���@�Ĝ@��D@��@�bN@���@�K�@��@��+@�5?@��T@�@��-@���@��h@�G�@���@�I�@�1'@�9X@�9X@�1@�|�@�+@�"�@��@�"�@�+@�;d@�33@��@��@�X@�;@\)@;d@~ȴ@~E�@~E�@~E�@~E�@~5?@}@}`B@}`B@}O�@}�@|�@|�@{�
@{��@z�@z��@z~�@zn�@z~�@z~�@z�\@z��@z�!@z��@z��@z��@z��@z�\@{o@{33@z�!@y%@w|�@vv�@vv�@v��@v��@v�R@v��@vv�@v5?@v@u�-@uO�@t�@t�@tz�@tI�@s��@t1@t1@s�m@sƨ@sƨ@s�F@s�F@s�@sS�@s@s@s"�@s"�@r�H@r^5@r=q@r^5@q�@qG�@q%@p��@pb@o\)@o�P@ol�@nV@nE�@n5?@m�-@m��@m@m�@l��@k�F@k�@kS�@j��@jJ@i�^@ix�@h�9@h  @g\)@g�@f�+@e�@e��@e`B@e`B@e/@eV@d�@d�@d�j@c�F@c��@cS�@c@b~�@bM�@bJ@a�#@aG�@`  @_�@` �@^�@]/@[ƨ@[�F@[��@[t�@["�@Z��@ZM�@Y�^@Y�7@Y�7@Y�7@Y�7@Y�7@Y�7@Y�7@Y�7@Y�7@Y�7@Yhs@Yhs@Y&�@X��@XĜ@XĜ@Xr�@W��@V��@V@U��@U@U�-@Up�@UV@T��@T(�@S�m@Sƨ@S��@S�@SS�@S@S@R�@R�\@R~�@R=q@R=q@R=q@R-@R-@RJ@R�@R�@RJ@Q�7@QG�@QG�@QX@Q7L@PbN@P�@P��@PQ�@O�;@O�w@O��@O�w@N��@N@NV@Nv�@N5?@N{@M�-@M��@M?}@L�/@L�@L�D@L�@Kt�@K"�@J�H@Jn�@I�^@I&�@H1'@H  @Hb@G�@Gl�@G�@F�@F��@Fv�@FE�@E@E/@DI�@C�m@C�F@Ct�@CdZ@C33@C@B��@B��@B�!@B��@B��@B�\@B~�@B-@A�@A��@A��@Ahs@A&�@A�@A%@@��@@��@@��@@�9@@Q�@?�;@?\)@?�@?�@>ȴ@>ff@>E�@>{@=��@=O�@<�@<9X@;t�@;S�@;C�@;"�@;o@;@;@:�@:�H@:�!@:�!@:~�@9��@8��@8bN@7�@7+@6�R@6��@6E�@5�@5�T@5�h@5?}@4�D@4Z@4�@3ƨ@3��@3�@3t�@3C�@333@333@333@333@333@2�@2-@1��@1x�@17L@0��@0�9@0�9@0r�@/�;@/l�@/;d@.��@.ȴ@.�R@.v�@-�T@-@-�@-�@-V@,��@,j@+�
@+�F@+��@+�@+S�@*�@*��@*��@*��@*��@*~�@*M�@*J@)�#@)��@)�7@)X@)%@(��@(��@(�`@(��@(�9@(Q�@( �@'�@'�;@'��@'��@'K�@'
=@&��@&ff@&$�@%�T@%O�@$�/@$��@$�D@$�D@$(�@#ƨ@#��@#dZ@#"�@"�!@"~�@"=q@"J@!��@!�@!�@!�^@!��@!��@!hs@!X@!%@ ��@ r�@ r�@ A�@ b@��@\)@�R@�+@ff@�@�@��@j@I�@I�@9X@9X@�@�@�@�m@��@C�@@��@�!@~�@M�@J@�@�^@X@G�@7L@%@�9@bN@A�@1'@�;@��@�@��@|�@+@��@��@�+@�+@{@��@�@p�@O�@?}@�@��@�@�D@�D@�D@�D@�D@�D@z�@z�@Z@(�@�@1@1@�F@�@S�@C�@"�@o@@�@�@�H@��@��@��@��@��@�!@��@��@��@��@�!@�!@�\@~�@~�@n�@^5@-@�@�@J@J@��@��@G�@&�@��@�`@Ĝ@��@�@b@�P@l�@\)@;d@�@�y@ȴ@V@@��@`B@?}@��@��@��@�D@I�@1@��@�m@�
@t�@33@o@@
�H@
�!@
�\@
~�@
~�@
n�@	��@	�^@	��@	X@	&�@��@�9@�u@�@r�@  @l�@K�@;d@+@�@�@�@��@v�@v�@v�@v�@ff@$�@@��@`B@?}@?}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A� �A��A��A��A��A�oA�bA�bA�bA�bA�oA�oA�oA��yA�7LA˃A�ZAʥ�Aɏ\A�VA�ffAć+A�I�AuA��A��uA���A�$�A�z�A��A��mA���A���A�(�A�|�A��hA��jA���A��A�1'A�l�A���A�%A���A�ffA�A�bNA�ȴA���A�A���A�G�A�&�A�~�A�7LA�  A���A�XA��;A��A�G�A��^A�\)A���A�I�A���A�A���A�oA�t�A��A�ƨA��hA�
=A��A�`BA�ȴA��uA�C�A�v�A��TA�\)A��yA�ȴA���A��A���A�A�A��mA���A�A��^A���A�~�A+A|�yA|  A{�TAy��AvbAu/At��At��As�Asl�Ar�HAq�Ap��Apr�ApJAop�Am��Al�Ak��Ak;dAj��AhĜAh=qAgƨAfȴAd��Ac;dAaVA`��A_��A^ �A\�RA\VA[A[p�A["�AZ9XAY�AX��AWAVv�AV$�AU�#AUdZAT-AS��ARI�AQ`BAP(�AN�/ANbNAN{AM��AM�AL��AL�\AJ~�AG33AFA�AEAD��AB�AAoA@�+A@�A>��A=��A=�A<ȴA<�!A<��A<E�A;�A9�;A8ZA6�+A4{A2^5A1�A0{A/XA.Q�A-��A-�A-`BA-%A,�yA,ȴA,JA+�wA*��A'�A&jA%�wA%p�A%�A#x�A"��A"E�A ��A bNA �A�PAS�A��A�hAbA�A�A��A\)A�A-A�A�uAt�AZA�7A�`A�jA�DA��A%A1'Al�AVA�hAȴA��AbA
��A	��A	
=Ar�A�AS�A��A��A9XA�TA��A7LA/A%AffA�A1'Ax�A?}A�A ��A ��A ��A �D@�~�@��@�A�@�C�@�`B@��y@�O�@�b@���@�b@�x�@�
=@�C�@柾@�V@��T@�hs@��@��m@㕁@�K�@��#@�j@���@܋D@�hs@�o@�`B@�l�@���@���@�n�@��@�&�@̓u@�(�@˾w@�|�@�^5@ɑh@��@ǶF@Ų-@�Ĝ@�bN@�b@���@�|�@�S�@���@�~�@�@�1@��@��#@�l�@�+@�@���@�v�@�=q@�@��@��9@�-@�/@��j@�(�@��@���@���@���@��P@�l�@�C�@��@��@�V@�@�/@��u@�b@�1@���@���@��@�ƨ@��@�;d@���@�-@��#@���@�p�@��`@�1@�\)@��y@�V@�O�@��`@��9@��@�+@�v�@���@���@��^@��@�G�@�%@�r�@�ƨ@�o@�-@�G�@���@��w@�@���@��P@��@�t�@�t�@�l�@�l�@�dZ@��@���@�J@�@�Z@��P@�\)@�;d@�+@�
=@��y@���@�$�@��@��T@��h@�O�@��`@��
@�;d@��@�ȴ@�v�@�M�@��@���@�/@��@���@�Ĝ@��D@��@�bN@���@�K�@��@��+@�5?@��T@�@��-@���@��h@�G�@���@�I�@�1'@�9X@�9X@�1@�|�@�+@�"�@��@�"�@�+@�;d@�33@��@��@�X@�;@\)@;d@~ȴ@~E�@~E�@~E�@~E�@~5?@}@}`B@}`B@}O�@}�@|�@|�@{�
@{��@z�@z��@z~�@zn�@z~�@z~�@z�\@z��@z�!@z��@z��@z��@z��@z�\@{o@{33@z�!@y%@w|�@vv�@vv�@v��@v��@v�R@v��@vv�@v5?@v@u�-@uO�@t�@t�@tz�@tI�@s��@t1@t1@s�m@sƨ@sƨ@s�F@s�F@s�@sS�@s@s@s"�@s"�@r�H@r^5@r=q@r^5@q�@qG�@q%@p��@pb@o\)@o�P@ol�@nV@nE�@n5?@m�-@m��@m@m�@l��@k�F@k�@kS�@j��@jJ@i�^@ix�@h�9@h  @g\)@g�@f�+@e�@e��@e`B@e`B@e/@eV@d�@d�@d�j@c�F@c��@cS�@c@b~�@bM�@bJ@a�#@aG�@`  @_�@` �@^�@]/@[ƨ@[�F@[��@[t�@["�@Z��@ZM�@Y�^@Y�7@Y�7@Y�7@Y�7@Y�7@Y�7@Y�7@Y�7@Y�7@Y�7@Yhs@Yhs@Y&�@X��@XĜ@XĜ@Xr�@W��@V��@V@U��@U@U�-@Up�@UV@T��@T(�@S�m@Sƨ@S��@S�@SS�@S@S@R�@R�\@R~�@R=q@R=q@R=q@R-@R-@RJ@R�@R�@RJ@Q�7@QG�@QG�@QX@Q7L@PbN@P�@P��@PQ�@O�;@O�w@O��@O�w@N��@N@NV@Nv�@N5?@N{@M�-@M��@M?}@L�/@L�@L�D@L�@Kt�@K"�@J�H@Jn�@I�^@I&�@H1'@H  @Hb@G�@Gl�@G�@F�@F��@Fv�@FE�@E@E/@DI�@C�m@C�F@Ct�@CdZ@C33@C@B��@B��@B�!@B��@B��@B�\@B~�@B-@A�@A��@A��@Ahs@A&�@A�@A%@@��@@��@@��@@�9@@Q�@?�;@?\)@?�@?�@>ȴ@>ff@>E�@>{@=��@=O�@<�@<9X@;t�@;S�@;C�@;"�@;o@;@;@:�@:�H@:�!@:�!@:~�@9��@8��@8bN@7�@7+@6�R@6��@6E�@5�@5�T@5�h@5?}@4�D@4Z@4�@3ƨ@3��@3�@3t�@3C�@333@333@333@333@333@2�@2-@1��@1x�@17L@0��@0�9@0�9@0r�@/�;@/l�@/;d@.��@.ȴ@.�R@.v�@-�T@-@-�@-�@-V@,��@,j@+�
@+�F@+��@+�@+S�@*�@*��@*��@*��@*��@*~�@*M�@*J@)�#@)��@)�7@)X@)%@(��@(��@(�`@(��@(�9@(Q�@( �@'�@'�;@'��@'��@'K�@'
=@&��@&ff@&$�@%�T@%O�@$�/@$��@$�D@$�D@$(�@#ƨ@#��@#dZ@#"�@"�!@"~�@"=q@"J@!��@!�@!�@!�^@!��@!��@!hs@!X@!%@ ��@ r�@ r�@ A�@ b@��@\)@�R@�+@ff@�@�@��@j@I�@I�@9X@9X@�@�@�@�m@��@C�@@��@�!@~�@M�@J@�@�^@X@G�@7L@%@�9@bN@A�@1'@�;@��@�@��@|�@+@��@��@�+@�+@{@��@�@p�@O�@?}@�@��@�@�D@�D@�D@�D@�D@�D@z�@z�@Z@(�@�@1@1@�F@�@S�@C�@"�@o@@�@�@�H@��@��@��@��@��@�!@��@��@��@��@�!@�!@�\@~�@~�@n�@^5@-@�@�@J@J@��@��@G�@&�@��@�`@Ĝ@��@�@b@�P@l�@\)@;d@�@�y@ȴ@V@@��@`B@?}@��@��@��@�D@I�@1@��@�m@�
@t�@33@o@@
�H@
�!@
�\@
~�@
~�@
n�@	��@	�^@	��@	X@	&�@��@�9@�u@�@r�@  @l�@K�@;d@+@�@�@�@��@v�@v�@v�@v�@ff@$�@@��@`B@?}@?}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B5?B(�B�B$�BB�B%�B8RB9XB:^B;dB �BB��BŢB�fB�B"�B49B?}B.BM�BS�BR�BM�BI�BB�BC�BB�B:^B,B#�B{BB�B�5BǮB��B�TB�#B��BȴB�FB��Bz�BK�B�Bz�Bv�Bx�Bu�Br�BhsB]/BYB^5BT�BG�B+B�B�B�B�BB
��B
��B
��B
��B
�B
�)B
�FB
��B
�FB
�9B
��B
�B
}�B
�B
k�B
YB
ZB
ZB
?}B
�B
:^B
B�B
=qB
5?B
0!B
,B
"�B
�B
"�B
�B
bB
B	��B
B	��B	��B	�;B	�B	�fB	�B	��B	�qB	�?B	B	�^B	��B	��B	�!B	�B	�B	��B	��B	��B	��B	�DB	�B	�JB	�1B	�B	q�B	r�B	dZB	bNB	VB	VB	YB	ZB	S�B	M�B	K�B	;dB	�B��B	\B	VB��B�mB�B��B��B�B�B�B��B��B��B�B�B��BȴB�jB��B�3B�XB�^B�}B�LBBB��B��B�wB�dB�!B�B��By�B�+B�\B�{B�VB{�B�%B�+B{�B�B�+B�B�Bv�BjBffBe`Bl�Be`Bk�BffB]/BZBR�BK�BK�BO�BR�BXBS�BF�BE�BB�B?}BC�B5?B6FBC�B8RB,B+B6FB5?B9XB8RB;dB=qB:^B;dB;dB9XB<jB6FB-B"�B!�B,B7LB8RB8RB5?B49B-B�B�B+B!�B�BuB�B�BbBoB	7BbBB$�B'�B&�B$�B"�B!�B$�B"�B�B{BVB�B	7BbB�B�B	7B&�B,B+B+B,B-B.B-B&�B'�B+B#�B �B-B49B6FB6FB5?B6FB49B1'B,B#�B�B�B(�BC�BE�BD�BC�BC�B?}B49B!�B0!BM�BT�BXB[#B_;B`BB_;B_;B^5B]/B]/B\)BXBYB[#B^5BbNBhsBhsBhsBgmBffBe`Be`BdZBffBhsBjBiyBe`Be`BiyBm�Bm�Bl�Bu�Bx�Bu�Bt�B{�B�B�%B�+B�B�B�B�B�B�%B�+B�=B�bB�7B�DB�{B�FB�LB�LB�RB�RB�RB�FB�9B�3B�9B�RB�3B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�/B�fB�sB�sB�B�B�B�B��B��B��B��B��B��B��B��B	B	%B		7B	JB	bB	hB	hB	hB	bB	oB	�B	�B	 �B	 �B	!�B	!�B	(�B	0!B	1'B	49B	5?B	6FB	6FB	33B	1'B	6FB	;dB	S�B	[#B	]/B	_;B	bNB	cTB	cTB	bNB	aHB	cTB	ffB	ffB	e`B	e`B	e`B	hsB	iyB	iyB	n�B	p�B	t�B	w�B	x�B	y�B	z�B	z�B	{�B	|�B	}�B	~�B	� B	�7B	�PB	�DB	�=B	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�?B	�FB	�9B	�3B	�?B	�FB	�9B	�LB	�}B	�wB	�dB	�wB	��B	�wB	��B	��B	�}B	�qB	�}B	ŢB	ĜB	ÖB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	��B	�B	�)B	�/B	�/B	�/B	�/B	�/B	�#B	�B	�NB	�TB	�5B	�;B	�TB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
1B
	7B

=B

=B

=B

=B

=B

=B

=B
	7B
1B
	7B
DB
VB
PB
JB
bB
uB
hB
bB
oB
{B
oB
\B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
%�B
%�B
$�B
%�B
'�B
'�B
'�B
(�B
+B
+B
+B
+B
+B
)�B
(�B
)�B
+B
-B
.B
-B
-B
/B
/B
.B
.B
/B
.B
.B
2-B
33B
33B
33B
33B
33B
33B
33B
2-B
2-B
1'B
/B
.B
2-B
1'B
33B
33B
5?B
5?B
5?B
6FB
5?B
5?B
49B
7LB
8RB
8RB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
:^B
9XB
7LB
9XB
:^B
:^B
;dB
<jB
<jB
;dB
9XB
;dB
<jB
<jB
=qB
=qB
<jB
<jB
>wB
>wB
>wB
@�B
?}B
>wB
?}B
A�B
B�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
F�B
G�B
F�B
H�B
H�B
H�B
G�B
I�B
J�B
L�B
K�B
J�B
J�B
L�B
K�B
K�B
K�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
N�B
N�B
N�B
N�B
M�B
O�B
O�B
O�B
N�B
N�B
M�B
M�B
O�B
O�B
N�B
N�B
M�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
R�B
S�B
T�B
T�B
T�B
T�B
VB
VB
T�B
W
B
W
B
VB
T�B
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
XB
W
B
W
B
ZB
ZB
ZB
XB
YB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
^5B
^5B
^5B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
]/B
^5B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
aHB
bNB
bNB
bNB
aHB
bNB
bNB
bNB
bNB
aHB
aHB
`BB
cTB
cTB
cTB
bNB
bNB
bNB
aHB
aHB
e`B
e`B
e`B
e`B
dZB
dZB
cTB
dZB
e`B
e`B
gmB
ffB
gmB
gmB
hsB
gmB
hsB
jB
jB
iyB
hsB
iyB
k�B
k�B
jB
jB
k�B
l�B
l�B
k�B
iyB
jB
l�B
k�B
l�B
l�B
l�B
m�B
n�B
m�B
k�B
k�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
r�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B�vB�RB�B5�B*�B!-B'8B�B#B(sB:B;�B<�B=�B$�B�B�CB�.B�B$B'B8BCaB2BO�BU�BT�BO�BKDBDBDgBC-B;dB-�B%`B�B�B��B��B��B�TB��B��B��BɆB��B�IB.BQhB�gB|6Bw�By�Bv`BsMBi�B^�BZ7B^�BU�BIlB-�B�B�BVB�B�B OB
�B
��B
�+B
�OB
��B
��B
��B
��B
��B
�#B
�zB
� B
�B
m�B
[qB
[=B
Z�B
BAB
B
;JB
B�B
=�B
6+B
0�B
,�B
$B
B
# B
]B
�B
�B	��B
�B	��B	��B	�|B	�"B	�RB	ٴB	�3B	�}B	��B	�GB	��B	��B	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	sMB	s�B	fB	c�B	W�B	WsB	Y�B	Z�B	T�B	N�B	LJB	<�B	�B��B	bB	\B��B�0B�=B��B��B�'B� B��B�>B�B�B�CB��BѷB��B��B�B�ZB��B��B�iB��B�B��B�B��B��B��B�AB��B��B}<B��B�.B��B�BB~B�B�B}�B��B��B��B��BxBl"Bh>Bf�BmwBf�Bk�BgB^OB[=BTaBMjBM6BQ BS�BX_BT{BG�BF�BC�B@�BDgB72B7LBC�B9rB-�B,WB72B6+B:*B9$B<B=�B:�B;�B;�B9�B<�B6�B./B$&B#nB,�B7�B8�B8�B5�B4�B-�B \B�B+QB"�B�B�B�B�BB�B)B�BYB%`B(>B'RB%`B#nB"hB%,B#TB�B�BB�BxB�B�B�B�B'mB,�B+�B+�B,�B-�B.}B-wB'�B(�B+�B$�B"4B-�B4�B6�B6�B5�B6zB4�B1�B,�B%`B�B�B*BC�BE�BD�BC�BC�B@B5tB$�B1�BNVBUgBXyB[qB_VB`\B_VB_VB^jB]dB]dB\xBX�BY�B[�B^�Bb�Bh�Bh�Bh�Bg�Bf�Be�Be�Bd�Bf�Bh�Bj�Bi�Be�BfBi�Bm�BnBmCBvBy$Bv`ButB|jB�UB�?B�zB�mB�mB�mB��B��B��B��B��B��B�XB��B��B�FB�fB�fB��B�lB�lB�zB��B��B��B��B�TB�B��B��B��B�B�B�"B�\B�&B�B�@B�FB�{BҽBݘB�B�B��B�B��B��B��B��B��B��B�B��B�>B�fB�<B	oB	tB		�B	~B	}B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	"NB	)*B	0!B	1AB	49B	5?B	6`B	6zB	3�B	1�B	6�B	<PB	TB	[=B	]dB	_pB	bNB	cTB	cTB	bhB	a|B	cnB	ffB	ffB	ezB	e�B	e�B	h�B	i�B	i�B	n�B	p�B	t�B	w�B	x�B	y�B	z�B	z�B	|B	|�B	~B	B	�4B	�B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	��B	�IB	�;B	�aB	�tB	�FB	�nB	��B	�ZB	�`B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	żB	ĶB	��B	��B	�B	��B	�B	�B	�B	� B	�.B	�B	�B	�B	�
B	�$B	�$B	�$B	�+B	�9B	�{B	�1B	�CB	�IB	�~B	�IB	�IB	�IB	�qB	ڠB	�NB	�nB	��B	��B	�B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�2B	�	B	��B	��B	�B	��B	�B	�*B	�*B	��B	��B	�B	�"B
 4B
'B
3B
9B
SB
1B
	RB

XB

=B

=B

=B

rB

=B

=B
	RB
fB
	lB
^B
VB
jB
~B
bB
uB
�B
�B
�B
{B
�B
�B
�B
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
%�B
%�B
$�B
&B
'�B
(
B
(
B
)B
+B
+B
+B
+B
+B
*B
)*B
*0B
+6B
-)B
./B
-)B
-CB
/OB
/5B
.IB
.IB
/OB
.cB
.cB
2-B
33B
3MB
33B
33B
33B
33B
3MB
2GB
2GB
1AB
/iB
.cB
2aB
1vB
3hB
3�B
5ZB
5ZB
5ZB
6`B
5tB
5tB
4�B
7fB
8�B
8lB
9rB
:^B
:�B
:xB
;JB
;dB
;dB
;dB
:xB
9rB
7�B
9rB
:xB
:xB
;�B
<�B
<�B
;dB
9�B
;B
<�B
<�B
=�B
=�B
<�B
<�B
>�B
>�B
>�B
@�B
?�B
>�B
?�B
A�B
B�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
F�B
G�B
F�B
H�B
H�B
H�B
G�B
I�B
J�B
L�B
K�B
J�B
J�B
L�B
K�B
K�B
K�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
N�B
OB
N�B
N�B
NB
O�B
O�B
O�B
N�B
N�B
NB
NB
PB
PB
OB
OB
N"B
Q B
RB
R�B
R�B
R�B
SB
R�B
R�B
RB
RB
R B
SB
TB
T�B
UB
UB
UB
VB
VB
U2B
W
B
W$B
VB
U2B
V9B
W$B
W$B
W?B
XB
X+B
XB
X+B
W$B
W?B
ZB
Z7B
ZB
XEB
YKB
[WB
[#B
[=B
[WB
[=B
[=B
\CB
]/B
]/B
^B
^5B
^5B
]/B
]/B
]/B
]dB
]IB
^B
^5B
^5B
]IB
^OB
_VB
`BB
`\B
`\B
aHB
abB
aHB
abB
abB
aHB
bNB
aHB
aHB
a|B
bNB
bNB
bNB
bNB
bNB
bNB
abB
aHB
bNB
b4B
bNB
abB
bNB
bNB
bNB
bNB
abB
aHB
`vB
cTB
cnB
cTB
bhB
bhB
bNB
a|B
a|B
ezB
e`B
ezB
ezB
dZB
dtB
c�B
dtB
ezB
ezB
g�B
f�B
g�B
g�B
h�B
g�B
h�B
j�B
jB
i�B
h�B
i�B
k�B
k�B
j�B
j�B
k�B
lqB
l�B
k�B
i�B
j�B
l�B
k�B
l�B
l�B
l�B
m�B
n�B
m�B
k�B
k�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
q�B
q�B
q�B
q�B
p�B
p�B
p�B
p�B
p�B
r�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�U<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810210035412018102100354120181021003541201810210200192018102102001920181021020019201810220022592018102200225920181022002259  JA  ARFMdecpA19c                                                                20181017093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181017003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181017003521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181017003522  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181017003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181017003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181017003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181017003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181017003523  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181017003523                      G�O�G�O�G�O�                JA  ARUP                                                                        20181017005626                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181017153617  CV  JULD            G�O�G�O�F�K�                JM  ARCAJMQC2.0                                                                 20181020153541  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181020153541  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181020170019  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181021152259  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                