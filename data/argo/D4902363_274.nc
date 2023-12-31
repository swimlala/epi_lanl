CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-27T00:35:14Z creation;2018-08-27T00:35:19Z conversion to V3.1;2019-12-19T07:34:10Z update;     
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20180827003514  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_274                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�|��N�1   @�|��6� @9��2�W��d_����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A���A�  B   B��B  B  B   B(  B0ffB7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  Dy�D��D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @2�\@\)@��@��A�
A?�
A_�
A�
A��A��A��A��AиRA�RA��A��B�\B��B��B��B'��B0\)B7�\B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�D��D�\D\D�\D\D�\D\D�\Dx�D��D\D�\D\D�\D��D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<x�D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO��DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds��Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�B�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D��D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D�|{D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D��D�?�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��`A��yA��mA��`A��TA��`A��`A��;A��/A��#A��/A��TA��HA��HA���A�A�p�A��A�K�A�S�A�jA͋DA���AɬA���A�VA���A���A�A�A�+A�E�A��A���A�1A�7LA��-A�VA��A���A�-A��A�S�A��A�\)A�9XA��^A�^5A�?}A�|�A��FA��A�oA�(�A�n�A�Q�A���A��/A�/A�ZA�%A�/A��
A�-A��A��/A�I�A��/A���A�I�A�1'A�XA��uA�(�A���A��A�-A��+A���A�=qA��HA���A�/A���A��A�K�A�ZA�I�A�7LA�VA���A��#A�ƨA�A���A�Q�A���A�ffA�jA��7A�  A�G�A~�A}x�A|{A{;dAz��AxA�Au�7As�Aq��Am�;Al�\Ak�FAj�uAh�`Ag;dAf�Ad�`Ac�#A`��A_XA^��A]?}A[��AZ��AZ1'AX�AW`BAVJAU\)AU7LAT�yAS;dAR �AQ/AP(�AN��AM%AK33AI�FAH�HAG�-AF^5AE�ADr�AC��AB�\AA�A@�A?VA=��A<Q�A;�mA;�A9��A8�A8ffA8$�A7�A5�A3��A3G�A2ȴA2�A1��A0z�A/?}A/%A-\)A,�\A,r�A,$�A+�^A+"�A)�
A(z�A'�A&E�A%��A$ZA#�A"��A!�A!�A!XA �jA �DA ZA 5?A A�wA/A�`A��A��A~�Al�A�AoAA�A��A�
AG�A��A�AA�A��A�A�FA��A��A�hAK�A��A��At�AbNA+A��A��A/AjA�
Ax�A
�RA
=qA
$�A	�
AjA�
A?}Av�A��A��A  A�AƨA ��A �DA ff@��m@�C�@���@�x�@�Q�@��@�(�@�@�{@�&�@�bN@���@�t�@��@�ȴ@�J@�9X@��T@�33@�@�j@�F@�dZ@�+@旍@��#@��
@�(�@�%@�I�@�M�@� �@ָR@��@�%@�Z@��m@ӍP@�@ѡ�@Ь@�z�@�9X@�b@��;@ϥ�@�S�@�@�^5@�Z@���@ư!@ư!@Ƈ+@�@�1@�K�@�ff@��`@��m@�K�@�p�@��
@��-@�V@���@���@�9X@���@��F@��P@�5?@��-@��j@��@��!@��@�@��7@�p�@��@��F@�V@���@�ƨ@�o@��R@�M�@���@��@�\)@�v�@�{@��@�G�@�1'@��+@���@�9X@��
@��H@��+@�5?@��@�  @��;@���@��@�33@��@���@�V@���@�V@��@��@��@�33@�@��@���@���@��+@�~�@�n�@�M�@�$�@�J@��@���@�x�@�G�@�?}@�/@�V@�Ĝ@�Z@��;@�ȴ@���@�&�@���@��@�bN@��;@��
@���@���@�ff@�E�@��@�hs@�&�@���@��9@�Z@�b@�|�@�v�@��@�@��-@�x�@�7L@��u@�(�@�b@��
@���@���@��P@�t�@�C�@��@��!@�n�@�@��h@�`B@�?}@�%@�z�@���@��;@��F@�C�@�33@�"�@��@�o@�o@�
=@�@���@�n�@��T@�hs@��@��`@��/@��9@��@�Q�@�I�@�9X@�1'@�1@~�@~@}@}p�@}�@|�/@|I�@{33@yhs@x��@xb@w+@vV@u@u�@u/@t�@t�D@t(�@s��@s�
@sƨ@s�@s33@r��@r^5@rJ@rJ@q�^@q�7@qhs@p��@p�u@pQ�@p  @o�w@o\)@ol�@n��@n��@n��@nE�@m@m`B@l��@l�j@l�D@lI�@k��@kC�@ko@j�!@j��@j�\@j~�@j-@i�^@i�7@iG�@h�`@hr�@hA�@h  @g��@g|�@g�P@gK�@f��@f{@ep�@d�j@d(�@d1@c�m@cS�@b��@bn�@bM�@b�@a��@aG�@a7L@a&�@`�`@`�u@`Q�@`1'@_��@_
=@^ff@^@]�-@]�h@]`B@]?}@]/@\��@\�D@\9X@[�
@[�F@[�F@[S�@Z��@Z^5@ZJ@Y��@Y�7@YX@Y7L@X�u@W�@WK�@V�R@VE�@V{@V@U�T@U�-@U��@U�@UO�@UV@T�j@T9X@T(�@T(�@T�@S��@S��@SdZ@SS�@R��@R�@Q��@Qx�@P��@P�u@PbN@P  @O�w@O�w@O�@O��@O|�@O\)@OK�@N�@NE�@N$�@N{@N@M�T@M��@MV@L�@L��@L�j@L�D@Lj@LZ@L(�@K��@K�m@K�
@Kƨ@K��@K"�@J��@J��@J�\@Jn�@I��@I��@Ihs@I7L@H��@HQ�@G��@G;d@Fȴ@Fv�@F5?@E��@EO�@E?}@E/@E�@D�@D�@D9X@C��@C�@C33@B�!@BM�@B�@A��@AX@AG�@A�@@��@@Ĝ@@�9@@��@@bN@@Q�@@b@@  @?�@?�@>ȴ@>�+@>ff@>$�@=��@=��@=V@<�/@<��@<Z@<(�@;�m@;��@;dZ@;C�@;o@:�H@:n�@:J@9��@9x�@9&�@8�`@8��@8�@8b@7��@7�@6�@6ȴ@6��@6�+@6v�@6V@5�-@5O�@4��@4�j@4�D@4j@4Z@49X@3�
@333@3@2^5@2M�@2=q@2J@1X@1%@0��@0�9@0�u@0Q�@01'@0b@0  @/�@/�@/�@/�;@/�w@/�P@/l�@/\)@/K�@/
=@.ȴ@.ff@.ff@.@-��@-`B@-/@,�@,�j@,�D@,Z@,Z@,I�@,1@+��@+��@+t�@+S�@+C�@+"�@+@*��@*��@*^5@*-@*J@)��@)��@)hs@)G�@)%@(�`@(��@(�9@(�u@(r�@(Q�@( �@(  @'�@'�P@'|�@'l�@'K�@&��@&V@&5?@&5?@&@%��@%/@$�j@$z�@$(�@#�m@#��@#C�@"�@"�!@"�\@"M�@!�^@!x�@!7L@!%@ �`@ �9@ r�@ bN@ Q�@ A�@ b@�;@�w@�P@|�@+@�y@�@�R@��@ff@V@@p�@��@z�@(�@�m@�
@�F@t�@"�@o@@��@�@�#@�^@�7@hs@&�@%@��@�`@Ĝ@��@r�@bN@1'@�@�w@l�@;d@;d@+@�y@��@v�@{@@�h@�@?}@��@��@��@�@��@�@�D@9X@1@��@�m@��@"�@�H@�!@�\@n�@^5@M�@-@-@�@��@��@X@%@�`@Ĝ@�u@�@bN@1'@  @�@�;@��@��@|�@|�@\)@+@
=@��@�y@�@�R@�+@v�@v�@V@V@E�@E�@{@@�T@�T@@�-@�-@��@�@`B@�j@�D@�D@z�@�@�@o@
�\@
~�@
^5@
-@
J@	�@	�7@	G�@	%@��@�`@Ĝ@�@ �@b@b@�;@�P@|�@l�@K�@;d@+@�@�@�@�@�@�@
=@��@�y@�y@�y@�R@�+@5?@��@@�-@�@?}@V@�@�/@�@��@��@��@z�@z�@z�@z�@Z@(�@��@��@ƨ@��@S�@@��@�\@=q@-@J@J@J@��@��@��@�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��`A��yA��mA��`A��TA��`A��`A��;A��/A��#A��/A��TA��HA��HA���A�A�p�A��A�K�A�S�A�jA͋DA���AɬA���A�VA���A���A�A�A�+A�E�A��A���A�1A�7LA��-A�VA��A���A�-A��A�S�A��A�\)A�9XA��^A�^5A�?}A�|�A��FA��A�oA�(�A�n�A�Q�A���A��/A�/A�ZA�%A�/A��
A�-A��A��/A�I�A��/A���A�I�A�1'A�XA��uA�(�A���A��A�-A��+A���A�=qA��HA���A�/A���A��A�K�A�ZA�I�A�7LA�VA���A��#A�ƨA�A���A�Q�A���A�ffA�jA��7A�  A�G�A~�A}x�A|{A{;dAz��AxA�Au�7As�Aq��Am�;Al�\Ak�FAj�uAh�`Ag;dAf�Ad�`Ac�#A`��A_XA^��A]?}A[��AZ��AZ1'AX�AW`BAVJAU\)AU7LAT�yAS;dAR �AQ/AP(�AN��AM%AK33AI�FAH�HAG�-AF^5AE�ADr�AC��AB�\AA�A@�A?VA=��A<Q�A;�mA;�A9��A8�A8ffA8$�A7�A5�A3��A3G�A2ȴA2�A1��A0z�A/?}A/%A-\)A,�\A,r�A,$�A+�^A+"�A)�
A(z�A'�A&E�A%��A$ZA#�A"��A!�A!�A!XA �jA �DA ZA 5?A A�wA/A�`A��A��A~�Al�A�AoAA�A��A�
AG�A��A�AA�A��A�A�FA��A��A�hAK�A��A��At�AbNA+A��A��A/AjA�
Ax�A
�RA
=qA
$�A	�
AjA�
A?}Av�A��A��A  A�AƨA ��A �DA ff@��m@�C�@���@�x�@�Q�@��@�(�@�@�{@�&�@�bN@���@�t�@��@�ȴ@�J@�9X@��T@�33@�@�j@�F@�dZ@�+@旍@��#@��
@�(�@�%@�I�@�M�@� �@ָR@��@�%@�Z@��m@ӍP@�@ѡ�@Ь@�z�@�9X@�b@��;@ϥ�@�S�@�@�^5@�Z@���@ư!@ư!@Ƈ+@�@�1@�K�@�ff@��`@��m@�K�@�p�@��
@��-@�V@���@���@�9X@���@��F@��P@�5?@��-@��j@��@��!@��@�@��7@�p�@��@��F@�V@���@�ƨ@�o@��R@�M�@���@��@�\)@�v�@�{@��@�G�@�1'@��+@���@�9X@��
@��H@��+@�5?@��@�  @��;@���@��@�33@��@���@�V@���@�V@��@��@��@�33@�@��@���@���@��+@�~�@�n�@�M�@�$�@�J@��@���@�x�@�G�@�?}@�/@�V@�Ĝ@�Z@��;@�ȴ@���@�&�@���@��@�bN@��;@��
@���@���@�ff@�E�@��@�hs@�&�@���@��9@�Z@�b@�|�@�v�@��@�@��-@�x�@�7L@��u@�(�@�b@��
@���@���@��P@�t�@�C�@��@��!@�n�@�@��h@�`B@�?}@�%@�z�@���@��;@��F@�C�@�33@�"�@��@�o@�o@�
=@�@���@�n�@��T@�hs@��@��`@��/@��9@��@�Q�@�I�@�9X@�1'@�1@~�@~@}@}p�@}�@|�/@|I�@{33@yhs@x��@xb@w+@vV@u@u�@u/@t�@t�D@t(�@s��@s�
@sƨ@s�@s33@r��@r^5@rJ@rJ@q�^@q�7@qhs@p��@p�u@pQ�@p  @o�w@o\)@ol�@n��@n��@n��@nE�@m@m`B@l��@l�j@l�D@lI�@k��@kC�@ko@j�!@j��@j�\@j~�@j-@i�^@i�7@iG�@h�`@hr�@hA�@h  @g��@g|�@g�P@gK�@f��@f{@ep�@d�j@d(�@d1@c�m@cS�@b��@bn�@bM�@b�@a��@aG�@a7L@a&�@`�`@`�u@`Q�@`1'@_��@_
=@^ff@^@]�-@]�h@]`B@]?}@]/@\��@\�D@\9X@[�
@[�F@[�F@[S�@Z��@Z^5@ZJ@Y��@Y�7@YX@Y7L@X�u@W�@WK�@V�R@VE�@V{@V@U�T@U�-@U��@U�@UO�@UV@T�j@T9X@T(�@T(�@T�@S��@S��@SdZ@SS�@R��@R�@Q��@Qx�@P��@P�u@PbN@P  @O�w@O�w@O�@O��@O|�@O\)@OK�@N�@NE�@N$�@N{@N@M�T@M��@MV@L�@L��@L�j@L�D@Lj@LZ@L(�@K��@K�m@K�
@Kƨ@K��@K"�@J��@J��@J�\@Jn�@I��@I��@Ihs@I7L@H��@HQ�@G��@G;d@Fȴ@Fv�@F5?@E��@EO�@E?}@E/@E�@D�@D�@D9X@C��@C�@C33@B�!@BM�@B�@A��@AX@AG�@A�@@��@@Ĝ@@�9@@��@@bN@@Q�@@b@@  @?�@?�@>ȴ@>�+@>ff@>$�@=��@=��@=V@<�/@<��@<Z@<(�@;�m@;��@;dZ@;C�@;o@:�H@:n�@:J@9��@9x�@9&�@8�`@8��@8�@8b@7��@7�@6�@6ȴ@6��@6�+@6v�@6V@5�-@5O�@4��@4�j@4�D@4j@4Z@49X@3�
@333@3@2^5@2M�@2=q@2J@1X@1%@0��@0�9@0�u@0Q�@01'@0b@0  @/�@/�@/�@/�;@/�w@/�P@/l�@/\)@/K�@/
=@.ȴ@.ff@.ff@.@-��@-`B@-/@,�@,�j@,�D@,Z@,Z@,I�@,1@+��@+��@+t�@+S�@+C�@+"�@+@*��@*��@*^5@*-@*J@)��@)��@)hs@)G�@)%@(�`@(��@(�9@(�u@(r�@(Q�@( �@(  @'�@'�P@'|�@'l�@'K�@&��@&V@&5?@&5?@&@%��@%/@$�j@$z�@$(�@#�m@#��@#C�@"�@"�!@"�\@"M�@!�^@!x�@!7L@!%@ �`@ �9@ r�@ bN@ Q�@ A�@ b@�;@�w@�P@|�@+@�y@�@�R@��@ff@V@@p�@��@z�@(�@�m@�
@�F@t�@"�@o@@��@�@�#@�^@�7@hs@&�@%@��@�`@Ĝ@��@r�@bN@1'@�@�w@l�@;d@;d@+@�y@��@v�@{@@�h@�@?}@��@��@��@�@��@�@�D@9X@1@��@�m@��@"�@�H@�!@�\@n�@^5@M�@-@-@�@��@��@X@%@�`@Ĝ@�u@�@bN@1'@  @�@�;@��@��@|�@|�@\)@+@
=@��@�y@�@�R@�+@v�@v�@V@V@E�@E�@{@@�T@�T@@�-@�-@��@�@`B@�j@�D@�D@z�@�@�@o@
�\@
~�@
^5@
-@
J@	�@	�7@	G�@	%@��@�`@Ĝ@�@ �@b@b@�;@�P@|�@l�@K�@;d@+@�@�@�@�@�@�@
=@��@�y@�y@�y@�R@�+@5?@��@@�-@�@?}@V@�@�/@�@��@��@��@z�@z�@z�@z�@Z@(�@��@��@ƨ@��@S�@@��@�\@=q@-@J@J@J@��@��@��@�7111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�yB�sB�sB�sB�yB�yB�yB�yB�B�B�B�yB�yB�mB�fB�;B��B�B~�B�\B��B��Bv�B�B�BN�Be`B]/BW
B��B��Bt�Be`B_;B?}BVB\)B\)B_;B^5Bl�B`BB[#BH�BB�B;dB&�B��B�B1B��B�
B�/B�B�TBƨB��B�dBÖB�B�'B��B��B��B�bB�uB�PB��B�DBw�B� B�+B� Bn�BgmBYBP�BF�B@�B&�BDB
��B
��B
�B
�
B
ÖB
�RB
�9B
B
ƨB
ŢB
ÖB
�dB
�B
��B
�\B
y�B
n�B
n�B
^5B
Q�B
F�B
?}B
<jB
0!B
bB	�B	�B	�sB	B	ɺB	��B	B	�-B	�B	��B	��B	�{B	u�B	w�B	�B	n�B	e`B	l�B	e`B	R�B	L�B	E�B	O�B	R�B	H�B	-B	,B	'�B	�B	\B	  B��B��B	B��B�B��B�B�B�B�B�sB�/B�#B��B�BB�#BĜB��B�
B��B��B�B�B�jB�jB�?B�?B��B��B�B��B��B�B��B��B�\B~�Bu�B{�Bt�B{�Bo�Bn�B}�B|�B~�B�By�B�B�B� B|�Bv�Bq�Bp�Be`B]/Bl�BbNBm�Bs�Bq�Bm�BffB]/B^5BbNB^5BW
BH�BN�BZBO�BO�B]/B\)BXBR�BC�BA�B?}BL�BA�BD�BB�BE�BG�BA�BD�BH�B@�B-B8RB6FB49B0!B1'B-B'�B �B'�B-B1'B-B)�B,B!�B�B�BVB�B!�B#�B$�B(�B&�B%�B!�B�B	7B%BBoB�B�B!�B�B�BoBB�B��B{B
=B%B�B�B�B�B"�B!�B�B�B�B(�B'�B'�B&�B$�B!�B�BuBB��B\B-B'�B�B�B#�B�B�B!�B%�B�B �B�B6FB;dB<jB9XB;dB:^B8RB.B5?B49B2-B;dB@�BD�BE�BE�B?}B8RB8RB<jBI�BM�BR�BP�BM�BF�BT�BW
B^5BaHB\)BT�BXB\)Bm�Bp�Bo�Bu�Bv�Bs�Bq�B�1B�=B�7B�+B�DB�1B�1B�+B�JB�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�RB�qB�wB��B��BƨBǮBĜBŢBɺBȴBǮB��B��B�B�B�B��B�B�`B�B�B�B�B�B��B��B��B��B	B	B	B	B	B	B	%B	%B	1B	VB	\B	\B	VB	uB	�B	�B	�B	'�B	(�B	)�B	,B	,B	,B	+B	)�B	)�B	-B	2-B	9XB	>wB	B�B	A�B	C�B	E�B	I�B	I�B	I�B	H�B	G�B	P�B	XB	YB	YB	YB	W
B	S�B	R�B	^5B	`BB	cTB	gmB	k�B	p�B	q�B	s�B	t�B	u�B	y�B	y�B	z�B	z�B	z�B	|�B	~�B	�B	�B	�%B	�7B	�=B	�=B	�\B	�bB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�-B	�'B	�-B	�9B	�9B	�9B	�FB	�LB	�XB	�^B	�qB	�wB	�qB	�jB	�}B	��B	ĜB	ƨB	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�)B	�5B	�5B	�5B	�;B	�5B	�;B	�;B	�BB	�TB	�TB	�HB	�HB	�TB	�`B	�fB	�fB	�mB	�mB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
%B
+B
%B
1B
+B
%B
1B
	7B
DB
JB
PB
PB
\B
hB
hB
hB
bB
bB
\B
\B
uB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
#�B
"�B
#�B
$�B
$�B
#�B
#�B
$�B
%�B
'�B
)�B
(�B
(�B
(�B
'�B
&�B
(�B
)�B
+B
+B
,B
-B
+B
)�B
)�B
-B
,B
/B
/B
.B
,B
/B
1'B
0!B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
2-B
2-B
2-B
33B
33B
33B
2-B
2-B
2-B
49B
2-B
2-B
49B
5?B
5?B
5?B
6FB
7LB
8RB
7LB
6FB
6FB
8RB
8RB
8RB
9XB
9XB
9XB
8RB
8RB
9XB
9XB
:^B
:^B
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
=qB
>wB
>wB
=qB
<jB
>wB
?}B
@�B
?}B
>wB
?}B
?}B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
C�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
K�B
J�B
J�B
J�B
J�B
I�B
H�B
J�B
J�B
L�B
M�B
N�B
N�B
M�B
N�B
O�B
O�B
N�B
N�B
P�B
Q�B
Q�B
Q�B
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
S�B
S�B
T�B
VB
VB
T�B
T�B
VB
T�B
T�B
XB
XB
XB
XB
ZB
YB
YB
YB
YB
YB
XB
YB
ZB
ZB
YB
XB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
[#B
[#B
]/B
\)B
\)B
]/B
^5B
^5B
_;B
^5B
^5B
_;B
`BB
`BB
`BB
_;B
`BB
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
bNB
bNB
bNB
bNB
bNB
aHB
bNB
bNB
cTB
bNB
cTB
cTB
bNB
bNB
aHB
`BB
bNB
cTB
bNB
aHB
aHB
bNB
cTB
ffB
ffB
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
hsB
iyB
jB
iyB
iyB
jB
k�B
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
k�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
n�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
s�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�_B�sB�sB�sB�yB�yB�yB�yB�B�B�B�yB�B�B�B�B�(B#TB�4B�hB��B�nB}qB��B�+BW?Bl�Bc�B^�B�B��By�Bi_Bc�BD�BYeB^�B^B`�B_�Bl�Ba|B\�BJ�BC�B<�B)�B�cB$B
=B�<B��B޸B��B�B�B�dB�VB�gB�B�-B�fB�1B��B��B�aB�VB��B��By�B��B�1B�UBp�BiBZ�BR�BG�BA�B)*B�B
�0B
��B
�5B
�B
�?B
��B
�B
�GB
��B
��B
ðB
��B
��B
��B
� B
|6B
p�B
o�B
`'B
S�B
HfB
A B
=�B
1vB
[B	��B	�;B	��B	��B	�DB	�B	�3B	�nB	�B	�mB	�ZB	�9B	y$B	y�B	��B	p�B	gRB	mwB	f�B	T�B	N�B	GEB	P�B	S@B	IlB	/OB	-wB	)DB	/B	NB	uB	;B��B	3B�B�MB�B�B��B�B��B�B�!B��BּB��B�B�B��BרBӏBΊB��B�CB�<B�"B�FB�+B�B�\B��B��B��B�=B�`B�kB�}B��Bw�B}<BvzB}Bq[Bp;B~�B}�B�B�uBz�B�[B�UB�OB}VBwfBr|BqvBf�B^�BmBc�Bm�Bs�Bq�Bm�BgB^OB_Bb�B^�BXBJ�BO�BZ�BQNBP�B]~B\�BX�BS�BESBCBABM�BB�BE�BC�BFtBH�BB�BESBIBAoB/ B9$B7LB5tB1[B2-B.}B)_B"NB(�B-�B1vB-�B*�B,�B"�B�B�B.BOB"�B$�B%zB)DB'RB&LB"NBQB
�B�B�BuB7B;B"4B 'BB&B�B�3B��B�B�B�BSB/BxB;B#:B"4BVB�BjB)*B(>B($B'B%,B"4BB,B�B�>B}B,�B(>B �B�B$tB �B�B"�B&�B1B!�B!B6�B;�B<�B9�B;�B:�B8�B/B5�B5B33B<B@�BD�BE�BE�B@B9XB9rB=�BJXBNVBS[BQhBN�BG�BU�BW�B^�Ba|B\�BVBYKB]IBm�BqBpUBvBw2BtnBr�B�1B�XB�lB��B�xB��B��B��B��B��B�B��B�B��B�B��B�B��B��B��B�B�B�B�B�2B�,B�$B�B�DB�$B�,B�FB�tB��B��B��B��B��B��B��B��B��B�B�%B��B�B�B�B�B�B�SB�yB՛B��B�B��B�B��B��B�!B�B�"B�"B�.B	'B	'B	;B	;B	UB	SB	tB	YB	�B	pB	�B	�B	�B	�B	�B	�B	B	'�B	)B	*B	,"B	,"B	,"B	+B	*0B	*eB	-wB	2�B	9�B	>�B	B�B	A�B	C�B	E�B	I�B	I�B	I�B	H�B	H1B	Q4B	X+B	YKB	YKB	YKB	WsB	T{B	S�B	^jB	`�B	c�B	g�B	k�B	p�B	q�B	s�B	t�B	vB	y�B	y�B	z�B	z�B	{B	}"B	.B	� B	�9B	�YB	�RB	�rB	��B	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�
B	�$B	�>B	�"B	�)B	�IB	�'B	�GB	�GB	�vB	�aB	�TB	�TB	�nB	�zB	��B	��B	��B	��B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	��B	��B	�B	�B	� B	��B	�B	�B	�B	�B	�B	�2B	�[B	�EB	�QB	�CB	�OB	�OB	�OB	�VB	�jB	�VB	�pB	�\B	�nB	�nB	�B	�|B	�B	�zB	�B	�B	�B	�B	��B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�"B	��B	��B	�B	�B	�B	�B	�"B	�"B
 B
B
;B
 B
 B
 4B
-B
3B
3B
3B
MB
B
3B
3B
B
9B
9B
3B
GB
9B
EB
EB
EB
YB
_B
YB
KB
zB
�B
fB
	RB
�B
dB
jB
�B
vB
hB
�B
�B
}B
}B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
#�B
#B
#�B
$�B
$�B
$B
$B
%B
&B
($B
)�B
)B
)*B
)B
(
B
'8B
)B
*B
+B
+B
,"B
,�B
+6B
*0B
*0B
-CB
,=B
/5B
/OB
./B
,qB
/5B
1AB
0;B
1AB
1AB
2GB
2GB
33B
33B
33B
33B
2GB
2GB
2GB
3MB
3MB
3hB
2GB
2GB
2aB
49B
2aB
2|B
4TB
5tB
5tB
5ZB
6`B
7�B
8RB
7fB
6`B
6zB
8RB
8�B
8lB
9XB
9rB
9rB
8lB
8lB
9rB
9rB
:xB
:xB
9�B
:�B
;B
;B
;B
<jB
<�B
<jB
<�B
<�B
<�B
<�B
;�B
=�B
>�B
>�B
=�B
<�B
>�B
?�B
@�B
?�B
>�B
?�B
?�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
C�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
K�B
J�B
J�B
J�B
J�B
I�B
IB
J�B
J�B
L�B
M�B
N�B
OB
M�B
OB
O�B
O�B
OB
OB
Q B
RB
RB
RB
SB
S&B
TB
S�B
S�B
TB
TB
TB
T,B
T,B
TB
TB
UB
VB
VB
T�B
UB
VB
U2B
U2B
X+B
X+B
X+B
X+B
ZB
YB
YB
Y1B
Y1B
Y1B
X+B
Y1B
ZB
Z7B
Y1B
XEB
Z7B
[WB
[=B
\]B
\)B
\)B
\CB
\)B
[=B
[=B
]/B
\CB
\]B
]IB
^OB
^OB
_;B
^OB
^OB
_VB
`\B
`BB
`BB
_VB
`BB
`\B
`\B
`BB
aHB
aHB
aHB
aHB
abB
abB
bhB
bNB
bhB
bNB
bNB
bNB
abB
bhB
bhB
cTB
bhB
cTB
cTB
bhB
bNB
abB
`�B
bhB
cTB
bhB
a|B
a|B
b�B
c�B
ffB
f�B
ffB
g�B
f�B
e�B
g�B
g�B
hsB
h�B
h�B
g�B
h�B
i_B
jB
i�B
i�B
jB
k�B
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
k�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
n�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
p�B
p�B
q�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
s�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808310040002018083100400020180831004000201808310200172018083102001720180831020017201809010028012018090100280120180901002801  JA  ARFMdecpA19c                                                                20180827093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180827003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180827003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180827003518  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180827003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180827003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180827003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180827003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180827003519  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180827003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20180827005607                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180827153739  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180830154000  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180830154000  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180830170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180831152801  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                