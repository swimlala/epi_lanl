CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-19T00:35:24Z creation;2016-08-19T00:35:26Z conversion to V3.1;2019-12-19T08:32:41Z update;     
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
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20160819003524  20200115101517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_028                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��6T�> 1   @��7�s�@;��=�K�dgQ���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ Dм�D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@2�\@x��@��@��A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B0\)B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D\D�\D\D�D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;��D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�<{D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dм{D���D�?�D��Dѿ�D���D�?�D��Dҿ�D���D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؿ�D���D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��TA��HA���A���A͸RAͲ-AͬAͥ�A͡�A͍PA�jA�I�A��A���A�jA�K�A�5?A��A�
=A���A��;AˍPA�A��A�
=A��A�^5A�1A��RA���A�?}A��hA�VA��A�oA���A��PA��9A��
A�x�A��A�oA���A�A�33A�ZA���A�{A�"�A��`A��FA�z�A�ZA�-A� �A���A���A��;A��RA���A�1A��
A�(�A���A�1'A��
A��!A��A��FA�\)A���A��A�jA�-A�7LA���A�=qA�bNA�$�A��HA�n�A��!A�=qA���A��7A�^5A�5?A���A�|�A�?}A��#A��!A�33A~ȴA~(�A}�;A}��A|��AzI�Aw�wAu�^AuVAtz�AsAs;dAsVAq��Ao��Ao��Ao
=An�An{Al��Al  Ak��AkO�Aj�Aj  Ai;dAg\)Ae%Ab�Ab=qAa��A`�9A_��A_�A]��A\�/A\�A[x�AY�AXȴAXn�AX{AWx�AV�AVM�AU��AT��AT�AS�#ASK�AQ�AP�\APv�AP$�AO�wAO�AN=qALȴAL1'ALJAK��AJbAGS�AE%AC�7AB�AB�AAA@I�A?��A<��A<E�A<JA;�A;�
A;��A9�
A8��A8�jA8��A8r�A7��A7��A7�FA7�hA7?}A6��A6r�A5�A5��A5S�A5%A4�`A4�uA41A3l�A2ȴA1��A1K�A1�A0�jA0=qA/"�A.M�A-33A+�A*�A)�A)|�A(��A(ffA'"�A%�#A$��A#dZA �A A�A�7A�HAn�AI�A�TA��A&�A%A��A�AVAp�AC�A/A�!AQ�A�-A&�A�^A�A�uA�AffAS�A�A��A�!A�-A
�A
�jA
~�A	�#A�/A�A
=A1'A�A�`AE�Ap�A�A �A Z@��w@�C�@���@���@���@�1@���@��`@��u@�dZ@�G�@�@�R@�bN@� �@�R@�7@�/@��@�b@◍@���@�"�@��`@���@�%@؋D@�|�@���@��@�z�@�1'@ӕ�@�n�@�9X@��@·+@�J@�x�@̼j@ʇ+@�j@���@�l�@�;d@ģ�@þw@Õ�@�+@��@��#@��9@���@���@��@�?}@��9@�t�@�?}@��
@�l�@��H@��\@�M�@���@�x�@�/@�V@��D@�I�@���@���@��@��`@�I�@�  @�dZ@��T@�7L@���@�I�@��@��F@��@�v�@�@���@�p�@���@��@�Z@�1'@�dZ@�E�@�@��^@�%@�(�@��
@���@��w@��@���@�C�@��y@���@��@��h@�`B@���@���@��D@��@�z�@���@��h@�V@��`@�Z@�b@��;@��P@��@���@��+@�5?@��-@�X@�&�@��@��j@���@�ƨ@�l�@�\)@�C�@���@�-@���@�G�@�  @�|�@�
=@�E�@�O�@��@���@���@�/@��@�Ĝ@��@�&�@��D@��@�ƨ@�C�@�"�@�+@��!@���@��@��-@�/@�%@��@�bN@�1@��w@�+@�M�@�$�@��@�J@���@�x�@�9X@�|�@��\@�ff@�$�@���@�p�@�&�@��7@���@�p�@�hs@��@���@� �@��m@��F@�+@�o@���@��R@��@�=q@��h@�?}@��@��@��@�&�@�hs@�@���@��y@���@�@�@��@��+@�^5@�{@��@�`B@��@�p�@��`@�r�@��@��@�@|�@|�@+@~�y@~��@~��@~ȴ@+@\)@+@
=@�@
=@~��@}�@{��@{o@z-@y��@y�@x��@y%@x�u@xQ�@wl�@vȴ@v�R@v�+@v@u@u��@t��@t�/@t�j@t��@tZ@tZ@tj@tI�@t(�@t1@s��@s��@s�m@s��@st�@sS�@s"�@r�H@rn�@q��@qx�@q�@q%@pĜ@p �@o�@n�@m�T@m`B@m/@l�/@l�D@l9X@l�@l1@k�
@k�F@k��@kS�@kC�@k@k@k@ko@ko@k@j=q@i�#@i�7@iX@i�@hQ�@g\)@g�@g
=@f��@f�@fE�@d�D@d9X@d�@d1@c�
@cC�@b��@bJ@a�#@a��@a�7@a�7@a�7@aG�@`r�@_�@_�P@_|�@_\)@^��@^��@^V@]�h@]?}@]/@\�/@\�D@\9X@[�F@[t�@[dZ@[33@[o@[@Z�@Z�!@Y�#@Y�@XĜ@Xr�@W�;@W�P@WK�@W�@Vȴ@V�+@V{@Up�@U?}@T�/@Tz�@S��@S��@SS�@R��@RJ@Q�7@QX@Q�@PĜ@P��@PbN@P1'@Pb@O��@OK�@N��@Nff@M��@M�@M�@L��@L�@L�/@L��@L�D@Lj@LI�@L(�@K��@K�
@K�F@K��@KS�@K33@K@J�H@J��@J�!@J^5@J-@I�@I��@I&�@I%@HĜ@HbN@H  @G|�@G\)@GK�@G+@F��@F�@F�R@Fff@FE�@E�-@D��@Dz�@DZ@DI�@D�@D�@C�
@C�F@C��@C�@CS�@B�@Bn�@B=q@B-@B�@A��@AX@@��@@1'@?�@?��@?�@?�@?�@?�@?|�@?
=@>ȴ@>ff@>{@=@=��@=�@=p�@=�@<�D@<j@<I�@<�@;�@:�@:�H@:�H@:��@:�\@9�@9��@9hs@8��@8�u@8  @7|�@7;d@7+@7�@6ȴ@6�+@6v�@6E�@5�T@5`B@5�@4��@4�@4j@4�@3S�@2��@2M�@2�@1�#@1�7@1G�@1&�@1�@1�@1�@1�@1&�@1&�@1&�@0��@0��@0Ĝ@0�u@0Q�@/�;@/�P@/�P@/l�@/+@.�y@.ȴ@.�R@.�+@.ff@-@-�h@-O�@-/@,�/@,�D@,z�@,�@+��@+t�@+"�@*�!@*J@)�@)�#@)X@(�9@(r�@(Q�@(  @'�w@'K�@&��@&�+@&$�@%�T@%�@%O�@%V@$��@$j@$j@$9X@#�m@#��@#dZ@#C�@"�H@"�!@"�\@"M�@!�^@!7L@!%@ A�@�w@�y@��@E�@��@��@�@p�@/@�/@Z@�@��@��@��@��@dZ@��@^5@��@��@�7@X@&�@��@�u@ �@�;@��@��@|�@
=@�@�@�R@�+@E�@$�@@�h@�@`B@?}@/@��@��@��@j@(�@�m@��@dZ@C�@33@33@"�@�@��@�\@^5@M�@=q@�^@�7@x�@x�@x�@X@&�@��@�9@�@  @�w@�@|�@+@�@ȴ@�+@5?@�@�@�T@��@�h@/@�@�/@�@�@��@�D@�D@z�@j@I�@�@�m@ƨ@��@S�@o@@@
�@
�@
�!@
=q@	��@	��@	hs@	G�@	G�@	7L@	%@Ĝ@�u@r�@Q�@ �@  @�@�w@�P@;d@��@�+@V@V@5?@$�@$�@{@�-@O�@V@��@�@��@�j@�@z�@�m@�F@�@dZ@C�@o@�@�@�@�H@��@M�@��@�#@�^@�^@��@��@�7@�7@�7@�7@�7@x�@x�@hs@X@7L@&�@�@%@ �`@ �9@ �@ 1'?��;?�\)?��?��R?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��TA��HA���A���A͸RAͲ-AͬAͥ�A͡�A͍PA�jA�I�A��A���A�jA�K�A�5?A��A�
=A���A��;AˍPA�A��A�
=A��A�^5A�1A��RA���A�?}A��hA�VA��A�oA���A��PA��9A��
A�x�A��A�oA���A�A�33A�ZA���A�{A�"�A��`A��FA�z�A�ZA�-A� �A���A���A��;A��RA���A�1A��
A�(�A���A�1'A��
A��!A��A��FA�\)A���A��A�jA�-A�7LA���A�=qA�bNA�$�A��HA�n�A��!A�=qA���A��7A�^5A�5?A���A�|�A�?}A��#A��!A�33A~ȴA~(�A}�;A}��A|��AzI�Aw�wAu�^AuVAtz�AsAs;dAsVAq��Ao��Ao��Ao
=An�An{Al��Al  Ak��AkO�Aj�Aj  Ai;dAg\)Ae%Ab�Ab=qAa��A`�9A_��A_�A]��A\�/A\�A[x�AY�AXȴAXn�AX{AWx�AV�AVM�AU��AT��AT�AS�#ASK�AQ�AP�\APv�AP$�AO�wAO�AN=qALȴAL1'ALJAK��AJbAGS�AE%AC�7AB�AB�AAA@I�A?��A<��A<E�A<JA;�A;�
A;��A9�
A8��A8�jA8��A8r�A7��A7��A7�FA7�hA7?}A6��A6r�A5�A5��A5S�A5%A4�`A4�uA41A3l�A2ȴA1��A1K�A1�A0�jA0=qA/"�A.M�A-33A+�A*�A)�A)|�A(��A(ffA'"�A%�#A$��A#dZA �A A�A�7A�HAn�AI�A�TA��A&�A%A��A�AVAp�AC�A/A�!AQ�A�-A&�A�^A�A�uA�AffAS�A�A��A�!A�-A
�A
�jA
~�A	�#A�/A�A
=A1'A�A�`AE�Ap�A�A �A Z@��w@�C�@���@���@���@�1@���@��`@��u@�dZ@�G�@�@�R@�bN@� �@�R@�7@�/@��@�b@◍@���@�"�@��`@���@�%@؋D@�|�@���@��@�z�@�1'@ӕ�@�n�@�9X@��@·+@�J@�x�@̼j@ʇ+@�j@���@�l�@�;d@ģ�@þw@Õ�@�+@��@��#@��9@���@���@��@�?}@��9@�t�@�?}@��
@�l�@��H@��\@�M�@���@�x�@�/@�V@��D@�I�@���@���@��@��`@�I�@�  @�dZ@��T@�7L@���@�I�@��@��F@��@�v�@�@���@�p�@���@��@�Z@�1'@�dZ@�E�@�@��^@�%@�(�@��
@���@��w@��@���@�C�@��y@���@��@��h@�`B@���@���@��D@��@�z�@���@��h@�V@��`@�Z@�b@��;@��P@��@���@��+@�5?@��-@�X@�&�@��@��j@���@�ƨ@�l�@�\)@�C�@���@�-@���@�G�@�  @�|�@�
=@�E�@�O�@��@���@���@�/@��@�Ĝ@��@�&�@��D@��@�ƨ@�C�@�"�@�+@��!@���@��@��-@�/@�%@��@�bN@�1@��w@�+@�M�@�$�@��@�J@���@�x�@�9X@�|�@��\@�ff@�$�@���@�p�@�&�@��7@���@�p�@�hs@��@���@� �@��m@��F@�+@�o@���@��R@��@�=q@��h@�?}@��@��@��@�&�@�hs@�@���@��y@���@�@�@��@��+@�^5@�{@��@�`B@��@�p�@��`@�r�@��@��@�@|�@|�@+@~�y@~��@~��@~ȴ@+@\)@+@
=@�@
=@~��@}�@{��@{o@z-@y��@y�@x��@y%@x�u@xQ�@wl�@vȴ@v�R@v�+@v@u@u��@t��@t�/@t�j@t��@tZ@tZ@tj@tI�@t(�@t1@s��@s��@s�m@s��@st�@sS�@s"�@r�H@rn�@q��@qx�@q�@q%@pĜ@p �@o�@n�@m�T@m`B@m/@l�/@l�D@l9X@l�@l1@k�
@k�F@k��@kS�@kC�@k@k@k@ko@ko@k@j=q@i�#@i�7@iX@i�@hQ�@g\)@g�@g
=@f��@f�@fE�@d�D@d9X@d�@d1@c�
@cC�@b��@bJ@a�#@a��@a�7@a�7@a�7@aG�@`r�@_�@_�P@_|�@_\)@^��@^��@^V@]�h@]?}@]/@\�/@\�D@\9X@[�F@[t�@[dZ@[33@[o@[@Z�@Z�!@Y�#@Y�@XĜ@Xr�@W�;@W�P@WK�@W�@Vȴ@V�+@V{@Up�@U?}@T�/@Tz�@S��@S��@SS�@R��@RJ@Q�7@QX@Q�@PĜ@P��@PbN@P1'@Pb@O��@OK�@N��@Nff@M��@M�@M�@L��@L�@L�/@L��@L�D@Lj@LI�@L(�@K��@K�
@K�F@K��@KS�@K33@K@J�H@J��@J�!@J^5@J-@I�@I��@I&�@I%@HĜ@HbN@H  @G|�@G\)@GK�@G+@F��@F�@F�R@Fff@FE�@E�-@D��@Dz�@DZ@DI�@D�@D�@C�
@C�F@C��@C�@CS�@B�@Bn�@B=q@B-@B�@A��@AX@@��@@1'@?�@?��@?�@?�@?�@?�@?|�@?
=@>ȴ@>ff@>{@=@=��@=�@=p�@=�@<�D@<j@<I�@<�@;�@:�@:�H@:�H@:��@:�\@9�@9��@9hs@8��@8�u@8  @7|�@7;d@7+@7�@6ȴ@6�+@6v�@6E�@5�T@5`B@5�@4��@4�@4j@4�@3S�@2��@2M�@2�@1�#@1�7@1G�@1&�@1�@1�@1�@1�@1&�@1&�@1&�@0��@0��@0Ĝ@0�u@0Q�@/�;@/�P@/�P@/l�@/+@.�y@.ȴ@.�R@.�+@.ff@-@-�h@-O�@-/@,�/@,�D@,z�@,�@+��@+t�@+"�@*�!@*J@)�@)�#@)X@(�9@(r�@(Q�@(  @'�w@'K�@&��@&�+@&$�@%�T@%�@%O�@%V@$��@$j@$j@$9X@#�m@#��@#dZ@#C�@"�H@"�!@"�\@"M�@!�^@!7L@!%@ A�@�w@�y@��@E�@��@��@�@p�@/@�/@Z@�@��@��@��@��@dZ@��@^5@��@��@�7@X@&�@��@�u@ �@�;@��@��@|�@
=@�@�@�R@�+@E�@$�@@�h@�@`B@?}@/@��@��@��@j@(�@�m@��@dZ@C�@33@33@"�@�@��@�\@^5@M�@=q@�^@�7@x�@x�@x�@X@&�@��@�9@�@  @�w@�@|�@+@�@ȴ@�+@5?@�@�@�T@��@�h@/@�@�/@�@�@��@�D@�D@z�@j@I�@�@�m@ƨ@��@S�@o@@@
�@
�@
�!@
=q@	��@	��@	hs@	G�@	G�@	7L@	%@Ĝ@�u@r�@Q�@ �@  @�@�w@�P@;d@��@�+@V@V@5?@$�@$�@{@�-@O�@V@��@�@��@�j@�@z�@�m@�F@�@dZ@C�@o@�@�@�@�H@��@M�@��@�#@�^@�^@��@��@�7@�7@�7@�7@�7@x�@x�@hs@X@7L@&�@�@%@ �`@ �9@ �@ 1'?��;?�\)?��?��R?���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B49B5?B5?B49B49B49B49B49B33B2-B1'B0!B0!B1'B7LBE�BS�B\)BbNBjBn�Bu�B�B�PB�7B�B{�Bt�BjBZBQ�B@�B/B!�B\BDB+B��B�B�fB��BŢB��B�wB�B��B��B��B�1Bq�BT�BA�B0!B �B\B��B�yB�BǮB�^B�B��B��B��B�\B�=B|�Br�Bl�BgmB\)BR�BG�B7LB&�B�B�BDB%BB
��B
�B
�B
�mB
�BB
�/B
�#B
��B
��B
��B
ĜB
��B
�^B
�B
��B
��B
��B
��B
|�B
m�B
]/B
W
B
R�B
M�B
H�B
F�B
?}B
0!B
,B
&�B
#�B
 �B
�B
�B
{B
hB
\B

=B
B	��B	�yB	�B	��B	��B	��B	�}B	�wB	�FB	�B	��B	��B	��B	��B	��B	�uB	�\B	�PB	�=B	�%B	�B	}�B	z�B	x�B	o�B	hsB	m�B	l�B	iyB	gmB	aHB	XB	P�B	N�B	I�B	?}B	&�B	�B	VB	DB		7B	+B	  B��B�B�B�B�B�B�B�fB�TB�TB�ZB�TB�BB�;B�;B�;B�BB�ZB�sB�sB�B�yB�mB�sB�mB�`B�BB�/B�
B��B�B�B��B��BɺBĜB�XB�-B�B�B�B��B��B��B��B��B�PB�1B�%B�B�B�B�B� By�Bw�Bw�Bw�Bv�Bt�Bu�Bv�Bw�Bv�Bv�Bs�Bn�BiyBdZBaHB_;BZBXBW
BS�BP�BO�BN�BM�BL�BJ�BF�BA�BA�B@�B>wB=qB;dB:^B:^B9XB8RB8RB7LB7LB6FB5?B49B1'B1'B.B0!B(�B&�B'�B$�B!�B!�B �B�B �B!�B�B"�B!�B"�B"�B"�B#�B#�B#�B#�B#�B$�B$�B'�B'�B'�B'�B'�B'�B+B,B-B-B-B1'B0!B0!B0!B1'B1'B33B33B5?B6FB6FB6FB9XB<jB=qB>wB?}B?}B@�BA�BB�BB�BB�BC�BD�BD�BD�BI�BK�BL�BM�BN�BR�BS�BS�BW
BXBXB[#B]/B^5B_;B_;BbNBcTBcTBcTBffBjBjBk�Bm�Bo�Bq�Bs�Bu�Bu�Bv�Bw�Bw�Bw�Bz�B}�B~�B�B�B�7B��B��B��B��B�B��B�B�B�'B�-B�-B�9B�FB�LB�RB�^B�^B�dB�dB�jB�wB�}B��BB��BÖBĜBǮBǮB��B��B��B��B��B��B�#B�/B�/B�5B�;B�TB�`B�fB�ZB�ZB�fB�B�B�B��B��B	  B	B	B	B	B	B	B	%B	%B	%B	%B	B	%B	+B		7B	JB	bB	uB	{B	�B	�B	"�B	&�B	(�B	+B	-B	.B	-B	-B	-B	-B	.B	2-B	6FB	8RB	8RB	5?B	8RB	;dB	;dB	<jB	<jB	@�B	G�B	N�B	S�B	VB	W
B	YB	ZB	\)B	\)B	]/B	^5B	`BB	aHB	bNB	aHB	^5B	]/B	`BB	aHB	cTB	ffB	gmB	iyB	k�B	m�B	p�B	t�B	v�B	y�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�1B	�1B	�1B	�1B	�7B	�JB	�\B	�hB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�3B	�9B	�9B	�?B	�FB	�?B	�?B	�FB	�FB	�RB	�RB	�XB	�XB	�^B	�^B	�dB	�wB	�}B	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�5B	�BB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B

=B

=B
DB
JB
PB
PB
VB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
)�B
+B
,B
,B
-B
-B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
/B
/B
1'B
2-B
1'B
1'B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
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
7LB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
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
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
J�B
J�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
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
T�B
T�B
T�B
T�B
T�B
T�B
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
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
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
\)B
\)B
\)B
]/B
^5B
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
`BB
`BB
aHB
aHB
aHB
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
cTB
cTB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
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
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
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
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B4TB5ZB5ZB4TB4TB4TB4TB4TB3hB2|B1�B0�B0�B1�B7�BE�BT,B\]Bb�BkBpUBzB�rB�HB��B�B� By�Bo B^BW�BEB7�B$�BB<B
�B��B�B�B��B��B��B��B�'B��B�hB�~B�6Bv+BXBDMB2�B#nBoB�B��BܬBɺB�B��B�yB��B��B��B��B~BBs�Bm�BiB]�BT�BJXB9XB(
B BB7BB�BSB
�}B
�B
�WB
�XB
��B
��B
�)B
�{B
ЗB
͟B
�mB
��B
�B
��B
�`B
��B
�\B
��B
�B
o�B
^B
W�B
S�B
N�B
IlB
HfB
A B
0�B
,�B
'�B
$�B
"4B
~B
$B
�B
 B
�B
�B
�B	��B	�B	��B	ԯB	�oB	�B	��B	� B	�LB	�!B	�B	��B	��B	�$B	�B	�FB	�.B	�<B	�DB	�EB	��B	~�B	|B	z�B	p�B	h�B	nB	mCB	jB	h�B	b�B	X�B	QhB	O�B	LB	B�B	)�B	QB	BB	B	
�B		B	�B�wB�|B��B��B��B�qB�wB�RB�B�B��B��B��B�pBߊB߾B��B��B�B��B�B��B��B�B�>B�LB�HB�jB׍B�gBּB��B�gB�.B�xB��B��B��B��B� B��B��B��B��B��B�+B�<B�7B��B��B��B��B��B��Bz*BxBx8Bx�Bw�Bu%BvFBw�Bx�Bw�BxBu�Bp�Bk6Be�Bc:B`�B[#BYBX�BUMBQ�BPbBO�BO(BN�BM�BG�BB�BB�BAoB?}B>wB<B:�B;B9�B8�B8�B7�B8RB7�B6�B5%B1�B2�B1vB1[B*B(�B*KB%�B"�B"4B!HB �B!�B#B!HB$ZB#:B#�B#nB#�B$�B$�B$ZB$@B$�B%�B&LB(�B(�B(sB(�B(�B)yB,WB,�B-wB-�B.�B1�B0oB0�B0�B1�B2-B4B4B5�B6�B6�B7fB:�B=VB=�B>�B?�B?�B@�BA�BB�BB�BCBC�BEBE9BE�BJ�BLJBM6BNpBO�BSuBTFBT{BWsBXEBX�B[�B]�B^�B_�B_�Bb�Bc�Bc�BdBgBj�Bj�Bl"Bn/Bo�Bq�Bs�Bu�Bu�BwBxBx8BxRB{dB~(BcB�AB�AB�lB��B��B��B�sB�QB�eB�]B�OB�vB�aB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�9BȀB�1B�0B�jB�pB�:B�B��B�#B�dB�IB�OB�;B�B��B�B��B�B�B��B�B�B�8B�HB	 4B	UB	UB	oB	aB	�B	�B	?B	%B	?B	YB	�B	B	�B		�B	~B	�B	�B	�B	�B	�B	"�B	'B	)B	+QB	-wB	.cB	-CB	-CB	-wB	-CB	.IB	2GB	6FB	8�B	8�B	5tB	8lB	;dB	;dB	<PB	<6B	@4B	G_B	N�B	S�B	VB	W$B	YKB	ZkB	\]B	\xB	]dB	^�B	`BB	a|B	b�B	a�B	^�B	]/B	`\B	abB	cnB	f�B	g�B	i�B	k�B	m�B	p�B	t�B	v�B	y�B	}B	B	�OB	��B	��B	�aB	�mB	�SB	�SB	�9B	�%B	�YB	�_B	�zB	�fB	�1B	�KB	�fB	�RB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�0B	�)B	�IB	�AB	�MB	�hB	�nB	�TB	��B	��B	�tB	�ZB	�`B	�zB	�lB	�lB	�rB	�rB	�xB	�xB	�B	��B	��B	ĜB	ĜB	ŢB	��B	żB	��B	��B	��B	�B	�B	�(B	�4B	�2B	�B	�+B	�1B	�eB	ۦB	�IB	�OB	�OB	�jB	��B	�B	�B	�nB	�tB	�tB	�zB	�zB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	�B	�"B	�B	�(B	�.B	�.B	�.B
 4B
;B
AB
AB
GB
aB
gB
YB
?B
EB
_B
EB
KB
fB
	RB
	lB

rB

�B
DB
~B
jB
jB
pB
pB
\B
vB
\B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
!�B
!�B
"B
#B
#B
$B
#�B
#�B
#�B
#�B
#�B
$B
%B
%�B
'B
&�B
(
B
(
B
($B
(
B
)B
)*B
*0B
)B
)B
)*B
*0B
*�B
+B
+B
+B
*0B
+B
,=B
,=B
-CB
-CB
-CB
-CB
.B
./B
./B
/5B
/5B
/5B
0UB
0UB
/5B
/5B
1AB
2GB
1AB
1vB
2|B
3MB
3MB
3MB
3MB
3MB
4TB
4TB
49B
4B
5?B
5?B
5?B
5?B
5ZB
6`B
6`B
6zB
6zB
6zB
6`B
6+B
7fB
9rB
9�B
:xB
:xB
:xB
:^B
:�B
;B
;B
;�B
;B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
IB
J�B
J�B
MB
N"B
M�B
NB
NB
M�B
N�B
N�B
N�B
N�B
OB
O�B
O�B
O�B
O�B
P�B
QB
Q4B
R B
R:B
RB
RB
RB
SB
SB
S&B
S&B
TB
T,B
TB
TB
TB
TB
T�B
UB
UB
UB
U2B
UB
VB
VB
VB
W$B
W?B
W$B
X+B
XB
X+B
X+B
XEB
Y1B
Y1B
Y1B
YB
YB
Y1B
YB
YKB
Z7B
Z7B
Z7B
Z7B
ZQB
[=B
[#B
[#B
[#B
[=B
\CB
\CB
\CB
\CB
\]B
]IB
^5B
^OB
^OB
^OB
^OB
_pB
_VB
_pB
_;B
_;B
_VB
_VB
`\B
`\B
`vB
abB
aHB
aHB
abB
aHB
aHB
abB
abB
abB
bhB
bhB
b�B
b�B
bhB
cTB
cTB
cTB
cnB
cnB
c�B
d�B
e`B
ezB
ezB
e`B
ffB
f�B
f�B
f�B
f�B
g�B
gmB
gmB
g�B
g�B
g�B
h�B
h�B
h�B
h�B
iyB
i�B
i�B
iyB
i�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
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
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608230036062016082300360620160823003606201806221212422018062212124220180622121242201804050405132018040504051320180405040513  JA  ARFMdecpA19c                                                                20160819093511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160819003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160819003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160819003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160819003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160819003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160819003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160819003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160819003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160819003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20160819012135                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160819153454  CV  JULD            G�O�G�O�F�!�                JM  ARCAJMQC2.0                                                                 20160822153606  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160822153606  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190513  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031242  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101517                      G�O�G�O�G�O�                