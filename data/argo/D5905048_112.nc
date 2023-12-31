CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-04-25T00:35:27Z creation;2017-04-25T00:35:30Z conversion to V3.1;2019-12-19T08:09:20Z update;     
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
resolution        =���   axis      Z        L  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  r�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170425003527  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               pA   JA  I2_0577_112                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�u�2 1   @�v����@2����&��d�MjO1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV�fDW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�3D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ Dؼ�D���D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D� D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@��@�z�A�
A?�
A_�
A�
A��A��A��A��A��A��A��A��B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�.B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%x�D%�\D&\D&�\D'\D'�\D(\D(�\D)��D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV��DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv\Dv�\Dw\Dw�\Dx\Dx�\Dy\Dy�\Dz\Dz�\D{\D{�\D|\D|�\D}\D}�\D~\D~�\D\D�\D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D��{D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D���D���D�?�D��D¿�D���D�?�D��Dÿ�D���D�?�D��DĿ�D���D�?�D��Dſ�D���D�?�D��Dƿ�D���D�?�D��Dǿ�D���D�?�D��Dȿ�D���D�?�D��Dɿ�D���D�?�D��Dʿ�D���D�?�D��D˿�D���D�?�D��D̿�D���D�?�D��DͿ�D���D�?�D��Dο�D���D�?�D��DϿ�D���D�?�D��Dп�D���D�?�D��Dѿ�D���D�?�D��Dҿ�D��D�?�D��Dӿ�D���D�?�D��DԿ�D���D�?�D��Dտ�D���D�?�D��Dֿ�D���D�?�D��D׿�D���D�?�D��Dؼ{D��{D�?�D��Dٿ�D���D�?�D��Dڿ�D���D�?�D��Dۿ�D���D�?�D��Dܿ�D���D�?�D��Dݿ�D���D�?�D��D޿�D���D�?�D��D߿�D���D�?�D��D࿮D���D�?�D��D΅D���D�?�D��D⿮D���D�?�D��D㿮D���D�?�D��D修D���D�?�D��D忮D���D�?�D��D濮D���D�?�D��D翮D���D�?�D��D迮D���D�?�D��D鿮D���D�?�D��D꿮D���D�?�D��D뿮D���D�?�D��D쿮D���D�?�D��D���D���D�?�D��DD���D�?�D��D￮D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D���D�?�D��D�D��D�B�D��D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AӰ!AӬAө�Aӝ�Aӟ�Aӗ�AӓuAӏ\A�S�A�ĜA�?}A��AС�AЏ\A��#A�7LAζFA�/A�\)A�~�AʼjAʝ�A�p�A��AɶFA�oA�hsA� �A�jAʲ-A��yA��A���Aȴ9A�dZA�-A�K�A���A�v�A�x�A�I�A��AȼjA�~�A�%A�;dA�"�Aŏ\A�VA�"�Aĕ�A��A�`BA7A�bA�  A���A�C�A�%A��A���A��+A�M�A�?}A�E�A���A�VA�bNA�bA�hsA���A���A���A��A���A�r�A�ĜA���A�l�A���A�XA�O�A��/A��A��A�^5A��FA�S�A�33A��RA�bNA���A�7LA��DA���A���A���A�hsA��+A�ƨA�\)A��9A�  A���A�A�`BA��A�I�A��A��A��FA��A�  A���A�dZA��9A�~�A���A�VA��A�1'A��HA��A���A���A���A�bNA��A��PA��A���A���A��A��A�A|�uAzI�Ax��AwC�AuXArĜApbAn�9AmAjA�Ag%Ae|�AeAdE�Ac�wAbA�A`JA]dZA[/AZ �AYx�AX~�AV=qAT�AS�;AR��AP�`APAOoAMO�AH�9AFE�AD-AA|�A>1'A<�A<(�A<�A:n�A8A�A6��A65?A3�-A1XA/�A.�/A,�RA+/A);dA'A&�A%|�A$�A$�A"�uA!XA
=A�wA��A�jA��A�FA+AE�A�A^5AbA�^A��A�PAoA�yA;dAjAȴAbA��A��A�PAM�A
A�A	K�A�\A�wAA��A�AC�A�9A�DA1'A�mA��A"�A jA 1'A J@�ƨ@���@�M�@�z�@��@�l�@���@���@�v�@���@�\@�h@� �@�!@��#@�/@���@�@�Z@�P@�
=@��H@��@��@���@��y@�ȴ@�-@�%@��#@���@�9@�t�@�"�@ާ�@�-@���@�G�@�%@ܬ@��m@��y@�V@���@��@ו�@�+@�V@�J@Ԭ@�K�@ҧ�@���@�o@́@�1'@�E�@��@���@�Z@Ɨ�@�&�@��
@��y@�J@��7@�G�@���@�bN@��P@�o@��\@�5?@���@�&�@�z�@�I�@��@�\)@��H@�ff@��T@��h@��@��;@��P@�o@��H@���@�n�@�$�@��T@���@�hs@�V@���@��u@�j@� �@���@��@���@��+@�ff@�{@���@��7@��@���@� �@�S�@�=q@�{@�x�@��-@��h@�hs@�x�@��-@�p�@���@�j@��;@��;@��m@��F@��P@�K�@�5?@���@���@���@�O�@�/@�%@��/@���@��@��P@���@�o@�ƨ@��@�9X@��@�b@���@���@��R@��@���@���@�ff@�$�@���@��9@�I�@�(�@�1@�hs@��@��m@�C�@�o@�@�M�@�5?@���@���@���@���@��@�?}@�O�@���@�hs@�?}@�O�@�O�@�O�@�X@�X@�p�@���@���@���@��7@��@��`@��@��`@��@�I�@�  @���@��
@���@���@��R@��\@�~�@�$�@���@��@��9@��D@�ƨ@��F@���@�t�@�@��y@��\@�v�@�M�@���@��@���@�/@�O�@�p�@�x�@��@��9@�z�@�I�@�A�@�Q�@��
@�dZ@�dZ@���@�n�@��T@��h@��^@��-@���@�`B@�&�@��@�G�@�/@��@���@��u@�b@��;@��F@��P@�|�@�l�@�S�@�@��R@��+@��+@�$�@�hs@�`B@�O�@�O�@�G�@�?}@�V@���@�Q�@�1'@�1@��;@��w@�|�@�dZ@�S�@�o@��y@���@�-@���@��h@�G�@�7L@�&�@��@��/@���@�Ĝ@�r�@��@�dZ@�33@��@�@���@��@�ȴ@��R@���@�-@�J@��@��@��@�x�@�&�@��/@�Q�@�b@��@��m@��@�\)@���@��+@��T@�/@�Ĝ@��u@��@�j@���@�dZ@�"�@��@���@���@�5?@��@�7L@�V@��/@�I�@�b@\)@~��@~�+@~V@~{@}�@}V@|��@|�@|j@{�
@{��@{33@zM�@y�^@yG�@xĜ@xbN@xA�@w�@w|�@v�y@vv�@vff@vE�@v$�@u�h@u�@uO�@t�D@s�m@s�
@sƨ@s��@sS�@sC�@r�@r��@rM�@rJ@q��@q�@pbN@o�;@o��@o|�@n�@nV@nE�@m�@m`B@l�@lz�@l9X@l�@kƨ@k��@j�@j=q@i��@ihs@i%@h��@h�9@hbN@g�w@g�@g�@g\)@f��@f�+@f$�@e�@e/@eV@d�/@dj@cƨ@c��@c�@c33@b��@bn�@b�@ahs@aX@aG�@a7L@a&�@a�@`��@_��@^��@]�@]�T@]O�@\1@[�F@[��@[t�@Z�@Zn�@ZM�@Y��@Y&�@X��@W�P@W+@W
=@V�y@Vȴ@Vff@U�T@U`B@T��@T��@Tz�@S��@Sƨ@S��@St�@S33@R�@R��@R~�@R^5@RM�@R�@Q��@Q�#@Q��@Qx�@QX@P��@P1'@O\)@N��@Nv�@Nff@N$�@M�@M��@MO�@L�@Lz�@L�@L1@K�
@K��@KC�@Ko@J�@J��@J~�@JM�@I�@IG�@H�9@HbN@H1'@H  @G�w@G�P@G|�@G�P@G+@F��@FE�@E�@E��@E@E�@E`B@D�@D�D@C��@CS�@B�H@B=q@A��@A�#@A��@A��@A7L@A%@@�`@@Ĝ@@�u@@�@@bN@?�;@?�@>ȴ@>�+@>@=��@=��@=p�@<�/@<��@<z�@<Z@<(�@;ƨ@;dZ@;@:�!@:��@:^5@:�@9�#@9��@9��@9x�@9G�@8Q�@8  @7�@7K�@6�@6�+@6V@5@5p�@5/@4�@4�D@49X@41@3�
@3�@333@2�H@2~�@2J@1�@1�@1�#@1�#@1�#@1�7@0��@0��@0�u@0�u@0A�@/�w@/��@/l�@/;d@/
=@.�R@.v�@.E�@.{@-�h@-�@,�/@,j@,9X@,(�@,�@,1@+�F@+o@*�@*�@*�H@*��@*n�@)��@)&�@(��@(Ĝ@(Ĝ@(Ĝ@(�9@(�9@(�9@(�u@'�;@'\)@'�@&$�@%��@%O�@$��@$��@$�j@$z�@$1@#��@#�@#t�@#dZ@#S�@#o@"��@"n�@"-@"J@!��@!�@!�@!�@!��@!��@!7L@ �`@ ��@ �@ bN@ 1'@�;@��@�P@\)@�@�@��@�+@v�@ff@V@V@E�@$�@�-@V@z�@I�@9X@9X@(�@1@�m@��@t�@C�@C�@33@�H@��@~�@-@�^@x�@hs@7L@Ĝ@��@�u@bN@b@�@��@l�@+@�y@�y@�y@ȴ@v�@5?@@�-@�h@�@/@�@I�@��@�
@�
@ƨ@�F@33@n�@�@J@J@��@�@�#@�7@&�@%@%@��@A�@�;@��@��@�P@|�@K�@
=@�y@��@v�@V@{@�T@�h@p�@?}@V@�/@�@��@z�@z�@z�@�D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AӰ!AӬAө�Aӝ�Aӟ�Aӗ�AӓuAӏ\A�S�A�ĜA�?}A��AС�AЏ\A��#A�7LAζFA�/A�\)A�~�AʼjAʝ�A�p�A��AɶFA�oA�hsA� �A�jAʲ-A��yA��A���Aȴ9A�dZA�-A�K�A���A�v�A�x�A�I�A��AȼjA�~�A�%A�;dA�"�Aŏ\A�VA�"�Aĕ�A��A�`BA7A�bA�  A���A�C�A�%A��A���A��+A�M�A�?}A�E�A���A�VA�bNA�bA�hsA���A���A���A��A���A�r�A�ĜA���A�l�A���A�XA�O�A��/A��A��A�^5A��FA�S�A�33A��RA�bNA���A�7LA��DA���A���A���A�hsA��+A�ƨA�\)A��9A�  A���A�A�`BA��A�I�A��A��A��FA��A�  A���A�dZA��9A�~�A���A�VA��A�1'A��HA��A���A���A���A�bNA��A��PA��A���A���A��A��A�A|�uAzI�Ax��AwC�AuXArĜApbAn�9AmAjA�Ag%Ae|�AeAdE�Ac�wAbA�A`JA]dZA[/AZ �AYx�AX~�AV=qAT�AS�;AR��AP�`APAOoAMO�AH�9AFE�AD-AA|�A>1'A<�A<(�A<�A:n�A8A�A6��A65?A3�-A1XA/�A.�/A,�RA+/A);dA'A&�A%|�A$�A$�A"�uA!XA
=A�wA��A�jA��A�FA+AE�A�A^5AbA�^A��A�PAoA�yA;dAjAȴAbA��A��A�PAM�A
A�A	K�A�\A�wAA��A�AC�A�9A�DA1'A�mA��A"�A jA 1'A J@�ƨ@���@�M�@�z�@��@�l�@���@���@�v�@���@�\@�h@� �@�!@��#@�/@���@�@�Z@�P@�
=@��H@��@��@���@��y@�ȴ@�-@�%@��#@���@�9@�t�@�"�@ާ�@�-@���@�G�@�%@ܬ@��m@��y@�V@���@��@ו�@�+@�V@�J@Ԭ@�K�@ҧ�@���@�o@́@�1'@�E�@��@���@�Z@Ɨ�@�&�@��
@��y@�J@��7@�G�@���@�bN@��P@�o@��\@�5?@���@�&�@�z�@�I�@��@�\)@��H@�ff@��T@��h@��@��;@��P@�o@��H@���@�n�@�$�@��T@���@�hs@�V@���@��u@�j@� �@���@��@���@��+@�ff@�{@���@��7@��@���@� �@�S�@�=q@�{@�x�@��-@��h@�hs@�x�@��-@�p�@���@�j@��;@��;@��m@��F@��P@�K�@�5?@���@���@���@�O�@�/@�%@��/@���@��@��P@���@�o@�ƨ@��@�9X@��@�b@���@���@��R@��@���@���@�ff@�$�@���@��9@�I�@�(�@�1@�hs@��@��m@�C�@�o@�@�M�@�5?@���@���@���@���@��@�?}@�O�@���@�hs@�?}@�O�@�O�@�O�@�X@�X@�p�@���@���@���@��7@��@��`@��@��`@��@�I�@�  @���@��
@���@���@��R@��\@�~�@�$�@���@��@��9@��D@�ƨ@��F@���@�t�@�@��y@��\@�v�@�M�@���@��@���@�/@�O�@�p�@�x�@��@��9@�z�@�I�@�A�@�Q�@��
@�dZ@�dZ@���@�n�@��T@��h@��^@��-@���@�`B@�&�@��@�G�@�/@��@���@��u@�b@��;@��F@��P@�|�@�l�@�S�@�@��R@��+@��+@�$�@�hs@�`B@�O�@�O�@�G�@�?}@�V@���@�Q�@�1'@�1@��;@��w@�|�@�dZ@�S�@�o@��y@���@�-@���@��h@�G�@�7L@�&�@��@��/@���@�Ĝ@�r�@��@�dZ@�33@��@�@���@��@�ȴ@��R@���@�-@�J@��@��@��@�x�@�&�@��/@�Q�@�b@��@��m@��@�\)@���@��+@��T@�/@�Ĝ@��u@��@�j@���@�dZ@�"�@��@���@���@�5?@��@�7L@�V@��/@�I�@�b@\)@~��@~�+@~V@~{@}�@}V@|��@|�@|j@{�
@{��@{33@zM�@y�^@yG�@xĜ@xbN@xA�@w�@w|�@v�y@vv�@vff@vE�@v$�@u�h@u�@uO�@t�D@s�m@s�
@sƨ@s��@sS�@sC�@r�@r��@rM�@rJ@q��@q�@pbN@o�;@o��@o|�@n�@nV@nE�@m�@m`B@l�@lz�@l9X@l�@kƨ@k��@j�@j=q@i��@ihs@i%@h��@h�9@hbN@g�w@g�@g�@g\)@f��@f�+@f$�@e�@e/@eV@d�/@dj@cƨ@c��@c�@c33@b��@bn�@b�@ahs@aX@aG�@a7L@a&�@a�@`��@_��@^��@]�@]�T@]O�@\1@[�F@[��@[t�@Z�@Zn�@ZM�@Y��@Y&�@X��@W�P@W+@W
=@V�y@Vȴ@Vff@U�T@U`B@T��@T��@Tz�@S��@Sƨ@S��@St�@S33@R�@R��@R~�@R^5@RM�@R�@Q��@Q�#@Q��@Qx�@QX@P��@P1'@O\)@N��@Nv�@Nff@N$�@M�@M��@MO�@L�@Lz�@L�@L1@K�
@K��@KC�@Ko@J�@J��@J~�@JM�@I�@IG�@H�9@HbN@H1'@H  @G�w@G�P@G|�@G�P@G+@F��@FE�@E�@E��@E@E�@E`B@D�@D�D@C��@CS�@B�H@B=q@A��@A�#@A��@A��@A7L@A%@@�`@@Ĝ@@�u@@�@@bN@?�;@?�@>ȴ@>�+@>@=��@=��@=p�@<�/@<��@<z�@<Z@<(�@;ƨ@;dZ@;@:�!@:��@:^5@:�@9�#@9��@9��@9x�@9G�@8Q�@8  @7�@7K�@6�@6�+@6V@5@5p�@5/@4�@4�D@49X@41@3�
@3�@333@2�H@2~�@2J@1�@1�@1�#@1�#@1�#@1�7@0��@0��@0�u@0�u@0A�@/�w@/��@/l�@/;d@/
=@.�R@.v�@.E�@.{@-�h@-�@,�/@,j@,9X@,(�@,�@,1@+�F@+o@*�@*�@*�H@*��@*n�@)��@)&�@(��@(Ĝ@(Ĝ@(Ĝ@(�9@(�9@(�9@(�u@'�;@'\)@'�@&$�@%��@%O�@$��@$��@$�j@$z�@$1@#��@#�@#t�@#dZ@#S�@#o@"��@"n�@"-@"J@!��@!�@!�@!�@!��@!��@!7L@ �`@ ��@ �@ bN@ 1'@�;@��@�P@\)@�@�@��@�+@v�@ff@V@V@E�@$�@�-@V@z�@I�@9X@9X@(�@1@�m@��@t�@C�@C�@33@�H@��@~�@-@�^@x�@hs@7L@Ĝ@��@�u@bN@b@�@��@l�@+@�y@�y@�y@ȴ@v�@5?@@�-@�h@�@/@�@I�@��@�
@�
@ƨ@�F@33@n�@�@J@J@��@�@�#@�7@&�@%@%@��@A�@�;@��@��@�P@|�@K�@
=@�y@��@v�@V@{@�T@�h@p�@?}@V@�/@�@��@z�@z�@z�@�D1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
B
%B
1B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
)�B
!�B
%�B
'�B
+B
8RB
A�B
@�B
?}B
G�B
\)B
��B
�wB
��B
��B�BVB
��BuB!�B0!B49BVB��B�#BoB{B!�B/B2-B?}BJ�BO�BXB_;BaHBgmBp�Bp�BiyBm�Bo�Bq�Br�Bs�Bv�Bu�Bu�Bu�BgmBYBT�BD�B6FB7LB?}BE�BG�BG�BO�B_;B�BƨB��B�BB�TB�;B�5B�ZB�mB�B�yB�`B�
B��BƨB��B�}B�jB�9B��B��B��B�JB�PB��B�%Bu�B[#B;dB33B+B2-B;dB6FB'�B1B�BB�B��B��B�PBw�Be`BP�BC�B2-B �B{B%B
��B
�B
�B
�fB
�/B
��B
�9B
��B
�uB
�PB
|�B
l�B
bNB
T�B
E�B
2-B
�B

=B
B	�B	��B	ĜB	��B	�jB	�LB	�3B	��B	��B	�B	w�B	q�B	l�B	aHB	S�B	M�B	H�B	>wB	7LB	0!B	)�B	oB	%B��B��B�ZB�#B�
B�B��BĜB�^B�9B�B��B��B��B�{B�JB�%B�B{�Bz�Bx�Bv�Bt�Bp�BjBhsBgmBgmBgmBffBaHBaHB^5B[#BZBZBZBZB]/B^5Be`BffBdZBdZBl�BgmBgmBe`BbNB`BB_;B_;BbNBe`BcTBaHBffBl�Bn�Bp�Bv�B|�B|�B|�B{�Bx�B}�B}�B�B�B�B�B�B�B� B�B�1B�=B�7B�1B�1B�7B�7B�7B�=B�DB�DB�VB��B��B��B��B��B��B�'B��BĜBȴB��B��B��B��B��B�
B�B�HB�;B�;B�TB�ZB�fB�sB�B�B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	1B	
=B	DB	JB	PB	VB	oB	�B	�B	�B	�B	�B	�B	 �B	"�B	'�B	(�B	)�B	+B	+B	+B	/B	33B	49B	5?B	5?B	6FB	7LB	9XB	:^B	;dB	<jB	>wB	?}B	@�B	A�B	D�B	F�B	G�B	H�B	K�B	N�B	N�B	O�B	Q�B	S�B	T�B	W
B	VB	ZB	[#B	`BB	dZB	e`B	gmB	jB	n�B	o�B	o�B	o�B	p�B	s�B	v�B	v�B	v�B	|�B	~�B	� B	� B	�B	�B	�+B	�7B	�7B	�VB	�DB	�1B	�DB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�?B	�-B	�3B	�9B	�?B	�9B	�9B	�?B	�LB	�XB	�^B	�^B	�^B	�dB	�wB	�}B	�}B	��B	ÖB	ĜB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�NB	�TB	�fB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
1B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
JB
PB
VB
VB
\B
VB
VB
\B
\B
\B
\B
bB
hB
uB
uB
{B
{B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
-B
-B
.B
/B
1'B
2-B
2-B
2-B
33B
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
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
9XB
9XB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
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
>wB
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
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
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
G�B
G�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
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
L�B
L�B
L�B
L�B
L�B
L�B
L�B
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
R�B
S�B
S�B
S�B
S�B
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
W
B
W
B
W
B
W
B
XB
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
ZB
[#B
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
_;B
_;B
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
dZB
dZB
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
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
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
jB
jB
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
m�B
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
s�B
r�B
r�B
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
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
9B
?B
�B
�B
�B
�B
B
�B
OB
�B
KB
EB
CB
$ZB
+QB
#�B
(�B
)�B
,�B
9�B
A�B
AB
@4B
H1B
[�B
�B
��B
бB
��B �B�B
��B�B"NB0;B3�BU2B��B�=B�B2B"hB/�B3�BAUBL�BQ BX�B_�Bb�Bh�Bq�Br-BjKBm�Bp;Br�Bs�ButBw�BvFBv�BxBi_BZ�BX+BGB9�B:�BB�BG�BIlBIBP�B_�B��B��B՛B�HB�ZBߤB�;B�FB��B�B�kB�$B�KB��BǮB�-B��B��B��B�0B��B��B��B�vB�bB��By>B]�B<�B4�B,qB3�B<�B9XB,�BB�'B�B��B�B��B�4B{JBh>BSuBF�B4�B# B$BB
��B
�B
�!B
�XB
�'B
��B
��B
�B
�B
�HB
�B
n�B
dZB
W�B
H�B
5B
�B
dB
%B	��B	ҽB	�mB	��B	��B	��B	��B	��B	��B	�uB	x�B	shB	oB	c:B	U2B	O�B	J�B	?�B	9$B	3MB	.�B	�B		7B��B�B��B�B��B�WB�uB�YB�B�fB��B��B��B�IB��B��B��B�AB|�B|BzBx�Bv�BsMBl"BiyBh
Bh$Bi*BiDBb�Bb�B_B[�BZ�B[�B[qBZ�B]~B^jBf�BhXBe�Be�Bm�BhsBi_Bg�Bc�BabB`vB`�Bd@Bg8BdtBbBf�BmBoBqABw�B}�B}VB}qB|�B{JBBB��B��B��B�B��B�GB��B��B�7B�)B��B��B��B�lB��B��B��B��B�0B��B��B��B��B�EB��B�dB�vB�AB�mB�B�6B�NB�:B�TB�@B�sB��B��B��B�BB��B��B��B�B�B�B��B��B�	B�0B��B��B�*B��B�HB��B	;B	-B	B	�B	�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	B		B	�B	!B	#nB	(sB	)_B	*eB	+QB	+�B	+�B	/iB	3�B	4nB	5tB	5�B	6zB	7�B	9�B	:�B	;�B	<�B	>�B	?�B	@�B	A�B	EB	F�B	G�B	H�B	LB	OB	OBB	PHB	RTB	T{B	U�B	W�B	VSB	ZkB	[#B	`\B	dtB	e`B	gmB	j�B	o B	o�B	o�B	o�B	p�B	s�B	v�B	wB	wfB	}"B	B	�4B	�4B	�'B	�GB	�EB	�RB	�lB	�(B	��B	�B	��B	�@B	��B	��B	��B	�B	�OB	��B	��B	��B	�B	�2B	�2B	�LB	��B	�:B	��B	��B	�ZB	��B	��B	��B	�hB	�nB	��B	�nB	��B	�ZB	�fB	�rB	�xB	�xB	�^B	�JB	��B	��B	�}B	��B	ÖB	ĜB	ǮB	ɺB	ʦB	��B	��B	�:B	�@B	�B	��B	�B	�2B	�MB	�FB	�B	�2B	׍B	�KB	�KB	�7B	�7B	�kB	یB	ݘB	ބB	�jB	ޞB	�pB	�VB	�vB	��B	�bB	�B	�hB	�B	�B	�B	�B	��B	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	��B	��B	�B	�B	��B	��B	�B	�B	�<B	�.B	�HB	�.B	�B
 4B
 B
 B
 B
AB
GB
MB
9B
mB
�B
1B
KB
1B
KB
KB
�B
fB
	�B

XB

XB

=B

rB

�B

XB

XB

�B

rB

rB

�B

�B

rB

rB

XB
^B
dB
jB
pB
pB
�B
�B
�B
vB
vB
vB
vB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
)B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
 B
 �B
 �B
 �B
!�B
!�B
"B
!�B
!�B
"�B
"�B
"�B
"�B
#B
#�B
#�B
$&B
%B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
&B
%�B
&B
&B
'B
'B
'B
'B
(
B
(>B
)B
)*B
)*B
*0B
*0B
*B
*B
*B
+B
+QB
+QB
,=B
-)B
-)B
-)B
-)B
-CB
.IB
/5B
1'B
2GB
2aB
2aB
3hB
3hB
3hB
3MB
3hB
3�B
4�B
4TB
4TB
4nB
5tB
5ZB
5tB
5tB
6FB
7LB
7LB
7fB
7fB
7�B
7�B
7�B
9�B
9rB
8�B
9�B
:xB
:xB
:xB
:�B
:�B
:xB
:�B
:�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=qB
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
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
G�B
G�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
J	B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
MB
N"B
N�B
N�B
OB
N�B
N�B
PB
P.B
O�B
O�B
Q B
Q B
QB
Q B
Q B
RB
R B
RB
RB
RB
SB
SB
SB
S&B
S@B
TB
T,B
T,B
T,B
UB
UB
U2B
U2B
VB
V9B
VB
VB
VB
VB
W$B
W?B
W$B
W?B
X+B
Y1B
YB
Y1B
YB
Y1B
YeB
YKB
Z7B
ZB
ZQB
Z7B
ZQB
Z7B
[=B
[=B
[=B
[=B
[=B
[=B
\CB
\]B
]~B
]dB
]dB
^OB
^5B
^OB
^OB
^jB
^jB
_;B
_!B
_VB
_VB
_�B
_�B
`vB
abB
aHB
aHB
aHB
abB
aHB
abB
abB
a�B
b�B
b�B
b�B
d�B
d�B
dtB
ezB
ezB
ezB
e�B
f�B
f�B
ffB
ffB
f�B
f�B
f�B
g�B
g�B
g�B
gmB
gmB
hsB
gmB
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
jB
jB
jB
jB
j�B
j�B
j�B
k�B
k�B
l�B
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
m�B
n�B
n�B
n�B
n�B
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
s�B
r�B
r�B
tB
tB
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
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
xB
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201704290036092017042900360920170429003609201806221312322018062213123220180622131232201804050713512018040507135120180405071351  JA  ARFMdecpA19c                                                                20170425093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170425003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170425003528  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170425003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170425003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170425003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170425003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170425003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170425003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170425003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20170425010701                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170425153543  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170428153609  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170428153609  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221351  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041232  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                