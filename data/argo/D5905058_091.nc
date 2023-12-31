CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-23T09:35:22Z creation;2018-09-23T09:35:25Z conversion to V3.1;2019-12-23T06:14:42Z update;     
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180923093522  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               [A   JA  I2_0675_091                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؃�[�j 1   @؃�q� @7XF�]d�cN-V1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ D�|�D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��A��A(��AH��Ah��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB
=qB=qB=qB"=qB*=qB2=qB:=qBB=qBJ=qBR=qBZ=qBb=qBj=qBr=qBz=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �\C�\C�\C�\C�\C
�\C��C�\C�\C�\C�\Cu�C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2�\C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CL�\CN�\CP�\CR�\CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch��Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~�\C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�:�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D*=D��D#�D��D#�D��D#�D��D	#�D	��D
*=D
��D#�D��D#�D�qD#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<��D=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI#�DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Du#�Du��Dv#�Dv��Dw#�Dw��Dx#�Dx��Dy#�Dy��Dz#�Dz��D{#�D{��D|#�D|��D}#�D}��D~#�D~��D#�D��D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D�D���D��D�Q�DÑ�D���D��D�Q�Dđ�D���D��D�Q�Dő�D���D��D�Q�DƑ�D���D��D�Q�DǑ�D���D��D�Q�Dȑ�D���D��D�Q�Dɑ�D���D��D�Q�Dʑ�D���D��D�Q�Dˑ�D���D��D�Q�D̑�D���D��D�Q�D͑�D���D��D�Q�DΑ�D���D��D�Q�Dϑ�D���D��D�Q�DБ�D���D��D�Q�Dю�D���D��D�Q�Dґ�D���D��D�Q�Dӑ�D���D��D�Q�Dԑ�D���D��D�Q�DՑ�D���D��D�Q�D֑�D���D��D�Q�Dב�D���D��D�Q�Dؑ�D���D��D�Q�Dّ�D���D��D�Q�Dڑ�D���D��D�Q�Dۑ�D���D��D�Q�Dܑ�D���D��D�Q�Dݑ�D���D��D�Q�Dޑ�D���D��D�Q�Dߑ�D���D��D�Q�D���D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�UD��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AЙ�AЛ�AЛ�AЛ�AП�AУ�AХ�AУ�AС�AУ�AХ�AЧ�AЩ�AЬAЬAЧ�AН�AУ�AН�AБhAЏ\AЍPAЕ�A�l�A�^5A�/A�-A���Aϴ9AΧ�A�9XAüjA�oA��+A��PA��A���A���A�+A���A��-A�  A��A���A��A��A��A��/A�^5A�E�A��A�I�A�A���A���A��#A�x�A���A���A�^5A��A�A���A��
A��TA�(�A��mA�bNA�A��DA�?}A�(�A���A�A�A��RA��A��9A�jA���A�jA��A�O�A�l�A���A�r�A�;dA���A��A���A�S�A�O�A���A�?}A�
=A���A�l�A�-A��A��PA�$�A��\A�oA���A�M�A���A�{A��A�VA�ZA��DA�"�A��
A��\A�JA�^5A�1'A�A~�DA|^5AyAv�+Au�AtVArbApQ�Am?}Al=qAi�wAf^5Ae+Ac�TA^1AZ1AXv�AW;dAT�/AR�+AQ+APn�AP �AO��AO;dAN~�AMl�AL$�AIoAG�AF�/AFv�AE
=AD-AC��AB��AB��AB-AA
=A?�A?\)A>VA=�PA<�yA<��A;�
A;oA:�/A:�A:(�A9%A7�PA69XA4 �A2�A2�+A1��A.��A.=qA-��A-G�A,��A,1A+
=A)K�A'`BA#��A"��A!�;A!��A  �AȴA;dAz�AƨA;dA��A�yAS�AQ�Ax�A�A��Ap�AoA�A�wA��AjA�
A^5A?}A
ȴA
�jA
�A
ZA	�hA�A�A%A�\A`BA(�A�9A�TAdZA n�A   @�\)@��\@��@��@�v�@�@�7L@���@�1'@�33@��^@���@�~�@�@�j@�;d@���@�9X@��@�z�@�
=@�=q@�?}@�ƨ@⟾@���@�ƨ@ޏ\@���@�Q�@��m@ڗ�@�G�@؃@���@ְ!@�@��@�=q@љ�@��@��
@Η�@�Q�@��@ʗ�@�x�@�A�@Ǖ�@�t�@��@�V@��@ŉ7@��`@���@¸R@�p�@�Q�@�ƨ@�C�@��7@�K�@��#@��7@�V@�j@��@��@��@��7@���@�I�@���@�o@�M�@�V@�bN@��@�C�@���@���@�Z@�A�@�9X@��;@��@��-@���@��u@�j@�bN@�Q�@�A�@���@��@��\@�n�@�ff@�$�@�{@�{@��@���@�O�@���@��u@�K�@��@��@���@��#@���@���@��#@���@�G�@�%@�Ĝ@��@�z�@�j@�Z@�1'@� �@�  @�  @�t�@�5?@�M�@���@���@��+@�n�@��@�@�r�@�b@�S�@��@��H@��H@��H@��+@�-@��#@��-@���@���@�$�@�n�@�$�@���@��T@��T@��#@���@���@��h@��h@�p�@��@�G�@�G�@��9@�\)@�dZ@���@�|�@�|�@�|�@�K�@�33@��+@���@��h@���@��7@�G�@��j@��@�1'@���@��j@�bN@��9@��u@���@��P@��@��@���@���@��@��y@��@��y@��y@��R@�ff@�=q@�@��^@���@���@�x�@��@��@��@��9@�r�@��;@��P@�;d@���@�v�@�^5@�-@���@��#@���@�hs@��@��9@���@��u@�r�@�1'@��;@�dZ@��H@���@�V@�=q@��@���@��^@��@�O�@�G�@�?}@���@�j@�(�@���@�l�@��@��+@�-@�@��-@�/@���@���@�r�@� �@�b@�  @���@��;@�ƨ@�t�@�+@��y@��R@��\@�ff@�5?@���@���@�p�@�G�@���@��j@���@��D@�z�@�j@�Q�@�(�@l�@~��@~��@~�+@~��@~��@~v�@~$�@}@}V@|�/@|�j@|I�@|(�@|�@|1@{dZ@z�H@z�\@z�@y��@y�7@yx�@yG�@x��@xQ�@xb@xA�@w�@wK�@v��@vȴ@v��@v5?@u�T@up�@up�@u?}@s��@s@r�H@s@r~�@q�^@q��@q��@qhs@qG�@q%@p��@pA�@o�w@ol�@o
=@nV@n5?@n5?@m@m/@l�@lZ@kS�@k@j�H@j�!@j��@j�\@j^5@j-@i�@i��@ix�@iG�@h��@h�u@hA�@g�@g\)@g�@f�@f��@f�+@e�T@e`B@d�j@dI�@d�@cƨ@c33@a��@a��@a�@`��@`1'@_�w@_
=@^��@]�T@]�h@]p�@]`B@]`B@]?}@\��@\�@\j@\(�@[��@[o@Z~�@Y��@Y��@Yx�@YG�@X��@Xb@W�w@WK�@V��@V@U�h@U�@U?}@T�@T�@T�/@T��@T�D@T9X@T1@S��@S�@SdZ@SC�@So@R�\@R�@Q��@Q�7@Q7L@P��@Pr�@PQ�@O��@O|�@Ol�@O�@N�y@Nff@M�-@M/@L�/@L�@L9X@K�@J�!@JM�@J-@I�#@I�^@IX@HĜ@H  @G�w@G�P@G\)@G;d@F��@F�R@Fff@F$�@F@E�-@E`B@D�/@D��@Dz�@DZ@D9X@D1@Cƨ@CdZ@Co@B�@A7L@A�@@�9@@Q�@@b@?�@?�;@?�w@?��@?l�@?\)@?;d@?;d@?
=@?
=@>v�@>V@>5?@=�@=/@<��@<�D@<z�@<Z@<I�@<9X@<(�@<(�@<�@<1@;��@;ƨ@;��@;S�@:��@:~�@:~�@:~�@:M�@:-@:J@9��@9��@9��@9��@9�7@9�7@9x�@9&�@8��@8�9@8��@8�@8Q�@81'@8 �@7�@7|�@7;d@7+@7
=@6��@6�y@6�+@6E�@6{@5��@5��@5`B@4�j@4I�@4I�@4�@3��@3S�@333@3o@2�\@1�@1hs@0Ĝ@0r�@0b@/�w@/�@/�@/�P@/\)@/�@.�@.��@.V@.{@-�T@-��@-��@-�h@-?}@-�@,�j@,9X@,(�@,�@+��@+�F@+dZ@+t�@+��@+dZ@+o@*��@*�!@*�\@*~�@*~�@*~�@*n�@*^5@*=q@*J@)��@)x�@(�9@(r�@(A�@(1'@( �@( �@'�@'�@'��@'�@'��@'�P@'�P@'l�@&v�@&E�@&5?@%�@%�h@%`B@%`B@$��@$�D@$9X@#�m@#�@#S�@#C�@"�\@"M�@"=q@"�@!�^@!X@!7L@!�@ �`@ �@ r�@ bN@ Q�@ A�@ 1'@ b@��@l�@\)@K�@K�@�@ȴ@�+@E�@$�@�@�T@��@`B@/@��@�D@z�@9X@�@��@��@@�@�@��@^5@-@J@�@�#@��@x�@�^@G�@%@�@�@�@%@�@�@�@%@�`@�9@�@bN@A�@ �@b@  @�@��@�w@��@|�@\)@�@�y@��@v�@V@5?@@��@O�@�/@�@�D@�D@z�@I�@(�@�@��@�
@�F@t�@o@��@��@��@�!@��@~�@^5@-@��@��@x�@x�@x�@hs@7L@bN@  @�@��@�w@�@|�@l�@\)@
=@ȴ@V@�@@�h@�@`B@?}@��@�@�/@�D@I�@(�@�@��@�F@��@S�@@
�@
�H@
��@
~�@
^5@
-@
J@	��@	�#@	�^@	��@	�7@	X@	&�@	&�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AЙ�AЛ�AЛ�AЛ�AП�AУ�AХ�AУ�AС�AУ�AХ�AЧ�AЩ�AЬAЬAЧ�AН�AУ�AН�AБhAЏ\AЍPAЕ�A�l�A�^5A�/A�-A���Aϴ9AΧ�A�9XAüjA�oA��+A��PA��A���A���A�+A���A��-A�  A��A���A��A��A��A��/A�^5A�E�A��A�I�A�A���A���A��#A�x�A���A���A�^5A��A�A���A��
A��TA�(�A��mA�bNA�A��DA�?}A�(�A���A�A�A��RA��A��9A�jA���A�jA��A�O�A�l�A���A�r�A�;dA���A��A���A�S�A�O�A���A�?}A�
=A���A�l�A�-A��A��PA�$�A��\A�oA���A�M�A���A�{A��A�VA�ZA��DA�"�A��
A��\A�JA�^5A�1'A�A~�DA|^5AyAv�+Au�AtVArbApQ�Am?}Al=qAi�wAf^5Ae+Ac�TA^1AZ1AXv�AW;dAT�/AR�+AQ+APn�AP �AO��AO;dAN~�AMl�AL$�AIoAG�AF�/AFv�AE
=AD-AC��AB��AB��AB-AA
=A?�A?\)A>VA=�PA<�yA<��A;�
A;oA:�/A:�A:(�A9%A7�PA69XA4 �A2�A2�+A1��A.��A.=qA-��A-G�A,��A,1A+
=A)K�A'`BA#��A"��A!�;A!��A  �AȴA;dAz�AƨA;dA��A�yAS�AQ�Ax�A�A��Ap�AoA�A�wA��AjA�
A^5A?}A
ȴA
�jA
�A
ZA	�hA�A�A%A�\A`BA(�A�9A�TAdZA n�A   @�\)@��\@��@��@�v�@�@�7L@���@�1'@�33@��^@���@�~�@�@�j@�;d@���@�9X@��@�z�@�
=@�=q@�?}@�ƨ@⟾@���@�ƨ@ޏ\@���@�Q�@��m@ڗ�@�G�@؃@���@ְ!@�@��@�=q@љ�@��@��
@Η�@�Q�@��@ʗ�@�x�@�A�@Ǖ�@�t�@��@�V@��@ŉ7@��`@���@¸R@�p�@�Q�@�ƨ@�C�@��7@�K�@��#@��7@�V@�j@��@��@��@��7@���@�I�@���@�o@�M�@�V@�bN@��@�C�@���@���@�Z@�A�@�9X@��;@��@��-@���@��u@�j@�bN@�Q�@�A�@���@��@��\@�n�@�ff@�$�@�{@�{@��@���@�O�@���@��u@�K�@��@��@���@��#@���@���@��#@���@�G�@�%@�Ĝ@��@�z�@�j@�Z@�1'@� �@�  @�  @�t�@�5?@�M�@���@���@��+@�n�@��@�@�r�@�b@�S�@��@��H@��H@��H@��+@�-@��#@��-@���@���@�$�@�n�@�$�@���@��T@��T@��#@���@���@��h@��h@�p�@��@�G�@�G�@��9@�\)@�dZ@���@�|�@�|�@�|�@�K�@�33@��+@���@��h@���@��7@�G�@��j@��@�1'@���@��j@�bN@��9@��u@���@��P@��@��@���@���@��@��y@��@��y@��y@��R@�ff@�=q@�@��^@���@���@�x�@��@��@��@��9@�r�@��;@��P@�;d@���@�v�@�^5@�-@���@��#@���@�hs@��@��9@���@��u@�r�@�1'@��;@�dZ@��H@���@�V@�=q@��@���@��^@��@�O�@�G�@�?}@���@�j@�(�@���@�l�@��@��+@�-@�@��-@�/@���@���@�r�@� �@�b@�  @���@��;@�ƨ@�t�@�+@��y@��R@��\@�ff@�5?@���@���@�p�@�G�@���@��j@���@��D@�z�@�j@�Q�@�(�@l�@~��@~��@~�+@~��@~��@~v�@~$�@}@}V@|�/@|�j@|I�@|(�@|�@|1@{dZ@z�H@z�\@z�@y��@y�7@yx�@yG�@x��@xQ�@xb@xA�@w�@wK�@v��@vȴ@v��@v5?@u�T@up�@up�@u?}@s��@s@r�H@s@r~�@q�^@q��@q��@qhs@qG�@q%@p��@pA�@o�w@ol�@o
=@nV@n5?@n5?@m@m/@l�@lZ@kS�@k@j�H@j�!@j��@j�\@j^5@j-@i�@i��@ix�@iG�@h��@h�u@hA�@g�@g\)@g�@f�@f��@f�+@e�T@e`B@d�j@dI�@d�@cƨ@c33@a��@a��@a�@`��@`1'@_�w@_
=@^��@]�T@]�h@]p�@]`B@]`B@]?}@\��@\�@\j@\(�@[��@[o@Z~�@Y��@Y��@Yx�@YG�@X��@Xb@W�w@WK�@V��@V@U�h@U�@U?}@T�@T�@T�/@T��@T�D@T9X@T1@S��@S�@SdZ@SC�@So@R�\@R�@Q��@Q�7@Q7L@P��@Pr�@PQ�@O��@O|�@Ol�@O�@N�y@Nff@M�-@M/@L�/@L�@L9X@K�@J�!@JM�@J-@I�#@I�^@IX@HĜ@H  @G�w@G�P@G\)@G;d@F��@F�R@Fff@F$�@F@E�-@E`B@D�/@D��@Dz�@DZ@D9X@D1@Cƨ@CdZ@Co@B�@A7L@A�@@�9@@Q�@@b@?�@?�;@?�w@?��@?l�@?\)@?;d@?;d@?
=@?
=@>v�@>V@>5?@=�@=/@<��@<�D@<z�@<Z@<I�@<9X@<(�@<(�@<�@<1@;��@;ƨ@;��@;S�@:��@:~�@:~�@:~�@:M�@:-@:J@9��@9��@9��@9��@9�7@9�7@9x�@9&�@8��@8�9@8��@8�@8Q�@81'@8 �@7�@7|�@7;d@7+@7
=@6��@6�y@6�+@6E�@6{@5��@5��@5`B@4�j@4I�@4I�@4�@3��@3S�@333@3o@2�\@1�@1hs@0Ĝ@0r�@0b@/�w@/�@/�@/�P@/\)@/�@.�@.��@.V@.{@-�T@-��@-��@-�h@-?}@-�@,�j@,9X@,(�@,�@+��@+�F@+dZ@+t�@+��@+dZ@+o@*��@*�!@*�\@*~�@*~�@*~�@*n�@*^5@*=q@*J@)��@)x�@(�9@(r�@(A�@(1'@( �@( �@'�@'�@'��@'�@'��@'�P@'�P@'l�@&v�@&E�@&5?@%�@%�h@%`B@%`B@$��@$�D@$9X@#�m@#�@#S�@#C�@"�\@"M�@"=q@"�@!�^@!X@!7L@!�@ �`@ �@ r�@ bN@ Q�@ A�@ 1'@ b@��@l�@\)@K�@K�@�@ȴ@�+@E�@$�@�@�T@��@`B@/@��@�D@z�@9X@�@��@��@@�@�@��@^5@-@J@�@�#@��@x�@�^@G�@%@�@�@�@%@�@�@�@%@�`@�9@�@bN@A�@ �@b@  @�@��@�w@��@|�@\)@�@�y@��@v�@V@5?@@��@O�@�/@�@�D@�D@z�@I�@(�@�@��@�
@�F@t�@o@��@��@��@�!@��@~�@^5@-@��@��@x�@x�@x�@hs@7L@bN@  @�@��@�w@�@|�@l�@\)@
=@ȴ@V@�@@�h@�@`B@?}@��@�@�/@�D@I�@(�@�@��@�F@��@S�@@
�@
�H@
��@
~�@
^5@
-@
J@	��@	�#@	�^@	��@	�7@	X@	&�@	&�@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B9XB9XB9XB9XB9XB9XB9XB9XB9XB9XB9XB9XB8RB8RB:^B9XB:^B8RB1'B&�B)�B/B7LB9XB@�BL�B]/B_;B_;Bp�Bv�Bw�By�B}�B~�B}�B{�Bv�Bu�Bt�Bv�Bw�B}�B{�Bw�Bs�Bo�BgmBbNBaHBcTBhsBiyBYBN�B@�B?}BcTBgmBe`BiyBl�BbNB]/B\)BC�B(�B�B�B\B%B��B��B�HBɺB�qB�dB�B��B�VB}�Br�BffBQ�BF�BB�BA�BS�BC�B:^B49B!�BbB%BB
��B
�B
�fB
�)B
��B
ŢB
��B
�XB
��B
�7B
hsB
P�B
A�B
5?B
�B

=B
B	��B	�B	�BB	��B	ÖB	�FB	��B	�VB	�B	`BB	<jB	/B	&�B	�B	JB	%B	  B��B��B��B	+B	B��B�B�B�B�B�sB�NB�5B�B�
B��B��B��B��BǮBĜB��B�wB�^B�LB�FB�FB�9B�B��B��B��B�uB�oB��B�VB�=B�+B�%B�7B�1B�Bz�Bp�BW
BT�BN�BK�BG�BA�BD�BB�B>wB=qB8RB5?B1'B.B.B.B,B,B)�B)�B,B&�B&�B&�B'�B'�B&�B&�B%�B%�B%�B'�B'�B'�B)�B+B,B,B,B,B-B.B-B,B,B+B,B,B+B+B+B)�B+B+B)�B)�B)�B(�B)�B)�B-B,B,B-B.B/B-B-B,B-B.B,B,B.B.B-B.B.B.B/B/B0!B0!B1'B2-B5?B6FB6FB:^B=qB?}B?}B@�BC�BC�BE�BF�BI�BK�BL�BN�BO�BN�BR�BVBXBYBZB\)B]/B]/BaHBe`BffBhsBiyBl�Bp�Bw�B}�B~�B�B�B�B�B�B�B�+B�JB�hB��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�!B�'B�9B�FBBǮBǮB��B��B�B�#B�#B�)B�;B�HB�TB�`B�`B�sB�sB�B�B�B�B��B��B	B	+B	DB	PB	VB	bB	uB	�B	�B	�B	�B	�B	!�B	"�B	"�B	&�B	-B	1'B	2-B	33B	6FB	;dB	?}B	K�B	L�B	M�B	O�B	Q�B	VB	W
B	ZB	[#B	]/B	^5B	`BB	aHB	cTB	`BB	dZB	hsB	k�B	m�B	o�B	p�B	p�B	p�B	p�B	o�B	p�B	p�B	o�B	o�B	p�B	u�B	z�B	{�B	|�B	� B	�%B	�B	�B	�B	�DB	�JB	�PB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�LB	�XB	�XB	�dB	�dB	�jB	�wB	�wB	�}B	��B	��B	��B	B	B	ÖB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�5B	�;B	�;B	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B
  B
B
B
B
B
%B
%B
%B
%B
%B
+B
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
DB
DB
DB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
VB
\B
\B
\B
bB
bB
hB
hB
hB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
,B
,B
,B
-B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
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
49B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
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
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
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
D�B
D�B
D�B
D�B
D�B
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
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
H�B
I�B
I�B
I�B
J�B
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
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
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
W
B
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
YB
YB
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
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
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
`BB
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
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
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
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
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
gmB
ffB
gmB
gmB
gmB
gmB
hsB
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B:*B:DB:*B:DB:*B:*B:*B:*B:*B:*B:*B9$B9$B9$B9$B9$B9$B9>B9$B9$B9$B9$B9>B8B8B:*B9$B:*B88B0�B&�B)�B.�B7B9$B@OBL�B\�B_B_BpoBv�Bw�By�B}�B~�B}�B{�Bv�Bu�Bt�Bv�Bw�B}�B{�Bw�Bs�BoiBg8BbBaBc Bh>BiDBX�BN�B@OB?HBc Bg8Be,BiDBlWBbB\�B[�BCaB(�B�B_B(B�B��B��B�BɆB�<B�0B��B�SB�"B}�Br|Bf2BQ�BFtBB[BAUBS�BCaB:*B4B!�B.B�B�B
�B
�oB
�2B
��B
ΥB
�mB
�OB
�	B
��B
�B
h>B
P�B
AUB
5B
kB

	B
�B	��B	�IB	�B	͟B	�GB	�B	�kB	�B	��B	`B	<6B	.�B	&�B	xB	�B	�B��B��B��B��B	�B	�B��B�B�OB�WB�CB�$B�B�B��BּB��BѷB˒B�rB�_B�gB�4B�BB�*B�B��B��B�B��B��B�dB�eB�@B� B�_B�"B��B��B��B��B��B��Bz�BpUBV�BT�BN�BK�BGzBAUBDMBB[B>BB=<B8B5B0�B-�B-�B-�B+�B+�B)�B)�B+�B&�B&�B&�B'�B'�B&�B&�B%�B%�B%�B'�B'�B'�B)�B*�B+�B+�B+�B+�B,�B-�B,�B+�B+�B*�B+�B+�B*�B*�B*�B)�B*�B*�B)�B)�B)�B(�B)�B)�B,�B+�B+�B,�B-�B.�B,�B,�B+�B,�B-�B+�B+�B-�B-�B,�B-�B-�B-�B.�B.�B/�B/�B0�B1�B4�B5�B5�B:*B="B?HB?.B@4BCGBCGBEmBFYBI�BK�BL~BN�BO�BN�BR�BU�BW�BX�BY�B[�B\�B\�B`�BeBf2Bh$Bi*BlWBpUBw�B}�B~�B��B��B��B��B��B��B��B��B�4B�?B�QB�WB�WB�WB�]B�pB��B��B��B��B��B��B��B��B��B��B��B�AB�zB�_BʌBԯB��B��B��B��B�B��B�B�B�B�$B�>B�0B�IB�UB�aB�tB��B	�B	�B	
�B	B	B	B	&B	_B	9B	KB	dB	pB	!|B	"�B	"�B	&�B	,�B	0�B	1�B	2�B	5�B	;B	?HB	KxB	L~B	M�B	O�B	Q�B	U�B	V�B	Y�B	Z�B	\�B	]�B	_�B	`�B	cB	`B	dB	h$B	k6B	mCB	oOB	pUB	pUB	pUB	pUB	oOB	pUB	poB	oOB	oOB	pUB	utB	z�B	{�B	|�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�,B	�?B	�KB	�kB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	�$B	�B	�0B	�B	�(B	�(B	�HB	�;B	�;B	�;B	�[B	�AB	�aB	�SB	�_B	�fB	ɆB	ɆB	�rB	�rB	�~B	̈́B	ΊB	̈́B	̈́B	ΥB	ѝB	ѝB	ԯB	��B	յB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�8B	�>B	�$B	�DB	�*B	�0B	�0B	�6B	�=B	�CB	�IB	�IB	�IB	�OB	�OB	�OB	�UB	�[B	�aB	�aB	�aB	�aB	�aB	�aB	�hB	�B	�nB	�nB	�nB	�tB	�tB	�tB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

	B
	�B
	�B
	�B
	�B
B

�B

�B
B
�B
�B
�B
�B
�B
�B
B
B
B
B
B
(B
B
B
.B
B
4B
B
B
 B
 B
@B
&B
,B
,B
FB
2B
SB
SB
?B
?B
?B
EB
EB
eB
QB
QB
QB
QB
QB
QB
QB
kB
WB
WB
WB
]B
]B
~B
dB
dB
dB
pB
�B
pB
pB
 vB
!|B
!|B
!|B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
2�B
3�B
4B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
2�B
2�B
3�B
3�B
3�B
3�B
4B
4�B
5B
5�B
6�B
7B
6�B
6�B
8B
9	B
9	B
9	B
9	B
9	B
9	B
:*B
:B
:B
:*B
:B
;B
;B
;B
;B
;B
;B
;B
<6B
<B
=<B
="B
="B
="B
="B
>(B
?.B
?HB
?.B
?.B
?.B
?.B
?.B
@OB
@4B
@4B
@4B
@4B
@4B
@4B
@4B
@4B
AUB
A;B
AUB
A;B
A;B
A;B
BAB
B[B
B[B
CGB
CGB
CGB
DMB
DMB
DgB
DgB
DgB
DMB
DMB
DgB
DMB
DgB
DMB
DgB
DMB
ESB
EmB
ESB
ESB
ESB
ESB
ESB
FYB
FYB
FtB
FYB
FYB
GzB
GzB
G_B
HfB
H�B
I�B
IlB
IlB
H�B
I�B
IlB
IlB
J�B
L~B
L~B
L~B
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
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
\�B
\�B
\�B
]�B
]�B
^�B
_B
^�B
^�B
^�B
^�B
`B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
_�B
`�B
`�B
`�B
aB
`�B
bB
bB
a�B
a�B
bB
a�B
a�B
a�B
c B
c B
c B
cB
cB
cB
cB
cB
cB
c B
cB
cB
dB
dB
dB
d&B
d&B
eB
e,B
eB
eB
fB
fB
f2B
g8B
g8B
gB
gB
gB
gB
f2B
fB
g8B
fB
gB
gB
g8B
gB
h$B
h$B
h$B
h$B
h$B
i*B
i*B
i*B
iDB
j0B
j0B
k6B
lWB
lWB
l=B
lWB
l=B
lWB
l=B
m]B
mCB
mCB
m]B
mCB
m]B
nIB
nIB
nIB
nIB
nIB
ncB
nIB
ncB
nIB
nIB
nIB
ncB
oO11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.56(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809290039042018092900390420180929003904201809300036092018093000360920180930003609JA  ARFMdecpA19c                                                                20180923183521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180923093522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180923093523  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180923093524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180923093525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180923093525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180923093525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180923093525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180923093525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180923093525                      G�O�G�O�G�O�                JA  ARUP                                                                        20180923095609                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180923154123  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180928153904  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180928153904  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180929153609  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                