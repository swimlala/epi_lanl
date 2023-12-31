CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-12-21T09:40:21Z creation;2020-12-21T09:40:23Z conversion to V3.1;2023-06-29T05:47:40Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        <  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     <  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     <  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oX   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     <  s(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     <  �4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     <  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     <  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     <  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     <  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ڐ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201221094021  20230705041504  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0675_283                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�P��b:�1   @�P��b��@6S�����b㧆�&�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�fD�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @]p�@��@��A��A(��AH��Ah��A�z�A�z�A�z�A��A�z�A�z�A�z�A�z�B=qB
=qB=qB��B"=qB*=qB2=qB:=qBB=qBJ=qBR=qBZ=qBb��Bj=qBr=qBz=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �\C�\C�\C�\C�\C
�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2�\C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CL�\CN�\CP�\CR�\CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~�\C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9*=D9��D:#�D:��D;#�D;��D<#�D<��D=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI*=DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Du#�Du�qDv#�Dv��Dw#�Dw��Dx#�Dx��Dy#�Dy��Dz#�Dz��D{#�D{��D|#�D|��D}#�D}��D~#�D~��D#�D��D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D��D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�UD��D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D���D���D��D�Q�D�D���D��D�Q�DÑ�D���D��D�Q�Dđ�D���D��D�Q�Dő�D���D��D�Q�DƑ�D���D��D�Q�DǑ�D���D��D�Q�Dȑ�D���D��D�Q�Dɑ�D���D��D�Q�Dʑ�D���D��D�Q�Dˑ�D���D��D�Q�D̑�D���D��D�Q�D͑�D���D��D�Q�DΑ�D���D��D�Q�Dϑ�D���D��D�Q�DБ�D���D��D�Q�Dё�D���D��D�Q�Dґ�D���D��D�Q�Dӑ�D���D��D�Q�Dԑ�D���D��D�Q�DՑ�D���D��D�Q�D֑�D���D��D�Q�Dב�D���D��D�Q�Dؑ�D���D��D�Q�Dّ�D���D��D�Q�Dڑ�D���D��D�Q�Dۑ�D���D��D�Q�Dܑ�D���D��D�Q�Dݑ�D���D��D�Q�Dޑ�D���D��D�Q�Dߑ�D���D��D�Q�D���D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�Q�D��D���D��D�UD�RD�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�  A�A�1A�JA�oA�
=A�VA�{A��A��A��A�VA�A�1A��A�oA�  A��A��#A��A���A���A���A���A���A�AƼjAƺ^AƾwAƾwAƩ�AƅAēuA�?}A� �A�bA��HA���A���A��#A�I�A��^A�  A�ƨA��^A�Q�A��A�%A�t�A��A��FA�\)A��A� �A�33A��+A�M�A��\A�~�A��DA��A�ƨA�bNA��\A���A�I�A�1'A�z�A�"�A��!A���A�A�A���A�K�A��`A���A�oA��!A���A�1A��-A��A�$�A�~�A� �A���A�&�A���A�Q�A��A���A�K�A��#A���A�/A���A�ZA���A��;A�%A��hA�t�AK�A}��AzM�AuG�Aq�7An=qAjbNAh^5Ad$�A`�A_%A]��A]/A[�mAZ�DAW?}AU33AS�AQ\)AO��ANjAK�AJ��AJ�AHQ�AGC�AFbNAEC�AB�HAAA?;dA=�TA;�A:�\A8ffA6��A6E�A5��A4��A3��A3K�A2$�A1�A0�A.��A-C�A,VA+��A+�A*�/A*bNA)��A(��A&��A%�A%K�A%�A$�A#�TA"�A!��A ffAVAv�Al�Av�AhsA{A��AE�AA`BA��An�A%A��A\)A"�A~�A��A7LA��A5?AA
=A	G�Av�A�FA\)A��A�A;dA�A�A��A�`A  A�7A�9A1'A �`A ��A jA   @�$�@�G�@�  @��H@��@���@�E�@�1'@�M�@�?}@��@���@���@���@�?}@��;@���@�J@�9@�P@旍@�7L@�l�@◍@�7L@��@���@�?}@� �@���@��@��/@��y@���@�x�@� �@�;d@�M�@�/@�r�@�ȴ@Ͳ-@�hs@�X@�V@˾w@�K�@�@�7L@���@�r�@�Z@ǥ�@�5?@ļj@��@�\)@���@���@���@���@���@��@�n�@�E�@��@��`@��@�M�@��h@��D@�Q�@�1'@�K�@�V@�@��@�(�@��P@�33@���@��@�@��^@��7@��j@�\)@�33@��!@���@�p�@�?}@��j@���@�C�@���@��#@�X@��D@�A�@��@�\)@�
=@�~�@�$�@��#@���@�/@���@��u@�bN@�9X@��F@���@�K�@��R@���@���@�ff@��-@�/@��@��9@���@���@�j@�bN@�bN@�1'@��
@���@�dZ@���@��!@�~�@���@���@��@���@���@��h@���@���@��h@��7@�O�@���@�ƨ@�dZ@��y@��+@��R@��@��\@�-@�{@��@���@�7L@��D@��@��@�l�@�\)@�"�@�~�@�-@��@��7@�hs@�X@�X@��h@�p�@��@�(�@���@���@��w@��w@��@��@�C�@���@��R@���@�~�@�5?@��#@�@�x�@�?}@�%@��@��`@��/@���@���@��D@�b@���@�l�@�C�@�+@��@�o@���@��y@�ȴ@��!@���@���@��+@�{@��T@���@���@�@���@�?}@�%@���@��@��@��/@�Ĝ@��@�Z@�b@���@���@�t�@�o@��y@���@���@�v�@�E�@�5?@��@��#@�@���@���@�`B@�&�@��j@�I�@��m@��F@���@�l�@�S�@�33@�@��!@��+@�$�@��@��#@��h@��@�G�@���@��`@���@��j@��@�Z@��@��;@��@��P@�\)@���@���@���@��\@�5?@��@��T@���@��7@�X@��@��j@��u@�I�@��@|�@�@~E�@}p�@|�@|j@{��@{��@z�@z^5@y��@y�@x��@xQ�@x  @w�@w\)@vff@u��@u?}@t��@t�@t��@tz�@t9X@t1@sƨ@s��@sS�@s"�@s@s@r�H@rM�@q�#@q��@q%@pĜ@p�9@pr�@pbN@pA�@o�@o\)@n�@nV@n$�@m�T@m�h@mO�@m�@l�/@lz�@lZ@l1@k��@ko@jM�@j-@j=q@j=q@jM�@jn�@jn�@i�@ix�@i7L@i7L@i&�@i%@h�9@h��@h�9@h�@hA�@h  @g�@g��@g;d@fV@f5?@f@e��@e�h@e/@d�j@d�D@dz�@dj@d(�@cƨ@c��@b��@b^5@b=q@b-@a��@a�^@a��@a�^@ahs@a&�@aG�@a%@`�u@`�@`A�@`b@_��@_��@_�P@_l�@_\)@_;d@^��@^v�@]�@]@]?}@]�@]V@\�j@\j@\9X@[��@[��@[t�@[dZ@[C�@[o@Z�H@Z��@Zn�@ZJ@Y��@Y�7@Y&�@Y%@X��@Xr�@W�;@Wl�@WK�@W
=@V�y@V�@Vȴ@Vȴ@V��@Vv�@VE�@U��@U?}@T�/@T��@TI�@T�@S��@SS�@So@R��@R�\@RM�@RJ@Q��@QX@Q%@PĜ@PQ�@Pb@O�@O�w@O�@O�@O�P@O
=@N��@Nv�@N{@M?}@L�@L�@LI�@L1@K�@J�@J~�@JM�@I��@I�^@IX@H�`@HbN@H �@G�w@G�@G;d@F��@Fv�@F$�@F@E��@E�-@E�-@E��@E/@D�@D�@Dj@D1@C�m@C�
@Cƨ@Cƨ@C��@C��@CS�@C33@B�\@B�\@B=q@A�@A�^@AX@@��@@��@@1'@?��@?;d@>��@>ff@>E�@>@=��@=�h@<�j@<(�@;��@;��@;�@;dZ@;33@:�@:�\@9��@9��@9�7@9hs@9G�@9&�@9%@8��@8Q�@8  @7��@7|�@7\)@7K�@6��@6��@6V@5�-@5�h@5`B@4�/@4��@41@3��@3dZ@3C�@2�@2�\@2^5@2M�@2-@1��@17L@1�@1%@1%@0Ĝ@0Q�@01'@0b@/�@/�w@/��@/��@/|�@/;d@.��@.v�@.V@.E�@-�@,�@,�D@,�@+�
@+��@+�@+dZ@+C�@+o@*��@*�\@*~�@*n�@*-@)�^@)x�@)G�@(��@(�u@(1'@'�;@'�P@'l�@'\)@'�@&�y@&�R@&ff@&E�@&$�@&@%�@%�T@%�-@%�h@%p�@%`B@%O�@$�@$�@$z�@$I�@$(�@$1@#�
@#�F@#�F@#��@#t�@#S�@#33@#@"��@"^5@"=q@!�@!��@!�^@!�7@!X@!�@ ��@ �@ bN@ Q�@ 1'@�@�@�@   @   @   @   @�@��@�w@�@�P@l�@
=@��@E�@$�@�@��@��@�@V@�@j@I�@1@��@��@�m@��@S�@��@~�@n�@-@J@J@�#@X@&�@Ĝ@��@A�@b@�w@\)@�@
=@
=@
=@��@��@�R@$�@p�@?}@�@��@�@�@�/@��@I�@(�@�@�m@ƨ@��@t�@S�@33@@�@�!@~�@=q@�@��@��@x�@G�@7L@&�@&�@�@�@%@%@��@��@�9@��@r�@Q�@1'@�;@�@�P@|�@l�@\)@\)@\)@;d@��@�R@v�@E�@5?@$�@{@��@�h@O�@�@V@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�  A�A�1A�JA�oA�
=A�VA�{A��A��A��A�VA�A�1A��A�oA�  A��A��#A��A���A���A���A���A���A�AƼjAƺ^AƾwAƾwAƩ�AƅAēuA�?}A� �A�bA��HA���A���A��#A�I�A��^A�  A�ƨA��^A�Q�A��A�%A�t�A��A��FA�\)A��A� �A�33A��+A�M�A��\A�~�A��DA��A�ƨA�bNA��\A���A�I�A�1'A�z�A�"�A��!A���A�A�A���A�K�A��`A���A�oA��!A���A�1A��-A��A�$�A�~�A� �A���A�&�A���A�Q�A��A���A�K�A��#A���A�/A���A�ZA���A��;A�%A��hA�t�AK�A}��AzM�AuG�Aq�7An=qAjbNAh^5Ad$�A`�A_%A]��A]/A[�mAZ�DAW?}AU33AS�AQ\)AO��ANjAK�AJ��AJ�AHQ�AGC�AFbNAEC�AB�HAAA?;dA=�TA;�A:�\A8ffA6��A6E�A5��A4��A3��A3K�A2$�A1�A0�A.��A-C�A,VA+��A+�A*�/A*bNA)��A(��A&��A%�A%K�A%�A$�A#�TA"�A!��A ffAVAv�Al�Av�AhsA{A��AE�AA`BA��An�A%A��A\)A"�A~�A��A7LA��A5?AA
=A	G�Av�A�FA\)A��A�A;dA�A�A��A�`A  A�7A�9A1'A �`A ��A jA   @�$�@�G�@�  @��H@��@���@�E�@�1'@�M�@�?}@��@���@���@���@�?}@��;@���@�J@�9@�P@旍@�7L@�l�@◍@�7L@��@���@�?}@� �@���@��@��/@��y@���@�x�@� �@�;d@�M�@�/@�r�@�ȴ@Ͳ-@�hs@�X@�V@˾w@�K�@�@�7L@���@�r�@�Z@ǥ�@�5?@ļj@��@�\)@���@���@���@���@���@��@�n�@�E�@��@��`@��@�M�@��h@��D@�Q�@�1'@�K�@�V@�@��@�(�@��P@�33@���@��@�@��^@��7@��j@�\)@�33@��!@���@�p�@�?}@��j@���@�C�@���@��#@�X@��D@�A�@��@�\)@�
=@�~�@�$�@��#@���@�/@���@��u@�bN@�9X@��F@���@�K�@��R@���@���@�ff@��-@�/@��@��9@���@���@�j@�bN@�bN@�1'@��
@���@�dZ@���@��!@�~�@���@���@��@���@���@��h@���@���@��h@��7@�O�@���@�ƨ@�dZ@��y@��+@��R@��@��\@�-@�{@��@���@�7L@��D@��@��@�l�@�\)@�"�@�~�@�-@��@��7@�hs@�X@�X@��h@�p�@��@�(�@���@���@��w@��w@��@��@�C�@���@��R@���@�~�@�5?@��#@�@�x�@�?}@�%@��@��`@��/@���@���@��D@�b@���@�l�@�C�@�+@��@�o@���@��y@�ȴ@��!@���@���@��+@�{@��T@���@���@�@���@�?}@�%@���@��@��@��/@�Ĝ@��@�Z@�b@���@���@�t�@�o@��y@���@���@�v�@�E�@�5?@��@��#@�@���@���@�`B@�&�@��j@�I�@��m@��F@���@�l�@�S�@�33@�@��!@��+@�$�@��@��#@��h@��@�G�@���@��`@���@��j@��@�Z@��@��;@��@��P@�\)@���@���@���@��\@�5?@��@��T@���@��7@�X@��@��j@��u@�I�@��@|�@�@~E�@}p�@|�@|j@{��@{��@z�@z^5@y��@y�@x��@xQ�@x  @w�@w\)@vff@u��@u?}@t��@t�@t��@tz�@t9X@t1@sƨ@s��@sS�@s"�@s@s@r�H@rM�@q�#@q��@q%@pĜ@p�9@pr�@pbN@pA�@o�@o\)@n�@nV@n$�@m�T@m�h@mO�@m�@l�/@lz�@lZ@l1@k��@ko@jM�@j-@j=q@j=q@jM�@jn�@jn�@i�@ix�@i7L@i7L@i&�@i%@h�9@h��@h�9@h�@hA�@h  @g�@g��@g;d@fV@f5?@f@e��@e�h@e/@d�j@d�D@dz�@dj@d(�@cƨ@c��@b��@b^5@b=q@b-@a��@a�^@a��@a�^@ahs@a&�@aG�@a%@`�u@`�@`A�@`b@_��@_��@_�P@_l�@_\)@_;d@^��@^v�@]�@]@]?}@]�@]V@\�j@\j@\9X@[��@[��@[t�@[dZ@[C�@[o@Z�H@Z��@Zn�@ZJ@Y��@Y�7@Y&�@Y%@X��@Xr�@W�;@Wl�@WK�@W
=@V�y@V�@Vȴ@Vȴ@V��@Vv�@VE�@U��@U?}@T�/@T��@TI�@T�@S��@SS�@So@R��@R�\@RM�@RJ@Q��@QX@Q%@PĜ@PQ�@Pb@O�@O�w@O�@O�@O�P@O
=@N��@Nv�@N{@M?}@L�@L�@LI�@L1@K�@J�@J~�@JM�@I��@I�^@IX@H�`@HbN@H �@G�w@G�@G;d@F��@Fv�@F$�@F@E��@E�-@E�-@E��@E/@D�@D�@Dj@D1@C�m@C�
@Cƨ@Cƨ@C��@C��@CS�@C33@B�\@B�\@B=q@A�@A�^@AX@@��@@��@@1'@?��@?;d@>��@>ff@>E�@>@=��@=�h@<�j@<(�@;��@;��@;�@;dZ@;33@:�@:�\@9��@9��@9�7@9hs@9G�@9&�@9%@8��@8Q�@8  @7��@7|�@7\)@7K�@6��@6��@6V@5�-@5�h@5`B@4�/@4��@41@3��@3dZ@3C�@2�@2�\@2^5@2M�@2-@1��@17L@1�@1%@1%@0Ĝ@0Q�@01'@0b@/�@/�w@/��@/��@/|�@/;d@.��@.v�@.V@.E�@-�@,�@,�D@,�@+�
@+��@+�@+dZ@+C�@+o@*��@*�\@*~�@*n�@*-@)�^@)x�@)G�@(��@(�u@(1'@'�;@'�P@'l�@'\)@'�@&�y@&�R@&ff@&E�@&$�@&@%�@%�T@%�-@%�h@%p�@%`B@%O�@$�@$�@$z�@$I�@$(�@$1@#�
@#�F@#�F@#��@#t�@#S�@#33@#@"��@"^5@"=q@!�@!��@!�^@!�7@!X@!�@ ��@ �@ bN@ Q�@ 1'@�@�@�@   @   @   @   @�@��@�w@�@�P@l�@
=@��@E�@$�@�@��@��@�@V@�@j@I�@1@��@��@�m@��@S�@��@~�@n�@-@J@J@�#@X@&�@Ĝ@��@A�@b@�w@\)@�@
=@
=@
=@��@��@�R@$�@p�@?}@�@��@�@�@�/@��@I�@(�@�@�m@ƨ@��@t�@S�@33@@�@�!@~�@=q@�@��@��@x�@G�@7L@&�@&�@�@�@%@%@��@��@�9@��@r�@Q�@1'@�;@�@�P@|�@l�@\)@\)@\)@;d@��@�R@v�@E�@5?@$�@{@��@�h@O�@�@V@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�TB�TB�TB�NB�TB�NB�NB�NB�NB�NB�TB�TB�HB�HB�HB�;B�HB�;B�BB�/B�/B�5B�5B�HB�HB�HB�HB�NB�TB�ZB�`B�mB�B	7B�BYB� B� Bu�BgmBS�BT�BQ�BO�BN�BM�BL�BW
BT�BW
BS�BP�BM�BJ�BF�BA�B=qB;dB5?B-B!�B,B5?B;dB:^B8RB2-B&�B�B�B�B�BbB��B�B�fB�HB�BɺB�jB�!B�VBr�Bl�B`BBYBM�BE�B6FB�BuB\B	7B
�mB
�5B
�
B
ŢB
�LB
�B
��B
u�B
aHB
L�B
=qB
.B
�B	�B	��B	�jB	��B	�VB	t�B	ZB	P�B	F�B	B�B	:^B	1'B	�B	bB	B��B�B�yB�5B�B��B��BŢB��B�dB�9B��B��B��B��B�bB�bB�1B�+B�B�B~�B|�Bz�Bw�Bu�Br�BiyBhsBgmBffBe`Be`BbNBaHB\)B[#BYBXBXBVBT�BS�BS�BP�BO�BP�BM�BK�BL�BI�BI�BK�BG�BD�BA�B>wB;dB;dB;dB<jB;dB:^B;dB;dB=qB;dB6FB2-B2-B2-B1'B5?B<jBH�BK�BI�BL�BK�BK�BI�BI�BG�BF�BF�BE�BF�BH�BF�BC�BD�BE�BL�BL�BJ�BI�BK�BI�BJ�BK�BN�BT�BW
BYBYBZBYB[#BZB[#BYBXBYBXBYBYBXBYBYBYBZBZB[#B\)B_;BbNBcTBdZBcTBcTBe`BhsBjBo�Bq�Br�Br�Bq�Bs�Bv�Bv�Bv�Bw�Bz�B{�B{�B|�B� B�B�B�B�B�DB�\B�\B�\B�oB��B��B��B��B��B��B�B�B�!B�-B�9B�?B�?B�?B�XB�}B��B��BÖBƨBȴB��B��B��B�B�B�B�HB�HB�`B�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	%B	%B	%B	+B	PB	\B	hB	�B	�B	�B	 �B	 �B	 �B	!�B	(�B	+B	-B	.B	/B	0!B	33B	7LB	9XB	<jB	A�B	B�B	B�B	C�B	D�B	E�B	H�B	J�B	L�B	M�B	N�B	N�B	Q�B	T�B	T�B	T�B	W
B	[#B	]/B	^5B	aHB	bNB	dZB	gmB	hsB	k�B	p�B	s�B	v�B	{�B	}�B	� B	�B	�B	�1B	�1B	�%B	�+B	�+B	�1B	�=B	�DB	�DB	�PB	�bB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�3B	�LB	�RB	�RB	�RB	�XB	�^B	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	ÖB	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�ZB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
+B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
DB
DB
DB
JB
JB
PB
PB
PB
PB
VB
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
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
!�B
"�B
"�B
#�B
$�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
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
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
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
33B
33B
49B
49B
49B
5?B
5?B
6FB
5?B
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
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
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
A�B
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
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
G�B
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
I�B
J�B
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
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
R�B
S�B
R�B
S�B
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
XB
XB
XB
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
ZB
ZB
ZB
ZB
[#B
[#B
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
ZB
ZB
[#B
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
]/B
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
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
dZB
e`B
e`B
ffB
ffB
ffB
ffB
gmB
ffB
ffB
gmB
hsB
hsB
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
o�B
o�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�:B� B� B�B� B�B�B�B�B�B� B�:B�-B�-B�-B��B�-B�!B�'B�B��B�B�B�B�-B�B�-B�B� B�ZB��B��B��B�B 'BYKB��B��B~BBoOBZBZ�BS&BP.BOBBO\BPbBZkBW�BX�BUgBQ�BO(BNpBH�BCGB?�B=B72B.cB!|B,B6+B<�B;�B9�B49B(>B vBpB�B�B�B�6B�IB�B�NB�$B��B��B�TB��Bs�Bm�Ba-BZ7BO\BHKB9$B�B,BNBjB
�>B
ߤB
�eB
�EB
��B
��B
�eB
x�B
c�B
N�B
@B
2aB
B	�B	�B	��B	��B	�&B	x�B	[�B	R:B	GzB	D3B	<�B	4�B	 BB	�B	%B��B�B�=B��B�eB��B�B��B�AB�B�`B�B��B�;B�B��B��B�B��B��B�3B�B~(B{�By	BxBt9BjBiBh$Bf�BfBfLBc�Bc B]B[�BYKBX�BYBWYBV9BU�BUgBQ�BQ4BR BO(BM6BM�BJ�BKxBM�BIlBFtBCB?�B;�B;�B<6B=VB;�B:�B;�B<B>�B="B7B2�B2aB2GB1B4�B<6BIRBMBJ�BM�BLdBL�BJrBJ�BG�BF�BG+BF�BG+BI7BGBC{BD�BF%BNBM�BK^BJ�BL�BJXBK�BL�BO�BU�BW�BY�BY�BZ�BZB[�BZ�B[�BY�BX�BY�BX�BY�BY�BX�BZBYBYBZ�BZ�B[�B\�B_�Bc Bc�BdZBc:Bc�Be�Bh�Bk6Bo�Bq�Br�Br�BrBtnBwfBwBv�BxBz�B|jB|PB}qB�OB�B�B�'B��B��B��B��B��B�oB��B�	B�B��B�TB�LB�"B�B�;B�aB�9B�?B�ZB��B��B�cB��B��BðBƨB��B�BB�,B�2B�SB�9B�QB�HB�bB�zB�B��B��B�B�B��B��B��B��B��B�B��B	-B	-B	�B	�B	%B	_B	jB	BB	NB	gB	kB	�B	 �B	 �B	 �B	!�B	(�B	+B	-B	.B	/B	0UB	2�B	72B	9>B	<PB	A;B	BAB	B[B	CaB	D�B	E�B	H�B	K)B	L�B	M�B	N�B	N�B	Q�B	T�B	UB	T�B	V�B	[#B	]IB	^jB	aHB	bNB	d@B	g8B	hsB	k�B	p�B	s�B	v�B	{�B	}�B	�B	��B	�B	�fB	��B	�B	��B	��B	��B	�	B	�DB	�)B	�6B	�HB	�NB	�hB	�TB	�aB	�YB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	��B	��B	��B	��B	��B	�MB	�B	�B	�B	�B	�>B	�DB	�<B	�BB	�HB	�.B	�HB	�OB	�iB	��B	�{B	ňB	ȀB	ɠB	��B	̘B	͟B	͹B	��B	��B	бB	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�B	�B	�B	�B	�B	�BB	�HB	�4B	�NB	�@B	�&B	�`B	�2B	�LB	�RB	�>B	�DB	�_B	�yB	�eB	�eB	�kB	�qB	�qB	�B	�B	�iB	�oB	��B	�B	�B	�vB	�B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
�B
�B
B
�B
3B
B
%B
B
�B
B
�B
�B
�B
�B
B
�B
	B
	B
	B
	B
	7B
DB
B
)B
B
B
B
6B
B
6B
<B
<B
BB
(B
(B
.B
.B
4B
4B
4B
:B
TB
TB
[B
aB
2B
MB
2B
2B
9B
SB
sB
�B
kB
WB
qB
qB
qB
qB
]B
�B
�B
xB
xB
�B
�B
�B
qB
qB
xB
�B
�B
�B
~B
~B
~B
~B
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
"�B
#�B
$�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
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
)�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
/ B
/ B
.�B
/ B
/�B
/�B
/�B
/�B
/�B
/�B
0B
1B
1B
0�B
2B
1�B
1�B
2B
3B
2�B
2�B
2�B
3B
4B
4B
4B
5B
5%B
6B
5%B
6B
5�B
5�B
6B
6+B
72B
7B
72B
7fB
8B
9$B
9>B
9$B
9>B
:DB
:^B
;0B
;0B
<PB
<PB
<jB
<PB
=<B
=<B
=VB
=VB
>]B
>BB
>BB
?cB
?HB
?HB
?.B
?HB
?cB
@OB
@OB
@iB
AUB
AUB
AUB
A;B
A;B
AUB
AUB
AoB
AUB
BuB
B[B
BuB
CaB
CaB
C{B
C{B
CaB
C{B
D�B
D�B
D�B
EmB
EmB
E�B
EmB
E�B
E�B
G�B
GzB
GzB
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
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
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
R�B
S�B
SB
S�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
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
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Y�B
Y�B
ZB
ZB
Z�B
Z�B
Y�B
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Y�B
Y�B
ZB
Y�B
Y�B
Z�B
Z�B
ZB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
]B
\�B
\�B
^B
^B
^B
^5B
^B
^B
^B
^B
^B
^B
^B
_!B
_B
_!B
_B
_B
^�B
_B
_!B
`B
aHB
aB
a-B
aB
a�B
a�B
bB
b4B
c:B
c:B
c B
c B
d&B
d&B
d@B
d&B
eB
eB
eB
e,B
e,B
d@B
eFB
eFB
f2B
f2B
f2B
f2B
gB
f2B
fLB
g8B
hXB
h>B
hXB
hXB
h>B
h>B
h>B
hXB
h>B
iDB
iDB
iDB
iDB
jKB
jKB
jKB
jeB
kQB
k6B
k6B
k6B
k6B
kQB
kQB
k6B
k6B
kkB
kQB
lWB
lWB
lWB
lWB
lqB
lWB
mCB
mCB
mCB
m]B
mCB
mCB
m]B
mwB
m]B
m]B
n}B
ncB
nIB
ncB
n}B
ncB
ncB
oiB
oOB
oOB
oOB
oO111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.56(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202012260035552020122600355520201226003555202306231726402023062317264020230623172640202012270029412020122700294120201227002941  JA  ARFMdecpA19c                                                                20201221184007  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201221094021  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20201221094022  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201221094022  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201221094022  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20201221094022  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201221094022  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20201221094022  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20201221094023  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201221094023                      G�O�G�O�G�O�                JA  ARUP                                                                        20201221095229                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20201221153326  CV  JULD            G�O�G�O�Fʄg                JM  ARCAJMQC2.0                                                                 20201225153555  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201225153555  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20201226152941  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082640  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041504                      G�O�G�O�G�O�                