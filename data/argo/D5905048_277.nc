CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-02T00:35:23Z creation;2018-09-02T00:35:27Z conversion to V3.1;2019-12-19T07:29:49Z update;     
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180902003523  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_277                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�~67�� 1   @�~7'�} @4���v��da^��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~fD~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�Ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@s33@�ff@�ffA33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Ds3D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D's3D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dw��Dxl�Dx��Dyl�Dy��Dzl�Dz��D{l�D{��D|l�D|��D}l�D}�3D~l�D~��Dl�D��D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD¶fD��fD�6fD�vfDöfD��fD�6fD�vfDĶfD��fD�6fD�vfDŶfD��fD�6fD�vfDƶfD��fD�6fD�vfDǶfD��fD�6fD�vfDȶfD��fD�6fD�vfDɶfD��fD�6fD�vfDʶfD��fD�6fD�vfD˶fD��fD�6fD�vfD̶fD��fD�6fD�vfDͶfD��fD�6fD�vfDζfD��fD�6fD�vfD϶fD��fD�6fD�vfDжfD��fD�6fD�vfDѶfD��fD�6fD�vfDҶfD��fD�6fD�vfDӶfD��fD�6fD�vfDԶfD��fD�6fD�vfDնfD��fD�6fD�vfDֶfD��fD�6fD�vfD׶fD��fD�6fD�vfDضfD��fD�6fD�vfDٶfD��fD�6fD�vfDڶfD��fD�6fD�vfD۶fD��fD�6fD�vfDܶfD��fD�6fD�vfDݶfD��fD�6fD�vfD޶fD��fD�6fD�vfD߶fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD���D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�`BA�bNA�\)A�ZA�S�A�XA�ZA�S�A�O�A�O�A�Q�A�Q�A�S�A�S�A�VA�S�A�Q�A�Q�A�Q�A�M�A�=qA���AܶFA�$�Aۏ\A�I�A�bAڃA���A�jA�oA�v�A�VA��A���A��/A׉7A��A���A�?}A��Aԛ�A�ȴA��
A�/A��A�bNA̍PA��mA��A�ZAǾwAƕ�A��A��mAŕ�A��
A�+Aò-AÏ\A�VA���A�Q�A�oA���A�ZA��#A���A���A�ĜA�bNA�t�A��A��TA���A�p�A��mA��A�(�A�;dA�A�+A��#A�Q�A���A��A��FA��PA��A���A���A�p�A��+A�ZA�33A��A��mA��`A�5?A�E�A���A��A��^A��hA��A��`A�^5A� �A��A�~�A�$�A��uA���A��jA�JA33A}x�Az�uAxAt�RAqAn(�AiAf^5Ac�A`��A^v�A\�yAZ�AY�AYG�AV��AS��AR(�AQ�AP�\AOƨAOS�ANbNAK�
AH��AG�TAFz�ADI�AB��AA�A@�!A=�A:�A9VA7�A6$�A4Q�A2��A21A0I�A/��A-C�A,ffA,5?A+x�A*��A*A�A*M�A*VA*Q�A*E�A*A�A*�A)�
A)��A)+A((�A'dZA&�A$v�A!�#A�wA��A/A\)A��A
=A�^Av�A�A�#AG�A;dA7LA&�A��A$�A�RAS�A�A�A��A
$�A�`A$�A��Ax�A7LA�`A��A1'A�`AC�A^5A��A E�@��@��@���@�`B@�(�@��P@�\)@�{@���@�@��R@�~�@�^5@�5?@��^@���@� �@�%@��@�1@�@�\)@�
=@�h@��@�^5@�\@@�v�@�@��T@�{@�$�@�$�@�J@��@���@홚@�b@���@�l�@���@�5?@��@��@�w@�"�@�~�@���@���@�(�@��
@�;d@���@�{@��/@ڧ�@�I�@љ�@���@д9@�z�@�(�@�  @Ϯ@�ff@�(�@˥�@��@��@�1@��@�=q@���@�p�@�+@�`B@�j@��@�l�@�ƨ@��@�|�@�bN@ļj@��
@�+@��@���@���@�z�@��;@�-@�V@��D@��@���@�G�@���@�dZ@��@�v�@��+@�M�@��9@���@�V@�I�@��@��@�^5@�5?@�5?@�^5@���@��R@���@���@�ȴ@��\@���@��!@�=q@��@��-@�x�@�%@�Z@�M�@�`B@�X@�V@���@��@��@�Z@�A�@�9X@�9X@�(�@��@�C�@��H@�@���@��@��9@�A�@�A�@�9X@�(�@��@�I�@�j@�Z@��m@�S�@���@�M�@��@�@�7L@��@���@�j@��@�ƨ@��@�dZ@�"�@�@���@���@�^5@�5?@�@�hs@�7L@��@���@�z�@�Z@�1@��;@��F@��@���@���@���@���@�|�@�S�@���@�^5@�-@���@��T@��^@��h@�`B@��@�V@��`@�Ĝ@���@�I�@�  @��w@���@�t�@�C�@��@�@���@��@�ȴ@�^5@��T@���@�`B@��@��/@��@��@�j@�A�@�1'@��@���@�K�@��@��@��R@��+@�-@��@��7@�?}@���@�z�@�Q�@�A�@� �@��
@��P@�+@��y@��!@�ff@��#@��-@��@�X@�&�@���@���@��@�z�@�9X@�  @���@�\)@�S�@�S�@�o@��y@�n�@�=q@���@���@���@���@��^@�x�@�V@���@� �@�  @���@�@��H@��!@��+@�n�@�E�@�$�@���@���@�`B@�O�@�?}@�%@���@��D@�1'@�b@���@���@���@�t�@�dZ@�+@���@���@���@�=q@��^@�hs@�V@���@���@�Q�@�;@�w@��@K�@~�R@~$�@}�-@|�j@|�D@|(�@z�@zn�@z=q@z�@z�\@z-@y��@yhs@xĜ@x�u@x  @w�@v��@v�R@w�@wK�@w+@w�P@w|�@v��@vff@u@t��@t(�@t�@s��@s�m@s�m@st�@s33@r��@r^5@r=q@r�@q�@qx�@qG�@p  @o\)@n��@n5?@m�@l��@lZ@l1@k��@j��@jM�@i�7@i&�@i%@h�`@h �@g�P@g\)@gK�@gK�@g;d@f��@f�R@f�+@e�@e?}@d�@d��@d�@d�D@dZ@d9X@d�@c�
@c�F@cdZ@co@b�@b��@b�\@b^5@b=q@a��@aG�@`��@`r�@`Q�@`A�@`A�@_�@^v�@]��@]/@]V@\�/@\�D@[��@[o@Z�H@Y��@Y��@Y�7@Yhs@YG�@Y�@X�9@Xr�@X �@X  @X  @X  @W�@W�P@V��@V��@VE�@V@U�h@U`B@UV@T��@S��@S��@SC�@R�\@R^5@R^5@R=q@Q��@Q��@Q��@Q&�@P�`@P�9@P�u@Pr�@PbN@PQ�@PQ�@PA�@P  @O|�@O\)@O+@O�@O
=@O
=@O
=@N��@N�+@N5?@N$�@M�-@M/@L�j@Lj@LI�@L9X@L(�@L�@K��@K��@K��@KdZ@KC�@Ko@J^5@I��@I�7@IG�@I&�@H�`@H�9@H�9@H�u@Hr�@HbN@HA�@H �@G�P@G+@Fȴ@FV@F5?@E�@E�-@E�@EO�@EO�@E?}@D��@DZ@D1@C�m@C�
@C��@CC�@C@B�\@Bn�@B=q@B�@A��@A��@Ax�@AG�@A%@A%@@��@@�u@@r�@@bN@?��@?�w@?�@?��@?+@>ff@>5?@>5?@>{@=��@=?}@<�j@<�D@<I�@;ƨ@;S�@:�H@:��@:��@:�!@:~�@:n�@:=q@9�7@9�@8��@8Q�@7�@7l�@6�y@6�R@6V@65?@6@5@5O�@4�/@3�m@3C�@2�!@2�\@2~�@2n�@2^5@2M�@1��@1&�@1�@1�@1%@0��@0�`@0�`@0Ĝ@0�@/�@/�P@/\)@/K�@/�@.�y@.ȴ@.�+@.5?@.@-��@-�@,�/@,��@,9X@,1@+ƨ@+��@+t�@+o@*�!@*�\@*^5@)�@)��@)hs@)G�@)&�@(�u@(1'@'�@'�;@'��@'�w@'�P@'\)@';d@'+@&��@&�R@&�+@&ff@&E�@&{@&@%�@%�-@%�h@%�@%p�@%?}@$�D@$I�@$1@$1@#�m@#�F@#t�@#33@#o@"�@"��@"�!@"��@"��@"=q@!��@!��@!�7@!x�@!X@!7L@!&�@!&�@!�@ ��@ ��@ Ĝ@ r�@ bN@   @�w@�@�@��@��@�P@|�@|�@|�@l�@l�@\)@K�@K�@�@��@�@��@$�@@��@��@`B@/@��@��@�@�@�@��@��@�D@j@9X@(�@1@ƨ@�F@��@C�@"�@�@�!@n�@^5@M�@�#@�^@��@7L@&�@&�@�@�@%@��@Ĝ@Ĝ@Ĝ@�9@�u@1'@  @�;@��@�P@|�@K�@
=@�y@�R@E�@5?@$�@�@�@�@�T@�-@p�@�@�j@��@ƨ@t�@o@��@=q@-@-@�@�#@�^@��@�7@hs@G�@7L@�@%@�`@�u@bN@Q�@b@��@�@�@��@l�@\)@\)@K�@;d@�y@��@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�`BA�bNA�\)A�ZA�S�A�XA�ZA�S�A�O�A�O�A�Q�A�Q�A�S�A�S�A�VA�S�A�Q�A�Q�A�Q�A�M�A�=qA���AܶFA�$�Aۏ\A�I�A�bAڃA���A�jA�oA�v�A�VA��A���A��/A׉7A��A���A�?}A��Aԛ�A�ȴA��
A�/A��A�bNA̍PA��mA��A�ZAǾwAƕ�A��A��mAŕ�A��
A�+Aò-AÏ\A�VA���A�Q�A�oA���A�ZA��#A���A���A�ĜA�bNA�t�A��A��TA���A�p�A��mA��A�(�A�;dA�A�+A��#A�Q�A���A��A��FA��PA��A���A���A�p�A��+A�ZA�33A��A��mA��`A�5?A�E�A���A��A��^A��hA��A��`A�^5A� �A��A�~�A�$�A��uA���A��jA�JA33A}x�Az�uAxAt�RAqAn(�AiAf^5Ac�A`��A^v�A\�yAZ�AY�AYG�AV��AS��AR(�AQ�AP�\AOƨAOS�ANbNAK�
AH��AG�TAFz�ADI�AB��AA�A@�!A=�A:�A9VA7�A6$�A4Q�A2��A21A0I�A/��A-C�A,ffA,5?A+x�A*��A*A�A*M�A*VA*Q�A*E�A*A�A*�A)�
A)��A)+A((�A'dZA&�A$v�A!�#A�wA��A/A\)A��A
=A�^Av�A�A�#AG�A;dA7LA&�A��A$�A�RAS�A�A�A��A
$�A�`A$�A��Ax�A7LA�`A��A1'A�`AC�A^5A��A E�@��@��@���@�`B@�(�@��P@�\)@�{@���@�@��R@�~�@�^5@�5?@��^@���@� �@�%@��@�1@�@�\)@�
=@�h@��@�^5@�\@@�v�@�@��T@�{@�$�@�$�@�J@��@���@홚@�b@���@�l�@���@�5?@��@��@�w@�"�@�~�@���@���@�(�@��
@�;d@���@�{@��/@ڧ�@�I�@љ�@���@д9@�z�@�(�@�  @Ϯ@�ff@�(�@˥�@��@��@�1@��@�=q@���@�p�@�+@�`B@�j@��@�l�@�ƨ@��@�|�@�bN@ļj@��
@�+@��@���@���@�z�@��;@�-@�V@��D@��@���@�G�@���@�dZ@��@�v�@��+@�M�@��9@���@�V@�I�@��@��@�^5@�5?@�5?@�^5@���@��R@���@���@�ȴ@��\@���@��!@�=q@��@��-@�x�@�%@�Z@�M�@�`B@�X@�V@���@��@��@�Z@�A�@�9X@�9X@�(�@��@�C�@��H@�@���@��@��9@�A�@�A�@�9X@�(�@��@�I�@�j@�Z@��m@�S�@���@�M�@��@�@�7L@��@���@�j@��@�ƨ@��@�dZ@�"�@�@���@���@�^5@�5?@�@�hs@�7L@��@���@�z�@�Z@�1@��;@��F@��@���@���@���@���@�|�@�S�@���@�^5@�-@���@��T@��^@��h@�`B@��@�V@��`@�Ĝ@���@�I�@�  @��w@���@�t�@�C�@��@�@���@��@�ȴ@�^5@��T@���@�`B@��@��/@��@��@�j@�A�@�1'@��@���@�K�@��@��@��R@��+@�-@��@��7@�?}@���@�z�@�Q�@�A�@� �@��
@��P@�+@��y@��!@�ff@��#@��-@��@�X@�&�@���@���@��@�z�@�9X@�  @���@�\)@�S�@�S�@�o@��y@�n�@�=q@���@���@���@���@��^@�x�@�V@���@� �@�  @���@�@��H@��!@��+@�n�@�E�@�$�@���@���@�`B@�O�@�?}@�%@���@��D@�1'@�b@���@���@���@�t�@�dZ@�+@���@���@���@�=q@��^@�hs@�V@���@���@�Q�@�;@�w@��@K�@~�R@~$�@}�-@|�j@|�D@|(�@z�@zn�@z=q@z�@z�\@z-@y��@yhs@xĜ@x�u@x  @w�@v��@v�R@w�@wK�@w+@w�P@w|�@v��@vff@u@t��@t(�@t�@s��@s�m@s�m@st�@s33@r��@r^5@r=q@r�@q�@qx�@qG�@p  @o\)@n��@n5?@m�@l��@lZ@l1@k��@j��@jM�@i�7@i&�@i%@h�`@h �@g�P@g\)@gK�@gK�@g;d@f��@f�R@f�+@e�@e?}@d�@d��@d�@d�D@dZ@d9X@d�@c�
@c�F@cdZ@co@b�@b��@b�\@b^5@b=q@a��@aG�@`��@`r�@`Q�@`A�@`A�@_�@^v�@]��@]/@]V@\�/@\�D@[��@[o@Z�H@Y��@Y��@Y�7@Yhs@YG�@Y�@X�9@Xr�@X �@X  @X  @X  @W�@W�P@V��@V��@VE�@V@U�h@U`B@UV@T��@S��@S��@SC�@R�\@R^5@R^5@R=q@Q��@Q��@Q��@Q&�@P�`@P�9@P�u@Pr�@PbN@PQ�@PQ�@PA�@P  @O|�@O\)@O+@O�@O
=@O
=@O
=@N��@N�+@N5?@N$�@M�-@M/@L�j@Lj@LI�@L9X@L(�@L�@K��@K��@K��@KdZ@KC�@Ko@J^5@I��@I�7@IG�@I&�@H�`@H�9@H�9@H�u@Hr�@HbN@HA�@H �@G�P@G+@Fȴ@FV@F5?@E�@E�-@E�@EO�@EO�@E?}@D��@DZ@D1@C�m@C�
@C��@CC�@C@B�\@Bn�@B=q@B�@A��@A��@Ax�@AG�@A%@A%@@��@@�u@@r�@@bN@?��@?�w@?�@?��@?+@>ff@>5?@>5?@>{@=��@=?}@<�j@<�D@<I�@;ƨ@;S�@:�H@:��@:��@:�!@:~�@:n�@:=q@9�7@9�@8��@8Q�@7�@7l�@6�y@6�R@6V@65?@6@5@5O�@4�/@3�m@3C�@2�!@2�\@2~�@2n�@2^5@2M�@1��@1&�@1�@1�@1%@0��@0�`@0�`@0Ĝ@0�@/�@/�P@/\)@/K�@/�@.�y@.ȴ@.�+@.5?@.@-��@-�@,�/@,��@,9X@,1@+ƨ@+��@+t�@+o@*�!@*�\@*^5@)�@)��@)hs@)G�@)&�@(�u@(1'@'�@'�;@'��@'�w@'�P@'\)@';d@'+@&��@&�R@&�+@&ff@&E�@&{@&@%�@%�-@%�h@%�@%p�@%?}@$�D@$I�@$1@$1@#�m@#�F@#t�@#33@#o@"�@"��@"�!@"��@"��@"=q@!��@!��@!�7@!x�@!X@!7L@!&�@!&�@!�@ ��@ ��@ Ĝ@ r�@ bN@   @�w@�@�@��@��@�P@|�@|�@|�@l�@l�@\)@K�@K�@�@��@�@��@$�@@��@��@`B@/@��@��@�@�@�@��@��@�D@j@9X@(�@1@ƨ@�F@��@C�@"�@�@�!@n�@^5@M�@�#@�^@��@7L@&�@&�@�@�@%@��@Ĝ@Ĝ@Ĝ@�9@�u@1'@  @�;@��@�P@|�@K�@
=@�y@�R@E�@5?@$�@�@�@�@�T@�-@p�@�@�j@��@ƨ@t�@o@��@=q@-@-@�@�#@�^@��@�7@hs@G�@7L@�@%@�`@�u@bN@Q�@b@��@�@�@��@l�@\)@\)@K�@;d@�y@��@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�?B	ƨB	ƨB	�?B	�9B	��B	��B	��B	��B	�B	�!B	�3B	�wB	��B	��B	��B	ĜB	�-B	�dB	��B
B
'�B
;dB
YB
gmB
y�B
��B
ɺB
��BuB\B+B�B&�B6FBF�BL�BG�BB�B8RB�B{B{B�B�B<jBC�B1'B%�BA�BI�BQ�BiyBq�BYBN�BP�BS�B@�B'�BB,B"�B:^B,B\B	7BPB
�;B
�/B
��B
ƨB
�?B
�B
��B
t�B
7LB
'�B	�B	�}B	�!B	�3B	�B	�PB	�=B	��B	q�B	YB	gmB	Q�B	>wB	H�B	0!B	#�B	
=B�B�ZB�dB�wB�XBB�dB��B�XB�jB�jB��B��B�XB�qB�dB�jB�qB�B��B�JB�wB�9B�?B�dBĜBÖB�B��B�!B�'B��B��B��B�3B��B��B�7B��B�FB�wB��B��B�ZB�`B�ZB�NB�BB�#B��B��BƨB�LB��B��B�BjB\)BaHBS�BL�BM�BbNB[#BS�BaHBXBp�B~�B|�Bx�Bo�BaHBR�BVB]/B]/Be`BffBm�B{�B�B�DB�\B�bB�{B�=B}�Br�B�B�Bw�BhsB�PB��B��B��B��B��B��B�\B��B��B��B��B��B��B��B��B�jB�RB�'B�FB�^B�?B�B��B�dB��B��B��B��B��B�B�
B��B��B��B��BƨB�LB�'B��B�}BB�dBÖB��BǮBÖBÖB��B��BĜB��B��B�^B�B��Bz�B�{B��B��B��B��B��B��B��B��B�B�B��B��B�!B�?B�LB�9B��B��B�!B�dBB��B�5B�B	B��B	B	B	DB	+B	+B	DB		7B	%B		7B	VB	JB	%B	B	B	hB	hB	VB	�B	hB	B��B	  B	B	%B	1B	PB	�B	�B	�B	!�B	#�B	#�B	%�B	(�B	+B	5?B	9XB	8RB	;dB	;dB	=qB	9XB	49B	+B	/B	:^B	:^B	=qB	@�B	F�B	J�B	P�B	R�B	T�B	VB	W
B	T�B	XB	\)B	[#B	ZB	W
B	bNB	ffB	jB	k�B	m�B	p�B	t�B	v�B	t�B	w�B	r�B	z�B	w�B	�B	�B	�B	�1B	�=B	�JB	�PB	�\B	�\B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	�-B	�3B	�9B	�?B	�?B	�FB	�FB	�FB	�RB	�RB	�RB	�XB	�RB	�^B	�dB	�wB	�wB	��B	B	ĜB	ƨB	ƨB	ŢB	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�#B	�;B	�BB	�;B	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�ZB	�sB	�sB	�mB	�mB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B	��B
B
B
  B
B
%B
+B
1B
+B
+B
1B
1B

=B
	7B
DB
JB
\B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
!�B
"�B
"�B
!�B
!�B
 �B
 �B
�B
 �B
!�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
'�B
'�B
(�B
+B
+B
+B
+B
(�B
%�B
(�B
,B
-B
-B
,B
)�B
+B
-B
+B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
1'B
1'B
0!B
/B
.B
0!B
0!B
1'B
0!B
1'B
1'B
0!B
0!B
0!B
1'B
0!B
2-B
2-B
2-B
1'B
33B
49B
2-B
49B
5?B
7LB
7LB
8RB
8RB
9XB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
:^B
:^B
8RB
9XB
:^B
9XB
:^B
:^B
<jB
=qB
>wB
>wB
>wB
>wB
>wB
=qB
=qB
>wB
<jB
:^B
;dB
<jB
>wB
?}B
?}B
?}B
@�B
A�B
A�B
@�B
@�B
@�B
?}B
A�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
C�B
B�B
A�B
C�B
E�B
E�B
D�B
D�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
H�B
H�B
I�B
H�B
F�B
I�B
I�B
H�B
G�B
F�B
J�B
K�B
J�B
I�B
I�B
H�B
K�B
J�B
I�B
J�B
K�B
N�B
M�B
M�B
M�B
M�B
L�B
I�B
K�B
K�B
L�B
L�B
L�B
L�B
N�B
N�B
P�B
O�B
O�B
N�B
N�B
M�B
O�B
Q�B
T�B
T�B
T�B
T�B
S�B
Q�B
R�B
W
B
W
B
W
B
VB
VB
VB
VB
S�B
R�B
T�B
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
VB
VB
W
B
VB
VB
W
B
XB
XB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
ZB
\)B
\)B
]/B
\)B
[#B
\)B
^5B
_;B
_;B
_;B
^5B
^5B
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
`BB
`BB
aHB
`BB
_;B
]/B
`BB
aHB
bNB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
bNB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
e`B
dZB
e`B
dZB
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
iyB
iyB
jB
jB
iyB
jB
iyB
iyB
jB
jB
iyB
jB
k�B
k�B
jB
k�B
k�B
k�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
n�B
n�B
n�B
o�B
n�B
n�B
m�B
l�B
l�B
l�B
k�B
n�B
n�B
n�B
n�B
p�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444444444411111111111144444111111111111111111111111111111111111111441111111111444444111111111111114444111111441111111111111111111111111111111111111141111111111111111111114441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�eB	�eB	��B	��B	��B	�_B	��B	��B	�%B	��B	��B	�8B	��B	��B	��B	�9B	�}B	յB	ѝB	�mB	ǮB	�`B	�cB	�sB
9B
)�B
=VB
Z�B
jB
}qB
��B
��B
��B�BbB�B�B'�B6�BG_BNBH�BCaB9�BBB_B!BB=�BD�B6+B*KBB�BL~BT�BjeBr�B[qBQ�BR�BT�BB�B,B�B./B%�B<PB/�B�B�BB
��B
��B
�B
ȚB
��B
�B
�KB
x�B
=VB
,B	�-B	��B	�nB	�ZB	��B	��B	�B	�WB	u�B	]�B	i�B	U�B	A�B	K)B	3�B	'RB	pB��B�B� BB��B��B��BðB��B��B��B�2B��B�B�wB��B��B�wB�B�)B�}B�}B�zB�B�qB�%B�%B�iB��B��B�B�B�2B�!B�B�&B�@B�dB��B��B��BˬBҽB�ZB�zB�B�B��BیB՛BѝBǔB��B�B��B��Bm�B_Bc:BVSBO\BPHBc�B]BVBb�BZ7BqvB.B}"By>BpoBb�BU2BX+B_!B_!BgBh�BoOB}B��B��B��B� B�B�xB� Bu%B�gB�[BzBkB��B��B��B��B�=B�5B��B�NB�,B�_B�eB�KB�DB��B��B��B�B��B��B��B��B��B�}B��B��B��B�(B�"B�JB��B�B�$B�2B�@B�NB�B�+B��B�|B�LB� B�GB��B�3B�B�KB�3B�3B�;B�;B�B�'B�'B�JB�wB�B.B�B�mB�B�KB�_B�_B��B��B�7B��B��B�B�RB�B��B��B�B��B�>B�B��B��BϫB��B��B	�B�.B	�B	�B	�B	B	�B	�B	
	B	_B	
#B	�B	B	+B	AB	MB	�B	�B	�B	�B	 B	�B��B	 �B	�B	�B	�B	�B	�B	�B	�B	!�B	$B	$B	&B	)*B	+QB	5ZB	9�B	8�B	;�B	;�B	=�B	9�B	5B	,WB	/�B	:xB	:�B	=�B	@�B	F�B	KB	Q4B	S&B	U2B	VSB	WYB	U�B	XyB	\CB	[�B	Z�B	XB	b�B	f�B	j�B	k�B	m�B	p�B	t�B	v�B	u?B	xRB	s�B	{0B	x�B	�;B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�7B	�B	�!B	�B	�B	�,B	�FB	�8B	�*B	�6B	�6B	�=B	�QB	�QB	�KB	�_B	��B	�|B	��B	��B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�9B	�+B	�)B	�)B	�B	�B	�(B	�B	�.B	�.B	�4B	�HB	�VB	�HB	�TB	�[B	�FB	�aB	�[B	�aB	�uB	�FB	�{B	�aB	�EB	�eB	�eB	�yB	ؓB	�B	�qB	�xB	ܬB	��B	ߊB	�vB	ߊB	�B	��B	��B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�/B	� B	�B	�B	� B	�)B	��B	��B	��B	�B	�B	��B	�B	�3B	�B	��B	��B	�B	�B	�%B	�+B	�B	�	B	�	B	�>B	�0B	�6B	�0B	�6B	�PB	�0B	�dB	�^B	�JB	�dB	�VB	�VB	�qB	�]B
[B
[B
oB
UB
UB
UB	��B
GB
uB
 �B
aB
tB
_B
KB
zB
�B
fB
�B

rB
	�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
	B
#B
B
!B
B
/B
�B
B
�B
B
B
/B
B
"B
#B
#B
"B
!�B
 �B
 �B
 'B
!B
!�B
#�B
$B
%B
%B
%B
&B
&B
'8B
'8B
'B
(>B
)*B
)DB
)DB
)DB
(>B
(>B
)DB
+QB
+QB
+6B
+QB
)yB
&�B
)yB
,WB
-]B
-]B
,WB
*B
+QB
-CB
+�B
.cB
.cB
.IB
.cB
.cB
.}B
/iB
/iB
0oB
1[B
1AB
0UB
/iB
.}B
0oB
0�B
1vB
0�B
1vB
1�B
0�B
0�B
0oB
1[B
0oB
2|B
2aB
2|B
1vB
3�B
4TB
2�B
4�B
5tB
7�B
7�B
8�B
8lB
9rB
8�B
8�B
8�B
9�B
:�B
:�B
;�B
;B
:xB
:�B
8�B
9�B
:�B
9�B
:�B
:�B
<�B
=�B
>�B
>�B
>�B
>�B
>�B
=�B
=�B
>�B
<�B
:�B
;�B
<�B
>�B
?�B
?�B
?�B
@�B
A�B
A�B
@�B
@�B
@�B
?�B
A�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
C�B
B�B
A�B
C�B
E�B
E�B
D�B
D�B
E�B
FB
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
IB
IB
I�B
IB
GB
I�B
I�B
IB
HB
GB
KB
K�B
KB
J	B
J	B
IB
LB
J�B
J	B
K)B
LB
OB
N"B
NB
N"B
N"B
MB
J#B
L0B
LB
MB
MB
M6B
M6B
O(B
OBB
QB
PB
PB
O(B
O(B
NVB
P.B
RTB
U2B
U2B
UB
UMB
TFB
RoB
S[B
W?B
W$B
W?B
V9B
VB
V9B
V9B
T,B
S@B
U2B
WYB
WYB
W?B
WYB
WYB
V9B
VSB
W?B
VSB
VmB
W?B
X_B
XEB
YKB
ZQB
ZQB
ZQB
ZkB
ZkB
[qB
[qB
ZkB
\xB
\]B
]~B
\xB
[qB
\]B
^jB
_pB
_VB
_pB
^jB
^jB
_pB
_�B
_pB
_pB
_�B
`�B
`�B
`vB
`\B
`�B
`vB
`vB
abB
`�B
_�B
]~B
`�B
a|B
b�B
a�B
a�B
a�B
a�B
b�B
c�B
c�B
c�B
cnB
c�B
b�B
b�B
b�B
d�B
dtB
d�B
d�B
ezB
e�B
e�B
e�B
d�B
e�B
d�B
e�B
d�B
e�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
f�B
f�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
i�B
i�B
j�B
j�B
i�B
j�B
i�B
i�B
j�B
j�B
i�B
j�B
k�B
k�B
j�B
k�B
k�B
k�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
l�B
n�B
n�B
n�B
o�B
n�B
n�B
m�B
l�B
l�B
l�B
k�B
n�B
n�B
n�B
n�B
p�B
r�B
r�B
r�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
r�B
r�B
r�B
s�B
r�B
s�B
tB
t�B
t�B
uB
t�B
u�B
t�B
t�B
uB
t�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.3(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809060035462018090600354620180906003546201809060200252018090602002520180906020025201809070028112018090700281120180907002811  JA  ARFMdecpA19c                                                                20180902093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180902003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180902003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180902003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180902003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180902003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180902003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180902003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180902003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180902003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20180902005617                      G�O�G�O�G�O�                JA  COOAcooa1.0                                                                 20181025063606  CF  PSAL            C�  C�  ?�                  JA  COOAcooa1.0                                                                 20181025063606  CF  PSAL            C�  C�  ?�                  JA  COOAcooa1.0                                                                 20181025063606  CF  PSAL            C�  C�  ?�                  JA  COOAcooa1.0                                                                 20181025063606  CF  PSAL            C�  C�  ?�                  JA  COOAcooa1.0                                                                 20181025063606  CF  PSAL            D  D� ?�                  JA  COOAcooa1.0                                                                 20181025063607  CF  PSAL            D  D� ?�                  JA  COOAcooa1.0                                                                 20181025063607  CF  PSAL            D  D  ?�                  JA  COOAcooa1.0                                                                 20181025063607  CF  PSAL            D*  D+  ?�                  JA  ARUP                                                                        20181025091511                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180902153606  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180903000000  CF  PSAL_ADJUSTED_QCC�  D+  G�O�                JM  ARCAJMQC2.0                                                                 20180905153546  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180905153546  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180905170025  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180906152811  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                