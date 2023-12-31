CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-24T00:35:39Z creation;2018-03-24T00:35:44Z conversion to V3.1;2019-12-19T07:42:26Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180324003539  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_223                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�U�u�1   @�U�:� @4�*�0��dDN���U1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B'��B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ Dż�D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D��3D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@�33@�33A33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��BffB&ffB.ffB6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+��C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D �3Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"�3D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*�fD+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dw��Dxl�Dx��Dyl�Dy��Dzl�Dz��D{l�D{��D|l�D|��D}l�D}��D~l�D~��Dl�D��D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD���D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD¶fD��fD�6fD�vfDöfD��fD�6fD�vfDĶfD��fD�6fD�vfDų3D��fD�6fD�vfDƶfD��fD�6fD�vfDǶfD��fD�6fD�vfDȶfD��fD�6fD�vfDɶfD��fD�6fD�vfDʶfD��fD�6fD�vfD˶fD��fD�6fD�vfD̶fD��fD�6fD�vfDͶfD��fD�6fD�vfDζfD��fD�6fD�vfDϹ�D��fD�6fD�vfDжfD��fD�6fD�vfDѶfD��fD�6fD�vfDҶfD��fD�6fD�vfDӶfD��fD�6fD�vfDԶfD��fD�6fD�vfDնfD��fD�6fD�vfDֶfD��fD�6fD�vfD׶fD��fD�6fD�vfDضfD��fD�6fD�vfDٶfD��fD�6fD�vfDڶfD��fD�6fD�vfD۶fD��fD�6fD�vfDܶfD��fD�6fD�vfDݶfD��fD�6fD�vfD޶fD��fD�6fD�vfD߶fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�9�D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�O�A�M�A�M�A�M�A�Q�A�Q�A�XA�XA�ZA�ZA�\)A�\)A�^5A�^5A�\)A�\)A�^5A�^5A�S�A�+A�JAƬA���AžwA��;A�1A���A�AŋDA�S�A�?}A�=qA� �AăA�;dA�A���AÝ�A�Q�A�$�A��TA\A�ffA�G�A�  A���A���A�v�A���A���A��A���A��A��A���A���A���A��+A�n�A�  A�ZA�A�n�A��PA��A�ƨA��HA�E�A�S�A���A�hsA��TA���A�ȴA�$�A�ZA�z�A��A���A�\)A�jA�9XA��A�ffA��hA��jA�?}A��A��-A�n�A���A��A���A�K�A�ĜA���A��A� �A��A��A�`BA��wA�~�A�I�A�|�A��A�bA��uA�Q�A���A�t�A��uA��A�M�A�-A�oA���A�ƨA��+A���A�7LA�A�7LA~�/A{�-Ay�Aw�mAs��An�Ak&�Ai�Af-Acp�A_��A\bAY�hAV�jAUG�AS"�AO%AN~�AMdZAL=qAK�wAKdZAJ�AJ��AJ^5AI�hAH�DAFn�AD=qAC��AAt�A?ƨA=�A;x�A9��A9"�A8�A6��A3��A2bA1G�A0��A,��A*��A)K�A'`BA'&�A&��A$�A#ƨA"��A!l�A M�A  A�^A33Al�A�AC�A��A��A"�AffA��A�wA�A��Av�AffA9XA�A��A�RAƨAhsA
M�A	�TA	A�TAXA�A%A�+A{A�
AdZA+A��AM�A�@���@�$�@���@��H@�@���@���@���@��9@��@�9@��D@��@�1'@� �@�+@�h@��/@��@�{@�hs@�|�@�v�@��;@���@��@�9X@ߝ�@�?}@ڗ�@�$�@�hs@�K�@�%@�j@�r�@�r�@�I�@� �@��
@�$�@Л�@�K�@�5?@��@��@�E�@�x�@�Ĝ@�l�@�v�@�%@�1@�|�@¸R@���@��@��/@�Ĝ@��D@���@�|�@�\)@��\@�O�@��m@��
@���@��
@��
@��w@��w@�S�@�&�@�Q�@�j@�I�@��@���@�ff@���@�`B@�r�@��@�33@��T@�x�@�p�@�hs@�X@�G�@�V@��/@�r�@��w@�S�@��@��@�@��@�@�+@�ȴ@�`B@�z�@��m@�o@�ȴ@�v�@�5?@��@���@�p�@�G�@���@��j@�I�@��@���@�l�@�"�@��!@�@��`@�j@�  @��F@���@��w@�A�@�1'@�1@���@�C�@�@�ȴ@�~�@�v�@�V@��@�n�@��+@�~�@�n�@�ff@�^5@�E�@��^@�`B@�X@�O�@�?}@�/@��@���@�Ĝ@���@�Z@�b@���@���@��P@�dZ@��y@�5?@���@��-@��h@��7@�/@���@���@�hs@�&�@�z�@��@���@�
=@��@���@�v�@�v�@�5?@�-@�@��T@���@�O�@�G�@�7L@��@���@��`@��@�r�@�I�@�9X@�(�@�1@���@��@�b@���@���@�^5@�ff@��@��#@�$�@��H@��y@���@��@���@��\@�ff@�5?@��@��@�@�5?@�E�@�-@���@��h@�p�@�O�@�/@�V@���@��9@�Z@�9X@�(�@� �@�  @��@��;@��w@�K�@��@�@��R@�M�@��@��#@�@��^@��@�hs@�&�@���@��@��@�Z@��
@��@�S�@�C�@�C�@�C�@�;d@��@�ff@�E�@�J@���@��@���@�x�@�?}@��@���@���@��9@��@�Z@�(�@��@��;@��
@���@��w@��@�K�@��@��y@���@�ȴ@���@�v�@�M�@��@�@��^@���@�p�@�O�@��@��@���@��9@���@�b@��m@��
@���@��@�l�@�C�@�o@���@���@���@���@��\@�ff@�{@��-@�hs@�7L@���@���@�Q�@�(�@���@��P@�S�@�C�@�"�@��@���@�$�@�@��7@�O�@�V@��@��/@��j@��D@�r�@�bN@�Z@�Z@�I�@��@K�@
=@~�y@~��@~E�@}�h@}V@|�j@{�
@{o@z��@y��@yG�@x��@x�`@xĜ@x�9@xr�@x  @w
=@v��@v��@vff@vE�@v{@up�@tz�@s�
@s33@r�\@rM�@rJ@q�#@q7L@p�9@pA�@pb@o��@n�@nV@m��@mp�@m/@lz�@l�@k��@k�
@k�@k"�@j�!@jM�@i��@i�#@i��@iX@i�@h��@h��@g�;@gK�@f�R@e�@e?}@d�D@d(�@d1@c�@c@b�\@bM�@bJ@a��@aG�@`��@`Q�@` �@`  @_�@_�;@_|�@_\)@_;d@^�y@^V@]@]`B@\�/@\�D@\(�@[ƨ@[dZ@[33@Z��@Z~�@Y��@Yhs@Y7L@Y&�@Y�@Y%@X�`@X��@Xb@W�w@Wl�@W;d@W+@V�R@V��@V{@U�h@Up�@U`B@U`B@T�@T(�@T1@S��@S��@Sƨ@S��@SdZ@S33@S@R�H@R��@R=q@Qhs@Q7L@Q%@P�9@O�@N�R@M�T@Mp�@L��@L�@L�D@LI�@Kƨ@KdZ@K@J��@JM�@I�@I��@Ix�@IX@IG�@I�@H�`@H�`@H�9@H�u@H �@G\)@G;d@G�@F�y@FV@E�@EV@D��@DZ@C��@C��@Ct�@C33@C@B�H@B~�@B=q@B�@BJ@A��@A�#@A��@A�7@AG�@A&�@@��@@Ĝ@@r�@@1'@@  @?�w@?��@?l�@?
=@>��@>�@>�R@>��@>E�@>$�@>$�@>{@=�T@=�h@=�@<�@<�D@<(�@;ƨ@;t�@;dZ@;C�@;"�@;o@:��@:=q@9�#@9��@9hs@9�@8��@8��@8bN@8A�@8  @7�@7|�@7l�@7;d@6�y@6v�@6V@6V@6$�@6{@5�T@5�@4�@4��@4I�@3��@3ƨ@3��@3�@3t�@2��@2M�@2J@1�7@0�`@0bN@0A�@01'@0 �@0b@/�;@/�w@/��@/\)@/
=@.�y@.��@-��@-`B@-/@,�@,I�@+��@+�
@+�@+o@*M�@)�@)�#@)��@)��@)�^@)�7@)X@)&�@(��@(A�@(1'@( �@(  @'�@'��@'�@'l�@'+@&��@&�y@&�R@&��@&�+@&V@&{@%��@%�-@%`B@%/@%V@$��@$��@$(�@#�F@#S�@#o@#@"�@"�@"~�@"=q@"-@"J@!��@!��@!x�@!X@!G�@!&�@!�@!%@ ��@ ��@ �@�@�w@|�@;d@��@ȴ@��@v�@V@�@�-@�h@�h@�@`B@O�@?}@/@�@��@�/@��@��@z�@��@�m@�F@��@dZ@"�@o@@�@��@M�@��@�@�@�#@��@X@&�@��@�`@�`@�`@��@Ĝ@�9@�9@�u@r�@Q�@�;@K�@+@�@�@��@�y@�y@�y@�y@ȴ@��@5?@�@�T@@�-@��@��@��@��@�h@�h@�@O�@�@�j@�D@I�@(�@��@�m@ƨ@�@S�@C�@C�@33@33@"�@o@@�@�H@�!@��@~�@~�@^5@=q@J@��@��@�7@hs@G�@%@A�@ �@b@�@��@�w@�@|�@;d@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�O�A�M�A�M�A�M�A�Q�A�Q�A�XA�XA�ZA�ZA�\)A�\)A�^5A�^5A�\)A�\)A�^5A�^5A�S�A�+A�JAƬA���AžwA��;A�1A���A�AŋDA�S�A�?}A�=qA� �AăA�;dA�A���AÝ�A�Q�A�$�A��TA\A�ffA�G�A�  A���A���A�v�A���A���A��A���A��A��A���A���A���A��+A�n�A�  A�ZA�A�n�A��PA��A�ƨA��HA�E�A�S�A���A�hsA��TA���A�ȴA�$�A�ZA�z�A��A���A�\)A�jA�9XA��A�ffA��hA��jA�?}A��A��-A�n�A���A��A���A�K�A�ĜA���A��A� �A��A��A�`BA��wA�~�A�I�A�|�A��A�bA��uA�Q�A���A�t�A��uA��A�M�A�-A�oA���A�ƨA��+A���A�7LA�A�7LA~�/A{�-Ay�Aw�mAs��An�Ak&�Ai�Af-Acp�A_��A\bAY�hAV�jAUG�AS"�AO%AN~�AMdZAL=qAK�wAKdZAJ�AJ��AJ^5AI�hAH�DAFn�AD=qAC��AAt�A?ƨA=�A;x�A9��A9"�A8�A6��A3��A2bA1G�A0��A,��A*��A)K�A'`BA'&�A&��A$�A#ƨA"��A!l�A M�A  A�^A33Al�A�AC�A��A��A"�AffA��A�wA�A��Av�AffA9XA�A��A�RAƨAhsA
M�A	�TA	A�TAXA�A%A�+A{A�
AdZA+A��AM�A�@���@�$�@���@��H@�@���@���@���@��9@��@�9@��D@��@�1'@� �@�+@�h@��/@��@�{@�hs@�|�@�v�@��;@���@��@�9X@ߝ�@�?}@ڗ�@�$�@�hs@�K�@�%@�j@�r�@�r�@�I�@� �@��
@�$�@Л�@�K�@�5?@��@��@�E�@�x�@�Ĝ@�l�@�v�@�%@�1@�|�@¸R@���@��@��/@�Ĝ@��D@���@�|�@�\)@��\@�O�@��m@��
@���@��
@��
@��w@��w@�S�@�&�@�Q�@�j@�I�@��@���@�ff@���@�`B@�r�@��@�33@��T@�x�@�p�@�hs@�X@�G�@�V@��/@�r�@��w@�S�@��@��@�@��@�@�+@�ȴ@�`B@�z�@��m@�o@�ȴ@�v�@�5?@��@���@�p�@�G�@���@��j@�I�@��@���@�l�@�"�@��!@�@��`@�j@�  @��F@���@��w@�A�@�1'@�1@���@�C�@�@�ȴ@�~�@�v�@�V@��@�n�@��+@�~�@�n�@�ff@�^5@�E�@��^@�`B@�X@�O�@�?}@�/@��@���@�Ĝ@���@�Z@�b@���@���@��P@�dZ@��y@�5?@���@��-@��h@��7@�/@���@���@�hs@�&�@�z�@��@���@�
=@��@���@�v�@�v�@�5?@�-@�@��T@���@�O�@�G�@�7L@��@���@��`@��@�r�@�I�@�9X@�(�@�1@���@��@�b@���@���@�^5@�ff@��@��#@�$�@��H@��y@���@��@���@��\@�ff@�5?@��@��@�@�5?@�E�@�-@���@��h@�p�@�O�@�/@�V@���@��9@�Z@�9X@�(�@� �@�  @��@��;@��w@�K�@��@�@��R@�M�@��@��#@�@��^@��@�hs@�&�@���@��@��@�Z@��
@��@�S�@�C�@�C�@�C�@�;d@��@�ff@�E�@�J@���@��@���@�x�@�?}@��@���@���@��9@��@�Z@�(�@��@��;@��
@���@��w@��@�K�@��@��y@���@�ȴ@���@�v�@�M�@��@�@��^@���@�p�@�O�@��@��@���@��9@���@�b@��m@��
@���@��@�l�@�C�@�o@���@���@���@���@��\@�ff@�{@��-@�hs@�7L@���@���@�Q�@�(�@���@��P@�S�@�C�@�"�@��@���@�$�@�@��7@�O�@�V@��@��/@��j@��D@�r�@�bN@�Z@�Z@�I�@��@K�@
=@~�y@~��@~E�@}�h@}V@|�j@{�
@{o@z��@y��@yG�@x��@x�`@xĜ@x�9@xr�@x  @w
=@v��@v��@vff@vE�@v{@up�@tz�@s�
@s33@r�\@rM�@rJ@q�#@q7L@p�9@pA�@pb@o��@n�@nV@m��@mp�@m/@lz�@l�@k��@k�
@k�@k"�@j�!@jM�@i��@i�#@i��@iX@i�@h��@h��@g�;@gK�@f�R@e�@e?}@d�D@d(�@d1@c�@c@b�\@bM�@bJ@a��@aG�@`��@`Q�@` �@`  @_�@_�;@_|�@_\)@_;d@^�y@^V@]@]`B@\�/@\�D@\(�@[ƨ@[dZ@[33@Z��@Z~�@Y��@Yhs@Y7L@Y&�@Y�@Y%@X�`@X��@Xb@W�w@Wl�@W;d@W+@V�R@V��@V{@U�h@Up�@U`B@U`B@T�@T(�@T1@S��@S��@Sƨ@S��@SdZ@S33@S@R�H@R��@R=q@Qhs@Q7L@Q%@P�9@O�@N�R@M�T@Mp�@L��@L�@L�D@LI�@Kƨ@KdZ@K@J��@JM�@I�@I��@Ix�@IX@IG�@I�@H�`@H�`@H�9@H�u@H �@G\)@G;d@G�@F�y@FV@E�@EV@D��@DZ@C��@C��@Ct�@C33@C@B�H@B~�@B=q@B�@BJ@A��@A�#@A��@A�7@AG�@A&�@@��@@Ĝ@@r�@@1'@@  @?�w@?��@?l�@?
=@>��@>�@>�R@>��@>E�@>$�@>$�@>{@=�T@=�h@=�@<�@<�D@<(�@;ƨ@;t�@;dZ@;C�@;"�@;o@:��@:=q@9�#@9��@9hs@9�@8��@8��@8bN@8A�@8  @7�@7|�@7l�@7;d@6�y@6v�@6V@6V@6$�@6{@5�T@5�@4�@4��@4I�@3��@3ƨ@3��@3�@3t�@2��@2M�@2J@1�7@0�`@0bN@0A�@01'@0 �@0b@/�;@/�w@/��@/\)@/
=@.�y@.��@-��@-`B@-/@,�@,I�@+��@+�
@+�@+o@*M�@)�@)�#@)��@)��@)�^@)�7@)X@)&�@(��@(A�@(1'@( �@(  @'�@'��@'�@'l�@'+@&��@&�y@&�R@&��@&�+@&V@&{@%��@%�-@%`B@%/@%V@$��@$��@$(�@#�F@#S�@#o@#@"�@"�@"~�@"=q@"-@"J@!��@!��@!x�@!X@!G�@!&�@!�@!%@ ��@ ��@ �@�@�w@|�@;d@��@ȴ@��@v�@V@�@�-@�h@�h@�@`B@O�@?}@/@�@��@�/@��@��@z�@��@�m@�F@��@dZ@"�@o@@�@��@M�@��@�@�@�#@��@X@&�@��@�`@�`@�`@��@Ĝ@�9@�9@�u@r�@Q�@�;@K�@+@�@�@��@�y@�y@�y@�y@ȴ@��@5?@�@�T@@�-@��@��@��@��@�h@�h@�@O�@�@�j@�D@I�@(�@��@�m@ƨ@�@S�@C�@C�@33@33@"�@o@@�@�H@�!@��@~�@~�@^5@=q@J@��@��@�7@hs@G�@%@A�@ �@b@�@��@�w@�@|�@;d@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��BB�B]/B��B�RB�}BǮB�BVB\B{B�BuB!�B&�B'�B)�B(�B-B49B7LB<jB=qB8RB%�B%�B/B&�B0!BS�Bv�B}�B�B� B~�B}�By�Bt�BgmBVBe`Bm�B_;BW
BA�B?}BQ�BO�B1'B�B7LB�B{B�B�BDBJBB�B�B�NB�/BǮBƨBɺBÖBǮB�jB��B��B��B��B��B{�B�DB�%Bo�BZBcTBI�B&�B�B%B
�#B
�B
�3B
��B
ȴB
�RB
��B
�B
�B
��B
��B
��B
�bB
�%B
u�B
^5B
.B
5?B
33B
$�B
1B
  B	�B	ŢB	��B	�PB	�bB	s�B	^5B	@�B	,B	$�B	�B	uB	1B�TB	+B	B	  B	B	B��B��B��B�B�HB�BƨB�/BŢB��B�RB�B�B�dB�9B��B�VB��B��B��Bo�Bt�B|�Bx�B�DB�Bu�Bv�Bv�Bn�BffBYBQ�Bm�B_;B^5Bl�BjBhsBl�BiyBn�Bs�Bl�Br�Bt�Br�Bk�B]/BO�BT�BM�B@�BL�B]/BS�BO�BS�BL�BL�BXBXB\)BXBZBR�BS�BI�B?}BH�BP�BH�BQ�BXBP�BF�BF�BK�BO�BhsBm�BjBjB`BB`BBbNB[#BdZBgmB]/B`BBYBcTBdZB_;BhsB^5B^5Bo�Bo�Bk�Bn�B�B�1B�7B�7B�1B�B{�Bz�B~�B�B}�B�7B�JB�JB�JB�7B�PB�\B�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�}BŢBȴBȴBǮBƨBB�^B��B�B�B�B��B�B�
B�B�B�5B�BB�HB�B��B��B��B��B��B��B��B��B��B	B	B	
=B	DB	PB	VB	bB	VB	uB	�B	�B	$�B	'�B	+B	-B	/B	2-B	49B	7LB	:^B	;dB	@�B	B�B	D�B	C�B	E�B	D�B	F�B	J�B	N�B	S�B	ZB	]/B	e`B	e`B	e`B	e`B	e`B	jB	l�B	l�B	p�B	t�B	x�B	�B	�B	�%B	�1B	�=B	�JB	�JB	�=B	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�LB	�LB	�?B	�LB	�FB	�LB	�XB	�^B	�^B	�qB	�jB	�qB	�jB	�dB	�dB	�qB	��B	��B	�}B	�}B	��B	��B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	��B	��B	��B	ƨB	ƨB	��B	��B	��B	�
B	�/B	�#B	�#B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�;B	�HB	�BB	�;B	�BB	�NB	�TB	�TB	�ZB	�`B	�`B	�`B	�sB	�yB	�yB	�yB	�B	�yB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B	��B
  B
  B
  B
B
B
B
%B
B
B
+B
+B
	7B

=B

=B
DB
JB
DB
PB
\B
\B
VB
VB
VB
\B
\B
\B
VB
JB
\B
bB
bB
hB
hB
hB
hB
hB
oB
{B
{B
uB
oB
hB
hB
uB
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
 �B
!�B
"�B
"�B
"�B
!�B
!�B
 �B
#�B
%�B
$�B
$�B
#�B
!�B
 �B
"�B
#�B
#�B
%�B
%�B
%�B
#�B
$�B
%�B
&�B
%�B
$�B
&�B
'�B
'�B
(�B
'�B
'�B
)�B
)�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
)�B
(�B
+B
+B
)�B
+B
,B
.B
.B
-B
-B
.B
/B
/B
/B
.B
/B
1'B
2-B
2-B
2-B
2-B
1'B
1'B
2-B
1'B
/B
0!B
1'B
0!B
1'B
1'B
1'B
2-B
33B
2-B
2-B
2-B
2-B
5?B
5?B
5?B
5?B
5?B
49B
33B
5?B
5?B
6FB
7LB
5?B
6FB
6FB
6FB
8RB
8RB
8RB
6FB
7LB
:^B
:^B
:^B
9XB
9XB
:^B
:^B
:^B
:^B
9XB
9XB
8RB
:^B
:^B
9XB
8RB
6FB
:^B
<jB
=qB
?}B
?}B
?}B
>wB
?}B
?}B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
B�B
C�B
C�B
B�B
A�B
A�B
D�B
D�B
C�B
A�B
A�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
J�B
L�B
L�B
L�B
L�B
K�B
L�B
M�B
L�B
L�B
K�B
K�B
L�B
L�B
L�B
M�B
N�B
O�B
O�B
N�B
N�B
M�B
N�B
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
Q�B
R�B
Q�B
Q�B
Q�B
S�B
S�B
S�B
S�B
R�B
Q�B
Q�B
S�B
S�B
T�B
T�B
VB
VB
T�B
R�B
T�B
VB
T�B
T�B
W
B
YB
YB
YB
YB
YB
YB
YB
XB
XB
YB
XB
W
B
YB
ZB
ZB
YB
[#B
\)B
[#B
[#B
[#B
]/B
_;B
_;B
_;B
^5B
^5B
^5B
^5B
^5B
]/B
`BB
`BB
`BB
`BB
`BB
`BB
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
bNB
aHB
bNB
bNB
bNB
aHB
aHB
bNB
cTB
dZB
e`B
e`B
dZB
cTB
dZB
e`B
e`B
dZB
ffB
ffB
ffB
gmB
ffB
gmB
gmB
ffB
ffB
e`B
dZB
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
jB
iyB
hsB
k�B
k�B
k�B
jB
k�B
l�B
l�B
k�B
k�B
jB
k�B
m�B
m�B
m�B
k�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
l�B
l�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
o�B
n�B
n�B
o�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
u�B
u�B
t�B
s�B
r�B
v�B
v�B
v�B
v�B
w�B
w�B
v�B
w�B
w�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
��B
�/B
�dBB5B]�B�7B�B��BȀB�B�B�B�BB�B"hB'�B(�B*B)�B-�B4�B8B<�B>B9�B(�B(
B0!B(�B2BUgBw2B~BB� B�4B.B~(BzDButBiBYeBf�Bn�Ba�BY�BE�BC-BS�BRTB5%B]B9$B�B$B BBB6BjB�B�B�+B�`B�'B�BȴB�xB��B�KB��B��B��B�B��B�KBB��B��G�O�B\�Bd�BL�B*�B�B	RB
��B
ܒB
�B
�B
��B
�xB
��B
��B
�YB
��B
�B
��B
��B
�+B
wfG�O�B
33B
8B
5ZB
'mB
B
-B	�fG�O�B	��B	�hB	�@B	w�B	b4B	ESB	0oB	(>B	�B	�B	xB�
B	�B	�B	oB	�B	�B��B��B��B�B�:B��B�lBޞBȚB��B��B� B�OB�6B��B��B��B��B�jB��G�O�Bw�BHB{0B��B�YBx8Bx�Bx�Bp�BhsB\CBT�Bn�Ba�B`'Bm�Bk�Bi�Bm�Bj�BoOBtTBm�Bs3Bu%BsBlqB_BR:BV�BO�BC�BNpB^BU�BQhBUBN�BN<BX�BX�B\�BX�BZ�BS�BT�BK^BA�BI�BRBJ=BR�BX�BQ�BH�BHKBM6BQ4Bh�Bm�BkBkBa|BaHBc B\�Be,BhXB^�BabBZ�Bd@Be`B`�Bi_B`'B`Bp!Bp�Bm)BpB��B�fB��B��B��B��B}VB|B�B�9B�B�	B�B�B�B�rB�VB��B�MB�+B�_B�YB�BB�:B�B�@B��B�DB�_B��B��B�B��BżB��B��B��B��B�GB�B�xB�9B�BּBյB�B��BڠB��B��B��B�hB��B�B�B��B��B�B�B�FB��B�VB	uB	�B	
rB	�B	jB	�B	B	�B	FB	EB	QB	%FB	(XB	+QB	-]B	/�B	2|B	4�B	7�B	:�B	<B	@�B	B�B	D�B	DB	F?B	E�B	GzB	K^B	OBB	TaB	ZQB	]/B	e,B	e�B	e�B	e�B	e�B	j�B	l�B	l�B	p�B	t�B	y	B	��B	�9B	�YB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�@B	�>B	�8B	�zB	��B	�B	�IB	�iB	�iB	��B	�iB	�MB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�zB	�+B	�B	�6B	� B	�
B	��B	�=B	�WB	�kB	�eB	�QB	�eB	چB	یB	�OB	�VB	�pB	�bB	��B	ߤB	��B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�+B	�	B	�0B	�B	��B	�B	�RB	�LB	�B	�6B	�BB	�<B	�PB	�JB	�<B	�BB	�HB
;B
 OB	�HB
 OB
 OB
 OB
[B
9B
SB
YB
�B
�B
zB
�B
	lB

�B

rB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�G�O�B

B
B
�B
	B
B
�B
�B
�B
�B
�B
 B
�B
�B
�B
/B
B
�B
B
�B
B
/B
B
G�O�B
5B
 B
 BB
!-B
"B
#B
#B
#B
"B
"B
!-B
$&B
&B
%B
%B
$&G�O�B
!bB
#:B
$&B
$&B
&B
&B
&2G�O�B
%,B
&2B
'B
&LB
%`B
'8B
(XB
(>B
)*B
(XB
(>B
*KB
*0B
)_B
)_B
)DB
*KB
*0B
+QB
+QB
+6B
+QB
,=G�O�B
)_B
+kB
+kB
*eB
+kB
,qB
.IB
.cB
-]B
-wB
.cB
/OB
/OB
/iB
.cB
/�B
1vB
2aB
2aB
2aB
2|B
1vB
1vB
2|B
1vG�O�B
0oB
1vB
0�B
1[B
1vB
1�B
2|B
3�B
2|B
2�B
2|B
2|B
5�B
5ZB
5ZB
5�B
5tB
4�B
3�B
5tB
5�B
6zB
7�G�O�B
6�B
6�B
6�B
8lB
8�B
8�G�O�B
7�B
:xB
:�B
:�B
9�B
9�B
:�B
:�B
:�B
:�B
9�B
9�B
8�B
:�B
:�B
9�B
8�B
6�B
:�B
<�B
=�B
?�B
?�B
?�B
>�B
?�B
?�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
B�B
C�B
C�B
B�B
A�B
A�B
D�B
D�B
C�G�O�B
A�B
E�B
E�B
F�B
GB
G�B
G�B
G�B
IB
IB
H�B
H�B
J	B
I�B
J�B
I�B
I�B
I�B
J	B
J�B
J	B
J	B
J	B
KB
KB
KB
K�B
K�B
K)B
MB
MB
MB
MB
K�B
MB
M�B
MB
MB
LB
L0B
MB
M6B
M6B
N"B
OB
O�B
P.B
OB
O(B
N"B
OB
O(B
P.B
P.B
PB
Q4B
QB
QB
R B
Q4B
QB
R:B
S&B
R B
R B
RTB
T,B
T,B
T,B
T,B
S@B
R:B
RTB
TFB
T,B
UMB
U2B
VSB
VSB
UgG�O�B
UMB
V9B
UMB
UMB
WsB
Y1B
Y1B
YKB
YKB
YKB
YKB
YKB
X_B
X_B
YeB
X_B
WsB
YB
ZQB
ZQB
YeB
[WB
\]B
[qB
[�B
[�B
]~B
_pB
_VB
_VB
^�B
^jB
^jB
^jB
^jB
]~B
`vB
`\B
`vB
`\B
`�B
`vB
_�B
_pB
`vB
a|B
a|B
a|B
a�B
a|B
a|B
a|B
b�B
a|B
b�B
b�B
b�B
a�B
a�B
b�B
c�B
d�B
ezB
e�B
d�B
c�B
d�B
e�B
e�B
d�B
f�B
f�B
f�B
g�B
f�B
g�B
g�B
f�B
f�B
e�B
d�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
i�B
h�B
k�B
k�B
k�B
j�B
k�B
l�B
l�B
k�B
k�B
j�B
k�B
m�B
m�B
m�G�O�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
m�B
m�B
l�B
l�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
o�B
n�B
n�B
o�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
tB
t�B
t�B
t�B
uB
t�B
uB
tB
t�B
u�B
u�B
uB
s�B
sB
v�B
v�B
v�B
wB
xB
w�B
v�B
xB
xB
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111114111111141111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111141111111111111111111111141111111111111111411111114111111111111111111111114111111111111111111111111141111111111111111111111141111114111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.3(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803280034262018032800342620180328003426201806221328042018062213280420180622132804201804050732022018040507320220180405073202  JA  ARFMdecpA19c                                                                20180324093522  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180324003539  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180324003542  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180324003542  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180324003543  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180324003543  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180324003543  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180324003543  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180324003544  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180324003544                      G�O�G�O�G�O�                JA  ARUP                                                                        20180324005621                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180324153441  CV  JULD            G�O�G�O�F­�                JM  ARSQJMQC2.0                                                                 20180326000000  CF  PSAL_ADJUSTED_QCCD  D�@ G�O�                JM  ARCAJMQC2.0                                                                 20180327153426  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180327153426  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404223202  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042804  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                