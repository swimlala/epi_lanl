CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-01-31T00:35:33Z creation;2017-01-31T00:35:35Z conversion to V3.1;2019-12-19T08:16:06Z update;     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20170131003533  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               TA   JA  I2_0577_084                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��tj�e 1   @��uW; @3�˒:)��d��_p1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� DfD� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�3D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @l��@�ff@�ffA33A;33A[33A{33A���A���A���A���A͙�A���A���A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B_33Bf��Bn��Bw33B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci��Ck�3Cm�3Co�3Cq�3Cs��Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D�3D	l�D	��D
l�D
��Dl�D��DffD��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`s3D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dw��Dxl�Dx��Dyl�Dy��Dzl�Dz��D{l�D{��D|l�D|��D}l�D}��D~l�D~�3Dl�D��D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�s3D��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��3D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD���D��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD¶fD��fD�6fD�vfDöfD��fD�6fD�vfDĶfD��fD�6fD�vfDŶfD��fD�6fD�vfDƶfD��fD�6fD�vfDǶfD��fD�6fD�vfDȶfD��fD�6fD�vfDɶfD��fD�6fD�vfDʶfD��fD�6fD�vfD˶fD��fD�6fD�vfD̶fD��fD�6fD�vfDͶfD���D�6fD�vfDζfD��fD�6fD�vfD϶fD��fD�6fD�vfDжfD��fD�6fD�vfDѶfD��fD�6fD�vfDҶfD��fD�6fD�vfDӶfD��fD�6fD�vfDԶfD��fD�6fD�vfDնfD��fD�6fD�vfDֶfD��fD�6fD�vfD׶fD��fD�6fD�vfDضfD��fD�6fD�vfDٶfD��fD�6fD�vfDڶfD��fD�6fD�vfD۶fD��fD�6fD�vfDܶfD��fD�6fD�vfDݶfD��fD�6fD�vfD޶fD��fD�6fD�vfD߶fD��fD�33D�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD幚D��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD���D�9�D�` 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�K�A�K�A�K�A�K�A�K�A�M�A�Q�A�K�A�M�A�K�A�K�A�M�A�O�A�S�A�XA�XA�XA�VA�VA�XA�VA�O�A�O�A�O�A�O�A�Q�A�S�A�ZA�XA�ZA�\)A�ZA�S�A�VA�n�Aɰ!Aɣ�Aɩ�A�ƨA���A�ƨA���Aɺ^Aɇ+A�9XA�1'A� �A�A���A�t�A�"�A��Aǣ�AǕ�AǇ+A�dZA�-A���A�A�A�A�A���Aô9A�~�A�\)A�O�A��A¸RA�O�A�x�A��PA�A�Q�A��A���A�A�z�A���A��RA��+A�VA��A�5?A�bA��A�1A�+A���A�$�A�r�A�|�A�/A�hsA���A�K�A��
A��yA��PA���A��yA��A��`A�`BA�XA�oA�t�A��A��DA��/A���A��HA��\A���A���A�-A�r�A��A�K�A�dZA�7LA���A�S�A���A��-A��TA��A���A���A�
=A;dA}"�A|5?Az��Aw"�AvQ�Aut�At9XAr~�AoK�An�HAn��An�AlVAj��Ai�wAit�AiO�Ag�
Ad^5AcS�AbA�A`(�A]�^A\�yA\JAZz�AX��AW��AWS�AVr�AU|�ATȴAS+AQ��AO%ALZAK"�AJ1AG��AF �AE/AC��AA�A@(�A>I�A<��A:ĜA9�A9hsA8bNA7�FA6A�A4^5A1�A17LA0�/A/�A.ffA-%A+�^A*��A(  A&��A&z�A%�TA$��A#�-A#G�A#%A"��A"�A ��A��A�;AK�A�AA�\AbA�DAG�Av�A�
A��A�A�/A�DA�AA+A��A(�A�A�FAG�AjA�A��A-A&�A��AJAXA
ffA
(�A	+A	dZA	��A	O�A	7LA	�mA
�HAx�A
�RA
1A	K�AE�A��A�PA�AC�A1A�7A ��@��@��y@�@�/@�Ĝ@�Z@��;@�t�@�+@���@���@��-@��-@�x�@�w@@�P@�@��@@�-@�j@��m@��H@�Z@�"�@���@�t�@�E�@�`B@�@�Q�@�dZ@ݡ�@ۅ@���@�x�@�%@׍P@���@�-@թ�@�V@�ƨ@ҸR@���@�/@�(�@�dZ@���@�C�@�|�@��m@Л�@�z�@�ȴ@�+@�~�@þw@��@��@��m@���@���@��@�-@���@���@�@���@��h@��7@�hs@�V@��D@��@��w@�C�@���@�ff@�$�@��7@�Z@��@���@��;@��;@�b@�t�@���@�E�@���@�z�@��@�hs@���@�Ĝ@�(�@�;d@���@�=q@��@���@���@���@�I�@���@�t�@��!@��-@�G�@�7L@�&�@��/@���@��@�A�@���@��@���@��@��\@�~�@�V@�J@���@���@��@�G�@�j@�  @��w@��P@�|�@�S�@��@�33@�Z@���@�z�@��u@��w@��@�b@��@� �@��@�  @��F@��@�t�@�+@�$�@���@�\)@�{@���@�`B@�V@�Ĝ@��@��u@�j@�Z@�I�@�1'@�1@��@��@�|�@�C�@�ȴ@�=q@�5?@�5?@���@��T@���@��^@���@��@�hs@�`B@�7L@�/@�&�@�/@�7L@���@��@��@�9X@�b@��m@��w@��@�K�@���@�ff@�$�@�J@�J@��#@���@���@�?}@�V@���@�z�@�b@���@�S�@��y@���@�n�@�E�@�{@��#@�O�@��@�%@��j@��9@���@�z�@�Q�@� �@�  @���@��@�K�@��@��@���@���@��\@�v�@�M�@�5?@�-@�$�@�J@���@�7L@���@�9X@�b@��@�t�@�C�@�
=@��H@���@��!@���@�~�@�ff@�$�@��@���@��7@�`B@�X@�G�@�/@��/@��@�z�@�(�@�b@�  @��@��
@���@��w@���@��@�K�@�"�@��H@���@��+@�V@�=q@�$�@���@�@��h@�G�@���@��@��u@�Q�@�1'@�;@|�@K�@;d@�@
=@~$�@}V@|�@|j@|(�@{�m@z�H@y��@y�@y%@x��@x��@x�u@x �@w�w@w+@v�R@vV@v@u�@u�@tz�@s�F@sdZ@s@r��@r�\@rn�@rn�@r�@q��@qhs@q&�@pĜ@p�@pA�@o�@o+@n�@nff@m�T@m�h@mV@l�@lz�@l9X@l�@l1@kƨ@kdZ@j��@j��@j^5@j-@j�@i��@iX@h�`@h�@h�@h�u@h�u@h�u@h�u@h�@hQ�@h  @gK�@f�y@f��@fV@e��@eV@d�@dj@cƨ@c33@b~�@a�^@`�u@`A�@_�@_\)@_
=@^��@^�R@^@]p�@\�D@\(�@[�m@[dZ@[C�@[C�@[33@[@Z�H@Z�!@Zn�@Z=q@Y�^@Y7L@X�9@X �@Xb@X  @W�@W�@W�;@W�@Wl�@W+@V@U�@UO�@U/@U�@T��@Tz�@T(�@S�
@SS�@R��@R�\@R~�@RM�@R=q@RJ@Q��@Q�^@Q��@Q%@P�u@P�@P �@O�P@N��@M�@Kƨ@Kt�@K33@K"�@K@J��@J�!@Jn�@Ix�@I&�@H��@HbN@Hb@G�P@F�+@F$�@E@E`B@EO�@E�@D�@Dz�@DZ@DI�@D(�@C�m@C��@Co@B��@B�\@BM�@A�7@AX@AX@AX@AX@A%@@��@@Ĝ@@��@@ �@?K�@?�@>�y@>�R@>v�@>V@>E�@>5?@>$�@>{@>@=��@=�@=`B@=?}@=/@=V@<��@<��@;ƨ@;dZ@;C�@;C�@;"�@:��@:�\@:~�@:^5@:M�@:=q@:J@9��@97L@9%@8��@8r�@81'@81'@8b@7�;@7�@6��@6ȴ@6��@6E�@6@5��@5@5��@5p�@5?}@5V@4��@4�/@4��@4�D@4�@3�m@3��@3C�@2�H@2~�@2M�@2�@2�@2-@2�@1��@1hs@1�@0��@0�9@0�u@0�@0r�@0Q�@0 �@0  @/�@/�;@/�w@/l�@/+@.�y@.�+@.v�@-�h@,��@,�@,��@,�@,�D@,j@+��@+�
@+��@+dZ@*�H@*n�@*=q@)��@)��@)�7@)�7@)�@(��@(�9@(1'@'��@'�@&�@&ȴ@&ȴ@&ȴ@&�R@&��@&�R@&�+@&v�@&V@&$�@%@%��@%V@$j@$I�@$�@#�m@#��@#C�@#o@"��@"��@"n�@"M�@"-@!��@!��@!��@!�7@!�7@!X@!%@ Ĝ@  �@�;@�w@|�@+@�@��@ff@5?@�@�-@�@p�@/@��@�/@�D@Z@9X@�F@t�@@�\@n�@n�@^5@^5@M�@=q@�@�@-@�@�@�^@G�@��@�9@Q�@�P@K�@K�@+@+@�@��@�y@v�@{@�@�h@`B@/@V@�j@I�@1@�
@�F@��@t�@t�@dZ@dZ@S�@S�@C�@C�@C�@C�@o@�!@M�@=q@J@��@�7@�7@x�@hs@X@hs@hs@&�@r�@bN@A�@�@��@l�@;d@�y@ȴ@�+@V@E�@$�@��@�-@�@��@�/@�@��@�D@z�@j@Z@I�@9X@�@��@�m@��@��@��@�
@�F@�@t�@t�@S�@C�@33@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�K�A�K�A�K�A�K�A�K�A�M�A�Q�A�K�A�M�A�K�A�K�A�M�A�O�A�S�A�XA�XA�XA�VA�VA�XA�VA�O�A�O�A�O�A�O�A�Q�A�S�A�ZA�XA�ZA�\)A�ZA�S�A�VA�n�Aɰ!Aɣ�Aɩ�A�ƨA���A�ƨA���Aɺ^Aɇ+A�9XA�1'A� �A�A���A�t�A�"�A��Aǣ�AǕ�AǇ+A�dZA�-A���A�A�A�A�A���Aô9A�~�A�\)A�O�A��A¸RA�O�A�x�A��PA�A�Q�A��A���A�A�z�A���A��RA��+A�VA��A�5?A�bA��A�1A�+A���A�$�A�r�A�|�A�/A�hsA���A�K�A��
A��yA��PA���A��yA��A��`A�`BA�XA�oA�t�A��A��DA��/A���A��HA��\A���A���A�-A�r�A��A�K�A�dZA�7LA���A�S�A���A��-A��TA��A���A���A�
=A;dA}"�A|5?Az��Aw"�AvQ�Aut�At9XAr~�AoK�An�HAn��An�AlVAj��Ai�wAit�AiO�Ag�
Ad^5AcS�AbA�A`(�A]�^A\�yA\JAZz�AX��AW��AWS�AVr�AU|�ATȴAS+AQ��AO%ALZAK"�AJ1AG��AF �AE/AC��AA�A@(�A>I�A<��A:ĜA9�A9hsA8bNA7�FA6A�A4^5A1�A17LA0�/A/�A.ffA-%A+�^A*��A(  A&��A&z�A%�TA$��A#�-A#G�A#%A"��A"�A ��A��A�;AK�A�AA�\AbA�DAG�Av�A�
A��A�A�/A�DA�AA+A��A(�A�A�FAG�AjA�A��A-A&�A��AJAXA
ffA
(�A	+A	dZA	��A	O�A	7LA	�mA
�HAx�A
�RA
1A	K�AE�A��A�PA�AC�A1A�7A ��@��@��y@�@�/@�Ĝ@�Z@��;@�t�@�+@���@���@��-@��-@�x�@�w@@�P@�@��@@�-@�j@��m@��H@�Z@�"�@���@�t�@�E�@�`B@�@�Q�@�dZ@ݡ�@ۅ@���@�x�@�%@׍P@���@�-@թ�@�V@�ƨ@ҸR@���@�/@�(�@�dZ@���@�C�@�|�@��m@Л�@�z�@�ȴ@�+@�~�@þw@��@��@��m@���@���@��@�-@���@���@�@���@��h@��7@�hs@�V@��D@��@��w@�C�@���@�ff@�$�@��7@�Z@��@���@��;@��;@�b@�t�@���@�E�@���@�z�@��@�hs@���@�Ĝ@�(�@�;d@���@�=q@��@���@���@���@�I�@���@�t�@��!@��-@�G�@�7L@�&�@��/@���@��@�A�@���@��@���@��@��\@�~�@�V@�J@���@���@��@�G�@�j@�  @��w@��P@�|�@�S�@��@�33@�Z@���@�z�@��u@��w@��@�b@��@� �@��@�  @��F@��@�t�@�+@�$�@���@�\)@�{@���@�`B@�V@�Ĝ@��@��u@�j@�Z@�I�@�1'@�1@��@��@�|�@�C�@�ȴ@�=q@�5?@�5?@���@��T@���@��^@���@��@�hs@�`B@�7L@�/@�&�@�/@�7L@���@��@��@�9X@�b@��m@��w@��@�K�@���@�ff@�$�@�J@�J@��#@���@���@�?}@�V@���@�z�@�b@���@�S�@��y@���@�n�@�E�@�{@��#@�O�@��@�%@��j@��9@���@�z�@�Q�@� �@�  @���@��@�K�@��@��@���@���@��\@�v�@�M�@�5?@�-@�$�@�J@���@�7L@���@�9X@�b@��@�t�@�C�@�
=@��H@���@��!@���@�~�@�ff@�$�@��@���@��7@�`B@�X@�G�@�/@��/@��@�z�@�(�@�b@�  @��@��
@���@��w@���@��@�K�@�"�@��H@���@��+@�V@�=q@�$�@���@�@��h@�G�@���@��@��u@�Q�@�1'@�;@|�@K�@;d@�@
=@~$�@}V@|�@|j@|(�@{�m@z�H@y��@y�@y%@x��@x��@x�u@x �@w�w@w+@v�R@vV@v@u�@u�@tz�@s�F@sdZ@s@r��@r�\@rn�@rn�@r�@q��@qhs@q&�@pĜ@p�@pA�@o�@o+@n�@nff@m�T@m�h@mV@l�@lz�@l9X@l�@l1@kƨ@kdZ@j��@j��@j^5@j-@j�@i��@iX@h�`@h�@h�@h�u@h�u@h�u@h�u@h�@hQ�@h  @gK�@f�y@f��@fV@e��@eV@d�@dj@cƨ@c33@b~�@a�^@`�u@`A�@_�@_\)@_
=@^��@^�R@^@]p�@\�D@\(�@[�m@[dZ@[C�@[C�@[33@[@Z�H@Z�!@Zn�@Z=q@Y�^@Y7L@X�9@X �@Xb@X  @W�@W�@W�;@W�@Wl�@W+@V@U�@UO�@U/@U�@T��@Tz�@T(�@S�
@SS�@R��@R�\@R~�@RM�@R=q@RJ@Q��@Q�^@Q��@Q%@P�u@P�@P �@O�P@N��@M�@Kƨ@Kt�@K33@K"�@K@J��@J�!@Jn�@Ix�@I&�@H��@HbN@Hb@G�P@F�+@F$�@E@E`B@EO�@E�@D�@Dz�@DZ@DI�@D(�@C�m@C��@Co@B��@B�\@BM�@A�7@AX@AX@AX@AX@A%@@��@@Ĝ@@��@@ �@?K�@?�@>�y@>�R@>v�@>V@>E�@>5?@>$�@>{@>@=��@=�@=`B@=?}@=/@=V@<��@<��@;ƨ@;dZ@;C�@;C�@;"�@:��@:�\@:~�@:^5@:M�@:=q@:J@9��@97L@9%@8��@8r�@81'@81'@8b@7�;@7�@6��@6ȴ@6��@6E�@6@5��@5@5��@5p�@5?}@5V@4��@4�/@4��@4�D@4�@3�m@3��@3C�@2�H@2~�@2M�@2�@2�@2-@2�@1��@1hs@1�@0��@0�9@0�u@0�@0r�@0Q�@0 �@0  @/�@/�;@/�w@/l�@/+@.�y@.�+@.v�@-�h@,��@,�@,��@,�@,�D@,j@+��@+�
@+��@+dZ@*�H@*n�@*=q@)��@)��@)�7@)�7@)�@(��@(�9@(1'@'��@'�@&�@&ȴ@&ȴ@&ȴ@&�R@&��@&�R@&�+@&v�@&V@&$�@%@%��@%V@$j@$I�@$�@#�m@#��@#C�@#o@"��@"��@"n�@"M�@"-@!��@!��@!��@!�7@!�7@!X@!%@ Ĝ@  �@�;@�w@|�@+@�@��@ff@5?@�@�-@�@p�@/@��@�/@�D@Z@9X@�F@t�@@�\@n�@n�@^5@^5@M�@=q@�@�@-@�@�@�^@G�@��@�9@Q�@�P@K�@K�@+@+@�@��@�y@v�@{@�@�h@`B@/@V@�j@I�@1@�
@�F@��@t�@t�@dZ@dZ@S�@S�@C�@C�@C�@C�@o@�!@M�@=q@J@��@�7@�7@x�@hs@X@hs@hs@&�@r�@bN@A�@�@��@l�@;d@�y@ȴ@�+@V@E�@$�@��@�-@�@��@�/@�@��@�D@z�@j@Z@I�@9X@�@��@�m@��@��@��@�
@�F@�@t�@t�@S�@C�@33@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�?B�!B�-B�^B�wB��B��BŢB��B�ZB�fB�mB�B�B��B	7BhB�B�B�B�B �B'�B49BK�Bk�Bl�Bq�Bs�Bs�Bw�B}�B�B�hB��B�B�'B�?B�RB�9B��B��B��B�oB�PB� B�7B�bB��B�uB�DB�%Bx�BaHBN�BE�B@�B49B.B�BVB��B�B�`B�#B��B��B��B�wB�LB�-B��B��B��B�1B�B}�Bo�BffBZBP�B@�B%�B{B	7BB
��B
�B
�/B
�B
��B
�-B
�uB
�7B
z�B
q�B
gmB
N�B
A�B
;dB
2-B
(�B
\B

=B
1B
B	��B	�B	�mB	�ZB	�NB	�/B	ȴB	�}B	�LB	�'B	��B	��B	��B	�\B	�%B	�B	{�B	x�B	p�B	l�B	bNB	XB	F�B	5?B	/B	,B	"�B	�B	bB	DB	B��B��B�B�yB�ZB�NB�/B�B��B��BB�}B�wB�dB�LB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�{B�{B�{B�{B�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�9B�?B�RB�XB�dB�XB�dBÖBĜB��B��B��B�5B�mB�B��B	PB	�B	�B	�B	�B	VB	JB	B	B��B�B�B�B�yB�sB�mB�TB�HB�BB�/B�#B�B�B�
B�
B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺB��B��B��B��B�
B�
B�B�)B�5B�;B�BB�HB�NB�TB�`B�fB�sB�B�B��B��B��B��B	B	oB	�B	�B	'�B	+B	%�B	�B	PB	+B	B	JB	
=B	
=B	JB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	(�B	,B	.B	33B	49B	6FB	:^B	;dB	=qB	>wB	A�B	F�B	H�B	H�B	F�B	F�B	@�B	>wB	?}B	@�B	?}B	?}B	F�B	J�B	K�B	M�B	P�B	Q�B	R�B	S�B	XB	XB	]/B	cTB	e`B	ffB	ffB	gmB	hsB	iyB	jB	l�B	l�B	m�B	r�B	s�B	s�B	u�B	x�B	z�B	z�B	z�B	|�B	�B	�+B	�=B	�DB	�JB	�PB	�oB	��B	��B	��B	��B	�B	�B	�B	�-B	�-B	�3B	�3B	�3B	�3B	�3B	�-B	�-B	�!B	�B	�B	��B	�B	�B	�-B	�-B	�3B	�3B	�3B	�9B	�9B	�?B	�FB	�LB	�XB	�^B	�jB	��B	��B	B	ĜB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�/B	�;B	�BB	�NB	�fB	�sB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
B
%B
%B
%B
%B
%B
%B
%B
%B
+B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
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
oB
oB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
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
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
/B
.B
.B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
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
49B
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
7LB
7LB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
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
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
@�B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
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
K�B
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
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
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
XB
YB
YB
YB
YB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
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
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
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
k�B
k�B
k�B
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
p�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
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
w�B
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
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�sB��B�%B�;B�-B�xB��B��B��B�?BԕB�tB�B��B�B�vB��B	�BB�B�BB]B!�B)�B6�BNVBk�BmBrBtBt�Bx�BcB��B��B�zB�OB��B�fB��B�fB�*B�OB�B��B��B�MB��B�B��B�B��B�^B�BezBQ�BHBC�B6�B2�B�B�B��B�%B�B�~B��B��B�uB��B�lB��B��B�:B�sB��B�+B�4Bq�BhXB\]BT�BD�B(�B�B
XB�B
�}B
�qB
ޞB
�=B
��B
��B
�MB
��B
|�B
tB
kB
P.B
B�B
=VB
4�B
+�B
B

�B
	�B
?B	��B	�B	�$B	�`B	��B	��B	�rB	�oB	�B	��B	�-B	�)B	�B	�4B	��B	�'B	}<B	z*B	rB	n�B	d�B	[�B	I�B	6�B	1B	.�B	%FB	QB	�B	"B	�B�qB�B��B�B�zB��BޞB�]BյB��B�aB��B�B��B�>B�B�;B�B�B�nB��B�NB�B�xB�CB��B�B��B��B��B�gB��B��B�gB��B��B�\B��B��B�OB��B�;B��B��B�vB��B��B��B�yB��B�B�OB�ZB��B��B�B��B��B��B��B�SB��B��B�B��B�RB�B�B	PB	�B	�B	�B	
B	�B	�B	�B	gB��B��B��B�B�0B�*B�
B��B��B��BݲBیBںB��B�yB��B�mB��B�6B�B�B�B�VB� B��BˬB��B̘B��B�xB� B��BԯBյB��B�+BۦBݲB�B�\B��B�hB� B�B��B�8B�yB�B�nB��B��B��B�jB	B	oB	mB	�B	(�B	,�B	(�B	�B	\B	�B	B	B	
�B	
�B	B	B	�B	�B	�B	�B	�B	�B	�B	)B	OB	 \B	&LB	)yB	,�B	.�B	3�B	4�B	72B	:�B	;�B	=�B	>�B	A�B	GEB	I�B	I�B	H�B	H�B	A�B	?�B	@ B	@�B	@4B	@iB	G+B	KDB	L~B	NVB	QNB	RTB	S[B	T�B	X_B	X�B	]�B	c�B	e�B	f�B	f�B	g�B	h�B	i�B	j�B	l�B	l�B	ncB	sB	tB	tB	v+B	y>B	{0B	{JB	{dB	}�B	�{B	�zB	��B	�xB	�~B	��B	�oB	�
B	��B	�B	�,B	��B	�"B	�B	�aB	�aB	��B	��B	��B	��B	��B	��B	�B	�AB	�UB	��B	��B	�wB	��B	��B	�aB	�hB	��B	�hB	��B	�nB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�B	�B	�B	�B	� B	�4B	�TB	�YB	�YB	�eB	�dB	ߊB	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�%B	�FB	�2B	�$B	�DB	�JB	�PB	�6B	�"B	�<B	�BB	�wB
 4B
 OB
 OB
UB
;B
;B
;B
UB
AB
[B
aB
{B
�B
MB
mB
SB
SB
mB
mB
SB
YB
YB
�B
�B
�B
�B
�B
tB
�B
tB
�B
tB
_B
tB
tB
_B
_B
fB
�B
	�B
	lB
	�B
	�B
	lB
	�B
	�B
	�B

�B

�B

�B

rB
	�B
	�B
	lB

rB

�B

rB
�B
�B
�B
�B
xB
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
7B
	B
�B
�B
�B
�B
	B
B
�B
B
B
B
!B
B
!B
 BB
 �B
!-B
"B
"B
"B
"B
"B
"B
#B
#B
#:B
$B
$&B
$@B
%FB
&B
&LB
&2B
'8B
'RB
($B
(>B
)DB
)*B
)DB
)*B
)_B
)DB
*KB
*0B
*KB
*KB
*KB
+6B
+kB
,WB
,"B
,=B
,=B
,"B
,=B
,=B
,WB
,qB
,WB
-]B
-CB
.}B
.cB
.}B
/OB
.cB
.}B
/�B
0�B
0�B
1�B
1[B
1�B
1vB
1[B
1vB
1vB
1�B
2|B
2�B
3�B
3hB
3�B
3hB
3MB
3�B
3hB
4�B
4nB
4nB
4nB
4�B
5�B
5�B
6�B
7�B
7fB
7�B
7fB
7�B
7�B
7�B
7�B
7�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
;�B
;�B
;�B
<�B
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
>B
>B
?B
@�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
BB
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
FB
F�B
F�B
F�B
G�B
HB
IB
H�B
H�B
H�B
IB
J	B
I�B
I�B
J#B
K)B
KB
J�B
J�B
LB
LB
K�B
K�B
K�B
K�B
LB
K�B
MB
MB
MB
MB
MB
NB
N<B
N<B
OB
O(B
OB
O(B
P.B
PB
O�B
P.B
PB
PB
P.B
PHB
QNB
Q4B
Q4B
R:B
R:B
R B
R B
R:B
R:B
RTB
S@B
S@B
S@B
TFB
TFB
T,B
T,B
TFB
TFB
TFB
T,B
T,B
T,B
TFB
TaB
U2B
U2B
U2B
UgB
VSB
VSB
VSB
V9B
V9B
VSB
V9B
VSB
WYB
W?B
WYB
W?B
W?B
W?B
WYB
WYB
X_B
X+B
X_B
XEB
XEB
X_B
YKB
YKB
YeB
YB
ZkB
[WB
\]B
\CB
\xB
\xB
\xB
\xB
\]B
]~B
]�B
]~B
^jB
^jB
^jB
^OB
^�B
^�B
_pB
_pB
_�B
`�B
`�B
a|B
a|B
abB
abB
abB
a|B
abB
a|B
a|B
a�B
a|B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
ezB
e�B
ezB
e�B
e�B
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
h�B
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
k�B
k�B
k�B
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
p�B
p�B
p�B
o�B
o�B
p�B
p�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
s�B
t�B
vB
u�B
v�B
wB
wB
v�B
wB
xB
xB
xB
xB
xB
xB
xB
xB
xB
w�B
w�B
xB
w�B
w�B
xB
x�B
y$B
y	B
y	B
y�B
y�B
y�B
zB
z*B
{0B
z�B
{B
{0B
z�B
z�B
z�B
{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.3(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201702040036002017020400360020170204003600201806221308372018062213083720180622130837201804050709192018040507091920180405070919  JA  ARFMdecpA19c                                                                20170131093508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170131003533  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170131003534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170131003534  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170131003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170131003535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170131003535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170131003535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170131003535  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170131003535                      G�O�G�O�G�O�                JA  ARUP                                                                        20170131010326                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170131153901  CV  JULD            G�O�G�O�F�k�                JM  ARGQJMQC2.0                                                                 20170131153901  CV  JULD_LOCATION   G�O�G�O�F�k�                JM  ARGQJMQC2.0                                                                 20170131153901  CV  LATITUDE        G�O�G�O�A�                JM  ARGQJMQC2.0                                                                 20170131153901  CV  LONGITUDE       G�O�G�O��&̋                JM  ARCAJMQC2.0                                                                 20170203153600  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170203153600  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220919  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040837  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                