CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-05-30T10:00:42Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  px   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ވ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޸   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20200530100042  20200530100042  4903320 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  8282                            2B  A   NAVIS_A                         1161                            170425                          863 @�Xܺ��1   @�Yq�)@8��E��c��+J1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   A   A   @���@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�C3Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�ff@�ffA33A;33A[33A{33A���A�fgA���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>fgBF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC��gC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��gC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTs3DT�3DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dw��Dxl�Dx��Dyl�Dy��Dzl�Dz��D{l�D{��D|l�D|��D}l�D}��D~l�D~��Dl�D��D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�y�D���D��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�s3D��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD¶fD��fD�6fD�vfDöfD��fD�6fD�vfDĶfD��fD�6fD�vfDŶfD��fD�9�D�vfDƶfD��fD�6fD�vfDǶfD��fD�6fD�vfDȶfD��fD�6fD�vfDɶfD��fD�6fD�vfDʶfD��fD�6fD�vfD˶fD��fD�6fD�vfD̶fD��fD�6fD�vfDͶfD��fD�6fD�vfDζfD��fD�6fD�vfD϶fD��fD�6fD�vfDжfD��fD�6fD�vfDѶfD��fD�6fD�vfDҶfD��fD�6fD�vfDӶfD��fD�6fD�vfDԶfD��fD�6fD�vfDնfD��fD�6fD�vfDֶfD��fD�6fD�vfD׶fD��fD�6fD�vfDضfD��fD�6fD�vfDٶfD��fD�6fD�vfDڶfD��fD�6fD�vfD۶fD��fD�6fD�vfDܶfD��fD�6fD�vfDݶfD��fD�6fD�vfD޶fD��fD�6fD�vfD߶fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�<�D�vfD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A�M�A�O�A�M�A�K�A�K�A�?}A�5?A���A�XA�bAŁA�1'A��mAě�A�n�A�ZA�1'A�G�A�A�XA��A�ffA��yA��hA�9XA��A���A�1'A��TA��;A��
A��-A��A��uA���A��RA�&�A���A�-A�r�A��^A�XA�
=A�VA�K�A���A�/A��A�jA��
A���A�7LA��mA��DA�I�A�$�A���A��^A��FA�VA���A��7A���A�ȴA���A��A�XA�1'A�
=A�r�A���A�7LA�dZA��jA�ƨA��7A�t�A��A���A���A��
A�;dA��DA��A���A��A��A��DA�Q�A�A~ �A}�
A}�7A|A�A{l�Az�+Awp�AvJAu"�Asx�Ar��ArbAp��An��AmO�Ak�Aj�+Ah �Af��AchsAc�AbI�A`�uA\ȴAZ�AX��AW�#AW|�AVI�ATQ�AShsAQ��APJAO
=AN{AJ�jAHbAF��AD��AC
=A@�+A>��A<�yA9�PA7�#A6(�A5"�A41A2bNA0��A0��A/�A.ȴA-��A,��A,v�A,�A+��A+G�A*��A*(�A)33A(^5A';dA%�A%"�A$z�A#��A"�`A!dZA ��A E�A��AO�A-A�PA�A9XA{A�PAffA�A��A��AffAt�A�A��A��A�A��A�/A��A��A��A�A	�wA	VA�A��A9XA�7A��A�A�yA�PA�RA��A�^AXAG�A"�A ��A �j@���@��R@�O�@���@��`@�|�@���@�dZ@�@�^@���@�9@��m@�w@@�h@�l�@�J@�&�@�(�@�+@���@��@�\)@�+@�hs@߶F@߅@���@���@�b@�-@�o@�^5@ղ-@��@Դ9@Դ9@ӥ�@���@љ�@ѩ�@��@мj@�S�@�S�@�~�@�v�@�%@�ƨ@��H@��@�Ĝ@�ƨ@�dZ@�ȴ@��@�Z@Å@�v�@��@��@��@��@�j@���@�@��@�bN@��@��;@��P@��@���@�5?@�ff@�ff@�V@���@���@��@��@���@�^5@�@��@��@�
=@���@�V@���@�7L@��@��9@�9X@���@�|�@��H@�E�@�J@���@�?}@���@��@��R@�G�@�r�@��@��-@��@��P@�C�@�ff@�G�@���@���@��P@��@�ƨ@���@�C�@�n�@�5?@�E�@��@���@��9@��@�Z@�I�@�9X@�b@�  @��@�t�@�E�@��`@��H@���@�&�@���@�Ĝ@���@�r�@�1'@��@�  @���@��P@�|�@�\)@��@�n�@�5?@��@��7@�x�@�hs@��@�bN@�1@�  @��;@�ƨ@��w@��w@��@���@��@�dZ@�33@���@��+@�V@�$�@��T@���@�x�@�hs@�G�@��@���@��j@��u@�Z@�1'@�  @��
@��F@�33@��@��H@���@���@���@���@���@�v�@�E�@�@���@���@��@��@���@��`@�A�@�9X@�b@�b@�b@�  @���@�ƨ@���@��P@�l�@�K�@�o@�o@�
=@�ȴ@�{@���@�hs@�V@���@���@��u@��@�bN@�I�@�1'@�1@��
@��w@�33@��@��H@���@�^5@�J@��@���@�/@��@�l�@��^@�`B@��/@�z�@�9X@�;@+@~�@~�R@~5?@~{@~$�@}�h@|z�@|j@|Z@|9X@|1@{�m@{ƨ@{ƨ@{�@{dZ@{"�@z��@z~�@z^5@z�@y�@y��@y��@y��@y��@y�@yX@y%@xĜ@x�@x1'@w�w@w\)@w\)@w\)@wK�@w�@v�y@v�R@vV@v5?@u@u�h@up�@uO�@t��@t�@t�j@t��@tj@t1@s�@rJ@q�^@q��@q��@r-@r^5@q��@q%@pĜ@p�@pQ�@pQ�@pQ�@p�9@pbN@p�u@q��@q�@nȴ@n��@n�@n��@o
=@n��@nff@n$�@m�-@n5?@n��@nff@nff@nE�@n$�@m�T@m�-@m?}@l��@l�@kƨ@kdZ@ko@j��@j~�@j�@i��@i��@ihs@i�@h��@hr�@hbN@hQ�@hA�@hA�@hA�@h1'@g�@g��@g��@g;d@f�@fȴ@f�+@e@ep�@e?}@d�@dZ@d1@c�m@c��@ct�@cdZ@cS�@cdZ@cC�@b�@b�!@bn�@b^5@b�@a�7@aG�@a7L@a�@`r�@`b@_��@_
=@]@]�@\��@[ƨ@[dZ@[C�@["�@Z�@Z�\@Y�#@Y7L@Y%@Y%@X��@XbN@Wl�@V��@U�@U?}@T�D@St�@Rn�@R~�@R=q@R-@RJ@R-@Q�@QX@P�`@P�@PbN@P �@O��@O�@O�P@O|�@O\)@O\)@OK�@O;d@O+@N�@N5?@M�T@M@M�@Mp�@M`B@MO�@M�@L��@LZ@K�
@Kƨ@K�F@K��@K33@K@J��@J��@J�\@J^5@J-@I�#@H�`@H��@Hr�@H �@G�P@F�@Fȴ@F�+@F5?@E�h@Ep�@E�@E�@EO�@D�@D��@Dz�@D�@C�m@C��@C"�@B��@B~�@B-@A��@A��@A�^@A�^@A�^@A��@Ax�@AX@AX@AG�@AG�@AG�@A&�@@�`@@��@@bN@?�@?l�@>ȴ@>v�@>ff@>@=�T@=�-@=`B@=/@=V@<��@<��@<�@<�j@<j@<�@;��@;ƨ@;��@;o@:��@:�!@:�\@:M�@9��@9�^@9�7@8��@8A�@8b@7�@7�;@7�w@7+@6v�@6$�@6{@5@5/@4�/@4�@4Z@3�
@3�@3dZ@3S�@3"�@2��@2��@2~�@2=q@1x�@1�@0�`@0Ĝ@0��@0�@0Q�@0  @/��@/��@/K�@/
=@.�@.$�@-�h@-p�@-O�@-V@,�@,z�@,I�@,�@+ƨ@+dZ@*~�@*=q@*=q@*�@*J@*J@)�@)�@)�^@)&�@(�9@(�u@(�@(bN@(1'@'�@'�@'|�@'l�@'\)@'�@&ȴ@&V@&$�@&@%��@%p�@%O�@%�@$��@$�/@$��@$��@$z�@$(�@#��@#��@#dZ@#"�@#@"�H@"��@"�!@"��@"��@"~�@"M�@"=q@"-@!��@!hs@!X@!G�@ ��@ �u@ �@ bN@ 1'@   @|�@;d@
=@�y@ȴ@��@ff@$�@$�@{@@�-@�h@`B@?}@�@�j@�D@Z@(�@dZ@S�@C�@o@�@�@�H@��@��@�!@��@n�@^5@��@�#@��@�^@7L@%@��@��@�@Q�@A�@b@�w@��@�P@|�@\)@+@��@�@�R@v�@5?@@��@�@V@�/@Z@9X@(�@�@��@�m@�m@�@dZ@S�@"�@o@@@@�@��@��@~�@J@�@�#@�#@�^@�7@�7@x�@�@�u@Q�@1'@ �@�@��@��@l�@�@�y@�@�R@�+@$�@�h@�@`B@O�@/@�@��@��@j@I�@9X@�m@�@S�@S�@C�@33@"�@"�@o@@
��@
n�@	��@	��@	��@	x�@	hs@	G�@	�@��@��@�@Q�@ �@  @�@�w@�@��@l�@;d@+@+@
=@
=@�y@�@�@��@��@�+@V@{@�T@�-@��@�@V@�/@�j@9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�C�A�M�A�O�A�M�A�K�A�K�A�?}A�5?A���A�XA�bAŁA�1'A��mAě�A�n�A�ZA�1'A�G�A�A�XA��A�ffA��yA��hA�9XA��A���A�1'A��TA��;A��
A��-A��A��uA���A��RA�&�A���A�-A�r�A��^A�XA�
=A�VA�K�A���A�/A��A�jA��
A���A�7LA��mA��DA�I�A�$�A���A��^A��FA�VA���A��7A���A�ȴA���A��A�XA�1'A�
=A�r�A���A�7LA�dZA��jA�ƨA��7A�t�A��A���A���A��
A�;dA��DA��A���A��A��A��DA�Q�A�A~ �A}�
A}�7A|A�A{l�Az�+Awp�AvJAu"�Asx�Ar��ArbAp��An��AmO�Ak�Aj�+Ah �Af��AchsAc�AbI�A`�uA\ȴAZ�AX��AW�#AW|�AVI�ATQ�AShsAQ��APJAO
=AN{AJ�jAHbAF��AD��AC
=A@�+A>��A<�yA9�PA7�#A6(�A5"�A41A2bNA0��A0��A/�A.ȴA-��A,��A,v�A,�A+��A+G�A*��A*(�A)33A(^5A';dA%�A%"�A$z�A#��A"�`A!dZA ��A E�A��AO�A-A�PA�A9XA{A�PAffA�A��A��AffAt�A�A��A��A�A��A�/A��A��A��A�A	�wA	VA�A��A9XA�7A��A�A�yA�PA�RA��A�^AXAG�A"�A ��A �j@���@��R@�O�@���@��`@�|�@���@�dZ@�@�^@���@�9@��m@�w@@�h@�l�@�J@�&�@�(�@�+@���@��@�\)@�+@�hs@߶F@߅@���@���@�b@�-@�o@�^5@ղ-@��@Դ9@Դ9@ӥ�@���@љ�@ѩ�@��@мj@�S�@�S�@�~�@�v�@�%@�ƨ@��H@��@�Ĝ@�ƨ@�dZ@�ȴ@��@�Z@Å@�v�@��@��@��@��@�j@���@�@��@�bN@��@��;@��P@��@���@�5?@�ff@�ff@�V@���@���@��@��@���@�^5@�@��@��@�
=@���@�V@���@�7L@��@��9@�9X@���@�|�@��H@�E�@�J@���@�?}@���@��@��R@�G�@�r�@��@��-@��@��P@�C�@�ff@�G�@���@���@��P@��@�ƨ@���@�C�@�n�@�5?@�E�@��@���@��9@��@�Z@�I�@�9X@�b@�  @��@�t�@�E�@��`@��H@���@�&�@���@�Ĝ@���@�r�@�1'@��@�  @���@��P@�|�@�\)@��@�n�@�5?@��@��7@�x�@�hs@��@�bN@�1@�  @��;@�ƨ@��w@��w@��@���@��@�dZ@�33@���@��+@�V@�$�@��T@���@�x�@�hs@�G�@��@���@��j@��u@�Z@�1'@�  @��
@��F@�33@��@��H@���@���@���@���@���@�v�@�E�@�@���@���@��@��@���@��`@�A�@�9X@�b@�b@�b@�  @���@�ƨ@���@��P@�l�@�K�@�o@�o@�
=@�ȴ@�{@���@�hs@�V@���@���@��u@��@�bN@�I�@�1'@�1@��
@��w@�33@��@��H@���@�^5@�J@��@���@�/@��@�l�@��^@�`B@��/@�z�@�9X@�;@+@~�@~�R@~5?@~{@~$�@}�h@|z�@|j@|Z@|9X@|1@{�m@{ƨ@{ƨ@{�@{dZ@{"�@z��@z~�@z^5@z�@y�@y��@y��@y��@y��@y�@yX@y%@xĜ@x�@x1'@w�w@w\)@w\)@w\)@wK�@w�@v�y@v�R@vV@v5?@u@u�h@up�@uO�@t��@t�@t�j@t��@tj@t1@s�@rJ@q�^@q��@q��@r-@r^5@q��@q%@pĜ@p�@pQ�@pQ�@pQ�@p�9@pbN@p�u@q��@q�@nȴ@n��@n�@n��@o
=@n��@nff@n$�@m�-@n5?@n��@nff@nff@nE�@n$�@m�T@m�-@m?}@l��@l�@kƨ@kdZ@ko@j��@j~�@j�@i��@i��@ihs@i�@h��@hr�@hbN@hQ�@hA�@hA�@hA�@h1'@g�@g��@g��@g;d@f�@fȴ@f�+@e@ep�@e?}@d�@dZ@d1@c�m@c��@ct�@cdZ@cS�@cdZ@cC�@b�@b�!@bn�@b^5@b�@a�7@aG�@a7L@a�@`r�@`b@_��@_
=@]@]�@\��@[ƨ@[dZ@[C�@["�@Z�@Z�\@Y�#@Y7L@Y%@Y%@X��@XbN@Wl�@V��@U�@U?}@T�D@St�@Rn�@R~�@R=q@R-@RJ@R-@Q�@QX@P�`@P�@PbN@P �@O��@O�@O�P@O|�@O\)@O\)@OK�@O;d@O+@N�@N5?@M�T@M@M�@Mp�@M`B@MO�@M�@L��@LZ@K�
@Kƨ@K�F@K��@K33@K@J��@J��@J�\@J^5@J-@I�#@H�`@H��@Hr�@H �@G�P@F�@Fȴ@F�+@F5?@E�h@Ep�@E�@E�@EO�@D�@D��@Dz�@D�@C�m@C��@C"�@B��@B~�@B-@A��@A��@A�^@A�^@A�^@A��@Ax�@AX@AX@AG�@AG�@AG�@A&�@@�`@@��@@bN@?�@?l�@>ȴ@>v�@>ff@>@=�T@=�-@=`B@=/@=V@<��@<��@<�@<�j@<j@<�@;��@;ƨ@;��@;o@:��@:�!@:�\@:M�@9��@9�^@9�7@8��@8A�@8b@7�@7�;@7�w@7+@6v�@6$�@6{@5@5/@4�/@4�@4Z@3�
@3�@3dZ@3S�@3"�@2��@2��@2~�@2=q@1x�@1�@0�`@0Ĝ@0��@0�@0Q�@0  @/��@/��@/K�@/
=@.�@.$�@-�h@-p�@-O�@-V@,�@,z�@,I�@,�@+ƨ@+dZ@*~�@*=q@*=q@*�@*J@*J@)�@)�@)�^@)&�@(�9@(�u@(�@(bN@(1'@'�@'�@'|�@'l�@'\)@'�@&ȴ@&V@&$�@&@%��@%p�@%O�@%�@$��@$�/@$��@$��@$z�@$(�@#��@#��@#dZ@#"�@#@"�H@"��@"�!@"��@"��@"~�@"M�@"=q@"-@!��@!hs@!X@!G�@ ��@ �u@ �@ bN@ 1'@   @|�@;d@
=@�y@ȴ@��@ff@$�@$�@{@@�-@�h@`B@?}@�@�j@�D@Z@(�@dZ@S�@C�@o@�@�@�H@��@��@�!@��@n�@^5@��@�#@��@�^@7L@%@��@��@�@Q�@A�@b@�w@��@�P@|�@\)@+@��@�@�R@v�@5?@@��@�@V@�/@Z@9X@(�@�@��@�m@�m@�@dZ@S�@"�@o@@@@�@��@��@~�@J@�@�#@�#@�^@�7@�7@x�@�@�u@Q�@1'@ �@�@��@��@l�@�@�y@�@�R@�+@$�@�h@�@`B@O�@/@�@��@��@j@I�@9X@�m@�@S�@S�@C�@33@"�@"�@o@@
��@
n�@	��@	��@	��@	x�@	hs@	G�@	�@��@��@�@Q�@ �@  @�@�w@�@��@l�@;d@+@+@
=@
=@�y@�@�@��@��@�+@V@{@�T@�-@��@�@V@�/@�j@9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�FB�?B�?B�?B�?B�?B�9B�9B�'B�B��B�B�B�B�3B�3B�-B�!B�B��B��B�!B��B�B�B�/B�;B�TB�sB�B�B�B�B��BDB�B�B(�B/B.B5?B2-B6FB33B1'B+B �B�B�BoB
=BB��B�B�5B�B��BÖB�?B��B��B�%Bx�Bs�Br�Bp�Bn�Bm�BjBgmBaHBT�BM�BB�B5?B$�B�BPBB
��B
�sB
��B
�}B
�3B
��B
��B
�PB
�B
o�B
[#B
G�B
7LB
33B
0!B
%�B
�B
�B	��B	�B	�B	�BB	�B	��B	ȴB	�FB	��B	��B	��B	�+B	� B	jB	dZB	^5B	O�B	;dB	)�B	 �B	�B	{B	VB	B��B��B��B�B�B�5B��BȴB�jB�-B��B��B��B��B�VB�%B�B~�Bz�Bs�Br�Bp�Bm�Bk�BjBhsBgmBgmBffBe`BdZBbNB`BB`BB\)B[#BZBXBVBT�BR�BR�BP�BO�BO�BN�BM�BL�BK�BK�BI�BG�BF�BD�BA�B?}B=qB:^B;dB;dB:^B8RB6FB5?B1'B/B.B,B+B)�B(�B'�B%�B%�B$�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B{BuBoBhBbBbB\B\BoBuB{BoBbBVBVBPBVB\BVBPBVBVB\BVB\BhBbB{B�B�B�B�B�B�B�B�B$�B/B7LB8RB:^BI�BL�BQ�BT�BW
BYB[#B^5B`BBe`BiyBk�Bm�Bo�Bs�Bu�Bv�By�Bz�Bz�Bz�Bx�Bw�Bw�By�By�Bz�Bz�B�B�B�7B�VB�hB�bB�PB�PB�JB�JB�VB�{B��B��B��B�LB�qB��B�jB�dB�dB�^B�^B�qBB��B��BĜB��B��B��B��BŢBĜBǮBĜB��B��BÖBĜBĜBƨB��B��B��B��B�B�B�
B�
B�)B�;B�TB�`B�fB�mB�sB�yB�B�B�B��B	B	  B��B��B��B	  B	B	B	B	+B	+B	1B	
=B	DB	DB	JB	\B	uB	{B	�B	�B	�B	�B	�B	 �B	#�B	%�B	'�B	)�B	)�B	)�B	+B	,B	/B	2-B	49B	6FB	;dB	=qB	>wB	A�B	B�B	C�B	D�B	E�B	F�B	H�B	J�B	L�B	M�B	O�B	P�B	Q�B	R�B	VB	YB	^5B	`BB	aHB	bNB	bNB	cTB	e`B	k�B	n�B	o�B	o�B	o�B	p�B	r�B	r�B	r�B	s�B	u�B	w�B	y�B	z�B	}�B	~�B	� B	� B	�B	�B	�B	�%B	�%B	�1B	�JB	�PB	�VB	�VB	�\B	�\B	�\B	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�LB	�LB	�RB	�RB	�RB	�XB	�wB	��B	B	ÖB	ĜB	ƨB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
	7B
1B
1B
1B
1B
+B
%B
+B
1B
	7B

=B
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
VB
VB
VB
\B
bB
bB
hB
oB
oB
hB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
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
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
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
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
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
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
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
N�B
N�B
N�B
N�B
O�B
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
S�B
S�B
S�B
S�B
S�B
S�B
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
VB
VB
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
^5B
^5B
^5B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
_;B
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
jB
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
l�B
l�B
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
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�FB�?B�?B�?B�?B�?B�9B�9B�'B�B��B�B�B�B�3B�3B�-B�!B�B��B��B�!B��B�B�B�/B�;B�TB�sB�B�B�B�B��BDB�B�B(�B/B.B5?B2-B6FB33B1'B+B �B�B�BoB
=BB��B�B�5B�B��BÖB�?B��B��B�%Bx�Bs�Br�Bp�Bn�Bm�BjBgmBaHBT�BM�BB�B5?B$�B�BPBB
��B
�sB
��B
�}B
�3B
��B
��B
�PB
�B
o�B
[#B
G�B
7LB
33B
0!B
%�B
�B
�B	��B	�B	�B	�BB	�B	��B	ȴB	�FB	��B	��B	��B	�+B	� B	jB	dZB	^5B	O�B	;dB	)�B	 �B	�B	{B	VB	B��B��B��B�B�B�5B��BȴB�jB�-B��B��B��B��B�VB�%B�B~�Bz�Bs�Br�Bp�Bm�Bk�BjBhsBgmBgmBffBe`BdZBbNB`BB`BB\)B[#BZBXBVBT�BR�BR�BP�BO�BO�BN�BM�BL�BK�BK�BI�BG�BF�BD�BA�B?}B=qB:^B;dB;dB:^B8RB6FB5?B1'B/B.B,B+B)�B(�B'�B%�B%�B$�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B{BuBoBhBbBbB\B\BoBuB{BoBbBVBVBPBVB\BVBPBVBVB\BVB\BhBbB{B�B�B�B�B�B�B�B�B$�B/B7LB8RB:^BI�BL�BQ�BT�BW
BYB[#B^5B`BBe`BiyBk�Bm�Bo�Bs�Bu�Bv�By�Bz�Bz�Bz�Bx�Bw�Bw�By�By�Bz�Bz�B�B�B�7B�VB�hB�bB�PB�PB�JB�JB�VB�{B��B��B��B�LB�qB��B�jB�dB�dB�^B�^B�qBB��B��BĜB��B��B��B��BŢBĜBǮBĜB��B��BÖBĜBĜBƨB��B��B��B��B�B�B�
B�
B�)B�;B�TB�`B�fB�mB�sB�yB�B�B�B��B	B	  B��B��B��B	  B	B	B	B	+B	+B	1B	
=B	DB	DB	JB	\B	uB	{B	�B	�B	�B	�B	�B	 �B	#�B	%�B	'�B	)�B	)�B	)�B	+B	,B	/B	2-B	49B	6FB	;dB	=qB	>wB	A�B	B�B	C�B	D�B	E�B	F�B	H�B	J�B	L�B	M�B	O�B	P�B	Q�B	R�B	VB	YB	^5B	`BB	aHB	bNB	bNB	cTB	e`B	k�B	n�B	o�B	o�B	o�B	p�B	r�B	r�B	r�B	s�B	u�B	w�B	y�B	z�B	}�B	~�B	� B	� B	�B	�B	�B	�%B	�%B	�1B	�JB	�PB	�VB	�VB	�\B	�\B	�\B	�\B	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�LB	�LB	�RB	�RB	�RB	�XB	�wB	��B	B	ÖB	ĜB	ƨB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
	7B
1B
1B
1B
1B
+B
%B
+B
1B
	7B

=B
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
VB
VB
VB
\B
bB
bB
hB
oB
oB
hB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
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
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
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
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
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
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
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
N�B
N�B
N�B
N�B
O�B
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
S�B
S�B
S�B
S�B
S�B
S�B
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
VB
VB
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
^5B
^5B
^5B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
_;B
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
jB
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
l�B
l�B
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
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200530100042                              AO  ARCAADJP                                                                    20200530100042    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200530100042  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200530100042  QCF$                G�O�G�O�G�O�0               