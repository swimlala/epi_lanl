CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-07-29T09:01:17Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  `    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ol   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۔   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޔ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20220729090117  20220729090117  5906101 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               tA   AO  7907                            2B  A   NAVIS_A                         1015                            170425                          863 @�����t1   @��֬�)�@*�-�d�7KƧ�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         tA   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @s33@�33@�ffA33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B���B�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]��C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%fgD%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJs3DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dw��Dxl�Dx��Dyl�Dy��Dzl�Dz��D{l�D{��D|l�D|��D}l�D}��D~l�D~��Dl�D��D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��3D��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD���D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD¶fD��fD�6fD�vfDöfD��fD�6fD�vfDĶfD��fD�6fD�vfDŶfD��fD�6fD�vfDƶfD��fD�6fD�vfDǶfD��fD�6fD�vfDȶfD��fD�6fD�vfDɶfD��fD�6fD�vfDʶfD��fD�6fD�vfD˶fD��fD�6fD�vfD̶fD��fD�6fD�vfDͶfD��fD�6fD�vfDζfD��fD�6fD�vfD϶fD��fD�6fD�vfDжfD��fD�6fD�vfDѶfD��fD�6fD�vfDҶfD��fD�6fD�vfDӶfD��fD�6fD�vfDԶfD��fD�6fD�vfDնfD��fD�6fD�vfDֶfD��fD�6fD�vfD׶fD��fD�6fD�vfDضfD��fD�6fD�vfDٶfD��fD�6fD�vfDڶfD��fD�6fD�vfD۶fD��fD�6fD�vfDܶfD��fD�6fD�vfDݶfD��fD�6fD�vfD޶fD��fD�6fD�vfD߶fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD���D�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aܕ�Aܗ�Aܙ�Aܡ�Aܡ�Aܡ�Aܣ�Aܡ�Aܝ�Aܗ�AܑhA�z�A�t�A�jA�C�A�1'A��A���A���A���A�ĜA���AۮA۟�A�n�A�G�A�;dA�A�bNA��A�
=A���A��A��A��TA��#A���A�9XA�|�A�A�A���A�v�A���A�E�A��A�JA���A��PA�/A�VA�XA�bA�/A�A�O�A��A�oA�33A�z�A��#A�\)A�t�A��9A��A�E�A�ĜA��^A�A�%A���A�?}A��TA��7A�Q�A�(�A���A�=qA��A�dZA��A�G�A�33A�;dA�ĜA���A�\)A�O�A�I�A�bA���AO�A|��Ax�RAo��Al��AhAdjA`1A]+A[�wAW�ANI�AI��AC`BA@��A=�-A9�A7��A6{A4��A1��A/��A/�A,�A+l�A*bA)XA(Q�A'A%��A$��A#�
A"�HA"A�A!|�A!t�A!7LA �A �uA Ap�AȴA��A�A�AXA;dA��A��AK�A;dA7LA+AXAhsA�AC�Az�A5?AA�
A��A��A��A��A��A�A;dA�A�AXA�hAA�-A7LA��A�TA�7AXAjA;dA
�A
A�A
$�A
{A	��A�A�A��A��A��A�AdZA33AG�A;dAr�A�wAA�A  A�^Al�A
=AA�A  A�
A�FA|�AC�AdZAp�AK�A �H@�ƨ@��R@��@�7L@�/@�/@��/@�z�@�Q�@�1'@��m@���@��@�|�@�M�@��`@�j@��
@�E�@���@�`B@�7L@��@�r�@�(�@��@���@���@� �@�dZ@��@�+@�G�@�1'@�C�@�@�!@�@�Ĝ@�j@�1@�\)@���@��y@�^5@�?}@���@�@�r�@�b@�R@�$�@�@�O�@��/@�I�@߾w@��@ާ�@��@ݙ�@�p�@�X@�7L@�/@���@���@�I�@�l�@٩�@��`@�j@�1'@��@���@�"�@�$�@�Ĝ@Ӆ@�@�v�@љ�@�O�@�z�@��@У�@д9@�bN@��;@��y@�E�@�G�@���@̓u@�r�@�(�@�dZ@ʸR@�5?@�p�@��@ȓu@�b@�C�@ư!@�J@š�@���@�I�@öF@�K�@�=q@�p�@���@�9X@�t�@��R@��@���@�`B@�X@�V@��`@�Ĝ@��@�bN@�  @���@�o@���@���@�{@���@�`B@��/@�z�@� �@��;@���@�S�@�+@�@���@��y@��@���@�~�@�{@���@��@���@���@��
@�C�@�v�@��T@���@�bN@�Q�@�b@�t�@�"�@�ff@���@�7L@��9@��@�z�@�Z@���@��H@��\@�M�@�@���@���@�Z@��@��m@�|�@�;d@��@�V@��@��@��@�`B@���@�z�@�z�@�I�@�(�@��w@�K�@��y@��@�-@���@�&�@��@��P@���@�V@�5?@�$�@�{@�J@���@���@��@�`B@�&�@�r�@�b@��@��
@�t�@��@���@��@�@�7L@��@�r�@�I�@�9X@�1'@�b@��@���@��m@�t�@�
=@�ȴ@��R@���@��+@�v�@�ff@�5?@�J@�@��@���@�@�hs@�V@���@�z�@��
@��w@���@���@��@�K�@�;d@��@�@��H@��!@��!@��R@�~�@��@��7@��@��@�@�p�@�X@��`@�Ĝ@�Ĝ@�r�@�I�@�(�@�ƨ@���@�|�@���@�M�@�=q@�5?@��@�J@��@��#@�@���@���@��h@�x�@�/@���@�z�@� �@�1@���@�l�@��@�ȴ@���@�=q@��-@��@�?}@�&�@��@��@��@��`@�Ĝ@�z�@�1@��m@��w@�l�@�33@�ȴ@�E�@�$�@�{@�@��#@��-@�`B@�&�@���@��@�Ĝ@��u@�9X@�b@��;@��F@�|�@�\)@�C�@��@��H@��+@�=q@��T@���@���@�p�@�7L@���@��`@��j@��D@��@�P@
=@~ȴ@~�+@~$�@}O�@|�D@|1@{33@z�\@y�@yX@x��@x�u@x �@w;d@w
=@vE�@uO�@uV@t��@tI�@t1@s��@s33@r~�@q��@qG�@q�@q%@p�`@p�@pr�@pA�@p1'@p  @o��@oK�@n��@n��@n@mO�@mV@l�/@l��@lZ@l(�@kƨ@kdZ@k@j~�@jM�@i�^@ihs@h��@h�9@g�@g;d@fv�@e��@ep�@eO�@e/@d�@d�@dZ@c�F@c33@b�\@b�@a��@a�^@ahs@a7L@`Ĝ@`�@` �@`  @_�@_�@^ȴ@^��@]�@]�@]p�@]/@\�@\�j@\�@\��@\j@\�@[��@[dZ@[C�@[33@Z��@ZM�@Y��@Y7L@Y%@X��@XbN@XA�@W��@W+@V�R@V�+@V{@U��@U�h@U/@T�/@TZ@T9X@T�@S��@S"�@R�\@RJ@Q��@Q7L@P�9@PbN@P �@O�;@Ol�@O;d@O
=@N�R@NV@N{@M`B@L��@L�@L�@L�j@L�D@LI�@K�m@K"�@J��@J^5@JJ@I�@Ix�@H��@H�9@H�@Hr�@H �@G�w@GK�@F��@F5?@E�h@EO�@EV@D��@D�@Dj@D9X@C�
@CC�@C@B�!@B=q@A�#@A�7@AX@A7L@A7L@A&�@@��@@��@@r�@@r�@?�w@?�@?
=@>��@>�@>��@>v�@>V@>$�@=�T@=@=`B@=?}@=/@<�@;��@;"�@;@:�@:�H@:��@:��@:�!@:�!@:�\@:=q@9�^@9��@9x�@9X@9%@8bN@7��@7��@7l�@7K�@6��@6�@6��@6�+@6ff@6@5��@5p�@5?}@5�@4�@4�/@4z�@3�F@3S�@3o@3o@3@2�@2��@2J@1G�@1%@0�9@0�@0r�@0Q�@01'@0b@/�;@/�@/|�@/;d@.�@.�+@-�T@-�@-O�@-?}@-/@-�@,�@,�/@,�j@,I�@+��@+ƨ@+dZ@*�@*�\@*�@)��@)��@)�7@)G�@(��@(�9@(��@(�u@(�@(Q�@(1'@( �@(  @'�@'��@'�w@'��@'�@'l�@'+@&�@&��@&ff@&{@%�T@%�h@%`B@%`B@%`B@%O�@%V@$��@$�@$Z@$(�@#��@#�F@#��@#�@#dZ@#C�@#@"�\@"=q@!�@!�#@!��@!��@!�7@!�7@!x�@!hs@!G�@!�@ ��@ �9@ �@ Q�@ A�@ 1'@�@|�@K�@K�@K�@;d@��@��@ff@ff@{@�T@@�-@O�@�@V@�@��@�@��@�j@z�@�@�m@��@dZ@S�@S�@C�@33@@��@=q@��@��@��@�^@7L@7L@��@�`@��@Ĝ@��@�@bN@1'@1'@ �@�@�w@+@
=@�y@�@�R@��@ff@E�@{@�@�@�T@��@`B@V@�@��@Z@�@��@�F@�@dZ@S�@C�@33@"�@�@�H@��@n�@-@�@�@J@J@�@�^@x�@�7@�7@x�@G�@&�@�@��@�`@Ĝ@��@�u@�@bN@bN@r�@r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aܕ�Aܗ�Aܙ�Aܡ�Aܡ�Aܡ�Aܣ�Aܡ�Aܝ�Aܗ�AܑhA�z�A�t�A�jA�C�A�1'A��A���A���A���A�ĜA���AۮA۟�A�n�A�G�A�;dA�A�bNA��A�
=A���A��A��A��TA��#A���A�9XA�|�A�A�A���A�v�A���A�E�A��A�JA���A��PA�/A�VA�XA�bA�/A�A�O�A��A�oA�33A�z�A��#A�\)A�t�A��9A��A�E�A�ĜA��^A�A�%A���A�?}A��TA��7A�Q�A�(�A���A�=qA��A�dZA��A�G�A�33A�;dA�ĜA���A�\)A�O�A�I�A�bA���AO�A|��Ax�RAo��Al��AhAdjA`1A]+A[�wAW�ANI�AI��AC`BA@��A=�-A9�A7��A6{A4��A1��A/��A/�A,�A+l�A*bA)XA(Q�A'A%��A$��A#�
A"�HA"A�A!|�A!t�A!7LA �A �uA Ap�AȴA��A�A�AXA;dA��A��AK�A;dA7LA+AXAhsA�AC�Az�A5?AA�
A��A��A��A��A��A�A;dA�A�AXA�hAA�-A7LA��A�TA�7AXAjA;dA
�A
A�A
$�A
{A	��A�A�A��A��A��A�AdZA33AG�A;dAr�A�wAA�A  A�^Al�A
=AA�A  A�
A�FA|�AC�AdZAp�AK�A �H@�ƨ@��R@��@�7L@�/@�/@��/@�z�@�Q�@�1'@��m@���@��@�|�@�M�@��`@�j@��
@�E�@���@�`B@�7L@��@�r�@�(�@��@���@���@� �@�dZ@��@�+@�G�@�1'@�C�@�@�!@�@�Ĝ@�j@�1@�\)@���@��y@�^5@�?}@���@�@�r�@�b@�R@�$�@�@�O�@��/@�I�@߾w@��@ާ�@��@ݙ�@�p�@�X@�7L@�/@���@���@�I�@�l�@٩�@��`@�j@�1'@��@���@�"�@�$�@�Ĝ@Ӆ@�@�v�@љ�@�O�@�z�@��@У�@д9@�bN@��;@��y@�E�@�G�@���@̓u@�r�@�(�@�dZ@ʸR@�5?@�p�@��@ȓu@�b@�C�@ư!@�J@š�@���@�I�@öF@�K�@�=q@�p�@���@�9X@�t�@��R@��@���@�`B@�X@�V@��`@�Ĝ@��@�bN@�  @���@�o@���@���@�{@���@�`B@��/@�z�@� �@��;@���@�S�@�+@�@���@��y@��@���@�~�@�{@���@��@���@���@��
@�C�@�v�@��T@���@�bN@�Q�@�b@�t�@�"�@�ff@���@�7L@��9@��@�z�@�Z@���@��H@��\@�M�@�@���@���@�Z@��@��m@�|�@�;d@��@�V@��@��@��@�`B@���@�z�@�z�@�I�@�(�@��w@�K�@��y@��@�-@���@�&�@��@��P@���@�V@�5?@�$�@�{@�J@���@���@��@�`B@�&�@�r�@�b@��@��
@�t�@��@���@��@�@�7L@��@�r�@�I�@�9X@�1'@�b@��@���@��m@�t�@�
=@�ȴ@��R@���@��+@�v�@�ff@�5?@�J@�@��@���@�@�hs@�V@���@�z�@��
@��w@���@���@��@�K�@�;d@��@�@��H@��!@��!@��R@�~�@��@��7@��@��@�@�p�@�X@��`@�Ĝ@�Ĝ@�r�@�I�@�(�@�ƨ@���@�|�@���@�M�@�=q@�5?@��@�J@��@��#@�@���@���@��h@�x�@�/@���@�z�@� �@�1@���@�l�@��@�ȴ@���@�=q@��-@��@�?}@�&�@��@��@��@��`@�Ĝ@�z�@�1@��m@��w@�l�@�33@�ȴ@�E�@�$�@�{@�@��#@��-@�`B@�&�@���@��@�Ĝ@��u@�9X@�b@��;@��F@�|�@�\)@�C�@��@��H@��+@�=q@��T@���@���@�p�@�7L@���@��`@��j@��D@��@�P@
=@~ȴ@~�+@~$�@}O�@|�D@|1@{33@z�\@y�@yX@x��@x�u@x �@w;d@w
=@vE�@uO�@uV@t��@tI�@t1@s��@s33@r~�@q��@qG�@q�@q%@p�`@p�@pr�@pA�@p1'@p  @o��@oK�@n��@n��@n@mO�@mV@l�/@l��@lZ@l(�@kƨ@kdZ@k@j~�@jM�@i�^@ihs@h��@h�9@g�@g;d@fv�@e��@ep�@eO�@e/@d�@d�@dZ@c�F@c33@b�\@b�@a��@a�^@ahs@a7L@`Ĝ@`�@` �@`  @_�@_�@^ȴ@^��@]�@]�@]p�@]/@\�@\�j@\�@\��@\j@\�@[��@[dZ@[C�@[33@Z��@ZM�@Y��@Y7L@Y%@X��@XbN@XA�@W��@W+@V�R@V�+@V{@U��@U�h@U/@T�/@TZ@T9X@T�@S��@S"�@R�\@RJ@Q��@Q7L@P�9@PbN@P �@O�;@Ol�@O;d@O
=@N�R@NV@N{@M`B@L��@L�@L�@L�j@L�D@LI�@K�m@K"�@J��@J^5@JJ@I�@Ix�@H��@H�9@H�@Hr�@H �@G�w@GK�@F��@F5?@E�h@EO�@EV@D��@D�@Dj@D9X@C�
@CC�@C@B�!@B=q@A�#@A�7@AX@A7L@A7L@A&�@@��@@��@@r�@@r�@?�w@?�@?
=@>��@>�@>��@>v�@>V@>$�@=�T@=@=`B@=?}@=/@<�@;��@;"�@;@:�@:�H@:��@:��@:�!@:�!@:�\@:=q@9�^@9��@9x�@9X@9%@8bN@7��@7��@7l�@7K�@6��@6�@6��@6�+@6ff@6@5��@5p�@5?}@5�@4�@4�/@4z�@3�F@3S�@3o@3o@3@2�@2��@2J@1G�@1%@0�9@0�@0r�@0Q�@01'@0b@/�;@/�@/|�@/;d@.�@.�+@-�T@-�@-O�@-?}@-/@-�@,�@,�/@,�j@,I�@+��@+ƨ@+dZ@*�@*�\@*�@)��@)��@)�7@)G�@(��@(�9@(��@(�u@(�@(Q�@(1'@( �@(  @'�@'��@'�w@'��@'�@'l�@'+@&�@&��@&ff@&{@%�T@%�h@%`B@%`B@%`B@%O�@%V@$��@$�@$Z@$(�@#��@#�F@#��@#�@#dZ@#C�@#@"�\@"=q@!�@!�#@!��@!��@!�7@!�7@!x�@!hs@!G�@!�@ ��@ �9@ �@ Q�@ A�@ 1'@�@|�@K�@K�@K�@;d@��@��@ff@ff@{@�T@@�-@O�@�@V@�@��@�@��@�j@z�@�@�m@��@dZ@S�@S�@C�@33@@��@=q@��@��@��@�^@7L@7L@��@�`@��@Ĝ@��@�@bN@1'@1'@ �@�@�w@+@
=@�y@�@�R@��@ff@E�@{@�@�@�T@��@`B@V@�@��@Z@�@��@�F@�@dZ@S�@C�@33@"�@�@�H@��@n�@-@�@�@J@J@�@�^@x�@�7@�7@x�@G�@&�@�@��@�`@Ĝ@��@�u@�@bN@bN@r�@r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B"�B"�B"�B!�B"�B"�B!�B!�B!�B"�B#�B&�B&�B&�B)�B+B0!B33B5?B6FB8RB9XB<jB>wBF�BL�BL�BK�BE�BA�BA�BA�B@�B@�B?}B?}B>wB9XB-BD�B��B�B��B�B  B�qB��B��B�B	7B�B�BhB+BB��B��B�B�yB�HB�B��B��BĜB�^B�-B��B��B�bBv�BjBYB7LB49B1'B+B �BbB
��B
�/B
�^B
��B
�7B
�B
x�B
n�B
dZB
Q�B
=qB
9XB
+B
�B
DB	�B	��B	�B	��B	}�B	hsB	`BB	L�B	2-B	�B	hB	+B	  B��B�B�B�B�yB�mB�B��B��B	B	
=B	\B	uB	�B	�B	#�B	(�B	,B	.B	33B	7LB	5?B	2-B	.B	1'B	49B	49B	8RB	9XB	;dB	?}B	A�B	>wB	@�B	D�B	F�B	G�B	N�B	]/B	ZB	ZB	YB	YB	`BB	hsB	hsB	iyB	l�B	ffB	l�B	�B	~�B	�+B	�JB	��B	��B	�B	�RB	�^B	�XB	�XB	�LB	�FB	�FB	�9B	�9B	�?B	�?B	�?B	�dB	�wB	ÖB	ĜB	ÖB	ÖB	B	B	ÖB	ŢB	��B	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�TB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B	��B
  B
  B
  B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B

=B
DB
DB
	7B
	7B
1B
1B
	7B
	7B
	7B

=B

=B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
	7B

=B
	7B
DB
DB
JB
JB
JB
DB
DB
JB
JB
JB
JB
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
bB
hB
oB
oB
oB
oB
oB
hB
hB
hB
hB
hB
hB
bB
hB
{B
{B
{B
uB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
%�B
$�B
$�B
"�B
$�B
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
%�B
%�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
,B
,B
-B
-B
-B
-B
-B
-B
.B
-B
-B
-B
.B
.B
.B
.B
/B
/B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
0!B
1'B
1'B
1'B
2-B
33B
49B
49B
33B
2-B
2-B
6FB
7LB
7LB
7LB
8RB
8RB
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
<jB
<jB
<jB
=qB
=qB
>wB
>wB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
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
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
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
K�B
K�B
L�B
L�B
L�B
L�B
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
O�B
O�B
N�B
N�B
N�B
N�B
N�B
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
O�B
O�B
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
XB
XB
W
B
W
B
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
]/B
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
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
cTB
dZB
cTB
dZB
cTB
cTB
cTB
dZB
dZB
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
hsB
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
k�B
k�B
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
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
v�B
v�B
w�B
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
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�%B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�JB
�PB
�PB
�PB
�VB
�VB
�VB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B"�B"�B"�B!�B"�B"�B!�B!�B!�B"�B#�B&�B&�B&�B)�B+B0!B33B5?B6FB8RB9XB<jB>wBF�BL�BL�BK�BE�BA�BA�BA�B@�B@�B?}B?}B>wB9XB-BD�B��B�B��B�B  B�qB��B��B�B	7B�B�BhB+BB��B��B�B�yB�HB�B��B��BĜB�^B�-B��B��B�bBv�BjBYB7LB49B1'B+B �BbB
��B
�/B
�^B
��B
�7B
�B
x�B
n�B
dZB
Q�B
=qB
9XB
+B
�B
DB	�B	��B	�B	��B	}�B	hsB	`BB	L�B	2-B	�B	hB	+B	  B��B�B�B�B�yB�mB�B��B��B	B	
=B	\B	uB	�B	�B	#�B	(�B	,B	.B	33B	7LB	5?B	2-B	.B	1'B	49B	49B	8RB	9XB	;dB	?}B	A�B	>wB	@�B	D�B	F�B	G�B	N�B	]/B	ZB	ZB	YB	YB	`BB	hsB	hsB	iyB	l�B	ffB	l�B	�B	~�B	�+B	�JB	��B	��B	�B	�RB	�^B	�XB	�XB	�LB	�FB	�FB	�9B	�9B	�?B	�?B	�?B	�dB	�wB	ÖB	ĜB	ÖB	ÖB	B	B	ÖB	ŢB	��B	ɺB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�TB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B	��B
  B
  B
  B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B

=B
DB
DB
	7B
	7B
1B
1B
	7B
	7B
	7B

=B

=B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
	7B

=B
	7B
DB
DB
JB
JB
JB
DB
DB
JB
JB
JB
JB
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
bB
hB
oB
oB
oB
oB
oB
hB
hB
hB
hB
hB
hB
bB
hB
{B
{B
{B
uB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
&�B
%�B
$�B
$�B
"�B
$�B
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
%�B
%�B
&�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
,B
,B
-B
-B
-B
-B
-B
-B
.B
-B
-B
-B
.B
.B
.B
.B
/B
/B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
0!B
1'B
1'B
1'B
2-B
33B
49B
49B
33B
2-B
2-B
6FB
7LB
7LB
7LB
8RB
8RB
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
<jB
<jB
<jB
=qB
=qB
>wB
>wB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
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
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
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
K�B
K�B
L�B
L�B
L�B
L�B
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
O�B
O�B
N�B
N�B
N�B
N�B
N�B
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
O�B
O�B
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
XB
XB
W
B
W
B
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
]/B
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
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
cTB
dZB
cTB
dZB
cTB
cTB
cTB
dZB
dZB
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
hsB
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
k�B
k�B
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
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
v�B
v�B
w�B
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
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�%B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�JB
�PB
�PB
�PB
�VB
�VB
�VB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.30 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220729090117                              AO  ARCAADJP                                                                    20220729090117    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220729090117  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20220729090117  QCF$                G�O�G�O�G�O�0               