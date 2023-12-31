CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:25:40Z creation;2022-06-04T19:25:41Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604192540  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               VA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�}���/1   @�}�g(��@*��+�dXbM�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�33A   AffA@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B���B̙�Bϙ�B�  B�  B���B�  B�  B�ffB왚B�  B�  B�  B�  C   C  C�fC�fC  C
33C  C�C�fC�fC  C  C  C  C  C  C   C"  C$  C&  C(33C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CTL�CU��CW�fCY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @&fg@l��@���@�ffA��A;33A[33A{33A���A���A�fgA���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�  B�  B�ffB�ffB�ffB�ffB�ffB�33B�  B�  B�ffB�ffB�33B�ffB�ffB���B�  B�ffB�ffB�ffB�ffB�ffC�3C��C��C�3C	�fC�3C��C��C��C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�fC)�3C+�3C-��C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ��CT  CU� CW��CY��C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��gC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��gC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��gC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGs3DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk�gDll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dw��Dxl�Dx��Dyl�Dy��Dzl�Dz��D{l�D{��D|l�D|��D}l�D}��D~l�D~��Dl�D��D�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�9�D�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD¶fD��fD�6fD�vfDöfD��fD�6fD�vfDĶfD��fD�6fD�vfDŶfD��fD�6fD�vfDƶfD��fD�6fD�vfDǶfD��fD�6fD�vfDȶfD��fD�6fD�vfDɶfD��fD�6fD�vfDʶfD��fD�6fD�vfD˶fD��fD�6fD�vfD̶fD��fD�6fD�vfDͶfD��fD�6fD�vfDζfD��fD�6fD�vfD϶fD��fD�6fD�vfDжfD��fD�6fD�vfDѶfD��fD�6fD�vfDҶfD��fD�6fD�vfDӶfD��fD�6fD�vfDԶfD��fD�6fD�vfDնfD��fD�6fD�vfDֶfD��fD�6fD�vfD׶fD��fD�6fD�vfDضfD��fD�6fD�vfDٶfD��fD�6fD�vfDڶfD��fD�6fD�vfD۶fD��fD�6fD�vfDܶfD��fD�6fD�vfDݶfD��fD�6fD�vfD޶fD��fD�6fD�vfD߶fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD�fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�vfD��fD��fD�6fD�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��A���A��`A��A��oA�� A��A���A���A��>A��A���A��|A��BA��QA��AҿHAҷ�AҳhAҧ�AҔAҎ"AҌ~AҊrA҈�A҇�A҆%A҃�A҃{AҁoAҀ�Aҁ;AҀ�AҀ�A��A�.A�~�A�~�A�}VA�{�A�d�A�<�A���A�jKAȞ�A�_A�YA�f�Aŏ(A��mA��NA�ΥA�A���A�=A��A��A��7A�WsA��A��EA��A�PA�J�A���A��TA��aA��<A��LA�{A��}A�,�A�c�A��9A��JA��VA�6�A��QA��A��?A��.A��OA�H�A��FA��A���A�YA�R A~�9AxԕAr��AmMAk�Ag��Ade,A`�PA\�]AZAT��AP�XAM�AK��AJRTAH�AG��AEiDA=($A;�]A9�A7�A5�A4R�A3��A34�A2��A1��A0A0N<A/��A/PHA.�A.��A.�oA.J#A.T�A.DgA.OA-��A-G�A-!A+��A*8�A(�
A'��A'w�A&b�A$�]A$ݘA$x�A$TaA"��A"�vA"�A#�KA$tTA$��A$��A$��A$��A$�PA$��A$v`A$,�A#��A#
=A"�^A"��A"_A!$tA �AIRA�A[WA��A�zA��AVA��An/AA
�A��A%�A�
AXAqAیAXyA��A^�A��A�AG�A��A\�A�AoA�A�A�}AU�A�MA�XA��Am�A�Ae�A��A�A�Ab�AHA�A�FA�bA�EA�A�oAXA8A��A9XA
�oA	��A	�@A	�DA	`�A	1�A�.A��A��A�AjA�A'�A�XAB�A��A�A+�A֡A��Am]A^�A�A�$Ap;A6zA��A ��A �@�Mj@��9@�6@��@��N@���@��!@���@�]d@�8@��@��@�;@��K@���@���@���@�x�@�S&@�+@�"h@��@��@�~�@��@�t@��@�B�@��,@�M@��@��@�J#@�~@��@�?�@��A@�@���@�Z�@��@��@���@�(�@���@�@�\)@���@�Z�@��@���@߲-@�=�@�m�@ݏ�@��)@��@�E�@�ϫ@�B�@�ں@�I�@ו�@�C@���@�;�@�t�@���@�4n@� �@��A@��@�4@ҩ�@ҕ�@�I�@�$@�e@ѓ�@�;@Ќ@��@ϡ�@�Dg@��|@�Z@͖S@�0U@ˣn@�+@�Ĝ@�_@�M@���@�hs@��@�N�@��N@�n/@�4@��s@�D�@Ż0@�x@��@ĉ�@��@�f�@���@�h
@�,=@�u@�w2@���@�\�@���@�4@�(@��c@�6@��[@��@�Q@��@�-w@�ں@���@�Ov@�s�@�A @�!-@��@��@���@��X@��@��@���@�8@��,@��@�b�@���@�Ĝ@�tT@�<�@�u@��@��@��q@�|@�ی@��z@�c�@���@�͟@��r@�#:@��@� \@��@�`�@��)@�=@���@�j@��@�ϫ@���@���@�F�@��]@�z�@�_@��k@�4@��@�m�@��A@�+@��e@�%�@���@�$t@��@���@�+k@��)@���@�Y�@��@��p@�m�@�#:@���@�t�@�K�@�/@�;@��K@��,@��z@� �@���@��@��*@�\)@��@�	l@�;@���@��@�J�@���@��@�{�@�]d@�5?@�O@���@�|�@��@���@��@���@���@���@�rG@�@O@��@�o@��@���@��@��@�v�@�Ov@�<�@�:�@��@���@�B�@�$t@��@��)@���@�_�@��j@�|@�\)@�@��@�h�@���@�o @�4�@��@���@��4@���@�J@��P@�K�@�&@��E@��.@�K^@��@��-@�!�@�҉@��+@�h
@�E�@�ϫ@�e,@�L�@��@���@�Ov@���@��k@�~�@�Y�@�33@�'�@��@�@�ں@�:*@� �@�e@�G@���@�O�@�1�@��@��5@���@���@���@�w�@�+k@���@���@�qv@�H�@��@��R@���@�Q@�M@��@�qv@�@��f@��9@�r�@�1�@�J@���@���@�7L@��@���@�\�@�-�@��@��z@���@���@�v`@�`B@�F�@��@��F@�W�@�~@�w@~��@~{�@}�D@|�@|[�@{�@{��@{H�@z&�@y��@yQ�@x�@xH@w�P@v��@v6�@u�@u��@u|@uL�@u�@u�@t�5@t�@toi@s��@sJ#@r��@rC�@r{@qϫ@q2a@p��@p��@o�@ob�@oS@n��@n�@n�@nTa@nu@m��@mw2@mX@m:�@l�@l,=@k�
@kdZ@j�@j&�@i�-@iO�@h��@h��@h|�@h'R@g~�@gY@f��@e��@e��@e&�@dɆ@d��@d(�@ca@c'�@c@b�y@b��@bu%@b{@a�z@aS&@a�@`�@`r�@`Z@`/�@_�0@_��@_6z@^�@^J@]�@]�@]Dg@]@\�z@\Z@\~@[�[@[4�@Z��@Zd�@Y��@Y��@Y0�@Y%@X�|@X'R@W��@WiD@W'�@V��@V�s@VM�@V�@V�@U@T�@T`�@T,=@S�@S��@SF�@S i@Rں@R��@R��@R_�@R@Q�d@Q��@Qj@Q!�@P�@PQ�@O�+@O��@O{J@O.I@N�@N҉@N��@N&�@M��@M�-@M^�@L�@L?�@L	�@K�w@KX�@K!-@J�y@J�L@Js�@JOv@J1�@I�@I�o@I�o@I�o@I�@I�@Ix�@I!�@HC-@G��@GH�@F��@F�}@Fxl@FQ@F!�@E�#@E�@EX@E�@D�$@D�@Dj@D%�@C��@Bl�@B�@A��@A�@Am]@A@@�@@�@@I�@?��@?��@?�g@?x@?'�@>�c@>xl@>3�@>�@=�>@=�d@=�X@=2a@=q@<��@<��@<��@<z�@;�+@;��@;F�@:�B@:c @9��@9|@9[W@9S&@9?}@9!�@9-w@8��@8�p@8�_@8��@8j@89X@7�6@7�4@7]�@7O@7@6xl@5�>@5&�@4c�@4"h@3ݘ@3a@2��@2YK@2	@1��@1�o@1��@1u�@1�@0��@0<�@0�@/˒@/��@/j�@.�@.kQ@._@-�d@-rG@-�@,�@,C-@,4n@,�@+��@+�@*��@*�L@*Ov@*	@*@)��@)�t@)T�@(�E@(�e@(%�@'�@'��@'P�@&�'@&}V@&_�@&L0@&@�@&6�@&�@%ϫ@%o @%%@$��@$U2@$�@#��@#�k@#y�@#Mj@"�c@"�@"�}@"��@"V@"�@!�)@!��@!��@!��@!|@!Dg@!0�@!%@ ��@ K^@ *�@ b@�&@��@��@t�@@O@1�@+@�@ں@��@ff@($@�9@��@��@A @;@��@�p@�@|�@e�@PH@$@@�;@��@~�@X�@C�@>�@9�@@ں@�@v�@a|@R�@?@@�@|@Vm@4@�@��@�p@�$@]d@(�@b@�@��@��@iD@,�@�2@�m@�b@�\@��@kQ@�.@�M@Dg@0�@�@[�@	�@�[@RT@�@��@i�@E�@+k@_@�t@�@��@K^@��@�
@��@��@��@�P@�4@W?@�@�@��@{�@GE@4@u@��@�@��@�"@X@%@�5@��@��@?�@��@ƨ@�@�F@��@J#@�@
��@
Z�@
J�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��A���A��`A��A��oA�� A��A���A���A��>A��A���A��|A��BA��QA��AҿHAҷ�AҳhAҧ�AҔAҎ"AҌ~AҊrA҈�A҇�A҆%A҃�A҃{AҁoAҀ�Aҁ;AҀ�AҀ�A��A�.A�~�A�~�A�}VA�{�A�d�A�<�A���A�jKAȞ�A�_A�YA�f�Aŏ(A��mA��NA�ΥA�A���A�=A��A��A��7A�WsA��A��EA��A�PA�J�A���A��TA��aA��<A��LA�{A��}A�,�A�c�A��9A��JA��VA�6�A��QA��A��?A��.A��OA�H�A��FA��A���A�YA�R A~�9AxԕAr��AmMAk�Ag��Ade,A`�PA\�]AZAT��AP�XAM�AK��AJRTAH�AG��AEiDA=($A;�]A9�A7�A5�A4R�A3��A34�A2��A1��A0A0N<A/��A/PHA.�A.��A.�oA.J#A.T�A.DgA.OA-��A-G�A-!A+��A*8�A(�
A'��A'w�A&b�A$�]A$ݘA$x�A$TaA"��A"�vA"�A#�KA$tTA$��A$��A$��A$��A$�PA$��A$v`A$,�A#��A#
=A"�^A"��A"_A!$tA �AIRA�A[WA��A�zA��AVA��An/AA
�A��A%�A�
AXAqAیAXyA��A^�A��A�AG�A��A\�A�AoA�A�A�}AU�A�MA�XA��Am�A�Ae�A��A�A�Ab�AHA�A�FA�bA�EA�A�oAXA8A��A9XA
�oA	��A	�@A	�DA	`�A	1�A�.A��A��A�AjA�A'�A�XAB�A��A�A+�A֡A��Am]A^�A�A�$Ap;A6zA��A ��A �@�Mj@��9@�6@��@��N@���@��!@���@�]d@�8@��@��@�;@��K@���@���@���@�x�@�S&@�+@�"h@��@��@�~�@��@�t@��@�B�@��,@�M@��@��@�J#@�~@��@�?�@��A@�@���@�Z�@��@��@���@�(�@���@�@�\)@���@�Z�@��@���@߲-@�=�@�m�@ݏ�@��)@��@�E�@�ϫ@�B�@�ں@�I�@ו�@�C@���@�;�@�t�@���@�4n@� �@��A@��@�4@ҩ�@ҕ�@�I�@�$@�e@ѓ�@�;@Ќ@��@ϡ�@�Dg@��|@�Z@͖S@�0U@ˣn@�+@�Ĝ@�_@�M@���@�hs@��@�N�@��N@�n/@�4@��s@�D�@Ż0@�x@��@ĉ�@��@�f�@���@�h
@�,=@�u@�w2@���@�\�@���@�4@�(@��c@�6@��[@��@�Q@��@�-w@�ں@���@�Ov@�s�@�A @�!-@��@��@���@��X@��@��@���@�8@��,@��@�b�@���@�Ĝ@�tT@�<�@�u@��@��@��q@�|@�ی@��z@�c�@���@�͟@��r@�#:@��@� \@��@�`�@��)@�=@���@�j@��@�ϫ@���@���@�F�@��]@�z�@�_@��k@�4@��@�m�@��A@�+@��e@�%�@���@�$t@��@���@�+k@��)@���@�Y�@��@��p@�m�@�#:@���@�t�@�K�@�/@�;@��K@��,@��z@� �@���@��@��*@�\)@��@�	l@�;@���@��@�J�@���@��@�{�@�]d@�5?@�O@���@�|�@��@���@��@���@���@���@�rG@�@O@��@�o@��@���@��@��@�v�@�Ov@�<�@�:�@��@���@�B�@�$t@��@��)@���@�_�@��j@�|@�\)@�@��@�h�@���@�o @�4�@��@���@��4@���@�J@��P@�K�@�&@��E@��.@�K^@��@��-@�!�@�҉@��+@�h
@�E�@�ϫ@�e,@�L�@��@���@�Ov@���@��k@�~�@�Y�@�33@�'�@��@�@�ں@�:*@� �@�e@�G@���@�O�@�1�@��@��5@���@���@���@�w�@�+k@���@���@�qv@�H�@��@��R@���@�Q@�M@��@�qv@�@��f@��9@�r�@�1�@�J@���@���@�7L@��@���@�\�@�-�@��@��z@���@���@�v`@�`B@�F�@��@��F@�W�@�~@�w@~��@~{�@}�D@|�@|[�@{�@{��@{H�@z&�@y��@yQ�@x�@xH@w�P@v��@v6�@u�@u��@u|@uL�@u�@u�@t�5@t�@toi@s��@sJ#@r��@rC�@r{@qϫ@q2a@p��@p��@o�@ob�@oS@n��@n�@n�@nTa@nu@m��@mw2@mX@m:�@l�@l,=@k�
@kdZ@j�@j&�@i�-@iO�@h��@h��@h|�@h'R@g~�@gY@f��@e��@e��@e&�@dɆ@d��@d(�@ca@c'�@c@b�y@b��@bu%@b{@a�z@aS&@a�@`�@`r�@`Z@`/�@_�0@_��@_6z@^�@^J@]�@]�@]Dg@]@\�z@\Z@\~@[�[@[4�@Z��@Zd�@Y��@Y��@Y0�@Y%@X�|@X'R@W��@WiD@W'�@V��@V�s@VM�@V�@V�@U@T�@T`�@T,=@S�@S��@SF�@S i@Rں@R��@R��@R_�@R@Q�d@Q��@Qj@Q!�@P�@PQ�@O�+@O��@O{J@O.I@N�@N҉@N��@N&�@M��@M�-@M^�@L�@L?�@L	�@K�w@KX�@K!-@J�y@J�L@Js�@JOv@J1�@I�@I�o@I�o@I�o@I�@I�@Ix�@I!�@HC-@G��@GH�@F��@F�}@Fxl@FQ@F!�@E�#@E�@EX@E�@D�$@D�@Dj@D%�@C��@Bl�@B�@A��@A�@Am]@A@@�@@�@@I�@?��@?��@?�g@?x@?'�@>�c@>xl@>3�@>�@=�>@=�d@=�X@=2a@=q@<��@<��@<��@<z�@;�+@;��@;F�@:�B@:c @9��@9|@9[W@9S&@9?}@9!�@9-w@8��@8�p@8�_@8��@8j@89X@7�6@7�4@7]�@7O@7@6xl@5�>@5&�@4c�@4"h@3ݘ@3a@2��@2YK@2	@1��@1�o@1��@1u�@1�@0��@0<�@0�@/˒@/��@/j�@.�@.kQ@._@-�d@-rG@-�@,�@,C-@,4n@,�@+��@+�@*��@*�L@*Ov@*	@*@)��@)�t@)T�@(�E@(�e@(%�@'�@'��@'P�@&�'@&}V@&_�@&L0@&@�@&6�@&�@%ϫ@%o @%%@$��@$U2@$�@#��@#�k@#y�@#Mj@"�c@"�@"�}@"��@"V@"�@!�)@!��@!��@!��@!|@!Dg@!0�@!%@ ��@ K^@ *�@ b@�&@��@��@t�@@O@1�@+@�@ں@��@ff@($@�9@��@��@A @;@��@�p@�@|�@e�@PH@$@@�;@��@~�@X�@C�@>�@9�@@ں@�@v�@a|@R�@?@@�@|@Vm@4@�@��@�p@�$@]d@(�@b@�@��@��@iD@,�@�2@�m@�b@�\@��@kQ@�.@�M@Dg@0�@�@[�@	�@�[@RT@�@��@i�@E�@+k@_@�t@�@��@K^@��@�
@��@��@��@�P@�4@W?@�@�@��@{�@GE@4@u@��@�@��@�"@X@%@�5@��@��@?�@��@ƨ@�@�F@��@J#@�@
��@
Z�@
J�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
S�B
T�B
TaB
T{B
T�B
TaB
S�B
TFB
TFB
T,B
S�B
S�B
T,B
S�B
S�B
S@B
SuB
R�B
R�B
R�B
R B
RB
RB
RB
R B
Q�B
Q�B
RB
R B
RB
RB
RB
RB
R B
RB
R B
RB
RB
RB
RB
RB
QNB
P.B
I�B
E�B
8�B
8B
O(B
O(B
bNB
��B
��B
�\B
��B�B5�BE�BMPBP}BR:BUMBU�Bi_B�PB��B�;B�[B�BیBیB�'B�B�B��B�B�gB��B��Bo5BOB6zB^B
یB
��B
��B
{0B
c�B
?�B
)_B
�B	�FB	��B	�;B	�B	{�B	f�B	QB	>B	+�B	MB	B��B�MB�B�B�B�cB��B�rB	�B	�B	"�B	2�B	<B	>�B	E�B	YeB	t�B	��B	�&B	�;B	� B	бB	�B	�BB	��B	�5B	��B
�B

=B
�B
bB
B
MB
{B
B
�B
�B
+B
B
B
B
�B
 �B
6FB
DMB
GB
H�B
K�B
T�B
VB
VB
VmB
V�B
W�B
W�B
W$B
V�B
VSB
T,B
Q4B
NB
M�B
Q�B
S�B
TB
S�B
TaB
T�B
T�B
VmB
V�B
W�B
Y1B
[WB
ZkB
\CB
\�B
[WB
XEB
V�B
VB
V�B
X�B
V�B
U�B
T�B
T�B
S�B
S�B
R B
PB
N�B
L�B
K�B
I�B
F�B
B'B
=qB
:*B
@ B
?}B
?B
>�B
<�B
9�B
6FB
0�B
./B
,�B
+�B
)�B
%�B
#:B
"NB
"B
#�B
#nB
#�B
#�B
$@B
!�B
 �B
 \B
 'B
VB
dB
�B
CB
/B
/B
�B
VB
VB
�B
�B
�B
/B
CB
QB
�B
B
�B
�B
�B
�B
�B
kB
eB
�B
YB
�B
gB
MB
�B
�B
�B
�B
B
�B
�B
B
�B
B
�B
�B
	�B
�B
�B
�B
�B
�B
SB
B
�B
[B
AB
B
B
9B
B
�B
+B
B
B
	B
?B
�B
�B
+B
zB
�B
�B
�B
MB
GB
[B
UB
 B
  B	�}B	�.B	��B	��B	�.B	��B	�wB	��B	�wB	��B	��B	��B	��B	�]B	�.B	��B	��B	��B	��B	��B	�wB	�BB	��B	��B	�wB	�wB	�BB	�(B	��B	��B	�(B	�(B	�B	�B	�BB	�BB	��B	�B	�]B	�BB	�wB	�(B	�BB	�wB	�BB	�BB	�]B	�BB	�wB	�B	��B	�BB	�(B	�B	�]B	��B	��B	�cB	�HB	�HB	��B	�}B	�HB	��B	�cB	��B	�cB	�cB	�HB	�}B
 B
UB
�B
�B
�B
�B
�B
�B
�B
aB
�B
�B
9B
9B
mB
�B
�B
�B
YB
?B
YB
?B
B
_B
�B
�B
	B
�B
	7B
	RB
	�B

#B

	B

=B

�B
xB
B
B
�B
�B
�B
�B
B
�B
�B
"B
pB
�B
B
\B
�B
HB
}B
�B
B
NB
NB
�B
�B
B
TB
TB
oB
TB
TB
�B
�B
@B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
B
MB
2B
MB
B
�B
�B
B
�B

B

B
$B

B
?B
�B
�B
+B
_B
+B
+B
B
yB
yB
�B
�B
�B
�B
�B
�B
KB
B
B
KB
B
B
�B
�B
#B
WB
�B
qB
�B
�B
�B
/B
/B
�B
jB
�B
B
!B
VB
�B
pB
 BB
 �B
!B
!B
!|B
!�B
!�B
"NB
"hB
#TB
#nB
#�B
#�B
#�B
$ZB
$�B
$�B
$�B
%,B
%zB
&B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
(>B
($B
($B
($B
(sB
)_B
)_B
)�B
)�B
)�B
)�B
)�B
*KB
*�B
+�B
+�B
+�B
+�B
,=B
,�B
,�B
-)B
-]B
-]B
.}B
.�B
.�B
/ B
/iB
/�B
/�B
/�B
0UB
0�B
0�B
1[B
1�B
1�B
2aB
2GB
2�B
2�B
2�B
2|B
2�B
3B
33B
3MB
3hB
3�B
4B
4B
4B
4TB
4nB
4�B
4�B
4�B
5ZB
5�B
5�B
6+B
6�B
6�B
7�B
7�B
8B
88B
88B
8lB
8�B
8�B
8�B
8�B
8�B
9rB
9�B
:�B
:�B
:�B
:�B
;JB
;B
;�B
<6B
<�B
<�B
<�B
<�B
<�B
=qB
=�B
=�B
>B
=�B
=�B
>B
>�B
>�B
>�B
?}B
?�B
@ B
@4B
@�B
@�B
@�B
@�B
A�B
A�B
BB
B�B
B�B
CB
C-B
CGB
C�B
D�B
D�B
EB
EB
EB
EmB
E�B
E�B
E�B
E�B
F�B
FtB
F�B
F�B
GB
F�B
GzB
G�B
HKB
H1B
HfB
H�B
H�B
HfB
H�B
H�B
H�B
IB
IRB
I�B
I�B
J#B
JrB
JrB
J=B
J�B
KB
K)B
K^B
KxB
K�B
K�B
LB
K�B
L0B
L�B
M6B
MPB
M�B
M�B
NB
N"B
NVB
NVB
NVB
N�B
N�B
OB
OB
O\B
O�B
PB
PbB
P�B
P�B
P�B
QNB
QNB
QNB
Q�B
Q�B
Q�B
R B
RTB
SB
S&B
S@B
S�B
S�B
TB
T,B
T{B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
UMB
UgB
VB
V9B
V�B
W$B
W?B
WsB
WsB
WsB
W�B
W�B
W�B
XEB
X�B
X�B
YB
YKB
ZB
Z�B
[#B
[#B
[qB
[�B
\B
\�B
\�B
\�B
]B
]B
]B
]dB
]�B
]�B
^OB
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
`BB
`�B
`BB
`\B
`'B
`\B
`vB
`\B
`\B
`�B
`�B
`�B
a�B
a�B
bNB
bhB
b�B
c:B
c�B
c�B
c�B
c�B
d&B
dZB
d&B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
e,B
eFB
e`B
e�B
e�B
e�B
ffB
gB
gmB
g�B
g�B
gRB
h
B
hXB
h�B
h�B
iB
i_B
i�B
i�B
i�B
i�B
jB
jB
j�B
j�B
kB
kQB
kQB
kQB
k�B
k�B
lqB
lqB
m)B
l�B
mwB
m�B
n/B
ncB
n}B
n}B
n}B
n}B
n�B
n�B
oB
o�B
o�B
p!B
poB
p�B
p�B
qB
qAB
q[B
q[B
qvB
rB
r-B
r-B
r-B
rGB
r|B
r�B
sB
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
tB
tB
t9B
tTB
tnB
tTB
tTB
tnB
t�B
t�B
t�B
uZB
utB
utB
vB
v`B
vFB
v`B
vzB
v�B
v�B
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
xRB
x�B
x�B
x�B
y	B
yXB
y�B
y�B
y�B
z*B
z^B
z^B
z^B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{0B
z�B
{B
{JB
{JB
{JB
{dB
|B
|B
|B
|B
|B
}B
}B
}VB
}�B
}�B
~B
~BB
~(B
~BB
~BB
~�B
B
cB
�B
�B
� B
� B
�B
�B
�OB
�4B
��B
��B
�B
� B
�;B
�UB
�UB
�;B
�UB
��B
��B
��B
��B
�AB
�AB
�'B
��B
��B
�-B
�aB
�{B
�{B
��B
�B
�MB
��B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
S�B
T�B
TaB
T{B
T�B
TaB
S�B
TFB
TFB
T,B
S�B
S�B
T,B
S�B
S�B
S@B
SuB
R�B
R�B
R�B
R B
RB
RB
RB
R B
Q�B
Q�B
RB
R B
RB
RB
RB
RB
R B
RB
R B
RB
RB
RB
RB
RB
QNB
P.B
I�B
E�B
8�B
8B
O(B
O(B
bNB
��B
��B
�\B
��B�B5�BE�BMPBP}BR:BUMBU�Bi_B�PB��B�;B�[B�BیBیB�'B�B�B��B�B�gB��B��Bo5BOB6zB^B
یB
��B
��B
{0B
c�B
?�B
)_B
�B	�FB	��B	�;B	�B	{�B	f�B	QB	>B	+�B	MB	B��B�MB�B�B�B�cB��B�rB	�B	�B	"�B	2�B	<B	>�B	E�B	YeB	t�B	��B	�&B	�;B	� B	бB	�B	�BB	��B	�5B	��B
�B

=B
�B
bB
B
MB
{B
B
�B
�B
+B
B
B
B
�B
 �B
6FB
DMB
GB
H�B
K�B
T�B
VB
VB
VmB
V�B
W�B
W�B
W$B
V�B
VSB
T,B
Q4B
NB
M�B
Q�B
S�B
TB
S�B
TaB
T�B
T�B
VmB
V�B
W�B
Y1B
[WB
ZkB
\CB
\�B
[WB
XEB
V�B
VB
V�B
X�B
V�B
U�B
T�B
T�B
S�B
S�B
R B
PB
N�B
L�B
K�B
I�B
F�B
B'B
=qB
:*B
@ B
?}B
?B
>�B
<�B
9�B
6FB
0�B
./B
,�B
+�B
)�B
%�B
#:B
"NB
"B
#�B
#nB
#�B
#�B
$@B
!�B
 �B
 \B
 'B
VB
dB
�B
CB
/B
/B
�B
VB
VB
�B
�B
�B
/B
CB
QB
�B
B
�B
�B
�B
�B
�B
kB
eB
�B
YB
�B
gB
MB
�B
�B
�B
�B
B
�B
�B
B
�B
B
�B
�B
	�B
�B
�B
�B
�B
�B
SB
B
�B
[B
AB
B
B
9B
B
�B
+B
B
B
	B
?B
�B
�B
+B
zB
�B
�B
�B
MB
GB
[B
UB
 B
  B	�}B	�.B	��B	��B	�.B	��B	�wB	��B	�wB	��B	��B	��B	��B	�]B	�.B	��B	��B	��B	��B	��B	�wB	�BB	��B	��B	�wB	�wB	�BB	�(B	��B	��B	�(B	�(B	�B	�B	�BB	�BB	��B	�B	�]B	�BB	�wB	�(B	�BB	�wB	�BB	�BB	�]B	�BB	�wB	�B	��B	�BB	�(B	�B	�]B	��B	��B	�cB	�HB	�HB	��B	�}B	�HB	��B	�cB	��B	�cB	�cB	�HB	�}B
 B
UB
�B
�B
�B
�B
�B
�B
�B
aB
�B
�B
9B
9B
mB
�B
�B
�B
YB
?B
YB
?B
B
_B
�B
�B
	B
�B
	7B
	RB
	�B

#B

	B

=B

�B
xB
B
B
�B
�B
�B
�B
B
�B
�B
"B
pB
�B
B
\B
�B
HB
}B
�B
B
NB
NB
�B
�B
B
TB
TB
oB
TB
TB
�B
�B
@B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
B
MB
2B
MB
B
�B
�B
B
�B

B

B
$B

B
?B
�B
�B
+B
_B
+B
+B
B
yB
yB
�B
�B
�B
�B
�B
�B
KB
B
B
KB
B
B
�B
�B
#B
WB
�B
qB
�B
�B
�B
/B
/B
�B
jB
�B
B
!B
VB
�B
pB
 BB
 �B
!B
!B
!|B
!�B
!�B
"NB
"hB
#TB
#nB
#�B
#�B
#�B
$ZB
$�B
$�B
$�B
%,B
%zB
&B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
(>B
($B
($B
($B
(sB
)_B
)_B
)�B
)�B
)�B
)�B
)�B
*KB
*�B
+�B
+�B
+�B
+�B
,=B
,�B
,�B
-)B
-]B
-]B
.}B
.�B
.�B
/ B
/iB
/�B
/�B
/�B
0UB
0�B
0�B
1[B
1�B
1�B
2aB
2GB
2�B
2�B
2�B
2|B
2�B
3B
33B
3MB
3hB
3�B
4B
4B
4B
4TB
4nB
4�B
4�B
4�B
5ZB
5�B
5�B
6+B
6�B
6�B
7�B
7�B
8B
88B
88B
8lB
8�B
8�B
8�B
8�B
8�B
9rB
9�B
:�B
:�B
:�B
:�B
;JB
;B
;�B
<6B
<�B
<�B
<�B
<�B
<�B
=qB
=�B
=�B
>B
=�B
=�B
>B
>�B
>�B
>�B
?}B
?�B
@ B
@4B
@�B
@�B
@�B
@�B
A�B
A�B
BB
B�B
B�B
CB
C-B
CGB
C�B
D�B
D�B
EB
EB
EB
EmB
E�B
E�B
E�B
E�B
F�B
FtB
F�B
F�B
GB
F�B
GzB
G�B
HKB
H1B
HfB
H�B
H�B
HfB
H�B
H�B
H�B
IB
IRB
I�B
I�B
J#B
JrB
JrB
J=B
J�B
KB
K)B
K^B
KxB
K�B
K�B
LB
K�B
L0B
L�B
M6B
MPB
M�B
M�B
NB
N"B
NVB
NVB
NVB
N�B
N�B
OB
OB
O\B
O�B
PB
PbB
P�B
P�B
P�B
QNB
QNB
QNB
Q�B
Q�B
Q�B
R B
RTB
SB
S&B
S@B
S�B
S�B
TB
T,B
T{B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UB
UMB
UgB
VB
V9B
V�B
W$B
W?B
WsB
WsB
WsB
W�B
W�B
W�B
XEB
X�B
X�B
YB
YKB
ZB
Z�B
[#B
[#B
[qB
[�B
\B
\�B
\�B
\�B
]B
]B
]B
]dB
]�B
]�B
^OB
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
`BB
`�B
`BB
`\B
`'B
`\B
`vB
`\B
`\B
`�B
`�B
`�B
a�B
a�B
bNB
bhB
b�B
c:B
c�B
c�B
c�B
c�B
d&B
dZB
d&B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
e,B
eFB
e`B
e�B
e�B
e�B
ffB
gB
gmB
g�B
g�B
gRB
h
B
hXB
h�B
h�B
iB
i_B
i�B
i�B
i�B
i�B
jB
jB
j�B
j�B
kB
kQB
kQB
kQB
k�B
k�B
lqB
lqB
m)B
l�B
mwB
m�B
n/B
ncB
n}B
n}B
n}B
n}B
n�B
n�B
oB
o�B
o�B
p!B
poB
p�B
p�B
qB
qAB
q[B
q[B
qvB
rB
r-B
r-B
r-B
rGB
r|B
r�B
sB
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
tB
tB
t9B
tTB
tnB
tTB
tTB
tnB
t�B
t�B
t�B
uZB
utB
utB
vB
v`B
vFB
v`B
vzB
v�B
v�B
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
xRB
x�B
x�B
x�B
y	B
yXB
y�B
y�B
y�B
z*B
z^B
z^B
z^B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{0B
z�B
{B
{JB
{JB
{JB
{dB
|B
|B
|B
|B
|B
}B
}B
}VB
}�B
}�B
~B
~BB
~(B
~BB
~BB
~�B
B
cB
�B
�B
� B
� B
�B
�B
�OB
�4B
��B
��B
�B
� B
�;B
�UB
�UB
�;B
�UB
��B
��B
��B
��B
�AB
�AB
�'B
��B
��B
�-B
�aB
�{B
�{B
��B
�B
�MB
��B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105246  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192540  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192541  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192541                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042552  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042552  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                